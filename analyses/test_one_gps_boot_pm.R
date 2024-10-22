##### Setup #####

# devtools::install_github("fasrc/CausalGPS", ref="develop")
library(data.table)
library(fst)
library(CausalGPS)
library(wCorr)
library(mgcv)
library(ggplot2)
library(tidyverse)
library(splines)

rm(list = ls())
gc()

#trimmed exposure? please specify
trim <- "Untrimmed"

# get directories, classifications of variables, formulas, and functions
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

i <- 1 # change the number to specify exposure

# set exposure
# same
per <- "same"
cap_name <- c(1.2, 0.75, 3.0)
exposure <- exposure_name[i]
cap <- cap_name[i]
temp <- temp_same[1]
rh <- rh_same[1]
temp_out <- temp_same
rh_out <- rh_same

# ## prior
# per <- "prior"
# cap_name_yest <- c(1.5, 0.5, 3.0) 
# exposure <- exposure_name_yest[i]
# cap <- cap_name_yest[i]
# temp <- temp_prior[1]
# rh <- rh_prior[1]
# temp_out <- temp_same
# rh_out <- rh_same

for (array_id in c(1:500)){
df <- read_fst(paste0("~/nsaph_projects/vwang_causal_airpol_pd/code/bootstrap_code/boot_dat/boot_", array_id, ".fst"),
               as.data.table = T)

df_gps <- df %>%
  select("zip", "year", "pm25.current_year", "no2.current_year", "ozone_summer.current_year",
         "pm25.one_year_lag", "no2.one_year_lag", "ozone_summer.one_year_lag", 
         "mean_bmi", "smoke_rate", "hispanic", "pct_blk", 
         "pir", "poverty", "education", "popdensity", "pct_owner_occ",
         "mean_bmi_sq", "smoke_rate_sq", "hispanic_sq", "pct_blk_sq",
         "pir_sq", "poverty_sq", "education_sq", "popdensity_sq", "pct_owner_occ_sq",
         "summer_tmmx", "summer_rmax", "summer_tmmx_yest", "summer_rmax_yest",
         "summer_tmmx_sq", "summer_rmax_sq", "summer_tmmx_yest_sq", "summer_rmax_yest_sq", "region") %>%
  distinct()

df_gps[, id := 1:nrow(df_gps)]
df_gps <- as.data.frame(df_gps)
df_gps_sub <- df_gps[, c("id", "zip", "year", exposure)]

set.seed(array_id)
gps <- estimate_gps(df_gps[,c("id", exposure)],
                    df_gps[,c("id", "mean_bmi", "smoke_rate", "hispanic", "pct_blk", 
                          "pir", "poverty", "education", "popdensity", "pct_owner_occ",
                          "region", "year", temp, rh)],
                    params = list(xgb_nrounds = seq(10, 50),
                                  xbg_eta = seq(0.1, 0.4, 0.01)),
                    sl_lib = c("m_xgboost"),
                    nthread = 4)

gps_w_sub <- gps$dataset
gps_w <- merge(df_gps_sub, gps_w_sub, by=c("id", exposure))

# stabilize GPS using marginal probability of exposure (modeled normally) and cap extreme weights at 10
marginal_prob <- dnorm(gps_w[,c(exposure)],
                             mean = mean(gps_w[,c(exposure)]),
                             sd = sd(gps_w[,c(exposure)]))
gps_w$stabilized_ipw <- marginal_prob / gps_w$gps ## check estimate_gps
gps_w$trim_stabilized_ipw <- ifelse(gps_w$stabilized_ipw > quantile(gps_w$stabilized_ipw, prob=c(0.99)), 
                                       quantile(gps_w$stabilized_ipw, prob=c(0.99)), 
                                       ifelse(gps_w$stabilized_ipw <quantile(gps_w$stabilized_ipw, prob=c(0.01)),
                                              quantile(gps_w$stabilized_ipw, prob=c(0.01)),
                                              gps_w$stabilized_ipw))
gps_w <- gps_w %>%
  distinct(zip, year, trim_stabilized_ipw)
pseudopop_w <- merge(df, gps_w, by = c("zip", "year"))

# run parametric outcome model
cl <- parallel::makeCluster(4, type = "PSOCK")
m2 <- bam(as.formula(paste("n_hosp ~", paste(c(exposure, strata_vars), collapse = "+", sep = ""))),
                         data = pseudopop_w,
                         offset = log(n_persons * n_years),
                         family = poisson(link = "log"),
                         weights = trim_stabilized_ipw,
                         samfrac = 0.05,
                         chunk.size = 5000,
                         control = gam.control(trace = TRUE),
                         nthreads = 4,
                         cluster = cl)
parallel::stopCluster(cl)
cat(paste(exposure, "method2", trim, m2$coefficients[exposure], sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/boot_SE_", per, ".txt"),
    append = TRUE)

m2 <- glm(as.formula(paste("n_hosp ~", paste(c(exposure, strata_vars, zip_vars, temp_out, rh_out), collapse = "+", sep = ""))),
          data = pseudopop_w,
          offset = log(n_persons * n_years),
          family = poisson(link = "log"),
          weights = trim_stabilized_ipw)
cat(paste(exposure, "method4", trim, m2$coefficients[exposure], sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/boot_SE_", per, ".txt"),
    append = TRUE)


###### match

gps_m_sub <- gps$dataset
gps_m <- as.data.table(merge(df_gps_sub, gps_m_sub, by=c("id", exposure)))

setkey(gps_m, year)
zip_list_yr <- split(gps_m, gps_m$year)

m_pseudo_list <- lapply(zip_list_yr,
                        match_within_yr,
                        gps_mx=gps$gps_mx,
                        w_mx=gps$w_mx,
                        expo=exposure)

matched <- rbindlist(m_pseudo_list)

matched <- matched %>%
  distinct(zip, year, counter_weight)
pseudopop_m <- merge(df, matched, by = c("zip", "year"))

# run parametric outcome model
cl <- parallel::makeCluster(4, type = "PSOCK")
m2 <- bam(as.formula(paste("n_hosp ~", paste(c(exposure, strata_vars), collapse = "+", sep = ""))),
                         data = pseudopop_m,
                         offset = log(n_persons * n_years),
                         family = poisson(link = "log"),
                         weights = counter_weight,
                         samfrac = 0.05,
                         chunk.size = 5000,
                         control = gam.control(trace = TRUE),
                         nthreads = 4,
                         cluster = cl)
parallel::stopCluster(cl)
cat(paste(exposure, "method3", trim, m2$coefficients[exposure], sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/boot_SE_", per, ".txt"),
    append = TRUE)
}