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
library(WeightIt)

# Setup
rm(list = ls())
gc()

# get directories, classifications of variables, formulas, and functions
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

#trimmed exposure? please specify
trim <- "low"

i <- 1 # change the number to specify exposure

per <- "same"
exposure <- exposure_name[i]  
temp <- temp_same[1]
rh <- rh_same[1]

## REMEMBER TO SPECIFY BOOTSTRAP SAMPLE FOLDER

for (array_id in c(1:500)){
df <- read_fst(paste0("~/nsaph_projects/vwang_causal_airpol_pd/code/bootstrap_code/boot_dat_low_pm/boot_", array_id, ".fst"),
               as.data.table = T)

df_gps <- df %>%
  select("zip", "year", "pm25.current_year", "no2.current_year", "ozone_summer.current_year",
         "pm25.one_year_lag", "no2.one_year_lag", "ozone_summer.one_year_lag", 
         "mean_bmi", "smoke_rate", "hispanic", "pct_blk", 
         "pir", "poverty", "education", "popdensity", "pct_owner_occ",
         "mean_bmi_sq", "smoke_rate_sq", "hispanic_sq", "pct_blk_sq",
         "pir_sq", "poverty_sq", "education_sq", "popdensity_sq", "pct_owner_occ_sq",
         "summer_tmmx", "summer_rmax", 
         "summer_tmmx_sq", "summer_rmax_sq", "region") %>%
  distinct()

df_gps[, id := 1:nrow(df_gps)]
df_gps <- as.data.frame(df_gps)

df_gps_sub <- df_gps[, c("id", "zip", "year", exposure)]

# same
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

pseudopop_w <- merge(df, subset(gps_w, select = c("zip", "year", "trim_stabilized_ipw")),
                     by = c("zip", "year"))

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

# run parametric outcome model
m2 <- glm(as.formula(paste("n_hosp ~", paste(c(exposure, strata_vars, zip_vars, temp_same, rh_same), collapse = "+", sep = ""))),
          data = pseudopop_w,
          offset = log(n_persons * n_years),
          family = poisson(link = "log"),
          weights = trim_stabilized_ipw)

cat(paste(exposure, "method4", trim, m2$coefficients[exposure], sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/boot_SE_", per, ".txt"),
    append = TRUE)
}