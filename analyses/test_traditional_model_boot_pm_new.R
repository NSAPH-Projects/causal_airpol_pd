
##### Setup #####

library(data.table)
library(fst)
library(mgcv)
library(tidyverse)

rm(list = ls())
gc()


# Setup


# get directories, classifications of variables, and formulas
dir_code <- "/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

#trimmed exposure? please specify
trim <- "Untrimmed"

i <- 1 # change the number to specify exposure

# set exposure
## same
per <- "same"
exposure <- exposure_name[i]  # change the number to specify exposure
temp <- temp_same
rh <- rh_same

## prior
exposure_yest <- exposure_name_yest[i]  # change the number to specify exposure


# set parameters for this computing job
n_cores <- 4

# get data

for (array_id in c(1:500)){
df_boot <- read_fst(paste0("~/nsaph_projects/vwang_causal_airpol_pd/code/bootstrap_code/boot_dat/boot_", array_id, ".fst"),
                                      as.data.table = T)

df_boot <- as.data.frame(df_boot)


  
# run model (Poisson regression)

  # # run model (Poisson regression)
  # bam_associational <- glm(as.formula(paste("n_hosp ~", paste(c(exposure, temp, rh,
  #                                                               strata_vars,
  #                                                               zip_vars),
  #                                                             collapse = "+", sep = ""))),
  #                          data = df_boot,
  #                          offset = log(n_persons * n_years),
  #                          family = quasipoisson(link = "log"))
  # cat(paste(exposure, "method1", trim, bam_associational$coefficients[exposure], sep = ","),
  #     sep = "\n",
  #     file = paste0(dir_results, "naive_approach/boot_SE_", per, ".txt"),
  #     append = TRUE)
  # 
  # # run model (Poisson regression)
  # bam_associational <- glm(as.formula(paste("n_hosp ~", paste(c(exposure_yest, temp_yest, rh_yest,
  #                                                               strata_vars,
  #                                                               zip_vars),
  #                                                             collapse = "+", sep = ""))),
  #                          data = df_boot,
  #                          offset = log(n_persons * n_years),
  #                          family = quasipoisson(link = "log"))
  # cat(paste(exposure_yest, "method1", trim, bam_associational$coefficients[exposure_yest], sep = ","),
  #     sep = "\n",
  #     file = paste0(dir_results, "naive_approach/boot_SE_", per_yest, ".txt"),
  #     append = TRUE)
  
  bam_associational <- glm(as.formula(paste("n_hosp ~", paste(c(exposure, temp, rh,
                                                                exposure_yest, 
                                                           strata_vars,
                                                           zip_vars),
                                                         collapse = "+", sep = ""))),
                           data = df_boot,
                           offset = log(n_persons * n_years),
                           family = quasipoisson(link = "log"))
  cat(paste(exposure, "method1_new", trim, bam_associational$coefficients[exposure], array_id, sep = ","),
      sep = "\n",
      file = paste0(dir_results, "mutually_adj_approach/boot_SE_", per, "_new.txt"),
      append = TRUE)
}
