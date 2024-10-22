
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
trim <- "low"

i <- 1 # change the number to specify exposure

# set exposure
per <- "same"
exposure <- exposure_name[i]  # change the number to specify exposure
temp <- temp_same
rh <- rh_same


# set parameters for this computing job
n_cores <- 4

# get data

for (array_id in c(1:500)){
df_boot <- read_fst(paste0("~/nsaph_projects/vwang_causal_airpol_pd/code/bootstrap_code/boot_dat_low_pm/boot_", array_id, ".fst"),
                                      as.data.table = T)

# run model (Poisson regression)
bam_associational <- glm(as.formula(paste("n_hosp ~", paste(c(exposure, temp, rh,
                                                              strata_vars,
                                                              zip_vars),
                                                            collapse = "+", sep = ""))),
                         data = df_boot,
                         offset = log(n_persons * n_years),
                         family = quasipoisson(link = "log"))
  cat(paste(exposure, "method1", trim, bam_associational$coefficients[exposure], array_id, sep = ","),
      sep = "\n",
      file = paste0(dir_results, "naive_approach/boot_SE_", per, ".txt"),
      append = TRUE)
}
