##### Setup #####
rm(list = ls())
gc()

n_threads = 48
Sys.setenv(OMP_NUM_THREADS = n_threads)
library(data.table)
library(fst)
library(tidyverse)
library(splitstackshape)
options(stringsAsFactors = FALSE)
setDTthreads(threads = n_threads)
threads_fst(n_threads)

# # get directories, classifications of variables, formulas, and functions
# dir_code <- "/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/code/"
# source(paste0(dir_code, "constants.R"))

# read in data
dat <- read_fst("~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged.fst",
                           as.data.table = TRUE) %>% filter(pm25.current_year <=9)
# dat <- read_fst("~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged_trim.fst",
#                 as.data.table = TRUE) %>% filter(no2.current_year <=53)
# dat <- read_fst("~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged_trim.fst",
#                 as.data.table = TRUE) %>% filter(ozone_summer.current_year <=70)

dat[, .N] # 17031918; 39782218; 39666303

setkey(dat, year, sex, dual, race, age_grp)
dat[, strata := .GRP, by = .(year, sex, dual, race, age_grp)]
setkey(dat, zip)

zips <- dat[, unique(zip)]
length(zips) # n = 32577; 34758; 34753
round(2 * sqrt(length(zips))) # m = 361; 373; 373
#sqrt(373 / 34761) # 0.1035877


B <- 525
zipcodes <- unique(dat$zip)

for (i in 1:B) {
  sam <- data.frame(zip=sample(zipcodes, round(2 * sqrt(length(zips))), replace = TRUE)) %>%
    group_by(zip) %>%
    summarise(n=n())
  df_boot <- dat %>%
    left_join(sam, by="zip") %>%
    filter(!is.na(n))
  setDT(df_boot)
  df_boot <- expandRows(df_boot, "n")
  write_fst(df_boot, paste0("~/nsaph_projects/vwang_causal_airpol_pd/code/bootstrap_code/boot_dat_low_pm/boot_", i, ".fst"))
}
