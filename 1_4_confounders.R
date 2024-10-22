# ############################################################################ #
#' Project: Causal ADRD                                     
#' Code: gather census and BRFSS data for every year/zipcode     
#' Inputs: denominator data (2000-2016), BRFSS data (exclude b/c no zip after 2012)                             
#' Outputs: year/zip confounders   
#' Author: Daniel Mork                                                         
#' Last updated: Mar 09, 2022                                                  
#' Memory to run: <32 GB
# ############################################################################ #
rm(list = ls())
gc()

##### 0. Setup #####
library(data.table)
library(fst)
library(tidyverse)
#library(dplyr)
#library(NSAPHutils)
#library(zipcode)
#data(zipcode)
options(stringsAsFactors = FALSE)

setDTthreads(threads = 16)

PD_agg <- read_fst("/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/data/PD_aggregated.fst", as.data.table = T)


##### 1. read in expsoure and confounder data #####
# Load zip code covars  
dir_confounders <- "/n/dominici_nsaph_l3/Lab/projects/analytic/confounders/"
zip_yr_dat <-
  rbindlist(lapply(2000:2016, function(y) {
    fread(paste0(dir_confounders, "merged_confounders_", y, ".csv"))
  }))

# Remove zips with missing data (might want to select cols of interest first)
zip_yr_dat <- zip_yr_dat[complete.cases(zip_yr_dat)]



##### 2. temp data #####
# load gridmet max temp
dir_temps <- "/n/dominici_nsaph_l3/Lab/projects/analytic/temperature_seasonal_zipcode/"

# Seasonal temperatures
temp_yr_dat <- fread(paste0(dir_temps, "temperature_seasonal_zipcode_combined.csv"))
temp_yr_dat <- temp_yr_dat[complete.cases(temp_yr_dat)]

setDT(PD_agg)
pd_census1 <- merge(PD_agg, zip_yr_dat, by.x = c("zip", "year"), by.y = c("ZIP", "year"),
                    all.x = TRUE)
pd_census <- merge(pd_census1, temp_yr_dat, by.x = c("zip", "year"), by.y = c("ZIP", "year"),
                   all.x = TRUE)

# merge patient data with previous year temp and rh
temp_yr_dat[, yr_yest := year-1]
setnames(temp_yr_dat,
         old=c("summer_tmmx", "summer_rmax"),
         new=c("summer_tmmx_yest", "summer_rmax_yest"))
temp_yr_dat <- temp_yr_dat[,-c(2, 5:6)]

pd_census <- merge(pd_census, temp_yr_dat, by.x = c("zip", "year"), by.y = c("ZIP", "yr_yest"),
                   all.x = TRUE)

rm(zip_yr_dat, PD_agg, temp_yr_dat, pd_census1)
pd_census[, zip := as.factor(zip)]
pd_census[, year := as.factor(year)]
pd_census[, age_grp := as.factor(age_grp)]
pd_census[, sex := as.factor(sex)]
pd_census[, race := as.factor(race)]
pd_census[, dual := as.factor(dual)]
pd_census[, pir := medianhousevalue/(medhouseholdincome + 1)] # avoid divide by 0
pd_census[, region := as.factor(region)]
#rm(PD_agg, pd_census1, zip_yr_dat, pd_compl)

pd_census <- pd_census[complete.cases(pd_census)]

# make sure categorical variables are factors
pd_census[, `:=`(zip = as.factor(zip),
                 year = as.factor(year),
                 age_grp = as.factor(age_grp),
                 sex = as.factor(sex),
                 race = as.factor(race),
                 dual = as.factor(dual),
                 region = as.factor(region))]

pd_census[, `:=`(mean_bmi_sq = mean_bmi^2,
                 smoke_rate_sq = smoke_rate^2,
                 hispanic_sq = hispanic^2,
                 pct_blk_sq = pct_blk^2,
                 pir_sq = pir^2,
                 poverty_sq = poverty^2,
                 education_sq = education^2,
                 popdensity_sq = popdensity^2,
                 pct_owner_occ_sq = pct_owner_occ^2,
                 summer_tmmx_sq = summer_tmmx^2,
                 summer_tmmx_yest_sq = summer_tmmx_yest^2,
                 summer_rmax_sq = summer_rmax^2,
                 summer_rmax_yest_sq = summer_rmax_yest^2)]

write_fst(pd_census, "~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged.fst")

# trim
pd_census_trim <- read_fst("~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged.fst",
               as.data.table = TRUE) %>%
  mutate(outer_pm_tod = ifelse((pm25.current_year>quantile(pm25.current_year, prob=c(0.95))) | (pm25.current_year<quantile(pm25.current_year, prob=c(0.05))),
                        1, 0),
         outer_pm_yest = ifelse((pm25.one_year_lag>quantile(pm25.one_year_lag, prob=c(0.95))) | (pm25.one_year_lag<quantile(pm25.one_year_lag, prob=c(0.05))),
                           1, 0),
         outer_no2_tod = ifelse((no2.current_year>quantile(no2.current_year, prob=c(0.95))) | (no2.current_year<quantile(no2.current_year, prob=c(0.05))),
                               1, 0),
         outer_no2_yest = ifelse((no2.one_year_lag>quantile(no2.one_year_lag, prob=c(0.95))) | (no2.one_year_lag<quantile(no2.one_year_lag, prob=c(0.05))),
                                1, 0),
         outer_o3_tod = ifelse((ozone_summer.current_year>quantile(ozone_summer.current_year, prob=c(0.95))) | (ozone_summer.current_year<quantile(ozone_summer.current_year, prob=c(0.05))),
                               1, 0),
         outer_o3_yest = ifelse((ozone_summer.one_year_lag>quantile(ozone_summer.one_year_lag, prob=c(0.95))) | (ozone_summer.one_year_lag<quantile(ozone_summer.one_year_lag, prob=c(0.05))),
                                1, 0))
write_fst(pd_census_trim, "~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged_trim.fst")

#############################################################################################
pd_census <- read_fst("~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged.fst",
               as.data.table = TRUE)

pd_census_no_indiv <- pd_census %>%
  select("zip", "year", "pm25.current_year", "no2.current_year", "ozone_summer.current_year",
         "pm25.one_year_lag", "no2.one_year_lag", "ozone_summer.one_year_lag", 
         "mean_bmi", "smoke_rate", "hispanic", "pct_blk", 
         "pir", "poverty", "education", "popdensity", "pct_owner_occ",
         "mean_bmi_sq", "smoke_rate_sq", "hispanic_sq", "pct_blk_sq",
         "pir_sq", "poverty_sq", "education_sq", "popdensity_sq", "pct_owner_occ_sq",
         "summer_tmmx", "summer_rmax", "summer_tmmx_yest", "summer_rmax_yest",
         "summer_tmmx_sq", "summer_rmax_sq", "summer_tmmx_yest_sq", "summer_rmax_yest_sq", "region") %>%
  distinct()

write_fst(pd_census_no_indiv, "~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged_no_indiv.fst")

#trim
pd_census_trim <- read_fst("~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged_trim.fst",
                      as.data.table = TRUE)

pd_census_no_indiv <- pd_census_trim %>%
  select("zip", "year", "pm25.current_year", "no2.current_year", "ozone_summer.current_year",
         "pm25.one_year_lag", "no2.one_year_lag", "ozone_summer.one_year_lag", 
         "mean_bmi", "smoke_rate", "hispanic", "pct_blk", 
         "pir", "poverty", "education", "popdensity", "pct_owner_occ",
         "pir_sq", "poverty_sq", "education_sq", "popdensity_sq", "pct_owner_occ_sq",
         "summer_tmmx", "summer_rmax", "summer_tmmx_yest", "summer_rmax_yest", 
         "summer_tmmx_sq", "summer_rmax_sq", "summer_tmmx_yest_sq", "summer_rmax_yest_sq", "region",
         "outer_pm_tod", "outer_pm_yest", "outer_no2_tod", "outer_no2_yest", "outer_o3_tod", "outer_o3_yest") %>%
  distinct()

write_fst(pd_census_no_indiv, "~/nsaph_projects/vwang_causal_airpol_pd/data/pd_merged_no_indiv_trim.fst")
