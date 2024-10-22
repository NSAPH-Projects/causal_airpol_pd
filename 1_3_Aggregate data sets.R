# ############################################################################ #
#' Project: Medicare FFS enrollment until first PD hospitalization     
#' Code: remove denominator years after first PD hosp
#' Inputs: denominator files, QD exposure data, meteorological data, hosp data             
#' Outputs: aggregated data for time to first PD hospitalization                
#' Author: Daniel Mork                                                  
#' Date: 5/19/2023
# ############################################################################ #

setwd("/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/data")

# Setup ---------------------
library(data.table)
library(fst)
options(stringsAsFactors = FALSE)

# Run first:
# source("1. Medicare FFS enrollment.R")
# source("2. Merge PD hospitalization data.R")
dt <- read_fst("complete_PD_denom.fst", as.data.table = TRUE)


setkey(dt, zip, cohort, year, age_grp, sex, race, dual, region)


# Prepare denominator for aggregation:
#   Limit: 2 year 'clean' period, stop following after event year, 
#         begin year 2001 to pair with 2000 exposures
#   Select: # persons, # hospitalizations
#   By: zip, cohort, year, age_grp, sex, race, dual
dt_PD <- dt[PD_year >= cohort + 2 & # 2 year 'clean' period
                PD_year >= year &     # censor observations after PD event
                year >= 2001]           # begin with year 2001 to pair with 2000 exposures
# Year 2000 data is removed due to 1 year lag in exposure linkage
#   so year 2000 cohort now begins in 2001
dt_PD[cohort == 2000, cohort := 2001]
dt_PD[, .(uniqueN(qid))] # 50053399 unique individuals
# Save updated denominator data
dt_PD[, bene_id := NULL]
dt_PD[, dual := as.integer(dual)]
dt_PD[, PD_year := as.integer(PD_year)]
write_fst(dt_PD, "complete_PD_denom_2yrclean.fst")
#write.csv(dt_PD, "complete_PD_denom.csv")


# Aggregate PD data by zip-year-strata
#   strata = {cohort, age group, sex, race, Medicaid eligible}
PD_agg <- dt_PD[, .(n_persons = .N, n_hosp = sum(PD)),
                    by = .(zip, cohort, year, age_grp, sex, race, dual, region)]
PD_agg[, n_years := 1L + year - cohort] # calculate years of observation
PD_agg[, .(.N, sum(n_hosp))] # 41948558 units; 5935558 PD events
write_fst(PD_agg, "PD_aggregated.fst")
#write.csv(PD_agg, "PD_aggregated.csv")
