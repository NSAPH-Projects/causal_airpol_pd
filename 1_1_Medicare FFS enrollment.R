# ############################################################################ #
#' Project: Medicare FFS enrollment until first PD hospitalization                                
#' Code: determine FFS enrollment period for each individual
#' Inputs: pre-processed denominator files
#' Outputs: annual Medicare beneficiary data
#' Author: Daniel Mork                                                  
#' Date: 5/30/2023
# ############################################################################ #

setwd("/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/data")

# Setup ---------------------
library(data.table)
library(fst)
options(stringsAsFactors = FALSE)
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/analytic/"

# Read denominator files ---------------------
cat("Reading denominator files...")
f <- list.files(paste0(dir_data, "denom_by_year/"), pattern = "\\.fst", full.names = TRUE)
vars <- c("qid", "year", "zip", "hmo_mo", "age", "race", "sex", "dual", "dead", "statecode")
dt <- rbindlist(lapply(f[2:18], read_fst, columns = vars, as.data.table = TRUE))
setkey(dt, qid, year)
dt[,.N] # 651691916 total person years of data
# Check how many people removed
dt[year < 2000 | hmo_mo != 0, .N] # 162148450 person years removed
dt <- dt[year >= 2000 &  # entry in 2000
           hmo_mo == 0 & # retain FFS only (zero HMO months)
           race != 0 & sex != 0] # known race and sex
dt[, hmo_mo := NULL] # drop HMO variable
dt <- unique(dt, by = c("qid", "year")) # remove any duplicate columns
dt[,.N] # 489543466 total person-years


# Last year of continuous enrollment ---------------------
cat("\nDetermine last year FFS")
# function to get last year of continuous enrollment in Medicare FFS
last_yr_cont <- function(year) { 
  year[first(which(!(first(year):2017 %in% year))) - 1] }
dt[, cohort := first(year), by = qid] # define entry year
dt[, last_year_ffs := last_yr_cont(year), by = qid] # define last year continuously enrolled
# remove records for any years after first departure from FFS
dt[year > last_year_ffs, .N] # 12039782 removed
dt <- dt[year <= last_year_ffs]
dt[, .N] # 477503684 total person-years
# corrected age (in case of inconsistencies)
dt[, age_corrected := first(age) + year - first(year), by = qid]
dt[, age := NULL]
# Define age groups for aggregation ---------------------
dt[, age_grp := cut(age_corrected, breaks = c(64, 69, 74, 79, 84, 89, 94, Inf))]


# Merge RTI race code ---------------------
# available 2009-2014 and 2016 in current data
rti <- rbindlist(lapply(c(2009:2014, 2016), function(y) {
  d <- fread(paste0(dir_data, "auxiliary_medicare_cols/rti_race_", y, ".csv"))
  d[, year := y]
}), fill = TRUE)
setkey(rti, qid, year)
rti <- rti[!is.na(rti_race_cd) & rti_race_cd != 0 & rti_race_cd != "X"]
dt <- merge(dt, rti, by = c("qid", "year"), all.x = TRUE)
dt[is.na(rti_race_cd) | rti_race_cd == "", rti_race_cd := race]
rm(rti)


# Save data ---------------------
write_fst(dt, "denom_file1.fst")
