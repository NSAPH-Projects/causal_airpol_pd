# ############################################################################ #      
#' Project: Medicare FFS enrollment until first PD hospitalization     
#' Code: combine qid entry/exit with first PD hospitalization
#' Inputs: annual beneficiary data, identified PD hospitalizations            
#' Outputs: exposures in grid by qid (rows) and year (cols)                    
#' Author: Daniel Mork                                                                                     
#' Date: 5/19/2023
# ############################################################################ #

setwd("/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/data")

# Setup ---------------------
library(data.table)
library(fst)
options(stringsAsFactors = FALSE)
dir_data <- "~/n/dominici_nsaph_l3/Lab/projects/dmork_dataverse_pd_first_hosp_denom/"
dir_hosp <- "/n/dominici_nsaph_l3/Lab/projects/analytic/parkinsons_disease/"

# Run first:
# source(paste0(dir_code, "1. Medicare FFS enrollment.R"))
dt <- read_fst("/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/data/denom_file1.fst", as.data.table = TRUE)


#### Read PD hospitalization data, merge with denominator ####
cat("\nRead PD hospitalization data")
f <- list.files(dir_hosp, pattern = "\\.csv", full.names = TRUE)
PDhosp <- rbindlist(lapply(f, fread))

# Restrict to first ADRD hospitalization
firstPD <- PDhosp[, .(PD_year = min(year)), by = bene_id]
firstPD[, PD := TRUE]
firstPD[, .N] # 1256380
rm(PDhosp)

# Match firstHosp to qid/year in dt
cat("\nMerge hospitalization data")
dt <- merge(dt, firstPD, by.x = "qid", by.y = "bene_id", all.x = TRUE)
dt[is.na(PD), PD := FALSE]
dt[(PD), PD := (year == PD_year)]
dt[is.na(PD_year), PD_year := last_year_ffs]
rm(firstPD)

# 4 regions based on census?
NE <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
S <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
       "AR", "MD", "DE", "OK", "TX", "LA")
MW <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
W <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")

dt[, region := ifelse(statecode %in% NE, "NE",
                      ifelse(statecode %in% S, "S", 
                             ifelse(statecode %in% MW, "MW",
                                    ifelse(statecode %in% W, "W", "Other"))))]

# Save updated denominator data
write_fst(dt, "complete_PD_denom.fst")
