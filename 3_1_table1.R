#### Note: This script should be run after the data have been cleaned, processed, and aggregated data but NOT trimmed (i.e., files #1-5 in the code/aggregation folder have been run) ####


#### Set up computations ####

library(data.table)
library(fst)

# get directories and classifications of variables
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

# optional: user sets the number of cores in this computing job
# more important: request enough memory (at least 100 GB) to open the large patient dataset
n_cores <- 48
setDTthreads(threads = n_cores)


### Get all patient data ###
dt <- read_fst(paste0(dir_data, "complete_PD_denom_2yrclean.fst"),
               as.data.table = TRUE)
setkey(dt, zip, cohort, year, age_grp, sex, race, dual, region)
dt <- dt[PD_year >= cohort + 2 & # 2 year 'clean' period
           PD_year >= year & # censor observations after ADRD event
           year >= 2001] # begin with year 2001 to pair with 2000 exposures
dt[cohort == 2000, cohort := 2001] # start 2000 cohort in 2001
dt <- subset(dt, select = c("qid",
                            "cohort",
                            "PD_year",
                            "last_year_ffs",
                            "PD",
                            "zip",
                            indiv_var_names)) # includes "year"

# get subset of patients who experienced PD event
dt_PD <- dt[year == PD_year][PD == TRUE]
PD_patients <- dt_PD$qid
dt_PD <- dt[qid %in% PD_patients]

# get subset of patients who did not experienced PD event
dt_PD_no <- dt[year == PD_year][PD == FALSE]
PD_no_patients <- dt_PD_no$qid
dt_PD_no <- dt[qid %in% PD_no_patients]

# get data for each patient's FFS entry year only (i.e., ignoring later years of observation), for full cohort and ADRD cohort
dt_entry <- dt[year == cohort]
dt_PD_entry <- dt_entry[qid %in% PD_patients]
dt_PD_no_entry <- dt_entry[qid %in% PD_no_patients]

# collect relevant datasets into list for easy indexing
cohorts <- list(dt, dt_PD, dt_PD_no)
entry_year_cohorts <- list(dt_entry, dt_PD_entry, dt_PD_no_entry)
names(cohorts) <- c("Full", "PD", "PD_no")
names(entry_year_cohorts) <- c("Full", "PD", "PD_no")


#### Set up txt file to store results ####
cat(paste("Variable",
          "Cohort",
          "Value",
          sep = ","),
    sep = "\n",
    file = paste0(dir_results, "table1/tab1.txt"),
    append = TRUE)


#### Calculate and save number of individuals and person-years ####
for (cohort in c("Full", "PD", "PD_no")){
  cat(paste("Number of individuals",
            paste0(cohort, "Cohort"),
            uniqueN(cohorts[[cohort]][["qid"]]),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Number of person-years",
            paste0(cohort, "Cohort"),
            nrow(cohorts[[cohort]]),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
}


### Calculate and save individual-level variables, at each patient's year of FFS entry ###
for (cohort in c("Full", "PD", "PD_no")){
  
  # percent of each race (RTI-augmented race codes)
  cat(paste("Non-Hispanic White",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["race"]] == 1), " (", round(mean(entry_year_cohorts[[cohort]][["race"]] == 1) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Black",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["race"]] == 2), " (", round(mean(entry_year_cohorts[[cohort]][["race"]] == 2) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Hispanic",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["race"]] == 5), " (", round(mean(entry_year_cohorts[[cohort]][["race"]] == 5) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Asian/Pacific Islander",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["race"]] == 4), " (", round(mean(entry_year_cohorts[[cohort]][["race"]] == 4) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("American Indian/Alaska Native",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["race"]] == 6), " (", round(mean(entry_year_cohorts[[cohort]][["race"]] == 6) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Other",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["race"]] == 3), " (", round(mean(entry_year_cohorts[[cohort]][["race"]] == 3) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  
  # percent female (coded as 2 in the data) and male (coded as 1 in the data)
  cat(paste("Female",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["sex"]] == 2), " (", round(mean(entry_year_cohorts[[cohort]][["sex"]] == 2) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Male",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["sex"]] == 1), " (", round(mean(entry_year_cohorts[[cohort]][["sex"]] == 1) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  
  # percent Medicaid eligible
  cat(paste("Not Medicaid-eligible",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["dual"]] == 0), " (", round(mean(entry_year_cohorts[[cohort]][["dual"]] == 0) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("Medicaid eligible",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["dual"]] == 1), " (", round(mean(entry_year_cohorts[[cohort]][["dual"]] == 1) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  
  # percent of each age group (5-year bins)
  cat(paste(levels(entry_year_cohorts[[cohort]][["age_grp"]]),
            paste0(cohort, "Cohort"),
            paste0(table(entry_year_cohorts[[cohort]][["age_grp"]]), " (", round(prop.table(table(entry_year_cohorts[[cohort]][["age_grp"]])) * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
}


### Calculate and save region, at each patient's year of FFS entry ###
for (cohort in c("Full", "PD", "PD_no")){
  
  # percent of each race (RTI-augmented race codes)
  cat(paste("NE",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["region"]] == "NE"), " (", round(mean(entry_year_cohorts[[cohort]][["region"]] == "NE") * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("S",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["region"]] == "S"), " (", round(mean(entry_year_cohorts[[cohort]][["region"]] == "S") * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("MW",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["region"]] == "MW"), " (", round(mean(entry_year_cohorts[[cohort]][["region"]] == "MW") * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
  cat(paste("W",
            paste0(cohort, "Cohort"),
            paste0(sum(entry_year_cohorts[[cohort]][["region"]] == "W"), " (", round(mean(entry_year_cohorts[[cohort]][["region"]] == "W") * 100, 1), "%)"),
            sep = ","),
      sep = "\n",
      file = paste0(dir_results, "table1/tab1.txt"),
      append = TRUE)
}


### Calculate and save mean and SD of exposures and neighborhood variables, at each patient's year of FFS entry ###
# Load zip code covars  
dir_confounders <- "/n/dominici_nsaph_l3/Lab/projects/analytic/confounders/"
zip_yr_dat <-
  rbindlist(lapply(2000:2016, function(y) {
    fread(paste0(dir_confounders, "merged_confounders_", y, ".csv"))
  }))

# Remove zips with missing data (might want to select cols of interest first)
zip_yr_dat <- zip_yr_dat[complete.cases(zip_yr_dat)]
zip_yr_dat[, pir := medianhousevalue/(medhouseholdincome + 1)] # avoid divide by 0


##### 2. temp data #####
# load gridmet max temp
dir_temps <- "/n/dominici_nsaph_l3/Lab/projects/analytic/temperature_seasonal_zipcode/"

# Seasonal temperatures
temp_yr_dat <- fread(paste0(dir_temps, "temperature_seasonal_zipcode_combined.csv"))
temp_yr_dat <- temp_yr_dat[complete.cases(temp_yr_dat)]


setnames(zip_yr_dat,
         old="ZIP",
         new="zip")
setnames(temp_yr_dat,
         old="ZIP",
         new="zip")



# merge patient data with exposures and confounders, at each patient's FFS entry year
for (cohort in c("Full", "PD", "PD_no")){
  entry_year_cohorts[[cohort]] <- merge(entry_year_cohorts[[cohort]], temp_yr_dat,
                                        by.x = c("zip", "year"), by.y = c("zip", "year"),
                                        all.x = TRUE)
  entry_year_cohorts[[cohort]] <- merge(entry_year_cohorts[[cohort]], zip_yr_dat,
                                        by.x = c("zip", "year"), by.y = c("zip", "year"),
                                        all.x = TRUE)
}

# merge patient data with previous year temp and rh
temp_yr_dat[, yr_yest := year-1]
setnames(temp_yr_dat,
         old=c("summer_tmmx", "summer_rmax"),
         new=c("summer_tmmx_yest", "summer_rmax_yest"))
temp_yr_dat <- temp_yr_dat[,-c(2, 5:6)]

for (cohort in c("Full", "PD", "PD_no")){
  entry_year_cohorts[[cohort]] <- merge(entry_year_cohorts[[cohort]], temp_yr_dat,
                                        by.x = c("zip", "year"), by.y = c("zip", "yr_yest"),
                                        all.x = TRUE)
}

# entry_year_cohorts$Full <- entry_year_cohorts$Full[,-c(13:45)]
# entry_year_cohorts$PD <- entry_year_cohorts$PD[,-c(13:45)]

# calculate and save mean and SD of exposures and neighborhood variables, at each patient's year of FFS entry
for (cohort in c("Full", "PD", "PD_no")){
  for (var in c(exposure_name, exposure_name_yest, temp_same[1], temp_prior[1], rh_same[1], rh_prior[1], zip_quant_var_names)){
    cat(paste(var,
              paste0(cohort, "Cohort"),
              paste0(round(mean(entry_year_cohorts[[cohort]][[var]], na.rm = T), 3),
                     " (", round(sd(entry_year_cohorts[[cohort]][[var]], na.rm = T), 3), ")"),
              sep = ","),
        sep = "\n",
        file = paste0(dir_results, "table1/tab1.txt"),
        append = TRUE)
  }
}


# read table 1
tab <- fread(paste0(dir_results, "table1/tab1.txt"), fill = T)
