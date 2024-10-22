# constants

### NOTE: User should update directory paths for data, code, and results ###

# directories for code and results to be saved (separate from data directory)
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
dir_results <- "~/nsaph_projects/vwang_causal_airpol_pd/results/"

# directory for cleaned, processed, and aggregated data (PRIVATE CMS DATA, SHOULD NOT BE EXPORTED)
dir_data <- "~/nsaph_projects/vwang_causal_airpol_pd/data/"

# directories for private raw data (PRIVATE CMS DATA, SHOULD NOT BE EXPORTED)
dir_denominator <- "/n/dominici_nsaph_l3/Lab/projects/analytic/denom_by_year/"
dir_hosp <- "/n/dominici_nsaph_l3/Lab/projects/analytic/parkinsons_disease/"

# directories for public raw data
dir_pm25 <- "/n/dominici_lab_ro/lab/data/exposures/exposure/"
dir_no2 <- "/nfs/home/D/dam9096/shared_space/ci3_exposure/no2/whole_us/annual/zipcode/qd_predictions_ensemble/ywei_aggregations/"
dir_ozone <- "/n/dominici_lab_ro/lab/data/exposures/exposure/"
dir_temp <- "/n/dominici_nsaph_l3/Lab/projects/analytic/temperature_seasonal_zipcode/temperature_seasonal_zipcode_combined.csv"
dir_confounders <- "/n/dominici_nsaph_l3/Lab/projects/analytic/confounders/"


### Variables and formulas for this analysis (after the data have been cleaned, processed, and aggregated) ###

# Classify variables in dataset

offset_var_names <- c("n_persons", "n_years")
exposure_name <- c("pm25.current_year", "no2.current_year", "ozone_summer.current_year")
exposure_name_yest <- c("pm25.one_year_lag", "no2.one_year_lag", "ozone_summer.one_year_lag")
zip_quant_var_names <- c("mean_bmi", "smoke_rate", "hispanic", "pct_blk",
                         "pir", "poverty", "education", "popdensity", "pct_owner_occ")
zip_quant_var_names_sq <- c("mean_bmi_sq", "smoke_rate_sq", "hispanic_sq", "pct_blk_sq",
                         "pir_sq", "poverty_sq", "education_sq", "popdensity_sq", "pct_owner_occ_sq")
temp_same <- c("summer_tmmx", "summer_tmmx_sq")
temp_prior <- c("summer_tmmx_yest", "summer_tmmx_yest_sq")
rh_same <- c("summer_rmax", "summer_rmax_sq")
rh_prior <- c("summer_rmax_yest", "summer_rmax_yest_sq")
zip_unordered_cat_var_names <- c("region")
strata_vars <- c("year", "sex", "race", "dual", "age_grp") # include year to account for possible confounding, such as billing incentives changing year to year
zip_vars <- c(zip_quant_var_names, zip_quant_var_names_sq, zip_unordered_cat_var_names)


indiv_quant_var_names <- NULL
indiv_unordered_cat_var_names <- c("sex", "race", "dual", "age_grp", "year", "region") # note that age_grp here is an unordered categorical variable
zip_var_names <- c(zip_quant_var_names, zip_unordered_cat_var_names)
indiv_var_names <- c(indiv_unordered_cat_var_names, indiv_quant_var_names)

# matching function
match_within_yr <- function(dat,
                            gps_mx,
                            w_mx,
                            expo) {
  df_cgps_gps <- list()
  class(df_cgps_gps) <- "cgps_gps"
  df_cgps_gps$dataset <- as.data.frame(dat)
  df_cgps_gps$e_gps_pred <- dat$e_gps_pred
  df_cgps_gps$w_resid <- dat$w_resid
  df_cgps_gps$e_gps_std_pred <- dat$e_gps_std_pred
  df_cgps_gps$gps_mx <- gps_mx
  df_cgps_gps$w_mx <- w_mx
  
  match_pop <- compile_pseudo_pop(data_obj = df_cgps_gps,
                                  ci_appr = "matching",
                                  gps_density = "normal",
                                  exposure_col_name = c(expo),
                                  dist_measure="l1",
                                  covar_bl_method="absolute",
                                  covar_bl_trs=0.1,
                                  covar_bl_trs_type="mean",
                                  delta_n=cap,
                                  scale=1,
                                  bin_seq = NULL,
                                  nthread = 4)
  
  return(match_pop)
}