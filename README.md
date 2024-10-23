# The Effect of Air Pollution on Hospitalizations with Parkinson’s Disease among Medicare Beneficiaries Nationwide
## by Veronica A. Wang1, Scott Delaney1, Lauren E. Flynn2, Brad A. Racette3,4, Gary W. Miller5, Danielle Braun6,7, Antonella Zanobetti1, Daniel Mork6

1Department of Environmental Health, Harvard T.H. Chan School of Public Health, Boston, MA, USA
2Division of Pulmonary Medicine, Boston Children’s Hospital, Boston, MA, USA
3Barrow Neurological Institute, Phoenix, AZ, USA
4Faculty of Health Sciences, University of the Witwatersrand, Johannesburg, South Africa 
5Department of Environmental Health Sciences, Mailman School of Public Health, Columbia University, New York NY, USA
6Department of Biostatistics, Harvard T.H. Chan School of Public Health, Boston, MA, USA
7Department of Data Science, Dana Farber Cancer Institute, Boston, MA, USA

### Aim
Examine the effect of annual exposure to fine particulate matter (PM2.5), nitrogen dioxide (NO2), and ozone (O3), on the rate of first hospitalization with a PD-related diagnosis (hospitalization with PD) among Medicare Fee-for-Service beneficiaries (2001-2016)

### Methods
Fitted four models: 1) traditional outcome stratification, 2) marginal structural, 3) doubly robust, and 4) generalized propensity score (GPS) matching Poisson regression models, adjusted for sociodemographic and meteorological confounders and long-term trends

### File descriptions:

Note that files that begin with (1) are for data processing and formatting; (2) is for initializing files for restuls; (3) table and figure generation

#### 1_1_Medicare FFS enrollement.R
Reads in denominator files and merges in RTI race values

#### 1_2_Merge PD hospitalization data.R
Reads in PD hospitalization data, restricts to first PD hospitlaization, assigns region based on residential state

#### 1_3_Aggregate data sets.R
Aggregates PD data by zip, year, strata

#### 1_4_confounders.R
Reads in zipcode covariates and temperature and creates indicator for trimming; merge everything together

#### 2_0_setup_process_res.R
Initialize documents to store estimates and bootstrapped estimates

#### 3_1_table1.R
Creates Table 1 (overall characteristics)

#### 3_2_pseudo_cor_fig1final.spin.R
Creates Figure 1 (exposure-confounder correlation in pseudopopulations)

#### 3_3_process_results_fig2final.R
Creates Figure 2 (main results)

#### 3_4_process_results_low_tableS2final.R
Creates Figure S2 (same as Figure 2 but among zips and years that are under air pollution policy standards)

#### 3_5_process_results_match_tableS3final.R
Creates Figure S3 (GPS matching results in trimmed dataset)


#### 'analyses' folder
##### setup_bootstrap_samples.R
Create fst files with bootstrapped samples for efficiency in full dataset

##### setup_bootstrap_samples_low.R
Create fst files with bootstrapped samples for efficiency in subset of zips and years that are under air pollution policy standards

##### setup_bootstrap_samples_match.R
Create fst files with bootstrapped samples for efficiency in trimmed dataset for GPS matching (supplemental materials)

##### test_one_gps_boot_pm.R
Main analyses [marginal structural, doubly robust, and GPS matching models in full dataset]: Creates GPS and fits marginal structural model (method 2) and doubly robust (method 4); creates matching counter weight and fits GPS matching model (method 3); change index to change air pollution exposure (1 is pm2.5; 2 is no2; 3 is o3)

##### test_one_gps_boot_pm_low.R
Same as 'test_one_gps_boot_pm.R' but among zips and years that are under air pollution policy standards

##### test_one_gps_match_boot_pm.R
Same as 'test_one_gps_boot_pm.R' but among trimmed datatset for GPS matching only (supplemental materials)

##### test_tradiational_model_boot_pm_low.R
Same as 'test_tradiational_model_boot_pm_new.R' but among zips and years that are under air pollution policy standards

##### test_tradiational_model_boot_pm_new.R
Main analyses [traditional outcome stratification model] (method 1); change index to change air pollution exposure (1 is pm2.5; 2 is no2; 3 is o3)

