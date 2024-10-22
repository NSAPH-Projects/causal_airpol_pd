library(data.table)
library(tidyverse)

# get directories and classifications of variables
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

# read in regression results (coefficient for exposure)
a1_same <- cbind(fread(paste0(dir_results, "naive_approach/estimates_same.txt")), approach="Prior-year exposure not adjusted") %>%
  filter(Trim=="Trimmed")

# read in bootstraps
a1_same_se <- cbind(fread(paste0(dir_results, "naive_approach/boot_SE_same.txt")), approach="Prior-year exposure not adjusted") %>%
  filter(Trim=="Trimmed") %>%
  select(-c("it"))

boot_se <- a1_same_se %>%
  group_by(Exposure) %>%
  summarise(var_boot=var(BootSE)) %>%
  mutate(fac=ifelse(Exposure=="pm25.current_year", 369/34052, 
                    ifelse(Exposure=="no2.current_year", 372/34652, 370/34274)),
         se=sqrt(var_boot*fac)) 

# iqr pm25 current 3.71688; no2 current 13.83793; ozone summer current 10.09139
coef <- a1_same %>%
  mutate(iqr=ifelse(Exposure =="pm25.current_year", 3.71688,
                    ifelse(Exposure =="no2.current_year", 13.83793,
                           ifelse(Exposure =="ozone_summer.current_year", 10.09139, NA))),
         irr=exp(Coefficient*iqr)) %>%
  distinct()

t <- coef %>%
  left_join(boot_se) %>%
  mutate(lci=exp(iqr*(Coefficient-1.96*se)),
         uci=exp(iqr*(Coefficient+1.96*se)),
         Exposure=recode(Exposure,
                         "pm25.current_year" = "PM2.5",
                         "no2.current_year" = "NO2",
                         "ozone_summer.current_year" = "O3"),
         Exposure=factor(Exposure, levels=c("PM2.5", "NO2", "O3"))) %>%
  select(Exposure, irr, lci, uci)

t <- t %>% 
  arrange(Exposure) %>% 
  mutate(tog=paste(sprintf("%.2f",round(irr, 2)), 
                   " (", sprintf("%.2f",round(lci, 2)), ", ", 
                   sprintf("%.2f",round(uci, 2)), ")", sep = ""))
