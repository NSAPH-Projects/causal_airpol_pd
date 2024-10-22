library(data.table)
library(tidyverse)

# get directories and classifications of variables
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

# read in regression results (coefficient for exposure)
a1_same <- fread(paste0(dir_results, "naive_approach/estimates_same.txt")) %>%
  filter(Trim=="low")

# read in bootstraps
a1_same_se <- fread(paste0(dir_results, "naive_approach/boot_SE_same.txt"), fill=T)  %>%
  filter(Trim=="low")

boot_se <- a1_same_se %>%
  group_by(Exposure) %>%
  summarise(var_boot=var(BootSE)) %>%
  mutate(fac=ifelse(Exposure=="pm25.current_year", 361/32577, 
                    ifelse(Exposure=="no2.current_year", 373/34758, 373/34753)),
         se=sqrt(var_boot*fac)) 

# iqr pm25 current 3.71688; no2 current 13.83793; ozone summer current 10.09139
coef <- a1_same %>%
  mutate(iqr=ifelse(Exposure =="pm25.current_year", 2.222418,
                    ifelse(Exposure =="no2.current_year", 13.77943,
                           ifelse(Exposure =="ozone_summer.current_year", 10.02804, NA))),
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
         Exposure=factor(Exposure, levels=c("PM2.5", "NO2", "O3")),
         Method=recode(Method,
                       "method1" = "Traditional",
                       "method2" = "MSM",
                       "method4" = "DR"),
         Method=factor(Method, levels=c("Traditional", "MSM", "DR"))) %>%
  select(Exposure, Method, irr, lci, uci)


t <- t %>% 
  arrange(Exposure) %>% 
  mutate(tog=paste(sprintf("%.2f",round(irr, 2)), 
                   " (", sprintf("%.2f",round(lci, 2)), ", ", 
                   sprintf("%.2f",round(uci, 2)), ")", sep = ""))
