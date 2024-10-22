library(data.table)
library(tidyverse)
library(ggh4x)

# get directories and classifications of variables
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/git/"
source(paste0(dir_code, "constants.R"))

# read in cor coef in pseudo pop
## unadj
cor_a1_pm <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_unadj_Untrimmed.rds")),
                pol="pm25", approach="naive", adj="no") %>%
  mutate(meth="m2")
cor_a1_no2 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_unadj_Untrimmed.rds")),
                pol="no2", approach="naive", adj="no") %>%
  mutate(meth="m2")
cor_a1_o3 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_unadj_Untrimmed.rds")),
                pol="o3", approach="naive", adj="no") %>%
  mutate(meth="m2")

cor_a1_pm3 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_unadj_Untrimmed.rds")),
                   pol="pm25", approach="naive", adj="no") %>%
  mutate(meth="m3")
cor_a1_no23 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_unadj_Untrimmed.rds")),
                    pol="no2", approach="naive", adj="no") %>%
  mutate(meth="m3")
cor_a1_o33 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_unadj_Untrimmed.rds")),
                   pol="o3", approach="naive", adj="no") %>%
  mutate(meth="m3")

## naive
same_pm_m2_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_method2_Untrimmed.rds")), 
              pol="pm25", per="same", approach="naive", meth="m2", adj="yes")
same_pm_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_method3_Untrimmed.rds")), 
                    pol="pm25", per="same", approach="naive", meth="m3", adj="yes")

same_no2_m2_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_method2_Untrimmed.rds")), 
                    pol="no2", per="same", approach="naive", meth="m2", adj="yes")
same_no2_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_method3_Untrimmed.rds")), 
                    pol="no2", per="same", approach="naive", meth="m3", adj="yes")

same_o3_m2_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_method2_Untrimmed.rds")), 
                    pol="o3", per="same", approach="naive", meth="m2", adj="yes")
same_o3_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_method3_Untrimmed.rds")), 
                    pol="o3", per="same", approach="naive", meth="m3", adj="yes")

coef <- rbind(same_pm_m2_a1, same_pm_m3_a1, same_no2_m2_a1, same_no2_m3_a1, 
              same_o3_m2_a1, same_o3_m3_a1)
coef_un <- rbind(cor_a1_pm, cor_a1_no2, cor_a1_o3, 
              cor_a1_pm3, cor_a1_no23, cor_a1_o33) %>%
  filter(per=="same")

# iqr pm25 current 3.71688; no2 current 13.83793; ozone summer current 10.09139
full <- coef %>%
  filter(type=="done") %>%
  select(var, spearman_cor, pol, approach, meth, adj)
full_un <- coef_un %>% select(var, spearman_cor, pol, approach, meth, adj)
full <- rbind(full, full_un) 

t_un <- full %>%
  group_by(pol, approach, meth, adj) %>%
  summarise(avg_cor=round(mean(spearman_cor), 2))


test <- full %>%
  left_join(t_un, by=c("pol", "approach", "meth", "adj")) %>%
  mutate(#avg_cor_should=ifelse(adj=="no" & approach=="naive", avg_cor_un_should, avg_cor_should),
         pol=factor(pol, levels=c("pm25", "no2", "o3")),
         approach=factor(approach, levels=c("naive", "mutual")),
         var=factor(var, levels=c("pct_owner_occ", "popdensity", "education",
                                  "poverty", "pir", "pct_blk", "hispanic",
                                  "smoke_rate", "mean_bmi", "summer_rmax", "summer_tmmx",
                                  "region","ozone_summer.one_year_lag", "no2.one_year_lag", 
                                  "pm25.one_year_lag")),
         var=recode(var,
                    "mean_bmi"="BMI",
                    "smoke_rate" = "% Ever smokers",
                    "hispanic" = "% Hispanic",
                    "pct_blk" = "% Black",
                    "pir" = "PIR",
                    "poverty" = "% Below poverty line",
                    "education" = "% Without high school degree",
                    "popdensity" = "Population density",
                    "pct_owner_occ" = "% Owner-occupied housing",
                    "summer_tmmx" = "Temperature",
                    "summer_rmax" = "Relative humidity",
                    "region" = "US region",
                    "pm25.one_year_lag" = "Prior-year PM2.5",
                    "no2.one_year_lag" = "Prior-year NO2",
                    "ozone_summer.one_year_lag" = "Prior-year O3"),
         pol=recode(pol,
                    "pm25" = "PM2.5",
                    "no2" = "NO2",
                    "o3" = "O3"),
         approach=recode(approach,
                         "naive" = "Prior-year exposure not adjusted",
                         "mutual" = "Prior-year exposure adjusted"),
         meth=recode(meth,
                     "m2" = "MSM",
                     "m3" = "GPS matching"),
         meth=factor(meth, levels=c("MSM", "GPS matching")),
         adj=recode(adj,
                    "no" = "Unadjusted",
                    "yes" = "Adjusted"))

test %>%
  ggplot(aes(x=var, y=spearman_cor, shape=adj)) +
  geom_hline(aes(yintercept=0.1), color="grey") +
  geom_hline(aes(yintercept=avg_cor, linetype=adj), color="black") +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  facet_nested(pol ~ meth, scales = "free_y") +
  coord_flip() +
  scale_shape_manual(values = c(16, 1)) +
  ylab("Spearman correlation") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

################################################
## for suppl gps matching trimmed
################################################

## unadj
cor_a1_pm3 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_unadj_Untrimmed.rds")),
                    pol="pm25", approach="naive", adj="no") %>%
  mutate(meth="m3")
cor_a1_no23 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_unadj_Untrimmed.rds")),
                     pol="no2", approach="naive", adj="no") %>%
  mutate(meth="m3")
cor_a1_o33 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_unadj_Untrimmed.rds")),
                    pol="o3", approach="naive", adj="no") %>%
  mutate(meth="m3")

## naive
same_pm_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_method3_Trimmed.rds")), 
                       pol="pm25", per="same", approach="naive", meth="m3", adj="yes")
same_no2_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_method3_Trimmed.rds")), 
                        pol="no2", per="same", approach="naive", meth="m3", adj="yes")
same_o3_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_method3_Trimmed.rds")), 
                       pol="o3", per="same", approach="naive", meth="m3", adj="yes")

coef <- rbind(same_pm_m3_a1, same_no2_m3_a1, same_o3_m3_a1)
coef_un <- rbind(cor_a1_pm3, cor_a1_no23, cor_a1_o33) %>%
  filter(per=="same")

# iqr pm25 current 3.71688; no2 current 13.83793; ozone summer current 10.09139
full <- coef %>%
  select(var, spearman_cor, pol, approach, meth, adj)
full_un <- coef_un %>% select(var, spearman_cor, pol, approach, meth, adj)
full <- rbind(full, full_un) 

t_un <- full %>%
  group_by(pol, approach, meth, adj) %>%
  summarise(avg_cor=round(mean(spearman_cor), 2))


test <- full %>%
  left_join(t_un, by=c("pol", "approach", "meth", "adj")) %>%
  mutate(#avg_cor_should=ifelse(adj=="no" & approach=="naive", avg_cor_un_should, avg_cor_should),
    pol=factor(pol, levels=c("pm25", "no2", "o3")),
    approach=factor(approach, levels=c("naive", "mutual")),
    var=factor(var, levels=c("pct_owner_occ", "popdensity", "education",
                             "poverty", "pir", "pct_blk", "hispanic",
                             "smoke_rate", "mean_bmi", "summer_rmax", "summer_tmmx",
                             "region","ozone_summer.one_year_lag", "no2.one_year_lag", 
                             "pm25.one_year_lag")),
    var=recode(var,
               "mean_bmi"="BMI",
               "smoke_rate" = "% Ever smokers",
               "hispanic" = "% Hispanic",
               "pct_blk" = "% Black",
               "pir" = "PIR",
               "poverty" = "% Below poverty line",
               "education" = "% Without high school degree",
               "popdensity" = "Population density",
               "pct_owner_occ" = "% Owner-occupied housing",
               "summer_tmmx" = "Temperature",
               "summer_rmax" = "Relative humidity",
               "region" = "US region",
               "pm25.one_year_lag" = "Prior-year PM2.5",
               "no2.one_year_lag" = "Prior-year NO2",
               "ozone_summer.one_year_lag" = "Prior-year O3"),
    pol=recode(pol,
               "pm25" = "PM2.5",
               "no2" = "NO2",
               "o3" = "O3"),
    approach=recode(approach,
                    "naive" = "Prior-year exposure not adjusted",
                    "mutual" = "Prior-year exposure adjusted"),
    meth=recode(meth,
                "m2" = "MSM",
                "m3" = "GPS matching"),
    meth=factor(meth, levels=c("MSM", "GPS matching")),
    adj=recode(adj,
               "no" = "Unadjusted",
               "yes" = "Adjusted"))

test %>%
  ggplot(aes(x=var, y=spearman_cor, shape=adj)) +
  geom_hline(aes(yintercept=0.1), color="grey") +
  geom_hline(aes(yintercept=avg_cor, linetype=adj), color="black") +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  facet_nested(pol ~ meth, scales = "free_y") +
  coord_flip() +
  scale_shape_manual(values = c(16, 1)) +
  ylab("Spearman correlation") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

################################################
## for suppl low pollution
################################################

## unadj
cor_a1_pm3 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_unadj_low.rds")),
                    pol="pm25", approach="naive", adj="no") %>%
  mutate(meth="m2")
cor_a1_no23 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_unadj_low.rds")),
                     pol="no2", approach="naive", adj="no") %>%
  mutate(meth="m2")
cor_a1_o33 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_unadj_low.rds")),
                    pol="o3", approach="naive", adj="no") %>%
  mutate(meth="m2")

## naive
same_pm_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_pm25.current_year_method2_low.rds")), 
                       pol="pm25", per="same", approach="naive", meth="m2", adj="yes")
same_no2_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_no2.current_year_method2_low.rds")), 
                        pol="no2", per="same", approach="naive", meth="m2", adj="yes")
same_o3_m3_a1 <- cbind(readRDS(paste0(dir_results, "naive_approach/cor_ozone_summer.current_year_method2_low.rds")), 
                       pol="o3", per="same", approach="naive", meth="m2", adj="yes")

coef <- rbind(same_pm_m3_a1, same_no2_m3_a1, same_o3_m3_a1)
coef_un <- rbind(cor_a1_pm3, cor_a1_no23, cor_a1_o33) 

# iqr pm25 current 3.71688; no2 current 13.83793; ozone summer current 10.09139
full <- coef %>%
  select(var, spearman_cor, pol, approach, meth, adj)
full_un <- coef_un %>% select(var, spearman_cor, pol, approach, meth, adj)
full <- rbind(full, full_un) 

t_un <- full %>%
  group_by(pol, approach, meth, adj) %>%
  summarise(avg_cor=round(mean(spearman_cor), 2))


test <- full %>%
  left_join(t_un, by=c("pol", "approach", "meth", "adj")) %>%
  mutate(#avg_cor_should=ifelse(adj=="no" & approach=="naive", avg_cor_un_should, avg_cor_should),
    pol=factor(pol, levels=c("pm25", "no2", "o3")),
    approach=factor(approach, levels=c("naive", "mutual")),
    var=factor(var, levels=c("pct_owner_occ", "popdensity", "education",
                             "poverty", "pir", "pct_blk", "hispanic",
                             "smoke_rate", "mean_bmi", "summer_rmax", "summer_tmmx",
                             "region","ozone_summer.one_year_lag", "no2.one_year_lag", 
                             "pm25.one_year_lag")),
    var=recode(var,
               "mean_bmi"="BMI",
               "smoke_rate" = "% Ever smokers",
               "hispanic" = "% Hispanic",
               "pct_blk" = "% Black",
               "pir" = "PIR",
               "poverty" = "% Below poverty line",
               "education" = "% Without high school degree",
               "popdensity" = "Population density",
               "pct_owner_occ" = "% Owner-occupied housing",
               "summer_tmmx" = "Temperature",
               "summer_rmax" = "Relative humidity",
               "region" = "US region",
               "pm25.one_year_lag" = "Prior-year PM2.5",
               "no2.one_year_lag" = "Prior-year NO2",
               "ozone_summer.one_year_lag" = "Prior-year O3"),
    pol=recode(pol,
               "pm25" = "PM2.5",
               "no2" = "NO2",
               "o3" = "O3"),
    approach=recode(approach,
                    "naive" = "Prior-year exposure not adjusted",
                    "mutual" = "Prior-year exposure adjusted"),
    meth=recode(meth,
                "m2" = "MSM",
                "m3" = "GPS matching"),
    meth=factor(meth, levels=c("MSM", "GPS matching")),
    adj=recode(adj,
               "no" = "Unadjusted",
               "yes" = "Adjusted"))

test %>%
  ggplot(aes(x=var, y=spearman_cor, shape=adj)) +
  geom_hline(aes(yintercept=0.1), color="grey") +
  geom_hline(aes(yintercept=avg_cor, linetype=adj), color="black") +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  facet_nested(pol ~ meth, scales = "free_y") +
  coord_flip() +
  scale_shape_manual(values = c(16, 1)) +
  ylab("Spearman correlation") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
