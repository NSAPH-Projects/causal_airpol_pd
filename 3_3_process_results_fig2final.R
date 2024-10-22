library(data.table)
library(tidyverse)

# get directories and classifications of variables
dir_code <- "~/nsaph_projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

# read in regression results (coefficient for exposure)
a1_same <- cbind(fread(paste0(dir_results, "naive_approach/estimates_same.txt")), approach="Prior-year exposure not adjusted") %>%
  filter(Method %in% c("method1", "method2", "method4"))

# read in bootstraps
a1_same_se <- cbind(fread(paste0(dir_results, "naive_approach/boot_SE_same.txt")), approach="Prior-year exposure not adjusted") %>%
  filter(Method %in% c("method1", "method2", "method4")) %>%
  select(-c("it"))

boot_se <- a1_same_se %>%
  mutate(Method=ifelse(Method=="method1_new", "method1", Method),
         Method=ifelse(Method=="method2_new", "method2", Method),
         Method=ifelse(Method=="method4_new", "method4", Method)) %>%
  group_by(Exposure, Method, approach) %>%
  summarise(se=sqrt(var(BootSE)*(373 / 34761)))

# iqr pm25 current 3.71688; no2 current 13.83793; ozone summer current 10.09139
coef <- a1_same %>%
  mutate(Method=ifelse(Method=="method1_new", "method1", Method),
         Method=ifelse(Method=="method2_new", "method2", Method),
         Method=ifelse(Method=="method4_new", "method4", Method)) %>%
  mutate(iqr=ifelse(Exposure %in% c("pm25.current_year", "pm25.one_year_lag"), 3.71688,
                    ifelse(Exposure %in% c("no2.current_year", "no2.one_year_lag"), 13.83793,
                           ifelse(Exposure %in% c("ozone_summer.current_year", "ozone_summer.one_year_lag"), 10.09139, NA))),
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
  select(Exposure, Method, irr, lci, uci, Trim)


t %>%
  filter(Trim=="Untrimmed") %>%
  ggplot(aes(x=Exposure, y=irr, group=Method)) +
  geom_errorbar(aes(ymin=lci, ymax=uci), size = 1, width=0, position = position_dodge(width=0.5)) +
  geom_point(aes(shape=Method), size=3.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(limits = c(0.90, 1.20)) +
  geom_hline(yintercept = 1, size = 1, linetype="dashed", color="black") +
  scale_shape_manual(values = c(16, 8 , 17, 10)) +
  labs(y="IRR of hospitalizations with PD\nper IQR increase in air pollution") +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(breaks = c("PM2.5", "NO2", "O3"),
                   labels = parse(text = c("PM[2.5]", "NO[2]", "O[3]")))

t <- t %>% 
  filter(approach == "Prior-year exposure not adjusted") %>%
  arrange(Exposure, approach, Method) %>% 
  mutate(tog=paste(sprintf("%.2f",round(irr, 2)), 
                   " (", sprintf("%.2f",round(lci, 2)), ", ", 
                   sprintf("%.2f",round(uci, 2)), ")", sep = ""))
