## Imports ----
library(here)
library(rstan)
library(brms)
library(fs)
library(tidyverse)

## Data ----
data_df  <- readRDS(here("data_working" , "analytical_df.RDS"))

## Fix zero outcomes ----
## NOTE: negbin is a log-linear model so we cannot have a zero outcome. We add
## one to the outcome and then add one to the offset as well (using log1p()).
data_df <- data_df %>%
    mutate(missing_grps_p1 = missing_grps + 1)

## Note that for the analysis, we drop the first and last days since it will
## never be possible to have all expected data on those days.
accel_df <- data_df %>%
    filter(sensor == "accel") %>%
    group_by(uid) %>%
    filter(day_in_study > min(day_in_study),
           day_in_study < max(day_in_study)) %>%
    ungroup()

gps_df <- data_df %>%
    filter(sensor == "gps") %>%
    group_by(uid) %>%
    filter(day_in_study > min(day_in_study),
           day_in_study < max(day_in_study)) %>%
    ungroup()

## Formulas ----
##  Nested in users with no phone indicator
f_m1u_dem <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + week_in_study + 
    (1 | uid)

## Android models ----
if (!file_exists(here("model_objects", "sensitivity_android_gps_m1u_dem.RDS"))) {
    gps_m1u_dem <- brms::brm(
        f_m1u_dem,
        family = brms::negbinomial(),
        data = gps_df %>% filter(iphone == 0),
        iter = 10000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m1u_dem <- add_criterion(
        gps_m1u_dem, 
        model_name = "android_gps_m1u_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m1u_dem,
            "./model_objects/sensitivity_android_gps_m1u_dem.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "sensitivity_android_accel_m1u_dem.RDS"))) {
    accel_m1u_dem <- brms::brm(
        f_m1u_dem,
        family = brms::negbinomial(),
        data = accel_df %>% filter(iphone == 0),
        iter = 10000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m1u_dem <- add_criterion(
        accel_m1u_dem, 
        model_name = "android_accel_m1u_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m1u_dem,
            "./model_objects/sensitivity_android_accel_m1u_dem.RDS",
            compress = "xz")
}

## iPhone models ----
if (!file_exists(here("model_objects", "sensitivity_iphone_gps_m1u_dem.RDS"))) {
    gps_m1u_dem <- brms::brm(
        f_m1u_dem,
        family = brms::negbinomial(),
        data = gps_df %>% filter(iphone == 1),
        iter = 10000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m1u_dem <- add_criterion(
        gps_m1u_dem, 
        model_name = "iphone_gps_m1u_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m1u_dem,
            "./model_objects/sensitivity_iphone_gps_m1u_dem.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "sensitivity_iphone_accel_m1u_dem.RDS"))) {
    accel_m1u_dem <- brms::brm(
        f_m1u_dem,
        family = brms::negbinomial(),
        data = accel_df %>% filter(iphone == 1),
        iter = 10000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m1u_dem <- add_criterion(
        accel_m1u_dem, 
        model_name = "iphone_accel_m1u_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m1u_dem,
            "./model_objects/sensitivity_iphone_accel_m1u_dem.RDS",
            compress = "xz")
}
