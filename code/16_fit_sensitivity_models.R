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

## Sensitivity models using categorical regressions -----
if (!file_exists(here("model_objects", "sens_m2_dem_accel.RDS"))) {
    sens_m2_dem_accel <- brms::brm(
        formula = prop_cat ~ 1 + factor(sex, ordered = FALSE) +
            factor(edu_broad_cat, ordered = FALSE) +
            factor(race_cat, ordered = FALSE) +
            scaled_age + iphone +
            week_in_study +
            (1 | study_name / uid),
        data = accel_df,
        family = brms::categorical(),
        iter = 5000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    sens_m2_dem_accel <- add_criterion(
        sens_m2_dem_accel,
        model_name = "sens_m2_dem_accel",
        criterion = c("waic", "loo")
    )
    
    saveRDS(sens_m2_dem_accel,
            "./model_objects/sens_m2_dem_accel.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "sens_m2_dem_gps.RDS"))) {
    sens_m2_dem_gps <- brms::brm(
        formula = prop_cat ~ 1 + factor(sex, ordered = FALSE) +
            factor(edu_broad_cat, ordered = FALSE) +
            factor(race_cat, ordered = FALSE) +
            scaled_age + iphone +
            week_in_study +
            (1 | study_name / uid),
        data = gps_df,
        family = brms::categorical(),
        iter = 5000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    sens_m2_dem_gps <- add_criterion(
        sens_m2_dem_gps,
        model_name = "sens_m2_dem_gps",
        criterion = c("waic", "loo")
    )
    
    saveRDS(sens_m2_dem_gps,
            "./model_objects/sens_m2_dem_gps.RDS",
            compress = "xz")
}
