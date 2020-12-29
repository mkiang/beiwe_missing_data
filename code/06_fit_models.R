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

## Models ----
##  We run two sets of models (one on accel and one on GPS). The models are
##  named as [sensor]_m[levels]_[covariate type] where:
##      - [sensor] can be accel or gps, 
##      - [levels] represents nesting:
##          - 0 is non-hierarchical
##          - 1s is for study only
##          - 1u is for user only
##          - 2 for user in study
##      - [covariate type] can be:
##          - none (i.e., just time, iphone indicator and offset) 
##          - dem (full-case analysis with demographic covariates)
##          - dem_study (with study fixed effects). 
##  All modesl have time in study as a covariate.
##  
##  Primary models of interest are:
##      - accel_m2_dem
##      - gps_m2_dem
##  
##  All other models are secondary / sensitivity models. 
##  
##  NOTE: Because these are fully Bayesian models and can take a long time
##  to run (i.e., on the order of days), we check the existence of a saved
##  model first. 

## Formulas ----
##  Non-hierarchical models
f_m0_none <- missing_grps_p1 ~ 1 + offset(log1p(exp_grps)) + 
    week_in_study + iphone
f_m0_dem <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study
f_m0_dem_study <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study + 
    factor(study_code, ordered = FALSE)

##  Nested in study
f_m1s_dem <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study + 
    (1 | study_name)

##  Nested in users
f_m1u_dem <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study + 
    (1 | uid)
f_m1u_dem_study <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study + 
    factor(study_code, ordered = FALSE) + (1 | uid)

##  Nested users in user in study
f_m2_dem <- missing_grps_p1 ~  1 + offset(log1p(exp_grps)) +
    factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) + 
    factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study + 
    (1 | study_name / uid)

## Primary models ----
if (!file_exists(here("model_objects", "gps_m1u_dem.RDS"))) {
    gps_m1u_dem <- brms::brm(
        f_m1u_dem,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 10000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m1u_dem <- add_criterion(
        gps_m1u_dem, 
        model_name = "gps_m1u_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m1u_dem,
            "./model_objects/gps_m1u_dem.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "accel_m1u_dem.RDS"))) {
    accel_m1u_dem <- brms::brm(
        f_m1u_dem,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 10000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m1u_dem <- add_criterion(
        accel_m1u_dem, 
        model_name = "accel_m1u_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m1u_dem,
            "./model_objects/accel_m1u_dem.RDS",
            compress = "xz")
}

## Secondary models ----
## Accelerometer null models ----
if (!file_exists(here("model_objects", "accel_m0_none.RDS"))) {
    accel_m0_none <- brms::brm(
        formula = f_m0_none,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 1000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m0_none <- add_criterion(
        accel_m0_none, 
        model_name = "accel_m0_none",
        criterion = c("waic", "loo", "bayes_R2")
        )
    
    saveRDS(accel_m0_none,
            "./model_objects/accel_m0_none.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "accel_m0_dem.RDS"))) {
    accel_m0_dem <- brms::brm(
        formula = f_m0_dem,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 1000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m0_dem <- add_criterion(
        accel_m0_dem, 
        model_name = "accel_m0_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m0_dem,
            "./model_objects/accel_m0_dem.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "accel_m0_dem_study.RDS"))) {
    accel_m0_dem_study <- brms::brm(
        formula = f_m0_dem_study,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 1000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m0_dem_study <- add_criterion(
        accel_m0_dem_study, 
        model_name = "accel_m0_dem_study",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m0_dem_study,
            "./model_objects/accel_m0_dem_study.RDS",
            compress = "xz")
}

## Accelerometer two-level models ----
if (!file_exists(here("model_objects", "accel_m2_dem.RDS"))) {
    accel_m2_dem <- brms::brm(
        f_m2_dem,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 5000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .999,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m2_dem <- add_criterion(
        accel_m2_dem, 
        model_name = "accel_m2_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m2_dem,
            "./model_objects/accel_m2_dem.RDS",
            compress = "xz")
}

## User level ----
if (!file_exists(here("model_objects", "accel_m1u_dem_study.RDS"))) {
    accel_m1u_dem_study <- brms::brm(
        f_m1u_dem_study,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 5000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m1u_dem_study <- add_criterion(
        accel_m1u_dem_study, 
        model_name = "accel_m1u_dem_study",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m1u_dem_study,
            "./model_objects/accel_m1u_dem_study.RDS",
            compress = "xz")
}

## Study level ----
if (!file_exists(here("model_objects", "accel_m1s_dem.RDS"))) {
    accel_m1s_dem <- brms::brm(
        f_m1s_dem,
        family = brms::negbinomial(),
        data = accel_df,
        iter = 2500,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .999,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    accel_m1s_dem <- add_criterion(
        accel_m1s_dem, 
        model_name = "accel_m1s_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(accel_m1s_dem,
            "./model_objects/accel_m1s_dem.RDS",
            compress = "xz")
}

## GPS null models ----
if (!file_exists(here("model_objects", "gps_m0_none.RDS"))) {
    gps_m0_none <- brms::brm(
        formula = f_m0_none,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 1000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m0_none <- add_criterion(
        gps_m0_none, 
        model_name = "gps_m0_none",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m0_none,
            "./model_objects/gps_m0_none.RDS",
            compress = "xz")
}


if (!file_exists(here("model_objects", "gps_m0_dem.RDS"))) {
    gps_m0_dem <- brms::brm(
        formula = f_m0_dem,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 1000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m0_dem <- add_criterion(
        gps_m0_dem, 
        model_name = "gps_m0_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m0_dem,
            "./model_objects/gps_m0_dem.RDS",
            compress = "xz")
}

if (!file_exists(here("model_objects", "gps_m0_dem_study.RDS"))) {
    gps_m0_dem_study <- brms::brm(
        formula = f_m0_dem_study,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 1000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m0_dem_study <- add_criterion(
        gps_m0_dem_study, 
        model_name = "gps_m0_dem_study",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m0_dem_study,
            "./model_objects/gps_m0_dem_study.RDS",
            compress = "xz")
}


## GPS two-level models ----
if (!file_exists(here("model_objects", "gps_m2_dem.RDS"))) {
    gps_m2_dem <- brms::brm(
        f_m2_dem,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 7500,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .999,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m2_dem <- add_criterion(
        gps_m2_dem, 
        model_name = "gps_m2_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m2_dem,
            "./model_objects/gps_m2_dem.RDS",
            compress = "xz")
}

## User level ----
if (!file_exists(here("model_objects", "gps_m1u_dem_study.RDS"))) {
    gps_m1u_dem_study <- brms::brm(
        f_m1u_dem_study,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 5000,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .95,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m1u_dem_study <- add_criterion(
        gps_m1u_dem_study, 
        model_name = "gps_m1u_dem_study",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m1u_dem_study,
            "./model_objects/gps_m1u_dem_study.RDS",
            compress = "xz")
}

## Study level ----
if (!file_exists(here("model_objects", "gps_m1s_dem.RDS"))) {
    gps_m1s_dem <- brms::brm(
        f_m1s_dem,
        family = brms::negbinomial(),
        data = gps_df,
        iter = 2500,
        chains = 8,
        cores = 8,
        control = list(adapt_delta = .99,
                       max_treedepth = 20),
        save_all_pars = TRUE
    )
    
    gps_m1s_dem <- add_criterion(
        gps_m1s_dem, 
        model_name = "gps_m1s_dem",
        criterion = c("waic", "loo", "bayes_R2")
    )
    
    saveRDS(gps_m1s_dem,
            "./model_objects/gps_m1s_dem.RDS",
            compress = "xz")
}

## Notes about doing this in rstanarm ----
## Example code if we wanted to do this in rstanarm instead of brms
# if (!file_exists(here("model_objects", "accel_m0_dem_RSTANARM.RDS"))) {
#     accel_m0_dem <- stan_glm(
#         formula = missing_grps_p1 ~  1 +
#             factor(sex, ordered = FALSE) + factor(edu_broad_cat, ordered = FALSE) +
#             factor(race_cat, ordered = FALSE) + scaled_age + iphone + week_in_study,
#         family = neg_binomial_2(link = "log"),
#         data = accel_df,
#         iter = 5000,
#         chains = 8,
#         cores = 8,
#         control = list(adapt_delta = .95,
#                        max_treedepth = 20),
#         offset = log1p(exp_grps)
#     )
#     saveRDS(accel_m0_dem,
#             "./model_objects/accel_m0_dem_RSTANARM.RDS",
#             compress = "xz")
# }
# 
## brms will automatically convert multilevel models to a noncentered 
## parameterization — it is not clear to me this is the case for rstanarm. We 
## can always call back the original rstan object using model$fit so ultimately
## I don't think it makes a difference which one you want to use.
