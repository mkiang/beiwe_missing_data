## Imports ----
library(tidyverse)
library(here)
library(brms)

## Read in primary models ----
m_accel <- readRDS(here("model_objects", "accel_m1u_dem.RDS"))
m_gps <- readRDS(here("model_objects", "gps_m1u_dem.RDS"))

## Fixed effects ----
table3_estimates <- bind_rows(
    tibble(
        variable = rownames(fixef(m_accel)),
        beta_estimate = unname(fixef(m_accel)[, 1]),
        sd_estimate = unname(fixef(m_accel)[, 2]),
        lower_ci = unname(fixef(m_accel)[, 3]),
        upper_ci = unname(fixef(m_accel)[, 4])
    ) %>%
        mutate(sensor = "accel"),
    tibble(
        variable = rownames(fixef(m_gps)),
        beta_estimate = unname(fixef(m_gps)[, 1]),
        sd_estimate = unname(fixef(m_gps)[, 2]),
        lower_ci = unname(fixef(m_gps)[, 3]),
        upper_ci = unname(fixef(m_gps)[, 4])
    ) %>%
        mutate(sensor = "gps")
) 

## Add in better variable names
table3_estimates <- table3_estimates %>%
    mutate(variable_cat = factor(
        variable,
        levels = c(
            "Intercept",
            "week_in_study",
            "iphone",
            "factorsexorderedEQFALSEMale",
            "factoredu_broad_catorderedEQFALSE>EQ4yearscollege",
            "factorrace_catorderedEQFALSEBlack",
            "factorrace_catorderedEQFALSEAsian",
            "factorrace_catorderedEQFALSEOtherDMultiple",
            "factorrace_catorderedEQFALSEAmericanIndian",
            "scaled_age"
        ),
        labels = c(
            "Intercept",
            "Time (weeks)",
            "iOS device",
            "Male",
            "4-year degree or higher",
            "Black",
            "Asian",
            "American Indian",
            "Other/Multiple",
            "Age (10 years)"
        ),
        ordered = TRUE
    )) %>%
    arrange(sensor, variable_cat) %>%
    select(sensor, variable, variable_cat, everything())

table3_estimates <- table3_estimates %>%
    left_join(
        table3_estimates %>% transmute(
            sensor,
            variable_cat,
            beta_ci = sprintf(
                "%0.3f (%0.3f, %0.3f)",
                round(exp(beta_estimate), 3),
                round(exp(lower_ci), 3),
                round(exp(upper_ci), 3)
            ),
            round_sd = sprintf("%0.3f", round(sd_estimate, 3))
        )
    )

write_csv(table3_estimates,
          "./output/table2_estimates.csv")
saveRDS(table3_estimates, "./output/model_coefs.RDS")

summary(m_accel)$random
summary(m_gps)$random

summary(m_accel)$nobs
summary(m_gps)$nobs

loo(m_accel)
loo(m_gps)

waic(m_accel)
waic(m_gps)

bayes_R2(m_accel)
bayes_R2(m_gps)
