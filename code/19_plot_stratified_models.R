## Imports ----
library(tidyverse)
library(here)
library(brms)
source("./code/mk_nytimes.R")

## Read in primary models ----
m_accel <- readRDS(here("model_objects", "accel_m1u_dem.RDS"))
m_gps <- readRDS(here("model_objects", "gps_m1u_dem.RDS"))

## Read in stratified models ----
iphone_accel <- readRDS(here("model_objects", "sensitivity_iphone_accel_m1u_dem.RDS"))
android_accel <- readRDS(here("model_objects", "sensitivity_android_accel_m1u_dem.RDS"))
iphone_gps <- readRDS(here("model_objects", "sensitivity_iphone_gps_m1u_dem.RDS"))
android_gps <- readRDS(here("model_objects", "sensitivity_android_gps_m1u_dem.RDS"))

## Fixed effects ----
model_fixed_ests <- bind_rows(
    tibble(
        variable = rownames(fixef(m_accel)),
        beta_estimate = unname(fixef(m_accel)[, 1]),
        sd_estimate = unname(fixef(m_accel)[, 2]),
        lower_ci = unname(fixef(m_accel)[, 3]),
        upper_ci = unname(fixef(m_accel)[, 4])
    ) %>%
        mutate(sensor = "accel",
               phone_type = "both"),
    tibble(
        variable = rownames(fixef(m_gps)),
        beta_estimate = unname(fixef(m_gps)[, 1]),
        sd_estimate = unname(fixef(m_gps)[, 2]),
        lower_ci = unname(fixef(m_gps)[, 3]),
        upper_ci = unname(fixef(m_gps)[, 4])
    ) %>%
        mutate(sensor = "gps",
               phone_type = "both")
) %>%
    bind_rows(
        tibble(
            variable = rownames(fixef(iphone_accel)),
            beta_estimate = unname(fixef(iphone_accel)[, 1]),
            sd_estimate = unname(fixef(iphone_accel)[, 2]),
            lower_ci = unname(fixef(iphone_accel)[, 3]),
            upper_ci = unname(fixef(iphone_accel)[, 4])
        ) %>%
            mutate(sensor = "accel",
                   phone_type = "iphone"),
        tibble(
            variable = rownames(fixef(android_accel)),
            beta_estimate = unname(fixef(android_accel)[, 1]),
            sd_estimate = unname(fixef(android_accel)[, 2]),
            lower_ci = unname(fixef(android_accel)[, 3]),
            upper_ci = unname(fixef(android_accel)[, 4])
        ) %>%
            mutate(sensor = "accel",
                   phone_type = "android")
    )  %>%
    bind_rows(
        tibble(
            variable = rownames(fixef(iphone_gps)),
            beta_estimate = unname(fixef(iphone_gps)[, 1]),
            sd_estimate = unname(fixef(iphone_gps)[, 2]),
            lower_ci = unname(fixef(iphone_gps)[, 3]),
            upper_ci = unname(fixef(iphone_gps)[, 4])
        ) %>%
            mutate(sensor = "gps",
                   phone_type = "iphone"),
        tibble(
            variable = rownames(fixef(android_gps)),
            beta_estimate = unname(fixef(android_gps)[, 1]),
            sd_estimate = unname(fixef(android_gps)[, 2]),
            lower_ci = unname(fixef(android_gps)[, 3]),
            upper_ci = unname(fixef(android_gps)[, 4])
        ) %>%
            mutate(sensor = "gps",
                   phone_type = "android")
    ) %>% 
    mutate(
        sensor_cat = factor(
            sensor,
            levels = rev(c("accel", "gps")),
            labels = rev(c("Accelerometer", "GPS")),
            ordered = TRUE
        ),
        phone_cat = factor(
            phone_type,
            levels = c("android", "iphone", "both"),
            labels = c("Android only", "iOS only", "iOS and Android"),
            ordered = TRUE
        ),
        variable_cat = factor(
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
        )
    )

p1 <- ggplot(model_fixed_ests,
       aes(
           x = reorder(variable_cat, desc(variable_cat)),
           color = phone_cat,
           y = exp(beta_estimate),
           ymin = exp(lower_ci),
           ymax = exp(upper_ci)
       )) +
    geom_hline(
        yintercept = c(1/20, 1 / 10, 1 / 4, 4, 10),
        linetype = "dotted",
        alpha = .7,
        color = "black"
    ) +
    geom_hline(
        yintercept = 1,
        linetype = "solid",
        alpha = .7,
        color = "black"
    ) +
    geom_point(position = position_dodge2(width = .5)) +
    geom_linerange(position = position_dodge2(width = .5),
                   show.legend = FALSE) +
    scale_color_brewer("Devices included\nin the model", palette = "Dark2",
                       guide = guide_legend(reverse = TRUE)) +
    coord_flip() +
    scale_x_discrete(NULL, expand = c(0, 0)) +
    scale_y_continuous(
        "Relative Rate (95% Credible Interval)",
        trans = "log",
        breaks = c(1/20, 1 / 10, 1 / 4, 1, 4, 10),
        labels = c("1/20", "1/10", "1/4", "1", "4", "10")
    ) +
    mk_nytimes(
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "grey60", fill = NA)
    ) +
    facet_wrap( ~ sensor_cat)

ggsave("./output/figS2_fixed_effects_stratified_models.pdf", p1,
       width = 7, height = 4.5, scale = 1.25, device = cairo_pdf)
ggsave("./output/figS2_fixed_effects_stratified_models.jpg", p1,
       width = 7, height = 4.5, scale = 1.25, dpi = 300)
