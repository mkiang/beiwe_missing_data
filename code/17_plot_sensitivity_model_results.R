## Imports ----
library(tidyverse)
library(here)
library(brms)
source(here("code", "mk_nytimes.R"))

## Read in primary models ----
m_accel <- readRDS(here("model_objects", "sens_m2_dem_accel.RDS"))
m_gps <- readRDS(here("model_objects", "sens_m2_dem_gps.RDS"))

## Fixed effects ----
sens_estimates <- bind_rows(
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
) %>% 
    mutate(
        beta_exp = exp(beta_estimate),
        lower_exp = exp(lower_ci),
        upper_exp = exp(upper_ci)
    )

sens_estimates <- sens_estimates %>%
    mutate(
        missing_group = case_when(
            substr(variable, 1, 7) == "mu01025" ~ "missing_10_25",
            substr(variable, 1, 7) == "mu02505" ~ "missing_25_50",
            substr(variable, 1, 7) == "mu05075" ~ "missing_50_75",
            substr(variable, 1, 6) == "mu0751" ~ "missing_75_100"
        )
    ) %>%
    mutate(var_short = gsub("mu01025_|mu02505_|mu05075_|mu0751_", "", variable)) %>%
    mutate(missing_cat = factor(
        missing_group,
        levels = c(
            "missing_0_10",
            "missing_10_25",
            "missing_25_50",
            "missing_50_75",
            "missing_75_100"
        ),
        labels = c(
            "[0,0.1] (Reference)",
            "(0.1,0.25]",
            "(0.25,0.5]",
            "(0.5,0.75]",
            "(0.75,1]"
        ),
        ordered = TRUE
    )) %>%
    mutate(var_cat = factor(
        var_short,
        levels = rev(
            c(
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
            )
        ),
        labels = rev(
            c(
                "Intercept",
                "Time (weeks)",
                "iOS device",
                "Female",
                "4-year degree or higher",
                "Black",
                "Asian",
                "American Indian",
                "Other/Multiple",
                "Age (10 years)"
            )
        ),
        ordered = TRUE
    )) %>% 
    mutate(sensor_cat = factor(
        sensor,
        levels = c("accel", "gps"),
        labels = c("Accelerometer", "GPS"),
        ordered = TRUE
    ))

p1 <- ggplot(
    sens_estimates,
    aes(
        x = beta_exp,
        xmin = lower_exp,
        xmax = upper_exp,
        y = var_cat,
        group = missing_cat,
        color = missing_cat
    )
) +
    geom_vline(
        xintercept = c(1 / 50, 1 / 20, 1 / 4, 4, 20),
        linetype = "dotted",
        alpha = .7, 
        color = "black"
    ) + 
    geom_vline(
        xintercept = 1,
        linetype = "solid",
        alpha = .7,
        color = "black"
    ) +
    geom_point(position = position_dodge2(width = .5)) +
    geom_linerange(position = position_dodge2(width = .5)) +
    facet_grid( ~ sensor_cat,
                scales = "free") +
    scale_y_discrete(NULL, 
                     expand = c(0, 0)) + 
    scale_x_continuous(
        "Odds (95% CI)",
        trans = "log",
        breaks = c(1 / 50, 1 / 20, 1 / 4, 1, 4,  20),
        labels = c("1/50", "1/20", "1/4", "1", "4", "20")
    ) + 
    scale_color_brewer("Proportion missing", 
                       drop = FALSE,
                       palette = "Dark2") + 
    mk_nytimes(
        legend.position = "right", 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "grey60", fill = NA))

ggsave("./output/figSX_categorical_regression_fixed_effects.pdf", p1, 
       width = 7.5, height = 4.5, scale = 1, device = cairo_pdf)
ggsave("./output/figSX_categorical_regression_fixed_effects.jpg", p1, 
       width = 7.5, height = 4.5, scale = 1, dpi = 300)
