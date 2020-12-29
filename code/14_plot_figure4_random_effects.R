## Imports ----
library(tidyverse)
library(here)
library(brms)

## Read in primary models ----
m_accel <- readRDS(here("model_objects", "accel_m1u_dem.RDS"))
m_gps <- readRDS(here("model_objects", "gps_m1u_dem.RDS"))

random_effects <- bind_rows(
    tibble(
        uid = names(ranef(m_accel)$uid[, 1, ]),
        beta_estimate = unname(ranef(m_accel)$uid[, 1, ]),
        sd_estimate = unname(ranef(m_accel)$uid[, 2, ]),
        lower_ci = unname(ranef(m_accel)$uid[, 3, ]),
        upper_ci = unname(ranef(m_accel)$uid[, 4, ]),
        sensor = "accel"
    ),
    tibble(
        uid = names(ranef(m_gps)$uid[, 1, ]),
        beta_estimate = unname(ranef(m_gps)$uid[, 1, ]),
        sd_estimate = unname(ranef(m_gps)$uid[, 2, ]),
        lower_ci = unname(ranef(m_gps)$uid[, 3, ]),
        upper_ci = unname(ranef(m_gps)$uid[, 4, ]),
        sensor = "gps"
    )
) %>%
    arrange(sensor, beta_estimate) %>%
    group_by(sensor) %>% 
    mutate(x_rank = 1:n(), 
           sig_alpha = ifelse(upper_ci * lower_ci > 0, 1, .5),
           sensor_cat = factor(sensor,
                  levels = c("accel", "gps"),
                  labels = c("Accelerometer", "GPS"),
                  ordered = TRUE))

x1 <- ggplot(random_effects, 
       aes(x = x_rank, y = beta_estimate, alpha = sig_alpha, 
           ymin = lower_ci, ymax = upper_ci)) + 
    geom_hline(yintercept = 0, linetype = "solid", 
               alpha = .7, color = "black") + 
    geom_point() + 
    geom_linerange(show.legend = FALSE) + 
    facet_grid(sensor_cat ~ ., scales = "free") + 
    scale_color_brewer("Model", palette = "Set1") + 
    scale_alpha_identity() + 
    scale_x_continuous(NULL, expand = c(0, 4)) + 
    scale_y_continuous("Participant random effect (95% Credible Interval)") + 
    mk_nytimes(
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "grey60", fill = NA), 
        axis.text.x = element_blank())

ggsave("./output/fig4_random_effects.pdf", x1, 
       width = 6, height = 5, scale = 1, device = cairo_pdf)
ggsave("./output/fig4_random_effects.jpg", x1, 
       width = 6, height = 5, scale = 1, dpi = 300)
