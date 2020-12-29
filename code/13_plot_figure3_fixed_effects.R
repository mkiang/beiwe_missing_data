## Imports ----
library(tidyverse)
source("./code/mk_nytimes.R")

## Data
table3_estimates <- readRDS("./output/model_coefs.RDS") %>% 
    mutate(sensor_cat = factor(sensor,
                               levels = rev(c("accel", "gps")),
                               labels = rev(c("Accelerometer", "GPS")),
                               ordered = TRUE))


fixed_effects_plot <- ggplot(
    table3_estimates,
    aes(
        x = reorder(variable_cat, desc(variable_cat)),
        color = sensor_cat,
        y = exp(beta_estimate),
        ymin = exp(lower_ci),
        ymax = exp(upper_ci)
    )) +
    geom_hline(
        yintercept = c(1 / 10, 1 / 4, 4, 10),
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
    scale_color_brewer("Sensor", palette = "Set1",
                       guide = guide_legend(reverse = TRUE)) +
    coord_flip() +
    scale_x_discrete(NULL, expand = c(0, 0)) +
    scale_y_continuous(
        "Relative Rate (95% Credible Interval)",
        trans = "log",
        breaks = c(1 / 10, 1 / 4, 1, 4, 10),
        labels = c("1/10", "1/4", "1", "4", "10")
    ) + 
    mk_nytimes(
        legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "grey60", fill = NA))

ggsave("./output/fig3_fixed_effects.pdf", fixed_effects_plot, 
       width = 4.5, height = 4.5, scale = 1, device = cairo_pdf)
ggsave("./output/fig3_fixed_effects.jpg", fixed_effects_plot, 
       width = 4.5, height = 4.5, scale = 1, dpi = 300)
