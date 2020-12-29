## Imports
library(here)
library(tidyverse)
library(lubridate)
source("./code/mk_nytimes.R")

## Data ----
data_df  <- readRDS("./data_working/analytical_df.RDS")

## Start and end dates
start_end_df <- data_df %>%
    group_by(uid, study_code, study_name) %>%
    summarize(start_date = min(date),
              end_date = max(date)) %>%
    mutate(date_delta = end_date - start_date)  %>%
    ungroup() %>%
    arrange(study_code, start_date) %>%
    mutate(study_rank = 1:n()) %>%
    mutate(
        start_date_0 = 1 + as.numeric(difftime(start_date, min(start_date),
                                               units = "days")),
        end_date_0 = 1 + as.numeric(difftime(end_date, min(start_date),
                                             units = "days"))
    )

plot_df <- data_df %>%
    left_join(start_end_df %>%
                  select(uid, study_rank)) %>%
    ungroup() %>%
    mutate(date_0 = 1 + as.numeric(difftime(date, min(date),
                                            units = "days")))

x3 <- ggplot(
    plot_df,
    aes(
        y = study_rank,
        x = date_0,
        color = study_code,
        fill = study_code,
        alpha = prop_cat
    )
) +
    geom_tile(color = NA) +
    scale_fill_brewer("Cohort", palette = "Dark2") +
    scale_alpha_discrete("Sensor non-collection") +
    mk_nytimes(
        axis.text.y = element_blank(),
        legend.position = c(0, 0),
        legend.justification = c(0, 0)
    ) +
    theme(panel.grid.major.y = element_blank()) +
    scale_y_reverse("Participant",
                    expand = c(0, 1)) +
    scale_x_continuous("Time (days)",
                       breaks = c(0, 180, 365, 540, 730, 900))

ggsave("./output/figS1_data_collection_period_alpha.pdf", x3, 
       width = 6, height = 6, scale = 1.1, device = cairo_pdf)
ggsave("./output/figS1_data_collection_period_alpha.jpg", x3, 
       width = 6, height = 6, scale = 1.1, dpi = 300)
