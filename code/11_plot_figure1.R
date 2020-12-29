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
    filter(date_delta > 0) %>% 
    ungroup() %>% 
    arrange(date_delta) %>% 
    mutate(date_rank = 1:n()) %>% 
    arrange(start_date) %>% 
    mutate(start_rank = 1:n()) %>% 
    arrange(study_code, start_date) %>% 
    mutate(study_rank = 1:n()) %>% 
    mutate(
        start_date_0 = 1 + as.numeric(difftime(start_date, min(start_date), units = "days")),
        end_date_0 = 1 + as.numeric(difftime(end_date, min(start_date), units = "days"))
    )

x3 <- ggplot(start_end_df, 
       aes(y = rev(study_rank), x = start_date_0, group = uid, 
           color = study_code)) + 
    geom_segment(aes(yend = rev(study_rank), xend = end_date_0), 
                 alpha = .8, size = .6) + 
    geom_point(alpha = .7, size = .75) + 
    geom_point(aes(x = end_date_0), alpha = .75, size = .7) + 
    scale_color_brewer(NULL, palette = "Dark2") + 
    mk_nytimes(axis.text.y = element_blank(), 
               legend.position = c(0, 0), 
               legend.justification = c(0, 0)) + 
    theme(panel.grid.major.y = element_blank()) + 
    scale_y_continuous("Participant", expand = c(0, 1)) + 
    scale_x_continuous("Time (days)",
                       breaks = c(0, 180, 365, 540, 730, 900))

ggsave("./output/fig1_data_collection_period.pdf", x3, 
       width = 6, height = 6, scale = 1.1, device = cairo_pdf)
ggsave("./output/fig1_data_collection_period.jpg", x3, 
       width = 6, height = 6, scale = 1.1, dpi = 300)
