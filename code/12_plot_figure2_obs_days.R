## Imports
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)
source("./code/mk_nytimes.R")

## Data ----
data_df  <- readRDS("./data_working/analytical_df.RDS") 

## Plot of proportion of missingness per user-day overall
prop_per_user <- data_df %>% 
    group_by(study_code, uid) %>% 
    filter(day_in_study > min(day_in_study), 
           day_in_study < max(day_in_study)) %>% 
    group_by(study_code, uid, prop_cat, sensor_cat) %>% 
    summarize(n = n()) %>% 
    group_by(study_code, uid, sensor_cat) %>% 
    mutate(pr = n / sum(n), 
           sum_n = sum(n)) 

prop_per_user <- prop_per_user %>% 
    left_join(
        prop_per_user %>% 
            filter(prop_cat == "[0,0.1]") %>% 
            group_by(sensor_cat) %>% 
            arrange(desc(pr)) %>% 
            mutate(new_uid = 1:n()) %>% 
            select(study_code, uid, new_uid, sensor_cat)
    ) %>% 
    left_join(
        prop_per_user %>% 
            select(study_code, uid, sensor_cat, sum_n) %>% 
            distinct() %>% group_by(sensor_cat) %>% 
            arrange(sum_n, .by_group = TRUE) %>% 
            mutate(n_rank = 1:n())
    ) %>% 
    ungroup()

## Distribution of daily sensor non-collection
x1 <- ggplot(prop_per_user, aes(y = pr, x = new_uid, fill = prop_cat)) + 
    geom_col(position = "stack") + 
    mk_nytimes(legend.position = "none", 
               panel.grid = element_blank(), 
               axis.text.x = element_blank()) + 
    facet_wrap(~ sensor_cat, scales = "free", ncol = 2) + 
    scale_y_continuous("Proportion of days", expand = c(0, 0)) + 
    scale_x_continuous(NULL, #"Participant (ordered by days of observation)",
                       expand = c(0, 0)) + 
    scale_fill_brewer("Proportion of\nsensor non-collection", 
                      palette = "Spectral", direction = -1)

x2 <- ggplot(prop_per_user, aes(y = n, x = n_rank, fill = prop_cat)) + 
    geom_col(position = "stack") + 
    mk_nytimes(legend.position = "bottom", 
               panel.grid = element_blank(), 
               axis.text.x = element_blank()) + 
    facet_wrap(~ sensor_cat, scales = "free", ncol = 2) + 
    scale_y_continuous("Number of days", expand = c(0, 0)) + 
    scale_x_continuous("Participant (ordered by days of observation)", 
                       expand = c(0, 0)) + 
    scale_fill_brewer("Proportion of\nsensor non-collection", 
                      palette = "Spectral", direction = -1)

x3 <- x1 + x2 + plot_layout(ncol = 1)

ggsave("./output/fig2_dist_daily_non_collection.pdf", x3,
       width = 8, height = 6, scale = 1.1, device = cairo_pdf)
ggsave("./output/fig2_dist_daily_non_collection.jpg", x3,
       width = 8, height = 6, scale = 1.1, dpi = 300)

ggsave("./output/figS99x_dist_daily_non_collection_prop.pdf", 
       x1 + facet_wrap(~ sensor_cat, scales = "free", ncol = 1),
       width = 8, height = 6, scale = 1.1, device = cairo_pdf)
ggsave("./output/figS99x_dist_daily_non_collection_num.pdf", 
       x2 + facet_wrap(~ sensor_cat, scales = "free", ncol = 1),
       width = 8, height = 6, scale = 1.1, device = cairo_pdf)
ggsave("./output/figS99x_dist_daily_non_collection_prop.jpg", 
       x1 + facet_wrap(~ sensor_cat, scales = "free", ncol = 1),
       width = 8, height = 6, scale = 1.1, dpi = 300)
ggsave("./output/figS99x_dist_daily_non_collection_num.jpg", 
       x2 + facet_wrap(~ sensor_cat, scales = "free", ncol = 1),
       width = 8, height = 6, scale = 1.1, dpi = 300)
