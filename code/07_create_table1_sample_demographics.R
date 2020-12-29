## Imports
library(here)
library(fs)
library(tidyverse)
source(here("code", "utils.R"))

## Get data ----
data_df  <- readRDS(here("data_working" , "analytical_df.RDS"))

## Users demographics ----
user_demo <- data_df %>%
    select(study_code,
           uid,
           iphone,
           female,
           age,
           race,
           race_cat,
           education) %>%
    distinct()

TOTAL_N <- n_distinct(user_demo$uid)

table1 <- bind_rows(
    user_demo %>%
        group_by(study_code) %>%
        summarize(
            n_participants = n(),
            
            age_mean = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            
            n_iphone = sum(iphone == 1, na.rm = TRUE),
            n_android = sum(iphone == 0, na.rm = TRUE),
            
            n_gender_male = sum(female == 0, na.rm = TRUE),
            n_gender_female = sum(female == 1, na.rm = TRUE),
            n_gender_missing = sum(is.na(female)),
            
            n_edu_hs = sum(education == "hs", na.rm = TRUE),
            n_edu_u4 = sum(education == "associates", na.rm = TRUE),
            n_edu_4c = sum(education == "bachelors", na.rm = TRUE),
            n_edu_grad = sum(education == "masters", na.rm = TRUE),
            n_edu_missing = sum(is.na(education)),
            
            n_race_white = sum(race == "white", na.rm = TRUE),
            n_race_black = sum(race == "black", na.rm = TRUE),
            n_race_asian = sum(race == "asian", na.rm = TRUE),
            n_race_aian = sum(race == "aian", na.rm = TRUE),
            n_race_other = sum(race == "other", na.rm = TRUE),
            n_race_missing = sum(is.na(race))
        ) %>%
        ungroup(),
    user_demo %>%
        mutate(study_code = "Total") %>%
        group_by(study_code) %>%
        summarize(
            n_participants = n(),
            
            age_mean = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            
            n_iphone = sum(iphone == 1, na.rm = TRUE),
            n_android = sum(iphone == 0, na.rm = TRUE),
            
            n_gender_male = sum(female == 0, na.rm = TRUE),
            n_gender_female = sum(female == 1, na.rm = TRUE),
            n_gender_missing = sum(is.na(female)),
            
            n_edu_hs = sum(education == "hs", na.rm = TRUE),
            n_edu_u4 = sum(education == "associates", na.rm = TRUE),
            n_edu_4c = sum(education == "bachelors", na.rm = TRUE),
            n_edu_grad = sum(education == "masters", na.rm = TRUE),
            n_edu_missing = sum(is.na(education)),
            
            n_race_white = sum(race == "white", na.rm = TRUE),
            n_race_black = sum(race == "black", na.rm = TRUE),
            n_race_asian = sum(race == "asian", na.rm = TRUE),
            n_race_aian = sum(race == "aian", na.rm = TRUE),
            n_race_other = sum(race == "other", na.rm = TRUE),
            n_race_missing = sum(is.na(race))
        )
) 

table1_print <- table1 %>%
    mutate(
        age_print = sprintf("%0.1f (%0.1f)", round(age_mean, 1), round(age_sd, 1))
    ) %>%
    select(-study_code,
           -age_mean,
           -age_sd) %>%
    select(n_participants, age_print, everything()) %>%
    t()
colnames(table1_print) <- c(LETTERS[1:6], "total")
table1_print <- as_tibble(table1_print)
table1_print$variable <- c("participants", "age_sd", colnames(table1)[-c(1:4)])

table1_print <- table1_print %>%
    select(variable, everything()) %>%
    mutate(total_perc = ifelse(
        variable != "age_sd",
        sprintf("%s (%i%%)", total, round(as.numeric(total) / TOTAL_N * 100, 0)),
        total
    ))

write_csv(table1, here("output", "table1_transposed.csv"))
write_csv(table1_print,
          here("output", "table1.csv"))
