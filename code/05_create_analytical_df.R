## Imports
library(here)
library(tidyverse)

## Data ----
## Get a mapping of people who have multiple IDs
multi_id_map <-
    read_csv(here("data_working", "beiwe_multiple_user_id_mappings.csv"))
multi_id_replace <- multi_id_map$beiwe_user_id_2
names(multi_id_replace) <- multi_id_map$beiwe_user_id_1

## Grouping summaries
grouping_data <- bind_rows(
    readRDS(here("data_working", "accel_groupings.RDS")) %>%
        ungroup() %>%
        mutate(sensor = "accel"),
    readRDS(here("data_working", "gps_groupings.RDS")) %>%
        ungroup() %>%
        mutate(sensor = "gps")
) %>%
    ungroup()

## Grouping details ----
detail_data <- map_df(
    .x = fs::dir_ls("data_stripped",
                    recurse = TRUE,
                    regexp = "file_summary.RDS"),
    .f = ~ readRDS(.x)  %>%
        mutate(uid = str_replace_all(user_id, multi_id_replace))
)

detail_data <- detail_data %>%
    group_by(data_type, study_name, uid) %>%
    summarize(
        first_obs = min(first_obs),
        last_obs = max(last_obs),
        max_daily_obs = max(max_daily_obs),
        min_daily_obs = min(min_daily_obs),
        total_obs = sum(total_obs)
    ) %>%
    ungroup() %>%
    mutate(
        hours_of_obs = as.numeric(difftime(last_obs, first_obs, units = "hours")),
        study_cat = factor(
            study_name,
            levels = c(
                "study_a",
                "study_b",
                "study_c",
                "study_d",
                "study_e",
                "study_f"
            )
        ),
        study_code =
            factor(
                study_name,
                levels = c(
                    "study_a",
                    "study_b",
                    "study_c",
                    "study_d",
                    "study_e",
                    "study_f"
                ),
                labels = paste("Study", LETTERS[1:6]),
                ordered = TRUE
            ),
        sensor = ifelse(data_type == "accelerometer", "accel",
                        data_type)
    ) %>% 
    ungroup()

saveRDS(detail_data, "./data_working/grouping_details.RDS")

## Create a date variable
grouping_data <- grouping_data %>%
    mutate(date = lubridate::ymd(sprintf("%s/%s/%s", year, month, day))) %>%
    arrange(study_name, uid, date)

## Create a skeleton dataframe with the full date range for each user
user_start_end <- grouping_data %>%
    group_by(study_name, uid, sensor) %>%
    summarize(start_date = min(date, na.rm = TRUE),
              end_date = max(date, na.rm = TRUE))

all_dates <- vector("list", NROW(user_start_end))
for (i in 1:NROW(user_start_end)) {
    all_dates[[i]] <- tibble(
        study_name = user_start_end$study_name[i],
        uid = user_start_end$uid[i],
        date = seq.Date(
            from = user_start_end$start_date[i],
            to = user_start_end$end_date[i],
            by = 1
        ),
        sensor = user_start_end$sensor[i]
    )
}
all_dates <- bind_rows(all_dates)

## Combine data to the skeleton file
data_df <- all_dates %>%
    mutate(
        year = lubridate::year(date),
        month = lubridate::month(date),
        day = lubridate::day(date)
    ) %>%
    left_join(
        grouping_data %>%
            select(
                study_name,
                uid,
                sensor,
                exp_grps,
                female,
                age,
                race,
                education,
                iphone
            ) %>%
            distinct()
    ) %>%
    left_join(grouping_data %>%
                  select(study_name, uid, sensor, date, daily_groupings)) %>%
    mutate(daily_groupings = ifelse(is.na(daily_groupings), 0, daily_groupings)) %>%
    mutate(missing_grps = exp_grps - daily_groupings) %>%
    mutate(missing_grps = ifelse(missing_grps < 0, 0, missing_grps))

## Munging
data_df <- data_df %>%
    mutate(
        prop_missing = missing_grps / exp_grps,
        prop_missing = ifelse(prop_missing > 1, 1, prop_missing),
        prop_cat = cut(
            prop_missing,
            c(0, .1, .25, .5, .75, 1),
            include.lowest = TRUE,
            dig.lab = 2
        ),
        study_cat = factor(
            study_name,
            levels = c(
                "study_a",
                "study_b",
                "study_c",
                "study_d",
                "study_e",
                "study_f"
            ),
            labels = paste(LETTERS[1:6]),
            ordered = TRUE
        ),
        sex = factor(
            female,
            levels = c(1:0),
            labels = c("Female", "Male"),
            ordered = TRUE
        ),
        race_cat = factor(
            race,
            levels = c("white", "black", "asian",
                       "other", "aian"),
            labels = c("White", "Black", "Asian",
                       "Other/Multiple", "American Indian"),
            ordered = TRUE
        ),
        edu_cat = factor(
            education,
            levels = c("hs", "associates",
                       "bachelors", "masters"),
            labels = c("High School", "Associates",
                       "Bachelors", "Graduate Degree"),
            ordered = TRUE
        ),
        sensor_cat = factor(
            sensor,
            levels = c("accel", "gps"),
            labels = c("Accelerometer", "GPS")
        )
    ) %>%
    ungroup()

## Better education categories
data_df <- data_df %>%
    mutate(
        education_broad = case_when(
            education %in% c("hs", "associates") ~ ">=HS, <4 years college",
            education %in% c("bachelors", "masters") ~ ">=4 years college"
        )
    ) %>%
    mutate(edu_broad_cat = factor(
        education_broad,
        levels = c(">=HS, <4 years college",
                   ">=4 years college"),
        ordered = TRUE
    ))

## Add a time trend variable of days from start of the study
data_df <- data_df %>%
    group_by(study_name, uid, sensor) %>%
    arrange(year, month, day, .by_group = TRUE) %>%
    mutate(day_in_study = as.integer(date - min(date)) + 1) %>%
    mutate(week_in_study = day_in_study / 7) %>%
    ungroup()

## Scaled age (but scaled by users not by user-day) ----
## All ages will then be relative to the youngest person in the data set
data_df <- data_df %>%
    left_join(data_df %>%
                  select(uid, age) %>%
                  distinct() %>%
                  mutate(scaled_age = (age - min(age, na.rm = TRUE)))) %>%
    ungroup()

## Add a proper date variable and study code ----
data_df <- data_df %>%
    mutate(
        study_code =
            factor(
                study_name,
                levels = c(
                    "study_a",
                    "study_b",
                    "study_c",
                    "study_d",
                    "study_e",
                    "study_f"
                ),
                labels = paste("Study", LETTERS[1:6]),
                ordered = TRUE
            ),
        date = lubridate::ymd(sprintf("%i-%i-%i", year, month, day))
    )

saveRDS(data_df, "./data_working/analytical_df.RDS")
