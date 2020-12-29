## Imports ----
library(tidyverse)
library(metabeiwe)
library(fs)
source("./code/utils.R")

## Constants ----
sensors_to_summarize <- c("accelerometer", "gps")
cfig <- config::get()
u_changes    <- cfig$user_changes
data_folder  <- cfig$strip_folder

## Data ----
user_studies <- readRDS("./data_working/list_of_users_and_studies.RDS")

## Get files ----
all_data_files <- dir_ls(data_folder, recurse = TRUE, regexp = ".RDS")
data_files <- all_data_files[grepl(paste(sensors_to_summarize, collapse = "|"), 
                                   all_data_files)]
data_files <- data_files[!grepl("groupings_summary.RDS", data_files)]
data_files <- data_files[!grepl("file_summary.RDS", data_files)]

## Summarize ----
for (f in data_files) {
    if (!file.exists(sprintf("%s/groupings_summary.RDS", dirname(f))) | 
        !file.exists(sprintf("%s/file_summary.RDS", dirname(f)))) {
        print(f)
        
        ## Get file info
        f_info    <- metabeiwe:::.parse_file_name(f)
        user_id   <- f_info$user_id
        study_dir <- f_info$study_name
        data_type <- f_info$data_type
        buffer_t  <- return_buffer(data_type, user_id, user_studies)
        
        ## Load
        if (!file.exists(sprintf("%s/file_summary.RDS", dirname(f))) |
            !file.exists(sprintf("%s/groupings_summary.RDS", dirname(f)))) {
            data_df <- readRDS(f)
        }
        
        ## High level summary
        if (!file.exists(sprintf("%s/file_summary.RDS", dirname(f)))) {
            temp_summary <- summarize_data_df(data_df, f)
            saveRDS(temp_summary, 
                    sprintf("%s/file_summary.RDS", dirname(f)))
        }
        
        ## Groupings summary
        if (!file.exists(sprintf("%s/groupings_summary.RDS", dirname(f)))) {
            temp_groupings <- data_df %>% 
                rename_time() %>% 
                create_grouping_cols(buffer = buffer_t) %>% 
                summarize_groupings() %>% 
                create_metadata_cols(f)
            
            saveRDS(temp_groupings, 
                    sprintf("%s/groupings_summary.RDS", dirname(f)))
        }
        
        ## Clean up
        rm(data_df, temp_summary, temp_groupings); gc()
    } else {
        print(sprintf("Skipping %s", f))
    }
}
