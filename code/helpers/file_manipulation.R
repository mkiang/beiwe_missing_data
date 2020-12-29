library(tidyverse)
library(lubridate)

combine_csvs <- function(list_of_csv_files, na = c("", "NA", "unknown"), 
                         verbose = FALSE) {
    holder <- NULL
    for (f in list_of_csv_files) {
        if (verbose) {
            print(f)
            temp_df <- readr::read_csv(f, na = na)
        } else {
            temp_df <- readr::read_csv(f, na = na, col_types = cols())
        }
        holder <- rbind(holder, temp_df)
    }
    return(holder)
}


list_user_folders <- function(study_folder, full_path = FALSE) {
    x <- list.dirs(study_folder, recursive = FALSE, full.names = full_path)
    return(x)
}


parse_file_name <- function(csv_file, with_dttm = FALSE) {
    ## Takes a file name and returns the study folder, subject_id, 
    ## data stream, and a POSIX timestamp of the file.
    
    split_text <- strsplit(csv_file, '/')[[1]]
    fields     <- length(split_text)
    
    x <- list()
    x[["data_type"]]  <- split_text[fields - 1]
    x[["user_id"]]    <- split_text[fields - 2]
    x[["study_name"]] <- split_text[fields - 3]
    x[["file_path"]]  <- csv_file
    
    if (with_dttm) {
        x[["datetime"]]   <- lubridate::ymd_hms(split_text[fields])
    }
    
    return(x)
}


count_data_lines <- function(csv_file) {
    ## Returns the number of rows in a file --- subtract one for header
    n_rows <- R.utils::countLines(csv_file) - 1
    
    return(as.integer(n_rows))
}


summarize_csv_file <- function(csv_file) {
    parsed <- parse_file_name(csv_file)
    parsed[["n_rows"]] <- count_data_lines(csv_file)
    
    df <- tibble::as_data_frame(parsed) %>% 
        select(study_name, user_id, data_type, timestamp, n_rows, file_path)
    
    return(df)
}


bash_merge <- function(folder, awk = TRUE,
                       joined_file = "0000-merged.csv") {
    # Note: `cat` doesn't work if there is a header row.
    original_wd <- getwd()
    setwd(folder)
    if (awk){
        system(paste0("awk 'FNR==1 && NR!=1{next;}{print}' *.csv > ",
                      joined_file))
    } else {
        system(paste0("cat *.csv > ", joined_file))
    }
    setwd(original_wd)
}


