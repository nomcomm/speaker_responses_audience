rm(list = ls())
cat("\014")
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(openxlsx)
library(fs)

setwd(here::here())

#onsetoffset files
onsetoffset <- 
  read_excel("./data/onset_offset.xlsx", 
             col_types = c("text", "text", "date", "date"))
onsetoffset  <- onsetoffset %>%
  mutate(
    start = as_hms(start),
    stop = as_hms(stop)
  )


#Heartrate
##Add Time
parent_dir <- "./data/Datafiles/HR_CL_EyeTracking/00_raw_data"
folders <- list.dirs(parent_dir, recursive = FALSE)
new_dir <- "./data/Datafiles/HR_CL_EyeTracking/01_raw_data_time_added/HeartRate"
for (folder in folders) {
  cat("Checking folder:", folder, "\n")
  folder_name <- basename(folder)
  sub_speech <- gsub("_OMNICEPT$", "", folder_name) 
  files <- list.files(folder, pattern = "^HR-.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    cat("No HR files found in folder:", folder, "\n")
    next
  }
  for (file in files) {
    cat("Processing file:", file, "\n")
    heartrate <- tryCatch(
      read.csv(file, sep = ",", check.names = FALSE),
      error = function(e) {
        cat("Error reading file:", file, "\n")
        return(NULL)
      }
    )
    if (is.null(heartrate)) {
      cat("Could not read file:", file, "\n")
      next
    }
    if (nrow(heartrate) == 0) {
      cat("File is empty or contains no rows:", file, "\n")
      next
    }
    ts_sys_column <- grep("ts/sys", colnames(heartrate), value = TRUE)
    if (length(ts_sys_column) == 0) {
      cat("'ts/sys' column not found in file:", file, "\n")
      next
    }
    heartrate[[ts_sys_column]] <- as.numeric(heartrate[[ts_sys_column]])
    if (any(is.na(heartrate[[ts_sys_column]]))) {
      cat("Some 'ts/sys' values could not be converted to numeric in file:", file, "\n")
      next
    }
    options(scipen = 999)
    heartrate$Time <- with(heartrate, {
      time_in_seconds <- (get(ts_sys_column) / 1e6)  # Convert microseconds to seconds
      origin_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")  # UNIX epoch
      datetime <- origin_date + time_in_seconds  # Add seconds to the epoch
      datetime_adjusted <- datetime - (5 * 3600)  # Adjust for timezone offset
      time_only <- format(datetime_adjusted, "%H:%M:%S")  # Format as time
      time_only
    })
    output_file <- file.path(new_dir, paste0(sub_speech, "_HR_timeadded.csv"))
    write.csv(heartrate, output_file, row.names = FALSE)
    cat("File saved:", output_file, "\n")
  }
}

##Check onset and offset times of manually cut files
HR_input_folder <- 
  "./data/DataFiles/HR_CL_EyeTracking/02_raw_data_time_added_cut/HeartRate"
HR_csv_files <- 
  list.files(path = HR_input_folder, pattern = "\\.csv$", full.names = FALSE)
for (file in HR_csv_files) {
  filename <- file.path(HR_input_folder, file)
  matches <- str_split(file, "_|\\.", simplify = TRUE)
  subject <- matches[1]
  speech <- matches[2]
  match_row <- onsetoffset %>%
    filter(Participant_Code == subject & Speech_Number == speech)
  if (nrow(match_row) == 0) {
    cat("Skipping file with unmatched pattern:", filename, "\n")
    next
  }
  csv_data <- read.csv(filename)
  if (!"Time" %in% colnames(csv_data)) {
    cat("No 'Time' column found in:", filename, "\n")
    next
  }
  csv_data$Time <- as_hms(csv_data$Time)
  first_time <- csv_data$Time[1]
  last_time <- csv_data$Time[nrow(csv_data)]
  if (!(first_time >= match_row$start && last_time <= match_row$stop))
  {cat(file, "potential mismatch", "\n")} 
  
  file_start_mmss <- sprintf("%02d:%02d", minute(first_time), second(first_time))
  file_end_mmss   <- sprintf("%02d:%02d", minute(last_time), second(last_time))
  ref_start_mmss  <- 
    sprintf("%02d:%02d", minute(match_row$start), second(match_row$start))
  ref_end_mmss    <- 
    sprintf("%02d:%02d", minute(match_row$stop), second(match_row$stop))
  if (!(file_start_mmss >= ref_start_mmss && file_end_mmss <= ref_end_mmss)) {
    cat("recheck", file, "\n")
  }
}




if (!dir.exists(HR_output_folder)) {
  dir.create(HR_output_folder, recursive = TRUE)
}







#Pupil Dilation
##Add Time
parent_dir <- "./data/Datafiles/HR_CL_EyeTracking/00_raw_data"
folders <- list.dirs(parent_dir, recursive = FALSE)
new_dir <- "./data/Datafiles/HR_CL_EyeTracking/01_raw_data_time_added/EyeTracking"

for (folder in folders) {
  cat("Checking folder:", folder, "\n")
  folder_name <- basename(folder) 
  sub_speech <- gsub("_OMNICEPT$", "", folder_name)  
  files <- list.files(folder, pattern = "^EyeTracking-.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    cat("No EyeTracking files found in folder:", folder, "\n")
    next
  }
  for (file in files) {
    cat("Processing file:", file, "\n")
    eye_tracking <- tryCatch(
      read.csv(file, sep = ",", check.names = FALSE),
      error = function(e) {
        cat("Error reading file:", file, "\n")
        return(NULL)
      }
    )
    if (is.null(eye_tracking)) {
      cat("Could not read file:", file, "\n")
      next
    }
    if (nrow(eye_tracking) == 0) {
      cat("File is empty or contains no rows:", file, "\n")
      next
    }
    ts_sys_column <- grep("ts/sys", colnames(eye_tracking), value = TRUE)
    if (length(ts_sys_column) == 0) {
      cat("'ts/sys' column not found in file:", file, "\n")
      next
    }
    eye_tracking[[ts_sys_column]] <- as.numeric(eye_tracking[[ts_sys_column]])
    if (any(is.na(eye_tracking[[ts_sys_column]]))) {
      cat("Some 'ts/sys' values could not be converted to numeric in file:", file, "\n")
      next
    }
    options(scipen = 999)
    eye_tracking$Time <- with(eye_tracking, {
      time_in_seconds <- (get(ts_sys_column) / 1e6)  
      origin_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC") 
      datetime <- origin_date + time_in_seconds 
      datetime_adjusted <- datetime - (5 * 3600)  
      time_only <- format(datetime_adjusted, "%H:%M:%S") 
      time_only
    })
    output_file <- file.path(new_dir, paste0(sub_speech, "_EyeTracking_timeadded.csv"))
    write.csv(eye_tracking, output_file, row.names = FALSE)
    cat("File saved:", output_file, "\n")
  }
}

##Check onset and offset times of manually cut files
PD_input_folder <- 
  "./data/DataFiles/HR_CL_EyeTracking/02_raw_data_time_added_cut/EyeTracking"
PD_csv_files <- 
  list.files(path = PD_input_folder, pattern = "\\.csv$", full.names = FALSE)
for (file in PD_csv_files) {
  filename <- file.path(PD_input_folder, file)
  matches <- str_split(file, "_|\\.", simplify = TRUE)
  subject <- matches[1]
  speech <- matches[2]
  match_row <- onsetoffset %>%
    filter(Participant_Code == subject & Speech_Number == speech)
  if (nrow(match_row) == 0) {
    cat("Skipping file with unmatched pattern:", filename, "\n")
    next
  }
  csv_data <- read.csv(filename)
  if (!"Time" %in% colnames(csv_data)) {
    cat("No 'Time' column found in:", filename, "\n")
    next
  }
  csv_data$Time <- as_hms(csv_data$Time)
  first_time <- csv_data$Time[1]
  last_time <- csv_data$Time[nrow(csv_data)]
  if (!(first_time >= match_row$start && last_time <= match_row$stop))
  {cat(file, "potential mismatch", "\n")} 
  
  file_start_mmss <- sprintf("%02d:%02d", minute(first_time), second(first_time))
  file_end_mmss   <- sprintf("%02d:%02d", minute(last_time), second(last_time))
  ref_start_mmss  <- 
    sprintf("%02d:%02d", minute(match_row$start), second(match_row$start))
  ref_end_mmss    <- 
    sprintf("%02d:%02d", minute(match_row$stop), second(match_row$stop))
  if (!(file_start_mmss >= ref_start_mmss && file_end_mmss <= ref_end_mmss)) {
    cat("recheck", file, "\n")
  }
}


#EEG
#Add Time
input_folder <- "./data/DataFiles/EEG/00_raw_data"
output_folder <- "./data/DataFiles/EEG/01_raw_data_time_added"   
all_subfolders <- list.dirs(path = input_folder, recursive = FALSE, full.names = TRUE)
first_40_subfolders <- head(all_subfolders, 40)
for (subfolder in first_40_subfolders) {
  # Get subfolder name only (e.g., "Session1")
  subfolder_name <- basename(subfolder)
  # Create corresponding output subfolder
  output_subfolder <- file.path(output_folder, subfolder_name)
  if (!dir.exists(output_subfolder)) {
    dir.create(output_subfolder, recursive = TRUE)
  }
  # List CSV files in the input subfolder
  csv_files <- list.files(path = subfolder, pattern = "\\.csv$", full.names = TRUE)
  for (file_path in csv_files) {
    filename <- basename(file_path)
    # Extract time from the last underscore in the filename
    parts <- strsplit(filename, "_")[[1]]
    time_str <- sub("\\.csv$", "", parts[length(parts)])
    if (!grepl("^\\d{6}$", time_str)) {
      message(paste("⛔ Skipping (invalid time format):", filename))
      next
    }
    # Convert to hms and seconds since midnight
    start_time <- hms::as_hms(strptime(time_str, "%H%M%S"))
    start_seconds <- as.numeric(start_time)
    # Read CSV
    df <- tryCatch({
      read_csv(file_path, show_col_types = FALSE)
    }, error = function(e) {
      message(paste("⛔ Failed to read file:", filename))
      return(NULL)
    })
    if (is.null(df) || !"Counter" %in% names(df)) {
      message(paste("⛔ Skipping (missing 'Counter'):", filename))
      next
    }
    # Add new columns
    df <- df %>%
      mutate(
        Start_Recording = format(start_time, "%H:%M:%S"),
        Seconds = floor((row_number() - 1) / 250),
        Time = sprintf("%02d:%02d:%02d",
                       floor((as.numeric(start_time) + Seconds) / 3600),
                       floor(((as.numeric(start_time) + Seconds) %% 3600) / 60),
                       floor((as.numeric(start_time) + Seconds) %% 60))
      )
    # Save to output folder
    output_file <- file.path(output_subfolder, sub("\\.csv$", "_timeadded.csv", filename))
    write_csv(df, output_file)
    message(paste("✅ Processed:", output_file))
  }
}
second_40_subfolders <- all_subfolders[41:80]
for (subfolder in second_40_subfolders) {
  # Get subfolder name only (e.g., "Session1")
  subfolder_name <- basename(subfolder)
  # Create corresponding output subfolder
  output_subfolder <- file.path(output_folder, subfolder_name)
  if (!dir.exists(output_subfolder)) {
    dir.create(output_subfolder, recursive = TRUE)
  }
  # List CSV files in the input subfolder
  csv_files <- list.files(path = subfolder, pattern = "\\.csv$", full.names = TRUE)
  for (file_path in csv_files) {
    filename <- basename(file_path)
    # Extract time from the last underscore in the filename
    parts <- strsplit(filename, "_")[[1]]
    time_str <- sub("\\.csv$", "", parts[length(parts)])
    if (!grepl("^\\d{6}$", time_str)) {
      message(paste("⛔ Skipping (invalid time format):", filename))
      next
    }
    # Convert to hms and seconds since midnight
    start_time <- hms::as_hms(strptime(time_str, "%H%M%S"))
    start_seconds <- as.numeric(start_time)
    # Read CSV
    df <- tryCatch({
      read_csv(file_path, show_col_types = FALSE)
    }, error = function(e) {
      message(paste("⛔ Failed to read file:", filename))
      return(NULL)
    })
    if (is.null(df) || !"Counter" %in% names(df)) {
      message(paste("⛔ Skipping (missing 'Counter'):", filename))
      next
    }
    # Add new columns
    df <- df %>%
      mutate(
        Start_Recording = format(start_time, "%H:%M:%S"),
        Seconds = floor((row_number() - 1) / 250),
        Time = sprintf("%02d:%02d:%02d",
                       floor((as.numeric(start_time) + Seconds) / 3600),
                       floor(((as.numeric(start_time) + Seconds) %% 3600) / 60),
                       floor((as.numeric(start_time) + Seconds) %% 60))
      )
    # Save to output folder
    output_file <- file.path(output_subfolder, sub("\\.csv$", "_timeadded.csv", filename))
    write_csv(df, output_file)
    message(paste("✅ Processed:", output_file))
  }
}

##Check onset and offset times of manually cut files
EEG_input_folder <- 
  "./data/DataFiles/EEG/02_raw_data_time_added_cut"
EEG_subfolders <- list.dirs(path = EEG_input_folder, full.names = TRUE)
EEG_subfolders <- EEG_subfolders[EEG_subfolders != EEG_input_folder]
for (folder in EEG_subfolders) {
  EEG_csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = FALSE)
  for (file in EEG_csv_files) {
    filename <- file.path(folder, file)
    matches <- str_split(file, "_|\\.", simplify = TRUE)
    subject <- matches[1]
    speech <- matches[2]
    match_row <- onsetoffset %>%
      filter(Participant_Code == subject & Speech_Number == speech)
    if (nrow(match_row) == 0) {
      cat("Skipping file with unmatched pattern:", filename, "\n")
      next
    }
    csv_data <- read.csv(filename)
    if (!"Time" %in% colnames(csv_data)) {
      cat("No 'Time' column found in:", filename, "\n")
      next
    }
    csv_data$Time <- as_hms(csv_data$Time)
    first_time <- csv_data$Time[1]
    last_time <- csv_data$Time[nrow(csv_data)]
    if (!(first_time >= match_row$start && last_time <= match_row$stop))
    {cat(file, "potential mismatch", "\n")} 
    
    file_start_mmss <- sprintf("%02d:%02d", minute(first_time), second(first_time))
    file_end_mmss   <- sprintf("%02d:%02d", minute(last_time), second(last_time))
    ref_start_mmss  <- 
      sprintf("%02d:%02d", minute(match_row$start), second(match_row$start))
    ref_end_mmss    <- 
      sprintf("%02d:%02d", minute(match_row$stop), second(match_row$stop))
    if (!(file_start_mmss >= ref_start_mmss && file_end_mmss <= ref_end_mmss)) {
      cat("recheck", file, "\n")
    }
  }
}







