# jonashaslbeck@protonmail.com; Jan 27th, 2024

# --------------------------------------------------------
# ---------- What is happening here? ---------------------
# --------------------------------------------------------

# Here we preprocess the data of Grommisch 2020, as the author shared them
# online into a format that is more common and uniform, which we will
# then use in the tutorial

# --------------------------------------------------------
# ---------- Load Packages -------------------------------
# --------------------------------------------------------

library(plyr)
source("Helpers.R")


# --------------------------------------------------------
# ---------- Load Data -----------------------------------
# --------------------------------------------------------

load("Data/Grommisch2020/osfstorage-archive/Feel1_Grommisch_et.al.(2019).rda")
data_raw <- data

# --------------------------------------------------------
# ---------- Process -------------------------------------
# --------------------------------------------------------

# ------ Make Beep variable that starts at 1 every day ------

u_pers <- unique(data_raw$SEMA_ID)
n_pers <- length(u_pers)

# Storage
l_pers <- list()

for(i in 1:n_pers) {
  ss_data <- data_raw[data_raw$SEMA_ID == u_pers[i], ]
  l_days <- list()
  u_days <- unique(ss_data$DayNr)
  n_days <- length(u_days)
  for(d in 1:n_days) {
    ss_data_d <- ss_data[ss_data$DayNr == u_days[d], ]
    ss_data_d$Beep <- ss_data_d$RowNr - ss_data_d$RowNr[1] + 1
    l_days[[d]] <- ss_data_d$Beep
  }
  l_pers[[i]] <- unlist(l_days)
  print(i)
}

# ------ Subset Data Frame ------
data_cl <- data_raw[, c("SEMA_ID", "RowNr", "DayNr", "HAP", "RLX", "SAD", "ANG", "DASS_D_agg", "DASS_A_agg", "DASS_S_agg")]

# ------ Add New Beep Variable to data frame ------
data_cl$Beep <- unlist(l_pers)

# ------ Make Time Variable ------
colnames(data_cl)[2] <- "Time" # We can just the running beep variable for that; days are not missing
data_cl$Time <- data_cl$Time - 1 # To make sure time starts at 0

# Reorder
data_cl <- data_cl[, c("SEMA_ID", "Time", "Beep", "DayNr", "HAP", "RLX", "SAD", "ANG", "DASS_D_agg", "DASS_A_agg", "DASS_S_agg")]
head(data_cl)

# ------ Change some Labels -----
colnames(data_cl)[5:8] <- c("Happy", "Relaxed", "Sad", "Angry")


# ------ Fill in Missing data points -----
# This fills in NAs for missing beeps as indicated by RowNr variable

fill_missing_rows <- function(data) {
  # Ensure data is ordered by SEMA_ID and RowNr
  data <- data[order(data$SEMA_ID, data$Time), ]

  # Create an empty list to store results for each SEMA_ID
  filled_data_list <- list()

  # Loop through each unique SEMA_ID
  for (sema_id in unique(data$SEMA_ID)) {
    # Subset data for the current SEMA_ID
    subset_data <- subset(data, SEMA_ID == sema_id)

    # Create a full sequence of RowNr for the current SEMA_ID
    full_RowNr <- seq(min(subset_data$Time), max(subset_data$Time))

    # Create a new dataframe with full sequence of RowNr
    full_subset_data <- data.frame(Time = full_RowNr)

    # Merge with the subset data to find missing rows
    merged_subset_data <- merge(full_subset_data, subset_data, by = "Time", all.x = TRUE)

    # Fill missing SEMA_ID with the current SEMA_ID
    merged_subset_data$SEMA_ID <- sema_id

    # Set other columns to NA for newly created rows
    cols_to_fill <- setdiff(names(data), c("SEMA_ID", "Time"))
    merged_subset_data[is.na(merged_subset_data$DayNr), cols_to_fill] <- NA

    # Append the filled data for this SEMA_ID to the list
    filled_data_list[[length(filled_data_list) + 1]] <- merged_subset_data
  }
  # Combine all filled data into a single dataframe
  filled_data <- do.call(rbind, filled_data_list)

  return(filled_data)
}

data_cl_merg <- fill_missing_rows(data_cl)

# ------ Reorder ------
data_cl_merg <- data_cl_merg[, c("SEMA_ID", "Time", "Beep", "DayNr", "Happy", "Relaxed", "Sad", "Angry", "DASS_D_agg", "DASS_A_agg", "DASS_S_agg")]
head(data_cl_merg)


# --------------------------------------------------------
# ---------- Save ----------------------------------------
# --------------------------------------------------------

# ------ Save ------
saveRDS(data_cl_merg, "Files//Data_Grommisch2020_clean.RDS")



