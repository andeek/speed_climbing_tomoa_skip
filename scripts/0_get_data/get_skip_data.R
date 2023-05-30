# Required libraries
library(dplyr)
library(googlesheets4)
library(r2r)

# Import data from Google Drive
#m_tomoa_skip_data <- read_sheet('https://docs.google.com/spreadsheets/d/15vR-ZX_4U7oRExClp_SWz88g3hWKRGjETQssIACAgSk/edit?usp=sharing')
#w_tomoa_skip_data <- read_sheet('https://docs.google.com/spreadsheets/d/19m6dNDvdVuqtaQsVS0mt8r2iElL7Pe95vkm6cWiPq_E/edit?usp=sharing')

#make copies to update to (easier for testing)
m_times_update <- m_times
w_times_update <- w_times

# Join imported data into overall times for men and women
m_times_update <- m_times |>
  mutate(event_id = as.numeric(event_id)) |>
  left_join(
            m_tomoa_skip_data |>
            select(fname, lname, event_id, tomoa_skip, start_date, end_date, year),
            by = c("fname", "lname", "event_id"),
            ) |>
  mutate(tomoa_skip = ifelse(tomoa_skip == "NULL", NA, tomoa_skip))

w_times_update <- w_times |>
  mutate(event_id = as.numeric(event_id)) |>
  left_join(
    w_tomoa_skip_data |>
      select(fname, lname, event_id, tomoa_skip, start_date, end_date, year),
    by = c("fname", "lname", "event_id"),
  ) |>
  mutate(tomoa_skip = ifelse(tomoa_skip == "NULL", NA, tomoa_skip))

#Create empty hashmaps for mens and womens
m_hm <- hashmap()
w_hm <- hashmap()

# FIXME?
# The earliest event we have tomoa skip data for is 1048, so can assume all earlier events as false
m_times_update$tomoa_skip[as.double(m_times_update$event_id) < 1048] <- FALSE
w_times_update$tomoa_skip[as.double(w_times_update$event_id) < 1048] <- FALSE

# Sort the merged data set by event id so we can properly compare event IDs
m_times_update <-  m_times_update[order(as.double(m_times_update$event_id)), ]
w_times_update <- w_times_update[order(as.double(w_times_update$event_id)), ]

# For mens data
# tomoa skip is string type currently, so convert to bool
m_times_update$tomoa_skip <- as.logical(m_times_update$tomoa_skip)
for(i in 1:nrow(m_times_update)){
  
  row <- m_times_update[i, ]
  full_name <- paste(row$fname, row$lname, sep = " ")
  
  # Check if tomoa_skip is NA, true, and query is null
  # If so, we can had climber to hash map as the climber has not been seen
  # doing the skip yet
  if (!is.na(row$tomoa_skip) && row$tomoa_skip && is.null(query(m_hm, full_name))) {
    insert(m_hm, full_name, row$event_id)
  }
  
  # If query is not null, compare event_id values
  else if (!is.null(query(m_hm, full_name))) {
    # Compare current event_id to query event_id
    if (as.double(row$event_id) >= as.double(query(m_hm, full_name))) {
      m_times_update[i, "tomoa_skip"] <- TRUE
    }
  }
}


# For womens data (works the same)
w_times_update$tomoa_skip <- as.logical(w_times_update$tomoa_skip)
for(i in 1:nrow(m_times_update)){
  
  row <- w_times_update[i, ]
  full_name <- paste(row$fname, row$lname, sep = " ")

  if (!is.na(row$tomoa_skip) && row$tomoa_skip && is.null(query(m_hm, full_name))) {
    insert(m_hm, full_name, row$event_id)
  }

  else if (!is.null(query(m_hm, full_name))) {
    if (as.double(row$event_id) >= as.double(query(m_hm, full_name))) {
      w_times_update[i, "tomoa_skip"] <- TRUE
    }
  }
  
}


