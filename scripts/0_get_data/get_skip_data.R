# Load the required library 
library(googlesheets4)
library(dplyr)
library(r2r)

# Read sheets from Google drive
#w_tomoa_skip_data <- read_sheet('https://docs.google.com/spreadsheets/d/19m6dNDvdVuqtaQsVS0mt8r2iElL7Pe95vkm6cWiPq_E/edit?usp=sharing')
#m_tomoa_skip_data <- read_sheet('https://docs.google.com/spreadsheets/d/15vR-ZX_4U7oRExClp_SWz88g3hWKRGjETQssIACAgSk/edit?usp=sharing')

#Create empty hashmaps
m_hm <- hashmap()
w_hm <- hashmap()

#merge the data we gathered from IFCS videos into our scraped data
#mens_merged_data <- merge(m_times, m_tomoa_skip_data[, c("event_id", "fname", "lname", "tomoa_skip")], by = c("event_id", "fname", "lname"), all.x = TRUE)
womens_merged_data <- merge(w_times, w_tomoa_skip_data[, c("event_id", "fname", "lname", "tomoa_skip")], by = c("event_id", "fname", "lname"), all.x = TRUE)

# The earliest event we have tomoa skip data is 1048, so we assume all earlier events as false
mens_merged_data$tomoa_skip[as.double(mens_merged_data$event_id) < 1048] <- FALSE
womens_merged_data$tomoa_skip[as.double(womens_merged_data$event_id) < 1048] <- FALSE

# Sort the merged data set by event id so we can properly compare event IDs
mens_merged_data <-  mens_merged_data[order(as.double(mens_merged_data$event_id)), ]
womens_merged_data <- womens_merged_data[order(as.double(womens_merged_data$event_id)), ]


# For mens data
for(i in 1:nrow(mens_merged_data)){
  
  row <- mens_merged_data[i, ]
  full_name <- paste(row$fname, row$lname, sep = " ")
  
  # If tomoa_skip is TRUE and querying the hash map w/ climber name is null,
  # we haven't encountered this climber doing skip yet and insert their name w/ event ID into the hashmap.
  # This hashmap will store climbers first occurence of tamoa skip
  if(row$tomoa_skip == "TRUE" && is.null(query(m_hm, full_name))) {
    insert(m_hm, full_name, row$event_id)
  }
  # If query is not null we have seen climber do skip before
  else if(!is.null(query(m_hm, full_name))){
    # So compare current event id to the query event id in the hashmap (should always be the case but still check).
    # If so, this is after their first skip occurence so set tamoa_skip value to TRUE
    if(as.double(row$event_id) >= as.double(query(m_hm, full_name))){
      mens_merged_data[i, "tomoa_skip"] <- "TRUE"
    }
    
  }
     
}

# For women's data
for(i in 1:nrow(womens_merged_data)){
  
  row <- womens_merged_data[i, ]
  full_name <- paste(row$fname, row$lname, sep = " ")
  
  # If tomoa_skip is TRUE and querying the hash map w/ climber name is null,
  # we haven't encountered this climber doing skip yet and insert their name w/ event ID into the hashmap.
  # This hashmap will store climbers first occurence of tamoa skip
  if(row$tomoa_skip == "TRUE" && is.null(query(m_hm, full_name))) {
    insert(m_hm, full_name, row$event_id)
  }
  # If query is not null we have seen climber do skip before
  else if(!is.null(query(m_hm, full_name))){
    # So compare current event id to the query event id in the hashmap (should always be the case but still check).
    # If so, this is after their first skip occurence so set tamoa_skip value to TRUE
    if(as.double(row$event_id) >= as.double(query(m_hm, full_name))){
      womens_merged_data[i, "tomoa_skip"] <- "TRUE"
    }
    
  }
  
}


m_times <- mens_merged_data
w_times <- womens_merged_data




