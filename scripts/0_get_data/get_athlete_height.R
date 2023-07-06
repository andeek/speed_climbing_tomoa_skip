library(RSelenium)
library(rvest) ## scrapee tables

# Start the Selenium server and create a remote driver
driver <- rsDriver(browser = "firefox", chromever = NULL, iedrver = NULL, verbose = TRUE)
remote_driver <- driver$client

# Navigate to the webpage
remote_driver$navigate("https://components.ifsc-climbing.org/ranking-complete/?cup=&category=2#")
Sys.sleep(3)

# Find table element
table_elm <- remote_driver$findElement(using = "css", value = "#table_id")

# Get the HTML content of the table
table_html <- table_elm$getElementAttribute("innerHTML")[[1]]

# Parse the HTML table
table <- read_html(table_html)
rows <- html_nodes(table, "tr")

# Empty storage
df_heights_men <- data.frame(name = character())

# Iterate through each row in the table
for (row in rows) {
  # Find the first anchor tag within the row
  anchor_tag <- html_node(row, "a")
  
  # Check if an anchor tag is found
  if (!is.null(anchor_tag)) {
    # Get the link URL
    link_url <- html_attr(anchor_tag, "href")
    
    # Navigate to the link URL
    remote_driver$navigate(link_url)
    Sys.sleep(5)  # Increase sleep duration if needed
    
    # Scrape data on the navigated page
    tryCatch({
      # Attempt to find the .name element
      name_element <- remote_driver$findElement(using = "css", value = ".name")
      
      # Check if the element is found
      if (!is.null(name_element)) {
        climber_name <- name_element$getElementText()
        
        # Create a new row with the climber name
        new_row <- data.frame(name = climber_name, stringsAsFactors = FALSE)
        
        # Add the new row to the dataframe
        df_heights_men <- rbind(df_heights_men, new_row)
      } else {
        # Handle the case when .name element is not found
        cat("Error: .name element not found on the page\n")
      }
    }, error = function(e) {
      # Handle any other errors gracefully (e.g., print an error message)
      cat("Error occurred while scraping data:", conditionMessage(e), "\n")
    })
    
    # Go back to the previous page
    remote_driver$goBack()
    Sys.sleep(3)  # Wait for the page to load
  }
}




# Close the remote driver and stop the Selenium server
remote_driver$close()
driver$server$stop()