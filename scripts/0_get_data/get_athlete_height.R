library(RSelenium)
library(rvest) ## scrapee tables

# Start the Selenium server and create a remote driver
driver <- rsDriver(browser = "firefox", chromever = NULL, iedrver = NULL, verbose = TRUE)
remote_driver <- driver$client

# Navigate to the webpage
remote_driver$navigate("https://components.ifsc-climbing.org/ranking-complete/?cup=&category=2&year=2022") # TODO: loop through multiple years to get more climbers
Sys.sleep(3)

# Find table element
table_elm <- remote_driver$findElement(using = "css", value = "#table_id")

# Get the HTML content of the table
table_html <- table_elm$getElementAttribute("innerHTML")[[1]]

# Parse the HTML table
table <- read_html(table_html)
rows <- html_nodes(table, "tr")
rows <- rows[-1]

# Empty storage
df_heights_men <- data.frame(name = character(), height = numeric(), armspan = numeric())
colnames(df_heights_men) <- c("name", "height", "armspan")

# Iterate through each row in the table
for (row in rows) {
  # Find the first anchor tag within the row
  anchor_tag <- html_node(row, "a")
  
  if (!is.null(anchor_tag)) {
    # Get the link URL
    link_url <- html_attr(anchor_tag, "href")
    
    # Navigate to athlete page
    remote_driver$navigate(link_url)
    Sys.sleep(5)  
    
    # Scrape data on the navigated page
    personal_info_subtitle <- remote_driver$findElements(using = "css", value = ".personal-info .subtitle")
    personal_info_paragraph <- remote_driver$findElements(using = "css", value = ".personal-info .paragraph")
    climber_name <- remote_driver$findElement(using = "css", value = ".name")
    climber_name <- climber_name$getElementText()
    height <- ""
    arm_span <- ""
    
    # personal_info_subtitle[[1]]$getElementText()
    # Loop through and get the text elements
    for (i in 1:length(personal_info_subtitle)) {
      if (length(personal_info_subtitle) > 0) {
        print(personal_info_subtitle)
        subtitle_text <- personal_info_subtitle[[i]]$getElementText()
        if (subtitle_text == "HEIGHT") {
          height <- personal_info_paragraph[[i]]$getElementText()
        } else if (subtitle_text == "ARM SPAN") {
          arm_span <- personal_info_paragraph[[i]]$getElementText()
        }
      }
    }
    
    height <- ifelse(height=="", NA, as.numeric(height))
    arm_span <- ifelse(height=="", NA, as.numeric(arm_span))
    
    df_heights_men <- rbind(df_heights_men, c(climber_name, height, arm_span))
    
    # Go back to table page
    remote_driver$goBack()
    Sys.sleep(3)
  }
}


# Close the remote driver and stop the Selenium server
remote_driver$close()
driver$server$stop()
