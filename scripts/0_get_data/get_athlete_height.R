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
  
  if (!is.null(anchor_tag)) {
    # Get the link URL
    link_url <- html_attr(anchor_tag, "href")
    
    # Navigate to athlete page
    remote_driver$navigate(link_url)
    Sys.sleep(5)  
    
    # Scrape data on the navigated page
    
    
    # Go back to table page
    remote_driver$goBack()
    Sys.sleep(3)
  }
}


# Close the remote driver and stop the Selenium server
remote_driver$close()
driver$server$stop()