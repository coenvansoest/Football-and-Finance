library(chromote)
library(rvest)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(stringi)

scrape_transfers <- function(url, max_clicks = 25) {
  # Start a Chrome session
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  Sys.sleep(5)  # Wait for the initial page load
  
  # Function to Click "Show More" Until It Disappears
  click_show_more_until_end <- function(session, max_clicks) {
    for (i in 1:max_clicks) {
      Sys.sleep(3)  # Short wait before checking
      
      # Check if the "Show More" button exists
      button_exists <- session$Runtime$evaluate('document.querySelector(".transferTab__moreLink") !== null')$result$value
      
      if (button_exists) {
        # Click the button
        session$Runtime$evaluate('document.querySelector(".transferTab__moreLink").click();')
        Sys.sleep(3)  # Allow time for content to load
      } else {
        break  # Stop if button is gone
      }
    }
  }
  
  # Keep clicking until all data loads
  click_show_more_until_end(b, max_clicks)
  Sys.sleep(5)  # Final wait to ensure all content is present
  
  # Get updated page source
  page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  html_page <- read_html(page_source)
  
  # Extract Transfer Data
  transfer_dates <- html_page %>% html_nodes(".transferTab__date") %>% html_text()
  player_names <- html_page %>% html_nodes(".transferTab__player") %>% html_text()
  
  # Combine into a DataFrame
  transfer_data <- data.frame(
    Date = transfer_dates,
    Player = player_names,
    stringsAsFactors = FALSE
  )
  
  transfer <- transfer_data[-1,]
  
  # Define common last name prefixes
  prefixes <- c("van", "de", "von", "da", "der", "la", "di", "le")
  
  # Function to correctly swap first and last name
  swap_name <- function(name) {
    parts <- unlist(strsplit(name, " "))  # Split into words
    n <- length(parts)
    
    if (n > 1) {
      # Check if the first word is a prefix
      if (parts[1] %in% prefixes & n > 2) {
        return(paste(parts[n], paste(parts[1:(n-1)], collapse = " ")))
      } else {
        return(paste(parts[n], paste(parts[-n], collapse = " ")))
      }
    } else {
      return(name)
    }
  }
  
  # Apply the function to the "Player" column
  transfer$Player <- sapply(transfer$Player, swap_name)
  
  # Now we adjust the "mutate()" block to bump both November & December into the *following* year
  transfers <- transfer %>%
    mutate(date = as.Date(Date, format = "%d.%m.%Y")) %>%
    mutate(
      year  = year(date),
      month = month(date),
      # Shift both November (11) and December (12) to the next year
      adjusted_year = ifelse(month >= 11, year + 1, year),
      # Group months <= 4 or >= 11 into the first half of the "adjusted" season
      period_id = (adjusted_year - min(adjusted_year)) * 2 +
        ifelse(month <= 4 | month >= 11, 1, 2)
    ) %>%
    select(-adjusted_year, -month, -year, -Date) %>%
    rename(player_name = Player)
  
  # Close the Chrome session
  b$close()
  
  return(transfers)
}

# Example usage:
transfers <- scrape_transfers("https://www.flashscore.com/team/trabzonspor/MmsYDc03/transfers/", 250)
transfers$player_name<- stri_trans_general(transfers$player_name, "Latin-ASCII")
write.xlsx(transfers, file = "../trabzonspordates.xlsx")
#transfers<-read_xlsx('../besiktasdates.xlsx')
