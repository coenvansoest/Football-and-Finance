library(chromote)
library(rvest)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(stringi)

scrape_transfers <- function(url, max_clicks = 25) {
  #chrome session
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  Sys.sleep(5)  #slow to load page
  
  # press show more 
  click_show_more_until_end <- function(session, max_clicks) {
    for (i in 1:max_clicks) {
      Sys.sleep(3)  # slow to load
      
      #true or false whether button exists
      button_exists <- session$Runtime$evaluate('document.querySelector(".transferTab__moreLink") !== null')$result$value
      
      if (button_exists) {
        session$Runtime$evaluate('document.querySelector(".transferTab__moreLink").click();')
        Sys.sleep(3)
      } else {
        break  #finish if there is no more show more
      }
    }
  }
  
  click_show_more_until_end(b, max_clicks)
  Sys.sleep(5)  
  
  #extract from updated page
  page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  html_page <- read_html(page_source)
  
  #extract the transfer data
  transfer_dates <- html_page %>% html_nodes(".transferTab__date") %>% html_text()
  player_names <- html_page %>% html_nodes(".transferTab__player") %>% html_text()
  
  transfer_data <- data.frame(
    Date = transfer_dates,
    Player = player_names,
    stringsAsFactors = FALSE
  )
  
  transfer <- transfer_data[-1,]
  
  #correct name format
  prefixes <- c("van", "de", "von", "da", "der", "la", "di", "le")
  
  swap_name <- function(name) {
    parts <- unlist(strsplit(name, " "))
    n <- length(parts)
    
    if (n > 1) {
      if (parts[1] %in% prefixes & n > 2) {
        return(paste(parts[n], paste(parts[1:(n-1)], collapse = " ")))
      } else {
        return(paste(parts[n], paste(parts[-n], collapse = " ")))
      }
    } else {
      return(name)
    }
  }
  
  transfer$Player <- sapply(transfer$Player, swap_name)
  
  #put seasons correctly
  transfers <- transfer %>%
    mutate(date = as.Date(Date, format = "%d.%m.%Y")) %>%
    mutate(
      year  = year(date),
      month = month(date),
      adjusted_year = ifelse(month >= 11, year + 1, year),
      period_id = (adjusted_year - min(adjusted_year)) * 2 +
        ifelse(month <= 4 | month >= 11, 1, 2)
    ) %>%
    select(-adjusted_year, -month, -year, -Date) %>%
    rename(player_name = Player)
  
  b$close()
  
  return(transfers)
}

transfers <- scrape_transfers("https://www.flashscore.com/team/trabzonspor/MmsYDc03/transfers/", 250)
transfers$player_name<- stri_trans_general(transfers$player_name, "Latin-ASCII")
write.xlsx(transfers, file = "../trabzonspordates.xlsx")
#transfers<-read_xlsx('../besiktasdates.xlsx')
