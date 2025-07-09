total <- total %>% mutate(player_name = trimws(player_name))
total<-total%>%
  filter(!grepl('End', transfer_fee))
transfers <- transfers %>% mutate(player_name = trimws(player_name))


full<-total%>%
  left_join(transfers, by = c('period_id','player_name'))


unmatched <- anti_join(total, transfers, by = c("period_id", "player_name"))
print(unmatched)
miss<-unmatched%>%group_by(season)%>%summarise(count = n())
print(miss)
full<-full%>%na.omit()

prefixes <- c("van", "de", "von", "da", "der", "la", "di", "le")
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
transfers2<-transfers
transfers2$player_name <- sapply(transfers$player_name, swap_name)
extra<-unmatched%>%
  left_join(transfers2, by=c('period_id', 'player_name'))
extra<-extra%>%na.omit()
full<-full%>%rbind(extra)


duplicate_counts <- full %>%
  group_by(transfer_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

print(duplicate_counts)

full <- full %>%
  distinct(transfer_id, .keep_all = T)

write.xlsx(full, "../trabzonsporcomplete.xlsx")
