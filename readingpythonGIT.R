library(readxl)
library(stringi)
library(dplyr)

#change for every club
incoming_transfers_df <- read_excel("Trabzonspor_transfers.xlsx", sheet = "Incoming Transfers")
outgoing_transfers_df <- read_excel("Trabzonspor_transfers.xlsx", sheet = "Outgoing Transfers")

#making sure the seasons are correct
transfer_order <- c("Summer","Winter")

incoming_transfers_df <- incoming_transfers_df %>%
  mutate(
    transfer_window = factor(transfer_window, levels = transfer_order),
    window_code = if_else(transfer_window == "Summer", 0L, 1L),
    period_id = 22L + 2L * (season - 1995L) + window_code
  ) %>%
  select(-window_code)

outgoing_transfers_df <- outgoing_transfers_df %>%
  mutate(
    transfer_window = factor(transfer_window, levels = transfer_order),
    window_code = if_else(transfer_window == "Summer", 0L, 1L),
    period_id = 22L + 2L * (season - 1995L) + window_code
  ) %>%
  select(-window_code)

incoming_transfers_df$transfer_direction <- "Incoming"
outgoing_transfers_df$transfer_direction <- "Outgoing"

total <- rbind(incoming_transfers_df, outgoing_transfers_df)
total$player_name <- stri_trans_general(total$player_name, "Latin-ASCII")


