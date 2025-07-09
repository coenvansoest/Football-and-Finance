library(readxl)
library(stringi)
library(dplyr)

incoming_transfers_df <- read_excel("Trabzonspor_transfers.xlsx", sheet = "Incoming Transfers")
outgoing_transfers_df <- read_excel("Trabzonspor_transfers.xlsx", sheet = "Outgoing Transfers")

# Factor the windows so that "Summer" is level 0, "Winter" is level 1
transfer_order <- c("Summer","Winter")

incoming_transfers_df <- incoming_transfers_df %>%
  mutate(
    transfer_window = factor(transfer_window, levels = transfer_order),
    # 0 for Summer, 1 for Winter
    window_code = if_else(transfer_window == "Summer", 0L, 1L),
    # Start at period_id = 16 in Summer 1995
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

# Add an indicator for incoming/outgoing
incoming_transfers_df$transfer_direction <- "Incoming"
outgoing_transfers_df$transfer_direction <- "Outgoing"

# Combine into a total and fix any player name accents
total <- rbind(incoming_transfers_df, outgoing_transfers_df)
total$player_name <- stri_trans_general(total$player_name, "Latin-ASCII")


