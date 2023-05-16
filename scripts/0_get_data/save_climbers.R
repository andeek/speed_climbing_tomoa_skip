library(dplyr) ## data munge
library(tidyr) ## data tidy
library(readr) ## parse_number
library(googledrive) ## google drive interface

## load events and times
load("data/events.Rdata")
load("data/times.Rdata")

## restructure data for Tomoa Skip data collection
times |>
  filter(rank <= 16) |> ## people in the final
  select(event_id, rank, fname, lname, country) |>
  left_join(events, by = "event_id") |>
  select(event_id, event, start_date, end_date, year, rank, fname, lname, country) |>
  filter(year >= 2018) |>
  mutate(tomoa_skip = NA) -> data_collection

## save locally
write_csv(data_collection, "data/data_collection.csv")

## upload to drive
data_collection_drv <- drive_upload("data/data_collection.csv", path = "speed_climbing_tomoa_skip_data_collection", type = "spreadsheet")

## make shared
drive_share(data_collection_drv, role = "writer", type = "user", emailAddress = "Caleb.Chou@colostate.edu")
