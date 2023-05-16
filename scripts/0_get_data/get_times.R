library(RSelenium) ## connect to websites via selenium server
library(rvest) ## scrapee tables
library(dplyr) ## data munge
library(tidyr) ## data tidy
library(readr) ## parse_number
library(lubridate) ## dates

## load events
load("data/events.Rdata")

## remote driver
# driver <- rsDriver(browser = "chrome", chromever = "102.0.5005.27")
driver <- rsDriver(browser = "firefox", chromever = NULL, iedrver = NULL, verbose = TRUE)
remote_driver <- driver$client

## empty storage
df_overall <- data.frame()
df_qual <- data.frame()
df_final <- data.frame()
for(i in seq_len(nrow(events))) {
  e_id <- events[i, "event_id"]
  if(!is.na(e_id)) {
    ## this is a speed event
    ## navigate to results
    ## mens times ----
    remote_driver$navigate(paste0("https://components.ifsc-climbing.org/result-complete/?event=", e_id, "&result=2"))
    Sys.sleep(2)
    
    ## overall table
    table_elm <- remote_driver$findElements(using = "css", value = "#table_id")
    table_overall <- read_html(table_elm[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
    df_overall_men <- rvest::html_table(table_overall)[[1]]
    
    ## cleanup table
    if(ncol(df_overall_men) > 0) {
      if(ncol(df_overall_men) == 6) {
        df_overall_men |>
          rename(rank = Rank, fname = Name, lname = 3, country = Country, best_qual = Qualification, final = Final) |>
          mutate(fname = tolower(fname), lname = tolower(lname)) |>
          mutate(fall_qual = tolower(best_qual) == "fall") |>
          mutate(false_start_qual = tolower(best_qual) == "false start") |>
          mutate(fall_final = tolower(final) == "fall") |>
          mutate(false_start_final = tolower(final) == "false start") |>
          mutate(best_qual = ifelse(tolower(best_qual) == "fall", NA, best_qual),
                 final = ifelse(tolower(final) == "fall", NA, final)) |>
          mutate(best_qual = ifelse(class(best_qual) == "character", parse_number(best_qual), best_qual)) |>
          mutate(final = as.numeric(final)) |>
          mutate(sex = "M", event_id = e_id) |>
          bind_rows(df_overall) -> df_overall
      } else {
        df_overall_men |>
          rename(rank = Rank, fname = Name, lname = 3, country = Country, best_qual = Qualification) |>
          mutate(fname = tolower(fname), lname = tolower(lname)) |>
          mutate(fall_qual = tolower(best_qual) == "fall") |>
          mutate(false_start_qual = tolower(best_qual) == "false start") |>
          mutate(best_qual = ifelse(tolower(best_qual) == "fall", NA, best_qual)) |>
          mutate(best_qual = ifelse(class(best_qual) == "character", parse_number(best_qual), best_qual)) |>
          mutate(final = NA) |>
          mutate(sex = "M", event_id = e_id) |>
          bind_rows(df_overall) -> df_overall
      }
    }
    
    ## qualifiers
    qual_btn <- remote_driver$findElements(using = 'xpath', "//a[.='Qualification']")
    if(length(qual_btn) > 0) {
      qual_btn[[1]]$clickElement()
      Sys.sleep(2)
      table_elm <- remote_driver$findElements(using = "css", value = "#table_id")
      table_qual <- read_html(table_elm[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
      df_qual_men <- rvest::html_table(table_qual)[[1]]
    } else {
      df_qual_men <- data.frame()
    }
    
    ## cleanup
    if(ncol(df_qual_men) > 0) {
      if(ncol(df_qual_men) == 6) {
        ## sometimes there are not actually times in the qualifiers (old results)
        df_qual_men |>
          rename(rank = Rank, fname = Name, lname = 3, country = Country, lane_a = `Lane A`, lane_b = `Lane B`) |>
          mutate(fname = tolower(fname), lname = tolower(lname)) |>
          mutate(fall_lane_a = tolower(lane_a) == "fall") |>
          mutate(false_start_lane_a = tolower(lane_a) == "false start") |>
          mutate(fall_lane_b = tolower(lane_b) == "fall") |>
          mutate(false_start_lane_b = tolower(lane_b) == "false start") |>
          mutate(lane_a = ifelse(tolower(lane_a) == "fall", NA, lane_a),
                 lane_b = ifelse(tolower(lane_b) == "fall", NA, lane_b)) |>
          mutate(across(.cols = c(lane_a, lane_b), .fns = as.numeric)) |>
          mutate(sex = "M", event_id = e_id) |>
          bind_rows(df_qual) -> df_qual
      } else {
        df_qual_men |>
          rename(rank = Rank, fname = Name, lname = 3, country = Country) |>
          mutate(fname = tolower(fname), lname = tolower(lname), lane_a = NA, lane_b = NA) |>
          mutate(sex = "M", event_id = e_id) |>
          bind_rows(df_qual) -> df_qual
      }
    }
    
    ## finals
    final_btn <- remote_driver$findElements(using = 'xpath', "//a[.='Final']")
    if(length(final_btn) > 0) {
      final_btn[[1]]$clickElement()
      Sys.sleep(2)
      table_elm <- remote_driver$findElements(using = "css", value = "#table_id")
      table_final <- read_html(table_elm[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
      df_final_men <- rvest::html_table(table_final)[[1]]
    } else {
      df_final_men <- data.frame()
    }
    
    ## cleanup table
    if(ncol(df_final_men) > 0) {
      if("1/8" %in% names(df_final_men)) {
        df_final_men |>
          rename(rank = Rank, fname = Name, lname = 3, country = Country, first_round = `1/8`, quarter = `1/4`, semi = `1/2`, small_final = `Small Final`, big_final = Final) |>
          mutate(fname = tolower(fname), lname = tolower(lname)) |>
          mutate(across(first_round:big_final , function(col) { tolower(col) == "fall" }, .names = "fall_{.col}")) |>
          mutate(across(first_round:big_final , function(col) { tolower(col) == "false start" }, .names = "false_start_{.col}")) |>
          mutate(across(first_round:big_final, function(col) { ifelse(col == "FALL", NA, col)})) |>
          mutate(across(first_round:big_final, as.numeric)) |>
          mutate(sex = "M", event_id = e_id) |>
          bind_rows(df_final) -> df_final
      } else {
        df_final_men |>
          rename(rank = Rank, fname = Name, lname = 3, country = Country, quarter = `1/4`, semi = `1/2`, small_final = `Small Final`, big_final = Final) |>
          mutate(fname = tolower(fname), lname = tolower(lname)) |>
          mutate(across(quarter:big_final , function(col) { tolower(col) == "fall" }, .names = "fall_{.col}")) |>
          mutate(across(quarter:big_final , function(col) { tolower(col) == "false start" }, .names = "false_start_{.col}")) |>
          mutate(across(quarter:big_final, function(col) { ifelse(col == "FALL", NA, col)})) |>
          mutate(across(quarter:big_final, as.numeric)) |>
          mutate(sex = "M", first_round = NA, fall_first_round = NA, false_start_first_round = NA, event_id = e_id) |>
          bind_rows(df_final) -> df_final
      }
    }
    
    ## womens times ----
    # remote_driver$navigate(paste0("https://components.ifsc-climbing.org/result-complete/?event=", e_id, "&result=6"))
    # Sys.sleep(2)
    # 
    # ## overall table
    # table_elm <- remote_driver$findElements(using = "css", value = "#table_id")
    # table_overall <- read_html(table_elm[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
    # df_overall_women <- rvest::html_table(table_overall)[[1]]
    # 
    # ## cleanup table
    # if(ncol(df_overall_women) > 0) {
    #   if(ncol(df_overall_women) == 6) {
    #     df_overall_women |>
    #       rename(rank = Rank, fname = Name, lname = 3, country = Country, best_qual = Qualification, final = Final) |>
    #       mutate(fname = tolower(fname), lname = tolower(lname)) |>
    #       mutate(best_qual = ifelse(tolower(best_qual) == "fall", NaN, best_qual),
    #              final = ifelse(tolower(final) == "fall", NaN, final)) |>
    #       mutate(best_qual = ifelse(class(best_qual) == "character", parse_number(best_qual), best_qual)) |>
    #       mutate(final = as.numeric(final)) |>
    #       mutate(sex = "F", event_id = e_id) |>
    #       bind_rows(df_overall) -> df_overall
    #   } else {
    #     df_overall_women |>
    #       rename(rank = Rank, fname = Name, lname = 3, country = Country, best_qual = Qualification) |>
    #       mutate(fname = tolower(fname), lname = tolower(lname)) |>
    #       mutate(best_qual = ifelse(tolower(best_qual) == "fall", NaN, best_qual)) |>
    #       mutate(best_qual = ifelse(class(best_qual) == "character", parse_number(best_qual), best_qual)) |>
    #       mutate(final = NA) |>
    #       mutate(sex = "F", event_id = e_id) |>
    #       bind_rows(df_overall) -> df_overall
    #   }
    # }
    # 
    # ## qualifiers
    # qual_btn <- remote_driver$findElements(using = 'xpath', "//a[.='Qualification']")
    # if(length(qual_btn) > 0) {
    #   qual_btn[[1]]$clickElement()
    #   Sys.sleep(2)
    #   table_elm <- remote_driver$findElements(using = "css", value = "#table_id")
    #   table_qual <- read_html(table_elm[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
    #   df_qual_women <- rvest::html_table(table_qual)[[1]]
    # } else {
    #   df_qual_women <- data.frame()
    # }
    # 
    # ## cleanup
    # if(ncol(df_qual_women) > 0) {
    #   if(ncol(df_qual_women) == 6) {
    #     ## sometimes there are not actually times in the qualifiers (old results)
    #     df_qual_women |>
    #       rename(rank = Rank, fname = Name, lname = 3, country = Country, lane_a = `Lane A`, lane_b = `Lane B`) |>
    #       mutate(fname = tolower(fname), lname = tolower(lname)) |>
    #       mutate(lane_a = ifelse(tolower(lane_a) == "fall", NaN, lane_a),
    #              lane_b = ifelse(tolower(lane_b) == "fall", NaN, lane_b)) |>
    #       mutate(across(.cols = c(lane_a, lane_b), .fns = as.numeric)) |>
    #       mutate(sex = "M", event_id = e_id) |>
    #       bind_rows(df_qual) -> df_qual
    #   } else {
    #     df_qual_women |>
    #       rename(rank = Rank, fname = Name, lname = 3, country = Country) |>
    #       mutate(fname = tolower(fname), lname = tolower(lname), lane_a = NA, lane_b = NA) |>
    #       mutate(sex = "M", event_id = e_id) |>
    #       bind_rows(df_qual) -> df_qual
    #   }
    # }
    # 
    # ## finals
    # final_btn <- remote_driver$findElements(using = 'xpath', "//a[.='Final']")
    # if(length(final_btn) > 0) {
    #   final_btn[[1]]$clickElement()
    #   Sys.sleep(2)
    #   table_elm <- remote_driver$findElements(using = "css", value = "#table_id")
    #   table_final <- read_html(table_elm[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
    #   df_final_women <- rvest::html_table(table_final)[[1]]
    # } else {
    #   df_final_women <- data.frame()
    # }
    # 
    # ## cleanup table
    # if(ncol(df_final_women) > 0) {
    #   if("1/8" %in% names(df_final_women)) {
    #     df_final_women |>
    #       rename(rank = Rank, fname = Name, lname = 3, country = Country, first_round = `1/8`, quarter = `1/4`, semi = `1/2`, small_final = `Small Final`, big_final = Final) |>
    #       mutate(fname = tolower(fname), lname = tolower(lname)) |>
    #       mutate(across(first_round:big_final, function(col) { ifelse(col == "FALL", NaN, col)})) |>
    #       mutate(across(first_round:big_final, as.numeric)) |>
    #       mutate(sex = "M", event_id = e_id) |>
    #       bind_rows(df_final) -> df_final
    #   } else {
    #     df_final_women |>
    #       rename(rank = Rank, fname = Name, lname = 3, country = Country, quarter = `1/4`, semi = `1/2`, small_final = `Small Final`, big_final = Final) |>
    #       mutate(fname = tolower(fname), lname = tolower(lname)) |>
    #       mutate(across(first_round:big_final, function(col) { ifelse(col == "FALL", NaN, col)})) |>
    #       mutate(across(first_round:big_final, as.numeric)) |>
    #       mutate(sex = "M", first_round = NA, event_id = e_id) |>
    #       bind_rows(df_final) -> df_final
    #   }
    # }
    
  }
}

df_overall |>
  left_join(df_qual) |>
  left_join(df_final) -> times

## save times object
save(times, file = "data/times.Rdata")

## cleanup
remote_driver$close()
driver$server$stop()
