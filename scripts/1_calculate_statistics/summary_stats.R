# Required library
library(dplyr)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

# Average times for finals
m_times_update |>
  group_by(year) |>
  filter(rank <= 16) |>
  summarise(
    num_entries = n(),
    avg_final_time_ts = mean(final[tomoa_skip], na.rm = TRUE),
    avg_final_time_no_ts = mean(final[!tomoa_skip], na.rm = TRUE),
    avg_final_time = mean(final, na.rm = TRUE)
  )

w_times_update |>
  group_by(year) |>
  filter(rank <= 16) |>
  summarise(
    num_entries = n(),
    avg_final_time_ts = mean(final[tomoa_skip], na.rm = TRUE),
    avg_final_time_no_ts = mean(final[!tomoa_skip], na.rm = TRUE),
    avg_final_time = mean(final, na.rm = TRUE)
  )

# Average times for qualifiers
m_times_update |>
  group_by(year) |>
  summarise(
    num_entries = n(),
    avg_qual_time_ts = mean(best_qual[!tomoa_skip], na.rm = TRUE),
    avg_qual_time_no_ts = mean(best_qual[tomoa_skip], na.rm = TRUE),
    avg_qual_time = mean(best_qual, na.rm = TRUE)
  )
w_times_update |>
  group_by(year) |>
  summarise(
    num_entries = n(),
    avg_qual_time_ts = mean(best_qual[!tomoa_skip], na.rm = TRUE),
    avg_qual_time_no_ts = mean(best_qual[tomoa_skip], na.rm = TRUE),
    avg_qual_time = mean(best_qual, na.rm = TRUE)
  )

# Falls and false starts in qualifier
m_times_update |> 
  group_by(year) |> 
  summarise(
    num_entries = n(), 
    num_falls_qual_ts = sum(fall_qual & tomoa_skip, na.rm = TRUE), 
    num_falls_qual_no_ts = sum(fall_qual & !tomoa_skip, na.rm = TRUE),
    num_FS_qual_ts = sum(false_start_qual & tomoa_skip, na.rm = TRUE),
    num_FS_qual_no_ts = sum(false_start_qual & !tomoa_skip, na.rm = TRUE),
    )

w_times_update |> 
  group_by(year) |> 
  summarise(
    num_entries = n(), 
    num_falls_qual_ts = sum(fall_qual & tomoa_skip, na.rm = TRUE), 
    num_falls_qual_no_ts = sum(fall_qual & !tomoa_skip, na.rm = TRUE),
    num_FS_qual_ts = sum(false_start_qual & tomoa_skip, na.rm = TRUE),
    num_FS_qual_no_ts = sum(false_start_qual & !tomoa_skip, na.rm = TRUE),
)

# Falls and false starts in final
m_times_update |>
  group_by(year) |>
  filter(rank <= 16) |>
  summarise(
    num_entries = n(),
    num_falls_final_ts = sum(fall_final & tomoa_skip, na.rm = TRUE),
    num_falls_final_no_ts = sum(fall_final & !tomoa_skip, na.rm = TRUE),
    num_TS_final_ts = sum(fall_final & tomoa_skip, na.rm = TRUE),
    num_TS_final_no_ts = sum(fall_final & !tomoa_skip, na.rm = TRUE)
)

w_times_update |>
  group_by(year) |>
  filter(rank <= 16) |>
  summarise(
    num_entries = n(),
    num_falls_final_ts = sum(fall_final & tomoa_skip, na.rm = TRUE),
    num_falls_final_no_ts = sum(fall_final & !tomoa_skip, na.rm = TRUE),
    num_TS_final_ts = sum(fall_final & tomoa_skip, na.rm = TRUE),
    num_TS_final_no_ts = sum(fall_final & !tomoa_skip, na.rm = TRUE)
)


# Average number of climbers who use tomoa skip in a final
m_times_update |> 
  filter(rank <= 16) |> 
  group_by(year) |> 
  summarise(
    num_climbers_final_no_ts = sum(!tomoa_skip), 
    num_climbers_final_ts = sum(tomoa_skip), 
    num_events = length(unique(event_id))
    ) |> 
  mutate(rate_no_ts = num_climbers_final_no_ts / num_events, rate_ts = num_climbers_final_ts / num_events)

w_times_update |> 
  filter(rank <= 16) |> 
  group_by(year) |> 
  summarise(
    num_climbers_final_no_ts = sum(!tomoa_skip), 
    num_climbers_final_ts = sum(tomoa_skip), 
    num_events = length(unique(event_id))
  ) |> 
  mutate(rate_no_ts = num_climbers_final_no_ts / num_events, rate_ts = num_climbers_final_ts / num_events)












