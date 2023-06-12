library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

## Start here -----
# Best times per event
time <- m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  # Handle case where all values in time are NA 
  summarise(best_time = if (all(is.na(time))) NA else min(time, na.rm = TRUE)) |>
  filter(!is.na(best_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, best_time, color = tomoa_skip)) +
  facet_wrap(.~sex) + 
  ggtitle("Best times per event")

3
# Qualifying times for men and women
m_times_update |>
  bind_rows(w_times_update) |>
  # Does this create duplicates for best_qual if lane_a and lane_b are not NA?
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  #Remove outliers -- OK?
  filter(!is.na(qual_time) & qual_time < 60)  |>
  ggplot() +
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip)) + 
  facet_wrap(.~sex) + 
  ggtitle("Qualifying Times for Men and Women")

# Final times for men and women
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  # There are 3 outliers of ~100 seconds
  filter(!is.na(final_time) & final_time < 95)  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip)) + 
  facet_wrap(.~sex) + 
  ggtitle("Final Times for Men and Women")

# Mens qualifier times 
qual_plot <-m_times_update |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  #Remove outliers -- OK?
  filter(!is.na(qual_time) & qual_time < 60)  |>
  ggplot() +
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Men") + 
  theme(legend.position = "none")

# Mens final times
final_plot <- m_times_update |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  # There are 3 outliers of ~100 seconds
  filter(!is.na(final_time) & final_time < 95)  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Men")

plot_grid(qual_plot, final_plot, ncol = 2)

# Womens qualifier times 
qual_plot <-w_times_update |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  # Remove outliers
  filter(!is.na(qual_time) & qual_time < 60)  |>
  ggplot() +
  # Hiding legends to prevent compressing
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Women") + 
  theme(legend.position = "none")

# Womens final times
final_plot <- w_times_update |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  # There are 3 outliers of ~100 seconds
  filter(!is.na(final_time) & final_time < 95)  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Women")

plot_grid(qual_plot, final_plot, ncol = 2)


## Notes--
# Plot of users of skip over time ?

# Somehow visualise falls?
# test <- m_times_update |>
#   bind_rows(w_times_update) |>
#   pivot_longer(cols = c("fall_qual", "fall_final", "fall_lane_a", "fall_lane_b"), names_to = "round", values_to = "falls") |>
#   group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
#   filter(!is.na(falls)) |>
#   ggplot() +
#   geom_bar(aes(year, fill = falls), position = "dodge") +
#   labs(x = "Year", y = "Count")






