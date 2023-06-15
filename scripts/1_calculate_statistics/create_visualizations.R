library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

## Start here -----
# Best times per event
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  # Handle case where all values in time are NA 
  summarise(best_time = if (all(is.na(time))) NA else min(time, na.rm = TRUE)) |>
  filter(!is.na(best_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, best_time, color = tomoa_skip)) +
  facet_wrap(.~sex) + 
  ggtitle("Best Time Per Event") + 
  labs(x = "Start Date", y = "Best Time")



# Qualifying times for men and women
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  filter(!is.na(qual_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip)) + 
  facet_wrap(.~sex) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Men and Women") +
  labs(x = "Start Date", y = "Qualifier Time") 
  



# Final times for men and women
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  filter(!is.na(final_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip)) + 
  facet_wrap(.~sex) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Men and Women") + 
  labs(x = "Start Date", y = "Final Time")



# Mens qualifier times 
qual_plot <-m_times_update |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  filter(!is.na(qual_time))  |>
  ggplot() +
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Men") + 
  labs(x = "Start Date", y = "Qualifier Time") + 
  theme(legend.position = "none")

# Mens final times
final_plot <- m_times_update |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  # There are 3 outliers of ~100 seconds
  filter(!is.na(final_time))  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Men") + 
  labs(x = "Start Date", y = "Qualifier Time") 

#Combine two plots
plot_grid(qual_plot, final_plot, ncol = 2)


# Womens qualifier times 
qual_plot <-w_times_update |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  filter(!is.na(qual_time))  |>
  ggplot() +
  # Hiding legends to prevent compressing
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Women") + 
  theme(legend.position = "none") + 
  labs(x = "Start Date", y = "Qualifier Time")

# Womens final times
final_plot <- w_times_update |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  filter(!is.na(final_time))  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Women") + 
  labs(x = "Start Date", y = "Final Time")

plot_grid(qual_plot, final_plot, ncol = 2)


# Number of tomoa skip users per start date
m_times_update |>
  bind_rows(w_times_update) |>
  group_by(sex, start_date) |>
  filter(!is.na(tomoa_skip)) |>
  summarise(num_ts = sum(tomoa_skip), num_no_ts = sum(!tomoa_skip)) |>
  ggplot() +
  geom_point(aes(start_date, num_ts, color = "num_ts")) + 
  geom_point(aes(start_date, num_no_ts, color = "num_no_ts")) + 
  facet_wrap(.~sex) +
  ggtitle("Uses of tomoa skip per date") + 
  labs(x="Start Date", y = "Count")


# Number of falls -- Not very good
# m_times_update |>
#   bind_rows(w_times_update) |>
#   group_by(sex, start_date) |>
#   pivot_longer(cols = c("fall_qual", "fall_lane_a", "fall_lane_b", "fall_final"), names_to = "round", values_to = "fall") |>
#   filter(!is.na(fall)) |>
#   summarise(
#     fall_ts = sum(tomoa_skip & fall, na.rm = TRUE),
#     fall_no_ts = sum(!tomoa_skip & fall, na.rm = TRUE)
#   ) |>
#   ggplot() +
#   geom_col(aes(start_date, fall_ts, color = "fall_ts")) +
#   geom_col(aes(start_date, fall_no_ts, color = "fall_no_ts")) +
#   facet_wrap(.~sex) +
#   ggtitle("Number of falls with and without tomoa skip") +
#   labs(x = "Start Date", y = "Count")
  
  






