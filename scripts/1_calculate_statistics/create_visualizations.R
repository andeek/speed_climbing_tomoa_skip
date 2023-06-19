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
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), SHOW.LEGEND = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Men") + 
  labs(x = "Start Date", y = "Final Time") 

#Combine two plots
plot_grid(qual_plot, final_plot, ncol = 2) 



# Number of tomoa skip users per start date

#Label Names:
label_names <- c('F' = 'Womens', 'M' = 'Mens')

m_times_update |>
  bind_rows(w_times_update) |>
  group_by(sex, start_date) |>
  filter(!is.na(tomoa_skip)) |>
  summarise(num_ts = sum(tomoa_skip), num_no_ts = sum(!tomoa_skip)) |>
  ggplot() +
  geom_point(aes(start_date, num_ts, color = "Tomoa Skip")) + 
  geom_point(aes(start_date, num_no_ts, color = "No Tomoa Skip")) + 
  facet_wrap(.~sex, labeller = as_labeller(label_names)) +
  labs(x="Start Date", y = "Count") + 
  theme(legend.title = element_blank())



#Best Final and Best Qualifier
m_times_update |>
  # These pivot_longers are producing duplicates
  filter(rank <= 16) |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "qual_round", values_to = "qual_time") |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "final_round", values_to = "final_time") |>
  group_by(fname, lname, sex, event_id, tomoa_skip) |>
  summarise(
    best_qual = ifelse(all(is.na(qual_time)), NA, min(qual_time, na.rm = TRUE)),    
    best_final = ifelse(all(is.na(final_time)), NA, min(final_time, na.rm = TRUE))
  ) |>
  filter(!is.na(best_qual) & !is.na(best_final)) |>
  ggplot() +
  geom_jitter(aes(best_qual, best_final, color = tomoa_skip)) + 
  labs(x = "Best Qualifier Time", y = "Best Final Time") + 
  coord_cartesian(ylim = c(0, 30), xlim = c(0, 30)) + 
  scale_colour_discrete(name  ="",
                        breaks=c(TRUE, FALSE),
                        labels=c("Tomoa Skip", "No Tomoa Skip"))


  
  
  






