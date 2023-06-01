library(ggplot2)
library(dplyr)


#FIXME --- I want to plot this but can't figure out how to add date and tomoa_skip vals
# without it breaking best_times 

#Find each climbers best final time for each given year
best_times_m <- m_times_update |>
  group_by(year, fname, lname) |>
  summarise(best_time = ifelse(all(is.na(final)), NA, min(final, na.rm = TRUE))) |>
  ungroup() |>
  na.omit()

# Replace 'inf' values with NA
best_times_m$best_time[best_times_m$best_time == Inf] <- NA

best_times_w <- w_times_update |>
  group_by(year, fname, lname) |>
  summarise(best_time = ifelse(all(is.na(final)), NA, min(final, na.rm = TRUE))) |>
  ungroup() |>
  na.omit()

best_times_w$best_time[best_times_w$best_time == Inf] <- NA

#---Plot of all final times over date, colored by skip
ggplot(m_times_update, aes(x = start_date, y = final)) +
  geom_point() +
  labs(title = "Final Times Over Dates", x = "Date", y = "Final Time") + 
  coord_cartesian(ylim = c(0, 16)) 



