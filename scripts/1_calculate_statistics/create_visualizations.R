library(ggplot2)
library(dplyr)


#FIXME --- I want to plot this by date but can't figure out how to add date and tomoa_skip vals
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

# Plot of best times by year and colored by sex (Not Great -- want to use start_date instead of year)
ggplot() +
  geom_point(data = best_times_m, aes(x = year, y = best_time, color = "Men"), na.rm = TRUE) +
  geom_point(data = best_times_w, aes(x = year, y = best_time, color = "Women"), na.rm = TRUE) +
  labs(title = "Best Times Over Years", x = "Year", y = "Best Time") +
  scale_color_manual(values = c("Men" = "blue", "Women" = "red"))

#---Plot of all final times over date, colored by skip for mens
ggplot(m_times_update, aes(x = start_date, y = final, color = tomoa_skip)) +
  geom_point() +
  labs(title = "Final Times Over Dates", x = "Date", y = "Final Time") + 
  coord_cartesian(ylim = c(4, 20)) 

#---Plot of all final times over date, colored by skip for womens
ggplot(w_times_update, aes(x = start_date, y = final, color = tomoa_skip)) +
  geom_point() +
  labs(title = "Final Times Over Dates", x = "Date", y = "Final Time") + 
  coord_cartesian(ylim = c(4, 20)) 


