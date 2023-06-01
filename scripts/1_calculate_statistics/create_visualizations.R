library(ggplot2)
library(dplyr)

# Combine the first name and last name into a single 'name' column
m_times_full_name <- m_times_update |>
  mutate(name = paste(fname, lname))

# Group the data by year and name, and select the lowest time for each group
best_times <- m_times_full_name |>
  group_by(year, name, tomoa_skip, start_date) |>
  summarize(best_time = min(final)) 

#FIXME --- not getting yearly best times
# Create the scatterplot with the best times for each climber in each year
# This plot shows a each climbers best time for each given year (plotted by start date of comp)
plot1 <- ggplot(best_times, aes(x = start_date, y = best_time, color = tomoa_skip)) +
  geom_point() +
  labs(x = "Date", y = "Best Final Time") +
  ggtitle("Best Time for Each Climber")

print(plot1)
