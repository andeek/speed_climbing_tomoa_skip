# Required library
library(dplyr)

# Group the data frame by year and calculate the percentage of TRUE tomoa_skip values
# Calculate average final times where tomoa skip is true and where tomoa skip is false.
m_tomoa_skip_yearly <- m_times_update |>
  group_by(year) |>
  summarise(total_count = n(),
            false_count = sum(tomoa_skip == FALSE),
            true_count = sum(tomoa_skip == TRUE),
            percent_true = (true_count / total_count) * 100,
            #These are yearly averages of FINAL times with and without skip
            avg_skip_true = mean(final[tomoa_skip == TRUE], na.rm = TRUE),
            avg_skip_false = mean(final[tomoa_skip == FALSE], na.rm = TRUE),
            #this is OVERALL yearly final average, regardless of tomoa skip 
            avg_final = mean(final, na.rm = TRUE)
            )

w_tomoa_skip_yearly <- w_times_update |>
  group_by(year) |>
  summarise(total_count = n(),
            false_count = sum(tomoa_skip == FALSE),
            true_count = sum(tomoa_skip == TRUE),
            percent_true = (true_count / total_count) * 100,
            avg_skip_true = mean(final[tomoa_skip == TRUE], na.rm = TRUE),
            avg_skip_false = mean(final[tomoa_skip == FALSE], na.rm = TRUE),
            avg_final = mean(final, na.rm = TRUE),
            total_falls = sum(fall_qual) + sum(fall_final) +
              sum(fall_lane_a) + sum(fall_lane_b) + sum(fall_quarter) +
              sum(fall_semi) + sum(fall_small_final) + sum(fall_big_final) + 
              sum(fall_first_round)
            )






            


#Find the earliest date at which a tomoa skip is performed for mens and womens
filtered_data <- m_times_update |>
  filter(!is.na(start_date) & !is.na(tomoa_skip) & tomoa_skip == TRUE) 
m_earliest_skip <- min(filtered_data$start_date)

filtered_data <- w_times_update |>
  filter(!is.na(start_date) & !is.na(tomoa_skip) & tomoa_skip == TRUE) 
w_earliest_skip <- min(filtered_data$start_date)




cat('\n')
print("Mens Tomoa Skip by Year:")
print(m_tomoa_skip_yearly)
cat('\n')
print("Womens Tomoa Skip by Year:")
print(w_tomoa_skip_yearly)
cat('\n')

print("First occurence of tomoa skip for men:")
print(m_earliest_skip)
cat('\n')
print("First occurence of tomoa skip for women:")
print(w_earliest_skip)
