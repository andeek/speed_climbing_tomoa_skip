# Required library
library(dplyr)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

#FIXME
# -- notes -- 
# avg_final time in 2017 is 28, caused by 999.00 values.

# Group the data frame by year and calculate the percentage of TRUE tomoa_skip values
# Calculate average final times where tomoa skip is true and where tomoa skip is false.
m_tomoa_skip_yearly <- m_times_update |>
  group_by(year) |>
  summarise(total_count = n(),
            false_count = sum(!tomoa_skip, na.rm = TRUE),
            true_count = sum(tomoa_skip, na.rm = TRUE),
            # Yearly avgs of FINAL times for true and false tomoa skip
            final_avg_false = mean(final[!tomoa_skip], na.rm = TRUE),
            final_avg_true = mean(final[tomoa_skip], na.rm = TRUE),
            # Overall yearly final average, regardless of tomoa skip 
            final_avg_overall = mean(final, na.rm = TRUE),
            # Total falls in final
            final_total_falls = sum(fall_final, na.rm = TRUE),
            # Number of falls in final for climbers using and not using the skip
            final_falls_false = sum(fall_final & !tomoa_skip, na.rm = TRUE),
            final_falls_true = sum(fall_final & tomoa_skip, na.rm = TRUE),
            # Total false starts in final
            final_total_FS = sum(false_start_final, na.rm = TRUE),
            # Number of false starts in final for climbers using and not using skip
            final_FS_false = sum(false_start_final & !tomoa_skip, na.rm = TRUE),
            final_FS_true = sum(false_start_final & tomoa_skip, na.rm = TRUE),
            # Yearly avgs of best_qual times for true and false tomoa skip
            qual_avg_false = mean(best_qual[!tomoa_skip], na.rm = TRUE),
            qual_avg_true = mean(best_qual[tomoa_skip], na.rm = TRUE),
            # Overall yearly best_qual average, regardless of tomoa skip
            qual_avg_overall = mean(best_qual, na.rm = TRUE),
            # Total falls in qualifiers
            #FIXME -- should use fall_lane A + B ?
            qual_total_falls = sum(fall_qual, na.rm = TRUE),
            # Number of qualifying falls for climbers using and not using the skip
            qual_falls_false = sum(fall_qual & !tomoa_skip, na.rm = TRUE),
            qual_falls_true = sum(fall_qual & tomoa_skip, na.rm = TRUE),
            #. Total number of false starts in qualifiers
            qual_total_FS = sum(false_start_qual, na.rm = TRUE),
            # Number of false starts for climbers using and not using tomoa skip
            qual_FS_false = sum(false_start_qual & !tomoa_skip, na.rm = TRUE),
            qual_FS_true = sum(false_start_qual & tomoa_skip, na.rm = TRUE)
            )



w_tomoa_skip_yearly <- w_times_update |>
  group_by(year) |>
  summarise(total_count = n(),
            false_count = sum(!tomoa_skip, na.rm = TRUE),
            true_count = sum(tomoa_skip, na.rm = TRUE),
            final_avg_false = mean(final[!tomoa_skip], na.rm = TRUE),
            final_avg_true = mean(final[tomoa_skip], na.rm = TRUE),
            final_avg_overall = mean(final, na.rm = TRUE),
            final_total_falls = sum(fall_final, na.rm = TRUE),
            final_falls_false = sum(fall_final & !tomoa_skip, na.rm = TRUE),
            final_falls_true = sum(fall_final & tomoa_skip, na.rm = TRUE),
            final_total_FS = sum(false_start_final, na.rm = TRUE),
            final_FS_false = sum(false_start_final & !tomoa_skip, na.rm = TRUE),
            final_FS_true = sum(false_start_final & tomoa_skip, na.rm = TRUE),
            qual_avg_false = mean(best_qual[!tomoa_skip], na.rm = TRUE),
            qual_avg_true = mean(best_qual[tomoa_skip], na.rm = TRUE),
            qual_avg_overall = mean(best_qual, na.rm = TRUE),
            qual_total_falls = sum(fall_qual, na.rm = TRUE),
            qual_falls_false = sum(fall_qual & !tomoa_skip, na.rm = TRUE),
            qual_falls_true = sum(fall_qual & tomoa_skip, na.rm = TRUE),
            qual_total_FS = sum(false_start_qual, na.rm = TRUE),
            qual_FS_false = sum(false_start_qual & !tomoa_skip, na.rm = TRUE),
            qual_FS_true = sum(false_start_qual & tomoa_skip, na.rm = TRUE)
  )








