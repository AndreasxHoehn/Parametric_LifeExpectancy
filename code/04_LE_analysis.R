### Meta ###

# Author: Andreas H?hn
# Version: 1.0
# Date: 2022/05/10
# About: Estimating LE from individual-level data via flexsurv
# and comparing the results with the classic aggregated Chiang 1984 
# approach

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] LOGICAL CHECKS BEFORE START ###

# No Zero Lengths Intervals #
stopifnot(
  dim(cohort_long[episode_start_date == episode_end_date,])[[1]] == 0)

# No Entry after Exit and no deaths after Study End 
stopifnot(
  dim(cohort_long[study_entry  > study_exit])[[1]] == 0 &
  dim(cohort_long[dday         > study_exit])[[1]] == 0)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] CHIANG METHOD 1984 ###

result_chiang_females  <- .DataPrepChiang(data_input = cohort_long[men == 0,],
                                          definitions = definitions)
result_chiang_males    <- .DataPrepChiang(data_input = cohort_long[men == 1,],
                                          definitions = definitions)

result_chiang_females <- .LTBChiang(input_data = result_chiang_females)
result_chiang_males   <- .LTBChiang(input_data = result_chiang_males)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] TWO STATE FLEXSURV MODEL ###

# Results of Parametric Survival Model
result_parsurv <- .ParSurvLE(data_input = cohort_long,
                             definitions = definitions)
result_parsurv_females <- result_parsurv[[1]]
result_parsurv_males   <- result_parsurv[[2]]

result_parsurv_model   <- result_parsurv[[3]]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [4] Comparing Results ###

# females:
print(paste("females parametric:", round(result_parsurv_females[1,1],2)))
print(paste("females chiang:",     round(result_chiang_females$ex[1],2)))

# males:
print(paste("males parametric:", round(result_parsurv_males[1,1],2)))
print(paste("males chiang:",     round(result_chiang_males$ex[1],2)))

# visualize results:
png(filename = "plots/comparison_results.png",
    width = definitions$fig_width, height = definitions$fig_height)
plot(result_parsurv_model[[1]], main = "Comparing Fit of Model Estimates",
     xlab="age centered", 
     xlim = c(0, (definitions$age_max - definitions$age_center)))
lines(y = (result_chiang_males$lx/100000),
      x = seq(definitions$age_center, definitions$age_max, 10) - definitions$age_center,
      type = "l", col = "blue", lwd = 2)
lines(y = (result_chiang_females$lx/100000),
      x = seq(definitions$age_center, definitions$age_max, 10) - definitions$age_center,
      type = "l", col = "blue", lwd = 2)
# Add a legend
legend(1, 0.25, legend=c("Observed", "Parametric Model",
                         "Chiang 1984 with 10y age groups"),
       col = c("black", "red", "blue"), lty = c(1,1,1), cex = 2, lwd = 2, bty = "n")
dev.off()


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #





