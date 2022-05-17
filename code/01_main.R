### Meta ###

# Author: Andreas Höhn
# Version: 1.0
# Date: 2022/05/12
# About: the main control file 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Setting ###
# State Space: [1] Alive / [2] Death
# Study Design: Cohort Study Layout
# Summary: In example, we estimate LE at for a centered age using a parametric 
# survival model and plot the corresponding survival curves. This estimate is 
# compared with LE obtained via Chiang 1984 life table approach - the classic.

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Preparation ###

# deep clean work space
rm(list = ls())
gc(full = TRUE)

# seed
set.seed(20221005)

# display format
options(scipen = 1000)

# benchmark time 
benchmark_time <- list() 
benchmark_time$start <- Sys.time()

# libraries: to be installed/loaded automatically; called in user_functions
RequiredPackages <- c("here","data.table","flexsurv")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Definitions ###
# SIMULATED STUDY COHORT #
definitions <- list()   
definitions$register_size  <- 5000     # size of the register 
definitions$LE             <- c(85,80)   # LE for women [1] / men [2]
definitions$LE_sd          <- c(10,10)   # LE std dev for women [1 / men [2]
definitions$cohorts_range  <- c(base::as.Date("1900-01-01"),  # cohorts range
                                base::as.Date("2019-12-31"))  # lower / upper
# LONG FORMAT SETTINGS #
definitions$episode_length <- 30       # duration of episode in days 
definitions$year_length    <- 365.25   # duration of one year
definitions$rounding       <- 4
# COHORT STUDY # 
definitions$study_start    <- base::as.Date("2010-01-01") # start cohort study 
definitions$study_end      <- base::as.Date("2019-12-31") # end cohort study   
definitions$min_obs        <- 1                   # min days under observation
definitions$age_center     <- 30                  # centered for age 0

# PARAMETRIC SURV MODEL # 
definitions$distribution   <- "weibull" # distribution for flexsurv par model
definitions$age_max        <- 100        # maximum life span
# VISULAIZATION # 
definitions$fig_width      <- 800      # fig width
definitions$fig_height     <- 600      # fig height
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Files ###
source("code/02_user_functions.R")
source("code/03_simulate_cohort.R")
source("code/04_LE_analysis.R")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Benchmark Time ###

benchmark_time$end <- Sys.time()
print("Duration of Program:")
print(round(benchmark_time$end - benchmark_time$start), 
      definitions$rounding_results)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

