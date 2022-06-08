### Meta ###

# Author: Andreas H?hn
# Version: 1.0
# Date: 2022/05/12
# About: this file contains all user-written functions 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [0] Install Required Packages if not already installed + Load ###

# input: a character vector containing the names of all required packages
# output: downloads all required Packages if not already installed and loads them

.EnsurePackages <- function(packages_vector) {
  new_package <- packages_vector[!(packages_vector %in% 
                                     installed.packages()[, "Package"])]
  if (length(new_package) != 0) {
    install.packages(new_package, dependencies = TRUE) }
      sapply(packages_vector, suppressPackageStartupMessages(require),
      character.only = TRUE)

}
.EnsurePackages(RequiredPackages)
here <- here::here

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Create Cohort Function ###

# input: definitions with parameters for population
# output: a baseline-level cohort table with 1 row = 1 individual

.CreateCohort <- function(definitions) {
  
  # Baseline - Level Frame 
  data <- data.table::data.table(
    id    = seq(1:definitions$register_size),
    men   = c(rep(0, times = definitions$register_size/2),
            rep(1, times = definitions$register_size/2)),
    bday  = sample(x = seq(definitions$cohorts_range[1],
                         definitions$cohorts_range[2], by = "day"),
                 size = definitions$register_size, replace = TRUE), 
   life_length = round(c(rnorm(definitions$register_size/2,
                              definitions$LE[1],definitions$LE_sd[1]),
                        rnorm(definitions$register_size/2,
                              definitions$LE[2],definitions$LE_sd[2])), 
                      definitions$rounding))
  
  # date of death is based on simulated birthdays and lengths of life's 
  data[, dday        := bday + (life_length * definitions$year_length)]
  
  # we need the birthday of the center age for inclusion / exclusion into study 
  data[, bday_center := bday + 
                    (definitions$age_center * definitions$year_length)]
  
  # explicit return
  return(data)
  }

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Long-Format Episode Split ###

# input: baseline-level table with study entry and study exit information for
#        each individual + definitions with parameters for episode length
# output: a long-format study cohort based on episode length

# function for general episode splitting
.EpisodeSplitCohort  <- function(data_input,  definitions) {
  # specify columns to be carried over, default = all 
  all_columns   <- colnames(data_input)
  # create episodes based on episode length and framed by start / end dates
  data_output <- data_input[, .(episode = 1:ceiling(
    as.numeric((study_exit + 1 - study_entry) /
                 definitions$episode_length))), by = all_columns]
  # helper variable: maximum number of episodes
  data_output[, episode_max := max(episode), by = id]
  # define episode start dates
  data_output[, episode_start_date := (study_entry + ((episode - 1) * 
                                         definitions$episode_length))]
  # define episode end dates 
  data_output[, episode_end_date := episode_start_date + 
                (definitions$episode_length - 1)]
  data_output[(episode_max == episode) & (episode_end_date > study_exit),
              episode_end_date := study_exit]
  data_output$episode_max <- NULL
  setkey(data_output, id, episode_start_date)
  # extend the last episode until study exit
  data_output[, drop := 0]
  data_output[episode_start_date >= episode_end_date, drop := 1]
  data_output <- data_output[drop == 0]
  data_output[, episode_max := max(episode), by = id]
  data_output[(episode == episode_max) & (episode_end_date < study_exit),
              episode_end_date := study_exit, by = id]
  # episode length
  data_output[, episode_length := round(as.numeric(episode_end_date - 
                        episode_start_date) /  definitions$year_length, 2)]
 
  # chronological age #
  data_output[, age := round(as.numeric(episode_end_date - bday) / 
                        definitions$year_length, definitions$rounding)]
  
  # chronological age centered # 
  data_output[, age_center := age - definitions$age_center]
  
   # data minimization
  data_output$drop           <- NULL
  
  # logical check: typically triggered by study_entry > study exit
  stopifnot(dim(data_output[episode < 0,])[[1]] == 0) # no negative episodes 
  
  # explicit return
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] Data Preparation for Chiang 1984 LTB  ###

# input: a long-format cohort table
# output: a data set with Nx, Dx

# Prepare Data Function
.DataPrepChiang <- function(data_input, definitions) {
  
  data_mort <- copy(data_input)
  
  # identify deaths #
  data_mort[, death := 0]
  data_mort[dday <= episode_end_date & dday >= episode_start_date,
            death := 1, by="id"]
  
  # age #
  data_mort[, mid_slices := episode_end_date - 
              ((episode_end_date - episode_start_date) / 2)]
  
  data_mort[, age := as.numeric(round(((mid_slices-bday )/ 
                                         definitions$year_length),4))]
  data_mort[, age := trunc(age, prec = 0)]
  data_mort[, age_band := cut(age, c(-1, seq(9,99,10), Inf),
                              include.lowest=FALSE, left=FALSE)]
  levels(data_mort$age_band) <- c("0-9","10-19","20-29","30-39","40-49",
                          "50-59","60-69","70-79","80-89","90-99","100p")
  
  # Death Counts #
  deaths <- data_mort[, .(Dx = sum(death)), by="age_band"]
  setkey(deaths, age_band)
  
  # Exposure Pop #
  exposure <- data_mort[, .(Nx = sum(episode_length)), by="age_band"]
  setkey(exposure, age_band)
  
  result <- merge(deaths, exposure, by="age_band")
  
  # return
  return(result)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [4] Estimate Chiang 1984 LTB  ###

# input: a data set prepared for life table estimates
# output: a data set with ltbs

.LTBChiang <- function(input_data) {
  
  # Deaths and Exposure
  Dx <- input_data$Dx
  Nx <- input_data$Nx
  
  age_band   <- input_data$age_band
  age_band   <- droplevels(age_band)
  age_band   <- levels(as.factor(age_band))
  
  n                <- c(rep(10, length(age_band)-1),0)
  ax               <- c(rep(5,  length(age_band)-1),0)
  mx               <- Dx/Nx
  qx               <- (n*mx) / (1+(n-ax)*mx)
  qx[length(qx)]   <- 1.0
  px               <- 1.0-qx
  lx               <- rep(x=0, times=length(mx))
  lx[1]            <- 100000
  for (i in 2:(length(lx))){
    lx[i]          <- lx[i-1] * px[i-1]
  }
  dx              <- lx*qx
  dx[length(dx)]  <- lx[[length(dx)]]
  Lx              <- n*c(lx[-1],0) + ax * dx
  Lx[length(Lx)]  <- lx[length(Lx)]/mx[length(Lx)]
  Tx              <- rev(cumsum(rev(Lx)))
  ex              <- Tx/lx
  ltb_return <- data.table::data.table(age_band,
                n, Nx, Dx, mx, qx, ax, lx, dx, Lx, Tx, ex)
  # explicit return
  return(ltb_return)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [5] Estimate LE based on a two-state parametric model ###

# input: the long-format cohort table, and definitions
# output: estimates  of LE based 

.ParSurvLE <- function(data_input, definitions) {
  
  # copy dataset 
  data_input <- copy(cohort_long)
  
  # alive - death states 
  data_input[, state_bin := 1]
  data_input[dday == episode_end_date, state_bin := 2]
  
  # REDUCE DATA SET #
  data_input <- data_input[,c("id","men","episode","age_center","state_bin")]
  
  # Specify the transitions that are allowed in 'Q'
  Q <- rbind(c(0, 1),
             c(0, 0))
  
  # Transfer into tmat format
  tmat_twostate <- mstate::transMat(x = list(c(2), 
                                             c()),
                                    names = c("Alive", "Death"))
  # Number of possible transitions #
  no_of_trans <- sum(!is.na(tmat_twostate))
  
  # msm2urv reshapes the data 
  data <- msm::msm2Surv(data=data_input, Q = Q, 
                        subject="id", time="age_center", state="state_bin")
  
  # transition and men need to be factor
  data$trans  <- as.factor(data$trans)
  data$men    <- as.factor(data$men)
  
  # estimate model # 
  model <- vector(no_of_trans, mode="list")
  model[[1]] <-flexsurv::flexsurvreg(Surv(Tstart, Tstop, status) ~ men,
                      subset = (trans == 1), data = data,
                      dist = definitions$distribution)
  # gompertz usually fits best when the studied age range is 30-90
  # weibull usually fits best with respect to the entire age range
  # gompertz is proportional hazards exp(est) == Hazard Ration
  # weibull is accelerated failure time model exp(est) == Acceleration Factor
  
  # new data 
  new_data <- data.table(
    men = as.factor(c(0,1)),
    trans = as.factor(rep(1,times=2)))
  
  # Predictions #
  females_parsurv <- flexsurv::totlos.fs(model,
       trans = tmat_twostate, t = definitions$age_max - definitions$age_center,
       newdata = new_data[1], ci = FALSE, sing.inf = 1e+100, B=100)	
  males_parsurv <- flexsurv::totlos.fs(model,
       trans = tmat_twostate, t = definitions$age_max - definitions$age_center,
       newdata = new_data[2], ci = FALSE, sing.inf = 1e+100, B=100)	
  
  return(list(females_parsurv, males_parsurv, model))
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

