# PREAMBLE ----------------------------------------------------------------
# TITLE: run_model.R
# DESCRIPTION: Run economic model of psychosis and store results
# AUTHOR: Ed Penington
# DATE: 2024/02/19
# VERSION: 0.1

# Based on tutorial: https://github.com/Roche/descem/blob/main/vignettes/example_ssd.Rmd 

# https://github.com/Roche/descem
# to install: devtools::install_github("roche/descem", ref="main")
library(descem)

library(flexsurv) # survival distributions
library(dplyr) # pipe operator



# PARAMETERS --------------------------------------------------------------

# parameters that do not change on any patient or intervention loop
common_all_inputs <- add_item(util.stable = 0.8, #TEMP
                              util.acute = 0.4, #TEMP
                              cost.stable = 1000, #TEMP
                              cost.acute = 500, #TEMP
                              cost.int = 1000, #TEMP # cost of interventions
                              coef.noint_time_to_acute = 0.66, #TEMP
                              coef.noint_time_to_stable = 0.33, #TEMP
                              HR_int = 0.8 #TEMP # hazard ratio of intervention
                              ) 

# objects that do not change between intervention loops
common_pt_inputs <- add_item(age=max(16, rnorm(n=1, mean=23.52, sd=2.85)), #TEMP
                             death = 60 #TEMP
                             )

# objects that change through treatment loops
unique_pt_inputs <- add_item(fl.acute = 0)


# EVENT STRUCTURE ---------------------------------------------------------

# define initial events and intervention arms
init_event_list <- 
  # no intervention
  add_tte(trt="CM_MED", evts = c("stable", "acute", "death"), input = {
    stable <- 0
    acute <- draw_tte(1, dist="exp", coef1=coef_noint)
  }) %>%
  # intervention
  add_tte(trt="int", evt = c("stable", "acute", "death"), input = {
    stable <- 0
    acute <- draw_tte(1, dist = "exp", coef1=coef_noint, hr = HR_int)
  })

# define event reactions
evt_react_list <- 
  add_reactevt(name_evt="stable",
               input = {}) %>%
  add_reactevt(name_evt = "acute",
               input = {
                 modify_item(list(fl.acute = 1))
               }) %>%
  add_reactevt(name_evt = "death",
               input = {
                 modify_item(list(curtime = Inf)) # set time to inf to end simulation
               })

# PAYOFFS -----------------------------------------------------------------

util_ongoing <- add_util(evt = c("stable", "acute", "death"),
                         trt = c("int", "noint"),
                         util = util.stable * fl.stable + util.acute * (1-fl.stable)
)

cost_ongoing <- 
  add_cost(
    evt = c("stable", "acute", "death"),
    trt = "noint",
    cost = cost.stable * fl.stable + cost.acute * (1 - fl.stable) ) %>%
  add_cost(
    evt = c("stable", "acute", "death") ,
    trt = "int",
    cost = cost.stable * fl.stable + cost.acute * (1 - fl.stable) + cost.int * fl.stable)


# EXECUTE MODEL -----------------------------------------------------------

results <- RunSim(
  npats = 1000,
  n_sim = 1,
  psa_bool = FALSE,
  trt_list = c("int", "noint"),
  common_all_inputs = common_all_inputs,
  common_pt_inputs = common_pt_inputs,
  unique_pt_inputs = unique_pt_inputs,
  init_event_list = init_event_list,
  evt_react_list = evt_react_list,
  util_ongoing_list = util_ongoing,
  cost_ongoing_list = cost_ongoing,
  ncores = 2,
  drc = 0.035, # discount rate for costs
  drq = 0.035 # discount rate for qalys
)
