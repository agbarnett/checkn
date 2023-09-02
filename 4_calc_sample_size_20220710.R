# 4_calc_sample_size.R
# calculate sample size
# September 2023
library(dplyr)
library(stringr)
library(textclean)
library(tidyr)
source('99_sample_size_functions.R') # main function

# get data from 3_extract_parameters_20220710.R
data_sources = c('original','extra')
data_source = data_sources[2]
source('1_which_data.R')
load(file_params)

#***to do: repeated measures, add poisson+negbin, equivalence trials, and more listed throughout...

#examples that work
# > studies$number[49]
# [1] "ACTRN12613000376741"
# > studies$number[457]
# [1] "ACTRN12613001147774"


# big loop
N = nrow(studies)
update = NULL
for (k in 1:N) {
  
  if (k %in% seq(0,N,100)) print(k) # update
  
  # get the study details
  this_study = studies[k,]

  #if NA, assume alpha=0.05, beta=0.2, numsides=2
  alpha = ifelse(is.na(unlist(this_study$alpha)), 0.05, unlist(this_study$alpha))
  beta = 1 - ifelse(is.na(unlist(this_study$power)), 0.2, unlist(this_study$power))
  numsides = ifelse(is.na(this_study$numsides), 2, this_study$numsides)

  #unlist parameters
  n_extracted = unlist(this_study$n_extracted)
  expect_diff = unlist(this_study$expect_diff)
  sd_diff = unlist(this_study$sd_diff)
  crossover = ifelse(is.na(this_study$assignment), FALSE, (this_study$assignment == "Crossover"))
  factorial = ifelse(is.na(this_study$assignment), FALSE, (this_study$assignment == "Factorial")) 
  #cluster_rand = FALSE, m = NA, icc = NA,
  loss_followup_pct = ifelse(is.na(unlist(this_study$loss_followup_pct)), 0, unlist(this_study$loss_followup_pct))
  dropout_pct = ifelse(is.na(unlist(this_study$dropout_pct)), 0, unlist(this_study$dropout_pct))
  dropin_pct = ifelse(is.na(unlist(this_study$dropin_pct)), 0, unlist(this_study$dropin_pct))
  #unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5

  #initialise 
  n = vector(mode = 'numeric', length = 0)
  is_n_correct = NA
  n_extracted_less_calculated = NA
  n_fn_used = NA
  
  if (!any(is.na(expect_diff)) & !any(is.na(sd_diff))) {
  
    n_fn_used = "n_diffmeans"
    
    scenarios = expand.grid(alpha = alpha, beta = beta, 
                            expect_diff = expect_diff, sd_diff = sd_diff, 
                            loss_followup_pct = loss_followup_pct, 
                            dropout_pct = dropout_pct, 
                            dropin_pct = dropin_pct)
    
    for (i in 1:nrow(scenarios)) {
      
      n = c(n, n_diffmeans(expect_diff = scenarios$expect_diff[i], sd_diff = scenarios$sd_diff[i], 
                           mu0 = NA, mu1 = NA,  sd0 = NA, sd1 = NA,  n0 = NA, n1 = NA, 
                           alpha = scenarios$alpha[i], beta = scenarios$beta[i], numsides = numsides, 
                           type = 1, 
                           crossover = crossover, 
                           factorial = factorial, 
                           cluster_rand = FALSE, m = NA, icc = NA,
                           exp_loss_followup_pct = scenarios$loss_followup_pct[i], 
                           exp_dropout_pct = scenarios$dropout_pct[i], exp_dropin_pct = scenarios$dropin_pct[i], 
                           unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5)) 
    }
    

  n_sets = expand_grid(n = n, n_extracted = n_extracted)
  is_n_correct = abs(n_sets$n - n_sets$n_extracted) <= 1
  n_extracted_less_calculated = n_sets$n_extracted - n_sets$n
  
  }
  
  if (length(n) == 0) n = NA
  
  stats_section = this_study$stats_section
  
  ## store data #-n_calculated, -is_n_correct, -n_extracted_less_calculated, -n_fn_used
  frame = select(this_study, -stats_section) %>%
    mutate(stats_section = stats_section,
           n_calculated = list(n),
           is_n_correct = list(is_n_correct),
           n_extracted_less_calculated = list(n_extracted_less_calculated),
           n_fn_used = n_fn_used
           )
  update = bind_rows(update, frame)
  frame =  NULL # tidy up

}

# data edits
studies = mutate(update) 

#clean up
rm(expect_diff, sd_diff, alpha, beta, numsides, crossover, factorial, loss_followup_pct, dropin_pct, dropout_pct, n_fn_used,
   is_n_correct, n_extracted_less_calculated, scenarios, n_sets)

# save
save(studies, excluded, file = 'data/4_Stats_Sections_Processed_wParams_wN.RData')
