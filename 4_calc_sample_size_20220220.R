# 4_calc_sample_size.R
# calculate sample size
# February 2022
library(dplyr)
library(stringr)
library(textclean)
library(tidyverse)

# from 3_extract_parameters.R
load('data/Stats_Sections_Processed_wParams.RData')


#***to do: repeated measures, add poisson+negbin, equivalence trials, and more listed throughout...

#adjustments
adjustments <- function(n, exp_loss_followup_pct = 0, exp_dropout_pct = 0, exp_dropin_pct = 0, unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5, correction_pct = 0) {
  
  #misc correction
  n = n*(1 + correction_pct/100)
  
  #compliance
  n = n/(1-exp_dropout_pct/100-exp_dropin_pct/100)^2
  
  #unequal sized groups
  #q1 and q2 are the propn of participants in each grp, so that nq1 and nq2 are the number of participants allocated to each grp
  if (unequal_groups == TRUE) n = n*(1/4)*(1/unequal_q1+1/unequal_q2)
  
  #loss to follow up
  #*do this last after all other adjustments*
  n = n/(1-exp_loss_followup_pct/100)
  
  return(n)
  
}

#sample size functions

#continuous/normal
#difference in means, gives n per group unless crossover study
n_diffmeans <- function(expect_diff = NA, sd_diff = NA, mu0 = NA, mu1 = NA,  sd0 = NA, sd1 = NA,  n0 = NA, n1 = NA, 
                        alpha = 0.05, beta = 0.2, numsides = 2, 
                        type = 1, crossover = FALSE, factorial = FALSE, cluster_rand = FALSE, m = NA, icc = NA,
                        exp_loss_followup_pct = 0, exp_dropout_pct = 0, exp_dropin_pct = 0, unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5, correction_pct = 0) {
  
  if (is.na(expect_diff)) expect_diff = mu1 - mu0
  if (is.na(sd_diff)) sd_diff = sqrt(sd1^2/n1 + sd0^2/n0)
  
  #catch input error
  if (is.na(expect_diff)&(is.na(mu0)|is.na(mu1))) stop("Need either expect_diff or both mu0 and mu1.")
  if (is.na(sd_diff)&(is.na(sd0)|is.na(sd1)|is.na(n0)|is.na(n1))) stop("Need either sd_diff or all of sd0, sd1, n0, and n1.")
  if (is.na(expect_diff)|is.na(sd_diff)) stop("Both expect_diff and sd_diff are required.")
  if ((cluster_rand == TRUE)&(is.na(m)|is.na(icc))) stop("For a cluster-randomised trial, m (cluster size) and icc (intracluster correlation coefficient) are required.")
  
  #sample size
  Zalpha = qnorm(1-alpha/numsides)
  Zbeta = qnorm(1-beta)
  n = 2*((sd_diff^2)/(expect_diff^2))*(Zalpha+Zbeta)^2 
  
  #alternative formula as implemented in nQuery, from Dixon & Massey 1983
  if (type == 2) n = n + Zalpha^2/4
  
  #crossover study
  if (crossover == TRUE) n = ((sd_diff^2)/(expect_diff^2))*(Zalpha+Zbeta)^2 #total sample size, outcome measured twice on each subject
  
  #factorial study
  if (factorial == TRUE) n = 2*n #assuming 2x2 factorial study, will need to generalise
  
  #cluster randomised trial, Friedman et al 1998 (ch7, Fundamentals of Clinical Trials 3rd ed.)
  if (cluster_rand == TRUE) n = n*(1+(m-1)*icc)
  
  #adjustments
  n = adjustments(n = n, exp_loss_followup_pct = exp_loss_followup_pct, exp_dropout_pct = exp_dropout_pct, exp_dropin_pct = exp_dropin_pct, unequal_groups = unequal_groups, unequal_q1 = unequal_q1, unequal_q2 = unequal_q2, correction_pct = correction_pct)
  
  return(ceiling(n))
  
}

#binary/binomial
#difference in proportions, gives n per group unless crossover study 
n_diffprops <- function(p_ctrl = NA, p_int = NA, arr = NA, rrr = NA, f = NA, delta = NA,
                        alpha = 0.05, beta = 0.2, numsides = 2,
                        type = 1, crossover = FALSE, factorial = FALSE, cluster_rand = FALSE, m = NA, kappa = NA, pstar = NA,
                        exp_loss_followup_pct = 0, exp_dropout_pct = 0, exp_dropin_pct = 0, unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5, correction_pct = 0) {
  #p_int, p_ctrl: proportions/event rates in the intervention and control groups
  #arr, rrr: absolute/relative risk reduction
  
  #***to do: continuity correction***
  
  #catch input error
  if (is.na(p_ctrl)) stop("Proportion in control group (p_ctrl) is required.")
  if (is.na(p_int)&is.na(arr)&is.na(rrr)) stop("Need one of p_int, arr, or rrr.")
  if ((crossover == TRUE)&(is.na(delta))) delta = p_int-p_ctrl
  if ((crossover == TRUE)&(is.na(f)|is.na(delta))) stop("For crossover studies, f and delta (or both p_int and p_ctrl) are required.")
  if (crossover == TRUE) type = 5
  if ((cluster_rand == TRUE)&(is.na(m)|(is.na(kappa)&is.na(pstar)))) stop("For a cluster-randomised trial, m (cluster size) and either the kappa coefficient or pstar (concordancy) are required.")
  
  #use p_int if available, otherwise use arr or rrr to get p_int
  if (is.na(p_int)&!is.na(arr)) p_int = p_ctrl-arr #if absolute risk reduction (arr) given
  if (is.na(p_int)&!is.na(rrr)) p_int = p_ctrl-rrr*p_ctrl #if relative risk reduction (rrr) given
  
  #sample size
  pbar = (p_ctrl+p_int)/2
  qbar = 1 - pbar
  Zalpha = qnorm(1-alpha/numsides)
  Zbeta = qnorm(1-beta)
  n = ( ( Zalpha*sqrt(2*pbar*qbar) + Zbeta*sqrt(p_ctrl*(1-p_ctrl)+p_int*(1-p_int)) )^2 ) / ( (p_ctrl-p_int)^2 )
  
  #alternative given in Friedman et al 1998 (ch7, Fundamentals of Clinical Trials 3rd ed.)
  if (type == 2) n = ( 2*pbar*qbar*((Zalpha+Zbeta)^2) ) / ( (p_ctrl-p_int)^2 )
  
  #another version, Cundill & Alexander 2015 (doi: 10.1186/s12874-015-0023-0)
  if (type == 3) n = ( (p_ctrl*(1-p_ctrl)+p_int*(1-p_int))*((Zalpha+Zbeta)^2) ) / ( (p_ctrl-p_int)^2 )
  
  #if ORs are used instead of RRs, Cundill & Alexander 2015 (doi: 10.1186/s12874-015-0023-0)
  if (type == 4) n = ( ( 1/(p_ctrl*(1-p_ctrl)) + 1/(p_int*(1-p_int)) )*((Zalpha+Zbeta)^2) ) / ( (qlogis(p_ctrl)-qlogis(p_int) )^2 )
  
  #crossover study, using McNemar's test (paired data)
  if (type == 5) n = (f/(delta^2))*(Zalpha+Zbeta)^2
  
  #factorial study
  if (factorial == TRUE) n = 2*n #assuming 2x2 factorial study, will need to generalise
  
  #cluster randomised trial, Friedman et al 1998 (ch7, Fundamentals of Clinical Trials 3rd ed.)
  #needs m cluster size, and either the kappa coefficient or pstar (concordancy) 
  if ((cluster_rand == TRUE)&is.na(kappa)) kappa = ( pstar - (p_ctrl^m+(1-p_ctrl)^m) ) / ( 1 - (p_ctrl^m+(1-p_ctrl)^m) )
  if (cluster_rand == TRUE) n = n*(1+(m-1)*kappa)
  
  #adjustments
  n = adjustments(n = n, exp_loss_followup_pct = exp_loss_followup_pct, exp_dropout_pct = exp_dropout_pct, exp_dropin_pct = exp_dropin_pct, unequal_groups = unequal_groups, unequal_q1 = unequal_q1, unequal_q2 = unequal_q2, correction_pct = correction_pct)
  
  return(ceiling(n))
  
}

#survival, time to event 
#gives n per group
n_survival <- function(lambda_int = NA, lambda_ctrl = NA, 
                       alpha = 0.05, beta = 0.2, numsides = 2,
                       t = 0, t0 = 0,
                       type = 1, factorial = FALSE, 
                       exp_loss_followup_pct = 0, exp_dropout_pct = 0, exp_dropin_pct = 0, unequal_groups = FALSE, unequal_q1 = 0.5, unequal_q2 = 0.5, correction_pct = 0) {
  #lambda_int, lambda_ctrl: hazard rates in the intervention and control groups
  #t is the total follow up time, t0 is the recruitment time (t0<=t) 
  #type 1: survival time exponentially distributed, no censoring
  #type 2: survival time exponentially distributed, censoring, instantaneous recruitment and follow up over time t
  #type 3: survival time exponentially distributed, censoring, continuous recruitment over time t
  #type 4: survival time exponentially distributed, censoring, recruitment over time t0, with total follow-up time t
  
  #catch input error
  if (is.na(lambda_int)|is.na(lambda_ctrl)) stop("Hazard rates in intervention and control groups required.")
  if ((type == 2)&(t == 0)) stop("Non-zero total follow-up time t required.")
  if ((type == 3)&(t == 0)) stop("Non-zero total follow-up time t required.")
  if ((type == 4)&((t == 0)|(t0 == 0))) stop("Non-zero total follow-up time t and recruitment time t0 required.")
  
  #***to do: if hazard rates not provided, but survival probs***
  #***to do: other distributions: Cox, proportional hazards***
  
  Zalpha = qnorm(1-alpha/numsides)
  Zbeta = qnorm(1-beta)
  
  #source: Friedman et al 1998 (ch7, Fundamentals of Clinical Trials 3rd ed.)
  
  #sample size, no censoring
  #survival time exponentially distributed, no censoring
  if (type == 1) n = ( 2*(Zalpha + Zbeta)^2 ) / ( log(lambda_ctrl/lambda_int)^2 )
  
  #survival time exponentially distributed, censoring, instantaneous recruitment and follow up over time t
  if (type == 2) phi <- function(lambda,t,...) {(lambda^2)/(1-exp(-lambda*t))}
  
  #survival time exponentially distributed, censoring, continuous recruitment over time t
  if (type == 3) phi <- function(lambda,t,...) {((lambda^3)*t)/(lambda*t-1+exp(-lambda*t))}
  
  #survival time exponentially distributed, censoring, recruitment over time t0, with total follow-up time t
  if (type == 4) phi <- function(lambda,t,t0) {(lambda^2)/(1- (exp(-lambda*(t-t0))-exp(-lambda*t))/(lambda*t0))}
  
  #sample size, with censoring  
  if ((type >= 2)&(type <= 4)) n = ( ((Zalpha + Zbeta)^2)*(phi(lambda_ctrl,t,t0)+phi(lambda_int,t,t0)) ) / ( (lambda_int-lambda_ctrl)^2 )  
  
  #factorial study
  if (factorial == TRUE) n = 2*n
  
  #adjustments
  n = adjustments(n = n, exp_loss_followup_pct = exp_loss_followup_pct, exp_dropout_pct = exp_dropout_pct, exp_dropin_pct = exp_dropin_pct, unequal_groups = unequal_groups, unequal_q1 = unequal_q1, unequal_q2 = unequal_q2, correction_pct = correction_pct)
  
  return(ceiling(n))
  
}


#examples that work
# > studies$number[49]
# [1] "ACTRN12613000376741"
# > studies$number[457]
# [1] "ACTRN12613001147774"


# big loop
N = nrow(studies)
update = NULL
for (k in 1:N) {
  
  if (k %in% seq(0,N,100)) print(k)
  
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
  crossover = (this_study$assignment == "Crossover")
  factorial = (this_study$assignment == "Factorial") 
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
save(studies, excluded, file = 'data/Stats_Sections_Processed_wParams_wN.RData')
