# 99_sample_size_functions.R
# sample size functions
# September 2023

#adjustments to sample size
adjustments <- function(n, 
                        exp_loss_followup_pct = 0, 
                        exp_dropout_pct = 0, 
                        exp_dropin_pct = 0, 
                        unequal_groups = FALSE, 
                        unequal_q1 = 0.5, 
                        unequal_q2 = 0.5, 
                        correction_pct = 0) {
  
  #misc correction
  n = n*(1 + (correction_pct/100))
  
  #compliance
  n = n/(1-(exp_dropout_pct/100)-(exp_dropin_pct/100))^2
  
  #unequal sized groups
  #q1 and q2 are the propn of participants in each grp, so that nq1 and nq2 are the number of participants allocated to each grp
  if (unequal_groups == TRUE) n = n*(1/4)*(1/unequal_q1+1/unequal_q2)
  
  #loss to follow up
  #*do this last after all other adjustments*
  n = n/(1-(exp_loss_followup_pct/100))
  
  return(n)
  
}

##continuous/normal
#difference in means, gives n per group unless crossover study
n_diffmeans <- function(expect_diff = NA, 
                        sd_diff = NA, 
                        mu0 = NA, 
                        mu1 = NA,  
                        sd0 = NA, 
                        sd1 = NA,  
                        n0 = NA, 
                        n1 = NA, 
                        alpha = 0.05, 
                        beta = 0.2, 
                        numsides = 2, 
                        type = 1, 
                        crossover = FALSE, 
                        factorial = FALSE, 
                        cluster_rand = FALSE, 
                        m = NA, 
                        icc = NA,
                        exp_loss_followup_pct = 0, 
                        exp_dropout_pct = 0, 
                        exp_dropin_pct = 0, 
                        n_groups = 2, # added by AGB, number of groups/arms
                        unequal_groups = FALSE, 
                        unequal_q1 = 0.5, 
                        unequal_q2 = 0.5, 
                        correction_pct = 0) {
  
  if (is.na(expect_diff)) expect_diff = mu1 - mu0
  if (is.na(sd_diff)) sd_diff = sqrt(sd1^2/n1 + sd0^2/n0)
  
  #catch input error
  if (is.na(expect_diff)&(is.na(mu0)|is.na(mu1))) stop("Need either expect_diff or both mu0 and mu1.")
  if (is.na(sd_diff)&(is.na(sd0)|is.na(sd1)|is.na(n0)|is.na(n1))) stop("Need either sd_diff or all of sd0, sd1, n0, and n1.")
  if (is.na(expect_diff)|is.na(sd_diff)) stop("Both expect_diff and sd_diff are required.")
  if ((cluster_rand == TRUE)&(is.na(m)|is.na(icc))) stop("For a cluster-randomised trial, m (cluster size) and icc (intracluster correlation coefficient) are required.")
  
  #sample size per group
  Zalpha = qnorm(1-alpha/numsides)
  Zbeta = qnorm(1-beta)
  n = 2*((sd_diff^2)/(expect_diff^2))*(Zalpha+Zbeta)^2 
  
  #alternative formula as implemented in nQuery, from Dixon & Massey 1983
  if (type == 2) n = n + Zalpha^2/4
  
  #crossover study
  if (crossover == TRUE) n = ((sd_diff^2)/(expect_diff^2))*(Zalpha+Zbeta)^2 #total sample size, outcome measured twice on each subject
  
  #factorial study
  if (factorial == TRUE) n = 2*n #assuming 2x2 factorial study, will need to generalise
  
  #cluster randomised trial, Friedman et al 1998 (ch7, Fundamentals of Clinical Trials 3rd ed.); design effect
  if (cluster_rand == TRUE) n = n*(1+(m-1)*icc)
  
  #adjustments
  n = adjustments(n = n, exp_loss_followup_pct = exp_loss_followup_pct, exp_dropout_pct = exp_dropout_pct, exp_dropin_pct = exp_dropin_pct, unequal_groups = unequal_groups, unequal_q1 = unequal_q1, unequal_q2 = unequal_q2, correction_pct = correction_pct)
  
  # total sample size (n multiplied by groups)
  n = n * n_groups
  
  return(ceiling(n))
  
}

#binary/binomial
#difference in proportions, gives n per group unless crossover study 
n_diffprops <- function(p_ctrl = NA, 
                        p_int = NA, 
                        arr = NA, 
                        rrr = NA, 
                        f = NA, 
                        delta = NA,
                        alpha = 0.05, 
                        beta = 0.2, 
                        numsides = 2,
                        type = 1, 
                        crossover = FALSE, 
                        factorial = FALSE, 
                        cluster_rand = FALSE, 
                        m = NA, 
                        kappa = NA, 
                        pstar = NA,
                        exp_loss_followup_pct = 0, 
                        exp_dropout_pct = 0, 
                        exp_dropin_pct = 0, 
                        unequal_groups = FALSE, 
                        unequal_q1 = 0.5, 
                        unequal_q2 = 0.5, 
                        correction_pct = 0) {
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
n_survival <- function(lambda_int = NA, 
                       lambda_ctrl = NA, 
                       alpha = 0.05, 
                       beta = 0.2, 
                       numsides = 2,
                       t = 0, 
                       t0 = 0,
                       type = 1, 
                       factorial = FALSE, 
                       exp_loss_followup_pct = 0, 
                       exp_dropout_pct = 0, 
                       exp_dropin_pct = 0, 
                       unequal_groups = FALSE, 
                       unequal_q1 = 0.5, 
                       unequal_q2 = 0.5, 
                       correction_pct = 0) {
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
