## Code adapted from Katherine Hoffman's lmtp-tutorial repo

# survival changed to estimate

summarize_results <- function(est_trt, est_ctl, ci_level = 0.95,
                              ci_type = c("marginal", "simult")) {
  #############################################################################
  # summarizes results of timepoint-specific effect estimates
  # output: (list with 2 elements)
  #   * tables of contrast-specific effect estimates
  #   * table of difference of effect estimates across contrasts
  # input: lists of lmtp output objects at each timepoint; nominal CI level
  #############################################################################
  ci_type <- match.arg(ci_type)
  
  time_zero <- tibble(
    ci_lwr = 0,
    ci_upr = 0,
    est = 0,
    std_err = 0,
    time = 0
  )

  # loop over treatment and control contrasts
  effect_summary <- lapply(list(est_trt, est_ctl), function(lmtp_est) {
    # extract point and SE estimates, and CI, across all t
    effect_est <- do.call(c, lapply(lmtp_est, `[[`, "theta"))
    std_err <- do.call(c, lapply(lmtp_est, `[[`, "standard_error"))
    ci_upr <- do.call(c, lapply(lmtp_est, `[[`, "high"))
    ci_lwr <- do.call(c, lapply(lmtp_est, `[[`, "low"))
    
    # concatenate into tibble for output
    effect_results <- as_tibble(
      list(
        ci_lwr = ci_lwr, #just lower CI
        est = effect_est, # just effect
        ci_upr = ci_upr, # just upper CI
        std_err =  std_err,
        time = seq_along(lmtp_est)
      ))
    return(effect_results)
  }) |> set_names(c("trt", "ctl"))
  
  # effect_summary <-
  #   effect_summary |>
  #   mutate(ci_lwr = 1 - ci_upr, # switch to mortality estimates
  #          est = 1 - effect_est,
  #          ci_upr = 1 - ci_lwr,
  #          std_err =  std_err) |>
  #  left_join(time_zero) |> # add in time zero numbers 
  #   
  # extract EIF estimates for delta method
  eif_summary <- lapply(list(est_trt, est_ctl), function(lmtp_est) {
    # extract EIF estimates across all t
    eif_mat <- do.call(cbind, lapply(lmtp_est, `[[`, "eif"))
    return(eif_mat)
  }) |> set_names(c("trt", "ctl"))

  # use isotonic regression to enforce monotonicity
  effect_summary$trt <- isoproj(effect_summary$trt, eif_summary$trt,
                              ci_level, ci_type)
  effect_summary$ctl <- isoproj(effect_summary$ctl, eif_summary$ctl,
                              ci_level, ci_type)

  # simple delta method for difference
  n_obs <- nrow(eif_summary[[1]])
  effect_diff_est <- effect_summary$trt$est - effect_summary$ctl$est
  eif_diff_est <- eif_summary$trt - eif_summary$ctl
  std_err_diff_est <- sqrt(matrixStats::colVars(eif_diff_est) / n_obs)

  # compute scalar multiplier for CI or simultaneous band
  if (ci_type == "marginal") {
    ci_mult <- abs(qnorm(p = (1 - ci_level) / 2))
  } else if (ci_type == "simult") {
    ci_mult <- cb_simult(eif_diff_est, ci_level)
  }

  # create summary table of effect difference
  effect_diff_summary <- as_tibble(
    list(
      effect_est = effect_diff_est,
      std_err = std_err_diff_est
    )) |>
    mutate(
      ci_lwr = effect_est - ci_mult * std_err,
      ci_upr = effect_est + ci_mult * std_err,
      test_stat = effect_est / std_err,
      pval = 2 * (1 - pnorm(abs(test_stat)))
    ) |>
    relocate(ci_lwr, effect_est, ci_upr, std_err) |>
    rownames_to_column(var = "time") |>
    mutate(time = as.numeric(time))
  data.table::setattr(effect_diff_summary, "ci_type", ci_type)
  
  effect_summary <- map(1:2, function(x){
    full_join(effect_summary[[x]], time_zero)# |>
    #mutate(ci_upr = 1-ci_lwr,
    #       ci_lwr = 1-ci_upr,
   #        est = 1-est
    #       )
    })
  effect_diff_summary <- full_join(effect_diff_summary, time_zero |> rename(effect_est = est))# |>
    # mutate(ci_upr = ci_lwr,
    #        ci_lwr = ci_upr,
    #        effect_est = -effect_est
    # )
    
  # output both corrected effect estimates and difference estimate
  out <- list(effect_est = effect_summary, diff_est = effect_diff_summary)
  return(out)
}

isoproj <- function(est, eif, ci_level, ci_type = c("marginal", "simult")) {
  #############################################################################
  # projects effect estimates via isotonic regression, enforcing monotonicity
  # output: table of monotonic effect estimates across several timepoints
  # input: tables of effect estimates and corresponding EIFs at timepoints
  #############################################################################
  ci_type <- match.arg(ci_type)

  # compute CI multiplier constant
  if (ci_type == "marginal") {
    ci_mult <- abs(qnorm(p = (1 - ci_level) / 2))
  } else if (ci_type == "simult") {
    ci_mult <- cb_simult(eif, ci_level)
  }

  # projection by isotonic regression
  effect_isoproj <- isotone::gpava(z = est$time, y = 1 - est$est)
  est$est <- effect_est_iso <- 1 - effect_isoproj$y

  # construct CIs around corrected point estimates
  est$std_err <- se_eif <- sqrt(matrixStats::colVars(eif) / nrow(eif))

  # reconstruct CIs afer isotonicity correction
  # NOTE: Westling et al. claim (right after their Corollary 1, p3041) that
  # "Theorem 2 implies that Wald-type confidence bands constructed around [the
  # original estimate] have the same asymptotic coverage if they are
  # constructed around [the corrected estimate] instead."
  if (ci_type == "marginal") {
    # standard confidence limits can just be symmetric around isotonic
    # projection-corrected point estimates
    est$ci_lwr <- effect_est_iso_cil <- effect_est_iso - ci_mult * se_eif
    est$ci_upr <- effect_est_iso_ciu <- effect_est_iso + ci_mult * se_eif
  } else if (ci_type == "simult") {
    # isotonic projection of lower limit of simultaneous confidence band
    effect_cil_isoproj <- isotone::gpava(
      z = est$time, y = 1 - (effect_est_iso - ci_mult * se_eif)
    )
    est$ci_lwr <- effect_est_iso_cil <- 1 - effect_cil_isoproj$y

    # isotonic projection of upper limit of simultaneous confidence band
    effect_ciu_isoproj <- isotone::gpava(
      z = est$time, y = 1 - (effect_est_iso + ci_mult * se_eif)
    )
    est$ci_upr <- effect_est_iso_ciu <- 1 - effect_ciu_isoproj$y
  }
  data.table::setattr(est, "ci_type", ci_type)
  return(est)
}

cb_simult <- function(eif, ci_level) {
  #############################################################################
  # computes the multiplier for a simultaneous confidence band at a given level
  # output: scalar multiplier for creating a simultaneous confidence band
  # input: estimated EIF matrix and nominal confidence level for the band
  #############################################################################
  vcov_eif <- stats::cov(eif)
  rho_eif <- vcov_eif / sqrt(tcrossprod(diag(vcov_eif)))
  mvtnorm_eif <- mvtnorm::qmvnorm(ci_level, tail = "both", corr = rho_eif)
  ci_scaling <- abs(mvtnorm_eif$quantile)
  return(ci_scaling)
}

