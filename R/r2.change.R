#' Runs incremental validity analysis using latent variables and fitted
#' measurement models
#'
#' @description
#' Incremental validity using standard linear regressions is invalid with less
#' than perfect reliability. Latent variable models must be used instead.
#' However, this is difficult, especially if confidence intervals are reqiured,
#' which can only be created with bootstrap sampling.
#'
#' `r2.change` streamlines the process of running incremental validity
#' analyses with latent variables, including generating bootstrap confidence
#' intervals.
#'
#' @inheritParams sem.check
#' @param cfa_fit A named list of fitted lavaan objects of CFA models.
#' @param bif_fit A named list of fitted lavaan objects of bifactor models.
#' @param X
#' A length 1 character vector of the name of the the variable being tested.
#' @param Y A length 1 character vector of the name of the outcome variable.
#' @param Z
#' A character vector of the set of variables that X is being pitted against.
