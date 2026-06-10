#' Runs Exploratory Structural Equation Models based on Confirmatory and
#' Exploratory Factor Analyses model outputs and keys.
#'
#' esem.from.keys runs an exploratory structural equation model (ESEM) in lavaan
#' where the exploratory factor analysis (EFA) factors predict either
#' confirmatory factor analysis (CFA) factors in separate models for each CFA
#' factor OR bifactor factors in separate models for each bifactor model.
#' The function uses outputs from `efa.from.keys()` and either `cfa.from.keys()`
#' or `bifactor.from.keys` so it cannot be run without first running the
#' relevant upstream functions.
#'
#' @param efa_fit A fitted lavaan object of an EFA model.
#' @param cfa_fit
#' A named list of fitted lavaan objects of CFA models.
#' Can be `NULL` if `bif_fit` is not `NULL`.
#' @param bif_fit
#' A list of fitted lavaan objects of bifactor models.
#' Can be `NULL` if `cfa_fit` is not `NULL`.
#' @param efa_keys
#' A named list of keys. Names should be factor names, elements should be
#' vectors of items that comprised the factors in the EFA of `efa_fit`.
#' All items in the `efa_fit` object must be included an none excluded.
#' @param cfa_keys
#' A named list of keys. Names should be scale names, elements should a list of
#' items included in each scale.
#' `length(cfa_keys)` should equal `length(cfa_fit)`.
#' Can be `NULL` if `bif_keys` is not `NULL`.
#' @param bif_keys
#' Must match `keys` used in a previously run `bifactor.from.keys` function
#' call.
#' @param d
#' The data. This must include all observed variables used in any of the models.
#' @param name
#' A name for the collection of models. Defaults to 'esem'.
#' The name should be unique for each different function call from the
#' package or outputs from other calls will be overwritten.
#' @param out_dir
#' The directory where all function outputs will be saved. Defaults to 'output'.
#' @param fit_save `TRUE` to save model fit measures. `FALSE` otherwise.
#' @param fit_measures
#' A vector of fit measures to save, or `NULL` to select all fit measures.
#' Defaults to `NULL`. Irrelevant if `fit_save = FALSE`.
#' @param miss Sets the `missing` param, as per lavaan. Defaults to 'ML'.
#' @param hash_dir
#' A subdirectory of `out_dir` where data hashes are saved.
#' Defaults to 'hashes'.
#' @param check
#' Should the code check to see if previous outputs have been saved?
#' If `TRUE`, the model will not run if model code and a data hash have not
#' changed and output is of class lavaan.
#' If `FALSE`, the model will run regardless of the existence of previous
#' outputs.
#' @param save_out
#' Should outputs be saved to enable checking next time?
#' If `TRUE` model code, a hash of the data, and output will be saved and will
#' be checked against for changes next time the code is run.
#' If `FALSE`, nothing will be saved, the output will simply be returned as per
#' normal R functioning. Next time the code is run, models will be re-estimated
#' regardless of changes to code or data.
#'
#' @return
#' Returns a length 2 or 3 list of lists.
#' The first elements of the list is a list of fitted lavaan esem model output
#' objects of the same length as either `cfa_keys` or `bif_keys` (depending
#' on which is used).
#' The second element of the list is a list of parameter estimates from the
#' models (standardized if `std = TRUE`).
#' If `fit_save = TRUE`, then the list will have a third element, which will be
#' a matrix of fit measures for each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) where EFA factors predict a series of latent variables
#' in separate models using Burt's (1976) 2-stage procedure to prevent
#' interpretational confounding. The function is designed to reproduce the
#' analyses of Bainbridge, Ludeke, and Smillie (2022).
#'
#' Although the function will take any lavaan models that have successfully
#' produced standard lavaan outputs,
#' it will not complain if these have poor fit or other undesirable
#' characteristics.
#' If this matters to you, it would be worth checking the outputs of upstream
#' functions prior to running `esem.from.mods()`.
#'
#' The 2-stage procedure employed mitigates some of the problems of poor fitting
#' measurement models but, depending on your purpose and research
#' question(s), it could nevertheless be important to update models to ensure
#' good fit prior to running this function. There is currently no capability to
#' add modifications to CFA or bifactor models beyond removing items, however.
#'
#' @references
#' Bainbridge, T. F., Ludeke, S. G., & Smillie, L. D. (2022).
#' Evaluating the Big Five as an organizing framework for commonly used
#' psychological trait scales.
#' Journal of Personality and Social Psychology, 122(4), 749-777.
#'
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models.
#' Sociological Methods & Research, 5(1), 3-52.

# TODO: check @return in case of adding cfa and bifactor models together.
# TODO: Check bifactor model functionality.
# TODO: Add option for stand-alone items (in place of CFA or bifactor factors).

esem.from.mods <- function(
    efa_fit, cfa_fit = NULL, bif_fit = NULL, efa_keys, cfa_keys = NULL,
    bif_keys = NULL,
    d, name = "esem", out_dir = "output", fit_save = FALSE, fit_measures = NULL,
    miss = "ML", hash_dir = "hashes", check = TRUE, save_out = TRUE
) {
  if (is.null(cfa_fit) & is.null(bif_fit)) {
    stop("At least one of `cfa_fit` and `bif_fit` must be specified.")
  }
  if (sum(sapply(names(cfa_fit), function(x) x == "")) > 0) {
    stop(
      "At least one element of `cfa_fit` is unnamed. All elements must be named"
    )
  }
  if (sum(sapply(names(bif_fit), function(x) x == "")) > 0) {
    stop(
      "At least one element of `bif_fit` is unnamed. All elements must be named"
    )
  }
  if (sum(sapply(names(efa_fit), function(x) x == "")) > 0) {
    stop(
      "At least one element of `efa_fit` is unnamed. All elements must be named"
    )
  }
  if (sum(sapply(cfa_fit, function(x) class(x) != "lavaan")) > 0) {
    paste0(
      names(cfa_fit)[sapply(cfa_fit, function(x) class(x) != "lavaan")],
      stop("The above elements of `cfa_fit` are not objects of type lavaan.")
    )
  }
  if (sum(sapply(bif_fit, function(x) class(x) != "lavaan")) > 0) {
    paste0(
      names(bif_fit)[sapply(bif_fit, function(x) class(x) != "lavaan")],
      stop("The above elements of `bif_fit` are not objects of type lavaan.")
    )
  }
  if (class(efa_fit) != "lavaan") {
    stop("`efa_fit` is not an object of type lavaan.")
  }
  if (!is.null(cfa_fit)) {
    cfa_par <- sapply(cfa_fit, parameterEstimates, simplify = FALSE)
  }
  if (!is.null(bif_fit)) {
    bif_par <- sapply(bif_fit, parameterEstimates, simplify = FALSE)
  }
  efa_par <- parameterEstimates(efa_fit)
  if (!is.null(cfa_fit)) {
    mods_cfa <- mapply(
      function(x, sn) {
        if (length(x) > 1) {
          x0 <- cfa_par[[sn]]
          tmp0 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
          tmp1 <- tmp0[!(sn == tmp0$lhs & sn == tmp0$rhs), ]
          tmp <- paste0(
            paste(tmp1$lhs, tmp1$op, tmp1$est, "*", tmp1$rhs, collapse = "\n")
          )
        } else {
          tmp <- NULL
        }
        tmpe0 <- efa_par[[efa_name]]
        tmpe <- tmpe0[tmpe0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
        paste0(
          c(
            # EFA as CFA
            paste(tmpe$lhs, tmpe$op, tmpe$est, "*", tmpe$rhs, collapse = "\n"),
            # Scale CFA or Latent variable correlations (if only 1 item)
            if (length(x) != 1) tmp else {
              paste0(sn, " ~~ ", paste0(names(efa_keys), collapse = " + "))
            },
            # Regression
            paste(sn, "~", paste0(names(efa_keys), collapse = " + "))
          ),
          collapse = "\n"
        )
      },
      x = cfa_keys, sn = names(cfa_keys), SIMPLIFY = FALSE
    )
  }
  if (!is.null(bif_fit)) {
    mods_bif <- mapply(
      function(x, sn) {
        if (length(x) > 1) {
          x0 <- bif_par[[sn]]
          tmp0 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
          tmp1 <- tmp0[!(sn == tmp0$lhs & sn == tmp0$rhs), ]
          tmp <- paste0(
            paste(tmp1$lhs, tmp1$op, tmp1$est, "*", tmp1$rhs, collapse = "\n")
          )
        } else {
          tmp <- NULL
        }
        tmpe0 <- efa_par[[efa_name]]
        tmpe <- tmpe0[tmpe0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
        paste0(
          c(
            # EFA as CFA
            paste(tmpe$lhs, tmpe$op, tmpe$est, "*", tmpe$rhs, collapse = "\n"),
            # Scale CFA or Latent variable correlations (if only 1 item)
            if (length(x) != 1) tmp else {
              paste0(sn, " ~~ ", paste0(names(efa_keys), collapse = " + "))
            },
            # Regression
            paste(sn, "~", paste0(names(efa_keys), collapse = " + "))
          ),
          collapse = "\n"
        )
      },
      x = bif_keys, sn = names(bif_keys), SIMPLIFY = FALSE
    )
  }
  mods <- c(mods_cfa, mods_bif)
  mod_out <- sem.check(
    mods,
    d,
    name = name,
    kl_s = cfa_keys,
    kl_e = efa_keys,
    std = TRUE,
    fit_save = fit_save,
    fit_measures = fit_measures,
    hash_dir = hash_dir,
    miss = miss,
    std.lv = FALSE,  # Params are set from measurement models.
    check = check,
    save_out = save_out
  )
  r2 <- mapply(
    function(x, xn) {
      tmp <- x[x$op == "~~" & x$lhs == xn & x$rhs == xn, ]
      c(
        R2 = 1 - tmp$est.std,
        se = tmp$se,
        ci.lower = 1 - tmp$ci.upper,
        ci.upper = 1 - tmp$ci.lower
      )
    },
    x = mod_out$par_std, xn = names(mod_out$par_std), SIMPLIFY = FALSE
  ) |>
    do.call(rbind, args = _)
  b <- lapply(
    mod_out$par_std,
    function(x, xn) {
      tmp <- x[x$op == "~", ]
      tmp[-(1:2)]
    }
  )
  return(
    list(
      fit = mod_out$fit,
      par_std = mod_out$par_std,
      b = b,
      r2 = r2
    )
  )
}
