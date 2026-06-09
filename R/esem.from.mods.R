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
#' @param efa_name
#' Must match `name` used in a previously run `efa.from.keys` function call.
#' @param cfa_name
#' Must match `name` used in a previously run `cfa.from.keys` function call.
#' @param efa_keys
#' Must match `keys` used in a previously run `efa.from.keys` function call.
#' @param cfa_keys
#' Must match `keys` used in a previously run `cfa.from.keys` function call.
#' @param d
#' The data. This must include all observed variables used in any of the models.
#' @param name
#' A name for the collection of models. Defaults to 'esem'.
#' The name should be unique for each time any function is called from the
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
#'
#' @return
#' Returns a length 2 or 3 list of lists.
#' The first elements of the list is a list of fitted lavaan esem model output
#' objects of the same length as either `cfa_keys` or `bif_keys_g` (depending
#' on which is used).
#' The second element of a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_measures` is not FALSE, a matrix of fit measures for each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) where EFA factors predict a series of latent variables
#' in separate models using Burt's (1976) 2-stage procedure to prevent
#' interpretational confounding. The function is designed to reproduce the
#' analyses of Bainbridge, Ludeke, and Smillie (2022).
#'
#' Although the function will take any lavaan models that have successfully
#' produced standard lavaan outputs (i.e., fitted objects with non-NA parameter
#' estimates),
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

# TODO: Add bifactor model functionality.

esem.from.mods <- function(
    efa_name, cfa_name = NULL, bif_name = NULL, efa_keys, cfa_keys = NULL,
    bif_keys_g = NULL, bif_keys_b = NULL, bif_keys = NULL,
    d, name = NULL, out_dir = "output", fit_save = FALSE, fit_measures = NULL,
    miss = "ML", hash_dir = "hashes"
) {
  if (
    !file.exists(file.path(out_dir, cfa_name, paste0(cfa_name, "_par.rds")))
  ) {
    stop(
      paste0(
        cfa_name, "_par.rds", " not found at ",
        file.path(out_dir, cfa_name, paste0(cfa_name, "_par.rds")),
        ". You will need to run at least one CFA model using",
        "`cfa.from.keys()` for this object to be saved.",
        "If you have done that, ensure that `cfa_name` here matches the one",
        "you used to run the CFA models originally."
      )
    )
  }
  cfa_par <-
    readRDS(file.path(out_dir, cfa_name, paste0(cfa_name, "_par.rds")))
  if (
    !file.exists(file.path(out_dir, efa_name, paste0(efa_name, "_par.rds")))
  ) {
    stop(
      paste0(
        efa_name, "_par.rds", " not found at ",
        file.path(out_dir, efa_name, paste0(efa_name, "_par.rds")),
        ". You will need to run an EFA model using",
        "`efa.from.keys()` for this object to be saved.",
        "If you have done that, ensure that `efa_name` here matches the one",
        "you used to run the efa model originally."
      )
    )
  }
  efa_par <-
    readRDS(file.path(out_dir, efa_name, paste0(efa_name, "_par.rds")))
  if (
    !file.exists(file.path(out_dir, bif_name, paste0(bif_name, "_par.rds")))
  ) {
    stop(
      paste0(
        bif_name, "_par.rds", " not found at ",
        file.path(out_dir, bif_name, paste0(bif_name, "_par.rds")),
        ". You will need to run at least one bifactor model using",
        "`bifactor.from.keys()` for this object to be saved.",
        "If you have done that, ensure that `bif_name` here matches the one",
        "you used to run the bifactor models originally."
      )
    )
  }
  bif_par <-
    readRDS(file.path(out_dir, bif_name, paste0(bif_name, "_par.rds")))
  # TODO: Check that the _par.rds files matches expectations.
  if (is.null(cfa_name) & is.null(bif_name)) {
    stop("Either ")
  }
  if (!is.null(cfa_name)) {
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
    std.lv = FALSE  # Params are set from measurement models.
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
