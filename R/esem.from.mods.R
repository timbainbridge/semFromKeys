#' Runs ESEM models based on CFA and EFA model outputs and keys.
#'
#' esem.from.keys runs a exploratory structural equation model (ESEM) in lavaan
#' where the explorator factor analysis (EFA) factors predict CFA factors in
#' separate models for each CFA factor.
#' The function uses outputs from `cfa.from.keys()` and `efa.from.keys()` and
#' the same keys as those used in the functions.
#'
#' @param cfa_name
#' Must match `name` used in a previously run `cfa.from.keys` function call.
#' @param efa_name
#' Must match `name` used in a previously run `efa.from.keys` function call.
#' @param cfa_keys
#' Must match `keys` used in a previously run `cfa.from.keys` function call.
#' @param efa_keys
#' Must match `keys` used in a previously run `efa.from.keys` function call.
#' @param d
#' The data. This must include all observed variables used in any of the models.
#' @param name A name for the collection of models. Defaults to 'esem'.
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
#' @param mod_dir
#' A subdirectory of `out_dir` where model outputs are saved.
#' If `NULL`, it will be set as 'name'.
#'
#' @return
#' Returns a list of lists.
#' The elements are a list of lavaan bifactor model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_measures` is not FALSE, a matrix of fit measures for each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) where EFA factors predict a series of latent variables
#' in separate models using Burt's (1976) 2-stage procedure to prevent
#' interpretational confounding. The function is designed to reproduce the
#' analyses of Bainbridge, Ludeke, and Smillie (2022).
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
    cfa_name, efa_name, cfa_keys, efa_keys, d, name = "esem",
    out_dir = "output", fit_save = FALSE, fit_measures = NULL, miss = "ML",
    hash_dir = "hashes", mod_dir = NULL
) {
  # TODO: Add checks for cfa_name or cfa_fit and that inputs are correct.
  if (
    !file.exists(file.path(out_dir, cfa_name, paste0(cfa_name, "_par.rds")))
  ) {
    stop(
      paste0(
        cfa_name, "_par.rds", " not found at ",
        file.path(out_dir, cfa_name, paste0(cfa_name, "_par.rds"))
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
        file.path(out_dir, efa_name, paste0(efa_name, "_par.rds"))
      )
    )
  }
  efa_par <-
    readRDS(file.path(out_dir, efa_name, paste0(efa_name, "_par.rds")))
  # TODO: Check that the _par.rds files matches expectations.
  mods <- mapply(
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
  mod_out <- sem.check(
    mods,
    d,
    name = name,
    kl_s = cfa_keys,
    kl_e = efa_keys,
    mod_dir = mod_dir,
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
