#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `esem.from.keys` runs exploratory structural equation models (ESEM) in lavaan
#' where the exploratory factor analysis (EFA) factors predict confirmatory
#' factor analysis (CFA) factors and/or bifactor factors in separate models
#' for each CFA or bifactor model.
#' The function takes a fitted lavaan object from an EFA and lists of fitted
#' CFA and/or bifactor lavaan model objects as inputs so will typically
#' use outputs from [efa.from.keys()], and [cfa.from.keys()] or
#' [bifactor.from.keys()].
#'
#' @inheritParams sem.check
#' @param efa_fit A fitted lavaan object of an EFA model.
#' @param cfa_fit
#' A named list of fitted lavaan objects of CFA models.
#' Can be `NULL` if `bif_fit` is not `NULL`.
#' @param bif_fit
#' A named list of fitted lavaan objects of bifactor models.
#' Can be `NULL` if `cfa_fit` is not `NULL`.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "esem".
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models or outputs from calls with
#' the same name will be overwritten.
#'
#' @return
#' Returns a list of length 4 (if `fit_save = FALSE`) or
#' 5 (if `fit_save = TRUE`).
#' The elements of the list are: a list of lavaan model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' if `fit_save = TRUE`, a matrix of fit measures for each model;
#' a list of regression beta parameters from each model;
#' and a dataframe of R-squared values from each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) where EFA factors predict a series of latent variables
#' in separate models using Burt's (1976) 2-stage procedure to prevent
#' interpretational confounding. The function is designed to run analyses
#' equivalent to that of Bainbridge, Ludeke, and Smillie (2022).
#'
#' The function requires fitted lavaan objects as inputs in order to properly
#' employ the 2-stage procedure.
#' Using [efa.from.keys()], [cfa.from.keys()], and/or [bifactor.from.keys()]
#' should make this relatively straight-forward.
#'
#' Matching the philosophy of the package, the function is designed to run for
#' multiple models with a similar design. If you are using the function for a
#' single model, transform inputs into lists as appropriate.
#'
#' The model relies on [sem.check()] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' For further details on how this works, see the [sem.check()] function
#' documentation.
#'
#' Although the function will take any lavaan models that have successfully
#' produced standard lavaan outputs,
#' it will not complain if these have poor fit or other undesirable
#' characteristics (beyond warnings and errors produced by lavaan).
#' If this matters to you, it would be worth checking the outputs of upstream
#' functions prior to running `esem.from.mods()`.
#'
#' The 2-stage procedure employed mitigates some of the problems of poor fitting
#' measurement models but, depending on your purpose and research
#' question(s), it could nevertheless be important to update models to ensure
#' good fit prior to running this function. There is currently no capability
#' within `semFromKeys` to add modifications to CFA or bifactor models
#' (beyond removing items by removing them from the keys).
#'
#' For bifactor models predicted by ESEM factors, a complication is that fixing
#' orthogonal relationships between general and group factors is not possible
#' if any of the factors are regressed on any others.
#' This is because, in SEM, fixing correlations with a factor that is an outcome
#' fixes correlations with the residual,
#' which is, of course, not the requirement of a bifactor model.
#'
#' There are three solutions to this problem.
#' First, avoid using bifactor models.
#' This may be possible sometimes, but it frequently is not.
#' Second, avoid using regressions in models with bifactor models and compute
#' regressions based on latent variable correlations.
#' (This can be done by, e.g., creating a correlation matrix from the factor
#' correlations and using the [psych::setCor()] function).
#' This method produces accurate point estimates in the regressions,
#' but standard errors are biased as the method cannot account for uncertainty
#' in the correlations.
#' This is an adequate solution if p-values and confidence intervals are not
#' required.
#' Finally, if the 2-stage procedure is employed, then there is little room for
#' measurement models to change to allow factor correlations to change.
#' Therefore, the parameter can be relaxed and implied correlations between the
#' group and general factors ought to remain close to zero.
#' This is the method employed by `esem.from.mods()`.
#'
#' @seealso
#' [sem.check()], which this function uses for all the back-end;
#' [cfa.from.keys()], [efa.from.keys()], and [bifactor.from.keys()],
#' which are useful functions for creating inputs into `esem.from.mods()`; and
#' [lavaan::sem()], which is used to estimate the models.
#'
#' @references
#' Bainbridge, T. F., Ludeke, S. G., & Smillie, L. D. (2022).
#' Evaluating the Big Five as an organizing framework for commonly used
#' psychological trait scales.
#' Journal of Personality and Social Psychology, 122(4), 749-777.
#' doi.org/10.1037/pspp0000395.
#'
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.
#' doi.org/10.1177/004912417600500101.
#'
#' @importFrom lavaan summary
#' @export
#'
#' @examples
#' # Create CFA keys
#' keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#' keys <- sapply(
#'   keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' # Create bifactor keys
#' keys_g0 <- c("grit", "hope")
#' keys_g <- sapply(
#'   keys_g0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' keys_b1 <- sapply(
#'   keys_g0, function(x) keys0[grep(x, keys0)], simplify = FALSE
#' )
#' keys_b <- keys_b1
#' keys_b$hope <- keys_b1$hope[-1]
#' # Create EFA keys
#' # Using only 3 factors to save time
#' keys_e0 <- paste0("bfi_", c("e", "a", "c"))
#' # Using less than all items to save time on checks
#' # (This results in a less than ideal solution but it doesn't matter for an
#' # example)
#' keys_e <- sapply(
#'   keys_e0,
#'   function(x) {
#'     names(BFIGritHope)[grep(paste0(x, "\\d_[1-2]"), names(BFIGritHope))]
#'   },
#'   simplify = FALSE
#' )
#' # Create fitted objects to use as inputs
#' cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = FALSE)
#' efa_fit <-
#'   efa.from.keys(keys_e, BFIGritHope, check = FALSE, fit_save = FALSE)
#' bif_fit <- bifactor.from.keys(
#'   keys_g, keys_b, keys, BFIGritHope, check = FALSE, fit_save = FALSE
#' )
#' # Run models
#' esem_fit <- esem.from.mods(
#'   efa_fit$fit$efa, cfa_fit$fit, bif_fit$fit, data = BFIGritHope,
#'   fit_save = FALSE, check = FALSE
#' )
#' # Examine results
#' summary(esem_fit$fit$grit_c)  # Standard lavaan summary
#' esem_fit$par$grit_c           # Parameter estimates
#' esem_fit$r2                   # R-squareds
#' esem_fit$b                    # Betas

esem.from.mods <- function(
    efa_fit, cfa_fit = NULL, bif_fit = NULL, data,
    fit_save = FALSE, fit_measures = "all", miss = "ML", est = "default",
    name = "esem", check = FALSE, save_out = FALSE
) {
  if (is.null(cfa_fit) & is.null(bif_fit)) {
    stop("At least one of `cfa_fit` and `bif_fit` must be specified.")
  }
  if (!is.null(cfa_fit)) {
    if (sum(sapply(cfa_fit, function(x) !inherits(x, "lavaan"))) > 0) {
      paste0(
        names(cfa_fit)[sapply(cfa_fit, function(x) !inherits(x, "lavaan"))],
        stop("The above elements of `cfa_fit` are not objects of type lavaan.")
      )
    }
  }
  if (!is.null(bif_fit)) {
    if (sum(sapply(bif_fit, function(x) class(x) != "lavaan")) > 0) {
      paste0(
        names(bif_fit)[sapply(bif_fit, function(x) !inherits(x, "lavaan"))],
        stop("The above elements of `bif_fit` are not objects of type lavaan.")
      )
    }
  }
  if (is.null(efa_fit)) {
    stop(
      paste(
        "`efa_fit` is NULL.",
        "`efa_fit` must be a fitted lavaan object of an EFA model."
      )
    )
  }
  if (!inherits(efa_fit, "lavaan")) {
    stop("`efa_fit` is not an object of type lavaan.")
  }
  if (!is.null(cfa_fit)) {
    cfa_par <- sapply(cfa_fit, parameterEstimates, simplify = FALSE)
    # Extract factor names
    cfa_names <- sapply(
      cfa_par,
      function(x) {
        x1 <- unique(x$lhs[x$op == "=~"])
        if (length(x1) > 1) {
          stop(
            paste(
              "A CFA containing more than one latent variable has been found.",
              "Currently, the function only supports CFAs included in separate",
              "models.",
              "Please either use `bif_fit` and a model supported there,",
              "or separate the CFAs into separate measurement models.",
              "The offending factors are:\n",
              "    ",
              paste(x1, collapse = "\n    ")
            )
          )
        }
        return(x1)
      }
    )
    names(cfa_par) <- cfa_names
    if (!is.null(names(cfa_fit))) {
      if (sum(names(cfa_fit) != cfa_names) > 0) {
        og_warn <- getOption("warn")
        options(warn = 1)
        warning(
          paste(
            "The names of `cfa_fit` do not match the factor names.",
            "The set of functions in the semFromKeys package assume they do.",
            "Therefore, names of returned objects will not match the names of",
            "the `cfa_fit` input but will instead reflect the factor names."
          )
        )
        options(warn = og_warn)
      }
    }
    cfa_keys <- sapply(cfa_par, function(x) x$rhs[x$op == "=~"])
    names(cfa_keys) <- cfa_names
    if (sum(table(names(cfa_keys)) > 1) > 0) {
      stop(
        paste(
          "At least two different models in `cfa_fit` have factors with the",
          "same name.",
          "Please ensure that all factor names are unique."
        )
      )
    }
  }
  if (!is.null(bif_fit)) {
    bif_par <- sapply(bif_fit, parameterEstimates, simplify = FALSE)
    bif_keys <- sapply(bif_par, function(x) unique(x$rhs[x$op == "=~"]))
    # TODO: What happens if there's an item that's not in the general factor?
    bif_names <- mapply(
      x = bif_par, y = bif_keys,
      FUN = function(x, y) {
        tmp <- table(x$lhs[x$op == "=~" & x$rhs %in% y])
        names(tmp)[tmp == max(tmp)]
      }
    )
    names(bif_par) <- bif_names
    if (!is.null(names(bif_fit))) {
      if (sum(names(bif_fit) != bif_names) > 0) {
        warning(
          paste(
            "The names of `bif_fit` do not match the general factor names.",
            "Names of returned objects are based on factor names",
            "so they will not match the names of `bif_fit`."
          )
        )
      }
    }
    names(bif_keys) <- bif_names
    if (sum(table(names(bif_keys)) > 1) > 0) {
      stop(
        paste(
          "At least two different models in `bif_fit` have general factors",
          "with the same name.",
          "Please ensure that all factor names are unique."
        )
      )
    }
  }
  if (!is.null(cfa_fit) & !is.null(bif_fit)) {
    # TODO: Have I got a test for this?
    if (sum(names(cfa_keys) %in% names(bif_keys)) > 0) {
      stop(
        paste(
          "The following models in `cfa_fit` have identically named",
          "factor(s) in `bif_fit`:\n    ",
          paste(
            names(cfa_fit)[names(cfa_fit) %in% names(bif_fit)], collapse = "\n"
          ),
          "\n\n  Please ensure that CFA factors and bifactor general factors have",
          "unique names."
        )
      )
    }
  }
  efa_par <- parameterEstimates(efa_fit)
  efa_par1 <-
    efa_par[efa_par$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
  efa_mod <- paste(
    efa_par1$lhs, efa_par1$op, efa_par1$est, "*", efa_par1$rhs, collapse = "\n"
  )
  efa_facs <- unique(efa_par$lhs[efa_par$op == "=~"])
  efa_items <- unique(efa_par$rhs[efa_par$op == "=~"])
  efa_keys0 <- as.data.frame(
    t(
      sapply(
        efa_items,
        function(x) {
          tmp <- efa_par[efa_par$op == "=~" & efa_par$rhs == x, ]
          unlist(tmp[abs(tmp$est) == max(abs(tmp$est)), c("lhs", "rhs")])
        }
      )
    )
  )
  efa_keys <- sapply(
    efa_facs, function(x) efa_keys0$rhs[efa_keys0$lhs == x], simplify = FALSE
  )
  if (!is.null(cfa_fit)) {
    mods_cfa <- sapply(
      names(cfa_keys),
      function(x) {
        x0 <- cfa_par[[x]]
        x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
        x2 <- x1[!(x == x1$lhs & x == x1$rhs), ]
        x3 <- paste0(paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n"))
        paste0(
          c(
            efa_mod,
            x3,
            paste(x, "~", paste0(names(efa_keys), collapse = " + "))
          ),
          collapse = "\n"
        )
      },
      simplify = FALSE
    )
  }
  if (!is.null(bif_fit)) {
    mods_bif <- sapply(
      names(bif_keys),
      function(x) {
        x0 <- bif_par[[x]]
        x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
        x2 <- x1[!(x == x1$lhs & x1$op == "~~"), ]
        x3 <- paste0(paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n"))
        groupf <- unique(x0$lhs[x0$op == "=~" & !(x0$lhs %in% x)])
        paste0(
          c(
            efa_mod, x3,
            # Regression
            paste(x, "~", paste0(names(efa_keys), collapse = " + ")),
            # Correlations with group factors
            paste(
              paste(groupf, collapse = " + "),
              "~~",
              paste0(names(efa_keys), collapse = " + "),
              "+",
              x
            )
          ),
          collapse = "\n"
        )
      },
      simplify = FALSE
    )
  }
  if (!is.null(bif_fit) & !is.null(cfa_fit)) {
    mods <- c(mods_cfa, mods_bif)
    keys_s <- c(cfa_keys, bif_keys)
  } else if (!is.null(cfa_fit)) {
    mods <- mods_cfa
    keys_s <- cfa_keys
  } else {
    mods <- mods_bif
    keys_s <- bif_keys
  }
  mod_out <- sem.check(
    mods,
    data,
    name = name,
    keys_s = keys_s,
    keys_e = efa_keys,
    std = TRUE,  # For r2 calcs.
    fit_save = fit_save,
    fit_measures = fit_measures,
    miss = miss,
    est = est,
    std.lv = FALSE,  # Params are set from measurement models.
    check = check,
    save_out = save_out
  )
  r2 <- do.call(
    rbind,
    mapply(
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
    )
  )
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
      fit_measures = mod_out$fit_measures,
      b = b,
      r2 = r2
    )
  )
}
