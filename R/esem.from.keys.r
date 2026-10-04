#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `esem.from.keys` and `efa.from.mods` run exploratory structural equation
#' models (ESEM) in lavaan where exploratory factor analysis (EFA) factors
#' predict confirmatory factor analysis (CFA) factors.
#'
#' @inheritParams sem.check
#' @param data
#' A dataframe or object coercible to a dataframe.
#' Data must include all observed variables in keys.
#' @param keys_e
#' A named list of keys. Names must be factor names, elements must be
#' vectors of items that should be targeted to load on the factor.
#' @param keys
#' A named list of items in uni-dimensional factors.
#' Names must be the names of the factors.
#' List element must be a vector of items that load on the factors.
#' For bi-factor models, these should be group factor names and items.

# SAM does not currently work with efa and bifactor models.
# Keep this here in case it does at some stage.
# @param keys_b
# A named list of group factors in general factors.
# Names must be the names of the general factors.
# List element must be a vector of group factors, matching factors in `keys`.
# Defaults to `NULL` and should be `NULL` if only single factor models are
# included.

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
#' Defaults to "esam" for `esem.from.keys` and "esem" for `esem.from.mods`.
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.

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
#' The functions streamline running exploratory structural equation models
#' (ESEM) where EFA factors predict a series of latent variables in separate
#' models, similar to the primary analyses of Bainbridge, Ludeke, and Smillie
#' (2022).
#' `esem.from.keys` takes keys lists as inputs and `esem.from.mods` takes fitted
#' measurement models as inputs.
#' To prevent interpretational confounding (Burt, 1976), `esem.from.keys` uses
#' Rosseel and Loh's (2022) Structural After Measurement (SAM) method and
#' `esem.from.mods` uses Burt's (1976) 2-stage procedure.
#' In general, `esem.from.keys` should be used for CFA measurement models and
#' `esem.from.mods` should be used for bifactor measurement models, which are
#' not supported in `esem.from.keys`.
#'
#' The functions rely on [sem.check] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs (with
#' `save_out = TRUE`) and checking to see if anything has changed from prior
#' runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' For further details on how this works, see the [sem.check] function
#' documentation.
#'
#' @section Interpretational Confounding:
#' In Structural Equation Models (SEM), standard methods do not distinguish
#' between measurement and structural parameters.
#' As a result, measurement model parameters can change with the addition of
#' theoretically unrelated constructs in a structural model, and can change
#' differently for different sets of unrelated constructs.
#' This means that the unrelated constructs are changing the interpretation of
#' the latent variable, which Burt (1976) labelled "interpretational
#' confounding".
#'
#' There are various ways to deal with interpretational confounding.
#' The standard solution (other than ignoring it) is to create measurement
#' models with good fit first, then freely estimate the structural model with
#' checks to ensure adequate fit of the model and that measurement parameters do
#' not change substantially with different combinations of factors.
#' This is sometimes a good solution, but, in other cases, it is not.
#' For example, if the measurement model was for a well-established scale and it
#' requires changing, then it loses easy comparison with past research.
#' This issue is most relevant when changes to a measurement model require
#' entirely different factors, or items to be removed but it is still an
#' issue for less dramatic changes. When a single scale is being assessed,
#' these issues can be resolved by suggesting a thorough evaluation of the scale
#' and, perhaps, the suggestion of a new measurement model or a new scale for a
#' particular population; however, when many scales are being assessed this
#' solution is impractical, and may not solve the interpretational confounding
#' issue regardless.
#'
#' An alternative solution, proposed by Burt (1976) is to fix measurement model
#' parameters in a model estimating structural parameters.
#' This method means that misspecification of one measurement model cannot
#' affect other measurement models and that the interpretation of measured
#' constructs cannot change based on unrelated factors.
#' However, it is not a perfect solution---by fixing measurement parameters,
#' uncertainty in their estimation is neglected (e.g., Nagy et al., 2017), which
#' results in biased standard errors and fit statistics.
#'
#' A third option was proposed by Nagy and colleagues (2017), who introduced an
#' extension procedure such that item residuals are allowed to correlate with
#' external variables (or factors).
#' To make the model identifiable, these relationships are constrained using
#' one of a number of methods. If the sums of squares of correlations between
#' each factor's items' residuals and each external factor are minimised,
#' measurement parameters are preserved in the structural model without having
#' to constrain them directly.
#' As a result, unbiased standard errors are preserved while simultaneously
#' eliminating interpretational confounding.
#' Unfortunately, estimating these models becomes increasingly slow with more
#' items and factors, and the method only works with correlations, not
#' regressions, so some method to run regressions using the correlations needs
#' to be implemented.
#' These methods will typically bias the estimates due to ignored uncertainty in
#' the correlation estimates, thereby undermining the primary benefit of the
#' method.
#'
#' A final solution to interpretation confounding was proposed by Rosseel and
#' Loh (2022) with their SAM approach. This method essentially follows Burt's
#' (1976) method but adjust the procedure to overcome its issues.
#' They distinguish two SAM varieties--"local SAM" and "global SAM".
#' Local SAM uses the observed summary statistics of the parameters of the
#' measurement models to generate mean and covariance matrices to use in the
#' structural model, thereby preserving the structure of the measurement models
#' while also preserving the uncertainty.
#' Global SAM treats the measurement parameters as given, but corrects the
#' standard errors of the structural model.
#' Although local SAM is preferable in most circumstances, as implemented in
#' lavaan it currently (as at version 0.7-2) incorrectly sets ESEM factor
#' covariances as equal.
#'
#' @section Bifactor Measurement Models:
#' Bi-factor models are not currently supported in `esem.from.keys`.
#' [lavaan::sam]---the lavaan function that implements the SAM method---
#' currently treats all latent variables that are not in a regression path in
#' the structural model as unrelated to the other factors.
#' Given regressing the general factor of a bi-factor model on EFA factors
#' requires the group factors to correlate with the EFA factors,
#' [lavaan::sam] is currently inappropriate for bi-factor models predicted by
#' ESEM factors (as at lavaan version 0.7-2).
#'
#' As a result, `esem.from.mods` should be used whenever bi-factor models are
#' included. Note, however, that the solution to interpretational confounding
#' means that standard errors and fit statistics will be biased, so inferring
#' precise p-values, determining an optimal model, or checking that a model
#' surpasses some cut-off of "good fit" should only be used with care.
#' Instead, the function is intended to allow variables to be regressed on EFA
#' factors with much better measurement than would be achieved with aggregate
#' scores.
#'
#' Bi-factor models do come with a further complication---they require
#' orthogonal relationships between the general and group factors.
#' When a factor is an outcome of a regression in a structural model, it is not
#' possible to include such a constraint because the instructions normally used
#' to do so will constrain residual variance instead.
#' However, given the 2-stage procedure is employed, there is little room for
#' measurement models to change to allow factor correlations to change.
#' As a result, the parameter can be relaxed, and implied correlations between
#' the group and general factors should remain close to zero.
#' Given that `esem.from.mods` already uses the 2-stage procedure, this method
#' is employed when bi-factor models are used.
#'
#' @seealso
#' [sem.check], [lavaan::sam]
#'
#' @references
#' Bainbridge, T. F., Ludeke, S. G., & Smillie, L. D. (2022).
#' Evaluating the Big Five as an organizing framework for commonly used
#' psychological trait scales.
#' Journal of Personality and Social Psychology, 122(4), 749-777.
#' https://doi.org/10.1037/pspp0000395.
#'
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.
#' https://doi.org/10.1177/004912417600500101.
#'
#' Nagy, G., Brunner, M., Lüdtke, O., and Greiff, S. (2017).
#' Extension Procedures for Confirmatory Factor Analysis.
#' Journal of Experimental Education, 85(4).
#' https://doi.org/10.1080/00220973.2016.1260524.
#'
#' Rosseel, Y. & Loh, W. W. (2022).
#' A structural after measurement approach to structural equation modeling.
#' Psychological Methods, 29(3), 561-588.
#' https://doi.org/10.1037/met0000503.
#'
#' @importFrom lavaan summary
#'
#' @examples
#' # Create CFA keys
#' keys0 <- c("hope_a", "hope_p")
#' keys <- sapply(
#'   keys0,
#'   function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))],
#'   simplify = FALSE
#' )
# keys_b <- list(grit = c("grit_c", "grit_p"), hope = c("hope_a", "hope_p"))
#' # Create EFA keys
#' # Using only 3 factors and fewer items to save time for a simple example
#' # (This results in a less than ideal solution but it doesn't matter for an
#' # example)
#' keys_e0 <- paste0("bfi_", c("e", "a", "c"))
#' keys_e <- sapply(
#'   keys_e0,
#'   function(x) {
#'     names(BFIGritHope)[grep(paste0(x, "[1-2]_[1-2]"), names(BFIGritHope))]
#'   },
#'   simplify = FALSE
#' )
#'
#' # Run 'esem.from.keys' models
#' esam_fit <- esem.from.keys(BFIGritHope, keys_e, keys, fit_save = FALSE)
#' # Examine results
#' summary(esam_fit$fit$hope_a)  # Standard lavaan summary
#' esam_fit$r2                   # R-squareds
#' esam_fit$b                    # Betas
#'
#' # Run 'esem.from.mods' models
#' # First, create fitted objects to use as inputs
#' cfa_fit <- cfa.from.keys(keys, BFIGritHope, fit_save = FALSE)
#' efa_fit <- efa.from.keys(keys_e, BFIGritHope, fit_save = FALSE)
#' # Run models
#' esem_fit <- esem.from.mods(
#'   efa_fit$fit, cfa_fit$fit, data = BFIGritHope, fit_save = FALSE
#' )
#' # Examine results
#' summary(esem_fit$fit$grit_c)  # Standard lavaan summary
#' esem_fit$r2                   # R-squareds
#' esem_fit$b                    # Betas
#'
#' @export

esem.from.keys <- function(
    data, keys_e, keys,
    fit_save = TRUE, fit_measures = "all", miss = "default", est = "default",
    name = "esam", check = FALSE, save_out = FALSE
) {
  if (!is.list(keys)) {
    stop("'keys' is not a list.")
  }
  # Keys must be named
  if (any(names(keys_e) == "")) {
    stop("At least one element of 'keys_e' has an empty name.")
  }
  if (any(names(keys) == "")) {
    stop("At least one element of 'keys' has an empty name.")
  }

  ####### Modified from efa.from.keys #######
  target <- sapply(keys_e, function(y) ifelse(!unlist(keys_e) %in% y, 0, NA))
  mod_efa <- paste(
    paste0(
      paste0('efa("', name, '")*', names(keys_e), collapse = " + "),
      " =~\n",
      paste(unlist(keys_e), collapse = " + ")
    )
  )
  ####### Modified from efa.from.keys #######

  mods_cfa <- mapply(
    function(x, xn) paste(xn, "=~", paste(x, collapse = " + ")),
    x = keys, xn = names(keys), SIMPLIFY = FALSE
  )
  regr_cfa <- sapply(
    names(keys),
    function(x) paste(x, "~", paste0(names(keys_e), collapse = " + ")),
    simplify = FALSE
  )
  mods <- mapply(
    function(x, y) paste0(x, "\n", mod_efa, "\n", y),
    x = mods_cfa, y = regr_cfa, SIMPLIFY = FALSE
  )
  mod_out <- sem.check(
    mods,
    data,
    name = name,
    keys_s = keys,
    keys_e = keys_e,
    std = TRUE,  # For r2 calcs.
    fit_save = fit_save,
    fit_measures = fit_measures,
    miss = miss,
    est = est,
    std.lv = TRUE,
    ordered = NULL,
    check = check,
    save_out = save_out,
    use_sam = TRUE,
    target = target
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
  if (fit_save) {
    return(
      list(
        fit = mod_out$fit,
        par_std = mod_out$par_std,
        fit_measures = mod_out$fit_measures,
        b = b,
        r2 = r2
      )
    )
  } else {
    return(
      list(
        fit = mod_out$fit,
        par_std = mod_out$par_std,
        b = b,
        r2 = r2
      )
    )
  }
}

#' @rdname esem.from.keys
#' @export

esem.from.mods <- function(
    data, efa_fit, cfa_fit = NULL, bif_fit = NULL,
    fit_save = FALSE, fit_measures = "all", miss = "default", est = "default",
    name = "esem", check = FALSE, save_out = FALSE
) {
  if (is.null(cfa_fit) & is.null(bif_fit)) {
    stop("At least one of 'cfa_fit' and 'bif_fit' must be specified.")
  }
  if (!is.null(cfa_fit)) {
    # Single model instead of list.
    if (!is.list(cfa_fit) & inherits(cfa_fit, "lavaan")) {
      cfa_fit <- list(factor = cfa_fit)
    }
    if (sum(sapply(cfa_fit, function(x) !inherits(x, "lavaan"))) > 0) {
      stop(
        paste0(
          "The below elements of 'cfa_fit' are not objects of type lavaan.",
          "\n    ",
          paste0(
            names(cfa_fit)[sapply(cfa_fit, function(x) !inherits(x, "lavaan"))],
            collapse = "\n    "
          )
        )
      )
    }
  }
  if (!is.null(bif_fit)) {
    # Single model instead of list.
    if (!is.list(bif_fit) & inherits(bif_fit, "lavaan")) {
      bif_fit <- list(bifactor = bif_fit)
    }
    if (sum(sapply(bif_fit, function(x) !inherits(x, "lavaan"))) > 0) {
      stop(
        paste0(
          "The below elements of 'bif_fit' are not objects of type lavaan.",
          "\n    ",
          paste0(
            names(bif_fit)[sapply(bif_fit, function(x) !inherits(x, "lavaan"))],
            collapse = "\n    "
          )
        )
      )
    }
  }
  if (is.null(efa_fit)) {
    stop(
      paste(
        "'efa_fit' is NULL.",
        "'efa_fit' must be a fitted lavaan object of an EFA model."
      )
    )
  }
  if (!inherits(efa_fit, "lavaan")) {
    stop("'efa_fit' is not an object of type lavaan.")
  }
  if (!is.null(cfa_fit)) {
    cfa_par <- sapply(cfa_fit, parameterEstimates, simplify = FALSE)
    cfa_keys <- lapply(cfa_par, function(x) x$rhs[x$op == "=~"])
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
              "Please either use 'bif_fit' and a model supported there,",
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
    names(cfa_fit) <- names(cfa_par) <- names(cfa_keys) <- cfa_names
    lapply(
      cfa_par,
      function(x) {
        if (sum(x$op == "|") > 0) {
          stop(
            paste(
              "At least one element of 'cfa_fit' is a model with ordinal",
              "variables, which are not currently supported in",
              "'esem.from.mods'. To use the function, re-run the input models",
              "with all variables treated as continuous."
            )
          )
        }
      }
    )
    if (sum(table(names(cfa_keys)) > 1) > 0) {
      stop(
        paste(
          "At least two different models in 'cfa_fit' have factors with the",
          "same name.",
          "Please ensure that all factor names are unique."
        )
      )
    }
  }
  if (!is.null(bif_fit)) {
    bif_par <- sapply(bif_fit, parameterEstimates, simplify = FALSE)
    bif_keys <- lapply(bif_par, function(x) unique(x$rhs[x$op == "=~"]))
    bif_names <- mapply(
      x = bif_par, y = bif_keys,
      FUN = function(x, y) {
        tmp <- table(x$lhs[x$op == "=~" & x$rhs %in% y])
        names(tmp)[tmp == max(tmp)]
      }
    )
    names(bif_par) <- bif_names
    lapply(
      bif_par,
      function(x) {
        if (sum(x$op == "|") > 0) {
          stop(
            paste(
              "At least one element of 'bif_fit' is a model with ordinal",
              "variables, which are not currently supported in",
              "'esem.from.mods'. To use the function, re-run the input models",
              "with all variables treated as continuous."
            )
          )
        }
      }
    )
    if (!is.null(names(bif_fit))) {
      if (sum(names(bif_fit) != bif_names) > 0) {
        warning(
          paste(
            "The names of 'bif_fit' do not match the general factor names.",
            "Names of returned objects are based on factor names",
            "so they will not match the names of 'bif_fit'."
          )
        )
      }
    }
    names(bif_keys) <- bif_names
    if (sum(table(names(bif_keys)) > 1) > 0) {
      stop(
        paste(
          "At least two different models in 'bif_fit' have general factors",
          "with the same name.",
          "Please ensure that all factor names are unique."
        )
      )
    }
  }
  if (!is.null(cfa_fit) & !is.null(bif_fit)) {
    if (sum(names(cfa_keys) %in% names(bif_keys)) > 0) {
      stop(
        paste(
          "The following models in 'cfa_fit' have identically named",
          "factor(s) in 'bif_fit':\n    ",
          paste(
            names(cfa_fit)[names(cfa_fit) %in% names(bif_fit)], collapse = "\n"
          ),
          "\n\n  Please ensure that CFA factors and bifactor general factors",
          "have unique names."
        )
      )
    }
  }
  efa_par <- parameterEstimates(efa_fit)
  if (sum(efa_par$op == "|") > 0) {
    stop(
      paste(
        "At least one element of 'efa_fit' is a model with ordinal",
        "variables, which are not currently supported in",
        "'esem.from.mods'. To use the function, re-run the input models",
        "with all variables treated as continuous."
      )
    )
  }
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
    ordered = NULL,
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
  if (fit_save) {
    return(
      list(
        fit = mod_out$fit,
        par_std = mod_out$par_std,
        fit_measures = mod_out$fit_measures,
        b = b,
        r2 = r2
      )
    )
  } else {
    return(
      list(
        fit = mod_out$fit,
        par_std = mod_out$par_std,
        b = b,
        r2 = r2
      )
    )
  }
}
