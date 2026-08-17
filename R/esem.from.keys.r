#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `esem.from.keys` runs exploratory structural equation models (ESEM) in lavaan
#' where the exploratory factor analysis (EFA) factors predict confirmatory
#' factor analysis (CFA) factors
# and/or bifactor factors
#' in separate models for each CFA
# or bifactor
#' model.
#' The function takes a keys list to describe the EFA factors and keys lists to
#' describe the CFA
# and/or bifactor
#' models.
#'
#' @inheritParams sem.check
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

#' @param data
#' A dataframe or object coercible to a dataframe.
#' Data must include all observed variables in keys.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "esam".
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.

# @param exclude_factors
# A vector of group factors to exclude from bifactor models.
# Defaults to `NULL`. Irrelevant if `keys_b = NULL`.
# See details and the examples for more information.

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
#' in separate models using Rosseel and Loh's (2022) SAM method to prevent
#' interpretational confounding (see below). The function is designed to run
#' analyses analogous to that of Bainbridge, Ludeke, and Smillie (2022) only
#' using the SAM method instead of Burt's 2-stage method.
#'
#' The function is designed to run for multiple models with a similar design.
#' If you are using the function for a single model,
#' transform inputs into lists as appropriate.
#'
#' The model relies on [sem.check()] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' For further details on how this works,
#' see the [sem.check()] function  documentation.
#'
#' In SEM, standard methods do not distinguish between measurement and
#' structural parameters. As a result, measurement model parameters can change
#' with the addition of theoretically unrelated constructs in a structural
#' model, and can change differently for different sets of unrelated constructs.
#' This means that the unrelated constructs are changing the interpretation of
#' the latent variable, which Burt (1976) referred to as
#' "interpretational confounding".
#'
#' There are various ways to deal with interpretational confounding.
#' The standard solution (other than ignoring it) is to create good fitting
#' measurement models first, then freely estimate the structural model
#' with checks to ensure adequate fit of the model and that measurement
#' parameters do not substantially change with different combinations of
#' factors. This is sometimes a good solution, but, in other cases, it is not.
#' For example, if the measurement model was for a well-established scale and it
#' requires changing, then it loses easy comparison with past research.
#' This issue is most clearly relevant when changes to a measurement model
#' require entirely different factors, or items to be removed but it is still an
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
#' However, it is not a perfect solution because, by fixing measurement
#' parameters, uncertainty in their estimation is neglected
#' (e.g., Nagy et al., 2017), which results in biased standard errors and fit
#' statistics.
#'
#' A third option was proposed by Nagy and colleagues (2017),
#' who introduced an extension procedure such that item residuals are allowed to
#' correlate with external variables (or factors).
#' To make the model identifiable, these relationships are constrained using
#' one of a number of methods. If the sums of squares of correlations between
#' all combinations of factors' items and external factors are minimised,
#' measurement parameters in isolated measurement models are preserved in the
#' structural model without having to constrain them directly.
#' As a result, unbiased standard errors are preserved while simultaneously
#' eliminating interpretational confounding since the measurement parameters
#' from the measurement models are preserved regardless of external factors.
#' Unfortunately, estimating these models becomes increasingly slow with more
#' items and factors, such that it quickly becomes untenable.
#' Moreover, the method only works with correlations, not regressions,
#' so some method to run regressions using the correlations needs to be
#' implemented that does not itself result in biased estimates due to ignored
#' uncertainty in the correlation estimates.
#'
#' Although this latter issue may be solvable for Nagy and colleagues' (2017)
#' method, a more practical solution to these issues was proposed by
#' Rosseel and Loh (2022) with their SAM approach. This method essentially
#' follows Burt's (1976) method but adjust the procedure to overcome its issues.
#' They distinguish two SAM varieties--"local SAM" and "global SAM".
#' Local SAM uses the observed summary statistics of the parameters of the
#' measurement models to generate mean and covariance matrices to use in the
#' structural model, which preserves the structure of the measurement models
#' while also preserving the uncertainty.
#' Global SAM treats the measurement parameters as given, but corrects the
#' standard errors of the structural model. Although local SAM is preferable in
#' most circumstances, it currently (as at version 0.7-2) sets ESEM factor
#' covariances as equal, which is likely a bug.
#'
#' Given its practicality, `sem.path` and the absence of the aforementioned bug,
#' `esem.from.keys` uses the global SAM method.
#'
#' Note that bifactor models are not currently supported in `esem.from.keys`.
#' There is currently (as at version 0.7-2) no way to force lavaan to freely
#' estimate covariances with factors that are not involved in a structural path.
#' Instead the [lavaan::sam()] function treats them as 0, which alters the
#' model from what it ought to be. This may also be a bug in the `sam()`
#' function.
#'
#' In the meantime, I suggest using [esem.from.mods()] for bifactor models.
#' Standard errors and fit statistics are not quite right with that method but
#' they are likely close enough for most contexts and there are not any better
#' options that are easily implemented.
#'
#' @seealso
#' [sem.check()] [lavaan::sam()]
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
#' @importFrom lavaan summary
#' @export
#'
#' @examples
#' # Create CFA / bifactor keys
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
#'     names(BFIGritHope)[grep(paste0(x, "\\d_[1-2]"), names(BFIGritHope))]
#'   },
#'   simplify = FALSE
#' )
#' # Run models
#' esem_fit <- esem.from.keys(
#'   BFIGritHope, keys_e, keys, fit_save = FALSE
#' )
#' # Examine results
#' summary(esem_fit$fit$grit_c)  # Standard lavaan summary
#' esem_fit$r2                   # R-squareds
#' esem_fit$b                    # Betas

esem.from.keys <- function(
    data, keys_e, keys,
    # keys_b = NULL, exclude_factors = NULL,
    fit_save = TRUE, fit_measures = "all", miss = "ML", est = "default",
    name = "esam", check = FALSE, save_out = FALSE
) {
  ####### Copied from bifactor.from.keys #######
  # if (!is.list(keys_b)) {
  #   stop("'keys_b' is not a list.")
  # }
  if (!is.list(keys)) {
    stop("'keys' is not a list.")
  }
  # if (!is.null(keys_b)) {
  #   for (x in keys_b) {
  #     if (sum(!x %in% names(keys)) > 0) {
  #       grps <- x[!x %in% names(keys)]
  #       stop(
  #         paste0(
  #           "The following group factor(s) in 'keys_b' are not in 'keys':",
  #           "\n    ",
  #           paste(grps, collapse = "\n    "),
  #           "\n\nIf these are items, not group factors,",
  #           "check that keys_b only contains group factor names."
  #         )
  #       )
  #     }
  #   }
  # }
  ####### Copied from bifactor.from.keys #######

  # Keys must be named
  if (any(names(keys_e) == "")) {
    stop("At least one element of 'keys_e' has an empty name.")
  }
  if (any(names(keys) == "")) {
    stop("At least one element of 'keys' has an empty name.")
  }
  # if (!is.null(keys_b)) {
  #   if (any(names(keys) == "")) {
  #     stop("At least one element of 'keys_b' has an empty name.")
  #   }
  #   if (any(!unlist(keys_b) %in% names(keys))) {
  #     stop("Elements of 'keys_b' elements must match names of 'keys'.")
  #   }
  # }

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
  # mods_bif <- mapply(
  #   function(x, xn) {
  #     group_facs <- unlist(x)[!unlist(x) %in% exclude_factors]
  #     paste0(
  #       c(
  #         paste(xn, "=~", paste0(unlist(keys[unlist(x)]), collapse = " + ")),
  #         mapply(
  #           function(i, ni) paste(ni, "=~", paste0(i, collapse = " + ")),
  #           i = keys[group_facs], ni = names(keys[group_facs]), SIMPLIFY = FALSE
  #         ),
  #         paste(xn, "~~ 0 *", paste0(group_facs, collapse = " + 0 * ")),
  #         if (length(group_facs) > 1) {
  #           sapply(
  #             seq_along(group_facs[length(group_facs)]),
  #             function(i) {
  #               paste(
  #                 group_facs[i], "~~ 0 *",
  #                 paste0(group_facs[(i:length(group_facs))], collapse = " + ")
  #               )
  #             }
  #           )
  #         }
  #       ),
  #       collapse = "\n"
  #     )
  #   },
  #   x = keys_b, xn = names(keys_b), SIMPLIFY = FALSE
  # )
  # keys_bif <- lapply(keys_b, function(x) unlist(keys[x]))
  regr_cfa <- sapply(
    names(keys),
    function(x) paste(x, "~", paste0(names(keys_e), collapse = " + ")),
    simplify = FALSE
  )
  # regr_bif <- mapply(
  #   function(x, xn) {
  #     paste0(
  #       paste(xn, "~", paste0(names(keys_e), collapse = " + ")),
  #       "\n",
  #       paste(
  #         sapply(
  #           x[!x %in% exclude_factors],
  #           function(y) paste(y, "~", paste0(names(keys_e), collapse = " + "))
  #         ),
  #         collapse = "\n"
  #       )
  #     )
  #   },
  #   x = keys_b, xn = names(keys_b), SIMPLIFY = FALSE
  # )
  mods <-
    # c(
    mapply(
      function(x, y) paste0(x, "\n", mod_efa, "\n", y),
      x = mods_cfa, y = regr_cfa, SIMPLIFY = FALSE
    )
  # , mapply(
  #     function(x, y) paste0(x, "\n", mod_efa, "\n", y),
  #     x = mods_bif, y = regr_bif, SIMPLIFY = FALSE
  #   )
  # )
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
