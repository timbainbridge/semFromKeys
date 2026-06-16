#' Runs an EFA model based on items in a keys list.
#'
#' efa.from.keys runs a exploratory factor analysis (EFA) in lavaan with a
#' rotation targeted based on a keys list.
#'
#' @param keys
#' A named list of keys. Names must be factor names, elements must be
#' vectors of items that should be targeted to load on the factor.
#' @param d
#' The data. This must include all observed variables used in the models.
#' @param name
#' A name for the collection of models. Defaults to 'efa'.
#' The name should be unique for each time any function is called from the
#' package or outputs from other calls will be overwritten.
#' @param out_dir
#' The directory where all function outputs will be saved. Defaults to 'output'.
#' @param orthogonal
#' Sets the `orthogonal` param, as per lavaan (see [lavaan::lavOptions()]).
#' Defaults to `FALSE`.
#' @param std.lv Sets the `std.lv` param, as per lavaan. Defaults to `TRUE`.
#' @param fit_save `TRUE` to save model fit measures. `FALSE` otherwise.
#' @param fit_measures
#' A vector of fit measures to save or 'all' to select all fit measures,
#' as per the `fit.measures` parameter from lavaan's [lavaan::fitMeasures()]
#' function.
#' Defaults to 'all'. Irrelevant if `fit_save = FALSE`.
#' @param miss
#' Sets the `missing` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' Defaults to 'ML'.
#' @param est
#' Sets the `estimator` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' The default ('default') uses the lavaan default for the model being run.
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
#' Returns a list of lists.
#' The elements are a list of lavaan bifactor model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_save = TRUE`, a matrix of fit measures for each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) using Burt's (1976) 2-stage procedure to prevent
#' interpretational confounding in the context of ESEM. However, it can also be
#' used to easily run a targeted EFA with only a keys list to avoid having to
#' manually specify the target and model.
#'
#' The function was designed for use with established multidimensional scales,
#' such that a target is always reasonable.
#' The function does not currently support untargeted rotations.
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
#' @references
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.
#' http://journals.sagepub.com/doi/10.1177/004912417600500101.

efa.from.keys <- function(
    keys, d, name = "efa", out_dir = "output",
    orthogonal = FALSE, std.lv = TRUE, fit_save = TRUE, fit_measures = "all",
    miss = "ML", est = "default", check = TRUE, save_out = FALSE
) {
  target <- sapply(keys, function(y) ifelse(!unlist(keys) %in% y, 0, NA))
  mod <- list(
    paste(
      paste0(
        paste0('efa("', name, '")*', names(keys), collapse = " + "),
        " =~\n",
        paste(unlist(keys), collapse = " + ")
      )
    )
  )
  names(mod) <- name
  sem.check(
    mod,
    d,
    name = name,
    kl_s = NULL,
    kl_e = keys,
    std = FALSE,  # For use in 2-stage procedure, must use non-standardised.
    fit_save = fit_save,
    fit_measures = fit_measures,
    orthogonal = orthogonal,
    std.lv = std.lv,
    target = target,
    miss = miss,
    est = est,
    check = check,
    save_out = save_out
  )
}
