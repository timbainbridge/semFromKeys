#' Runs CFA models for multiple scales based on items in a keys list.
#'
#' `cfa.from.keys` runs a confirmatory factor analysis (CFA) model for each
#' element of a keys list. The keys list must be a named list of scales, where
#' each element is an item from the corresponding scale. The function is
#' designed to streamline running CFA models for all scales in a sample and to
#' input model outputs into downstream functions.
#'
#' @inheritParams sem.check
#' @param keys
#' A named list of keys.
#' Names should be scale names,
#' elements should a list of items included in each scale.
#' @param data
#' A dataframe or object coercible to a dataframe.
#' Data must include all observed variables in any of the keys.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to 'cfa'.
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models or outputs from calls with
#' the same name will be overwritten.
#' @param std.lv
#' Logical.
#' Sets the `std.lv` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' `TRUE` indicates that factor variances should be fixed to 1.
#' `FALSE` indicates that loadings of the first items of factors should be fixed
#' to 1.
#' Defaults to `TRUE`.
#'
#' @return
#' Returns a list of length 2 (if `fit_save = FALSE`) or
#' 3 (if `fit_save = TRUE`).
#' The elements of the list are: a list of lavaan model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_save = TRUE`, a matrix of fit measures for each model.
#'
#' @details
#' The model relies on [sem.check()] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' For further details on how this works, see the [sem.check()] function
#' documentation.
#'
#' The function does not provide any warnings for poor fit beyond those provided
#' by lavaan.
#' If any CFA models have poor fit, there is currently no capability to update
#' them beyond removing items by omitting them from the keys.
#' Any other changes to CFA models have to be made manually currently.
#' (Note that with `save_out = TRUE`,
#' model code is saved in `file.path(out_dir, name, paste0(name, _mod.rds)`,
#' which could help with manually updating models.)
#'
#' @seealso
#' [sem.check()], which `cfa.from.keys()` uses for all the back-end, and
#' [lavaan::sem()], which is used to estimate the models.
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
#' # Run models
#' cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = TRUE)
#' # Examine some results
#' summary(cfa_fit$fit$grit_c)                # Standard lavaan summary
#' cfa_fit$fit_measures[, c("cfi", "rmsea")]  # Fit measures

cfa.from.keys <- function(
    keys, data, fit_save = TRUE, fit_measures = "all",
    std.lv = TRUE, miss = "ML", est = "default",
    name = "cfa", check = FALSE, save_out = FALSE
) {
  mods <- mapply(
    function(y, z) {
      if (length(z) > 2) {
        paste(y, "=~", paste(sapply(z, "["), collapse = " + "))
      } else {
        if (length(z) == 2) {
          warning(paste(y, "is only length 2. Model results may be poor."))
          paste0(y, " =~ 1 * ", z[[1]], "\n", y, " =~ ", z[[2]])
        } else {
          stop(paste(y, "is only length 1. Model cannot be run."))
        }
      }
    },
    y = names(keys), z = keys, SIMPLIFY = FALSE
  )
  sem.check(
    mods,
    data,
    name = name,
    keys_s = keys,
    keys_e = NULL,
    std = FALSE,  # For use in 2-stage procedure, must use non-standardised.
    fit_save = fit_save,
    fit_measures = fit_measures,
    std.lv = std.lv,
    miss = miss,
    est = est,
    check = check,
    save_out = save_out
  )
}
