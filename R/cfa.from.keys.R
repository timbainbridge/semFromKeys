#' Runs CFA models for multiple scales based on items in a keys list.
#'
#' cfa.from.keys runs a confirmatory factor analysis (CFA) model for each
#' element of a keys list. The keys list must be a named list of scales, where
#' each element is an item from the corresponding scale. The function is
#' designed to streamline running CFA models for all scales in a sample and to
#' input model outputs into downstream functions.
#'
#' @param keys
#' A named list of keys. Names should be scale names, elements should a list of
#' items included in each scale.
#' @param d
#' The data. This must include all observed variables used in any of the models.
#' @param name
#' A subdirectory where model outputs will be saved when `save_out = TRUE`.
#' Defaults to 'cfa'. Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models or outputs from other
#' calls will be overwritten.
#' @param out_dir
#' The directory where all function outputs will be saved. Defaults to 'output'.
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
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
#' The elements are a list of lavaan CFA model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_measures` is not FALSE, a matrix of fit measures for each model.
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
#' summary(cfa_fit$fit$grit_c)  # Standard lavaan summary
#' cfa_fit$par$grit_c           # Parameter estimates
#' cfa_fit$fit_measures         # Fit measures

cfa.from.keys <- function(
    keys, d, name = "cfa", out_dir = "output", std.lv = TRUE,
    fit_save = TRUE, fit_measures = "all", miss = "ML", est = "default",
    check = TRUE, save_out = FALSE
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
    d,
    name = name,
    kl_s = keys,
    kl_e = NULL,
    std = FALSE,  # For use in 2-stage procedure, must use non-standardised.
    fit_save = fit_save,
    fit_measures = fit_measures,
    std.lv = std.lv,
    out_dir = out_dir,
    miss = miss,
    est = est,
    check = check,
    save_out = save_out
  )
}
