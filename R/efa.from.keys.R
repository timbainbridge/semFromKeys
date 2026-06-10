#' Runs an EFA model based on items in a keys list.
#'
#' efa.from.keys runs a exploratory factor analysis (EFA) in lavaan with a
#' rotation targeted based on a keys list. The keys list must be a named list of
#' scales, where the scales are thought to represent separate factors of an EFA
#' and each element is an item from the corresponding scale.
#'
#' @param keys
#' A named list of keys. Names should be factor names, elements should be
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
#' Sets the `orthogonal` param, as per lavaan. Defaults to `FALSE`.
#' @param std.lv Sets the `std.lv` param, as per lavaan. Defaults to `TRUE`.
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
#' Returns a list of lists.
#' The elements are a list of lavaan bifactor model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_measures` is not FALSE, a matrix of fit measures for each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) using Burt's (1976) 2-stage procedure to prevent
#' interpretational confounding in the context of ESEM. However, it can also be
#' used to easily run a targeted EFA with only a keys list to avoid having to
#' manually specify the target.
#'
#' @references
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.

efa.from.keys <- function(
    keys, d, name = "efa", out_dir = "output",
    orthogonal = FALSE, std.lv = TRUE, fit_save = TRUE, fit_measures = TRUE,
    miss = "ML", hash_dir = "hashes", check = TRUE, save_out = FALSE
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
    std = FALSE,
    fit_save = fit_save,
    fit_measures = fit_measures,
    orthogonal = orthogonal,
    std.lv = std.lv,
    target = target,
    hash_dir = hash_dir,
    miss = miss,
    check = check,
    save_out = save_out
  )
}
