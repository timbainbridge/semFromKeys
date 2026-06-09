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
#' A name for the collection of models. Defaults to 'cfa'.
#' The name should be unique for each time any function is called from the
#' package or outputs from other calls will be overwritten.
#' @param out_dir
#' The directory where all function outputs will be saved. Defaults to 'output'.
#' @param std.lv Sets the `std.lv` param, as per lavaan. Defaults to `TRUE`.
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
#' Returns a list of lists.
#' The elements are a list of lavaan CFA model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_measures` is not FALSE, a matrix of fit measures for each model.

cfa.from.keys <- function(
    keys, d, name = "cfa", out_dir = "output", std.lv = TRUE,
    fit_save = TRUE, fit_measures = TRUE, miss = "ML", hash_dir = "hashes"
) {
  if (is.null(out_dir)) {
    out_dir <- name
  }
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
    std = FALSE,
    fit_save = fit_save,
    fit_measures = fit_measures,
    std.lv = std.lv,
    out_dir = out_dir,
    hash_dir = hash_dir,
    miss = miss
  )
}
