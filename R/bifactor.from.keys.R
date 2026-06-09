#' Runs bifactor models for multiple scales based on items in a keys list.
#'
#' cfa.from.keys runs a confirmatory factor analysis (CFA) model for each
#' element of a keys list. The keys list must be a named list of scales, where
#' each element is an item from the corresponding scale. The function is
#' designed to steamline running CFA models for all scales in a sample and to
#' input model outputs into downstream functions.
#'
#' @param keys_g
#' A named list of general factors. Names must be the names of the general
#' factors, elements are vectors of items that will load on the general factor.
#' @param keys_b
#' A named list of group factors. Names must be the general factors.
#' Elements must be the group factor names.
#' @param keys
#' A named list of items of group factors. Names must be group factor names.
#' Elements must be items that load on the group factors.
#' Elements must include group factors across all bifactor models.
#' @param d
#' The data. This must include all observed variables used in any of the models.
#' @param name A name for the collection of models. Defaults to "bifactor".
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
#' @param mod_dir
#' A subdirectory of `out_dir` where model outputs are saved.
#' If NULL, it will be set as 'name'.
#'
#' @return
#' Returns a list of lists.
#' The elements are a list of lavaan bifactor model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_measures` is not FALSE, a matrix of fit measures for each model.

bifactor.from.keys <- function(
  keys_g, keys_b, keys, d,
  name = "bifactor", out_dir = "output",
  std.lv = TRUE, fit_save = TRUE, fit_measures = TRUE, miss = "ML",
  hash_dir = "hashes", mod_dir = NULL
) {
  if (is.null(out_dir)) {
    out_dir <- name
  }
  if (fit_measures != FALSE) {
    fit_save <- TRUE
  } else {
    fit_save <- FALSE
  }
  mods <- mapply(
    function(x, y, z) {
      tmp <- paste(z, "=~", paste(x, collapse = " + "))
      if (length(y) != 0) {
        tmp1 <- paste0(
          c(
            tmp,
            mapply(
              function(i, ni) paste(ni, "=~", paste0(i, collapse = " + ")),
              i = keys[y], ni = names(keys[y]), SIMPLIFY = FALSE
            )
          ),
          collapse = "\n"
        )
      } else {
        tmp1 <- tmp
      }
      tmp1
    },
    x = keys_g, y = keys_b, z = names(keys_g), SIMPLIFY = FALSE
  )
  sem.check(
    mods,
    d,
    name = name,
    kl_s = keys_g,
    kl_e = NULL,
    mod_dir = mod_dir,
    std = FALSE,
    fit_save = fit_save,
    fit_measures = TRUE,
    std.lv = std.lv,
    out_dir = out_dir,
    hash_dir = hash_dir,
    miss = miss,
    orthogonal = TRUE  # Must be TRUE for bifactor models.
  )
}
