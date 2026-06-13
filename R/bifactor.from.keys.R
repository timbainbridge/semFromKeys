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
#' @param name
#' A name for the collection of models. Defaults to 'bifactor'.
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
#' Please be careful with bifactor models.
#' In simulation studies, they can fit better than the models that generated the
#' data in the presence of unspecified complexity (which is frequently, if not
#' usually, the case; Murray & Johnson, 2013).
#' They can also produce negative residual variances.
#' lavaan will warn you of this and you should deal with it before proceeding.
#' Items can also load in atheoretical ways on group factors.
#' This could be some items loading negatively instead of positively or
#' vice versa or one or more items having insignificant loadings on a factor
#' that they should theoretically load on.
#'
#' These problems can often be solved by omitting a factor.
#' When items from one group factor dominate the general factor,
#' the best solution might be to remove the general factor and treat the group
#' factors as separate constructs.
#' Perhaps more frequently, it might be solved by omitting one (or more) group
#' factors (i.e., an S-1 model; Eid et al., 2017).
#' This can either be done a priori
#' (perhaps based on which one should theoretically be most 'central' to the
#' general factor)
#' or after examining the fully specified bifactor model and dropping the worst
#' performing factor.
#' The worst performing factor will be one that has items loading in the 'wrong'
#' direction or insignificantly or 1-2 items that load much more strongly than
#' other items.
#' When more than one group factor is poor, I am not aware of any specific
#' advice on which to remove, so use your judgment if previous research has not
#' got any advice for your case.
#'
#' @references
#' Eid, M., Geiser, C., Koch, T., & Heene, M. (2017).
#' Anomalous results in G-factor models: Explanations and alternatives.
#' Psychological Methods, 22(3), 541-562.
#' https://doi.apa.org/doi/10.1037/met0000083.
#'
#' Murray, A. L. & Johnson, W. (2013).
#' The limitations of model fit in comparing the bi-factor versus higher-order
#' models of human cognitive ability structure. Intelligence, 41(5), 407-422.
#' http://dx.doi.org/10.1016/j.intell.2013.06.004.

bifactor.from.keys <- function(
  keys_g, keys_b, keys, d, name = "bifactor", out_dir = "output",
  std.lv = TRUE, fit_save = TRUE, fit_measures = NULL, miss = "ML",
  hash_dir = "hashes", check = TRUE, save_out = FALSE
) {
  if (length(keys_g) != length(keys_b)) {
    stop(
      paste(
        "`keys_g` is not the same length as `keys_b`.",
        "Check that you have not mixed up `keys` with `keys_g` or `keys_b`."
      )
    )
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
    std = FALSE,
    fit_save = fit_save,
    fit_measures = fit_measures,
    std.lv = std.lv,
    out_dir = out_dir,
    hash_dir = hash_dir,
    miss = miss,
    orthogonal = TRUE,  # Must be TRUE for bifactor models.
    check = check,
    save_out = save_out
  )
}
