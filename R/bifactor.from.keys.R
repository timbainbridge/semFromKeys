#' Runs bifactor models for multiple scales based on keys lists.
#'
#' `bifactor.from.keys` runs a series of bifactor model from three keys lists---
#' one for items on general factor; one for items on group factors;
#' and one for group factors on general factors.
#' The keys list must be named appropriately
#' (i.e., general factor names, group factor names, and general factor names
#' for the three lists respectively).
#' The function is designed to streamline running CFA models for all scales in a
#' sample and to input model outputs into downstream functions.
#'
#' @inheritParams sem.check
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
#' @param data
#' The data. This must include all observed variables in any of the keys.
#' @param name
#' A subdirectory where model outputs will be saved when `save_out = TRUE`.
#' Defaults to 'bifactor'.
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models or outputs from other
#' calls will be overwritten.
#' @param std.lv
#' Sets the `std.lv` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' Defaults to `TRUE`.
#'
#' @return
#' Returns a list of lists.
#' The elements are a list of lavaan bifactor model output objects;
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
#' Please be careful with bifactor models.
#' In simulation studies, they can fit better than the models that generated the
#' data in the presence of unspecified complexity
#' (which is usually the case; Murray & Johnson, 2013).
#' They can also produce negative residual variances.
#' lavaan will warn you of this and you should deal with it before proceeding.
#' Items can also load in atheoretical ways on the general or group factors.
#' This could be some items loading negatively instead of positively or
#' vice versa or one or more items having insignificant loadings on a factor
#' that they should theoretically load on.
#'
#' These problems can often be solved by omitting a factor.
#' When items from one group factor dominate the general factor,
#' the best solution might be to remove the general factor and treat the group
#' factors as separate constructs.
#' Perhaps more frequently, these problems might be solved by omitting one
#' (or more) group factors (i.e., an S-1 model; Eid et al., 2017).
#' This can either be done a priori
#' (perhaps based on which one should theoretically be most 'central' to the
#' general factor)
#' or after examining the fully specified bifactor model and dropping the worst
#' performing factor.
#' When more than one group factor is poor, I am not aware of any specific
#' advice on which to remove, so use your judgment if previous research has not
#' got any advice for your case.
#'
#' @references
#' Eid, M., Geiser, C., Koch, T., & Heene, M. (2017).
#' Anomalous results in G-factor models: Explanations and alternatives.
#' Psychological Methods, 22(3), 541-562.
#' doi.org/10.1037/met0000083.
#'
#' Murray, A. L. & Johnson, W. (2013).
#' The limitations of model fit in comparing the bi-factor versus higher-order
#' models of human cognitive ability structure. Intelligence, 41(5), 407-422.
#' doi.org/10.1016/j.intell.2013.06.004.
#'
#' @seealso
#' [sem.check()], which `bifactor.from.keys()` uses for all the back-end, and
#' [lavaan::sem()], which is used to estimate the models.
#'
#' @export
#'
#' @examples
#' # Create keys
#' keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#' keys <- sapply(
#'   keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' keys_g0 <- c("grit", "hope")
#' keys_g <- sapply(
#'   keys_g0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' keys_b1 <- sapply(
#'   keys_g0, function(x) keys0[grep(x, keys0)], simplify = FALSE
#' )
#' keys_b <- keys_b1
#' bif_fit0 <- bifactor.from.keys(
#'   keys_g, keys_b, keys, BFIGritHope, check = FALSE, fit_save = TRUE
#' )
#' # Fix negative residual variance
#' keys_b$hope <- keys_b1$hope[-1]
#' bif_fit <- bifactor.from.keys(
#'   keys_g, keys_b, keys, BFIGritHope, check = FALSE, fit_save = TRUE
#' )
#' # Examine some results
#' summary(bif_fit$fit$grit)  # Standard lavaan summary
#' bif_fit$par$grit           # Parameter estimates
#' bif_fit$fit_measures       # Fit measures

bifactor.from.keys <- function(
  keys_g, keys_b, keys, data, name = "bifactor", out_dir = "output",
  std.lv = TRUE, fit_save = TRUE, fit_measures = "all", miss = "ML",
  est = "default", check = FALSE, save_out = FALSE
) {
  if (!is.list(keys_g)) {
    stop("`keys_g` is not a list.")
  }
  if (!is.list(keys_b)) {
    stop("`keys_b` is not a list.")
  }
  if (!is.list(keys)) {
    stop("`keys` is not a list.")
  }
  if (length(keys_g) != length(keys_b)) {
    stop(
      paste(
        "`keys_g` is not the same length as `keys_b`.",
        "Check that you have not mixed up `keys` with `keys_g` or `keys_b`,",
        "or otherwise misspecified one of these keys lists."
      )
    )
  }
  if (sum(names(keys_g) != names(keys_b)) > 0) {
    stop(
      paste(
        "Names of `keys_g` do not match names of `keys_b`.",
        "Check keys are correctly specified and that they have not been mixed",
        "up with `keys`."
      )
    )
  }
  sapply(
    keys_b,
    function(x) {
      if (sum(!x %in% names(keys)) > 0) {
        grps <- x[!x %in% names(keys)]
        stop(
          paste0(
            "The following group factor(s) in `keys_b` are not in `keys`:",
            "\n    ",
            paste(grps, collapse = "\n    "),
            "\n\n(If these are items, not group factors, ",
            "and you are using bifactor.from.keys, ",
            "check that keys_b only contains group factor names.)"
          )
        )
      }
    }
  )
  items <- with_options(
    list(warn = 1),
    mapply(
      function(x, xn, y) {
        sapply(
          y,
          function(z) {
            if (sum(!keys[[z]] %in% x) == length(keys[[z]])) {
              stop(
                paste0(
                  "All the items in the `",
                  z,
                  "` group factor are not in the `",
                  xn,
                  "` general factor."
                )
              )
            }
          }
        )
        if (sum(!unlist(keys[y]) %in% x) > 0) {
          warning(
            paste(
              "The following item(s) are in a group factor but not in the",
              "general factor:\n   ",
              paste(unlist(keys[y])[!unlist(keys[y]) %in% x], collapse = "\n    ")
            )
          )
          c(x, unlist(keys[y])[!unlist(keys[y]) %in% x])
        } else {
          x
        }
      },
      x = keys_g, xn = names(keys_g), y = keys_b
    )
  )
  mods <- mapply(
    function(x, y, z) {
      tmp <- paste(z, "=~", paste(x, collapse = " + "))
      paste0(
        c(
          tmp,
          mapply(
            function(i, ni) paste(ni, "=~", paste0(i, collapse = " + ")),
            i = keys[y], ni = names(keys[y]), SIMPLIFY = FALSE
          )
        ),
        collapse = "\n"
      )
    },
    x = keys_g, y = keys_b, z = names(keys_g), SIMPLIFY = FALSE
  )
  sem.check(
    mods,
    data,
    name = name,
    keys_s = items,
    keys_e = NULL,
    std = FALSE,  # For use in 2-stage procedure, must use non-standardised.
    fit_save = fit_save,
    fit_measures = fit_measures,
    std.lv = std.lv,
    out_dir = out_dir,
    miss = miss,
    est = est,
    orthogonal = TRUE,  # Must be TRUE for bifactor models.
    check = check,
    save_out = save_out
  )
}
