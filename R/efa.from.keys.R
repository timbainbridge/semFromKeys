#' Runs an EFA model based on items in a keys list.
#'
#' `efa.from.keys` runs a exploratory factor analysis (EFA) in lavaan with a
#' rotation targeted based on a keys list.
#'
#' @inheritParams sem.check
#' @inheritParams cfa.from.keys data std.lv extra
#' @param keys
#' A named list of keys. Names must be factor names, elements must be
#' vectors of items that should be targeted to load on the factor.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to 'efa'.
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#'
#' @return
#' Returns a list of lists.
#' The elements are a list of lavaan bi-factor model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_save = TRUE`, a matrix of fit measures for each model.
#'
#' @details
#' The function was designed to streamline running exploratory structural
#' equation models (ESEM) using [esem.from.mods]. However, it can also be
#' used to easily run a targeted EFA with only a keys list to avoid having to
#' manually specify the target and model.
#'
#' The function was designed for use with established multidimensional scales,
#' such that a target is always reasonable.
#' The function does not currently support untargeted rotations.
#'
#' The model relies on [sem.check] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' For further details on how this works, see the [sem.check] function
#' documentation.
#'
#' @seealso
#' [sem.check], [lavaan::sem], [esem.from.mods]
#'
#' @importFrom lavaan summary
#' @export
#'
#' @examples
#' # Create EFA keys
#' # Using only 3 factors to save time
#' keys_e0 <- paste0("bfi_", c("e", "a", "c"))
#' # Using less than all items to save time
#' # (This results in a less than ideal solution but it shouldn't matter for an
#' # example)
#' keys_e <- sapply(
#'   keys_e0,
#'   function(x) {
#'     names(BFIGritHope)[grep(paste0(x, "\\d_[1-2]"), names(BFIGritHope))]
#'   },
#'   simplify = FALSE
#' )
#' # Run model with selected fit measures.
#' efa_fit <- efa.from.keys(
#'   keys_e, BFIGritHope, check = FALSE,
#'   fit_save = TRUE, fit_measures = c("chisq", "df", "pvalue")
#' )
#' # Examine results
#' summary(efa_fit$fit)    # Standard lavaan summary
#' efa_fit$fit_measures    # Selected fit measures

efa.from.keys <- function(
    keys, data, extra = NULL,
    orthogonal = FALSE, fit_save = TRUE, fit_measures = "all",
    std.lv = TRUE, miss = "default", est = "default", ordered = NULL,
    name = "efa", check = FALSE, save_out = FALSE
) {
  target <- sapply(keys, function(y) ifelse(!unlist(keys) %in% y, 0, NA))

  ####### Modified from esem.from.keys #######
  if (!is.null(extra)) {
    # Removal all fixed values and parameter names; remove punctuation
    extra_vars <- lapply(
      extra,
      function(y) {
        tmp <- stringr::str_split(
          gsub("((\\+|~~|~).*?(\\*))", " ", y), "\\+|~|=|\n| ", simplify = TRUE
        )
        tmp[tmp != ""]
      }
    )
    extra_vars1 <- unique(unlist(extra_vars))
    extra_vars2 <- extra_vars1[!extra_vars1 %in% unlist(keys)]
    if (length(extra_vars2) > 0) {
      if (length(extra_vars2) > 1) {
        stop(
          paste0(
            "The following items were found in 'extra' but do not match ",
            "either a variable name in 'keys' or standard lavaan code.",
            "\n\n    ", paste(extra_vars2, collapse = ", ")
          )
        )
      } else {
        stop(
          paste0(
            "'", extra_vars2, "' was found in 'extra' but does not match ",
            "either a variable name in 'keys' or standard lavaan code."
          )
        )
      }
    }
    mod_extra <- mapply(
      xv = extra_vars, x = extra,
      FUN = function(xv, x) if (sum(!xv %in% unlist(keys)) == 0) x else ""
    )
    ####### Modified from esem.from.keys #######

    mod <- list(
      paste(
        paste0(
          paste0('efa("', name, '")*', names(keys), collapse = " + "),
          " =~\n", paste(unlist(keys), collapse = " + "),
          "\n", paste0(mod_extra, collapse = "\n")
        )
      )
    )
  } else {
    mod <- list(
      paste(
        paste0(
          paste0('efa("', name, '")*', names(keys), collapse = " + "),
          " =~\n", paste(unlist(keys), collapse = " + ")
        )
      )
    )
  }
  names(mod) <- name
  fit <- sem.check(
    mod,
    data,
    name = name,
    keys_s = NULL,
    keys_e = keys,
    std = FALSE,  # For use in 2-stage procedure, must use non-standardised.
    fit_save = fit_save,
    fit_measures = fit_measures,
    orthogonal = orthogonal,
    std.lv = std.lv,
    target = target,
    miss = miss,
    est = est,
    ordered = ordered,
    check = check,
    save_out = save_out
  )
  fit$fit <- fit$fit$efa
  fit$par <- fit$par$efa
  return(fit)
}
