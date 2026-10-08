#' Runs CFA models for multiple scales based on items in a keys list.
#'
#' `cfa.from.keys` runs a confirmatory factor analysis (CFA) model for each
#' element of a keys list. The keys list should be a named list of scales, where
#' each element contains a vector of items from the corresponding scale.
#' The function is designed to streamline running CFA models for all scales in a
#' sample and to input model outputs into downstream functions.
#'
#' @inheritParams sem.check
#' @param keys
#' A named list of keys.
#' Names should be scale names, elements should a vector of items included in
#' each scale.
#' @param data
#' A dataframe or object coercible to a dataframe.
#' Data must include all observed variables in any of the keys.
#' @param extra
#' A vector of strings of extra lavaan code to be added.
#' Code can be in any order and will be added to all models containing all
#' referenced items.
#' The argument is for allowing correlations between item residuals.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to 'cfa'.
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#' @param std.lv
#' Logical.
#' Sets the `std.lv` parameter, as per lavaan (see [lavaan::lavOptions]).
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
#' If `keys` is specified as a vector of items, the function will assume those
#' items are meant to comprise a single scale, and will convert the input to a
#' length 1 list with the element named 'factor'. A warning will be sent when
#' this occurs.
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
#' [sem.check], [lavaan::sem]
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
    keys, data, extra = NULL, fit_save = TRUE, fit_measures = "all",
    std.lv = TRUE, miss = "default", est = "default", ordered = NULL,
    name = "cfa", check = FALSE, save_out = FALSE
) {
  if (sum(sapply(keys, function(x) length(x) != 1)) == 0) {
    if (sum(sapply(keys, function(x) !(x %in% names(data)))) == 0) {
      warning(
        paste0(
          "'keys' appears to be a vector of items rather than a keys list and ",
          "has been converted into a length 1 keys list with the name 'factor'."
        )
      )
      keys <- list(factor = keys)
    }
  }

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
    sapply(
      extra_vars,
      function(x) {
        if (sum(sapply(keys, function(y) sum(!x %in% y) == 0)) == 0) {
          warning(
            paste0(
              "The extra code containing '", paste(x, collapse = "' and '"),
              "' includes items that are not both/all in any single model. ",
              "Therefore, the code has been included in any model. "
            )
          )
        }
      }
    )
    mod_extra <- mapply(
      k = keys, kn = names(keys),
      FUN = function(k, kn) {
        tmp <- mapply(
          xv = extra_vars, x = extra,
          FUN = function(xv, x) if (sum(!(xv %in% c(k, kn))) == 0) x else ""
        )
        tmp[tmp != ""]
      }
    )
    ####### Modified from esem.from.keys #######

    mods <- mapply(
      function(x, y, z) {
        if (length(z) > 3) {
          # Check no. df
          if (length(x) > choose(length(z) + 1, 2) - length(z) * 2 - 1) {
            stop(
              paste0(
                "There are not enough degrees of freedom in the model for '", y,
                "'. Remove at least one element of extra that includes only ",
                "items in '", y, "'."
              )
            )
          }
          paste(
            y, "=~", paste(z, collapse = " + "), "\n", paste(x, collapse = "\n")
          )
        } else {
          if (length(z) == 3) {
            # Not enough df for extra
            if (length(x) > 0) {
              warning(
                paste0(
                  "With only 3 items in '", y, "', there are not enough ",
                  "degrees of freedom to include anything in extra for the ",
                  "model. Therefore, the extra code that applied to the model ",
                  "has been omitted."
                )
              )
            }
            paste(y, "=~", paste(z, collapse = " + "))
          } else {
            if (length(z) == 2) {
              warning(paste(y, "is only length 2. Model results may be poor."))
              paste0(y, " =~ 1 * ", z[[1]], "\n", y, " =~ ", z[[2]])
            } else {
              stop(paste(y, "is only length 1. Model cannot be run."))
            }
          }
        }
      },
      x = mod_extra, y = names(keys), z = keys, SIMPLIFY = FALSE
    )
  } else {
    mods <- mapply(
      function(y, z) {
        if (length(z) > 2) {
          paste(y, "=~", paste(z, collapse = " + "))
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
  }
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
    ordered = ordered,
    check = check,
    save_out = save_out
  )
}
