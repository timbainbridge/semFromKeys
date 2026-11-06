#' Runs incremental validity analysis using latent variables and fitted
#' measurement models
#'
#' @description
#' Incremental validity using standard linear regressions is invalid with less
#' than perfect reliability. Latent variable models must be used instead.
#' However, this is difficult, especially if confidence intervals are required,
#' which can only be created with bootstrap sampling.
#'
#' `r2.change` streamlines the process of running incremental validity
#' analyses with latent variables, including generating bootstrap confidence
#' intervals.
#'
#' @inheritParams sem.check
# #' @param cfa_fit A named list of fitted lavaan objects of CFA models.
# #' @param bif_fit A named list of fitted lavaan objects of bi-factor models.
#' @param X
#' A length 1 character vector of the name of the the variable being tested.
#' @param Y A string of the name of the outcome variable.
#' @param Z
#' A character vector of the set of variables that X is being pitted against or
#' `NULL` to use all variables in `cfa_fit` and `bif_fit`. Defaults to `NULL`.
#' @param boot
#' Numeric or `NA`.
#' If `NA`, then bootstrap confidence intervals are not calculated.
#' If numeric, then bootstraps equal to the number are performed.
#' @param keys
#' A named list of items in uni-dimensional factors.
#' These can be names of single-factor latent variables or group factor latent
#' variables in a bifactor model.
#' Each list element must be a vector of items that load on the latent variable.
#' @param keys_g
#' A named list of items in general factors.
#' Names must be the names of the general factors.
#' Each list element must be a vector of items that load on the general factors.
#' `keys_g` must be the same length as `keys_b`.
#' Leave `keys_g = NULL` when no bifactor models are required.
#' @param keys_b
#' A named list of group factors in general factors.
#' Names must be the names of the general factors.
#' Each list element must be a vector of group factors.
#' `keys_b` must be the same length as `keys_g`.
#' Leave `keys_b = NULL` when no bifactor models are required.
#'
#' @return
#' A list of length X.
#' Elements include the fitted lavaan objects for the full and reduced models;
#' estimates of R-squared and R-squared change; and data frames of covariate
#' effect sizes from both the full and reduced models.
#'
#' @details
#' If using bootstrap confidence intervals, it is highly recommended that you
#' use `save_out = TRUE` and `check = TRUE`. This will require setting up a
#' cache directory with [cache.setup()] before running `r2.change()`
#'
#' @seealso
#' [cache.setup()] [sem.check()] [cfa.from.keys()] [bifactor.from.keys()]

# TODO: Add details
# TODO: Add example
# TODO: Add boot for bootstrap CIs for DR2.

r2.change <- function(
    data, X, Y, Z, keys, keys_g = NULL, keys_b = NULL, extra = NULL,
    boot = NA, boot_ci = .95,
    fit_save = TRUE, fit_measures = "all", progress = TRUE,
    std.lv = TRUE, miss = "default", est = "default", ordered = NULL,
    name = "r2d", check = FALSE, save_out = FALSE
  ) {
  if (!X %in% c(names(keys), names(keys_g), names(data))) {
    stop("'X' is not included in 'keys', 'keys_g', or 'data'.")
  }
  if (!Y %in% c(names(keys), names(keys_g), names(data))) {
    stop("'Y' is not included in 'keys', 'keys_g', or 'data'.")
  }
  if (sum(!Z %in% c(names(keys), names(keys_g), names(data))) > 0) {
    stop(
      paste(
        "At least one 'Z' variable is not included in 'keys', 'keys_g', or",
        "'data'."
      )
    )
  }
  if (!is.null(keys_g) & is.null(keys_b)) {
    stop(
      paste(
        "'keys_g' is specified and 'keys_b' is not. Both or neither must be",
        "specified."
      )
    )
  }
  if (is.null(keys_g) & !is.null(keys_b)) {
    stop(
      paste(
        "'keys_b' is specified and 'keys_g' is not. Both or neither must be",
        "specified."
      )
    )
  }
  if (sum(names(keys) %in% names(keys_g)) > 0) {
    stop(
      paste(
        "At least one element of 'keys' has the same name as an element of",
        "'keys_g'. General factor latent variables need different names to",
        "CFA or group factor names."
      )
    )
  }
  if (!is.na(boot)) {
    boot_min <- 2 / (1 - boot_ci)
    if (boot < boot_min) {
      warning(
        paste(
          "The number of bootstraps relative to the 'boot_ci' means that maximum",
          "and minimum bootstrap values will be used and confidence intervals",
          "will be unreliable.",
          "Consider using", boot_min * 5, "or more for 'boot'."
        )
      )
    } else {
      if (boot < 5 * boot_min) {
        warning(
          "The number of bootstraps relative to 'boot_ci' is small,",
          "meaning only a few values will fall outside the confidence interval,",
          "so interval values may not be reliable.",
          "Consider using", boot_min * 5, "or more for 'boot'."
        )
      }
    }
  }


  # TODO: Is some pre-treatment of extra required to sort CFA extra, bifactor
  #       extra, and inter-model extra?

  # #### Copied from cfa.from.keys #####
  # if (!is.null(extra)) {
  #   # Removal all fixed values and parameter names; remove punctuation
  #   extra_vars <- lapply(
  #     extra,
  #     function(y) {
  #       tmp <- stringr::str_split(
  #         gsub("((\\+|~~|~).*?(\\*))", " ", y), "\\+|~|=|\n| ", simplify = TRUE
  #       )
  #       tmp[tmp != ""]
  #     }
  #   )
  #   extra_vars1 <- unique(unlist(extra_vars))
  #   extra_vars2 <- extra_vars1[!extra_vars1 %in% unlist(items)]
  #   if (length(extra_vars2) > 0) {
  #     if (length(extra_vars2) > 1) {
  #       stop(
  #         paste0(
  #           "The following items were found in 'extra' but do not match ",
  #           "either a variable name in 'keys' or standard lavaan code.",
  #           "\n\n    ", paste(extra_vars2, collapse = ", ")
  #         )
  #       )
  #     } else {
  #       stop(
  #         paste0(
  #           "'", extra_vars2, "' was found in 'extra' but does not match ",
  #           "either a variable name in 'keys', 'keys_g', or standard lavaan ",
  #           "code."
  #         )
  #       )
  #     }
  #   }
  # }
  # #### Copied from cfa.from.keys #####

  # Only keep required models
  keys_g <- keys_g[names(keys_g) %in% c(X, Y, Z)]
  keys_b <- keys_b[names(keys_b) %in% c(X, Y, Z)]
  keys_c <- keys[names(keys) %in% c(X, Y, Z) &
                   !names(keys) %in% c(names(keys_g), names(keys_b))]

  dr2 <- function(
    data, kc = NULL, kg = NULL, kb = NULL, k = keys, extra = extra,
    fit_save = FALSE, progress = FALSE,
    miss = miss, est = est, ordered = ordered,
    name = name, boot_n = 0, check = check, save_out = save_out
  ) {
    # At least one CFA measurement model
    if (length(kc) > 0) {
      cfa_fit <- suppressMessages(
        cfa.from.keys(
          kc, data, extra = extra, fit_save = FALSE, progress = FALSE,
          miss = miss, est = est, ordered = ordered,
          name = paste0(name, "_c", boot_n), check = check, save_out = save_out
        )
      )
    }
    # At least one bifactor model
    if (length(kg) > 0) {
      bif_fit <- suppressMessages(
        bifactor.from.keys(
          kg, kb, k, data, extra = extra, fit_save = FALSE,
          progress = FALSE, miss = miss, est = est, ordered = ordered,
          name = paste0(name, "_b", boot_n), check = check, save_out = save_out
        )
      )
    }
    if (exists("cfa_fit")) {
      mods_c <- sapply(
        names(kc),
        function(x) {
          x0 <- cfa_fit$par[[x]]
          x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
          x2 <- x1[!(x == x1$lhs & x == x1$rhs), ]
          paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n")
        },
        simplify = FALSE
      )
    } else {
      mods_cfa <- NULL
    }
    if (exists("bif_fit")) {
      mods_b <- sapply(
        names(kg),
        function(x) {
          x0 <- bif_fit$par[[x]]
          x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
          x2 <- x1[!(x == x1$lhs & x1$op == "~~"), ]
          x3 <- paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n")
          groupf <- unique(x0$lhs[x0$op == "=~" & !(x0$lhs %in% x)])
          paste0(x3, "\n", x, " ~~ 0 * ", paste(groupf, collapse = " + 0 * "))
        },
        simplify = FALSE
      )
    } else {
      mods_bif <- NULL
    }
    mod <- paste0(
      paste(mods_c, collapse = "\n"), "\n",
      paste(mods_b, collapse = "\n"), "\n",
      paste(Y, "~", paste(Z, collapse = " + "), "+", X)
    )
    fit <- sem.check(
      mod, data, keys_s = unique(c(unlist(keys_c), unlist(keys_g))),
      fit_save = fit_save, fit_measures = fit_measures,
      miss = miss, est = est, std.lv = FALSE, ordered = ordered,
      orthogonal = FALSE,
      name = paste0(name, "_", boot_n), check = check, save_out = save_out,
      use_sam = FALSE
    )


    # TODO: Take R2s from full model
    # TODO: Calculate R2 of reduced model
    # TODO: Take regression table from models
    #       Should this be for all or just the main one?
    # TODO: Put together into a table.
  }
}
