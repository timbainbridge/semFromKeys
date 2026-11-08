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
#' Numeric or `NULL`.
#' If `NULL`, then bootstrap confidence intervals are not calculated.
#' If numeric, then bootstrap confidence intervals are calculated with a number
#' of bootstrap samples equal to the value of `boot`.
#' @param seed
#' A number used in [set.seed()] for bootstrap data selection.
#' Defaults to `NA` indicating a random
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
#' @param extra
#' A string or vector of strings of lavaan code to be appended to the model.
#' The argument can be used to set constraints, fix parameters to
#' specific values, or allow correlations between item residuals.
#' @param boot_ci
#' Numeric. The confidence interval proportion for bootstrap CIs.
#' Irrelevant if `boot = NULL`.
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
# TODO: Is it possible to set up the bootstrap samples first, then run all the
#       CFA models, all the bifactor models, and all the SEMs, rather than
#       piecemeal. The advantage would be that all the bootstrap SEMs can be run
#       in the same instance of sem.check.

r2.change <- function(
    data, X, Y, Z, keys, keys_g = NULL, keys_b = NULL, extra = NULL,
    boot = NULL, boot_ci = .95, seed = NULL,
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
  if (!is.null(boot)) {
    boot_min <- 2 / (1 - boot_ci)
    if (boot < boot_min) {
      warning(
        paste(
          "The number of bootstraps relative to the 'boot_ci' means that",
          "maximum and minimum bootstrap values will be used and confidence",
          "intervals will be unreliable.",
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

  ext.r2 <- function(par, Y) {
    ##### Adapted from sem.path #####
    xr <- par[par$op == "~~" & par$lhs == par$rhs & par$lhs %in% Y, ]
    r2 <- data.frame(
      R2 = 1 - xr$est.std,
      se = xr$se,
      ci.lower = 1 - xr$ci.upper,
      ci.upper = 1 - xr$ci.lower
    )
    b <- par[par$op == "~", -(1:2)]
    names(b)[1] <- "Variable"
    r <- par[par$op == "~~" & par$lhs != par$rhs, ]
    list(r2 = r2, b = b, r = r)
    ##### Adapted from sem.path #####
  }

  dr2 <- function(
    data, kc = keys_c, kg = keys_g, kb = keys_b, k = keys, ex = extra,
    fs = FALSE, fm = fit_measures, pro = FALSE,
    m = miss, est0 = est, ord = ordered,
    n = name, boot_n = 0, check0 = check, so = save_out
  ) {
    # At least one CFA measurement model
    if (length(kc) > 0) {
      cfa_fit <- suppressMessages(
        cfa.from.keys(
          kc, data, extra = ex, fit_save = FALSE, progress = FALSE,
          miss = m, est = est0, ordered = ord,
          name = paste0(n, "_c", boot_n), check = check0, save_out = so
        )
      )
    }
    # At least one bifactor model
    if (length(kg) > 0) {
      bif_fit <- suppressMessages(
        bifactor.from.keys(
          kg, kb, k, data, extra = ex, fit_save = FALSE,
          progress = FALSE, miss = m, est = est0, ordered = ord,
          name = paste0(n, "_b", boot_n), check = check0, save_out = so
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
    if (boot_n == 0) {
      mods <- as.list(
        paste0(
          paste(mods_c, collapse = "\n"), "\n",
          paste(mods_b, collapse = "\n"), "\n",
          c(
            paste(Y, "~", paste(Z, collapse = " + "), "+", X),
            paste(Y, "~", paste(Z, collapse = " + "), "\n", Y, "~~", X)
          )
        )
      )
      names(mods) <- c("full", "reduced")
    } else {
      mods <- list(
        full = paste0(
          paste(mods_c, collapse = "\n"), "\n",
          paste(mods_b, collapse = "\n"), "\n",
          paste(Y, "~", paste(Z, collapse = " + "), "+", X)
        )
      )
    }
    if (boot_n != 0) message("Running bootstrap model ", boot_n)
    fit <- sem.check(
      mods, data,
      keys_s = stats::setNames(
        rep(list(unique(c(unlist(kc), unlist(kg)))), length(mods)),
        nm = names(mods)
      ),
      fit_save = fs, fit_measures = fm,
      miss = m, est = est0, std.lv = FALSE, ordered = ord,
      orthogonal = FALSE,
      name = paste0(n, "_", boot_n), check = check0, save_out = so,
      use_sam = FALSE
    )
    full_out <- ext.r2(fit$par_std$full, Y)
    cm <- lavaan::lavInspect(fit$fit$full, what = "cor.all")
    yy <- cm[Y, Y, drop = FALSE]
    zz <- cm[Z, Z, drop = FALSE]
    zy <- cm[Z, Y, drop = FALSE]
    b <- solve(zz, zy)
    r2_reduced <- colSums(b * zy)/diag(yy)
    dr2 <- full_out$r2$R2 - r2_reduced
    if (boot_n != 0) return(dr2) else {
      return(
        c(
          fit,
          full = list(full_out), reduced = list(ext.r2(fit$par_std$reduced, Y)),
          dr2 = list(c(r2 = r2_reduced[[1]], dr2 = dr2[[1]]))
        )
      )
    }
  }
  base <- dr2(data, boot_n = 0)
  # TODO: Bootstraps. Initially just use the standard check, save_out for these.
  # TODO: Add issue to github to change this to do the checks prior to running
  # sem.check. The rationale is to save the time running through all the
  # sem.check code and to save the space of saving all the sem.check inputs and
  # outputs. This should be possible here (while not elsewhere) because all the
  # bootstrap inputs can be checked before entering anything into sem.check.
  # TODO: Perhaps create a function to do the checks. This could be useful for
  # users also (to see if something will run without actually running it).
}
