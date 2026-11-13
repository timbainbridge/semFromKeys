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
#'
#' @importFrom lavaan lavInspect

# TODO: Add details
# TODO: Add example
# TODO: Add boot for bootstrap CIs for DR2.
# TODO: Check 'extra' code for errors, remove irrelevant 'extra' code.
# TODO: Add 'extra' code to end of model
# TODO: Add more advanced warnings to boot_m code.

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
    if (nrow(data) < 200) {
      warning(
        paste(
          "'data' has < 200 rows.",
          "Using bootstraps may results in many models that fail to converge",
          "or other issues.",
          "Please examine the results carefully."
        )
      )
    }
    boot_min <- max(2 / (1 - boot_ci), 12)
    if (boot < 5 * boot_min) {
      warning(
        paste(
          "The number of bootstraps relative to the 'boot_ci' means that",
          "maximum and minimum bootstrap values will be used and confidence",
          "intervals will be unreliable.",
          "Consider using", boot_min * 12.5, "or more for 'boot'."
        )
      )
    } else {
      if (boot < 12.5 * boot_min) {
        warning(
          paste0(
            "The number of bootstraps relative to 'boot_ci' is small, ",
            "meaning only a few values will fall outside the confidence ",
            "interval, so interval values may not be reliable.\n",
            "Consider using at least ", boot_min * 12.5, " for 'boot'."
          )
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
  keys_s <- c(unlist(keys_g), unlist(keys_c))

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

  mm <- function(
    data, kc = keys_c, kg = keys_g, kb = keys_b, k = keys, ex = extra,
    pro = progress, m = miss, est0 = est, ord = ordered,
    n = name, boot_n = 0, check0 = check, so = FALSE, boot = boot
  ) {
    check1 <- ifelse(boot_n == 0, check0, FALSE)
    so1 <- ifelse(boot_n == 0, so, FALSE)
    # At least one CFA measurement model
    if (length(kc) > 0) {
      cfa_fit <- suppressWarnings(
        suppressMessages(
          cfa.from.keys(
            kc, data, extra = ex, fit_save = FALSE, progress = FALSE,
            miss = miss, est = est0, ordered = ord,
            name = paste0(n, "_c"), check = check1, save_out = so1
          )
        )
      )
      if (boot_n > 0) {
        pc <- sapply(
          cfa_fit$fit, function(x) lavInspect(x, what = "post.check")
        )
        if (sum(!pc) > 0) return(FALSE)
      }
    }
    # At least one bifactor model
    if (length(kg) > 0) {
      bif_fit <- suppressWarnings(
        suppressMessages(
          bifactor.from.keys(
            kg, kb, k, data, extra = ex, fit_save = FALSE,
            progress = FALSE, miss = miss, est = est0, ordered = ord,
            name = paste0(n, "_b"), check = check1, save_out = so1
          )
        )
      )
      if (boot_n > 0) {
        pc <- sapply(
          bif_fit$fit, function(x) lavInspect(x, what = "post.check")
        )
        if (sum(!pc) > 0) return(FALSE)
      }
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
            paste0(Y, " ~ ", paste(Z, collapse = " + "), "\n", Y, " ~~ ", X)
          )
        )
      )
      names(mods) <- c("full", "reduced")
      return(mods)
    } else {
      paste0(
        paste(mods_c, collapse = "\n"), "\n",
        paste(mods_b, collapse = "\n"), "\n",
        paste(Y, "~", paste(Z, collapse = " + "), "+", X)
      )
    }
  }

  if (progress) {
    message("Running baseline models")
  }
  base_m <- mm(data, boot_n = 0)
  ks <- stats::setNames(rep(list(keys_s), 2), names(base_m))
  base_f <- sem.check(
    base_m, data, keys_s = ks, fit_save = fit_save, fit_measures = fit_measures,
    miss = miss, est = est, ordered = ordered,
    name = name, check = check, save_out = save_out
  )
  pc <- sapply(base_f$fit, lavInspect, what = "post.check")
  if (sum(!pc) > 0) {
    message(
      paste(
        "A sensible solution to the baseline model was not found.",
        "Please review and resolve the issues and try again."
      )
    )
    return(base_f)
  }
  full_out <- ext.r2(base_f$par_std$full, Y)
  cm <- lavaan::lavInspect(base_f$fit$full, what = "cor.all")
  yy <- cm[Y, Y, drop = FALSE]
  zz <- cm[Z, Z, drop = FALSE]
  zy <- cm[Z, Y, drop = FALSE]
  b <- solve(zz, zy)
  r2_reduced <- colSums(b * zy)/diag(yy)
  dr2 <- full_out$r2$R2 - r2_reduced
  base_dr2 <- c(
    base_f,
    full = list(full_out), reduced = list(ext.r2(base_f$par_std$reduced, Y)),
    dr2 = list(c(r2 = r2_reduced[[1]], dr2 = dr2[[1]]))
  )
  if (is.null(boot)) return(base_dr2)
  if (check | save_out) {
    found <- FALSE
    for (i in 1:5) {
      env <- parent.frame(i)
      if (exists(".cache_env", envir = env)) {
        found <- TRUE
        break
      }
    }
    if (!found) {
      stop(
        paste(
          "A cache directory is not configured so cannot be used.",
          "Use the 'cache.setup()' function to configure a cache directory."
        )
      )
    }
    cache_dir <- get("cache_dir", envir = get(".cache_env", envir = env))
    check_b <- check.cache(
      name = name,
      miss = miss,
      est = est,
      std = std,
      std.lv = std.lv,
      ordered = ordered,
      data = data,
      keys_s = c(unlist(keys), unlist(keys_g), unlist(keys_b)),
      seed = seed
    )
    if (check_b$param_test & check_b$d_test) {
      if (file.exists(file.path(cache_dir, name, "dr2.rds"))) {
        return(file.path(cache_dir, name, "dr2.rds"))
      }
    }
  }
  # Select bootstrap samples
  if (!is.null(seed)) set.seed(seed)
  boot_samples <- sapply(
    1:boot, function(x) sample(nrow(data), nrow(data), TRUE)
  )
  message("Running bootstrap measurement models")
  boot_m <- lapply(
    1:boot,
    function(b) {
      if (progress & b %in% round((0:(boot/10)*10) + 1, 0)) {
        message(b, " / ", boot)
      }
      mm(data[boot_samples[, b], ], boot_n = b)
    }
  )
  # names(boot_m) <- paste0("B", 1:boot, "_full")
  message("Running bootstrap structural models")
  data1 <- data[unlist(keys_s)]
  if (!is.null(miss)) {
    if (miss == "default") {
      if (is.null(ordered)) {
        miss <- "ML"
      } else {
        miss <- "pairwise"
      }
    }
  }
  # Bootstrap models run with sem rather than sem.check because I do not want to
  # clog up people's cache with 1000s of models when save_out = TRUE and this
  # way is quicker, given there's no need to return actual models.
  boot_dr2 <- mapply(
    m = boot_m, s = as.data.frame(boot_samples), b = 1:boot,
    FUN = function(m, s, b) {
      if (m == FALSE) {
        return(NA)
      } else {
        if (progress & b %in% round((0:(boot/10)*10) + 1, 0)) {
          message(b, " / ", boot)
        }

        ##### Adapted from sem.check #####
        m1 <- if (est == "default") {
          if (is.null(ordered)) {
            sem(
              model   = m,
              data    = data1[s, ],
              missing = miss,
              std.lv  = TRUE
            )
          } else {
            sem(
              model   = m,
              data    = data1[s, ],
              missing = miss,
              std.lv  = TRUE,
              ordered = ordered
            )
          }
        } else {
          if (is.null(ordered)) {
            sem(
              model   = m,
              data    = data1[s, ],
              missing = miss,
              estimator = est,
              std.lv  = TRUE
            )
          } else {
            sem(
              model   = m,
              data    = data1[s, ],
              missing = miss,
              estimator = est,
              std.lv  = TRUE,
              ordered = ordered
            )
          }
        }
        ##### Adapted from sem.check #####

        ss <- standardizedSolution(m1)
        out <- ext.r2(ss, Y)
        cm <- lavaan::lavInspect(m1, what = "cor.all")
        yy <- cm[Y, Y, drop = FALSE]
        zz <- cm[Z, Z, drop = FALSE]
        zy <- cm[Z, Y, drop = FALSE]
        b <- solve(zz, zy)
        r2_r <- colSums(b * zy)/diag(yy)
        out$r2$R2 - r2_r[[1]]
      }
    }
  )
  if (save_out) {
    saveRDS(boot_dr2, file.path(cache_dir, name, "dr2.rds"))
    saveRDS(
      list(
        miss = miss, est = est, std = std, std.lv = std.lv,
        ordered = ordered, seed = seed
      ),
      file.path(cache_dir, name, "params.rds")
    )
  }
  alp <- (1 + c(-boot_ci, boot_ci))/2

  ##### Copied from boot's internal function, i.e., boot:::norm.inter #####
  norm.inter <- function (t, alpha) {
    t <- t[is.finite(t)]
    R <- length(t)
    rk <- (R + 1) * alpha
    if (!all(rk > 1 & rk < R))
      warning("extreme order statistics used as endpoints")
    k <- trunc(rk)
    inds <- seq_along(k)
    out <- inds
    kvs <- k[k > 0 & k < R]
    tstar <- sort(t, partial = sort(union(c(1, R), c(kvs, kvs + 1))))
    ints <- (k == rk)
    if (any(ints)) {
      out[inds[ints]] <- tstar[k[inds[ints]]]
    }
    out[k == 0] <- tstar[1L]
    out[k == R] <- tstar[R]
    not <- function(v) xor(rep(TRUE, length(v)), v)
    temp <- inds[not(ints) & k != 0 & k != R]
    temp1 <- qnorm(alpha[temp])
    temp2 <- qnorm(k[temp]/(R + 1))
    temp3 <- qnorm((k[temp] + 1)/(R + 1))
    tk <- tstar[k[temp]]
    tk1 <- tstar[k[temp] + 1L]
    out[temp] <- tk + (temp1 - temp2)/(temp3 - temp2) * (tk1 - tk)
    cbind(round(rk, 2), out)
  }
  ##### Copied from boot's internal function, i.e., boot:::norm.inter #####

  dr2_ci <- norm.inter(na.omit(boot_dr2), alp)[, 2]
  cv <- sum(!is.na(boot_dr2))
  base_dr2$dr2 <- data.frame(
    c(
      base_dr2$dr2, lower_ci = list(dr2_ci[1]), upper_ci = list(dr2_ci[2]),
      converged = list(cv)
    )
  )
  base_dr2
}
