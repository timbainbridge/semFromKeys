#' Run incremental validity analysis using latent variables
#'
#' @description
#' Incremental validity using standard linear regressions is invalid with less
#' than perfect (or close to perfect) reliability (Westfall & Yarkoni, 2016).
#' Latent variable models must be used instead.
#' However, this is difficult, especially if confidence intervals are required,
#' which can only be created with bootstrap sampling.
#'
#' `r2.change` streamlines the process of running incremental validity
#' analyses with latent variables, including generating bootstrap confidence
#' intervals.
#'
#' The function was inspired by work from Hayes (2021) and uses
#' `boot:::norm.inter` copied from `boot`.
#'
#' @inheritParams sem.check
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
#' `boot_ci` must be greater than 0 and less than 1.
#' Irrelevant if `boot = NULL`.
#'
#' @return
#' A list of length X.
#' Elements include the fitted lavaan objects for the full and reduced models;
#' estimates of R-squared and R-squared change; and data frames of covariate
#' effect sizes from both the full and reduced models.
#'
#' @details
#' Incremental validity analyses and 2-stage regressions are invalid when using
#' mean or sum scores (Westfall & Yarkoni, 2016). To illustrate, take two
#' separate scales that measure the same construct. Neither scale should have
#' incremental validity over the other. However, unique item variance means that
#' they will not correlate at 1 and any uncertainty in the measurement due to
#' unique items variance (and indicated by reliability) is obscured from the
#' model. Thus, the standard errors of the overlap in the scales' variance is
#' incorrectly too narrow and the likelihood of false positives increases.
#' The chance of a false positive increases with sample size and lower
#' reliabilities (down to .5), and rises towards 1 as N increases (Westfall &
#' Yarkoni, 2016).
#'
#' To solve this issue, latent variables must be used (Westfall & Yarkoni,
#' 2016). Latent variable models have two advantages over aggregate scores in
#' this context. First, they measure relationships between the shared variance
#' of their respective items, preserving the possibility of correlations of 1
#' and drastically reducing the impact of idiosyncratic item variance.
#' Second, poorer reliability increases the uncertainty in the measurement, such
#' that, as the correlation between two different scales of the same constructs
#' becomes smaller, uncertainty increases. Together these factors mean that
#' separate scales measuring the same constructs should fail to demonstrate
#' incremental validity at an appropriate rate.
#'
#' `r2.change` makes the process of running incremental validity analyses with
#' latent variables much easier. It takes data, keys, and names for the focal
#' variable (X), the outcome variable (Y), and variables to test against (Z) as
#' inputs, and produces an r-squared change figure, optionally with bootstrap
#' confidence intervals. Various other outputs of the process are also returned
#' (see `Value`).
#'
#' Traditionally incremental validity analyses rely on two models.
#' A reduced model regresses Y on Z and a full model adds X as a predictor.
#' R-squared change between the models is used to assess the incremental
#' validity of X predicting Y.
#' Such a method cannot be used naively with latent variables because excluding
#' X from the reduced model can change the structural model such that the
#' r-squared values cannot be compared.
#' Hayes (2021) discussed the different approaches to the issue.
#' One appropriate method leaves the X variable in the reduced model but allows
#' it to correlate with Y's residual.
#' Another method calculates r-squared for the reduced model using correlations
#' from the full model.
#' `r2.change` uses the first of these approaches to produce a regression
#' table for the reduced model (so it can be included in a table, for example)
#' and it uses the second approach to calculate r-squared changes both in the
#' base models and bootstrap samples. The latter method means that minor changes
#' in the models' convergence do not affect the r-squared estimates, even though
#' the models are merely different versions of the same model (i.e., same fit
#' statistics, degrees of freedom etc. just with a different structural path).
#'
#' The problem of models being incomparable when variables are added or removed
#' is known as "interpretational confounding" (Burt, 1976) and occurs because
#' the model fitting method does not distinguish between measurement and
#' structural parameters. Thus, measurement parameters will shift depending on
#' what other variables are in the model, thereby changing the interpretation
#' of the latent variables.
#' `r2.change` uses Burt's 2-stage approach to account for interpretational
#' confounding. The method produces accurate point estimates but somewhat biases
#' standard errors and fit statistics (Nagy et al., 2017).
#' This means standard errors for regression coefficients and other model
#' parameters will be slightly biased, but the r-squared change or bootstrap CIs
#' will no be affected because these are entirely calculated with point
#' estimates. For further details, see the "Interpretational Confounding"
#' section.
#'
#' If using bootstrap confidence intervals, it is highly recommended that you
#' use `save_out = TRUE` and `check = TRUE`, especially for more complex models.
#' The method requires setting up a cache directory with [cache.setup()] before
#' running but it then means that the bootstrap models will not have to be run
#' again if they have already been run and nothing has changed (including number
#' of bootstraps, argument inputs, or relevant data).
#' The functionality can be helpful if something has been changed in earlier
#' code and you are not sure if it affects the bootstrap models. If it did not,
#' then running the code will be quick and you will know that it is still
#' accurate.
#'
#' @inheritSection esem.from.keys Interpretational Confounding
#'
#' @seealso
#' [cache.setup()] [sem.check()] [cfa.from.keys()] [bifactor.from.keys()]
#'
#' @importFrom lavaan lavInspect
#' @importFrom stats qnorm
#' @importFrom stats na.omit
#' @export
#'
#' @references
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.
#' https://doi.org/10.1177/004912417600500101.
#'
#' Hayes, T. (2021).
#' R-squared change in structural equation models with latent variables and
#' missing data. Behavior Research Methods, 53(5), 2127-2157.
#' https://doi.org/10.3758/s13428-020-01532-y.
#'
#' Westfall, J., and Yarkoni, T. (2016).
#' Statistically controlling for confounding constructs is harder than you
#' think. PLoS ONE, 11(3), 1-23.
#' https://doi.org/10.1371/journal.pone.0152719.
#'
#' @examples
#' # Test incremental validity of grit_c over hope predicting grit_p
#' # Create keys
#' keys <- list(
#'   grit_p = paste0("grit_p_", 1:6),
#'   grit_c = paste0("grit_c_", 1:6),
#'   hope_p = paste0("hope_p_", 1:4)
#' )
#' keys_g <- list(hope = c(keys$hope_p, paste0("hope_a_", 1:4)))
#' keys_b <- list(hope = "hope_p")
#' dr2 <- r2.change(
#'   BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
#'   keys = keys, keys_g = keys_g, keys_b = keys_b, fit_save = FALSE
#' )
#' dr2$dr2

r2.change <- function(
    data, X, Y, Z, keys, keys_g = NULL, keys_b = NULL, extra = NULL,
    boot = NULL, boot_ci = .95, seed = NULL,
    fit_save = TRUE, fit_measures = "all", progress = TRUE,
    std.lv = TRUE, miss = "default", est = "default", ordered = NULL,
    name = "dr2", check = FALSE, save_out = FALSE
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
    if (boot_ci <= 0 | boot_ci >= 1) {
      stop("'boot_ci' must be greater than 0 and less than 1.")
    }
    if (nrow(data) < 200) {
      warning(
        paste(
          "'data' has < 200 rows.",
          "Using bootstraps may results in many models that fail to converge",
          "or other issues. Please examine the results carefully."
        )
      )
    }
    if (boot < 20) {
      stop(
        paste(
          "The number of bootstraps is extremely small. The function will not",
          "work with boot < 20 to insure bootstrap CIs are not too unreliable.",
          "\nHowever, consider setting boot = 200, 500, or 1000 to get",
          "reliable results."
        )
      )
    }
  }
  # Only keep required models
  keys_g <- keys_g[names(keys_g) %in% c(X, Y, Z)]
  keys_b <- keys_b[names(keys_b) %in% c(X, Y, Z)]
  keys_c <- keys[names(keys) %in% c(X, Y, Z) &
                   !names(keys) %in% c(names(keys_g), names(keys_b))]
  items <- c(X, Y, Z)[c(X, Y, Z) %in% names(data)]
  keys_s <- c(unlist(keys_g), unlist(keys_c), items)
  data1 <- data[unlist(keys_s)]
  if (sum(items %in% unlist(keys_c)) > 0) {
    stop(
      paste(
        "At least one element of 'items' is in 'keys'.",
        "Items must not be in any latent variable."
      )
    )
  }
  if (sum(items %in% unlist(keys_g)) > 0) {
    stop(
      paste(
        "At least one element of 'items' is in 'keys_g'.",
        "Items must not be in any latent variable."
      )
    )
  }
  if (!is.null(extra)) {

    #### Adapted from cfa.from.keys #####
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
    extra_vars2 <- extra_vars1[!extra_vars1 %in% unlist(keys_s)]
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
            "either a variable name in 'keys', 'keys_g', or standard lavaan ",
            "code."
          )
        )
      }
    }
    mod_extra_c <- unlist(
      sapply(
        keys_c,
        function(k) {
          tmp <- mapply(
            xv = extra_vars, x = extra,
            FUN = function(xv, x) if (sum(!xv %in% k) == 0) x else ""
          )
          tmp[tmp != ""]
        }
      )
    )
    #### Adapted from cfa.from.keys #####

    mod_extra_b <- unlist(
      sapply(
        keys_g,
        function(k) {
          tmp <- mapply(
            xv = extra_vars, x = extra,
            FUN = function(xv, x) if (sum(!xv %in% k) == 0) x else ""
          )
          tmp[tmp != ""]
        }
      )
    )
    mod_extra_gen <- unlist(
      sapply(extra, function(x) x[!x %in% c(mod_extra_c, mod_extra_b)])
    )
  } else {
    mod_extra_c <- NULL
    mod_extra_b <- NULL
    mod_extra_gen <- NULL
  }

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
    data, keys_c = keys_c, keys_g = keys_g, keys_b = keys_b, keys = keys,
    ex_c = mod_extra_c, ex_b = mod_extra_b, ex = mod_extra_gen,
    progress = progress, miss = miss, est = est, ordered = ordered,
    name = name, boot_n = 0, check = check, save_out = save_out
  ) {
    check1 <- ifelse(boot_n == 0, check, FALSE)
    save_out1 <- ifelse(boot_n == 0, save_out, FALSE)
    # At least one CFA measurement model
    if (length(keys_c) > 0) {
      cfa_fit <- suppressWarnings(
        suppressMessages(
          cfa.from.keys(
            keys_c, data, extra = ex_c, fit_save = FALSE, progress = FALSE,
            miss = miss, est = est, ordered = ordered,
            name = paste0(name, "_c"), check = check1, save_out = save_out1
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
    if (length(keys_g) > 0) {
      bif_fit <- suppressWarnings(
        suppressMessages(
          bifactor.from.keys(
            keys_g, keys_b, keys, data, extra = ex_b, fit_save = FALSE,
            progress = FALSE, miss = miss, est = est, ordered = ordered,
            name = paste0(name, "_b"), check = check1, save_out = save_out1
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
    if (!is.null(keys_c)) {
      mods_c <- sapply(
        names(keys_c),
        function(x) {
          x0 <- cfa_fit$par[[x]]
          x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
          x2 <- x1[!(x == x1$lhs & x == x1$rhs), ]
          paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n")
        },
        simplify = FALSE
      )
    } else {
      mods_c <- NULL
    }
    if (!is.null(keys_g)) {
      mods_b <- sapply(
        names(keys_g),
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
      mods_b <- NULL
    }
    if (boot_n == 0) {
      mods <- as.list(
        paste0(
          paste(mods_c, collapse = "\n"), "\n",
          paste(mods_b, collapse = "\n"), "\n",
          ex, "\n",
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
        ex, "\n",
        paste(Y, "~", paste(Z, collapse = " + "), "+", X)
      )
    }
  }

  if (progress) message("Running baseline models")
  base_m <- mm(
    data1, keys_c = keys_c, keys_g = keys_g, keys_b = keys_b, keys = keys,
    ex_c = mod_extra_c, ex_b = mod_extra_b, ex = mod_extra_gen,
    progress = progress, miss = miss, est = est, ordered = ordered,
    name = name, boot_n = 0, check = check, save_out = save_out
  )
  ks <- stats::setNames(rep(list(keys_s), 2), names(base_m))
  base_f <- sem.check(
    base_m, data1, keys_s = ks, fit_save = fit_save, fit_measures = fit_measures,
    miss = miss, est = est, ordered = ordered,
    name = name, check = check, save_out = save_out
  )
  pc <- sapply(base_f$fit, lavInspect, what = "post.check")
  if (sum(!pc) > 0) {
    warning(
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
  reduced <- ext.r2(base_f$par_std$reduced, Y)
  structural <- full_out
  structural$r2 <- rbind(full = full_out$r2, reduced = reduced$r2)
  structural$b <- list(full = full_out$b, reduced = reduced$b)
  structural$r <- list(full = full_out$r, reduced = reduced$r)
  base_dr2 <- c(
    base_f,
    structural = list(structural),
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
    params <- list(
      miss = miss, est = est, std = NULL, std.lv = std.lv, orthogonal = NULL,
      ordered = ordered, seed = seed
    )
  }
  if (check) {
    check_b <- check.cache(
      name = paste0(name, "_ci"),
      miss = miss,
      est = est,
      std.lv = std.lv,
      ordered = ordered,
      data = data1,
      keys_s = c(unlist(keys), unlist(keys_g), unlist(keys_b)),
      seed = seed
    )
    if (check_b$param_test & check_b$d_test) {
      if (
        file.exists(
          file.path(cache_dir, paste0(name, "_ci"), paste0(name, "_ci_dr2.rds"))
        )
      ) {
        boot_dr2 <- readRDS(
          file.path(cache_dir, paste0(name, "_ci"), paste0(name, "_ci_dr2.rds"))
        )
        run <- FALSE
      } else {
        run <- TRUE
      }
    } else {
      run <- TRUE
    }
  } else {
    run <- TRUE
  }
  if (run) {
    # Select bootstrap samples
    if (!is.null(seed)) set.seed(seed)
    boot_samples <- sapply(
      1:boot, function(x) sample(nrow(data1), nrow(data1), TRUE)
    )
    message("Running bootstrap measurement models")
    boot_m <- lapply(
      1:boot,
      function(b) {
        if (progress & b %in% round((0:(boot/10)*10) + 1, 0)) {
          message(b, " / ", boot)
        }
        mm(
          data1[boot_samples[, b], ],
          keys_c = keys_c, keys_g = keys_g, keys_b = keys_b, keys = keys,
          ex_c = mod_extra_c, ex_b = mod_extra_b, ex = mod_extra_gen,
          progress = progress, miss = miss, est = est, ordered = ordered,
          name = name, boot_n = b, check = check, save_out = save_out
        )
      }
    )
    message("Running bootstrap structural models")
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
  }
  if (save_out) {
    if (!dir.exists(file.path(cache_dir, paste0(name, "_ci")))) {
      dir.create(file.path(cache_dir, paste0(name, "_ci")))
    }
    hash_d <- md5(paste(data1, collapse = ""))
    saveRDS(
      boot_dr2,
      file.path(cache_dir, paste0(name, "_ci"), paste0(name, "_ci_dr2.rds"))
    )
    saveRDS(
      params,
      file.path(cache_dir, paste0(name, "_ci"), paste0(name, "_ci_params.rds"))
    )
    saveRDS(
      hash_d,
      file.path(cache_dir, paste0(name, "_ci"), paste0(name, "_ci_hash.rds"))
    )
  }
  alp <- (1 + c(-boot_ci, boot_ci))/2

  ##### Copied from boot's internal function, i.e., boot:::norm.inter #####
  norm.inter <- function (t, alpha) {
    t <- t[is.finite(t)]
    R <- length(t)
    rk <- (R + 1) * alpha
    if (!all(rk > 1 & rk < R)) {
      warning("extreme order statistics used as endpoints")
    }
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
