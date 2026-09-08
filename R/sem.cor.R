#' Creates a latent variable correlation matrix from fitted lavaan measurement
#' models
#'
#' `sem.cor` takes CFA outputs and produces a correlation matrix between latent
#' variables.
#'
#' @inheritParams sem.check
#' @param fit_y A named list of CFA or bi-factor fitted objects.
#' @param fit_x
#' 'NULL' or a named list of CFA or bi-factor fitted objects to be correlated
#' with fit_y variables. Defaults to 'NULL'.
#' @param items
#' A vector of single-item variables to correlate with `fit_y` latent variables.
#' Must not include any items contributing to the measurement of a `fit_y`
#' latent variable.
#' @param item_loadings
#' When single items are specified, items are included in models with single
#' item latent variables. `item_loadings` sets the loading of the item on the
#' factor.
#' It can be a single number to set all loadings equal or a vector of
#' length equal to the length of items.
#' Defaults allows the value to be free (which assumes perfect reliability).
#' Irrelevant if `items = NULL`.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "cors".
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#' @param nagy
#' Logical. Indicates whether to use Nagy and colleagues' (2017) extension
#' procedure instead of Burt's (1976) 2-stage procedure.
#'
#' @return
#' Returns a list of length 4-7 depending on option selections.
#' All versions include fitted lavaan models (`fit`);
#' a correlation matrix (`cor_mat`);
#' p-values of the correlations (`pvalues`); and
#' a list of upper and lower 95% confidence intervals of the correlations
#' (`ci`).
#' If `fit_save = TRUE`, a list of fit measures is also returned
#' (`fit_measures`), and,
#' if `nagy = TRUE`, matrices of residual correlations, their p-values, and
#' lists of their 95% confidence intervals are also returned (`residual_cors`).
#' If both `fit_y` and `fit_x` are specified, then there is one set of residual
#' correlation results for the items from each set of factors
#' (i.e., `residual_cors_x` and `residual_cors_y` for the items of 'x' and 'y'
#' factors, respectively).
#'
#' @details
#' The function computes correlations between latent variables from fitted CFA
#' or bi-factor models.
#' If both `fit_x = NULL` and `items = NULL`, then correlations between all
#' `fit_y` latent variables are computed. If either `fit_x` or `items`
#' are specified, then correlations will be computed between `fit_y` latent
#' variables and any specified `fit_x` latent variables and `items`.
#' Items are treated as single item latent variables with loadings of
#' `item_loadings` if specified or freely estimated otherwise.
#' Hierarchical models are not supported and the function will exit with an
#' error if included.
#'
#' Correlation are calculated in separate models.
#' Primarily, this approach means that correlations can be calculated with
#' typical sample sizes in a manageable time frame compared to including
#' everything in one model.
#'
#' To control for interpretational confounding (Burt, 1976), the function uses
#' either Burt's (1976) 2-stage procedure (`nagy = FALSE`) or Nagy and
#' colleagues' (2017) extension procedure (`nagy = TRUE`).
#' Interpretational confounding occurs when the interpretation of a latent
#' variable is confounded by the inclusion of a conceptually distinct variable
#' in the model.
#' That is, the loadings of items on a latent variable can change, in some
#' cases markedly, due to the inclusion of a conceptually distinct factor.
#' This means that the factor does not represent a consistent concept for
#' different models, even in the same sample, as the loadings can change model
#' to model.
#'
#' Burt (1976) proposed simply fixing measurement model parameters in the model
#' estimating structural parameters.
#' This method means that latent variables' interpretations cannot change with
#' the addition or removal of different variables and less than ideal fit at the
#' measurement level cannot affect latent variable correlations.
#' Burt's (1976) method therefore solve interpretational confounding.
#' However, Burt's method also underestimates uncertainty in the measurement
#' part of the full model (e.g., Nagy et al., 2017), which results in biased
#' standard errors and fit statistics, although effects are usually small.
#'
#' Alternatively, Nagy's method involves allowing item residuals to correlate
#' with external variables and constrains those relationships such that the
#' model is identifiable.
#' Specifically, one of the methods to constrain these relationships minimises
#' the sum of squares of the correlations between each latent variable's items'
#' residuals and each of the structural variables in the model (excluding the
#' factor(s) they form part of the measurement of).
#' By using this method, measurement parameters in the structural model match
#' those of isolated measurement models without having to constrain them
#' directly.
#' As a result, unbiased standard errors are preserved while simultaneously
#' eliminating interpretational confounding.
#'
#' Which method should be chosen?
#' Burt's method is faster but artificially constrains parameters,
#' thereby biasing standard errors and model fit indices.
#' They should, however, give very similar point estimates for correlations.
#' Therefore, Nagy's method should be preferred whenever confidence intervals
#' or model fit matter, except, perhaps, for large sets of variables.
#'
#' If Nagy and colleagues' (2017) method is selected (with `nagy = TRUE`, the
#' default), correlations between factors and item residuals will be included in
#' the output and may provide useful insight into idiosyncratic item variance
#' (Nagy et al., 2017). `nagy = TRUE` is not currently supported for measurement
#' models with more than one latent variable.
#' Any such models (except for hierarchical models, which are not supported at
#' all) will be switched to use Burt's method with a warning.
#' If all included measurement models included more than one latent variable,
#' then outputs will be switched to match `nagy = FALSE` with a warning.
#'
#' It is possible for latent variable correlations to produce a non-positive
#' definite correlation matrix between variables included in `fit_y` (when
#' `fit_x` and `items` are not specified), especially when closely related
#' factors are included.
#' If the matrix of latent variables is not positive definite, then the matrix
#' will be adjusted to the nearest positive definite matrix using the
#' [Matrix::nearPD] function, which employs the method developed by Higham
#' (2002), and a message will state that the matrix was adjusted, and what the
#' maximum adjustment to any cell was.
#' Confidence intervals will be adjusted by the same amount as the corresponding
#' cell of the correlation matrix.
#' P-values will not be adjusted, however, so will be very slightly incorrect.
#' In general, inaccuracies in p-values will be most likely for larger
#' correlations (i.e., ones more likely to be highly significant) but if the
#' maximum adjustment, printed in the warning, is large, then use p-values with
#' care.
#'
#' The model relies on [sem.check] for the back-end of running the models,
#' which enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' In the case of `sem.cor`, the number of correlations can add up quickly.
#' When `nagy = FALSE`, this is unlikely to be an issue as each model typically
#' runs in a fraction of a second, but with `nagy = TRUE` and 20 scales, 19 +
#' 18 + 17 + ... + 1 = 190 correlations would need to be calculated and, if they
#' took an average of 5 seconds each to run, the total run time would be ~16
#' minutes. In these cases, the functionality may be useful.
#' For further details on how this works, see the [sem.check] function
#' documentation.
#'
#' @seealso
#' [sem.check], [lavaan::sem], [matrixcalc::is.positive.definite],
#' [Matrix::nearPD]
#'
#' @importFrom matrixcalc is.positive.definite
#' @importFrom Matrix nearPD
#' @export
#'
#' @references
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.
#' https://doi.org/10.1177/004912417600500101.
#'
#' Higham, N. J. (2002).
#' Computing the nearest correlation matrix—a problem from finance.
#' IMA Journal of Numerical Analysis, 22(3), 329-343.
#' https://doi.org/10.1093/imanum/22.3.329.
#'
#' Nagy, G., Brunner, M., Lüdtke, O., and Greiff, S. (2017).
#' Extension Procedures for Confirmatory Factor Analysis.
#' Journal of Experimental Education, 85(4), 574-596.
#' https://doi.org/10.1080/00220973.2016.1260524.
#'
#' @examples
#' # Create CFA keys
#' keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#' keys <- sapply(
#'   keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' # Run CFA models
#' cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = FALSE)
#' # Find correlations between all cfa_fit constructs.
#' cors <- sem.cor(BFIGritHope, cfa_fit$fit, nagy = FALSE)
#' # View the correlation matrix
#' cors$cor_mat
#'
#' # Correlations of grit facets with hope facets and the first item from each
#' # Big Five factor.
#' items <- names(BFIGritHope)[grep("bfi_.*1_1", names(BFIGritHope))]
#' cors2 <- sem.cor(
#'   BFIGritHope, cfa_fit$fit[1:2], cfa_fit$fit[3:4], items, nagy = FALSE
#' )
#' # View correlations
#' cors2$cor_mat

sem.cor <- function(
    data, fit_y, fit_x = NULL, items = NULL, item_loadings = NULL, nagy = TRUE,
    fit_save = FALSE, fit_measures = "all", miss = "default", est = "default",
    name = "cors", check = FALSE, save_out = FALSE
) {
  # Single model instead of list.
  if (!is.list(fit_y) & inherits(fit_y, "lavaan")) {
    fit_y <- list(factor_y = fit_y)
  }
  if (sum(sapply(fit_y, function(x) !inherits(x, "lavaan"))) > 0) {
    stop(
      paste(
        "At least one of the elements of 'fit_y' is not an object of class",
        "lavaan."
      )
    )
  }
  if (!is.null(items)) {
    if (sum(!items %in% names(data)) > 0) {
      item_miss <- items[!items %in% names(data)]
      stop(
        paste0(
          "The following item(s) in 'items' are not in 'data':\n  ",
          paste(item_miss, collapse = "\n  ")
        )
      )
    }
    if (sum(items %in% names(fit_y)) > 0) {
      item_overlap <- items[items %in% names(fit_y)]
      stop(
        paste0(
          "The following item(s) in 'items' are included as a name of a fitted",
          " model in 'fit_y'. ",
          "Please ensure item names do not conflict with latent variable ",
          "names.\n    ",
          paste(item_overlap, collapse = "\n    ")
        )
      )
    }
    if (!is.null(item_loadings)) {
      if (length(item_loadings) != 1 & length(items) != length(item_loadings)) {
        stop(
          paste(
            "'item_loadings' must be either 'NULL', length 1, or length equal",
            "to the lenght or items."
          )
        )
      }
    }
  }
  if (!is.null(fit_x)) {
    # Single model instead of list.
    if (!is.list(fit_x) & inherits(fit_x, "lavaan")) {
      fit_x <- list(factor_x = fit_x)
    }
    if (sum(sapply(fit_x, function(x) !inherits(x, "lavaan"))) > 0) {
      stop(
        paste(
          "At least one of the elements of 'fit_x' is not an object of class",
          "lavaan."
        )
      )
    }
    if (sum(items %in% names(fit_x)) > 0) {
      item_overlap <- items[items %in% names(fit_x)]
      stop(
        paste0(
          "The following item(s) in 'items' are included as a name of a fitted",
          " model in 'fit_x'. ",
          "Please ensure item names do not conflict with latent variable ",
          "names.\n    ",
          paste(item_overlap, collapse = "\n    ")
        )
      )
    }
  }
  if (length(fit_y) <= 1 & is.null(items) & is.null(fit_x)) {
    stop(
      paste(
        "'fit_y' is not at least length 2, and 'fit_x' and items are not",
        "specified.",
        "At least one measurement model and one item or two measurement",
        "models must be specified to calculate correlations with or between",
        "latent variables."
      )
    )
  }
  par_y <- lapply(fit_y, parameterEstimates)
  if (!is.null(fit_x)) {
    par_x <- lapply(fit_x, parameterEstimates)
    par_yx <- c(par_y, par_x)
    sel_yx <- c(rep("fit_y", length(par_y)), rep("fit_x", length(par_x)))
    names(sel_yx) <- c(names(par_y), names(par_x))
  } else {
    par_yx <- par_y
    sel_yx <- rep("fit_y", length(par_y))
    names(sel_yx) <- names(par_y)
  }
  mapply(
    par = par_yx, sel = sel_yx,
    function(par, sel) {
      nm <- unique(par$lhs[par$op == "=~"])
      if (sum(par$op == "|") > 0) {
        stop(
          paste0(
            "The '", sel, "' model including '",
            paste(nm, collapse = "' and '"),
            "' includes ordinal variables, which are not currently ",
            "supported in 'sem.cor'."
          )
        )
      }
      var_lhs <- par$lhs[par$op == "=~"]
      var_rhs <- par$rhs[par$op == "=~"]
      sapply(
        var_lhs,
        function(x) {
          if (x %in% var_rhs) {
            stop(
              paste0(
                "The '", sel, "' model including '",
                paste(nm, collapse = "' and '"),
                "' appears to be a hierarchical model (i.e., there is at ",
                "least one variable that is both a latent variable and ",
                "contributes to a latent variable's measurement). ",
                "Hierarchical models are not currently supported in 'sem.cor'."
              )
            )
          }
        }
      )
    }
  )
  nagy_sel <- rep(nagy, length(par_yx))
  names(nagy_sel) <- names(par_yx)
  if (nagy) {
    ord_n <- c()
    cor_n <- c()
    cor_sel <- c()
    for (par_n in names(par_yx)) {
      par <- par_yx[[par_n]]
      sel <- sel_yx[[par_n]]
      nm <- unique(par$lhs[par$op == "=~"])
      if (length(nm) > 1) {
        nagy_sel[par_n] <- FALSE
        warn_cor <- FALSE
        warning(
          paste0(
            " The '", sel, "' model including '",
            paste(nm, collapse = "' and '"),
            "' includes more than one latent variable.\n   ",
            "Measurement models with more than one latent variable are not ",
            "currently supported in 'sem.cor' with 'nagy = TRUE'.\n   ",
            "Models including these variables will be switched to Burt's ",
            "method (i.e., 'nagy = FALSE')."
          )
        )
      } else {
        if (sum(par$op == "~~" & par$lhs != par$rhs & par$est != 0) > 0) {
          warn_cor <- TRUE
          cor_n <- c(cor_n, nm)
          cor_sel <- unique(c(cor_sel, sel))
        } else {
          warn_cor <- FALSE
        }
      }
    }
    if (warn_cor) {
      warning(
        paste0(
          " The '", paste(cor_sel, collapse = " and "),
          "' models including the latent variables listed below include ",
          "at least one correlation between two different variables ",
          "(such as correlated residuals).\n   ",
          "This is not currently supported in 'sem.cor' when ",
          "'nagy = TRUE'.\n   ",
          "The model has been run without them.\n   ",
          "If they are necessary, they are supported with 'nagy = FALSE'.",
          "\n\n      ",
          paste0(cor_n, collapse = "; ")
        )
      )
    }
  }
  if (nagy & sum(nagy_sel) == 0) {
    nagy <- FALSE
    warning(
      paste(
        "All correlations include a measurement model with more than one",
        "latent variable, which are not currently supported with ",
        "'nagy = TRUE'.\n   ",
        "Therefore, 'nagy' has been switch to 'FALSE'."
      )
    )
  }
  if (is.null(fit_x) & is.null(items)) {
    pars <- lapply(
      stats::setNames(
        seq_along(par_y[-length(par_y)]),
        names(par_y)[seq_along(par_y[-length(par_y)])]
      ),
      function(y) {
        lapply(
          stats::setNames(
            (y + 1):length(par_y), names(par_y)[(y + 1):length(par_y)]
          ),
          function(x) {
            x0 <- par_y[[x]]
            y0 <- par_y[[y]]
            list(x0 = x0, y0 = y0)
          }
        )
      }
    )
  }
  if (!is.null(fit_x)) {
    pars <- sapply(
      par_y,
      function(y) {
        sapply(par_x, function(x) list(x0 = x, y0 = y), simplify = FALSE)
      },
      simplify = FALSE
    )
  }
  if ((is.null(fit_x) & is.null(items)) | !is.null(fit_x)) {
    mod_key <- mapply(
      p0 = pars, yn = names(pars), SIMPLIFY = FALSE,
      FUN = function(p0, yn) {
        tmp <- mapply(
          p = p0, xn = names(p0), SIMPLIFY = FALSE,
          FUN = function(p, xn) {
            x <- p[["x0"]]
            y <- p[["y0"]]
            x1 <- x[x$op == "~~" | x$op == "=~", ]
            y1 <- y[y$op == "~~" | y$op == "=~", ]
            xfn <- unique(x1$lhs[x1$op == "=~"])
            yfn <- unique(y1$lhs[y1$op == "=~"])
            key_x <- unique(x1$rhs[x1$op == "=~"])
            key_y <- unique(y1$rhs[y1$op == "=~"])
            key0 <- unique(c(key_y, key_x))
            ns <- sum(nagy_sel[c(xn, yn)]) == 2
            # Any shared items?
            i <- x1$lhs[x1$lhs %in% y1$lhs]
            if (length(i) != 0) {
              if (ns) {
                stop(
                  paste0(
                    "The following item(s) are in both the '", yn,
                    "' and '", xn, "' models.\n  ",
                    "This is not supported when 'nagy = TRUE'.\n\n    ",
                    paste(i, collapse = "; ")
                  )
                )
              }
              if (length(i) >= min(length(key_x), length(key_y))) {
                if (length(key_x) < length(key_y)) {
                  shorter_f <- xn
                  longer_f <- yn
                }
                if (length(key_x) > length(key_y)) {
                  shorter_f <- yn
                  longer_f <- xn
                }
                if (length(key_x) == length(key_y)) {
                  stop(
                    paste0(
                      "All items between '", yn, "' and '", xn, "' are shared.",
                      "\nHave you included the same latent variable twice by ",
                      "mistake?"
                    )
                  )
                }
                stop(
                  paste0(
                    "All the items in '", shorter_f, "' are included in '",
                    longer_f, "'.\n",
                    "Finding correlations between such latent variables does ",
                    "not make sense."
                  )
                )
              }
              warning(
                paste0(
                  "The following item(s) are in both the '", yn, "' and '", xn,
                  "' models.\n   Is this intended?\n\n     ",
                  paste(i, collapse = "; ")
                )
              )
              # Unfix residual variance for shared items
              for (j in i) {
                x1 <- x1[!(x1$lhs == j & x1$op == "~~" & x1$rhs == j), ]
                y1 <- y1[!(y1$lhs == j & y1$op == "~~" & y1$rhs == j), ]
              }
            }
            if (!ns) {
              mod0 <- paste0(
                # CFA1
                paste(x1$lhs, x1$op, x1$est, "*", x1$rhs, collapse = "\n"),
                "\n",
                # CFA2
                paste(y1$lhs, y1$op, y1$est, "*", y1$rhs, collapse = "\n"),
                collapse = "\n"
              )
              return(
                list(
                  mod = mod0, key = key0,
                  xn = xn, yn = yn, xfn = xfn, yfn = yfn, ns = ns
                )
              )
            } else {
              x1l <- x1[x1$op == "=~", ]
              y1l <- y1[y1$op == "=~", ]
              x1u <- x1[x1$op == "~~" & x1$lhs != xn & x1$lhs == x1$rhs, ]
              y1u <- y1[y1$op == "~~" & y1$lhs != yn & y1$lhs == y1$rhs, ]
              x1v <- x1[x1$lhs == x1$rhs & x1$lhs == xn, ]
              y1v <- y1[y1$lhs == y1$rhs & y1$lhs == yn, ]
              mod0 <- paste0(
                # CFA1
                paste0(
                  x1l$lhs, x1l$op, "lx", seq_along(key_x), "*start(", x1l$est,
                  ")*", x1l$rhs,
                  collapse = "\n"
                ),
                ifelse(x1l$est[1] >= 0, "\nlx1>0\n", "\nlx1<0\n"),
                paste0(
                  x1u$lhs, x1u$op, "dx", seq_along(key_x), "*start(", x1u$est,
                  ")*", x1u$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(x1v$lhs, x1v$op, x1v$est, "*", x1v$rhs),
                "\n",
                # CFA2
                paste0(
                  y1l$lhs, y1l$op, "ly", seq_along(key_y), "*start(", y1l$est,
                  ")*", y1l$rhs,
                  collapse = "\n"
                ),
                ifelse(y1l$est[1] >= 0, "\nly1>0\n", "\nly1<0\n"),
                paste0(
                  y1u$lhs, y1u$op, "dy", seq_along(key_y), "*start(", y1u$est,
                  ")*", y1u$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(y1v$lhs, y1v$op, y1v$est, "*", y1v$rhs),
                "\n",
                # Extension parameters
                paste0(
                  mapply(
                    ye = key_y, yes = seq_along(key_y),
                    FUN = function(ye, yes) paste0(ye, "~~pxy", yes, "*", xn)
                  ),
                  collapse = "\n"
                ),
                "\n",
                paste0(
                  mapply(
                    xe = key_x, xes = seq_along(key_x),
                    FUN = function(xe, xes) paste0(xe, "~~pyx", xes, "*", yn)
                  ),
                  collapse = "\n"
                ),
                "\n",
                # Model constraints
                paste0(
                  "0==",
                  paste0(
                    sapply(
                      seq_along(key_y),
                      function(ys) {
                        paste0("ly", ys, "*pxy", ys, "/dy", ys)
                      }
                    ),
                    collapse = "+"
                  )
                ),
                "\n",
                paste0(
                  "0==",
                  paste0(
                    sapply(
                      seq_along(key_x),
                      function(xs) paste0("lx", xs, "*pyx", xs, "/dx", xs)
                    ),
                    collapse = "+"
                  )
                )
              )
              return(
                list(
                  mod = mod0, key = key0,
                  xn = xn, yn = yn, xfn = xfn, yfn = yfn, ns = ns
                )
              )
            }
          }
        )
        mod1 <- lapply(tmp, function(x) x$mod)
        key1 <- lapply(tmp, function(x) x$key)
        xn <- sapply(tmp, function(x) x$xn)
        yn <- sapply(tmp, function(x) x$yn)
        xfn <- lapply(tmp, function(x) x$xfn)
        yfn <- lapply(tmp, function(x) x$yfn)
        ns <- sapply(tmp, function(x) x$ns)
        return(
          list(
            mod = mod1, key = key1,
            xn = xn, yn = yn, xfn = xfn, yfn = yfn, ns = ns
          )
        )
      }
    )
    mods <- unlist(lapply(mod_key, function(x) x$mod), recursive = FALSE)
    key <- unlist(lapply(mod_key, function(x) x$key), recursive = FALSE)
    xn <- unlist(lapply(mod_key, function(x) x$xn), recursive = FALSE)
    yn <- unlist(lapply(mod_key, function(x) x$yn), recursive = FALSE)
    xfn <- unlist(lapply(mod_key, function(x) x$xfn), recursive = FALSE)
    yfn <- unlist(lapply(mod_key, function(x) x$yfn), recursive = FALSE)
    ns <- unlist(lapply(mod_key, function(x) x$ns), recursive = FALSE)
  } else {
    mods <- NULL
    key <- NULL
  }
  if (!is.null(items)) {
    # Correlations with single items
    mod_key_i <- mapply(
      y = par_y, ns = nagy_sel[names(fit_y)], SIMPLIFY = FALSE,
      FUN = function(y, ns) {
        y1 <- y[y$op == "=~" | (y$op == "~~" & y$lhs == y$rhs), ]
        yn <- unique(y$lhs[y$op == "=~"])
        yfn <- unique(y1$lhs[y1$op == "=~"])
        if (ns) {
          y1l <- y1[y1$op == "=~", ]
          y1u <- y1[y1$op == "~~" & y1$lhs != yn, ]
          y1v <- y1[y1$lhs == y1$rhs & y1$lhs == yn, ]
          key_y <- unique(y1$rhs[y1$op == "=~"])
        }
        item_overlap <- items[items %in% y1$rhs]
        if (length(item_overlap) > 0) {
          stop(
            paste0(
              "The following item(s) are in both 'items' and contributes ",
              "to the measurement of the '", yn, "' latent variable.\n  ",
              "This is not supported.\n  ",
              "Either remove the item(s) from 'items' or ",
              "(if appropriate) remove the item(s) from the latent ",
              "measurement of '", yn, "'.\n    ",
              paste(item_overlap, collapse = "\n    ")
            )
          )
        }
        if (length(item_loadings) == length(items)) {
          names(item_loadings) <- items
        }
        tmp <- lapply(
          stats::setNames(nm = items),
          function(i) {
            if (!is.null(item_loadings)) {
              if (length(item_loadings) == length(items)) {
                i_r <- paste(item_loadings[i], " * ")
              } else {
                i_r <- paste(item_loadings, " * ")
              }
            } else {
              i_r <- ""
            }
            i_l <- paste0(i, "_l")
            key0 <- c(y$rhs[y$op == "=~" & !y$rhs %in% unlist(yfn)], i)
            if (!ns) {
              mod0 <- paste0(
                # CFA
                paste(y1$lhs, y1$op, y1$est, "*", y1$rhs, collapse = "\n"),
                "\n",
                paste0(i_l, " =~ ", i_r, i),
                collapse = "\n"
              )
            } else {
              mod0 <- paste0(
                # CFA
                paste0(
                  y1l$lhs, y1l$op, "ly", seq_along(key_y), "*start(", y1l$est,
                  ")*", y1l$rhs,
                  collapse = "\n"
                ),
                ifelse(y1l$est[1] >= 0, "\nly1>0\n", "\nly1<0\n"),
                paste0(
                  y1u$lhs, y1u$op, "dy", seq_along(key_y), "*start(", y1u$est,
                  ")*", y1u$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(y1v$lhs, y1v$op, y1v$est, "*", y1v$rhs),
                "\n",
                # Item latent variable
                paste0(i_l, "=~", i_r, i),
                "\n",
                # Correlation
                paste0(yn, "~~", paste0(i_l, collapse = "+")),
                "\n",
                # Extension parameters
                paste0(
                  mapply(
                    ye = key_y, yes = seq_along(key_y),
                    FUN = function(ye, yes) paste0(ye, "~~piy", yes, "*", i_l)
                  ),
                  collapse = "\n"
                ),
                "\n",
                # Model constraints
                paste0(
                  "0==",
                  paste0(
                    sapply(
                      seq_along(key_y),
                      function(ys) paste0("ly", ys, "*piy", ys, "/dy", ys)
                    ),
                    collapse = "+"
                  )
                ),
                collapse = "\n"
              )
            }
            return(list(mod = mod0, key = key0))
          }
        )
        mod1 <- lapply(tmp, function(x) x$mod)
        key1 <- lapply(tmp, function(x) x$key)
        return(
          list(
            mod = mod1, key = key1,
            yfn = rep(yfn, each = length(items)),
            ns = rep(ns, each = length(items))
          )
        )
      }
    )
    mods_i <- unlist(lapply(mod_key_i, function(x) x$mod), recursive = FALSE)
    key_i <- unlist(lapply(mod_key_i, function(x) x$key), recursive = FALSE)
    yfn_i <- unlist(lapply(mod_key_i, function(x) x$yfn), recursive = FALSE)
    ns_i <- unlist(lapply(mod_key_i, function(x) x$ns), recursive = FALSE)
    if (is.null(mods)) {
      mods <- mods_i
      key <- key_i
    } else {
      mods <- c(mods, mods_i)
      key <- c(key, key_i)
    }
  }
  fit <- sem.check(
    mods,
    data,
    name = name,
    keys_s = key,
    fit_save = fit_save,
    fit_measures = fit_measures,
    miss = miss,
    est = est,
    check = check,
    save_out = save_out,
    std.lv = TRUE,
    ordered = NULL
  )
  extract <- c("est.std", "ci.lower", "ci.upper", "pvalue")
  if (!is.null(fit_x) | (length(fit_y) > 1 & is.null(items))) {
    cors_y <- sapply(
      extract,
      function(ext) {
        if (!is.null(items)) {
          sel <- !grepl(
            paste0("\\.", items, "$", collapse = "|"), names(fit$par_std)
          )
        } else {
          sel <- names(fit$par_std)
        }
        tmp <- do.call(
          rbind,
          mapply(
            x = fit$par_std[sel],
            xn0 = xfn,
            yn0 = yfn,
            FUN = function(x, xn0, yn0) {
              selr <- x$lhs %in% c(xn0, yn0) & x$rhs %in% c(xn0, yn0)
              selc <- c("lhs", "rhs", ext)
              x[selr, selc]
            },
            SIMPLIFY = FALSE
          )
        )
        tmp[!duplicated(tmp), ]
      },
      simplify = FALSE
    )
    if (is.null(fit_x)) {
      cor_mat_y <- sapply(
        extract,
        function(ext_n) {
          ext <- cors_y[[ext_n]]
          sapply(
            stats::setNames(nm = unique(unlist(c(yfn, xfn)))),
            function(x) {
              sapply(
                stats::setNames(nm = unique(unlist(c(yfn, xfn)))),
                function(y) {
                  sel <- (ext$lhs == x & ext$rhs == y) |
                    (ext$lhs == y & ext$rhs == x)
                  ext[[ext_n]][sel]
                }
              )
            }
          )
        },
        simplify = FALSE
      )
    } else {
      cor_mat_y <- sapply(
        extract,
        function(ext_n) {
          ext <- cors_y[[ext_n]]
          tmp <- sapply(
            stats::setNames(nm = unique(unlist(yfn))),
            function(y) {
              sapply(
                stats::setNames(nm = unique(unlist(xfn))),
                function(x) {
                  sel <- (ext$lhs == x & ext$rhs == y) |
                    (ext$lhs == y & ext$rhs == x)
                  ext[[ext_n]][sel]
                }
              )
            }
          )
          if (is.vector(tmp) & length(unique(unlist(xfn))) == 1) {
            tmp <- matrix(tmp, nrow = 1)
            colnames(tmp) <- unique(unlist(yfn))
            rownames(tmp) <- unique(unlist(xfn))
          }
          return(tmp)
        },
        simplify = FALSE
      )
    }
  }
  if (!is.null(items)) {
    cor_mat_yi <- sapply(
      extract,
      function(ext) {
        tmp <- sapply(
          stats::setNames(nm = unique(unlist(yfn_i))),
          function(y) {
            sapply(
              stats::setNames(nm = items),
              function(i) {
                ptn <- paste0("^", y, "\\.", i, "$", collapse = "|")
                x <- fit$par_std[grepl(ptn, names(fit$par_std))][[1]]
                sel <- x$lhs != x$rhs & x$op == "~~" &
                  grepl(y, x$lhs) & grepl(paste0(i, "_l"), x$rhs)
                x[[ext]][sel]
              }
            )
          }
        )
        if (is.vector(tmp)) {
          if (length(items) == 1) {
            tmp <- matrix(tmp, nrow = 1)
          } else {
            tmp <- matrix(tmp, ncol = 1)
          }
          rownames(tmp) <- items
          colnames(tmp) <- yfn_i
        }
        return(tmp)
      },
      simplify = FALSE
    )
  }
  if (is.null(fit_x) & is.null(items)) {
    if (!is.positive.definite(cor_mat_y$est.std)) {
      cor_mat_y0 <- as.matrix(nearPD(cor_mat_y$est.std, corr = TRUE)$mat)
      dif_mat <- cor_mat_y$est.std - cor_mat_y0
      cor_mat_y$est.std <- cor_mat_y0
      cor_mat_y$ci.lower <- cor_mat_y$ci.lower - dif_mat
      cor_mat_y$ci.upper <- cor_mat_y$ci.upper - dif_mat
      max_adj <- round(max(abs(dif_mat)), 3)
      max_adj <- ifelse(
        max_adj == 0, "< .001", format(max_adj, scientific = FALSE)
      )
      warning(
        paste0(
          "The correlation matrix between 'fit_y' constructs has been adjusted",
          " from initial estimates with the 'Matrix::nearPD' function ",
          "to ensure it is positive definite.\n  ",
          "The maximum adjustment to any cell was ", max_adj, ".\n  ",
          "'ci_lower' and 'ci_upper' were adjusted by the same absolute amount",
          " as the primary correlation matrix.\n  ",
          "p-values were not adjusted. In most cases, they would be extremely ",
          "similar, but if the maximum adjustment noted above is large, use ",
          "with caution."
        )
      )
    }
  }
  if (is.null(items)) {
    cor_mat <- cor_mat_y$est.std
    ci_lower <- cor_mat_y$ci.lower
    ci_upper <- cor_mat_y$ci.upper
    pvalue_mat <- cor_mat_y$pvalue
  } else if (is.null(fit_x)) {
    cor_mat <- cor_mat_yi$est.std
    ci_lower <- cor_mat_yi$ci.lower
    ci_upper <- cor_mat_yi$ci.upper
    pvalue_mat <- cor_mat_yi$pvalue
  } else {
    cor_mat <- rbind(cor_mat_y$est.std, cor_mat_yi$est.std)
    ci_lower <- rbind(cor_mat_y$ci.lower, cor_mat_yi$ci.lower)
    ci_upper <- rbind(cor_mat_y$ci.upper, cor_mat_yi$ci.upper)
    pvalue_mat <- rbind(cor_mat_y$pvalue, cor_mat_yi$pvalue)
  }
  if (nagy) {
    if ((length(fit_y) > 1 & is.null(items)) | !is.null(fit_x)) {
      if (!is.null(items)) {
        ns2 <- c(ns, ns_i)
      } else {
        ns2 <- ns
      }
    } else {
      ns2 <- ns_i
    }
    nagy_par <- fit$par_std[ns2]
    rcy0 <- sapply(
      extract,
      function(ext) {
        sapply(
          names(fit_y)[nagy_sel[names(fit_y)]],
          function(y0) {
            ptn <- paste0("^", y0, "\\.|\\.", y0, "$")
            y <- fit$par_std[ns2][grep(ptn, names(fit$par_std[ns2]))]
            do.call(
              cbind,
              lapply(
                y,
                function(y1) {
                  y2 <- y1[grepl("p((x|i)y|yx)", y1$label) & y1$lhs != y0, ]
                  y3 <- data.frame(y2[[ext]], row.names = y2$rhs)
                  names(y3) <- y2$lhs[[1]]
                  y3
                }
              )
            )
          },
          simplify = FALSE
        )
      },
      simplify = FALSE
    )
    if (!is.null(fit_x) | !is.null(items)) {
      rcy <- sapply(
        rcy0,
        function(rcy1) {
          names(rcy1) <- NULL
          do.call(rbind, rcy1)
        },
        simplify = FALSE
      )
    } else {
      rcy <- sapply(
        rcy0,
        function(rcy1) {
          rcy2 <- do.call(
            cbind,
            lapply(
              names(rcy1),
              function(y) {
                y1 <- rcy1[names(rcy1) != y]
                names(y1) <- rep("", length(y1))
                y2 <- do.call(rbind, lapply(y1, function(x) x[names(x) == y]))
                y3 <- y2[unique(unlist(key)), , drop = FALSE]
                rownames(y3) <- unique(unlist(key))
                y3
              }
            )
          )
          # Don't include items only in Burt models
          rcy2[rowSums(!is.na(rcy2)) != 0, ]
        },
        simplify = FALSE
      )
    }
    if (!is.null(fit_x)) {
      rcx <- sapply(
        extract,
        function(ext) {
          rcx1 <- sapply(
            names(fit_x)[nagy_sel[names(fit_x)]],
            function(x0) {
              ptn <- paste0("^", x0, "\\.|\\.", x0, "$")
              x <- fit$par_std[ns2][grep(ptn, names(fit$par_std[ns2]))]
              do.call(
                cbind,
                lapply(
                  x,
                  function(x1) {
                    x2 <- x1[grepl("pyx", x1$label), ]
                    x3 <- data.frame(x2[[ext]], row.names = x2$rhs)
                    names(x3) <- x2$lhs[[1]]
                    x3
                  }
                )
              )
            },
            simplify = FALSE
          )
          names(rcx1) <- NULL
          do.call(rbind, rcx1)
        },
        simplify = FALSE
      )
      if (fit_save) {
        return(
          list(
            fit = fit$fit,
            cor_mat = cor_mat, pvalues = pvalue_mat,
            ci = list(ci_lower = ci_lower, ci_upper = ci_upper),
            fit_measures = fit$fit_measures,
            residual_cors_y = rcy,
            residual_cors_x = rcx
          )
        )
      } else {
        return(
          list(
            fit = fit$fit,
            cor_mat = cor_mat, pvalues = pvalue_mat,
            ci = list(ci_lower = ci_lower, ci_upper = ci_upper),
            residual_cors_y = rcy,
            residual_cors_x = rcx
          )
        )
      }
    }
    if (fit_save) {
      return(
        list(
          fit = fit$fit,
          cor_mat = cor_mat, pvalues = pvalue_mat,
          ci = list(ci_lower = ci_lower, ci_upper = ci_upper),
          fit_measures = fit$fit_measures,
          residual_cors = rcy
        )
      )
    } else {
      return(
        list(
          fit = fit$fit,
          cor_mat = cor_mat, pvalues = pvalue_mat,
          ci = list(ci_lower = ci_lower, ci_upper = ci_upper),
          residual_cors = rcy
        )
      )
    }
  }
  if (fit_save) {
    return(
      list(
        fit = fit$fit,
        cor_mat = cor_mat, pvalues = pvalue_mat,
        ci = list(ci_lower = ci_lower, ci_upper = ci_upper),
        fit_measures = fit$fit_measures
      )
    )
  } else {
    return(
      list(
        fit = fit$fit,
        cor_mat = cor_mat, pvalues = pvalue_mat,
        ci = list(ci_lower = ci_lower, ci_upper = ci_upper)
      )
    )
  }
}
