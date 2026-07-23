#' Creates a latent variable correlation matrix from fitted lavaan measurement
#' models
#'
#' sem.cor takes CFA outputs and produces a correlation matrix between latent
#' variables and/or single items.
#'
#' @inheritParams sem.check
#' @param fit_y A named list of CFA fitted objects.
#' @param fit_x
#' A named list of CFA fitted objects to be correlated with fit_y variables
#' or 'NULL'.
#' @param items
#' A vector of single-item variables to correlate with 'fit_y' latent variables.
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
#' Returns a list of length 4.
#' The first element is the fitted lavaan models ('fit').
#' The second element is the correlation matrix ('cor_mat').
#' The third and fourth elements are the upper and lower confidence intervals
#' from the models ('ci_lower' and 'ci_upper', respectively).
#'
#' @details
#' The function computes correlations between latent variables from fitted CFA
#' models, including either all latent variables, or between two sets of latent
#' variables.
#' To compute a full correlation matrix include all CFA models in a list as
#' `fit_y`, alternatively,
#' to correlate a set of dependant variables with a set of independent
#' variables,
#' set `fit_y` as a list of fitted CFA models of dependant variables and
#' set `fit_x` as a list of fitted CFA models of independent variables.
#' Correlations between the `fit_y` latent variables and single items can also
#' be optionally included by setting `items` as a vector of item names.
#' In this case, items will be treated as single item latent variables with
#' reliability = `item_loadings`.
#'
#' Each correlation is calculated in a separate model.
#' This approach saves time for longer lists of variables compared to including
#' everything in one model and
#' it also means that excluding a variable cannot change correlations between
#' other variables (which is possible when all are included together).
#' The function uses either Burt's 2-stage procedure when `nagy = FALSE` or
#' Nagy and colleagues' (2017) extension procedure when `nagy = TRUE`
#' to control for interpretational confounding (Burt, 1976).
#'
#' Burt's method works by fixing measurement model parameters in the model
#' estimating structural parameters.
#' This method means that less than ideal fit at the measurement level does not
#' propagate though the model as the measurement parameters are fixed.
#' It also means that the latent variables interpretation cannot change with the
#' addition of different variables,
#' thereby solving interpretational confounding.
#' However, it is not a perfect solution because it underestimates uncertainty
#' in the measurement part of the structural model (e.g., Nagy et al., 2017),
#' which results in biased standard errors and fit statistics.
#'
#' Alternatively, Nagy's method involves allowing item residuals to correlate
#' with external variables (or factors) and constrains those relationships
#' such that the model is identifiable.
#' If the sums of squares of correlations between all combinations of factors'
#' items and external factors are minimised,
#' measurement parameters in the structural model match those of isolated
#' measurement models without having to constrain them directly.
#' As a result, unbiased standard errors are preserved while simultaneously
#' eliminating interpretational confounding.
#'
#' Burt's method is faster but artificially constrains parameters,
#' thereby biasing standard errors and model fit indices.
#' They should, however, give very similar point estimates for correlations.
#' Therefore, Nagy's method should be preferred whenever confidence intervals
#' or model fit matter, except for extremely large sets of variables.
#'
#' It is possible for latent variable correlations to produce a non-positive
#' definite correlation matrix between variables included in `fit_y`,
#' especially when closely related factors are included.
#' If the matrix of latent variables is not positive definite,
#' then the matrix will be adjusted to the nearest positive definite
#' matrix using the `[Matrix::nearPD()]` function,
#' which employs the method developed by Higham (2002).
#'
#' The model relies on [sem.check()] for the back-end of running the models,
#' which enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' In the case of `sem.cor()`, the number of correlations can add up quickly,
#' so the functionality may be useful
#' (e.g., with 20 scales there are 19 + 18 + 17 + ... + 1 = 190 correlations).
#' For further details on how this works, see the [sem.check()] function
#' documentation.
#'
#' @seealso
#' [sem.check()], which `sem.cor()` uses for all the back-end;
#' [lavaan::sem()], which is used to estimate the models;
#' [matrixcalc::is.positive.definite], which is used to assess whether the
#' correlation matrix between `fit_y` constructs is positive definite; and
#' [Matrix::nearPD()] for the function used to transform non-positive definite
#' matrices into positive definite matrices.
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
#' Journal of Experimental Education, 85(4).
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
    data, fit_y, fit_x = NULL, items = NULL,  item_loadings = NULL, nagy = TRUE,
    fit_save = FALSE, fit_measures = "all", miss = "ML", est = "default",
    name = "cors", check = FALSE, save_out = FALSE
) {
  # Single model instead of list.
  if (!is.list(fit_y) & inherits(fit_y, "lavaan")) {
    fit_y <- list(factor = fit_y)
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
      fit_x <- list(factor = fit_x)
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
  if (length(fit_y) <= 1) {
    if (is.null(items) & is.null(fit_x)) {
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
  }
  par1 <- lapply(fit_y, parameterEstimates)
  # Rename y objects to match factors
  names(fit_y) <- names(par1) <- sapply(
    par1,
    function(y) {
      y1 <- y[y$op %in% c("~~", "=~"), ]
      yn <- unique(y$lhs[y$op == "=~"])
      if (length(yn) > 1) {
        stop(
          paste0(
            "The 'fit_y' model including '", paste(yn, collapse = "' and '"),
            "' includes more than one latent variable. ",
            "These models are not currently supported by 'sem.cor()'."
          )
        )
      } else {
        yn
      }
    }
  )
  if (is.null(fit_x) & length(fit_y) >= 2) {
    pars <-lapply(
      stats::setNames(
        seq_along(par1[-length(par1)]),
        names(par1)[seq_along(par1[-length(par1)])]
      ),
      function(y) {
        lapply(
          stats::setNames(
            (y + 1):length(par1), names(par1)[(y + 1):length(par1)]
          ),
          function(x) {
            x0 <- par1[[x]]
            y0 <- par1[[y]]
            list(x0 = x0, y0 = y0)
          }
        )
      }
    )
  } else if (!is.null(fit_x)) {
    # Correlations between constructs in fit_y and constructs in fit_x
    par2 <- lapply(fit_x, parameterEstimates)
    names(fit_x) <- names(par2) <- sapply(
      par2,
      function(x) {
        x1 <- x[x$op %in% c("~~", "=~"), ]
        xn <- unique(x$lhs[x$op == "=~"])
        if (length(xn) > 1) {
          stop(
            paste0(
              "The 'fit_x' model including '", paste(xn, collapse = "' and '"),
              "' includes more than one latent variable. ",
              "These models are not currently supported by 'sem.cor()'. ",
              "Please include them separately."
            )
          )
        } else {
          xn
        }
      }
    )
    pars <- sapply(
      par1,
      function(y) {
        sapply(par2, function(x) list(x0 = x, y0 = y), simplify = FALSE)
      },
      simplify = FALSE
    )
  }
  if ((is.null(fit_x) & length(fit_y) >= 2) | !is.null(fit_x)) {
    mod_key <- lapply(
      pars,
      function(k) {
        tmp <- lapply(
          k,
          function(j) {
            x <- j[["x0"]]
            y <- j[["y0"]]
            x1 <- x[x$op == "~~" | x$op == "=~", ]
            y1 <- y[y$op == "~~" | y$op == "=~", ]
            key_x <- unique(x1$rhs[x1$op == "=~"])
            key_y <- unique(y1$rhs[y1$op == "=~"])
            key0 <- unique(c(key_x, key_y))
            xn <- unique(x1$lhs[x1$op == "=~"])
            yn <- unique(y1$lhs[y1$op == "=~"])
            # Any shared items. Need to unfix residual variance for these.
            i <- x1$lhs[x1$lhs %in% y1$lhs]
            if (length(i) != 0) {
              if (nagy) {
                stop(
                  paste0(
                    "The following item(s) are in both the '", yn, "' and '",
                    xn, "' factors. ",
                    "This is not supported when 'nagy = TRUE'.\n    ",
                    paste(i, collapse = "\n    ")
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
                    longer_f, "'.\n  ",
                    "Such a relationship should be specified as a bifactor ",
                    "model with a correlation of 0 between the group and ",
                    "general factors."
                  )
                )
              }
              warning(
                paste0(
                  "The following item(s) are in both the '", yn, "' and '", xn,
                  "' factors. ",
                  "If this is not intended, please correct it and disregard ",
                  "the correlation between these factors.\n    ",
                  paste(i, collapse = "\n    ")
                )
              )
              for (j in i) {
                x1 <- x1[!(x1$lhs == j & x1$op == "~~" & x1$rhs == j), ]
                y1 <- y1[!(y1$lhs == j & y1$op == "~~" & y1$rhs == j), ]
              }
            }
            if (!nagy) {
              mod0 <- paste0(
                # CFA1
                paste(x1$lhs, x1$op, x1$est, "*", x1$rhs, collapse = "\n"),
                "\n",
                # CFA2
                paste(y1$lhs, y1$op, y1$est, "*", y1$rhs, collapse = "\n"),
                collapse = "\n"
              )
              return(list(mod = mod0, key = key0))
            } else {
              x1l <- x1[x1$op == "=~", ]
              y1l <- y1[y1$op == "=~", ]
              x1u <- x1[x1$op == "~~" & x1$lhs != xn, ]
              y1u <- y1[y1$op == "~~" & y1$lhs != yn, ]
              x1v <- x1[x1$lhs == x1$rhs & x1$lhs == xn, ]
              y1v <- y1[y1$lhs == y1$rhs & y1$lhs == yn, ]
              mod0 <- paste0(
                # CFA1
                paste0(
                  x1l$lhs, x1l$op, "lx", seq_along(key_x), "*", x1l$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(
                  x1u$lhs, x1u$op, "dx", seq_along(key_x), "*", x1u$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(x1v$lhs, x1v$op, x1v$est, "*", x1v$rhs),
                "\n",
                # CFA2
                paste0(
                  y1l$lhs, y1l$op, "ly", seq_along(key_y), "*", y1l$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(
                  y1u$lhs, y1u$op, "dy", seq_along(key_y), "*", y1u$rhs,
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
              return(list(mod = mod0, key = key0))
            }
          }
        )
        mod1 <- lapply(tmp, function(x) x$mod)
        key1 <- lapply(tmp, function(x) x$key)
        return(list(mod = mod1, key = key1))
      }
    )
    mods <- unlist(lapply(mod_key, function(x) x$mod), recursive = FALSE)
    key <- unlist(lapply(mod_key, function(x) x$key), recursive = FALSE)
  } else {
    mods <- NULL
    key <- NULL
  }
  if (!is.null(items)) {
    # Correlations with single items
    mod_key_i <- lapply(
      par1,
      function(y) {
        y1 <- y[y$op %in% c("~~", "=~"), ]
        yn <- unique(y$lhs[y$op == "=~"])
        if (nagy) {
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
              "to the measurement of the '", yn, "' latent variable.",
              "\n  This is not supported.\n  ",
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
            key0 <- c(y$rhs[y$op == "=~"], i)
            if (!nagy) {
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
                  y1l$lhs, y1l$op, "ly", seq_along(key_y), "*", y1l$rhs,
                  collapse = "\n"
                ),
                "\n",
                paste0(
                  y1u$lhs, y1u$op, "dy", seq_along(key_y), "*", y1u$rhs,
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
        return(list(mod = mod1, key = key1))
      }
    )
    mods_i <- unlist(lapply(mod_key_i, function(x) x$mod), recursive = FALSE)
    key_i <- unlist(lapply(mod_key_i, function(x) x$key), recursive = FALSE)
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
    std.lv = TRUE
  )
  if (!is.null(fit_x) | length(fit_y) > 1) {
    xn0 <- sub(".*\\.", "", names(mods))
    xn <- xn0[!xn0 %in% items]
    yn <- sub("\\..*", "", names(mods))[!xn0 %in% items]
    cors_y <- mapply(
      x = fit$par_std[
        !grepl(paste0("\\.", items, "$", collapse = "|"), names(fit$par_std))
      ],
      xn0 = xn,
      yn0 = yn,
      FUN = function(x, xn0, yn0) x$est.std[x$lhs == xn0 & x$rhs == yn0]
    )
    ci_lower_y0 <- mapply(
      x = fit$par_std[
        !grepl(paste0("\\.", items, "$", collapse = "|"), names(fit$par_std))
      ],
      xn0 = xn,
      yn0 = yn,
      FUN = function(x, xn0, yn0) x$ci.lower[x$lhs == xn0 & x$rhs == yn0]
    )
    ci_upper_y0 <- mapply(
      x = fit$par_std[
        !grepl(paste0("\\.", items, "$", collapse = "|"), names(fit$par_std))
      ],
      xn0 = xn,
      yn0 = yn,
      FUN = function(x, xn0, yn0) x$ci.upper[x$lhs == xn0 & x$rhs == yn0]
    )
    if (is.null(fit_x)) {
      cor_mat_y <- sapply(
        names(fit_y),
        function(x) {
          sapply(
            names(fit_y),
            function(y) {
              ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
              tmp <- cors_y[grep(ptn, names(cors_y))]
              if (length(tmp) == 0) {
                tmp <- 1
              }
              tmp
            },
            USE.NAMES = FALSE
          )
        }
      )
      rownames(cor_mat_y) <- names(fit_y)
      ci_lower_y <- sapply(
        names(fit_y),
        function(x) {
          sapply(
            names(fit_y),
            function(y) {
              ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
              tmp <- ci_lower_y0[grep(ptn, names(ci_lower_y0))]
              if (length(tmp) == 0) {
                tmp <- 1
              }
              tmp
            },
            USE.NAMES = FALSE
          )
        }
      )
      rownames(ci_lower_y) <- names(fit_y)
      ci_upper_y <- sapply(
        names(fit_y),
        function(x) {
          sapply(
            names(fit_y),
            function(y) {
              ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
              tmp <- ci_upper_y0[grep(ptn, names(ci_upper_y0))]
              if (length(tmp) == 0) {
                tmp <- 1
              }
              tmp
            },
            USE.NAMES = FALSE
          )
        }
      )
      rownames(ci_upper_y) <- names(fit_y)
    } else {
      cor_mat_y <- sapply(
        names(fit_y),
        function(y) {
          sapply(
            names(fit_x),
            function(x) {
              ptn <- c(paste0(x, ".", y), paste0(y, ".", x))
              cors_y[names(cors_y) %in% ptn]
            },
            USE.NAMES = FALSE
          )
        }
      )
      ci_lower_y <- sapply(
        names(fit_y),
        function(y) {
          sapply(
            names(fit_x),
            function(x) {
              ptn <- c(paste0(x, ".", y), paste0(y, ".", x))
              ci_lower_y0[names(ci_lower_y0) %in% ptn]
            },
            USE.NAMES = FALSE
          )
        }
      )
      ci_upper_y <- sapply(
        names(fit_y),
        function(y) {
          sapply(
            names(fit_x),
            function(x) {
              ptn <- c(paste0(x, ".", y), paste0(y, ".", x))
              ci_upper_y0[names(ci_upper_y0) %in% ptn]
            },
            USE.NAMES = FALSE
          )
        }
      )
      if (is.vector(cor_mat_y) & length(fit_x) == 1) {
        cor_mat_y <- matrix(cor_mat_y, nrow = 1)
        colnames(cor_mat_y) <- names(fit_y)
        ci_lower_y <- matrix(ci_lower_y, nrow = 1)
        colnames(ci_lower_y) <- names(fit_y)
        ci_upper_y <- matrix(ci_upper_y, nrow = 1)
        colnames(ci_upper_y) <- names(fit_y)
      }
      rownames(cor_mat_y) <- names(fit_x)
      rownames(ci_lower_y) <- names(fit_x)
      rownames(ci_upper_y) <- names(fit_x)
    }
  }
  if (!is.null(items)) {
    cors_yi <- do.call(
      rbind,
      mapply(
        x =
          fit$par_std[grepl(paste0(items, collapse = "|"), names(fit$par_std))],
        y = rep(names(fit_y), each = length(items)),
        SIMPLIFY = FALSE,
        FUN = function(x, y) {
          x[
            x$lhs != x$rhs & x$op == "~~" & grepl(y, x$lhs),
            c("lhs", "rhs", "est.std", "ci.lower", "ci.upper")
          ]
        }
      )
    )
    items_l <- paste0(items, "_l")
    cor_mat_yi <- sapply(
      names(fit_y),
      function(x) {
        sapply(
          items_l,
          function(y) cors_yi$est.std[cors_yi$lhs == x & cors_yi$rhs == y]
        )
      }
    )
    ci_lower_yi <- sapply(
      names(fit_y),
      function(x) {
        sapply(
          items_l,
          function(y) cors_yi$ci.lower[cors_yi$lhs == x & cors_yi$rhs == y]
        )
      }
    )
    ci_upper_yi <- sapply(
      names(fit_y),
      function(x) {
        sapply(
          items_l,
          function(y) cors_yi$ci.upper[cors_yi$lhs == x & cors_yi$rhs == y]
        )
      }
    )
    if (is.vector(cor_mat_yi) & length(items) == 1) {
      cor_mat_yi <- matrix(cor_mat_yi, nrow = 1)
      colnames(cor_mat_yi) <- names(fit_y)
      ci_lower_yi <- matrix(ci_lower_yi, nrow = 1)
      colnames(ci_lower_yi) <- names(fit_y)
      ci_upper_yi <- matrix(ci_upper_yi, nrow = 1)
      colnames(ci_upper_yi) <- names(fit_y)
    }
    rownames(cor_mat_yi) <- items
    rownames(ci_lower_yi) <- items
    rownames(ci_upper_yi) <- items
  }
  if (is.null(fit_x) & length(fit_y) > 1) {
    if (!is.positive.definite(cor_mat_y)) {
      cor_mat_y0 <- as.matrix(nearPD(cor_mat_y, corr = TRUE)$mat)
      dif_mat <- cor_mat_y - cor_mat_y0
      cor_mat_y <- cor_mat_y0
      ci_lower_y <- ci_lower_y - dif_mat
      ci_upper_y <- ci_upper_y - dif_mat
      max_adj <- round(max(abs(dif_mat)), 3)
      max_adj <- ifelse(
        max_adj == 0, "< .001", format(max_adj, scientific = FALSE)
      )
      warning(
        paste0(
          "The correlation matrix between 'fit_y' constructs has been adjusted",
          " from initial estimates with the 'Matrix::nearPD()' function ",
          "to ensure it is positive definite.\n  ",
          "The maximum adjustment to any cell was ", max_adj, ".\n  ",
          "'ci_lower' and 'ci_upper' were adjusted by the same absolute amount",
          " as the primary correlation matrix."
        )
      )
    }
  }
  if (is.null(items)) {
    cor_mat <- cor_mat_y
    ci_lower <- ci_lower_y
    ci_upper <- ci_upper_y
  } else if (is.null(fit_x) & length(fit_y) == 1) {
    cor_mat <- cor_mat_yi
    ci_lower <- ci_lower_yi
    ci_upper <- ci_upper_yi
  } else {
    cor_mat <- rbind(cor_mat_y, cor_mat_yi)
    ci_lower <- rbind(ci_lower_y, ci_lower_yi)
    ci_upper <- rbind(ci_upper_y, ci_upper_yi)
  }
  if (fit_save) {
    return(
      list(
        fit = fit$fit, fit_measures = fit$fit_measures,
        cor_mat = cor_mat, ci_lower = ci_lower, ci_upper = ci_upper
      )
    )
  } else {
    return(
      list(
        fit = fit$fit,
        cor_mat = cor_mat, ci_lower = ci_lower, ci_upper = ci_upper
      )
    )
  }
}
