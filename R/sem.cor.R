#' Creates a latent variable correlation matrix from fitted lavaan measurement
#' models
#'
#' sem.cor takes CFA outputs and produces a latent variable correlation matrix
#' including all variables.
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
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "cors".
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#'
#' @return
#' Returns a list of length 4.
#' The first element is the fitted lavaan models ('fit').
#' The second element is the correlation matrix ('cor_mat').
#' The third and fourth elements are the upper and lower confidence intervals
#' from the models ('ci_lower' and 'ci_upper', respectively).
#'
#' @details
#' The function allows for all correlations between latent variables to be
#' calculated by including all fitted CFA models in `fit_y`.
#' Alternatively, if you only want to correlated a set of depenedant variables
#' with a set of independent variables, then use `fit_y` for the dependent
#' variables and `fit_x` for the independent variables.
#' Correlations between the y latent variables and items can also be optionally
#' included by including `items` as a vector of item names.
#' Thus, if `fit_x = NULL`, all `fit_y` latent variables will be correlated with
#' all other `fit_y` latent variables;
#' if `fit_x` is a list of fitted CFA objects, then `fit_y` latent variables
#' will be correlated with `fit_x` latent variables only; and
#' if `items` is a list of items, then all items will be correlated with all
#' `fit_y` latent variables.
#'
#' Each correlation between latent variables is calculated in a
#' separate model and correlations with items are calculated in a separate
#' model for each latent variable.
#' This approach saves time for longer lists of variables compared to including
#' everything in one model and
#' it also means that excluding a variable cannot change correlations between
#' other variables (which is possible when all are included together).
#' The function uses Burt's 2-stage procedure to control for interpretational
#' confounding.
#'
#' If the matrix of latent variables included in `fit_y` is not positive
#' definite, then the matrix will be adjusted to the nearest positive definite
#' matrix using the `[Matrix::nearPD()]` function, which employs the method
#' developed by Higham (2002).
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
#' @examples
#' # Create CFA keys
#' keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#' keys <- sapply(
#'   keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' # Run CFA models
#' cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = TRUE)
#' # Find correlations between all cfa_fit constructs.
#' cors <- sem.cor(BFIGritHope, cfa_fit$fit)
#' # View the correlation matrix
#' cors$cor_mat
#'
#' # Correlations of grit facets with hope facets and the first item from each
#' # Big Five factor.
#' items <- names(BFIGritHope)[grep("bfi_.*1_1", names(BFIGritHope))]
#' cors2 <- sem.cor(BFIGritHope, cfa_fit$fit[1:2], cfa_fit$fit[3:4], items)
#' # View correlations
#' cors2$cor_mat

# TODO: Add Nagy.

sem.cor <- function(
    data, fit_y, fit_x = NULL, items = NULL, miss = "ML", est = "default",
    name = "cors", check = FALSE, save_out = FALSE
) {
  if (sum(sapply(fit_y, function(x) !inherits(x, "lavaan"))) > 0) {
    stop(
      paste(
        "At least one of the elements of 'fit_y' is not an object of class",
        "lavaan."
      )
    )
  }
  if (is.null(names(fit_y))) {
    stop("'fit_y' names must not be 'NULL'.")
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
  }
  if (!is.null(fit_x)) {
    if (sum(sapply(fit_x, function(x) !inherits(x, "lavaan"))) > 0) {
      stop(
        paste(
          "At least one of the elements of 'fit_x' is not an object of class",
          "lavaan."
        )
      )
    }
    if (is.null(names(fit_x))) {
      stop("If specified, 'fit_x' names must not be 'NULL'.")
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
  sapply(
    par1,
    function(y) {
      y1 <- y[y$op %in% c("~~", "=~"), ]
      yn <- unique(y$lhs[y$op == "=~"])
      if (length(yn) > 1) {
        stop(
          paste0(
            "The 'fit_y' model including '", paste(yn, collapse = "' and '"),
            "' includes more than one latent variable. ",
            "These models are not currently supported by 'sem.cor()'. ",
            "Please include them separately."
          )
        )
      }
    }
  )
  if (is.null(fit_x)) {
    mod_key <- lapply(
      seq_along(par1[-length(par1)]),
      function(y) {
        tmp <- lapply(
          (y + 1):length(par1),
          function(x) {
            x1 <- par1[[x]]
            y1 <- par1[[y]]
            x2 <- x1[x1$op == "~~" | x1$op == "=~", ]
            y2 <- y1[y1$op == "~~" | y1$op == "=~", ]
            key_x <- unique(x1$rhs[x1$op == "=~"])
            key_y <- unique(y1$rhs[y1$op == "=~"])
            key0 <- c(x2$rhs[x2$op == "=~"], y2$rhs[y2$op == "=~"])
            # Any shared items. Need to unfix residual variance for these.
            i <- x1$lhs[x1$lhs %in% y1$lhs]
            if (length(i) != 0) {
              xf <- unique(x2$lhs[x2$op == "=~"])
              yf <- unique(y2$lhs[y2$op == "=~"])
              if (length(i) >= min(length(key_x), length(key_y))) {
                if (length(key_x) < length(key_y)) {
                  shorter_f <- xf
                  longer_f <- yf
                }
                if (length(key_x) > length(key_y)) {
                  shorter_f <- yf
                  longer_f <- xf
                }
                if (length(key_x) == length(key_y)) {
                  stop(
                    paste0(
                      "All items between '", yf, "' and '", xf, "' are shared.",
                      "\nHave you included the same latent variable twice by ",
                      "mistake?"
                    )
                  )
                }
                stop(
                  paste0(
                    "All the items in '", shorter_f, "' are included in '",
                    longer_f, "'.\n",
                    "Such a relationship should be specified as a bifactor ",
                    "model with a correlation of 0 between the group and ",
                    "general factors."
                  )
                )
              }
              warning(
                paste0(
                  "The following item(s) are in both the '", yf, "' and '", xf,
                  "' factors. ",
                  "If this is not intended, please correct it and disregard ",
                  "the correlation between these factors.\n    ",
                  paste(i, collapse = "\n    ")
                )
              )
              for (j in i) {
                x2 <- x2[!(x2$lhs == j & x2$op == "~~" & x2$rhs == j), ]
                y2 <- y2[!(y2$lhs == j & y2$op == "~~" & y2$rhs == j), ]
              }
            }
            mod0 <- paste0(
              # CFA1
              paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n"),
              "\n",
              # CFA2
              paste(y2$lhs, y2$op, y2$est, "*", y2$rhs, collapse = "\n"),
              collapse = "\n"
            )
            return(list(mod = mod0, key = key0))
          }
        )
        names(tmp) <- names(par1)[(y + 1):length(par1)]
        mod1 <- lapply(tmp, function(x) x$mod)
        key1 <- lapply(tmp, function(x) x$key)
        return(list(mod = mod1, key = key1))
      }
    )
    names(mod_key) <- names(par1)[seq_along(par1[-length(par1)])]
    mods <- unlist(lapply(mod_key, function(x) x$mod), recursive = FALSE)
    key <- unlist(lapply(mod_key, function(x) x$key), recursive = FALSE)
  } else {
    # Correlations between constructs in fit_y and constructs in fit_x
    par2 <- lapply(fit_x, parameterEstimates)
    sapply(
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
        }
      }
    )
    mod_key <- lapply(
      par1,
      function(y) {
        tmp <- lapply(
          par2,
          function(x) {
            x1 <- x[x$op == "~~" | x$op == "=~", ]
            y1 <- y[y$op == "~~" | y$op == "=~", ]
            key_x <- unique(x1$rhs[x1$op == "=~"])
            key_y <- unique(y1$rhs[y1$op == "=~"])
            key0 <- unique(c(key_x, key_y))
            # Any shared items. Need to unfix residual variance for these.
            i <- x1$lhs[x1$lhs %in% y1$lhs]
            if (length(i) != 0) {
              xf <- unique(x1$lhs[x1$op == "=~"])
              yf <- unique(y1$lhs[y1$op == "=~"])
              if (length(i) >= min(length(key_x), length(key_y))) {
                if (length(key_x) < length(key_y)) {
                  shorter_f <- xf
                  longer_f <- yf
                }
                if (length(key_x) > length(key_y)) {
                  shorter_f <- yf
                  longer_f <- xf
                }
                if (length(key_x) == length(key_y)) {
                  stop(
                    paste0(
                      "All items between '", yf, "' and '", xf, "' are shared.",
                      "\nHave you included the same latent variable twice by ",
                      "mistake?"
                    )
                  )
                }
                stop(
                  paste0(
                    "All the items in '", shorter_f, "' are included in '",
                    longer_f, "'.\n",
                    "Such a relationship should be specified as a bifactor ",
                    "model with a correlation of 0 between the group and ",
                    "general factors."
                  )
                )
              }
              warning(
                paste0(
                  "The following item(s) are in both the '", yf, "' and '", xf,
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
            mod0 <- paste0(
              # CFA1
              paste(x1$lhs, x1$op, x1$est, "*", x1$rhs, collapse = "\n"),
              "\n",
              # CFA2
              paste(y1$lhs, y1$op, y1$est, "*", y1$rhs, collapse = "\n"),
              collapse = "\n"
            )
            return(list(mod = mod0, key = key0))
          }
        )
        mod1 <- lapply(tmp, function(x) x$mod)
        key1 <- lapply(tmp, function(x) x$key)
        return(list(mod = mod1, key = key1))
      }
    )
    mods <- unlist(lapply(mod_key, function(x) x$mod), recursive = FALSE)
    key <- unlist(lapply(mod_key, function(x) x$key), recursive = FALSE)
  }
  if (!is.null(items)) {
    # Correlations with single items
    mod_key_i <- lapply(
      par1,
      function(y) {
        y1 <- y[y$op %in% c("~~", "=~"), ]
        yn <- unique(y$lhs[y$op == "=~"])
        item_overlap <- items[items %in% y1$rhs]
        if (length(item_overlap) > 0) {
          stop(
            paste0(
              "The following item(s) is in both 'items' and contributes to ",
              "the measurement of the '", yn, "' latent variable.",
              "\n  This is not supported.\n  ",
              "Please either remove the item(s) from 'items' or ",
              "(if appropriate) remove the item(s) from the latent ",
              "measurement of '", yn, "'.\n    ",
              paste(item_overlap, collapse = "\n    ")
            )
          )
        }
        mod0 <- paste0(
          # CFA
          paste(y1$lhs, y1$op, y1$est, "*", y1$rhs, collapse = "\n"),
          # Correlations
          "\n",
          paste(yn, "~~", paste0(items, collapse = " + "), "\n"),
          paste(
            sapply(
              seq_along(items)[-length(items)],
              function(i) {
                paste(
                  items[i],
                  "~~",
                  paste(items[i:length(items)], collapse = " + ")
                )
              }
            ),
            collapse = "\n"
          ),
          collapse = "\n"
        )
        key0 <- c(y$rhs[y$op == "=~"], items)
        return(list(mod = mod0, key = key0))
      }
    )
    mods_i <- lapply(mod_key_i, function(x) x$mod)
    key_i <- lapply(mod_key_i, function(x) x$key)
    names(mods_i) <- paste0(names(mods_i), ".items")
    names(key_i) <- paste0(names(key_i), ".items")
    mods <- c(mods, mods_i)
    key <- c(key, key_i)
  }
  fit <- sem.check(
    mods, data, keys_s = key, miss = miss, est = est, name = name,
    check = check, save_out = save_out
  )
  cors_y <- sapply(
    fit$par_std[!grepl("\\.items$", names(fit$par_std))],
    function(x) x$est.std[x$lhs != x$rhs & x$op == "~~"]
  )
  ci_lower_y0 <- sapply(
    fit$par_std, function(x) x$ci.lower[x$lhs != x$rhs & x$op == "~~"]
  )
  ci_upper_y0 <- sapply(
    fit$par_std, function(x) x$ci.upper[x$lhs != x$rhs & x$op == "~~"]
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
  } else {
    cor_mat_y <- sapply(
      names(fit_y),
      function(y) {
        sapply(
          names(fit_x),
          function(x) {
            ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
            cors_y[grep(ptn, names(cors_y))]
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
            ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
            ci_lower_y0[[grep(ptn, names(ci_lower_y0))]]
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
            ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
            ci_upper_y0[[grep(ptn, names(ci_upper_y0))]]
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
    if (is.vector(cor_mat_y) & length(fit_y) == 1) {
      cor_mat_y <- matrix(cor_mat_y, ncol = 1)
      colnames(cor_mat_y) <- names(fit_y)
      ci_lower_y <- matrix(ci_lower_y, ncol = 1)
      colnames(ci_lower_y) <- names(fit_y)
      ci_upper_y <- matrix(ci_upper_y, ncol = 1)
      colnames(ci_upper_y) <- names(fit_y)
    }
    rownames(cor_mat_y) <- names(fit_x)
    rownames(ci_lower_y) <- names(fit_x)
    rownames(ci_upper_y) <- names(fit_x)
  }
  if (!is.null(items)) {
    cors_yi <- do.call(
      rbind,
      mapply(
        x = fit$par_std[grepl("\\.items$", names(fit$par_std))],
        y = names(fit_y),
        SIMPLIFY = FALSE,
        FUN = function(x, y) {
          x[
            x$lhs != x$rhs & x$op == "~~" & grepl(y, x$lhs),
            c("lhs", "rhs", "est.std", "ci.lower", "ci.upper")
          ]
        }
      )
    )
    cor_mat_yi <- sapply(
      names(fit_y),
      function(x) {
        sapply(
          items,
          function(y) cors_yi$est.std[cors_yi$lhs == x & cors_yi$rhs == y]
        )
      }
    )
    ci_lower_yi <- sapply(
      names(fit_y),
      function(x) {
        sapply(
          items,
          function(y) cors_yi$ci.lower[cors_yi$lhs == x & cors_yi$rhs == y]
        )
      }
    )
    ci_upper_yi <- sapply(
      names(fit_y),
      function(x) {
        sapply(
          items,
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
      colnames(ci_upper_y) <- names(fit_y)
    }
    if (is.vector(cor_mat_yi) & length(fit_y) == 1) {
      cor_mat_yi <- matrix(cor_mat_yi, ncol = 1)
      colnames(cor_mat_yi) <- names(fit_y)
      ci_lower_yi <- matrix(ci_lower_yi, ncol = 1)
      colnames(ci_lower_yi) <- names(fit_y)
      ci_upper_yi <- matrix(ci_upper_yi, ncol = 1)
      colnames(ci_upper_yi) <- names(fit_y)
    }
    rownames(cor_mat_yi) <- items
    rownames(ci_lower_yi) <- items
    rownames(ci_upper_yi) <- items
    tmp <- fit$par_std[grep("items", names(fit$par_std))][[1]]
    cors_i <- tmp[
      tmp$lhs %in% items & tmp$rhs %in% items & tmp$op == "~~",
      c("lhs", "rhs", "est.std", "ci.lower", "ci.upper")
    ]
    if (length(items) > 1) {
      cor_mat_i <- sapply(
        items,
        function(x) {
          sapply(
            items,
            function(y) {
              cors_i$est.std[
                (cors_i$lhs == x & cors_i$rhs == y) |
                  (cors_i$lhs == y & cors_i$rhs == x)
              ]
            }
          )
        }
      )
      ci_lower_i <- sapply(
        items,
        function(x) {
          sapply(
            items,
            function(y) {
              cors_i$ci.lower[
                (cors_i$lhs == x & cors_i$rhs == y) |
                  (cors_i$lhs == y & cors_i$rhs == x)
              ]
            }
          )
        }
      )
      ci_upper_i <- sapply(
        items,
        function(x) {
          sapply(
            items,
            function(y) {
              cors_i$ci.upper[
                (cors_i$lhs == x & cors_i$rhs == y) |
                  (cors_i$lhs == y & cors_i$rhs == x)
              ]
            }
          )
        }
      )
    } else {
      cor_mat_i <- matrix(1, dimnames = list(items, items))
      ci_lower_i <- matrix(1, dimnames = list(items, items))
      ci_upper_i <- matrix(1, dimnames = list(items, items))
    }
  }
  if (is.null(fit_x)) {
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
          "'ci_lower' and 'ci_upper' were adjusted by the same amount as the ",
          "primary correlation matrix."
        )
      )
    }
  }
  if (is.null(items)) {
    cor_mat <- cor_mat_y
    ci_lower <- ci_lower_y
    ci_upper <- ci_upper_y
  } else {
    if (is.null(fit_x)) {
      cor_mat <- rbind(
        cbind(cor_mat_y, t(cor_mat_yi)), cbind(cor_mat_yi, cor_mat_i)
      )
      ci_lower <- rbind(
        cbind(ci_lower_y, t(ci_lower_yi)), cbind(ci_lower_yi, ci_lower_i)
      )
      ci_upper <- rbind(
        cbind(ci_upper_y, t(ci_upper_yi)), cbind(ci_upper_yi, ci_upper_i)
      )
    } else {
      cor_mat <- rbind(cor_mat_y, cor_mat_yi)
      ci_lower <- rbind(ci_lower_y, ci_lower_yi)
      ci_upper <- rbind(ci_upper_y, ci_upper_yi)
    }
  }
  return(
    list(
      fit = fit$fit, cor_mat = cor_mat, ci_lower = ci_lower, ci_upper = ci_upper
    )
  )
}
