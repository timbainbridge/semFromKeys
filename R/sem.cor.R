#' Creates a latent variable correlation matrix from fitted lavaan measurement
#' models
#'
#' A function to take CFA outputs and perform 2-stage procedure to for
#' structural models and produce a correlation matrix including all variables.
#' By default, each correlation between latent variables is calculated in a
#' separate model and correlations with items are calculated in a separate
#' model for each latent variable.
#' Alternatively, all variables can be included in the same model.
#' The default approach is to save time for longer lists of variables.
#'
#' @inheritParams sem.check
#' @param fit_y A named list of CFA fitted objects.
#' @param fit_x
#' A named list of CFA fitted objects or 'NULL'.
#' If 'NULL', all 'fit_y' latent variables will be correlated with all other
#' 'fit_y' latent variables.
#' If a list of CFA fitted objects, 'fit_y' latent variables will be correlated
#' with 'fix_x' latent variables.
#' @param items
#' A vector of single-item variables to correlate with 'fit_y' latent variables.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "cors".
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.

# TODO: Think about: Should overlapping items in 'items' and factors be an error?
# Given more than 1 factor, it could be reasonable to include an item's
# correlations with all other latent variables (it will mean a hole in the
# correlation matrix though).

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
  if ("items" %in% names(fit_y)) {
    stop(
      paste(
        "'items' is included as a name of a fitted model in 'fit_y'.",
        "'items' is reserved for the 'items' argument in the function.",
        "Please select an alternative name for the model."
      )
    )
  }
  if (!is.null(fit_x)) {
    if (sum(sapply(fit_x, function(x) !inherits(x, "lavaan"))) > 0) {
      stop(
        paste(
          "At least one of the elements of 'fit_x' is an object of class",
          "lavaan."
        )
      )
    }
    if (is.null(names(fit_x))) {
      stop("If 'fit_x' is specified, it elements must be named.")
    }
    if ("items" %in% names(fit_x)) {
      stop(
        paste(
          "'items' is included as a name of a fitted model in 'fit_x'.",
          "'items' is reserved for the 'items' argument in the function.",
          "Please select an alternative name for the model."
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
  # A version that computes correlations in bivariate models, puts them all
  # together and then checks the resultant matrix is positive definite.
  # If it's not, the matrix is iteratively multiplied by 0.995, diagonals set
  # back to 1, and checked again until the matrix is positive definite.
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
            key0 <- c(x2$rhs[x2$op == "=~"], y2$rhs[y2$op == "=~"])
            # Any shared items. Need to unfix residual variance for these.
            i <- x1$lhs[x1$lhs %in% y1$lhs]
            if (length(i) != 0) {
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
    mod_key <- lapply(
      par1,
      function(y) {
        tmp <- lapply(
          par2,
          function(x) {
            x1 <- x[x$op == "~~" | x$op == "=~", ]
            y1 <- y[y$op == "~~" | y$op == "=~", ]
            key0 <- c(x1$rhs[x1$op == "=~"], y1$rhs[y1$op == "=~"])
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
    # TODO: Warning is not quite right. It should not worry about overlap with
    # items from fit_x.
    item_overlap <- items[items %in% unlist(keys)]
    # if (!is.null(items) & length(item_overlap) > 0) {
    #   warning(
    #     paste0(
    #       "The following items in 'items' are also in a latent variable. ",
    #       "They will not be included in correlations with their factor.\n    ",
    #       paste(item_overlap, collapse = "\n    ")
    #     )
    #   )
    # }
    mod_key_i <- lapply(
      par1,
      function(y) {
        y1 <- y[y$op %in% c("~~", "=~"), ]
        yn <- unique(y$lhs[y$op == "=~"])
        if (length(yn) > 1) {
          stop(
            paste(
              "The model including", paste(yn, collapse = " and "),
              "includes more than one latent variable.",
              "These models are not currently supported by 'sem.cor()'.",
              "Please include them separately or "
            )
          )
        }
        mod0 <- paste0(
          # CFA
          paste(y1$lhs, y1$op, y1$est, "*", y1$rhs, collapse = "\n"),
          # Correlation
          "\n",
          # Make sure items in the latent variable are not included in items
          paste(
            yn, "~~", paste0(items[!items %in% y1$rhs], collapse = " + "), "\n"
          ),
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
    rownames(cor_mat_y) <- names(fit_x)
    ci_lower_y <- sapply(
      names(fit_y),
      function(y) {
        sapply(
          names(fit_x),
          function(x) {
            ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
            tmp <- ci_lower_y0[[grep(ptn, names(ci_lower_y0))]]
            if (length(tmp) == 0) {
              tmp <- 1
            }
            tmp
          },
          USE.NAMES = FALSE
        )
      }
    )
    rownames(ci_lower_y) <- names(fit_x)
    ci_upper_y <- sapply(
      names(fit_y),
      function(y) {
        sapply(
          names(fit_x),
          function(x) {
            ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
            tmp <- ci_upper_y0[[grep(ptn, names(ci_upper_y0))]]
            if (length(tmp) == 0) {
              tmp <- 1
            }
            tmp
          },
          USE.NAMES = FALSE
        )
      }
    )
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
    tmp <- fit$par_std[grep("items", names(fit$par_std))][[1]]
    cors_i <- tmp[
      tmp$lhs %in% items & tmp$rhs %in% items & tmp$op == "~~",
      c("lhs", "rhs", "est.std", "ci.lower", "ci.upper")
    ]
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
  }
  if (is.null(fit_x)) {
    if (!matrixcalc::is.positive.definite(cor_mat_y)) {
      multi <- 1
      while (!matrixcalc::is.positive.definite(cor_mat_y)) {
        cor_mat_y <- cor_mat_y * .995
        ci_lower_y <- ci_lower_y * .995
        ci_upper_y <- ci_upper_y * .995
        multi <- multi * .995
        diag(cor_mat) <- 1
      }
      warning(
        paste(
          "The correlation matrix between 'fit_y' constructs has been adjusted",
          "from initial estimates to ensure it is positive definite.",
          "All off-diagonal corelations were multiplied by", multi,
          "to achieve this."
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
#   else {
#     fit <- sem.check(
#       mods, data, keys_s = key, miss = miss, est = est, name = name,
#       check = check, save_out = save_out
#     )
#     cors <- sapply(
#       fit$par_std, function(x) x$est.std[x$lhs != x$rhs & x$op == "~~"]
#     )
#     cor_mat <- sapply(
#       names(fit_y),
#       function(y) {
#         sapply(
#           names(fit_x),
#           function(x) {
#             ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
#             tmp <- cors[grep(ptn, names(cors))]
#             if (length(tmp) == 0) {
#               tmp <- 1
#             }
#             tmp
#           },
#           USE.NAMES = FALSE
#         )
#       }
#     )
#     rownames(cor_mat) <- names(fit_x)
#     ci_lower0 <- sapply(
#       fit$par_std, function(x) x$ci.lower[x$lhs != x$rhs & x$op == "~~"]
#     )
#     ci_lower <- sapply(
#       names(fit_y),
#       function(y) {
#         sapply(
#           names(fit_x),
#           function(x) {
#             ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
#             tmp <- ci_lower0[grep(ptn, names(ci_lower0))]
#             if (length(tmp) == 0) {
#               tmp <- 1
#             }
#             tmp
#           },
#           USE.NAMES = FALSE
#         )
#       }
#     )
#     rownames(ci_lower) <- names(fit_x)
#     ci_upper0 <- sapply(
#       fit$par_std, function(x) x$ci.upper[x$lhs != x$rhs & x$op == "~~"]
#     )
#     ci_upper <- sapply(
#       names(fit_y),
#       function(y) {
#         sapply(
#           names(fit_x),
#           function(x) {
#             ptn <- paste0(x, "\\.", y, "|", y, "\\.", x)
#             tmp <- ci_upper0[grep(ptn, names(ci_upper0))]
#             if (length(tmp) == 0) {
#               tmp <- 1
#             }
#             tmp
#           },
#           USE.NAMES = FALSE
#         )
#       }
#     )
#   }
#   if (is.null(items)) {
#     return(
#       list(
#         fit = fit$fit,
#         cor_mat = cor_mat, ci_lower = ci_lower, ci_upper = ci_upper
#       )
#     )
#   } else {
#     fit_s <- sem.check(
#       mod,
#       function(i) sem(i, data, std.lv = TRUE, missing = miss1, estimator = est1)
#     )
#     cor_mat1 <- mapply(
#       function(i, ni) {
#         tmp <- standardizedSolution(i)
#         tmp1 <- setNames(
#           tmp$est.std[
#             (tmp$lhs == ni & tmp$rhs %in% items) |
#               (tmp$lhs %in% items & tmp$rhs == ni)
#           ],
#           items
#         )
#         tmp2 <- tmp$rhs[tmp$op == "=~"]
#         tmp1[tmp1 %in% tmp2] <- NA
#         return(tmp1)
#       },
#       i = fit_s, ni = names(fit_s)
#     )
#     ci_lower1 <- mapply(
#       function(i, ni) {
#         tmp <- standardizedSolution(i)
#         tmp1 <- setNames(
#           tmp$ci.lower[
#             (tmp$lhs == ni & tmp$rhs %in% items) |
#               (tmp$lhs %in% items & tmp$rhs == ni)
#           ],
#           items
#         )
#         tmp2 <- tmp$rhs[tmp$op == "=~"]
#         tmp1[tmp1 %in% tmp2] <- NA
#         return(tmp1)
#       },
#       i = fit_s, ni = names(fit_s)
#     )
#     ci_upper1 <- mapply(
#       function(i, ni) {
#         tmp <- standardizedSolution(i)
#         tmp1 <- setNames(
#           tmp$ci.upper[
#             (tmp$lhs == ni & tmp$rhs %in% items) |
#               (tmp$lhs %in% items & tmp$rhs == ni)
#           ],
#           items
#         )
#         tmp2 <- tmp$rhs[tmp$op == "=~"]
#         tmp1[tmp1 %in% tmp2] <- NA
#         return(tmp1)
#       },
#       i = fit_s, ni = names(fit_s)
#     )
#     cor_mat_f <- rbind(cor_mat, cor_mat1)
#     ciLower_f <- rbind(ci_lower, ci_lower1)
#     ciUpper_f <- rbind(ci_upper, ci_upper1)
#     return(
#       list(
#         fit = fit, fit_s = fit_s, cor_mat = cor_mat_f,
#         ci_lower = ciLower_f, ci_upper = ciUpper_f
#       )
#     )
#   }
}
