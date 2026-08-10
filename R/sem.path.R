#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `sem.path` runs latent variable structural equation models in lavaan.
#' The function takes lists of fitted CFA lavaan model objects
#' for the measurement models and lavaan code for the structural model and runs
#' uses these to create the model code and run the model.
#'
#' @inheritParams sem.check
#' @param path
#' Regression or paths between latent variables or single-item indicators in
#' lavaan code.
#' @param cfa_fit
#' A named list of fitted lavaan objects of CFA models.
#' Can be `NULL` if `bif_fit` is not `NULL`.
#' @param bif_fit
#' A named list of fitted lavaan objects of bifactor models.
#' Can be `NULL` if `cfa_fit` is not `NULL`.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "sem".
#' Irrelevant if both `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#' @param extra
#' Extra lavaan code to be added that is not included in `cfa_fit`, `bif_fit`,
#' or `path`.
#' For example, the argument can be used to set constraints, fix values,
#' or allow correlations between item residuals or latent variables.
#' Notably, the feature can be used to fix semi-partial correlations in
#' incremental validity analyses (see Hayes, 2001).
#' @param items
#' A vector of single-item indicators of structural elements.
#' 'items' must not include any items contributing to the measurement of a
#' latent variable.
#' If items are included in `path` but `items = NULL` and
#' `item_loadings = NULL`, then the function will find the items automatically.
#' @param item_loadings
#' When single items are specified, items are included in models with single
#' item latent variables.
#' `item_loadings` sets the loading of the item on the factor.
#' Set as `NULL` to allow the value to be freely estimated (default);
#' set as a single number to set all item loadings equal to that number; or
#' set as a vector of length equal to the length of items to set all loadings.
#' Irrelevant if `items = NULL`.
#' @param orth_items
#' Logical.
#' If `FALSE`, all single-item latent variable correlations with all other
#' latent variables are freely estimated.
#' If `TRUE`, single-item latent variables are fixed at 0 unless explicitly
#' freed or set to an alternative value
#' (using `extra`, see details for how to do this).
#' Irrelevant if `items = NULL`.
#'
#' @return
#' Returns a list of length 4 (if `fit_save = FALSE`) or
#' 5 (if `fit_save = TRUE`).
#' The elements of the list are: a lavaan model output object;
#' a data frame of parameter estimates from the model
#' (standardized if `std = TRUE`);
#' a vector of fit measures for the model if `fit_save = TRUE`;
#' a list of structural regression and correlation parameters from the model;
#' and a data frame of R-squared values from the model (if applicable).
#'
#' @details
#' TBD
#'
#' @importFrom stringr string_extract_all
#' @importFrom stringr str_split
#' @export
#'
#' @references
#' Hayes, T. (2001).
#' R-squared change in structural equation models with latent variables and
#' missing data.
#' Behavior Research Methods, 53(5), 2127-2157.
#' https://doi.org/10.3758/s13428-020-01532-y.


# TODO: Check that no element of items is in a measurement model.

sem.path <- function(
    path, data, cfa_fit, items = NULL, item_loadings = NULL, extra = NULL,
    fit_save = FALSE, fit_measures = "all", miss = "ML", est = "default",
    orth_items = FALSE,
    name = "sem", check = FALSE, save_out = FALSE
) {
  if (!is.null(items)) {
    if (any(sapply(items, function(x) !x %in% names(data)))) {
      stop("An item in 'items' does not match a variable name in 'data'.")
    }
  }

  ##### Below adapted from esem.from.mods() #####
  if (!is.list(cfa_fit) & inherits(cfa_fit, "lavaan")) {
    cfa_fit <- list(factor = cfa_fit)
  }
  if (sum(sapply(cfa_fit, function(x) !inherits(x, "lavaan"))) > 0) {
    stop(
      paste0(
        "The below elements of 'cfa_fit' are not objects of type lavaan.",
        "\n    ",
        paste0(
          names(cfa_fit)[sapply(cfa_fit, function(x) !inherits(x, "lavaan"))],
          collapse = "\n    "
        )
      )
    )
  }
  cfa_par <- sapply(cfa_fit, parameterEstimates, simplify = FALSE)
  cfa_keys <- sapply(cfa_par, function(x) x$rhs[x$op == "=~"])
  cfa_names <- sapply(
    cfa_par,
    function(x) {
      x1 <- unique(x$lhs[x$op == "=~"])
      if (length(x1) > 1) {
        stop(
          paste(
            "A CFA containing more than one latent variable has been found.",
            "Currently, the function only supports CFAs included in separate",
            "models.\n    ",
            paste(x1, collapse = "\n    ")
          )
        )
      }
      return(x1)
    }
  )
  names(cfa_fit) <- names(cfa_par) <- names(cfa_keys) <- cfa_names
  if (sum(table(names(cfa_keys)) > 1) > 0) {
    stop(
      paste(
        "At least two different models in 'cfa_fit' have factors with the",
        "same name.",
        "Please ensure that all factor names are unique."
      )
    )
  }
  ##### Above adapted from esem.from.mods() #####

  # Check which variables are outcomes and need free residual variance.
  y_vars <- stringr::str_extract_all(path, "(^|\n) *.*( |)~") |>
    unlist() |>
    gsub("~|\n| ", "", x = _) |>
    unique()
  x_vars <- stringr::str_extract_all(path, "(~~|~|\\+) *.*?(\n| |$|~~)") |>
    unlist() |>
    gsub("~| |\\+|\n", "", x = _) |>
    unique()
  if (!is.null(extra)) {
    extra_vars <- extra |>
      stringr::str_split("\\s*(?:~~|=~|~|\\+|\n)\\s*") |>
      unlist() |>
      unique()
    extra_vars <- extra_vars[!extra_vars %in% c(x_vars, y_vars)]
  }
  if (is.null(items) & is.null(item_loadings)) {
    all_vars <- c(y_vars, x_vars, extra_vars)
    items <- all_vars[!all_vars %in% names(cfa_par)]
    if (length(items) > 0) {
      item_miss <- items[!items %in% names(data)]
      if (length(item_miss) > 0) {
        stop(
          paste(
            item_miss[1], "is in 'path' but does not match either a latent",
            "variable name, nor a variable name in 'data'."
          )
        )
      }
    }
  }
  item_in_cfa <- items[items %in% unlist(cfa_keys)]
  if (length(item_in_cfa) > 0) {
    stop(
      paste(
        item_in_cfa[1], "is a single item but is also an item in a CFA model.",
        "Items that contribute to a CFA measurement model cannot",
        "also be a structural variable."
      )
    )
  }
  y_miss <- y_vars[!y_vars %in% c(names(data), names(cfa_fit))]
  if (length(y_miss) > 0) {
    stop(
      paste(
        y_miss[1], "is in 'path' but is not in 'items', 'cfa_fit',",
        "or a named variable in 'data'.",
        "Please check", y_miss[1], "matches the name of the relevant CFA,",
        "item, or variable name as appropriate.",
        "Note that latent variable names must match that of the factor",
        "in the lavaan model (which could differ from the object name)."
      )
    )
  }
  x_miss <- x_vars[!x_vars %in% c(names(data), names(cfa_fit))]
  if (length(x_miss) > 0) {
    stop(
      paste(
        x_miss[1], "is in 'path' but is not in 'items', 'cfa_fit',",
        "or a named variable in 'data'.",
        "Please check", x_miss[1], "matches the name of the relevant CFA,",
        "item, or variable name as appropriate.",
        "Note that latent variable names must match that of the factor",
        "in the lavaan model (which could differ from the object name)."
      )
    )
  }
  if (!is.null(extra)) {
    if (length(extra_vars) > 0) {
      extra_miss <- extra_vars[!extra_vars %in% c(names(data), names(cfa_fit))]
      if (length(extra_miss) > 0) {
        stop(
          paste(
            extra_miss[1], "is in 'extra' but is not in 'items', 'cfa_fit',",
            "or a named variable in 'data'.",
            "Please check", extra_miss[1], "matches the name of",
            "the relevant CFA, item, or variable name as appropriate.",
            "Note that latent variable names must match that of the factor",
            "in the lavaan model (which could differ from the object name)."
          )
        )
      }
    }
  }
  if (!is.null(items)) {
    mod_i <- paste0(
      lapply(
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
          paste0(i_l, " =~ ", i_r, i)
        }
      ),
      collapse = "\n"
    )
    if (length(items) > 1 & !orth_items) {
      item_cors <- paste(
        sapply(
          seq_along(items[-length(items)]),
          function(x) {
            paste0(
              "\n", items[x], "_l ~~ ",
              paste0(items[(x + 1):length(items)], "_l", collapse = " + ")
            )
          }
        ),
        collapse = "\n"
      )
    } else {
      item_cors <- NULL
    }
  } else {
    item_cors <- NULL
    mod_i <- NULL
  }
  # Full structural model
  mod <- sapply(
    cfa_par,
    function(x) {
      i <- x$lhs[x$op == "=~"] |> unique()
      if (sum(i %in% y_vars) >= 1) {
        for (j in i) {
          x <- x[
            !(
              (x$lhs == j & x$op == "~~" & x$rhs == j) |
                (x$lhs == j & x$op == "~1")
            ),
          ]
        }
      }
      x <- x[x$op %in% c("~~", "=~"), ]
      paste(
        c(
          # CFA model
          paste(x$lhs, x$op, x$est, "*", x$rhs, collapse = "\n"),
          # Correlations with items
          # (excluding Y vars, where Y is regressed on items, so cannot also be
          # correlated)
          if (length(items) > 0 & !orth_items) {
            sapply(
              i,
              function(j) {
                if (!j %in% y_vars) {
                  paste(
                    i, "~~", paste(items, collapse = " + "), collapse = "\n"
                  )
                } else {
                  ""
                }
              }
            )
          }
        ),
        collapse = "\n"
      )
    }
  ) |>
    paste0(collapse = "\n") |>
    paste0(mod_i, item_cors) |>
    paste0("\n", path)
  if (!is.null(extra)) {
    mod <- paste0(mod, "\n", extra)
  }
  fit <- sem.check(
    setNames(list(mod), nm = name), data = data,
    keys_s = setNames(list(c(unlist(cfa_keys), items)), nm = name),
    fit_save = fit_save, fit_measures = fit_measures,
    miss = miss, est = est, orthogonal = TRUE, std = TRUE,
    name = name, check = check, save_out = save_out
  )
  x <- fit$par_std[[name]]
  xr <- x[x$op == "~~" & x$lhs == x$rhs & x$lhs %in% y_vars, ]
  r2 <- data.frame(
    y_var = xr$lhs,
    R2 = 1 - xr$est.std,
    se = xr$se,
    ci.lower = 1 - xr$ci.upper,
    ci.upper = 1 - xr$ci.lower
  )
  b <- x[x$op == "~", -2]
  names(b)[1] <- "y_var"
  names(b)[2] <- "x_var"
  if (fit_save) {
    return(
      list(
        fit = fit$fit[[name]],
        par_std = fit$par_std[[name]],
        fit_measures = fit$fit_measures[name, ],
        b = b,
        r2 = r2
      )
    )
  } else {
    return(
      list(fit = fit$fit[[name]], par_std = fit$par_std[[name]], b = b, r2 = r2)
    )
  }
}
