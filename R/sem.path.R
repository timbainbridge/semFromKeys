#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `sem.path` runs latent variable structural equation models in lavaan.
#' The function takes lists of fitted CFA and/or bifactor lavaan model objects
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
#' @param item_loadings
#' When single items are specified, items are included in models with single
#' item latent variables.
#' `item_loadings` sets the loading of the item on the factor.
#' Set as `NULL` to allow the value to be freely estimated (default);
#' set as a single number to set all item loadings equal to that number; or
#' set as a vector of length equal to the length of items to set all loadings.
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
#' @importFrom stringr string_extract_all
#' @importFrom stringr str_split
#'
#' @references
#' Hayes, T. (2001).
#' R-squared change in structural equation models with latent variables and
#' missing data.
#' Behavior Research Methods, 53(5), 2127-2157.
#' https://doi.org/10.3758/s13428-020-01532-y.


# TODO: Check that no element of items is in a measurement model.


sem.path <- function(
    path, data, cfa_fit = NULL, bif_fit = NULL,
    items = NULL, item_loadings = NULL, extra = NULL,
    fit_save = FALSE, fit_measures = "all", miss = "ML", est = "default",
    name = "sem", check = FALSE, save_out = FALSE
) {
  if (!is.null(items)) {
    if (!items %in% names(data)) {
      stop(
        paste(
          "An  preditor in 'path' does not match a variable name in",
          "'data' nor a latent varialbe name in 'cfa_fit' or 'bif_fit'.",
          "Please check dependent variable names used in 'path' match those of",
          "the relevant CFA, bifactor, or item name as appropriate.",
          "Note that the name must match that in the lavaan model, not the",
          "name of the object (if different)."
        )
      )
    }
  }

  ##### Below copied from esem.from.mods() #####
  if (is.null(cfa_fit) & is.null(bif_fit)) {
    stop("At least one of 'cfa_fit' and 'bif_fit' must be specified.")
  }
  if (!is.null(cfa_fit)) {
    # Single model instead of list.
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
  }
  if (!is.null(bif_fit)) {
    # Single model instead of list.
    if (!is.list(bif_fit) & inherits(bif_fit, "lavaan")) {
      bif_fit <- list(bifactor = bif_fit)
    }
    if (sum(sapply(bif_fit, function(x) !inherits(x, "lavaan"))) > 0) {
      stop(
        paste0(
          "The below elements of 'bif_fit' are not objects of type lavaan.",
          "\n    ",
          paste0(
            names(bif_fit)[sapply(bif_fit, function(x) !inherits(x, "lavaan"))],
            collapse = "\n    "
          )
        )
      )
    }
  }
  if (!is.null(cfa_fit)) {
    cfa_par <- sapply(cfa_fit, parameterEstimates, simplify = FALSE)
    # Extract factor names
    cfa_names <- sapply(
      cfa_par,
      function(x) {
        x1 <- unique(x$lhs[x$op == "=~"])
        if (length(x1) > 1) {
          stop(
            paste(
              "A CFA containing more than one latent variable has been found.",
              "Currently, the function only supports CFAs included in separate",
              "models.",
              "Please either use 'bif_fit' and a model supported there,",
              "or separate the CFAs into separate measurement models.",
              "The offending factors are:\n",
              "    ",
              paste(x1, collapse = "\n    ")
            )
          )
        }
        return(x1)
      }
    )
    names(cfa_fit) <- names(cfa_par) <- cfa_names
    cfa_keys <- sapply(cfa_par, function(x) x$rhs[x$op == "=~"])
    names(cfa_keys) <- cfa_names
    if (sum(table(names(cfa_keys)) > 1) > 0) {
      stop(
        paste(
          "At least two different models in 'cfa_fit' have factors with the",
          "same name.",
          "Please ensure that all factor names are unique."
        )
      )
    }
  }
  if (!is.null(bif_fit)) {
    bif_par <- sapply(bif_fit, parameterEstimates, simplify = FALSE)
    bif_keys <- lapply(bif_par, function(x) unique(x$rhs[x$op == "=~"]))
    bif_names <- mapply(
      x = bif_par, y = bif_keys,
      FUN = function(x, y) {
        tmp <- table(x$lhs[x$op == "=~" & x$rhs %in% y])
        names(tmp)[tmp == max(tmp)]
      }
    )
    names(bif_fit) <- names(bif_par) <- bif_names
    if (!is.null(names(bif_fit))) {
      if (sum(names(bif_fit) != bif_names) > 0) {
        warning(
          paste(
            "The names of 'bif_fit' do not match the general factor names.",
            "Names of returned objects are based on factor names",
            "so they will not match the names of 'bif_fit'."
          )
        )
      }
    }
    names(bif_keys) <- bif_names
    if (sum(table(names(bif_keys)) > 1) > 0) {
      stop(
        paste(
          "At least two different models in 'bif_fit' have general factors",
          "with the same name.",
          "Please ensure that all factor names are unique."
        )
      )
    }
  }
  if (!is.null(cfa_fit) & !is.null(bif_fit)) {
    if (sum(names(cfa_keys) %in% names(bif_keys)) > 0) {
      stop(
        paste(
          "The following models in 'cfa_fit' have identically named",
          "factor(s) in 'bif_fit':\n    ",
          paste(
            names(cfa_fit)[names(cfa_fit) %in% names(bif_fit)], collapse = "\n"
          ),
          "\n\n  Please ensure that CFA factors and bifactor general factors",
          "have unique names."
        )
      )
    }
  }
  ##### Above copied from esem.from.mods() #####

  if (!is.null(cfa_fit)) {
    if (!is.null(bif_fit)) {
      par1 <- c(cfa_par, bif_par)
    } else {
      par1 <- cfa_par
    }
  } else {
    par1 <- bif_par
  }
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

    # TODO: Check for generality.

    extra_vars <- extra |>
      stringr::str_split("\\s*(?:~~|=~|~)\\s*") |>
      unlist() |>
      unique()

      # str_extract_all(extra, "(^|=~|~~) *.*?(~~|=~|\n|$)") |>
      # unlist() |>
      # gsub("=~|~~|.*\\*|\n| ", "", x = _) |>
      # unique()
  }
  if (!y_vars %in% c(names(data), names(cfa_fit), names(bif_fit))) {
    stop(
      paste(
        "A depenent variable in 'path' is not in 'data', 'cfa_fit', nor",
        "'bif_fit'.",
        "Please check dependent variable names used in 'path' match those of",
        "the relevant CFA, bifactor, or item name as appropriate.",
        "Note that the name must match that in the lavaan model, not the name",
        "of the object (if different)."
      )
    )
  }
  if (!is.null(extra)) {
    if (
      length(extra_vars) > 0 &
      !extra_vars %in% c(names(data), names(cfa_fit), names(bif_fit))
    ) {
      stop(
        paste(
          "A variable in 'extra' does not match a variable name in",
          "'data' nor a latent varialbe name in 'cfa_fit' or 'bif_fit'.",
          "Please check variable names used in 'extra' match those of",
          "the relevant CFA, bifactor, or item name as appropriate.",
          "Note that the name must match that in the lavaan model, not the",
          "name of the object (if different)."
        )
      )
    }
  }
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
        mod0 <- paste0(i_l, " =~ ", i_r, i)
      }
    ),
    collapse = "\n"
  )
  if (length(items) > 1) {
    item_cors <- paste(
      sapply(
        seq_along(items[-length(items)]),
        function(x) {
          paste0(
            "\n", items[x], "_l ~~ ",
            paste(items[(x + 1):length(items)], "_l", collapse = " + ")
          )
        }
      ),
      collapse = "\n"
    )
  } else {
    item_cors <- NULL
  }
  # Full structural model
  mod <- sapply(
    par1,
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
      # x$rhs[x$op == "~1"] <- "1"
      # x$op[x$op == "~1"] <- "~"
      x <- x[x$op %in% c("~~", "=~"), ]
      paste(
        c(
          # CFA model
          paste(x$lhs, x$op, x$est, "*", x$rhs, collapse = "\n"),
          # Correlations with items
          # (excluding Y vars, where Y is regressed on items, so cannot also be
          # correlated)
          if (length(items) > 0) {
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
    paste0(item_cors) |>
    paste0("\n", path)
  if (!is.null(extra)) {
    mod <- paste0(mod, "\n", extra)
  }
  mod0 <- gsub(" ", "", mod) |> gsub("\n\n", "\n", x = _)


  # TODO: Remind myself how to change this so gsubfn is not required.
  # (Likely from sem.cor)


  requireNamespace(gsubfn)
  m_test <- if (m0 != FALSE) {
    gsubfn::gsubfn(
      "([0-9]\\.[0-9]+)",
      ~format(round(as.numeric(x), 4), nsmall = 4),
      mod0
    ) ==
      gsubfn::gsubfn(
        "([0-9]\\.[0-9]+)",
        ~format(round(as.numeric(x), 4), nsmall = 4),
        m0
      )
  } else FALSE
  fit <- sem(
    mod, dat,
    orthogonal = orthogonal, missing = miss, estimator = est,
    std.lv = std.lv, ordered = ordered
  )
  return(fit)
}
