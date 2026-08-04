#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `sem.path` runs latent variable structural equation models in lavaan.
#' The function takes lists of fitted CFA and/or bifactor lavaan model objects
#' for the measurement models and lavaan code for the structural model and runs
#' uses these to create the model code and run the model.
#'
#' @inheritParams sem.check
#' @param path The full structural model in lavaan code.
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
#' Extra lavaan code to be added.
#' For example, to set reliability for a single item outcome or to allow
#' specific covariances, notably semi-partial correlations in incremental
#' validity analyses.
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

sem.path <- function(
    path, data, cfa_fit = NULL, bif_fit = NULL, name = "sem", extra = NULL,
    fit_save = FALSE, fit_measures = "all", miss = "ML", est = "default",
    name = "sem", check = FALSE, save_out = FALSE
) {

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
    bif_keys <- sapply(bif_par, function(x) unique(x$rhs[x$op == "=~"]))
    bif_names <- mapply(
      x = bif_par, y = bif_keys,
      FUN = function(x, y) {
        tmp <- table(x$lhs[x$op == "=~" & x$rhs %in% y])
        names(tmp)[tmp == max(tmp)]
      }
    )
    names(bif_par) <- bif_names
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

  # Check which variables are outcomes and need free residual variance.
  y_vars <- str_extract_all(path, "(^|\n) *.*( |)~") |>
    unlist() |>
    gsub("~|\n| ", "", x = _) |>
    unique()
  x_vars <- str_extract_all(path, "(~~|~|\\+) *.*?(\n| |$|~~)") |>
    unlist() |>
    gsub("~| |\\+|\n", "", x = _) |>
    unique()
  if (!is.null(extra)) {
    extra_vars <- str_extract_all(extra, "(^|=~|~~) *.*?(=~|\n|$)") |>
      unlist() |>
      gsub("=~|~~|.*\\*|\n| ", "", x = _) |>
      unique()
  }
  items <- x_vars[!x_vars %in% c(names(cfa_par), names(bif_par), "")]
  if (!items %in% names(data)) {
    stop("A preditor in path is not in ")
  }
  item_cors <- paste(
    sapply(
      seq_along(items[-length(items)]),
      function(x) {
        paste(
          items[x], "~~", paste(items[(x + 1):length(items)], collapse = " + ")
        )
      }
    ),
    collapse = "\n"
  )
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
      x$rhs[x$op == "~1"] <- "1"
      x$op[x$op == "~1"] <- "~"
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
    paste0("\n", item_cors) |>
    paste0("\n", path)
  if (extra != FALSE) {
    mod <- paste0(mod, "\n", extra)
  }
  mod0 <- gsub(" ", "", mod) |> gsub("\n\n", "\n", x = _)


  # TODO: Figure out how to change so gsubfn is not required.


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
