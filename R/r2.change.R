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
#' @param cfa_fit A named list of fitted lavaan objects of CFA models.
#' @param bif_fit A named list of fitted lavaan objects of bi-factor models.
#' @param X
#' A length 1 character vector of the name of the the variable being tested.
#' @param Y A string of the name of the outcome variable.
#' @param Z
#' A character vector of the set of variables that X is being pitted against or
#' `NULL` to use all variables in `cfa_fit` and `bif_fit`. Defaults to `NULL`.
#'
#' @return
#' A list of length X.
#' Elements include the fitted lavaan objects for the full and reduced models;
#' estimates of R-squared and R-squared change; and data frames of covariate
#' effect sizes from both the full and reduced models.

# TODO: Add boot for bootstrap CIs for DR2.
# TODO: Add check and save_out.
# TODO: Add other parameter options (miss, est, etc.)

r2.change <- function(data, X, Y, Z, cfa_fit = NULL, bif_fit = NULL) {
  if (!X %in% c(names(cfa_fit), names(bif_fit), names(data))) {
    stop("'X' is not an element of 'cfa_fit' or 'bif_fit', or in 'data'.")
  }
  if (!Y %in% c(names(cfa_fit), names(bif_fit), names(data))) {
    stop("'Y' is not an element of 'cfa_fit' or 'bif_fit', or in 'data'.")
  }
  if (sum(!Z %in% c(names(cfa_fit), names(bif_fit), names(data))) > 0) {
    stop(
      paste(
        "At least one 'Z' variable is not an element of 'cfa_fit' or",
        "'bif_fit', or in 'data'."
      )
    )
  }
  if (is.null(cfa_fit) & is.null(bif_fit)) {
    stop("At least one of 'cfa_fit' and 'bif_fit' must be specified.")
  }

  ###### Copied from esem.from.mods #####
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
    cfa_keys <- lapply(cfa_par, function(x) x$rhs[x$op == "=~"])
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
    names(cfa_fit) <- names(cfa_par) <- names(cfa_keys) <- cfa_names
    lapply(
      cfa_par,
      function(x) {
        if (sum(x$op == "|") > 0) {
          stop(
            paste(
              "At least one element of 'cfa_fit' is a model with ordinal",
              "variables, which are not currently supported in",
              "'r2.change'. To use the function, re-run the input models",
              "with all variables treated as continuous."
            )
          )
        }
      }
    )
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
    names(bif_par) <- bif_names
    lapply(
      bif_par,
      function(x) {
        if (sum(x$op == "|") > 0) {
          stop(
            paste(
              "At least one element of 'bif_fit' is a model with ordinal",
              "variables, which are not currently supported in",
              "'r2.change'. To use the function, re-run the input models",
              "with all variables treated as continuous."
            )
          )
        }
      }
    )
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
  if (!is.null(cfa_fit)) {
    mods_cfa <- sapply(
      names(cfa_keys),
      function(x) {
        x0 <- cfa_par[[x]]
        x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
        x2 <- x1[!(x == x1$lhs & x == x1$rhs), ]
        x3 <- paste0(paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n"))

      },
      simplify = FALSE
    )
  }
  if (!is.null(bif_fit)) {
    mods_bif <- sapply(
      names(bif_keys),
      function(x) {
        x0 <- bif_par[[x]]
        x1 <- x0[x0$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "est")]
        x2 <- x1[!(x == x1$lhs & x1$op == "~~"), ]
        x3 <- paste0(paste(x2$lhs, x2$op, x2$est, "*", x2$rhs, collapse = "\n"))
        groupf <- unique(x0$lhs[x0$op == "=~" & !(x0$lhs %in% x)])
      },
      simplify = FALSE
    )
  }
  ###### Copied from esem.from.mods #####

  # TODO: Select cfa_fit, bif_fit, only for variables in X, Y, Z.
  # TODO: Concatenate models
  # TODO: Create reduced and full models
  # TODO: Send to sem.check.
  # TODO: Take sem.check R2s from models
  # TODO: Calculate DR2.
  # TODO: Take regression table from models
  # TODO: Put together into a table.
}
