#' Runs ESEM based on CFA and EFA model outputs.
#'
#' *WARNING* Experimental.
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
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "sem".
#' Should never be "" or `NULL` but otherwise irrelevant if both
#' `save_out = FALSE` and `check = FALSE`.
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#' @param extra
#' Extra lavaan code to be added that is not created from `cfa_fit`, or included
#' in `path`.
#' For example, the argument can be used to set constraints, fix values,
#' or allow correlations between item residuals or latent variables.
#' Notably, the argument can be used to free semi-partial correlations in
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
#' single-item latent variables are freely estimated.
#' If `TRUE`, single-item latent variables are fixed at 0 unless explicitly
#' freed or set to an alternative value
#' (using `extra`, see details for how to do this).
#' Defaults to `TRUE`.
#' Irrelevant if `items = NULL` and no items are specified in `path` or `extra`.
#'
#' @return
#' Returns a list of length 5 (if `fit_save = FALSE`) or
#' 6 (if `fit_save = TRUE`).
#' The elements of the list are: a lavaan model output object;
#' a data frame of parameter estimates from the model;
#' a vector of fit measures for the model if `fit_save = TRUE`;
#' a list of structural regression and correlation parameters from the model;
#' and a data frame of R-squared values from the model (if applicable).
#'
#' @details
#' *WARNING* Experimental.
#'
#' The function runs SEM with fitted CFA models, a specified path, and,
#' optionally, items or 'extra' lavaan code.
#' The function uses Burt's (1976) 2-stage procedure to control for
#' interpretational confounding.
#'
#' If items are included, they are treated as single-item indicators of latent
#' variables with loadings set according to the `item_loadings` argument.
#' In some cases, it will be convenient for all single-item latent variables to
#' be allowed to correlate.
#' In these cases, setting `orth_items = FALSE` will achieve this without having
#' to specify every correlation.
#'
#' The model relies on [sem.check()] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' Given `sem.path` runs a single model at a time, it is unlikely to be
#' necessary for all but the most complex SEMs.
#' For further details on how this works,
#' see the [sem.check()] function  documentation.
#'
#' In SEM, standard methods do not distinguish between measurement and
#' structural parameters. As a result, measurement model parameters can change
#' with the addition of theoretically unrelated constructs in a structural
#' model, and can change differently for different sets of unrelated constructs.
#' This means that the unrelated constructs are changing the interpretation of
#' the latent variable.
#' This is known as "interpretational confounding" (Burt, 1976).
#'
#' There is some disagreement about how to deal with interpretational
#' confounding. The standard solution (other than ignoring it) is to create good
#' fitting measurement models first, then freely estimate the structural model
#' with checks to ensure adequate fit of the model and that interpretational
#' confounding is not an issue.
#' This is sometimes a good solution, but, in other cases, simply moves the
#' problem. If the measurement model was for a well-established scale and it is
#' changed, it loses easy comparison with past research.
#' This issue is most clearly relevant when changes to a measurement model
#' require entirely different factors, or items to be removed.
#' When a single scale is being assessed, these issues can be resolved by
#' suggesting a thorough evaluation of the scale and, perhaps, the suggestion of
#' a new measurement model or a new scale for a particular population.
#' However, when many scales are being assessed this solution is impractical.
#'
#' An alternative solution, proposed by Burt (1976) is to fix measurement model
#' parameters in a model estimating structural parameters.
#' This method means that misspecification of one measurement model cannot
#' affect other measurement models and that the interpretation of measured
#' constructs cannot change based on unrelated factors.
#' However, it is not a perfect solution because it underestimates uncertainty
#' in the measurement part of the structural model (e.g., Nagy et al., 2017),
#' which results in biased standard errors and fit statistics.
#'
#' A variety of options are available to correct these issues.
#' One such option was proposed by Nagy and colleagues (2017),
#' who propose and extension procedure such that item residuals are allowed to
#' correlate with external variables (or factors).
#' To ensure the model is identifiable these relationships are constrained using
#' one of a number of methods.
#' If the sums of squares of correlations between all combinations of factors'
#' items and external factors are minimised,
#' measurement parameters in isolated measurement models are preserved in the
#' structural model without having to constrain them directly.
#' As a result, unbiased standard errors are preserved while simultaneously
#' eliminating interpretational confounding since the measurement parameters
#' from the measurement models are preserved regardless of external factors.
#' Unfortunately, estimating these models becomes increasingly slow with more
#' items and factors, such that it quickly becomes untenable.
#' Moreover, the method only works with correlations, not regressions,
#' so some method to run regressions using the correlations needs to be
#' implemented that does not itself result in biased estimates due to ignored
#' uncertainty in the correlation estimates.
#'
#' Pragmatic solution to these issues include Rosseel and Loh's (2022)
#' "Structural-After-Measurement" (SAM) approaches.
#' These methods essentially follow Burt's (1976) method but adjust the
#' procedure to overcome its issues. One method (labelled "local SAM") uses
#' the observed summary statistics of the parameters of the measurement models
#' to generate mean and covariance matrices to use in the structural model,
#' which preserves the structure of the measurement models while also preserving
#' the uncertainty. Another method ("global SAM") treats the measurement
#' parameters as given, but corrects the standard errors.
#'
#' `sem.path` currently uses Burt's 2-stage procedure of fixing
#' measurement parameters in the structural models due to its use in
#' [esem.from.mods()], where point estimates were of primary concern in the
#' research that inspired the function
#' (i.e., Bainbridge, Ludeke, and Smillie, 2022).
#' However, this is *not* the best method, especially in this context, and
#' it is planned to change the default method to the local SAM method in the
#' future. Therefore, results may change when the update occurs and the function
#' should be considered experimental.
#'
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @export
#'
#' @references
#' Hayes, T. (2001).
#' R-squared change in structural equation models with latent variables and
#' missing data.
#' Behavior Research Methods, 53(5), 2127-2157.
#' https://doi.org/10.3758/s13428-020-01532-y.

# TODO: Add references.

sem.path <- function(
    path, data, cfa_fit, items = NULL, item_loadings = NULL, extra = NULL,
    fit_save = TRUE, fit_measures = "all", miss = "ML", est = "default",
    orth_items = TRUE,
    name = "sem", check = FALSE, save_out = FALSE
) {
  item_miss <- items[!items %in% names(data)]
  if (!is.null(items)) {
    if (length(item_miss) > 0) {
      stop(
        paste0(
          "'", item_miss[1], "' is in 'items' but does not match ",
          "a variable name in 'data'."
        )
      )
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
  x_vars <- x_vars[!x_vars %in% y_vars]
  if (!is.null(extra)) {
    extra_vars <- extra |>
      stringr::str_split("\\s*(?:~~|=~|~|\\+|\n)\\s*") |>
      unlist() |>
      unique()
    extra_vars <- extra_vars[!extra_vars %in% c(x_vars, y_vars)]
  }
  if (is.null(items) & is.null(item_loadings)) {
    if (is.null(extra)) {
      all_vars <- c(y_vars, x_vars)
    } else {
      all_vars <- c(y_vars, x_vars, extra_vars)
    }
    items <- all_vars[!all_vars %in% names(cfa_par)]
    if (length(items) > 0) {
      item_miss <- items[!items %in% names(data)]
      if (length(item_miss) > 0) {
        stop(
          paste0(
            "'", item_miss[1], "' is in 'path' but does not match either a ",
            "latent variable name, nor a variable name in 'data'."
          )
        )
      }
    }
  }
  item_in_cfa <- items[items %in% unlist(cfa_keys)]
  if (length(item_in_cfa) > 0) {
    stop(
      paste0(
        "'", item_in_cfa[1], "' is a structural variable but is also an item ",
        "in a CFA model. ",
        "Items that contribute to a CFA measurement model cannot ",
        "also be a structural variable."
      )
    )
  }
  y_miss <- y_vars[!y_vars %in% c(names(data), names(cfa_fit))]
  if (length(y_miss) > 0) {
    stop(
      paste0(
        "'", y_miss[1], "' is in 'path' but is not in 'items', 'cfa_fit', ",
        "or a named variable in 'data'. ",
        "Please check '", y_miss[1], "' matches the name of the relevant CFA, ",
        "item, or variable name as appropriate. ",
        "Note that latent variable names must match that of the factor ",
        "in the lavaan model (which could differ from the object name)."
      )
    )
  }
  x_miss <- x_vars[!x_vars %in% c(names(data), names(cfa_fit))]
  if (length(x_miss) > 0) {
    stop(
      paste0(
        "'", x_miss[1], "' is in 'path' but is not in 'items', 'cfa_fit', ",
        "or a named variable in 'data'. ",
        "Please check '", x_miss[1], "' matches the name of the relevant CFA, ",
        "item, or variable name as appropriate. ",
        "Note that latent variable names must match that of the factor ",
        "in the lavaan model (which could differ from the object name)."
      )
    )
  }
  if (!is.null(extra)) {
    if (length(extra_vars) > 0) {
      extra_miss <- extra_vars[!extra_vars %in% c(names(data), names(cfa_fit))]
      if (length(extra_miss) > 0) {
        # Note: it is possible to get here only if items is an entered argument.
        stop(
          paste0(
            "'", extra_miss[1], "' is in 'extra' but is not in 'items', ",
            "'cfa_fit', or a named variable in 'data'. ",
            "Please check '", extra_miss[1], "' matches the name of ",
            "the relevant CFA, item, or variable name as appropriate. ",
            "Note that latent variable names must match that of the factor ",
            "in the lavaan model (which could differ from the object name)."
          )
        )
      }
    }
  }
  if (!is.null(items)) {
    # mod_i <- paste0(
    #   lapply(
    #     stats::setNames(nm = items),
    #     function(i) {
    #       if (!is.null(item_loadings)) {
    #         if (length(item_loadings) == length(items)) {
    #           i_r <- paste(item_loadings[i], " * ")
    #         } else {
    #           i_r <- paste(item_loadings, " * ")
    #         }
    #       } else {
    #         i_r <- ""
    #       }
    #       i_l <- paste0(i, "_l")
    #       paste0(i_l, " =~ ", i_r, i)
    #     }
    #   ),
    #   collapse = "\n"
    # )
    if (length(items) > 1 & !orth_items) {
      item_cors <- paste(
        sapply(
          seq_along(items[-length(items)]),
          function(x) {
            paste0(
              "\n", items[x], " ~~ ",
              paste0(items[(x + 1):length(items)], collapse = " + ")
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
      x <- x[x$op %in% "=~", ]
      i <- x$lhs[x$op == "=~"] |> unique()
      xi <- lapply(i, function(y) x$rhs[x$lhs == y])
      # if (sum(i %in% y_vars) >= 1) {
      #   # Free residual variance for y factors.
      #   # Overly complex for single-factor CFA only but could be important for
      #   # multi-factor CFA or bifactor models.
      #   for (j in i) {
      #     x <- x[!(x$lhs == j & x$op == "~~" & x$rhs == j), ]
      #   }
      # }
      # # CFA model
      # paste(x$lhs, x$op, x$est, "*", x$rhs, collapse = "\n")
      mapply(
        function(j, k) {
          paste(j, "=~", paste(k, collapse = " + "))
        },
        j = i, k = xi, SIMPLIFY = FALSE
      ) |>
        paste(collapse = "\n")
    }
  ) |>
    paste0(collapse = "\n") |>
    paste0(item_cors) |>
    paste0("\n", path) |>
    paste0("\n", extra)
  fit <- sem.check(
    setNames(list(mod), nm = name), data = data,
    keys_s = setNames(list(c(unlist(cfa_keys), items)), nm = name),
    fit_save = fit_save, fit_measures = fit_measures,
    miss = miss, est = est, orthogonal = TRUE, std = TRUE,
    name = name, check = check, save_out = save_out, use_sam = TRUE
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
  cors <- x[x$op == "~~" & x$lhs != x$rhs, ]
  if (fit_save) {
    if (nrow(cors) != 0) {
      return(
        list(
          fit = fit$fit[[name]],
          par_std = fit$par_std[[name]],
          fit_measures = fit$fit_measures[name, ],
          b = b,
          r2 = r2,
          cors = cors
        )
      )
    } else {
      message(
        paste(
          "No correlations were found in the model. If that seems incorrect,",
          "please check you have included them in 'extra'."
        )
      )
      return(
        list(
          fit = fit$fit[[name]],
          par_std = fit$par_std[[name]],
          fit_measures = fit$fit_measures[name, ],
          b = b,
          r2 = r2
        )
      )
    }
  } else {
    if (nrow(cors) != 0) {
      return(
        list(
          fit = fit$fit[[name]],
          par_std = fit$par_std[[name]],
          b = b,
          r2 = r2,
          cors = cors
        )
      )
    } else {
      message(
        paste(
          "No correlations were found in the model. If that seems incorrect,",
          "please check you have included them in 'extra'."
        )
      )
      return(
        list(
          fit = fit$fit[[name]],
          par_std = fit$par_std[[name]],
          b = b,
          r2 = r2
        )
      )
    }
  }
}
