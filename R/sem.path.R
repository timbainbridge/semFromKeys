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
#'
#' @return
#' Returns a list of length 5 (if `fit_save = FALSE`) or
#' 6 (if `fit_save = TRUE`).
#' The elements of the list are: a lavaan model output object (fit);
#' a data frame of standardised parameter estimates (par_std);
#' a vector of fit measures for the model if `fit_save = TRUE` (fit_measures);
#' a data frame of structural regression parameters (b);
#' a data frame of R-squared values (r2); and
#' a data frame of structural correlation parameters (cors).
#'
#' @details
#' The function runs an SEM with fitted CFA models, a specified path, and,
#' optionally, 'extra' lavaan code.
#' The function uses Rosseel and Loh's (2022) "Structure-After-Measurement"
#' (SAM) procedure to control for interpretational confounding and includes the
#' `orth_x` argument to make it easy to allow all independent variables
#' (i.e., variables not predicted by any other variables) to correlate without
#' having to explicitly allow them.
#'
#' The model relies on [sem.check()] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' Given `sem.path` runs a single model at a time, it is unlikely to be
#' necessary except for extremely complex models.
#' For further details on how this works,
#' see the [sem.check()] function  documentation.
#'
#' In SEM, standard methods do not distinguish between measurement and
#' structural parameters. As a result, measurement model parameters can change
#' with the addition of theoretically unrelated constructs in a structural
#' model, and can change differently for different sets of unrelated constructs.
#' This means that the unrelated constructs are changing the interpretation of
#' the latent variable, which Burt (1976) referred to as
#' "interpretational confounding".
#'
#' There are various ways to deal with interpretational confounding.
#' The standard solution (other than ignoring it) is to create good fitting
#' measurement models first, then freely estimate the structural model
#' with checks to ensure adequate fit of the model and that measurement
#' parameters do not substantially change with different combinations of
#' factors. This is sometimes a good solution, but, in other cases, it is not.
#' For example, if the measurement model was for a well-established scale and it
#' requires changing, then it loses easy comparison with past research.
#' This issue is most clearly relevant when changes to a measurement model
#' require entirely different factors, or items to be removed but it is still an
#' issue for less dramatic changes. When a single scale is being assessed,
#' these issues can be resolved by suggesting a thorough evaluation of the scale
#' and, perhaps, the suggestion of a new measurement model or a new scale for a
#' particular population; however, when many scales are being assessed this
#' solution is impractical, and may not solve the interpretational confounding
#' issue regardless.
#'
#' An alternative solution, proposed by Burt (1976) is to fix measurement model
#' parameters in a model estimating structural parameters.
#' This method means that misspecification of one measurement model cannot
#' affect other measurement models and that the interpretation of measured
#' constructs cannot change based on unrelated factors.
#' However, it is not a perfect solution because, by fixing measurement
#' parameters, uncertainty in their estimation is neglected
#' (e.g., Nagy et al., 2017), which results in biased standard errors and fit
#' statistics.
#'
#' A third option was proposed by Nagy and colleagues (2017),
#' who introduced an extension procedure such that item residuals are allowed to
#' correlate with external variables (or factors).
#' To make the model identifiable, these relationships are constrained using
#' one of a number of methods. If the sums of squares of correlations between
#' all combinations of factors' items and external factors are minimised,
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
#' Although this latter issue may be solveable for Nagy and colleagues' (2017)
#' method, a more practical solution to these issues was proposed by
#' Rosseel and Loh (2022) with their SAM approach. This method essentially
#' follows Burt's (1976) method but adjust the procedure to overcome its issues.
#' They distinguish two SAM varieties--"local SAM" and "global SAM".
#' Local SAM uses the observed summary statistics of the parameters of the
#' measurement models to generate mean and covariance matrices to use in the
#' structural model, which preserves the structure of the measurement models
#' while also preserving the uncertainty.
#' Global SAM treats the measurement parameters as given, but corrects the
#' standard errors of the structural model.
#'
#' Given its practicality, `sem.path` uses the local SAM method.
#' Local SAM was selected over the global SAM because Rosseel and Loh (2022)
#' "recommend local SAM over global SAM whenever possible."
#' (p. 21 of the pre-print version).
#'
#' Note that variables that are included as structural parameters that are only
#' involved in
#'
#' @seealso [sam()] [sem.check()]
#'
#' @importFrom stringr str_split
#' @export
#'
#' @references
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1), 3-52.
#' https://doi.org/10.1177/004912417600500101.
#'
#' Nagy, G., Brunner, M., Lüdtke, O., and Greiff, S. (2017).
#' Extension Procedures for Confirmatory Factor Analysis.
#' Journal of Experimental Education, 85(4).
#' https://doi.org/10.1080/00220973.2016.1260524.
#'
#' Rosseel, Y. & Loh, W. W. (2022).
#' A structural after measurement approach to structural equation modeling.
#' Psychological Methods, 29(3), 561-588.
#' https://doi.org/10.1037/met0000503.
#'

sem.path <- function(
    path, data, cfa_fit, extra = NULL,
    fit_save = TRUE, fit_measures = "all", miss = "ML", est = "default",
    name = "sam", check = FALSE, save_out = FALSE
) {
  # item_miss <- items[!items %in% names(data)]
  # if (!is.null(items)) {
  #   if (length(item_miss) > 0) {
  #     stop(
  #       paste0(
  #         "'", item_miss[1], "' is in 'items' but does not match ",
  #         "a variable name in 'data'."
  #       )
  #     )
  #   }
  # }

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
  path_vars <- gsub("((\\+|~~|~).*?(\\*))", " ", path) |>
    # Remove punctuation
    gsub("\\+|~|\n", " ", x = _) |>
    stringr::str_split(" +") |>
    unlist() |>
    unique()
  y_vars <- sapply(path_vars, function(x) x[grep(paste0(x, "( |)~"), path)]) |>
    unlist()
  x_vars <- path_vars[!path_vars %in% y_vars]
  if (!is.null(extra)) {
    # Removal all fixed values and parameter names
    extra_vars <- gsub("((\\+|~~|~).*?(\\*))", " ", extra) |>
      # Remove punctuation
      gsub("\\+|~|\n", " ", x = _) |>
      stringr::str_split(" +") |>
      unlist() |>
      unique()
    extra_vars <- extra_vars[!extra_vars %in% c(x_vars, y_vars)]
  }
  items_s <- path_vars[!path_vars %in% names(cfa_fit)]
  if (length(items_s) > 0) {
    item_miss <- items_s[!items_s %in% names(data)]
    if (length(item_miss) > 0) {
      stop(
        paste0(
          "'", item_miss[1], "' is in 'path' but does not match ",
          "either a latent variable name, nor a variable name in 'data'."
        )
      )
    }
  }
  item_in_cfa <- items_s[items_s %in% unlist(cfa_fit)]
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
  items_m <- extra_vars[!extra_vars %in% names(cfa_fit)]
  if (length(items_m) > 0) {
    item_miss_m <- items_m[!items_m %in% names(data)]
    if (length(item_miss_m) > 0) {
      stop(
        paste0(
          "'", item_miss[1], "' is in 'extra' but does not match ",
          "either a latent variable name, nor a variable name in 'data'."
        )
      )
    }
  }
  # if (!is.null(extra)) {
  #   if (length(extra_vars) > 0) {
  #     extra_miss <- extra_vars[!extra_vars %in% c(names(data), names(cfa_fit))]
  #     if (length(extra_miss) > 0) {
  #       # Note: it is possible to get here only if items is an entered argument.
  #       stop(
  #         paste0(
  #           "'", extra_miss[1], "' is in 'extra' but is not in 'items', ",
  #           "'cfa_fit', or a named variable in 'data'. ",
  #           "Please check '", extra_miss[1], "' matches the name of ",
  #           "the relevant CFA, item, or variable name as appropriate. ",
  #           "Note that latent variable names must match that of the factor ",
  #           "in the lavaan model (which could differ from the object name)."
  #         )
  #       )
  #     }
  #   }
  # }
  # if (!is.null(items)) {
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
    if (length(x_vars) > 1) {
      x_cors <- paste(
        sapply(
          seq_along(x_vars[-length(x_vars)]),
          function(x) {
            paste0(
              x_vars[x], " ~~ ",
              paste0(x_vars[(x + 1):length(x_vars)], collapse = " + ")
            )
          }
        ),
        collapse = "\n"
      )
    } else {
      x_cors <- NULL
    }
  # } else {
  #   x_cors <- NULL
  #   mod_i <- NULL
  # }
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
    paste0("\n", x_cors) |>
    paste0("\n", path) |>
    paste0("\n", extra)
  fit <- sem.check(
    setNames(list(mod), nm = name), data = data,
    keys_s = setNames(list(c(unlist(cfa_keys), items)), nm = name),
    fit_save = fit_save, fit_measures = fit_measures,
    miss = miss, est = est, std = TRUE,
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
