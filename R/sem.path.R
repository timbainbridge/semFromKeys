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
#' For example, the argument can be used to set constraints, fix parameters to
#' specific values, or allow correlations between item residuals or latent
#' variables.
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
#' (SAM) procedure to control for interpretational confounding.
#'
#' Like lavaan, the model assumes independent variables in the structural model
#' should be allowed to correlate by default but extends the treatment to
#' single-item variables. As a corollary, if you do not want any combination of
#' independent variables to correlate freely, you will have to specify that in
#' 'extra' (see examples).
#'
#' The model relies on [sem.check] for the back-end of running the models.
#' This enables saving inputs and outputs from model runs
#' (with `save_out = TRUE`) and checking to see if anything has changed from
#' prior runs before running again (with `check = TRUE`).
#' The functionality was included for a number of very slow models or a lot of
#' faster models, such that time spent rerunning them would be onerous.
#' Given `sem.path` runs a single model at a time, it is unlikely to be
#' necessary except for extremely complex models.
#' For further details on how this works,
#' see the [sem.check] function  documentation.
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
#' Although this latter issue may be solvable for Nagy and colleagues' (2017)
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
#' Note that latent variables that are included in a measurement model that are
#' not part of a regression path are assumed (by lavaan) to be unrelated,
#' even if explicitly freed in the code. In most cases, you would not want such
#' variables; however, if you include a bi-factor model, group variables that
#' are not part of the structural path model will be assumed to be unrelated to
#' other structural variables. Similarly, if you are attempting to follow Hayes
#' (2021) recommendations for incremental validity, then there is no way to
#' force the focal variable to correlate with the outcome using SAM.
#' `sem.path` should, therefore, *NOT* be used with bifactor models or to test
#' for incremental validity using Hayes (2021) method.
#'
#' Finally, the function also does not currently work correctly with ESEM.
#' The latest lavaan version (0.7-2 at time of writing) and earlier incorrectly
#' treats factor covariances as equal in SAM with `sam_method = "local"`.
#' In the future, the function may change to allow users to select
#' `sem_method = "global"` or to select "global" dynamically when an ESEM is
#' included but neither of these is currently implemented, so you should also
#' *NOT* include ESEM in `sem.path` models.
#'
#' @seealso [lavaan::sam], [sem.check]
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
#' Hayes, T. (2021).
#' R-squared change in structural equation models with latent variables and
#' missing data, 53(5), 2127-2157.
#' https://doi.org/10.3758/s13428-020-01532-y.
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
#' @examples
#' # Create CFA keys
#' keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#' keys <- sapply(
#'   keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' # Run CFA models
#' cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = FALSE)
#' # Run a path model with grit_c allowed to correlate with hope_p's residual
#' # and the correlation between hope_a and grit_c constrained to 0.
#' # Note: "\n" indicates a new line and is interpreted identically to an new
#' # line by lavaan.
#' sem_fit <- sem.path(
#'   path = "grit_p ~ grit_c + hope_p + bfi_c1_1\nhope_p ~ hope_a",
#'   data = BFIGritHope,
#'   cfa_fit = cfa_fit$fit,
#'   extra = "grit_c ~~ hope_p\nhope_a ~~ 0*grit_c"
#' )
#' # Examine results
#' summary(sem_fit)      # Standard lavaan summary
#' sem_fit$b             # Standardised regression path coefficients
#' sem_fit$r2            # R^2 values
#' sem_fit$cors          # Correlations
#' sem_fit$fit_measures  # Fit measures

sem.path <- function(
    path, data, cfa_fit, extra = NULL,
    fit_save = TRUE, fit_measures = "all", miss = "default", est = "default",
    name = "sam", check = FALSE, save_out = FALSE
) {
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
  path_vars0 <- gsub("((\\+|~~|~).*?(\\*))", " ", path)
  # Remove punctuation
  path_vars1 <- gsub("\\+|~|\n", " ", path_vars0)
  path_vars <- unique(unlist(stringr::str_split(path_vars1, " +")))
  y_vars <-
    unlist(sapply(path_vars, function(x) x[grep(paste0(x, "( |)~"), path)]))
  x_vars <- path_vars[!path_vars %in% y_vars]
  if (!is.null(extra)) {
    # Removal all fixed values and parameter names
    extra_vars0 <- gsub("((\\+|~~|~).*?(\\*))", " ", extra)
    # Remove punctuation
    extra_vars1 <- gsub("\\+|~|\n", " ", extra_vars0)
    extra_vars2 <- unique(unlist(stringr::str_split(extra_vars1, " +")))
    extra_vars <- extra_vars2[!extra_vars2 %in% c(x_vars, y_vars)]
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
  if (length(items_s) > 0) {
    item_in_cfa <- items_s[items_s %in% unlist(cfa_keys)]
    if (length(item_in_cfa) > 0) {
      stop(
        paste0(
          "'", item_in_cfa[1], "' is a structural variable but is also an ",
          "item in a CFA model. ",
          "Items that contribute to a CFA measurement model cannot ",
          "also be a structural variable."
        )
      )
    }
  }
  if (!is.null(extra)) {
    if (length(extra_vars) > 0) {
      items_m <- extra_vars[!extra_vars %in% names(cfa_keys)]
      items <- c(items_s, items_m)
      if (length(items_m) > 0) {
        item_miss_m <- items_m[!items_m %in% names(data)]
        if (length(item_miss_m) > 0) {
          stop(
            paste0(
              "'", item_miss_m[1], "' is in 'extra' but does not match ",
              "either a latent variable name, nor a variable name in 'data'."
            )
          )
        }
      }
    } else {
      items <- items_s
    }
  } else {
    items <- items_s
  }
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
  # Full structural model
  mod0 <- paste0(
    sapply(
      cfa_par,
      function(x) {
        x <- x[x$op %in% "=~", ]
        i <- unique(x$lhs[x$op == "=~"])
        xi <- lapply(i, function(y) x$rhs[x$lhs == y])
        paste(
          mapply(
            function(j, k) paste(j, "=~", paste(k, collapse = " + ")),
            j = i, k = xi, SIMPLIFY = FALSE
          ),
          collapse = "\n"
        )
      }
    ),
    collapse = "\n"
  )
  mod1 <- paste0(mod0, "\n", x_cors, "\n", path, "\n", extra)
  mod <- list(mod1)
  names(mod) <- name
  keys_s <- list(c(unlist(cfa_keys), items))
  names(keys_s) <- name
  fit <- sem.check(
    mod, data = data, keys_s = keys_s,
    fit_save = fit_save, fit_measures = fit_measures,
    miss = miss, est = est, std = TRUE, ordered = NULL,
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
