#' Runs lavaan models after checking for code or data changes.
#'
#' @description
#' sem.check takes a model list, keys lists, and data, and
#' produces outputs from a series of lavaan models.
#' The function is primarily used as a function to be called by other function
#' within the package but it can also be run independently.
#'
#' For each model, the code optionally checks for previously saved model code,
#' a hash of data, and important parameter inputs.
#' If they exist and match values for the current code and data,
#' the model is not run, the previous output is loaded instead.
#' If either the code has changed, the hash of the data has changed, or
#' important parameter inputs have change, or any of these do not exist,
#' then the models are run as normal.
#'
#' @param mods A named list of lavaan models to run.
#' @param data
#' A dataframe or object coercible to a dataframe.
#' Data must include all observed variables used in any of the models.
#' @param keys_s A named keys list matching the names and length of mod.
#' @param keys_e A named keys list of the factors in an ESEM to be included.
#' @param std
#' Logical.
#' `TRUE` indicates that standardised parameter estimates should be saved;
#' `FALSE` indicates that unstandardised parameters estimates should be saved.
#' @param fit_save
#' Logical.
#' `TRUE` indicates model fit measures should be included in the output;
#' `FALSE` indicates model fit measures should not be included in the output.
#' `FALSE` may be desirable when fit measures are of little interest and when
#' they may take a long time to estimate.
#' Fit can still be examined for individual models with,
#' `lavaan::fitMeasures(fit$fit$[model name])`.
#' @param fit_measures
#' A vector of fit measures to save or 'all' to select all fit measures,
#' as per the `fit.measures` parameter from lavaan's [lavaan::fitMeasures()]
#' function.
#' Defaults to 'all'. Irrelevant if `fit_save = FALSE`.
#' @param target
#' A matrix indicating a rotation target,
#' as used in the `rotation.args` argument in lavaan (see [lavaan::efa()].
#' If `NULL` (default),
#' target is not specified and lavaan uses default behaviour.
#' Irrelevant when the model does not include an EFA or ESEM.
#' @param name
#' A string indicating a subdirectory where model outputs will be saved when
#' `save_out = TRUE` and checked against when `check = TRUE`.
#' Defaults to "sem".
#' The name should be unique for each set of models, or outputs from calls with
#' the same name will be overwritten.
#' @param orthogonal
#' Logical.
#' Sets the `orthogonal` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' `TRUE` indicates that unspecified latent variable correlations should be
#' fixed at 0;
#' `FALSE` indicates that unspecified latent variable correlations should be
#' freely estimated.
#' Defaults to `FALSE`.
#' @param miss
#' A string.
#' Sets the `missing` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' Defaults to 'ML'.
#' @param est
#' A string.
#' Sets the `estimator` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' The default ('default') uses the lavaan default for the model being run.
#' @param std.lv
#' Logical.
#' Sets the `std.lv` parameter, as per lavaan (see [lavaan::lavOptions()]).
#' `TRUE` indicates that factor variances should be fixed to 1.
#' `FALSE` indicates that loadings of the first items of factors should be fixed
#' to 1.
#' Defaults to `FALSE`.
#' @param check
#' Logical.
#' `TRUE` indicates that current inputs should be compared to previous inputs
#' if they exist and that the model should not be rerun if nothing has changed;
#' `FALSE` indicates that these checks should not be made and the model should
#' be run regardless of the existence of previous inputs.
#' @param save_out
#' Logical.
#' `TRUE` indicates that model code, a hash of the data, important input
#' parameter values, and output will be saved;
#' `FALSE` indicates that nothing will be saved.
#' Selecting `save_out = TRUE` enables the function to not rerun models next
#' time if `check = TRUE` the next time the code is run and nothing has changed
#' in the meantime.
#'
#' @return
#' Returns a list of length 2 (if `fit_save = FALSE`) or
#' 3 (if `fit_save = TRUE`).
#' The elements of the list are: a list of lavaan model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_save = TRUE`, a matrix of fit measures for each model.
#'
#' @details
#' The function is largely intended to be used as a helper function to upstream
#' functions,
#' including [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()], and
#' [esem.from.mods()].
#' Although it is recommended to use the appropriate upstream function whenever
#' possible,
#' there are not (currently) options to do so when customised lavaan models are
#' required;
#' for example, when allowing two items' residuals to correlate in a CFA.
#' `sem.check()` can be used in these cases (see example).
#'
#' Matching the philosophy of the package, the function is designed to run for
#' multiple models with a similar design. If you are using the function for a
#' single model, transform inputs into lists as appropriate or simply use lavaan
#' without the assistance of semFromKeys.
#'
#' The function includes functionality designed to save time re-running code
#' when lots of slow models are included.
#' To do this, when `save_out = TRUE` and a cache directory has been set,
#' the model will save various inputs and outputs from the function call, and,
#' when `check = TRUE`, the model will look for any previously saved outputs
#' from earlier model runs in the same cache directory and
#' only run again if nothing has changed.
#' In cases where something has changed,
#' then the function will re-run the models where something has changed
#' (but it will not for those where nothing has changed).
#' Changes to arguments that influence all lavaan model runs (e.g., miss or est)
#' will trigger all models to be re-run.
#'
#' For either `save_out = TRUE` or `check = TRUE`,
#' the function will look for a cache directory set and created by the
#' [cache.setup()] function.
#' If a cache directory has not been set for the current session,
#' then the function will exit with an error suggesting that either
#' [cache.setup()] be run or `save_out` and `check` set to `FALSE`.
#'
#' When the cache directory is found and output from previous runs are detected,
#' the comparisons performed are for:
#' * model code;
#' * hashes of the data (using [openssl::md5()]);
#' * values of the `miss`, `est`, `std`, `std.lv`, and `orthogonal`parameters;
#' * the class of model objects (i.e., class lavaan); and,
#' * the class of parameter estimates (i.e., class lavaan.data.frame).
#'
#' For most applications, the checking feature can be safely ignored by not
#' saving outputs (i.e., `fit_save = FALSE`)
#' and not checking for past saves (i.e., `check = FALSE`, both default),
#' but may be beneficial in cases with lots of models or very slow models.
#' However, the functionality can be safely used for faster runs too.
#'
#' @seealso
#' [cfa.from.keys()], [efa.from.keys()], [bifactor.from.keys()], and
#' [esem.from.mods()]---all these function depend upon `sem.check()` to work;
#' [lavaan::sem()], which is used to estimate the models;
#' [lavaan::parameterEstimates()], which is used to estimate parameter values;
#' and [lavaan::fitMeasures()], which is used to estimate fit statistics when
#' `fit_save = TRUE`.
#'
#' @importFrom stringr str_replace_all
#' @importFrom lavaan sem
#' @importFrom lavaan standardizedSolution
#' @importFrom lavaan parameterEstimates
#' @importFrom lavaan fitMeasures
#' @importFrom lavaan summary
#' @importFrom openssl md5
#' @importFrom withr with_options
#' @export
#'
#' @examples
#' # Create CFA keys
#' keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#' keys <- sapply(
#'   keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#' )
#' # Create model code
#' mods <- mapply(
#'   x = keys, y = names(keys), SIMPLIFY = FALSE,
#'   FUN = function(x, y) paste(y, "=~", paste(x, collapse = " + "))
#' )
#' # Edit model code to add correlated residuals
#' mods[[1]] <- paste0(mods[[1]], "\ngrit_c_1 ~~ grit_c_2")
#' # Estimate the models with sem.check()
#' cfa_fit <- sem.check(mods, BFIGritHope, keys, name = "cfa", check = FALSE)

sem.check <- function(
    mods, data, keys_s = NULL, keys_e = NULL,
    fit_save = FALSE, fit_measures = "all",
    miss = "ML", est = "default", std.lv = FALSE, std = TRUE,
    orthogonal = FALSE, target = NULL,
    name = "sem", check = FALSE, save_out = FALSE
) {
  if (!is.logical(fit_save)) {
    stop("'fit_save' is not logical. It should be 'TRUE' or 'FALSE'.")
  }
  if (fit_save & !is.character(fit_measures)) {
    stop("'fit_measures' is not a character vector.")
  }
  if (!is.logical(check)) {
    stop("'check' is not logical. It should be 'TRUE' or 'FALSE'.")
  }
  if (!is.logical(save_out)) {
    stop("'save_out' is not logical. It should be 'TRUE' or 'FALSE'.")
  }
  if (save_out | check) {
    found <- FALSE
    for (i in 1:5) {
      env <- parent.frame(i)
      if (exists(".cache_env", envir = env)) {
        found <- TRUE
        break
      }
    }
    if (!found) {
      stop(
        paste(
          "A cache directory is not configured so cannot be cleaned.",
          "Use the 'cache.setup()' function to configure a directory to clean."
        )
      )
    }
    cache_dir <- get("cache_dir", envir = get(".cache_env", envir = env))
    if (!is.character(name)) {
      stop("'name' is not a character string.")
    }
  }
  if (!is.logical(std)) {
    stop("'std' is not logical. It should be 'TRUE' or 'FALSE'.")
  }
  if (!is.logical(orthogonal)) {
    stop("'orthogonal' is not logical. It should be 'TRUE' or 'FALSE'.")
  }
  if (!is.logical(std.lv)) {
    stop("'std.lv' is not logical. It should be 'TRUE' or 'FALSE'.")
  }
  if (!is.list(mods)) {
    stop(
      paste(
        "'mods' is not a list.",
        "If you are trying to run a single model, you will have to make a",
        "length 1 list",
        "(e.g., 'mods = list(mod_name = mod)')"
      )
    )
  }
  data <- as.data.frame(data)
  if (is.null(keys_s) & is.null(keys_e)) {
    stop("At least one of 'keys_s' or 'keys_e' must be specified.")
  }
  if (!is.list(keys_s) & !is.null(keys_s)) {
    stop(
      paste(
        "'keys_s' is not a list.",
        "Please ensure that keys_s is specified correctly.",
        "See the function's help for further information."
      )
    )
  }
  if (!is.null(keys_s)) {
    if (length(mods) != length(keys_s)) {
      stop(
        paste(
          "'keys_s' and 'mods' are not the same length.",
          "If 'keys_s' is supposed to be specified, then it should be the same",
          "length as 'mods'."
        )
      )
    }
    if (sum(table(names(keys_s)) > 1) > 0) {
      stop(
        paste(
          "At least two elements of 'keys_s' share the same name.",
          "Please ensure that all model names are unique."
        )
      )
    }
  }
  if (!is.list(keys_e) & !is.null(keys_e)) {
    stop(
      paste(
        "'keys_e' is not a list.",
        "Please ensure that keys_s is specified correctly.",
        "See the function's help for further information."
      )
    )
  }
  if (!is.null(keys_e)) {
    if (sum(table(names(keys_e)) > 1) > 0) {
      stop(
        paste(
          "At least two elements of 'keys_e' share the same name.",
          "Please ensure that all model names are unique."
        )
      )
    }
    if (sum(sapply(keys_e, function(x) length(x) < 2)) > 0) {
      short_factors <-
        names(keys_e)[sapply(keys_e, function(x) length(x) == 1)]
      warning(
        paste(
          paste0("    ", short_factors, collapse = "    \n"),
          "\n\nThe above factors have only 1 item.",
          "This is not necessarily a problem",
          "(assuming no more serious errors have occured)",
          "but it is unusual for EFAs and may not be what you intended to do."
        )
      )
    }
  }
  if (sum(!(unlist(keys_s)) %in% colnames(data)) > 0) {
    una_items <- unlist(keys_s)[!(unlist(keys_s)) %in% colnames(data)]
    stop(
      paste0(
        "The following items are in 'keys_s' but they are not in 'data':",
        "\n      ",
        paste0(una_items, collapse = "\n      "),
        paste0(
          "\n\nEnsure that the column names of 'data' and keys list item ",
          "names match and that 'data' is a dataframe or coercible to a ",
          "dataframe.\n",
          "If using bifactor.from.keys, ensure that you have not swapped keys ",
          "inadvertently (e.g., keys_g for keys_b)."
        )
      )
    )
  }
  if (sum(!(unlist(keys_e)) %in% colnames(data)) > 0) {
    una_items <- unlist(keys_e)[!(unlist(keys_e)) %in% colnames(data)]
    message(paste0("  ", una_items, collapse = "  \n"))
    stop(
      paste(
        "The above items are in 'keys_e' but they are not in 'data'.",
        "Ensure that data is a data frame (or coercible into a data frame)",
        "and that column names of 'data' and keys list item names match."
      )
    )
  }
  if (save_out) {
    if (!dir.exists(file.path(cache_dir, name))) {
      dir.create(file.path(cache_dir, name))
    }
  }
  # Tell user which model set is running
  if (check) {
    # Load hashes, prior models, and critical parameters
    m0 <-
      if (file.exists(file.path(cache_dir, name, paste0(name, "_mod.rds")))) {
        tmp <- readRDS(file.path(cache_dir, name, paste0(name, "_mod.rds")))
        # Remove spaces
        lapply(tmp, function(x) gsub(" ", "", x))
      } else FALSE
    params0 <-
      if (
        file.exists(file.path(cache_dir, name, paste0(name, "_params.rds")))
      ) {
        readRDS(file.path(cache_dir, name, paste0(name, "_params.rds")))
      } else FALSE
    hash_d0 <-
      if (file.exists(file.path(cache_dir, name, paste0(name, "_hash.rds")))) {
        readRDS(file.path(cache_dir, name, paste0(name, "_hash.rds")))
      } else FALSE
    # Create hashes
    hash_d <- sapply(
      names(mods),
      function(x) {
        md5(paste(data[c(keys_s[[x]], unlist(keys_e))], collapse = ""))
      }
    )
    # Compare to previous hashes
    # if includes possibility of hash_d0 <- c(FALSE, [hash]) for some reason.
    hash_d_test <- if (length(hash_d0) != 1 | hash_d0[[1]] != FALSE) {
      sapply(
        names(mods),
        function(x) {
          if (x %in% names(hash_d0)) hash_d[[x]] == hash_d0[[x]] else FALSE
        }
      )
    } else FALSE
    # Remove spaces for comparison
    mods0 <- lapply(mods, function(x) gsub(" ", "", x))
    m_test <- if (length(m0) != 1 | m0[[1]] != FALSE) {
      sapply(
        names(mods0),
        function(x) {
          if (x %in% names(m0)) {
            # Only check to 4-decimal places.
            str_replace_all(
              mods0[[x]],
              "([0-9]+\\.[0-9]+)",
              ~format(round(as.numeric(.x), 4), nsmall = 4)
            ) ==
              str_replace_all(
                m0[[x]],
                "([0-9]\\.[0-9]+)",
                ~format(round(as.numeric(.x), 4), nsmall = 4)
              )
          } else FALSE
        }
      )
    } else FALSE
    params <- c(
      miss = miss, est = est, std = std, std.lv = std.lv,
      orthogonal = orthogonal
    )
    param_test <- if (length(params0) == length(params)) {
      ifelse(sum(params0 != params) > 0, FALSE, TRUE)
    } else FALSE
    # Load old object if it exists
    fit0 <-
      if (file.exists(file.path(cache_dir, name, paste0(name, "_fit.rds")))) {
        readRDS(file.path(cache_dir, name, paste0(name, "_fit.rds")))
      } else FALSE
    if (std == TRUE) {
      if (
        file.exists(file.path(cache_dir, name, paste0(name, "_par_std.rds")))
      ) {
        par0 <- readRDS(
          file.path(cache_dir, name, paste0(name, "_par_std.rds"))
        )
      } else {
        par0 <- FALSE
      }
    } else {
      if (file.exists(file.path(cache_dir, name, paste0(name, "_par.rds")))) {
        par0 <- readRDS(file.path(cache_dir, name, paste0(name, "_par.rds")))
      } else {
        par0 <- FALSE
      }
    }
    if (file.exists(file.path(cache_dir, name, paste0(name, "_fit_m.rds")))) {
      fit_m0 <- readRDS(file.path(cache_dir, name, paste0(name, "_fit_m.rds")))
    } else {
      fit_m0 <- FALSE
    }
    # If hashes are correct but the fitted object is moved or deleted, then the
    # fit0 object will be FALSE. To ensure this doesn't break everything, check
    # that the fit0 object is a lavaan object. If not, run again.
    fit_type <- sapply(
      names(mods),
      function(x) {
        ifelse(x %in% names(fit0), inherits(fit0[[x]], "lavaan"), FALSE)
      }
    )
    par_type <- sapply(
      names(mods),
      function(x) {
        ifelse(
          x %in% names(par0),
          inherits(par0[[x]], "lavaan.data.frame")[1],
          FALSE
        )
      }
    )
  } else {
    m_test <- FALSE
    hash_d_test <- FALSE
    param_test <- FALSE
    fit_type <- FALSE
    par_type <- FALSE
    fit_m0 <- FALSE
  }
  # Run
  # Warnings required to be printed immediately so people know which model
  # caused the issue -> use with_options.
  message("Fitting models")
  fit <- with_options(
    list(warn = 1),
    mapply(
      function(m1, hash_d1, n_mod, mods1, ft, n) {
        if (hash_d1 & m1 & ft & param_test) {
          fit0[[n_mod]]
        } else {
          if (length(mods) <= 1000) {
            # Print progress for every model
            message(paste(n, "/", length(mods), " ", n_mod))
          } else {
            # Print progress for every 10th model (starting from 1)
            if (n %in% round((0:(length(mods)/10)*10) + 1, 0)) {
              message(paste(n, "/", length(mods), " ", n_mod))
            }
          }
          if (is.null(target)) {
            if (est == "default") {
              sem(
                model   = mods1,
                data    = data[c(keys_s[[n_mod]], unlist(keys_e))],
                missing = miss,
                std.lv  = std.lv,
                orthogonal = orthogonal
              )
            } else {
              sem(
                model   = mods1,
                data    = data[c(keys_s[[n_mod]], unlist(keys_e))],
                missing = miss,
                estimator = est,
                std.lv  = std.lv,
                orthogonal = orthogonal
              )
            }
          } else {
            if (est == "default") {
              sem(
                model   = mods1,
                data    = data[c(keys_s[[n_mod]], unlist(keys_e))],
                missing = miss,
                std.lv  = std.lv,
                rotation = "target",
                rotation.args = list(
                  rstarts = 30, row.weights = "none", algorithm = "gpa",
                  std.ov = TRUE, target = target, orthogonal = orthogonal
                )
              )
            } else {
              sem(
                model   = mods1,
                data    = data[c(keys_s[[n_mod]], unlist(keys_e))],
                missing = miss,
                estimator = est,
                std.lv  = std.lv,
                rotation = "target",
                rotation.args = list(
                  rstarts = 30, row.weights = "none", algorithm = "gpa",
                  std.ov = TRUE, target = target, orthogonal = orthogonal
                )
              )
            }
          }
        }
      },
      mods1 = mods,
      m1 = m_test,
      hash_d1 = hash_d_test,
      n_mod = names(mods),
      ft = fit_type,
      n = seq_along(mods),
      SIMPLIFY = FALSE
    )
  )
  message("Generating parameter estimates")
  par <- mapply(
    function(m1, hash_d1, n_mod, fit1, pt, n) {
      if (hash_d1 & m1 & pt & param_test) {
        out <- par0[[n_mod]]
      } else {
        if (length(fit) <= 1000) {
          # Print progress for every model
          message(paste(n, "/", length(fit), " ", n_mod))
        } else {
          # Print progress for every 10th model (starting from 1)
          if (n %in% round((0:(length(fit)/10)*10) + 1, 0)) {
            message(paste0(n, "/", length(fit), " ", n_mod))
          }
        }
        if (std) {
          standardizedSolution(fit1)
        } else {
          parameterEstimates(fit1, standardized = TRUE)
        }
      }
    },
    fit1 = fit,
    m1 = m_test,
    hash_d1 = hash_d_test,
    n_mod = names(fit),
    pt = par_type,
    n = seq_along(fit),
    SIMPLIFY = FALSE
  )
  if (fit_save) {
    message("Generating model fit statistics")
    fit_m1 <- mapply(
      function(m1, hash_d1, n_mod, fit1, n) {
        # If the model's the same and fit measures exist...
        if (hash_d1 & m1 & (n_mod %in% rownames(fit_m0)) & param_test) {
          # If fit_measures == "all"
          if (fit_measures[1] == "all") {
            # If existing fit_m0 includes all fit stats...
            if (length(fit_m0[n_mod, ]) >= 55) {
              # Return existing
              fit_m0[n_mod, ]
            } else {
              # Else, recalculate
              message(paste(n, "/", length(fit), " ", n_mod))
              fitMeasures(fit1)
            }
          } else {
            # If fit_m0 includes all required fit measures
            if (
              sum(fit_measures %in% names(fit_m0[n_mod, ])) ==
              length(fit_measures)
            ) {
              # Return existing (with only required fit measures)
              fit_m0[n_mod, fit_measures]
            } else {
              # Else, recalculate with required fit measures
              message(paste(n, "/", length(fit), " ", n_mod))
              fitMeasures(fit1, fit.measures = fit_measures)
            }
          }
        } else {
          # If the model has changed, recalculate with required fit measures
          message(paste(n, "/", length(fit), " ", n_mod))
          if (fit_measures[1] == "all") {
            fitMeasures(fit1)
          } else {
            fitMeasures(fit1, fit.measures = fit_measures)
          }
        }
      },
      fit1 = fit,
      m1 = m_test,
      hash_d1 = hash_d_test,
      n_mod = names(fit),
      n = seq_along(fit),
      SIMPLIFY = FALSE
    )
    if (length(unlist(fit_m1)) == 0) {
      warning(
        "All the fit measures you have specified are not recognised by lavaan."
      )
    } else {
      fit_m1 <- do.call(rbind, args = fit_m1)
      if (ncol(fit_m1) != length(fit_measures) & fit_measures[1] != "all") {
        warning(
          paste(
            "The following fit measures that you specified are not recognised",
            "by lavaan:\n     ",
            paste(
              fit_measures[!fit_measures %in% colnames(fit_m1)],
              collapse = "\n      "
            ),
            collapse = ""
          )
        )
      }
    }
  }
  # Save models (so they can be read in next time)
  if (save_out) {
    saveRDS(fit, file.path(cache_dir, name, paste0(name, "_fit.rds")))
    if (std) {
      saveRDS(par, file.path(cache_dir, name, paste0(name, "_par_std.rds")))
    } else {
      saveRDS(par, file.path(cache_dir, name, paste0(name, "_par.rds")))
    }
    if (fit_save) {
      saveRDS(fit_m1, file.path(cache_dir, name, paste0(name, "_fit_m.rds")))
    }
    # Save mod + hash + params
    saveRDS(mods, file.path(cache_dir, name, paste0(name, "_mod.rds")))
    saveRDS(hash_d, file.path(cache_dir, name, paste0(name, "_hash.rds")))
    saveRDS(params, file.path(cache_dir, name, paste0(name, "_params.rds")))
  }
  # Return
  if (std) {
    if (fit_save) {
      return(list(fit = fit, par_std = par, fit_measures = fit_m1))
    } else {
      return(list(fit = fit, par_std = par))
    }
  } else {
    if (fit_save) {
      return(list(fit = fit, par = par, fit_measures = fit_m1))
    } else {
      return(list(fit = fit, par = par))
    }
  }
}
