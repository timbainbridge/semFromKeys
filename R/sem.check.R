#' Runs lavaan models after checking for code or data changes.
#'
#' @description
#' sem.check produces outputs from a series of lavaan models. For each model,
#' the code checks for previously saved model code and a hash of data and,
#' if they exist and match values for current code and data, the model is not
#' run, the previous output is loaded instead.
#' If either the code has changed, the hash of the data has changed,
#' or either one does not exist, then the models are run as normal.
#'
#' @param mods Named list of lavaan models to run.
#' @param dat
#' The data. This must include all observed variables used in any of the models.
#' @param name A name for the collection of models to be run.
#' @param kl_s A named keys list matching the names and length of mod.
#' @param kl_e A named keys list of the factors in an ESEM to be included.
#' @param std
#' `TRUE` to save standardised parameter estimates;
#' `FALSE` to save unstandardised parameters estimates.
#' @param fit_save `TRUE` to save model fit measures. `FALSE` otherwise.
#' @param fit_measures
#' A vector of fit measures to save, or `NULL` to select all fit measures.
#' Defaults to `NULL`. Irrelevant if `fit_save = FALSE`.
#' @param target
#' Set to `TRUE` if the model is an ESEM with a target rotation.
#' Set to `FALSE` otherwise.
#' @param out_dir
#' The directory where all function outputs will be saved. Defaults to 'output'.
#' @param hash_dir
#' A subdirectory of `out_dir` where data hashes are saved.
#' Defaults to 'hashes'.
#' @param orthogonal
#' Sets the `orthogonal` param, as per lavaan. Defaults to `FALSE`.
#' @param miss Sets the `missing` param, as per lavaan. Defaults to 'ML'.
#' @param std.lv Sets the `std.lv` param, as per lavaan. Defaults to `FALSE`.
#'
#' @return
#' Returns a list of lists.
#' The elements are a list of lavaan model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' and, if `fit_save` is not FALSE, a matrix of fit measures for each model.
#'
#' @details
#' The function is largely intended to be used as a helper function to upstream
#' functions.
#'
#' Matching the philosophy of the package, the function is designed to run for
#' multiple models with a similar design. If you are using the function for a
#' single model, transform inputs into lists as appropriate.
#'
#' Note that when the model includes at least one structural element,
#' the function accounts for interpretational confounding using Burt's (1976)
#' 2-stage procedure.
#'
#' @references
#' Burt, R. S. (1976).
#' Interpretational confounding of unobserved variables in Structural Equation
#' Models. Sociological Methods & Research, 5(1): 3-52.
#'
#' @importFrom gsubfn gsubfn
#' @importFrom lavaan sem
#' @importFrom lavaan standardizedSolution
#' @importFrom lavaan parameterEstimates
#' @importFrom lavaan fitMeasures
#' @importFrom openssl md5
#' @export

sem.check <- function(
    mods, dat, name, kl_s, kl_e, std = TRUE,
    fit_save = FALSE, fit_measures = NULL, target = FALSE,
    out_dir = "output", hash_dir = "hashes",
    orthogonal = FALSE, miss = "ML", std.lv = FALSE
) {
  if (!is.logical(fit_save)) {
    stop("`fit_save` is not logical. It should be `TRUE` or `FALSE`.")
  }
  if (!is.character(name)) {
    stop("`name` is not a character string.")
  }
  if (!is.character(out_dir)) {
    stop("`out_dir` is not a character string.")
  }
  if (!is.character(hash_dir)) {
    stop("`hash_dir` is not a character string.")
  }
  if (!is.logical(std)) {
    stop("`std` is not logical. It should be `TRUE` or `FALSE`.")
  }
  if (!is.logical(target)) {
    stop("`target` is not logical. It should be `TRUE` or `FALSE`.")
  }
  if (!is.logical(orthogonal)) {
    stop("`orthogonal` is not logical. It should be `TRUE` or `FALSE`.")
  }
  if (!is.logical(std.lv)) {
    stop("`std.lv` is not logical. It should be `TRUE` or `FALSE`.")
  }
  if (!is.list(mod)) {
    stop(
      paste(
        "`mod` is not a list.",
        "If you are trying to run a single model, you will have to make a",
        "length 1 list",
        "(e.g., mod = `list(mod_name = mod)`)"
      )
    )
  }
  dat <- as.data.frame(dat)
  if (!is.data.frame(dat)) {
    stop(
      paste(
        "`dat` cannot be coerced into a data.frame.",
        "Please use a data format that can be coerced into a data.frame."
      )
    )
  }
  if (!is.list(kl_s)) {
    stop(
      paste(
        "`kl_s` is not a list. Please ensure that kl_s is specified correctly.",
        "See the function's help for further information."
      )
    )
  }
  if (!is.list(kl_e)) {
    stop(
      paste(
        "`kl_e` is not a list. Please ensure that kl_s is specified correctly.",
        "See the function's help for further information."
      )
    )
  }
  if (sum(!(unlist(kl_s)) %in% colnames(dat)) > 0) {
    una_items <- unlist(kl_s)[!(unlist(kl_s)) %in% colnames(dat)]
    message(paste0("  ", una_items, collapse = "  \n"))
    stop(
      paste(
        "The above items are in `kl_s` but they are not in `dat`.",
        "Ensure that dat column names and keys list item names match."
      )
    )
  }
  if (sum(!(unlist(kl_e)) %in% colnames(dat)) > 0) {
    una_items <- unlist(kl_e)[!(unlist(kl_e)) %in% colnames(dat)]
    message(paste0("  ", una_items, collapse = "  \n"))
    stop(
      paste(
        "The above items are in `kl_e` but they are not in `dat`.",
        "Ensure that dat column names and keys list item names match."
      )
    )
  }
  # TODO: Add check for fit_measures, and miss.
  # TODO: For fit_measures, what error does lavaan spit out when confronted with
  #       A string that is not a fit measure? Can we simply rely on lavaan's
  #       errors?
  # TODO: What options are available for miss in lavaan? Can we simply rely on
  #       lavaan's errors?
  if (!dir.exists(out_dir)) dir.create(out_dir)
  if (!dir.exists(file.path(out_dir, name))) {
    dir.create(file.path(out_dir, name))
  }
  if (!dir.exists(file.path(out_dir, hash_dir))) {
    dir.create(file.path(out_dir, hash_dir))
  }
  # Tell user which model set is running
  message(paste("Running", name))
  # Load hashes and prior models
  m0 <-
    if (file.exists(file.path(out_dir, name, paste0(name, "_m.rds")))) {
      tmp <- readRDS(file.path(out_dir, name, paste0(name, "_m.rds")))
      # Remove spaces
      lapply(tmp, function(x) gsub(" ", "", x))
    } else FALSE
  hash_d0 <-
    if (file.exists(
      file.path(out_dir, hash_dir, paste0("hash_", name, "_d.rds"))
    )) {
      readRDS(file.path(out_dir, hash_dir, paste0("hash_", name, "_d.rds")))
    } else FALSE
  # Create hashes
  hash_d <- sapply(
    names(mods),
    function(x) md5(paste(dat[c(kl_s[[x]], unlist(kl_e))], collapse = ""))
  )
  # Compare to previous hashes
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
          gsubfn("([0-9]\\.[0-9]+)",
                 ~format(round(as.numeric(x), 4), nsmall = 4),
                 mods0[[x]]) ==
            gsubfn("([0-9]\\.[0-9]+)",
                   ~format(round(as.numeric(x), 4), nsmall = 4),
                   m0[[x]])
        } else FALSE
      }
    )
  } else FALSE
  # Load old object if it exists
  fit0 <-
    if (file.exists(file.path(out_dir, name, paste0(name, "_fit.rds")))) {
      readRDS(file.path(out_dir, name, paste0(name, "_fit.rds")))
    } else FALSE
  if (std == TRUE) {
    par0 <-
      if (file.exists(
        file.path(out_dir, name, paste0(name, "_par_std.rds"))
      )) {
        readRDS(file.path(out_dir, name, paste0(name, "_par_std.rds")))
      } else FALSE
  } else {
    par0 <-
      if (file.exists(
        file.path(out_dir, name, paste0(name, "_par.rds"))
      )) {
        readRDS(file.path(out_dir, name, paste0(name, "_par.rds")))
      } else FALSE
  }
  if (fit_save) {
    fit_m0 <- if (file.exists(
      file.path(out_dir, name, paste0(name, "_fit_m.rds"))
    )) {
      readRDS(file.path(out_dir, name, paste0(name, "_fit_m.rds")))
    } else FALSE
  }
  # If hashes are correct but the fitted object is moved or deleted, then the
  # fit0 object will be FALSE. To ensure this doesn't break everything, check
  # that the fit0 object is a lavaan object. If not, run again.
  fit_type <- sapply(
    names(mods),
    function(x) ifelse(x %in% names(fit0), class(fit0[[x]]) == "lavaan", FALSE)
  )
  par_type <- sapply(
    names(mods),
    function(x) {
      ifelse(
        x %in% names(par0), (class(par0[[x]]) == "lavaan.data.frame")[1], FALSE
      )
    }
  )
  # Run
  # tmp <- list()
  fit <- mapply(
    function(m1, hash_d1, n_mod, mods1, ft, n) {
      if (hash_d1 & m1 & ft) {
        fit0[[n_mod]]
      } else {
        if (length(mods) <= 1000) {
          # Print progress for every model
          message(paste0(n, " / ", length(mods), " Run ", name, ": ", n_mod))
        } else {
          # Print progress for every 10th model (starting from 1)
          if (n %in% round((0:(length(mods)/10)*10) + 1, 0)) {
            message(paste0(n, " / ", length(mods), " Run ", name, ": ", n_mod))
          }
        }
        if (!target |> is.matrix()) {
          sem(
            model   = mods1,
            data    = dat[c(kl_s[[n_mod]], unlist(kl_e))],
            missing = miss,
            std.lv  = std.lv,
            orthogonal = orthogonal
          )
        } else {
          sem(
            model   = mods1,
            data    = dat[c(kl_s[[n_mod]], unlist(kl_e))],
            missing = miss,
            std.lv  = std.lv,
            orthogonal = orthogonal,
            rotation = "target",
            rotation.args = list(
              rstarts = 30, row.weights = "none", algorithm = "gpa",
              std.ov = TRUE, target = target
            )
          )
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
  par <- mapply(
    function(m1, hash_d1, n_mod, fit1, pt, n) {
      if (hash_d1 & m1 & pt) {
        par0[[n_mod]]
      } else {
        if (length(fit) <= 1000) {
          # Print progress for every model
          message(paste0(n, " / ", length(fit), " Par ", name, ": ", n_mod))
        } else {
          # Print progress for every 10th model (starting from 1)
          if (n %in% round((0:(length(fit)/10)*10) + 1, 0)) {
            message(paste0(n, " / ", length(fit), " Par ", name, ": ", n_mod))
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
    fit_m1 <- mapply(
      function(m1, hash_d1, n_mod, fit1, n) {
        # If the model's the same and fit measures exist...
        if (hash_d1 & m1 & (n_mod %in% names(fit_m0))) {
          # If fit_measures == NULL
          if (is.null(fit_measures)) {
            # If existing fit_m0 includes all fit stats...
            if (length(fit_m0[[n_mod]]) >= 55) {
              # Return existing
              fit_m0[[n_mod]]
            } else {
              # Else, recalulate
              message(
                paste0(
                  n, " / ", length(fit), " Fit Measures ", name, ": ", n_mod
                )
              )
              fitMeasures(fit1)
            }
          } else {
            # If fit_m0 includes all required fit measures
            if (
              sum(fit_measures %in% names(fit_m0[[n_mod]])) ==
              length(fit_measures)
            ) {
              # Return existing (with only required fit measures)
              fit_m0[[n_mod]][fit_measures]
            } else {
              # Else, recalculate with required fit measures
              message(
                paste0(
                  n, " / ", length(fit), " Fit Measures ", name, ": ", n_mod
                )
              )
              fitMeasures(fit1, fit.measures = fit_measures)
            }
          }
        } else {
          # If the model has changed, recalculate with required fit measures
          message(
            paste0(
              n, " / ", length(fit), " Fit Measures ", name, ": ", n_mod
            )
          )
          if (is.null(fit_measures)) {
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
  }
  # Save models (so they can be read in next time)
  saveRDS(fit, file.path(out_dir, name, paste0(name, "_fit.rds")))
  if (std) {
    saveRDS(par, file.path(out_dir, name, paste0(name, "_par_std.rds")))
  } else {
    saveRDS(par, file.path(out_dir, name, paste0(name, "_par.rds")))
  }
  if (fit_save) {
    saveRDS(fit_m1, file.path(out_dir, name, paste0(name, "_fit_m.rds")))
  }
  # Save mod + hash
  saveRDS(mods, file.path(out_dir, name, paste0(name, "_m.rds")))
  saveRDS(hash_d, file.path(out_dir, hash_dir, paste0("hash_", name, "_d.rds")))
  # Return
  if (std) {
    if (fit_save) {
      return(list(fit = fit, par_std = par, fit_measures = fit_m1))
    } else {
      return(list(fit = fit, par_std = par))
    }
  } else {
    if (fit_save) {
      return(list(fit = fit, par = par, fit_measures = do.call(rbind, fit_m1)))
    } else {
      return(list(fit = fit, par = par))
    }
  }
}
