#' Runs ESEM based on CFA and EFA model outputs.
#'
#' `sem.path` runs latent variable structural equation models (ESEM) in lavaan.
#' The function takes lists of fitted CFA and/or bifactor lavaan model objects
#' for the measurement models and lavaan code for the structural model.
#'
#' @inheritParams sem.check
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
#'
#' @return
#' Returns a list of length 4 (if `fit_save = FALSE`) or
#' 5 (if `fit_save = TRUE`).
#' The elements of the list are: a list of lavaan model output objects;
#' a list of parameter estimates from the models (standardized if `std = TRUE`);
#' a matrix of fit measures for each model if `fit_save = TRUE`;
#' a list of regression beta parameters from each model;
#' and a dataframe of R-squared values from each model.


sem.path <- function(
    regr,     # Regression to run
    fit1,     # Fitted lavaan measurement models
    dat,      # Data -- must include all variables used in the model
    name,     # Name for the model
    kl_s,     # Named keys list of all the latent variables of mod
    mod_dir,  # Directory for saving model outputs
    extra = FALSE,  # Extra code to be added manually.
    # E.g., to set reliability for a single item outcome or to allow specific
    # covariances, notably semipartial correlations in incremental validity
    # analyses.
    hash_dir = here::here("output", "hashes"),  # Directory for hash files
    orthogonal = FALSE,  # Orthogonal or not, as per lavaan
    miss = "ML",     # Missing data treatment, as per lavaan
    est = "ML",      # Estimator to use, as per lavaan
    std.lv = FALSE,  # As per std.lv in lavaan
    ordered = NULL   # As per ordered in lavaan
) {
  if ((sapply(fit1, function(x) class(x) != "lavaan") |> sum()) > 0) {
    stop(
      "At least one of the elements of fit1 is not an object of class lavaan."
    )
  }
  if (!dir.exists(hash_dir)) dir.create(hash_dir)
  hash_d0 <-
    if (file.exists(
      here::here(hash_dir, paste0("hash_", name, "_d.rds"))
    )) {
      readRDS(here::here(hash_dir, paste0("hash_", name, "_d.rds")))
    } else FALSE
  # Create hashes
  require(openssl)
  hash_d <- md5(paste(dat[unlist(kl_s)], collapse = ""))
  # Compare to previous hash
  hash_d_test <- if (hash_d0 != FALSE) hash_d == hash_d0 else FALSE
  if (!dir.exists(mod_dir)) dir.create(mod_dir)
  m0 <-
    if (file.exists(here::here(mod_dir, paste0(name, "_m.rds")))) {
      tmp <- readRDS(here::here(mod_dir, paste0(name, "_m.rds")))
      # Remove spaces, double line breaks
      gsub(" ", "", tmp) |> gsub("\n\n", "\n", x = _)
    } else FALSE
  # Check which variables are outcomes and need free residual variance.
  require(stringr)
  y_vars <- str_extract_all(regr, "(^|\n) *.*( |)~") |>
    unlist() |>
    gsub("~|\n| ", "", x = _) |>
    unique()
  # if (is.null(z_vars)) {
  z_vars <- str_extract_all(regr, "(~~|~|\\+) *.*?(\n| |$|~~)") |>
    unlist() |>
    gsub("~| |\\+|\n", "", x = _) |>
    unique()
  # }
  extra_vars <- str_extract_all(extra, "(^|=~|~~) *.*?(=~|\n|$)") |>
    unlist() |>
    gsub("=~|~~|.*\\*|\n| ", "", x = _) |>
    unique()
  par0 <- lapply(fit1, parameterEstimates)
  # Remove any models from par0 that are not specified in the structural model.
  # Note names(par0) could include something different from the actual latent
  # variable name used in the code, so the latent variable name itself should
  # be extracted rather than using names(par0).
  par_sel <- sapply(
    par0,
    function(x) {
      tmp <- x$lhs[x$op == "=~"] |> unique()
      tmp %in% c(y_vars, z_vars, extra_vars)
    }
  )
  par1 <- par0[par_sel]
  items <- z_vars[!z_vars %in% c(names(par1), "")]
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
    paste0("\n", regr)
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
  # Load old object if it exists
  fit0 <- if (file.exists(here::here(mod_dir, paste0(name, "_fit.rds")))) {
    readRDS(here::here(mod_dir, paste0(name, "_fit.rds")))
  } else FALSE
  fit_type <- ifelse(class(fit0) == "lavaan", TRUE, FALSE)
  if (hash_d_test & m_test & fit_type) {
    message(paste("Use existing fit for", name))
    fit <- fit0
  } else {
    message(paste("New model estimates for", name))
    if (is.null(ordered)) {
      fit <- sem(
        mod, dat,
        orthogonal = orthogonal, missing = miss, estimator = est,
        std.lv = std.lv
      )
    } else {
      fit <- sem(
        mod, dat,
        orthogonal = orthogonal, missing = miss, estimator = est,
        std.lv = std.lv, ordered = ordered
      )
    }
  }
  # Save models (so they can be read in next time)
  saveRDS(fit, here::here(mod_dir, paste0(name, "_fit.rds")))
  # Save model / hashes
  saveRDS(mod, here::here(mod_dir, paste0(name, "_m.rds")))
  saveRDS(hash_d, here::here(hash_dir, paste0("hash_", name, "_d.rds")))
  # Return
  return(fit)
}
