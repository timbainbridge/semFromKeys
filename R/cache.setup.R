#' Sets up a cache directory for storing model inputs and outputs
#'
#' Using `save_out = TRUE` or `check = TRUE` from
#' [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()], or
#' [esem.from.mods()] requires a cache directory to save model objects to or
#' to check against.
#' By default, `cache.setup` uses the system's default cache location,
#' otherwise a location within the current working directory or project can be
#' selected.
#' The function needs to be run once after the environment is cleared whenever
#' a cache directory is required.
#'
#' @param location
#' The cache location to save model objects to to enable checking.
#' If `location = "user"` (default, recommended if unsure),
#' then the system's default cache location is used.
#' If any other string is used a folder will be created within either the
#' [getwd()] directory or,
#' if available, within the directory identified by [here::here()].
#' @param interactive
#' Logical.
#' `TRUE` indicates that confirmation will be required before a cache directory
#' is set *if* the default location is not used.
#' `FALSE` indicates that a cache directory will be set without confirmation.
#' Irrelevant if `location = "user"`.
#' It is recommended to use `TRUE`.
#' Defaults to `TRUE`.
#'
#' @return
#' The cache directory path (invisibly).
#' Primarily called to set up the cache configuration.
#'
#' @details
#' Saving outputs to save time running identical models more than once requires
#' files to be saved on your computer.
#' To avoid writing to your computer without your permission,
#' you are required to run this function first to ensure that you know *that*
#' files are being saved and *where* files are being saved.
#' The function temporarily sets a hidden environment variable '.cache_env'
#' (with, if empty,
#' `assign(".cache_env", new.env(parent = emptyenv()), envir = parent.frame(1))`
#' and with
#' `assign("cache_dir", cache_dir, envir = get(".cache_env", envir = parent.frame(1)))`,
#' if not), which will be removed whenever the environment is cleared.
#'
#' By default, the function creates the cache directory in the standard place
#' for the operating system being used, appended by `semFromKeys/[projectname]`,
#' if a project is being used and the `rstudioapi` is available.
#' If a project is not being used or the `rstudioapi` package is not available,
#' then the relative path with be `semFromKeys`.
#'
#' In the later case, if two function calls include the same `name` assignment,
#' then outputs from one will overwrite the other. Therefore, it is recommended
#' to either set the cache directory to something other than the default or use
#' the default within a project with the `rstudioapi` package installed.
#' Obviously this latter option will likely not work outside of RStudio,
#' so, in that case, it is recommended to use a custom location.
#'
#' To ensure that R knows what the cache directory is,
#' a hidden environment variable containing the information---`.cache_env`---is
#' saved within the working environment.
#' Other functions from the package will look for and use `.cache_env` to
#' identify the cache directory.
#' As a result, whenever the working environment is cleared
#' (e.g., upon closing RStudio) or whenever the `.cache_env` is otherwise
#' removed,
#' the `cache.setup` function will need to be re-run before the cache
#' functionality will work,
#' regardless of the status of the any recently used cache directories.
#'
#' Note also that the function relies on code that was unavailable in versions
#' of R prior to version 4.0,
#' so anyone using an earlier version of R will either have to update R or forgo
#' the caching functionality.
#'
#' Functions that directly or indirectly might require a cache directory are:
#' [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()],
#' [esem.from.mods()], and [sem.check()].
#'
#' @seealso
#' [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()],
#' [esem.from.mods()], and [sem.check()] for dependent functions;
#' [tools::R_user_dir()] for default cache directories;
#' [here::here()] for project-based cache directory setting; and
#' [cache.clean()] for a function to clean cache.
#'
#' @importFrom rstudioapi getActiveProject
#' @export
#'
#' @examples
#' \donttest{
#'   # Setup a cache directory
#'   cache.setup()
#'   # Now code with check = TRUE or save_out = TRUE will work, e.g.,
#'   # Create CFA keys
#'   keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
#'   keys <- sapply(
#'     keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
#'   )
#'   # Run models
#'   cfa_fit <- cfa.from.keys(
#'     keys, BFIGritHope, fit_save = TRUE, check = TRUE, save_out = TRUE
#'   )
#'   # Check that models are not run again.
#'   cfa_fit <- cfa.from.keys(
#'     keys, BFIGritHope, fit_save = TRUE, check = TRUE, save_out = TRUE
#'   )
#' }

cache.setup <- function(location = "user", interactive = TRUE) {
  if (getRversion() < "4.0") {
    stop(
      paste0(
        "Setting up cache requires R >= 4.0. ",
        "Your system is currently running version ", getRversion(), ". ",
        "Please update your R version or use 'check = FALSE' and",
        "'save_out = FALSE' for all function calls with these options."
      )
    )
  }
  if (location == "user") {
    if (!require(rstudioapi)) {
      cache_dir <- tools::R_user_dir("semFromKeys", which = "cache")
    } else {
      project <- getActiveProject()
      if (is.null(project)) {
        cache_dir <- tools::R_user_dir("semFromKeys", which = "cache")
      } else {
        project <- sub(".*/", "", project)
        cache_dir <- tools::R_user_dir(
          paste0("semFromKeys/", project), which = "cache"
        )
      }
    }
  } else {
    if (!is.character(location)) {
      stop("`location` is not a length 1 character vector")
    }
    if (interactive & interactive()) {
      if (requireNamespace("here")) {
        message(
          paste0(
            "The cache director will be set as: '",
            paste0(here::here(), "/", location), "'."
          )
        )
        response <- readline("Continue? (y/n): ")
      } else {
        message(
          paste0(
            "The cache director will be set as: ",
            paste0(getwd(), "/", location), "."
          )
        )
        response <- readline("Continue? (y/n): ")
      }
      if (tolower(substr(response, 1, 1)) != "y") {
        message("Cancelled. Cache director not set.")
        return(invisible(NULL))
      }
    }
    if (requireNamespace("here")) {
      cache_dir <- paste0(here::here(), "/", location)
    } else {
      cache_dir <- paste0(getwd(), "/", location)
    }
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  if (!exists(".cache_env", mode = "environment", envir = parent.frame(1))) {
    assign(".cache_env", new.env(parent = emptyenv()), envir = parent.frame(1))
  }
  assign(
    "cache_dir", cache_dir, envir = get(".cache_env", envir = parent.frame(1))
  )
  message(paste0("The cache directory is set as: '", cache_dir, "'."))
  return(invisible(cache_dir))
}
