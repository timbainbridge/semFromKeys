#' Sets up a cache directory for storing model inputs and outputs
#'
#' Using `save_out = TRUE` or `check = TRUE` from
#' [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()], or
#' [esem.from.mods()] requires a cache directory to save model objects to or
#' to check against.
#' By default, `cache.setup` uses the system's default cache location,
#' otherwise a location within the current working directory or project can be
#' selected.
#' The function needs to be run once per session whenever a cache directory is
#' required.
#'
#' @param location
#' The cache location to save model objects to to enable checking.
#' If `location = "user"` (default, recommended if unsure),
#' then the system's default cache location is used.
#' If any other string is used a folder will be created within either the
#' [getwd()] directory or,
#' if available, within the directory identified by [here::here()].
#'
#' @return
#' The cache directory path (invisibly). Primarily called for its side effect
#' of setting up the cache configuration.
#'
#' @details
#' To avoid writing to users' computers without permission,
#' users are required to run this function first to ensure that they know *that*
#' files are being saved and *where* files are being saved.
#' The function temporarily sets the option `semFromKeys_cache_dir`,
#' which is used by other functions from the package as the cache directory.
#' As a result, the function needs to be run once per session whenever a cache
#' directory is required.
#' All functions that utilise the cache directory will look for an
#' option (`getOption("semFromKeys_cache_dir")`) and if it is not set will
#' request that users either change options to not require the cache directory
#' or run this function first.
#'
#' Functions that directly or indirectly might require a cache directory are:
#' [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()],
#' [esem.from.mods()], and [sem.check()].
#'
#' @seealso
#' [cfa.from.keys()], [bifactor.from.keys()], [efa.from.keys()],
#' [esem.from.mods()], and [sem.check()] for dependent functions;
#' [tools::R_user_dir()] for default cache directories; and
#' [here::here()] for project-based cache directory setting.
#'
#' @export
#'
#' @examples
#' \donttest{  # Avoid creating a cache directory for tests of examples
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
#'   # Check that they are not estimated again.
#'   cfa_fit <- cfa.from.keys(
#'     keys, BFIGritHope, fit_save = TRUE, check = TRUE, save_out = TRUE
#'   )
#' }

cache.setup <- function(location = "user") {
  if (location == "user") {
    cache_dir <- tools::R_user_dir("semFromKeys", which = "cache")
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    options(semFromKeys_cache_dir = cache_dir)
    message(paste0("The cache directory is set as: '", cache_dir, "'."))
  } else {
    if (length(location) != 1 | !is.character(location)) {
      stop("`location` is not a length 1 character vector")
    }
    if (requireNamespace("here")) {
      cache_dir <- paste0(here::here(), "/", location)
    } else {
      cache_dir <- paste0(getwd(), "/", location)
    }
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    options(semFromKeys_cache_dir = cache_dir)
    message(paste0("The cache directory is set as: '", cache_dir, "'."))
  }
  return(invisible(cache_dir))
}
