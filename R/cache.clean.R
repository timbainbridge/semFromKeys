#' Cleans selected files from the current cache directory
#'
#' Functions in the package include options to write files to a cache directory,
#' set up with the [cache.setup()] function.
#' Obsoletely files might be created if the `name` parameter in a function call
#' is changed or if a function call is no longer being used.
#' The `cache.clean()` function provides a way to find files that have not be
#' modified in the last `older_than` days and lists the files for the user to
#' agree to delete or not.
#'
#' @param older_than
#' A positive number indicating number of days (fractions allowed).
#' Files with older modification times than this will be deleted
#' after confirmation if `interactive = TRUE`.
#' @param interactive
#' Logical.
#' `TRUE` indicates that confirmation will be required before files are deleted.
#' `FALSE` indicates that files will be deleted without requiring confirmation.
#' It is recommended to use `TRUE` except for carefully checked automated
#' processes.
#' Defaults to `TRUE`.
#'
#' @return `NULL` (invisibly). Primarily called to clean up the cache directory.
#'
#' @details
#' The function is interactive if run in an interactive session with
#' `interctive = TRUE`.
#' In addition to deleting old files, the function will also optionally delete
#' empty directories and, if the top level cache directory is also empty,
#' then it will also optionally delete the cache directory and unset it.
#' The latter will only be done in interactive sessions with
#' `interactive = TRUE` to avoid breaking non-interactive code.
#'
#' One way to clean only unused files is to run all current code and then
#' run `cache.clean()` with `older_than` set to something greater than the
#' number of days it takes for the code to run and less than when the code was
#' previously run. For example, if your code takes 5 minutes to run and you
#' previously ran it 2 days ago, you could run all your code and, when it has
#' finished, use `cache.clean(older_than = 1)`.
#'
#' @seealso [cache.setup()]
#'
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
#'   # Check that they are not estimated again.
#'   cfa_fit <- cfa.from.keys(
#'     keys, BFIGritHope, fit_save = TRUE, check = TRUE, save_out = TRUE
#'   )
#'   cache.clean(60/86400)  # Delete files not modified in the last minute.
#' }

cache.clean <- function(older_than = NULL, interactive = TRUE) {
  if (is.null(older_than)) {
    stop(
      "Please specify a value for `older_than`. To delete all files, set to 0."
    )
  }
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
        "Use the `cache.setup()` function to configure a directory to clean."
      )
    )
  }
  cache_dir <- get("cache_dir", envir = get(".cache_env", envir = env))
  files <- file.info(
    list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  )
  cutoff <- Sys.time() - (older_than * 86400)  # convert days to seconds
  files_del <- rownames(files)[
    files$mtime < cutoff &
      # Don't delete files that the package doesn't save.
      grepl("_(fit(|_m)|par(|_std|ams)|mod|hash).rds$", rownames(files))
  ]
  if (length(files_del) == 0) {
    message("No files to delete.")
  } else {
    # Prompt user if interactive
    if (interactive & interactive()) {
      message(
        paste0(
          "About to delete the following ", length(files_del),
          " file(s), older than ", older_than, " from\n '",
          cache_dir, "':\n\n  ",
          paste0(
            sub(paste0(cache_dir, ".*/"), "", files_del), collapse = "\n  "
          )
        )
      )
      response <- readline("Continue? (y/n): ")
      if (tolower(substr(response, 1, 1)) != "y") {
        message("Cancelled.")
        return(invisible(NULL))
      }
    }
    # Delete files
    unlink(files_del)
    message(paste("Deleted", length(files_del), "file(s)."))
  }
  dirs <- list.dirs(cache_dir, full.names = TRUE, recursive = TRUE)
  dirs_empty <- sapply(
    dirs,
    function(x) {
      tmp <- list.files(x, full.names = TRUE, recursive = TRUE)
      length(tmp) == 0
    }
  )
  dirs_del <- dirs[dirs_empty]
  dirs_del <- dirs_del[-1]  # Don't delete parent (i.e., cache_dir)
  if (length(dirs_del) > 0) {
    if (interactive & interactive()) {
      message(
        paste0(
          "\nAbout to delete the following ", length(dirs_del) - 1,
          " empty directories from\n '",
          cache_dir, "':\n\n  ",
          paste0(
            sub(paste0(cache_dir, ".*/"), "", dirs_del[-1]), collapse = "\n  "
          )
        )
      )
      response <- readline("Continue? (y/n): ")
      if (tolower(substr(response, 1, 1)) != "y") {
        message("Cancelled.")
        return(invisible(NULL))
      }
    }
    # Delete empty directories
    unlink(dirs_del)
  }
  if (length(list.files(cache_dir, full.names = TRUE, recursive = TRUE)) > 0) {
    return(invisible(NULL))
  }
  if (interactive & interactive()) {
    message(paste0("The top cache directory, '", cache_dir, "', is empty."))
    response <- readline(
      paste(
        "Do you want to delete it?",
        "Doing so will unset the cache directory (y/n): "
      )
    )
    if (tolower(substr(response, 1, 1)) != "y") {
      message("Cancelled.")
      return(invisible(NULL))
    }
    unlink(cache_dir)
    rm(.cache_env, envir = .GlobalEnv)
    message(
      paste0(
        "'", cache_dir, "' has been deleted.\n",
        "To use 'check = TRUE' or 'save_out = TRUE',",
        "'cache.setup()' will have to be reinitiated."
      )
    )
    return(invisible(NULL))
  } else invisible(NULL)
}
