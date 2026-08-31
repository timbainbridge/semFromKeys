test_that(
  "Cache directory not set",
  {
    expect_error(
      cache.clean(0, interactive = FALSE), "cache directory is not configured"
    )
  }
)
test_that(
  "'older_than' and 'name' not set",
  {
    cache.setup("tests/testthat/cache", interactive = FALSE)
    expect_error(
      cache.clean(interactive = FALSE), "specify a value for 'older_than'"
    )
  }
)
test_that(
  "No files to delete",
  {
    cache.setup("tests/testthat/cache", interactive = FALSE)
    expect_message(cache.clean(0, interactive = FALSE), "No files to delete")
  }
)
if (!interactive()) {  # Does not do anything if run interactively
  test_that(
    "'Interactive = TRUE' in non-interactive session",
    {
      cache.setup("tests/testthat/cache", interactive = FALSE)
      cfa.from.keys(keys[1], BFIGritHope, save_out = TRUE)
      expect_error(
        cache.clean(0, interactive = TRUE),
        "Running 'cache.clean' in a non-interactive session"
      )
      # Actually clean the cache
      cache.clean(0, interactive = FALSE)
    }
  )
}
test_that(
  "Test deletion messages",
  {
    cache.setup("tests/testthat/cache", interactive = FALSE)
    invisible(cfa.from.keys(keys[1], BFIGritHope, save_out = TRUE))
    expect_message(
      cache.clean(0, interactive = FALSE), "Deleted 6 file"
    )
  }
)
test_that(
  "Test file matching by name",
  {
    cache.setup("tests/testthat/cache", interactive = FALSE)
    invisible(cfa.from.keys(keys[1], BFIGritHope, save_out = TRUE))
    expect_message(
      cache.clean(name = "cfa", interactive = FALSE), "Deleted 6 file"
    )
  }
)
