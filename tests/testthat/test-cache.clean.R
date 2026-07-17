test_that(
  "Cache directory not set",
  {
    expect_error(
      cache.clean(0, interactive = FALSE), "cache directory is not configured"
    )
  }
)
test_that(
  "Test 'older_than' not set without cache.setup",
  {
    expect_error(
      cache.clean(interactive = FALSE), "specify a value for 'older_than'"
    )
  }
)
test_that(
  "Test 'older_than' not set with cache.setup",
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
