test_that(
  "Cache location invalid",
  {
    expect_error(
      cache.setup(c("tests/testthat/cache", "tests/testhtat/invalid")),
      "the condition has length > 1"
    )
    expect_error(
      cache.setup(42), "'location' is not a length 1 character vector"
    )
  }
)
