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
test_that(
  "Cache projectname invalid",
  {
    expect_error(
      cache.setup(
        projectname = c("tests/testthat/cache", "tests/testhtat/invalid")
      ),
      "'projectname' is not a length 1 character vector"
    )
    expect_error(
      cache.setup(projectname = 42),
      "'projectname' is not a length 1 character vector"
    )
  }
)
