test_that(
  "`std` is not logical",
  {
    mods <- mapply(
      x = keys, y = names(keys), SIMPLIFY = FALSE,
      FUN = function(x, y) paste(y, "=~", paste(x, collapse = " + "))
    )
    expect_error(
      sem.check(mods, BFIGritHope, keys, std = 42),
      "`std` is not logical"
    )
  }
)
test_that(
  "`mods` is not a list",
  {
    mods <- mapply(
      x = keys, y = names(keys), SIMPLIFY = FALSE,
      FUN = function(x, y) paste(y, "=~", paste(x, collapse = " + "))
    )
    expect_error(
      sem.check(mods$grit_c, BFIGritHope, keys),
      "`mods` is not a list"
    )
  }
)
test_that(
  "'keys_s' and 'mods' are a different length",
  {
    mods <- mapply(
      x = keys, y = names(keys), SIMPLIFY = FALSE,
      FUN = function(x, y) paste(y, "=~", paste(x, collapse = " + "))
    )
    keys_l <- keys[-4]
    expect_error(
      sem.check(mods, BFIGritHope, keys_l),
      "'keys_s' and 'mods' are not the same length"
    )
  }
)
