test_that(
  "Test normal behaviour when fit_save = FALSE",
  {
    efa_fit <- efa.from.keys(keys_e, BFIGritHope, check = FALSE, fit_save = FALSE)
    expect_equal(length(efa_fit), 2)
    expect_equal(length(efa_fit$fit), 1)
    expect_equal(length(efa_fit$par), 1)
    expect_equal(sum(sapply(efa_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)
test_that(
  "Test normal behaviour when orthogonal = TRUE",
  {
    efa_fit <- efa.from.keys(
      keys_e, BFIGritHope, check = FALSE, fit_save = FALSE, orthogonal = TRUE
    )
    expect_equal(length(efa_fit), 2)
    expect_equal(length(efa_fit$fit), 1)
    expect_equal(length(efa_fit$par), 1)
    expect_equal(sum(sapply(efa_fit$fit, function(x) class(x) != "lavaan")), 0)
    # Check orthogonal
    expect_equal(
      sum(
        abs(
          efa_fit$par$efa$est[
            grepl(
              paste0("^", names(keys_e), "$", collapse = "|"), efa_fit$par$efa$lhs
            ) &
              efa_fit$par$efa$op == "~~" &
              efa_fit$par$efa$lhs != efa_fit$par$efa$rhs
          ] > 10e-10
        )
      ),
      0
    )
  }
)
test_that(
  "Test normal behaviour when fit_save = TRUE",
  {
    efa_fit <- efa.from.keys(keys_e, BFIGritHope, check = FALSE, fit_save = TRUE)
    expect_equal(length(efa_fit), 3)
    expect_equal(length(efa_fit$fit), 1)
    expect_equal(length(efa_fit$par), 1)
    expect_equal(sum(sapply(efa_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)

# keys_e goes to kl_e in sem.check, not kl_s, so needs to be tested
test_that(
  "Misspecified keys",
  {
    keys_mistake <- keys_e
    keys_mistake$bfi_e[1] <- "mistake"
    expect_error(
      efa.from.keys(keys_mistake, BFIGritHope, check = FALSE, fit_save = FALSE),
      "items are in `kl_e` but they are not in `dat`"
    )
    expect_error(
      efa.from.keys(keys_e$bfi_e, BFIGritHope, check = FALSE, fit_save = FALSE),
      "`kl_e` is not a list"
    )
  }
)
test_that(
  "Key of length 1 warning",
  {
    keys_l1 <- keys_e
    keys_l1$bfi_e <- keys_e$bfi_e[1]
    expect_warning(
      efa.from.keys(keys_l1, BFIGritHope, check = FALSE, fit_save = FALSE),
      "factors have only 1 item"
    )
  }
)

# I am not going to stop people from including differently named scales in the
# same EFA (e.g., BFI + grit). There may be good reasons to do that and it will
# return many false positives
# (e.g., e1, e2, ..., a1, a2, ... will erroneously find Big Five factors as
# different scales).
