# Most tests are really tests of sem.check and most of them have been done for
# cfa.from.keys, so only a few additional ones are required here.

# Load data
d <- BFIGritHopeImp

# Create keys
# Using only 3 factors to save time on checks
keys0 <- paste0("bfi_", c("e", "a", "c"))#, "n", "o"))

# Using less than all items to save time on checks
keys <- sapply(
  keys0,
  function(x) names(d)[grep(paste0(x, "\\d_[1-2]"), names(d))],
  simplify = FALSE
)

# Test normal behaviour
fit_save <- FALSE
efa_fit <- efa.from.keys(keys, d, check = FALSE, fit_save = fit_save)
expect_equal(length(efa_fit), 2)
expect_equal(length(efa_fit$fit), 1)
expect_equal(length(efa_fit$par), 1)
expect_equal(sum(sapply(efa_fit$fit, function(x) class(x) != "lavaan")), 0)

# Orthogonal TRUE
efa_fit <-
  efa.from.keys(keys, d, check = FALSE, fit_save = fit_save, orthogonal = TRUE)
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
          paste0("^", names(keys), "$", collapse = "|"), efa_fit$par$efa$lhs
        ) &
          efa_fit$par$efa$op == "~~" &
          efa_fit$par$efa$lhs != efa_fit$par$efa$rhs
      ] > 10e-10
    )
  ),
  0
)

fit_save <- TRUE
efa_fit <- efa.from.keys(keys, d, check = FALSE, fit_save = fit_save)
expect_equal(length(efa_fit), 3)
expect_equal(length(efa_fit$fit), 1)
expect_equal(length(efa_fit$par), 1)
expect_equal(sum(sapply(efa_fit$fit, function(x) class(x) != "lavaan")), 0)

# keys goes to kl_e in sem.check, not kl_s, so needs to be tested

# Mistake in keys
fit_save <- FALSE
keys_mistake <- keys
keys_mistake$bfi_e[1] <- "mistake"
expect_error(
  efa.from.keys(
    keys_mistake, d, check = FALSE, fit_save = fit_save
  ),
  "items are in `kl_e` but they are not in `dat`"
)

# Non-list keys list
expect_error(
  efa.from.keys(keys$bfi_e, d, check = FALSE, fit_save = fit_save),
  "`kl_e` is not a list"
)

# Length 1 key
keys_l1 <- keys
keys_l1$bfi_e <- keys$bfi_e[1]
expect_warning(
  efa.from.keys(keys_l1, d, check = FALSE, fit_save = fit_save),
  "factors have only 1 item"
)

# I am not going to stop people from including differently named scales in the
# same EFA (e.g., BFI + grit). There may be good reasons to do that and it will
# return many false positives
# (e.g., e1, e2, ..., a1, a2, ... will erroneously find Big Five factors as
# different scales).
