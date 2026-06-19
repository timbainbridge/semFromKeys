# Create CFA keys
keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
keys <- sapply(
  keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)

# Create bifactor keys
keys_g0 <- c("grit", "hope")
keys_g <- sapply(
  keys_g0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)
keys_b1 <- sapply(keys_g0, function(x) keys0[grep(x, keys0)], simplify = FALSE)
keys_b <- keys_b1
# Fix negative residual variance
# (this is a lavaan warning, no need to test here)
keys_b$hope <- keys_b1$hope[-1]

# Create EFA keys
# Using only 3 factors to save time on checks
keys_e0 <- paste0("bfi_", c("e", "a", "c"))

# Using less than all items to save time on checks
# (This results in a less than ideal solution but it doesn't matter for the
# tests)
keys_e <- sapply(
  keys_e0,
  function(x) {
    names(BFIGritHope)[grep(paste0(x, "\\d_[1-2]"), names(BFIGritHope))]
  },
  simplify = FALSE
)

# Create fit objects to using in test-esem.from.mods
cfa_fit <- cfa.from.keys(keys, BFIGritHope, fit_save = FALSE)$fit
efa_fit <-
  efa.from.keys(keys_e, BFIGritHope, fit_save = FALSE)$fit$efa
bif_fit <- bifactor.from.keys(
  keys_g, keys_b, keys, BFIGritHope, fit_save = FALSE
)$fit
