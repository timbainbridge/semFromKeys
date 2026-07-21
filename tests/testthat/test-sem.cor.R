test_that(
  "Works with just fit_y",
  {
    cors <- sem.cor(BFIGritHope, cfa_fit)
    expect_equal(length(cors), 4)
    expect_equal(length(cors$fit), ncol(combn(names(cfa_fit), 2)))
    expect_equal(ncol(cors$cor_mat), length(cfa_fit))
    expect_equal(nrow(cors$cor_mat), ncol(cors$cor_mat))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
test_that(
  "Works with fit_y and items",
  {
    items <- names(BFIGritHope)[grep("bfi.*1_1", names(BFIGritHope))]
    cors <- sem.cor(BFIGritHope, cfa_fit, items = items)
    expect_equal(length(cors), 4)
    expect_equal(
      length(cors$fit),
      ncol(combn(names(cfa_fit), 2)) + length(cfa_fit) * length(items)
    )
    expect_equal(ncol(cors$cor_mat), length(cfa_fit))
    expect_equal(nrow(cors$cor_mat), length(cfa_fit) + length(items))
    expect_equal(ncol(cors$ci_lower), length(cfa_fit))
    expect_equal(nrow(cors$ci_lower), length(cfa_fit) + length(items))
    expect_equal(ncol(cors$ci_upper), length(cfa_fit))
    expect_equal(nrow(cors$ci_upper), length(cfa_fit) + length(items))
  }
)
test_that(
  "Works with fit_y and fit_x",
  {
    fit_y <- cfa_fit[1:2]
    fit_x <- cfa_fit[3:4]
    cors <- sem.cor(BFIGritHope, fit_y, fit_x)
    expect_equal(length(cors), 4)
    expect_equal(length(cors$fit), length(fit_y) * length(fit_x))
    expect_equal(ncol(cors$cor_mat), length(fit_y))
    expect_equal(nrow(cors$cor_mat), length(fit_x))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
test_that(
  "Works with fit_y, fit_x, and items",
  {
    fit_y <- cfa_fit[1:2]
    fit_x <- cfa_fit[3:4]
    items <- names(BFIGritHope)[grep("bfi.*1_1", names(BFIGritHope))]
    cors <- sem.cor(BFIGritHope, fit_y, fit_x, items = items)
    expect_equal(length(cors), 4)
    # y * x + y * i = y(x + i)
    expect_equal(
      length(cors$fit), length(fit_y) * (length(fit_x) + length(items))
    )
    expect_equal(ncol(cors$cor_mat), length(fit_y))
    expect_equal(nrow(cors$cor_mat), length(fit_x) + length(items))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
test_that(
  "Works with length 1 fit_y and items",
  {
    items <- "bfi_e1_1"
    fit_y <- cfa_fit[1]
    cors <- sem.cor(BFIGritHope, fit_y, items = items)
    expect_equal(length(cors), 4)
    expect_equal(length(cors$fit), length(fit_y))
    expect_equal(ncol(cors$cor_mat), length(fit_y))
    expect_equal(nrow(cors$cor_mat), ncol(cors$cor_mat))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
test_that(
  "Works with length 1 fit_y and fit_x",
  {
    fit_y <- cfa_fit[1]
    fit_x <- cfa_fit[3]
    cors <- sem.cor(BFIGritHope, fit_y, fit_x)
    expect_equal(length(cors), 4)
    expect_equal(length(cors$fit), length(fit_y) * length(fit_x))
    expect_equal(ncol(cors$cor_mat), length(fit_y))
    expect_equal(nrow(cors$cor_mat), length(fit_x))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
test_that(
  "Element of 'fit_y' not a lavaan object",
  {
    expect_error(
      sem.cor(BFIGritHope, c(cfa_fit, "Hello")),
      "'fit_y' is not an object of class lavaan"
    )
  }
)
test_that(
  "Items cannot match a fit_y name",
  {
    items <- "bfi_e1_1"
    fit_y <- cfa_fit
    names(fit_y)[1] <- items
    expect_error(
      sem.cor(BFIGritHope, fit_y, items = "bfi_e1_1"),
      "'items' are included as a name of a fitted model in 'fit_y'"
    )
  }
)
test_that(
  "Element of 'fit_x' not a lavaan object",
  {
    expect_error(
      sem.cor(BFIGritHope, cfa_fit[1:2], c(cfa_fit[3:4], "bfi_e1_1")),
      "'fit_x' is not an object of class lavaan"
    )
  }
)
test_that(
  "Items cannot match a fit_x name",
  {
    items <- "bfi_e1_1"
    fit_x <- cfa_fit[3:4]
    names(fit_x)[1] <- items
    expect_error(
      sem.cor(BFIGritHope, cfa_fit[1:2], fit_x, items = "bfi_e1_1"),
      "'items' are included as a name of a fitted model in 'fit_x'"
    )
  }
)
test_that(
  "Item not in data",
  {
    expect_error(
      sem.cor(BFIGritHope, cfa_fit, items = "HelloWorld"),
      "'items' are not in 'data'"
    )
  }
)
test_that(
  "Length 1 fit_y with no fit_x or items",
  {
    expect_error(
      sem.cor(BFIGritHope, cfa_fit[1]),
      "one measurement model and one item or two measurement models"
    )
  }
)
test_that(
  "Model with 2 factors in fit_y",
  {
    mod <- paste(
      "grit_c =~",
      paste(
        names(BFIGritHope)[grep("grit_c", names(BFIGritHope))], collapse = " + "
      ),
      "\ngrit_p =~",
      paste(
        names(BFIGritHope)[grep("grit_p", names(BFIGritHope))], collapse = " + "
      )
    )
    cfa_new <- sem(mod, BFIGritHope, std.lv = TRUE, missing = "ML")
    fit_y <- c(cfa_fit[3:4], list(new = cfa_new))
    expect_error(sem.cor(BFIGritHope, fit_y), "more than one latent variable")
  }
)
test_that(
  "Model with 2 factors in fit_x",
  {
    mod <- paste(
      "grit_c =~",
      paste(
        names(BFIGritHope)[grep("grit_c", names(BFIGritHope))], collapse = " + "
      ),
      "\ngrit_p =~",
      paste(
        names(BFIGritHope)[grep("grit_p", names(BFIGritHope))], collapse = " + "
      )
    )
    cfa_new <- sem(mod, BFIGritHope, std.lv = TRUE, missing = "ML")
    expect_error(
      sem.cor(BFIGritHope, cfa_fit[3:4], list(new = cfa_new)),
      "more than one latent variable"
    )
  }
)
test_that(
  "Item from fit_y in items",
  {
    expect_error(
      sem.cor(BFIGritHope, cfa_fit, items = "grit_c_1"),
      "in both 'items' and contributes to the measurement of the"
    )
  }
)
test_that(
  "Item from fit_x in items (should work)",
  {
    expect_equal(
      length(
        sem.cor(BFIGritHope, cfa_fit[3:4], cfa_fit[1:2], items = "grit_c_1")
      ),
      4
    )
  }
)
test_that(
  "All items shared in a different factor",
  {
    fit_x <- cfa.from.keys(
      list(grit = names(BFIGritHope)[grep("grit", names(BFIGritHope))]),
      BFIGritHope,
      fit_save = FALSE
    )$fit
    # Specifying fit_x
    expect_error(
      sem.cor(BFIGritHope, cfa_fit[1:2], fit_x),
      "items in 'grit_c' are included in 'grit'"
    )
    # Only with fit_y
    expect_error(
      sem.cor(BFIGritHope, c(cfa_fit[1:2], fit_x)),
      "items in 'grit_c' are included in 'grit'"
    )
  }
)
test_that(
  "Less than all items shared in different factors",
  {
    fit_x <- cfa.from.keys(
      list(
        grit_c = c(
          names(BFIGritHope)[grep("grit_c", names(BFIGritHope))], "grit_p_1"
        )
      ),
      BFIGritHope,
      fit_save = FALSE
    )$fit
    # Specifying fit_x
    expect_warning(
      sem.cor(BFIGritHope, cfa_fit[2], fit_x),
      "in both the 'grit_p' and 'grit_c' factors"
    )
    # Only with fit_y
    expect_warning(
      sem.cor(BFIGritHope, c(cfa_fit[2], fit_x)),
      "in both the 'grit_p' and 'grit_c' factors"
    )
  }
)
test_that(
  "Non-positive definite matrix message",
  {
    b5 <- c("e", "c", "n", "o")
    b5f <- sapply(b5, function(x) paste0(x, 1:3))
    b5f_key <- sapply(
      b5f,
      function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))],
      simplify = FALSE
    )
    # Reduced set to minimise run time while still producing a nonPD matrix.
    b5f_key <- b5f_key[!grepl("(e|o)(1|2)|(c|n)1", names(b5f_key))]
    b5f_cfa <- cfa.from.keys(b5f_key, BFIGritHope, fit_save = FALSE)
    expect_warning(
      sem.cor(BFIGritHope, c(b5f_cfa$fit, cfa_fit)),
      "correlation matrix between 'fit_y' constructs has been adjusted"
    )
  }
)
test_that(
  "Works with 'item_loadings' set",
  {
    cors <- sem.cor(
      BFIGritHope, cfa_fit[1:2], items = paste0("bfi_e1_", 1:2),
      item_loadings = .8
    )
    parest <- lavaan::parameterestimates(cors$fit$grit_p.bfi_e1_1)
    expect_equal(
      parest$est[parest$lhs == "bfi_e1_1_l" & parest$rhs == "bfi_e1_1"], .8
    )
    cors <- sem.cor(
      BFIGritHope, cfa_fit[1:2], items = paste0("bfi_e1_", 1:2),
      item_loadings = c(.8, .7)
    )
    parest1 <- lavaan::parameterestimates(cors$fit$grit_p.bfi_e1_1)
    expect_equal(
      parest$est[parest$lhs == "bfi_e1_1_l" & parest$rhs == "bfi_e1_1"], .8
    )
    parest <- lavaan::parameterestimates(cors$fit$grit_p.bfi_e1_2)
    expect_equal(
      parest$est[parest$lhs == "bfi_e1_2_l" & parest$rhs == "bfi_e1_2"], .7
    )
  }
)
test_that(
  "Works with 'nagy = TRUE' for fit_y only",
  {
    fit_y <- cfa_fit[1:2]
    cors <- sem.cor(BFIGritHope, fit_y, nagy = TRUE)
    expect_equal(length(cors), 4)
    expect_equal(length(cors$fit), ncol(combn(names(fit_y), 2)))
    expect_equal(ncol(cors$cor_mat), length(fit_y))
    expect_equal(nrow(cors$cor_mat), ncol(cors$cor_mat))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
test_that(
  "Works with 'nagy = TRUE' for fit_y and items",
  {
    fit_y <- cfa_fit[1]
    items <- names(BFIGritHope)[grep("bfi.c\\d_1", names(BFIGritHope))]
    cors <- sem.cor(BFIGritHope, fit_y, items = items, nagy = TRUE)
    expect_equal(length(cors), 4)
    expect_equal(
      length(cors$fit),
      ncol(combn(names(cfa_fit), 2)) + length(cfa_fit) * length(items)
    )
    expect_equal(ncol(cors$cor_mat), length(cfa_fit))
    expect_equal(nrow(cors$cor_mat), length(cfa_fit) + length(items))
    expect_equal(ncol(cors$ci_lower), length(cfa_fit))
    expect_equal(nrow(cors$ci_lower), length(cfa_fit) + length(items))
    expect_equal(ncol(cors$ci_upper), length(cfa_fit))
    expect_equal(nrow(cors$ci_upper), length(cfa_fit) + length(items))
  }
)
test_that(
  "Works with 'nagy = TRUE' for fit_y and fit_x",
  {
    fit_y <- cfa_fit[1]
    fit_x <- cfa_fit[2]
    cors <- sem.cor(BFIGritHope, fit_y, fit_x, nagy = TRUE)
    expect_equal(length(cors), 4)
    expect_equal(length(cors$fit), length(fit_y) * length(fit_x))
    expect_equal(ncol(cors$cor_mat), length(fit_y))
    expect_equal(nrow(cors$cor_mat), length(fit_x))
    expect_equal(ncol(cors$ci_lower), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_lower), nrow(cors$cor_mat))
    expect_equal(ncol(cors$ci_upper), ncol(cors$cor_mat))
    expect_equal(nrow(cors$ci_upper), nrow(cors$cor_mat))
  }
)
