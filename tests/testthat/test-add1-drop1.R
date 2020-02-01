context("methods [add1, drop1]")

tol <- 1e-6

# flatlizards GLMM
result <- rep(1, nrow(flatlizards$contests))
BTmodel1 <- BTm(result, winner, loser,
                ~ throat.PC1[..] + throat.PC3[..] + (1|..),
                data = flatlizards,
                tol = 1e-4, sigma = 2)
# add a term
BTmodel2 <- update(BTmodel1, formula = ~ . + head.length[..])

test_that("drop1 works with GLMM", {
    # check against expected values for single term deletions
    res <- drop1(BTmodel1)
    expect_known_value(res,
                       file = test_path("outputs/drop1.rds"),
                       tol = tol)
    # check against anova
    res2 <- drop1(BTmodel2, test = "Chisq")
    expect_equal(res2$Statistic[3], anova(BTmodel1, BTmodel2)$Statistic[2])
})

test_that("add1 with Chisq tests works with GLMM", {
    # check against expected values for single term additions
    res <- add1(BTmodel1, ~ . + head.length[..] + SVL[..], test = "Chisq")
    expect_known_value(res,
                       file = test_path("outputs/add1.rds"),
                       tol = tol)
    # check against anova
    expect_equal(res$Statistic[1], anova(BTmodel1, BTmodel2)$Statistic[2])
})
