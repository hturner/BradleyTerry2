context("data sets [flatlizards]")

tol <- 1e-6

##  standard BT model, using the bias-reduced maximum likelihood method:
result <- rep(1, nrow(flatlizards$contests))
BTmodel <- BTm(result, winner, loser, br = TRUE, 
               data = flatlizards$contests)

##  "structured" B-T model: abilities are determined by a linear predictor.
Whiting.model1 <- BTm(result, winner, loser, 
                      ~ throat.PC1[..] + throat.PC3[..] + 
                          head.length[..] + SVL[..], 
                      family = binomial, data = flatlizards)

##  Equivalently, fit the same model using glmmPQL:
Whiting.model1b <- BTm(result, winner, loser, 
                       ~ throat.PC1[..] + throat.PC3[..] +
                           head.length[..] + SVL[..] + (1|..), 
                       sigma = 0, sigma.fixed = TRUE, data = flatlizards)

##  Same predictor but with a normally distributed error
Whiting.model2 <- BTm(result, winner, loser, 
                      ~ throat.PC1[..] + throat.PC3[..] +
                          head.length[..] + SVL[..] + (1|..),
                      data = flatlizards)

##  Now use probit rather than logit as the link function:
Whiting.model3 <- BTm(result, winner, loser, 
                      ~ throat.PC1[..] + throat.PC3[..] +
                          head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"), data = flatlizards)

test_that("standard model as expected on flatlizards", {
    # check standard model
    # ignore family: mode of initialize changes between R versions
    res <- summary(BTmodel)
    res$family <- NULL
    expect_known_value(res,
                       file = test_path("outputs/flatlizards-BTmodel.rds"),
                       tol = tol)
    # check structured model against Table 1 of Whiting et al. (2006)
    # (for coefficients of covariates only, not separate lizard effects)
    cf <- coef(summary(Whiting.model1))[-(1:2),]
    expect_equal(unname(round(cf[, "Estimate"], 2)),
                 c(-0.09, 0.34, -1.13, 0.19))
    expect_equal(unname(round(cf[, "Std. Error"], 2)),
                 c(0.03, 0.11, 0.49, 0.1))
    # reported Z stat appear to be Chi-squared stat
    expect_equal(unname(round(cf[, "z value"]^2, 1)),
                 c(10.3, 9.5, 5.2, 3.6), tol = 1e-1)
    expect_equal(unname(signif(cf[, "Pr(>|z|)"], 1)),
                 c(0.001, 0.002, 0.02, 0.06))
    # check equiv glmmPQL against Table 1 of Whiting et al. (2006)
    # (for coefficients of covariates only, not separate lizard effects)
    cf <- coef(summary(Whiting.model1b))[-(1:2),]
    expect_equal(unname(round(cf[, "Estimate"], 2)),
                 c(-0.09, 0.34, -1.13, 0.19))
    expect_equal(unname(round(cf[, "Std. Error"], 2)),
                 c(0.03, 0.11, 0.49, 0.1))
    # reported Z stat appear to be Chi-squared stat
    expect_equal(unname(round(cf[, "z value"]^2, 1)),
                 c(10.3, 9.5, 5.2, 3.6), tol = 1e-1)
    expect_equal(unname(signif(cf[, "Pr(>|z|)"], 1)),
                 c(0.001, 0.002, 0.02, 0.06))
})

test_that("GLMM models as expected on flatlizards", {
    ##  The estimated coefficients (of throat.PC1, throat.PC3,
    ##  head.length and SVL are not changed substantially by
    ##  the recognition of an error term in the model
    cf <- coef(summary(Whiting.model1b))[-(1:2),]
    cf2 <- summary(Whiting.model2)$fixef[-(1:2),]
    expect_equal(cf[, "Estimate"],
                 cf2[, "Estimate"], tol = 0.5)
    ## but the estimated
    ## standard errors are larger, as expected.  The main conclusions from
    ## Whiting et al. (2006) are unaffected.
    expect_true(all(cf2[, "Std. Error"] >  cf[, "Std. Error"]))
    ##  Modulo the usual scale change between logit and probit, the results
    ##  are (as expected) very similar to Whiting.model2.
    cf3 <- summary(Whiting.model3)$fixef[-(1:2),]
    expect_equal(unname(cf2[, "Estimate"]/cf3[, "Estimate"]),
                 rep(1.6, 4), tol = 0.1)
    ## drop lizard 996as coef not estimable !! should be 96
    abilities <- BTabilities(Whiting.model3)[-55,]
    expect_known_value(abilities,
                       file = test_path("outputs/flatlizards-abilities.rds"),
                       tol = tol)
    resids <- residuals(Whiting.model3, "grouped")
    expect_known_value(resids,
                       file = test_path("outputs/flatlizards-residuals.rds"),
                       tol = tol)
})

