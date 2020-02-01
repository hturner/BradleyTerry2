context("data sets [baseball]")

##  This reproduces the analysis in Sec 10.6 of Agresti (2002).
##  pp 437-438 Categorical Data Analysis (2nd Edn.)

##  Simple Bradley-Terry model, ignoring home advantage:
baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")

##  Now incorporate the "home advantage" effect
baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)

test_that("baseball analysis reproducible", {
    # check model 1
    cf1 <- coef(summary(baseballModel1))
    # check against Table 10.11, column 3
    expect_identical(unname(round(sort(cf1[, "Estimate"]), 2)),
                     c(0.68, 1.11, 1.25, 1.29, 1.44, 1.58))
    # check statement that standard errors are about 0.3
    expect_identical(unname(round(cf1[, "Std. Error"], 1)),
                     rep(0.3, 6))
    # check model 2
    abilities <- exp(BTabilities(baseballModel2)[, "ability"])
    abilities <- abilities/sum(abilities)
    # check against Table 10.11, column 5
    expect_identical(unname(round(sort(abilities), 3)),
                     c(0.044, 0.088, 0.137, 0.157, 0.164, 0.190, 0.220))
    expect_identical(unname(round(coef(baseballModel2)["at.home"], 3)),
                     0.302)
    
})