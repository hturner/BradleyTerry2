context("implementation [countsToBinomial]")

test_that("countstoBinomial works as expected", {
    ## Convert frequencies to success/failure data
    results <- countsToBinomial(citations)
    lev <- c("Biometrika", "Comm Statist", "JASA", "JRSS-B")
    expect_equal(results, 
                 data.frame(player1 = factor(rep(lev[1:3], 3:1), lev),
                            player2 = factor(lev[c(2:4, 3:4, 4)], lev),
                            win1 = c(730, 498, 221, 68, 17, 142),
                            win2 = c(33, 320, 284, 813, 276, 325)))
})
    