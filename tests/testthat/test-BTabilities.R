context("implementation [BTabilities]")

# citations data

##  Convert frequencies to success/failure data
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")

##  First fit the "standard" Bradley-Terry model
citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)

##  Now the same thing with a different "reference" journal
citeModel2 <- update(citeModel, refcat = "JASA")

test_that("BTabilities works with changing refcat", {
    # standard model
    abilities1 <- BTabilities(citeModel)
    abilities2 <- BTabilities(citeModel2)
    ## check abilities
    expect_equal(abilities2[, "ability"], 
                 abilities1[, "ability"] - abilities1["JASA", "ability"])
    ## check standard errors
    M <- diag(4)
    M[3, ] <- -1
    M[, 3] <- 0
    V <- cbind(0, rbind(0, vcov(citeModel)))
    expect_equal(unname(abilities2[, "s.e."]), 
                 sqrt(diag(t(M) %*% V %*% M)))
})

test_that("BTabilities works with sum to zero contrasts", {
    # specify contrasts via contrast arg
    mod3 <- BTm(cbind(win1, win2), journal1,
                journal2, ~ journal, id = "journal", x = FALSE, 
                contrasts = list(journal = "contr.sum"), data = citations.sf)
    # or as attribute of factors
    citations.sf$journal1 <- C(citations.sf$journal1, "contr.sum")
    citations.sf$journal2 <- C(citations.sf$journal2, "contr.sum") 
    mod3b <-
        BTm(cbind(win1, win2), journal1, journal2, ~ journal, id = "journal", 
            x = FALSE, data = citations.sf)
    # results should be the same
    expect_equivalent(BTabilities(mod3), BTabilities(mod3b))
    # check vs deriving from model based on treatment contrasts
    M <- matrix(- 1/4, nrow = 4, ncol = 4)
    diag(M) <- 1 - 1/4
    expect_equivalent(BTabilities(mod3)[, "ability"], 
                      BTabilities(citeModel)[, "ability"] %*% M)
    V <- cbind(0, rbind(0, vcov(citeModel)))
    expect_equivalent(BTabilities(mod3)[, "s.e."], 
                      sqrt(diag(t(M) %*% V %*% M)))
})
