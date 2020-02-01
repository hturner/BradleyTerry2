context("bugs [nested model calls]")

tol <- 1e-6

## nested use of BTm (in response to Jing Hua Zhao's bug report)

## example data
x <- matrix(c(0,0, 0, 2, 0,0, 0, 0, 0, 0, 0, 0,
              0,0, 1, 3, 0,0, 0, 2, 3, 0, 0, 0,
              2,3,26,35, 7,0, 2,10,11, 3, 4, 1,
              2,3,22,26, 6,2, 4, 4,10, 2, 2, 0,
              0,1, 7,10, 2,0, 0, 2, 2, 1, 1, 0,
              0,0, 1, 4, 0,1, 0, 1, 0, 0, 0, 0,
              0,2, 5, 4, 1,1, 0, 0, 0, 2, 0, 0,
              0,0, 2, 6, 1,0, 2, 0, 2, 0, 0, 0,
              0,3, 6,19, 6,0, 0, 2, 5, 3, 0, 0,
              0,0, 3, 1, 1,0, 0, 0, 1, 0, 0, 0,
              0,0, 0, 2, 0,0, 0, 0, 0, 0, 0, 0,
              0,0, 1, 0, 0,0, 0, 0, 0, 0, 0, 0),nrow=12)
colnames(x) <- 1:12
rownames(x) <- 1:12

## function calling BTm, based on data created in function
fun1 <- function(x) {
    c2b <- countsToBinomial(x)
    names(c2b) <- c("allele1", "allele2", "transmitted", "nontransmitted")
    btx <- BTm(cbind(transmitted, nontransmitted), allele1, allele2,
               ~allele, id = "allele", data = c2b)
}

## function calling BTm, based on data and variables created in function
fun2 <- function(x) {
    c2b <- countsToBinomial(x)
    names(c2b) <- c("allele1", "allele2", "transmitted", "nontransmitted")
    denom <- with(c2b, transmitted + nontransmitted) 
    outcome <- with(c2b, transmitted/denom)
    btx <- BTm(outcome, allele1, allele2,
               ~allele, id = "allele", weights = denom, data = c2b)
}

test_that("nested call to BTm works", {
    # ignore family: mode of initialize changes between R versions
    res <- fun1(x)
    res$family <- NULL
    expect_known_value(res,
                       file = test_path("outputs/nested.rds"),
                       tol = tol)
    res2 <- fun2(x)
    res2$family <- NULL
    nm <- setdiff(names(res), c("call", "model"))
    expect_equal(res[nm], res2[nm])
})
