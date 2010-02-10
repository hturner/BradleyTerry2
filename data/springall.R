springall <- read.table("springall.txt", header=TRUE)
springall$row <- factor(springall$row, levels = 1:9)
springall$col <- (factor(c(0, springall$col), levels = 1:9))[-1]
springall$win.adj <- springall$win + springall$tie/2
springall$loss.adj <- springall$loss + springall$tie/2

springall <- list(contests = springall,
                  predictors = data.frame(
                      flav = rep(c(0.6, 4.8, 9.0), 3),
                      gel = rep(c(0, 2.4, 4.8), rep(3, 3))
                  ))
springall$predictors$flav.2 <- springall$predictors$flav ^ 2
springall$predictors$gel.2 <- springall$predictors$gel ^ 2
springall$predictors$flav.gel <- springall$predictors$flav *
                                  springall$predictors$gel
