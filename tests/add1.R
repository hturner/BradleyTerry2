library(BradleyTerry2)
data(flatlizards, package = "BradleyTerry2")

result <- rep(1, nrow(flatlizards$contests))
BTmodel1 <- BTm(result, winner, loser,
                ~ throat.PC1[..] + throat.PC3[..] + (1|..),
                data = flatlizards,
                tol = 1e-4, sigma = 2, trace = TRUE)

drop1(BTmodel1)

add1(BTmodel1, ~ . + head.length[..] + SVL[..], test = "Chisq")

BTmodel2 <- update(BTmodel1, formula = ~ . + head.length[..])

drop1(BTmodel2, test = "Chisq")
