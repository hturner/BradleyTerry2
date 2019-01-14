context("test-predict")

data(baseball)
# Standard BT model
baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")

# BT model with home advantage (don't rely on names in call)
baseballModel2 <- update(baseballModel1, 
                         player1 = data.frame(team = baseball$home.team, at.home = 1),
                         player2 = data.frame(team = baseball$away.team, at.home = 0),
                         formula = ~ team + at.home)

# set up test data
test_teams <- c("Milwaukee", "Detroit")
all_teams <- levels(baseball$home.team)

test_that("predict `type = ability` works for standard BT", {
    test1 <- list(player1 = factor(test_teams, levels = all_teams))
    
    pred <- predict(baseballModel1, newdata = test1, type = "ability", 
                    se.fit = TRUE)
    abilities <- BTabilities(baseballModel1)
    expect_equal(pred$fit, BTabilities(baseballModel1)[test_teams, "ability"],
                 check.attributes = FALSE)
    expect_equal(pred$se, BTabilities(baseballModel1)[test_teams, "s.e."],
                 check.attributes = FALSE)
    
    test2 <- data.frame(home.team = factor(test_teams, levels = all_teams))
    
    pred2 <- predict(baseballModel1, newdata = test2, type = "ability", 
                     se.fit = TRUE)
    expect_equal(pred2$fit, BTabilities(baseballModel1)[test_teams, "ability"],
                 check.attributes = FALSE)
    expect_equal(pred2$se, BTabilities(baseballModel1)[test_teams, "s.e."],
                 check.attributes = FALSE)
})

test_that("predict `type = ability` works for BT with home advantage", {
    # predict with and without home advantage
    test1 <- list(player1 = data.frame(team = factor(test_teams, 
                                                     levels = all_teams),
                                       at.home = rep(c(1, 0), each = 2)))
    pred1 <- predict(baseballModel2, newdata = test1, type = "ability", 
                     se.fit = TRUE)
    # predict against reference team with no home advantage
    test2 <- list(player1 = data.frame(team = factor(test_teams, 
                                                     levels = all_teams),
                                       at.home = rep(c(1, 0), each = 2)),
                  player2 = data.frame(team = factor("Baltimore", 
                                                     levels = all_teams),
                                       at.home = rep(c(0, 0), each = 2)))
    pred2 <- predict(baseballModel2, newdata = test2, type = "link", 
                    se.fit = TRUE)
    expect_equal(pred1, pred2)
})

# item-specific

## converges to fixed effects model
springall.model <- BTm(cbind(win.adj, loss.adj), col, row, 
                       ~ flav[..] + gel[..] + 
                           flav.2[..] + gel.2[..] + flav.gel[..] +
                           (1 | ..),
                       data = springall)

gel_seq <- seq(0, 4.8, length.out = 20)
flav_seq <- seq(0, 9, length.out = 20)
grid_features <- expand.grid(flav = flav_seq, gel = gel_seq)
grid_features <- cbind(`..` = factor(1:nrow(grid_features)),
                       grid_features)
features <- within(grid_features, {
    gel.2 <- gel^2
    flav.gel <- flav * gel
    flav.2 <- flav^2
})

test_that("predict `type = ability` works for item-specific covariates", {
    # predict over grid of flav and gel
    pred <- predict(springall.model, newdata = features, 
                    type = "ability", se.fit = TRUE)
    # predict for contests against first "player", where all
    n <- nrow(features)
    contests <- data.frame(col = factor(1:n, levels = 1:n),
                           row = factor(1, levels = 1:n))
    pred2 <- predict(springall.model, list(contests, features), se.fit = TRUE)
    expect_equal(pred, pred2)
})

