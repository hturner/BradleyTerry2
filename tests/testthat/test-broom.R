# test compatibility with broom
context("test-broom")

test_that("augment works based on original data", {
    skip_on_cran()
    
    library(broom)
    
    # Standard BT model
    data(baseball)
    baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                          data = baseball, id = "team")
    
    # Augment original fit with contest-level data, glm fitted values
    res <- augment(baseballModel1)
    expect_known_output(res, 
                        file = "outputs/augment.original.link.rds")
    
    # Augment original data with fitted abilities
    # - works, but result not structured well for .se.fit
    res2 <- augment(baseballModel1, type.predict = "ability")
    expect_known_output(res2, 
                        file = "outputs/augment.original.ability.rds")
})
 
test_that("augment works based on new data, standard BT", {   
    skip_on_cran()
    
    library(broom)
    
    # set up test data
    test_teams <- c("Milwaukee", "Detroit")
    all_teams <- levels(baseball$home.team)
    test <- data.frame(home.team = factor(test_teams, levels = all_teams),
                       away.team = factor(rev(test_teams), levels = all_teams))
    
    # Augment new contests with contest-level data
    res <- augment(baseballModel1, newdata = test)
    expect_known_output(res2, 
                        file = "outputs/augment.new.link.rds")
    
    # Augment new contests with fitted abilities for both players
    # - works, but result not structured well for .se.fit
    res2 <- augment(baseballModel1, newdata = test, type.predict = "ability")
    expect_known_output(res2, 
                        file = "outputs/augment.new.ability.both.rds")
    
    # Augment new one-sided contests with fitted abilities for player
    # - works, but result not structured well for .se.fit
    res2 <- augment(baseballModel1, newdata = test[1], type.predict = "ability")
    expect_known_output(res2, 
                        file = "outputs/augment.new.ability.one.rds")
    
})

test_that("augment works based on new data, with home advantage", {   
    skip_on_cran()
    
    library(broom)
    
    baseballModel2 <- update(baseballModel1, 
                             player1 = data.frame(team = baseball$home.team, 
                                                  at.home = 1),
                             player2 = data.frame(team = baseball$away.team, 
                                                  at.home = 0),
                             formula = ~ team + at.home)
    
    # Augment original fit with contest-level data, glm fitted values
    res <- augment(baseballModel2)
    
    # set up test data
    test_teams <- c("Milwaukee", "Detroit")
    all_teams <- levels(baseball$home.team)
    home.team <- data.frame(team = factor(test_teams, levels = all_teams),
                            at.home = 1)
    away.team <- data.frame(team = factor(rev(test_teams), levels = all_teams),
                            at.home = 0)
    
    predict(baseballModel2, 
            newdata = list(player1 = home.team, player2 = away.team))
    
    # Augment new contests with contest-level data
    # doesn't work as augment tries to make newdata a tibble
    #res <- augment(baseballModel2, 
    #               newdata = list(player1 = home.team, player2 = away.team),
    #               type.predict = "link")
    
})
