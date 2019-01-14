# test compatibility with broom
context("test-broom")

test_that("Package compatible with broom", {
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
