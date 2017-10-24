source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.state.ecs()
x <- sim.season.p_holder(x)
x <- f.itall.ecs(x)

write_csv(x, "output/80_10/state_equal_catch_share_80_10.csv")

# notes
# no behavior change - prob of fishing remains same
# quota is equally split across all participants/areas/ports


