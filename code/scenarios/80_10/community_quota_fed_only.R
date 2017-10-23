# scenario  for community quota in federal waters only
# large vessels only

source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.port.fed()
x <- sim.season.port(x)
x <- f.itall(x)

write_csv(x, "output/80_10/community_quota_fed_only_80_10.csv")