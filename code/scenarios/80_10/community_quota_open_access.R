source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.port.all()
x <- sim.season.port(x)
x <- f.itall(x)

write_csv(x, "output/80_10/community_quota_open_access_80_10.csv")

