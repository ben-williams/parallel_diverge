source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.baseline()
x <- sim.season(x)
status_quo <- f.itall(x)

write_csv(status_quo, "output/80_10/status_quo_80_10.csv")