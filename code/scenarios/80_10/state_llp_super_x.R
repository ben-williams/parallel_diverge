source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.state.sx(abc.state)
x <- sim.season.area(x)
x <- f.simulation.sx(x)

write_csv(x, "output/80_10/state_super_exclusive_80_10.csv")

# notes
# no behavior change - prob of fishing remains same
# quota is equally split across all participants/areas/ports

# sequential abc
x <- tac.state.sx(abc.state.seq)
x <- sim.season.area(x)
x1 <- f.simulation.sx(x) %>% mutate(rep = 1)

write_csv(x1, "output/80_10_abc_seq/state_super_exclusive_80_10_seq.csv")

# to be run later
x2 <- f.simulation.sx(x) %>% mutate(rep = 2)
x2 = f.clean.between(x1, x2); gc()
x3 <- f.simulation.sx(x) %>% mutate(rep = 3)
x3 = f.clean.between(x2, x3); gc()
x4 <- f.simulation.sx(x) %>% mutate(rep = 4)
x4 = f.clean.between(x3, x4); gc()
x5 <- f.simulation.sx(x) %>% mutate(rep = 5)
aa = f.clean.between(x4, x5); gc()
