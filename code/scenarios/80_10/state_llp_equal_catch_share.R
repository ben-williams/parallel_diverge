source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.state.ecs(abc.state)
x <- sim.season.p_holder(x)
x <- f.simulation.ecs(x)

write_csv(x, "output/80_10/state_equal_catch_share_80_10.csv")


# sequential abc ----
x <- tac.state.ecs(abc.state.seq)
x <- sim.season.p_holder(x)

x1 <- f.simulation.ecs(x) %>% mutate(rep = 1)

x1 %>% 
  write_csv(., "output/80_10_abc_seq/state_equal_catch_share_80_10_seq.csv")

# to be run later
x2 <- f.simulation(x) %>% mutate(rep = 2)
x2 = f.clean.between(x1, x2); gc()
x3 <- f.simulation(x) %>% mutate(rep = 3)
x3 = f.clean.between(x2, x3); gc()
x4 <- f.simulation(x) %>% mutate(rep = 4)
x4 = f.clean.between(x3, x4); gc()
x5 <- f.simulation(x) %>% mutate(rep = 5)
aa = f.clean.between(x4, x5); gc()