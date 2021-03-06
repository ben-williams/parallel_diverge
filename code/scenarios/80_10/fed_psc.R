# scenario  for community quota in federal waters only, with open access
# in state waters
# all vessels included aka parallel fishery

source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.fed.psc(abc.fed)
x <- sim.season(x)
x <- f.simulation.psc(x)

write_csv(x, "output/80_10/fed_psc_80_10.csv")


# sequential abc ----
x <- tac.fed.psc(abc.fed.seq)
x <- sim.season(x)
x1 <- f.simulation.psc(x) %>% mutate(rep = 1)
gc()

x1 %>% 
  write_csv(., "output/80_10_abc_seq/fed_psc_80_10_seq.csv")

# to be run later
x2 <- f.simulation(x) %>% mutate(rep = 2)
x2 = f.clean.between(x1, x2); gc()
x3 <- f.simulation(x) %>% mutate(rep = 3)
x3 = f.clean.between(x2, x3); gc()
x4 <- f.simulation(x) %>% mutate(rep = 4)
x4 = f.clean.between(x3, x4); gc()
x5 <- f.simulation(x) %>% mutate(rep = 5)
aa = f.clean.between(x4, x5); gc()