

# status quo parallel fishery for simulations 1-20

source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

# tac fed llp with co-op
# take last 5 years of status quo data
# decide who is in the coop based upon efficiency

read.csv("output/80_10/status_quo_80_10.csv") %>% 
  filter(sim>15, size>1) -> sq2

# randomly assign 85% of fleet to cooperative
sample(unique(sq2$p_holder), .85 * length(unique(sq2$p_holder))) -> incoop
# remove the 15% who did not join the coop
sq2 %>% 
  filter(!p_holder%in% incoop) -> outcoop

x <- f.tac.incoop.fed(abc.fed, sq2, incoop)
y <- f.tac.outcoop.fed(abc.fed, outcoop)


x <- sim.season(x)
y <- sim.season(y)

x <- f.simulation.coop(x)
y <- f.simulation.coop(y)

coop = bind_rows(x, y)

write_csv(coop, "output/80_10/fed_coop_80_10.csv")

gc()
gc()
gc()
gc()
gc()
# sequential abc

read.csv("output/80_10_abc_seq/status_quo_80_10_seq.csv") %>% 
  filter(sim>15, size>1) -> sq2

# randomly assign 85% of fleet to cooperative
sample(unique(sq2$p_holder), .85 * length(unique(sq2$p_holder))) -> incoop
# remove the 15% who did not join the coop
sq2 %>% 
  filter(!p_holder%in% incoop) -> outcoop

x <- f.tac.incoop.fed(abc.fed.seq, sq2, incoop)
y <- f.tac.outcoop.fed(abc.fed.seq, outcoop)

x <- sim.season(x)
y <- sim.season(y)

x <- f.simulation(x)
y <- f.simulation(y) %>% mutate(rep = 1)

coop = bind_rows(x, y)

write_csv(coop, "output/80_10_abc_seq/fed_coop_80_10_seq.csv")
