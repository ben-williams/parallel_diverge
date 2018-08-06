# status quo parallel fishery for simulations 1-20

source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.baseline(abc)
x <- sim.season(x)
status_quo <- f.simulation(x)

write_csv(status_quo, "output/80_10/status_quo_80_10.csv")

f.rev(status_quo, 0.12, 0.7) %>% 
  group_by(sim, d) %>% 
  summarise(rev = sum(n_rev/1000000)) %>% 
  group_by(d) %>% 
  mutate(mean = mean(rev)) %>% 
  ggplot(aes(sim, rev, color=factor(d))) + geom_point() + 
  geom_line(aes(sim, mean, color=factor(d)), lty=4) + 
  expand_limits(x = 0, y = 0)
