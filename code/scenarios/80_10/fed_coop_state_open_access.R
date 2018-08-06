

# status quo parallel fishery for simulations 1-20

source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel


# tac fed llp with co-op
# take last 5 years of status quo data
# decide who is in the coop based upon efficiency

read.csv("output/80_10/status_quo_80_10.csv") %>% 
  filter(sim>15) -> sq2

# randomly assign 85% of fleet to cooperative
sample(unique(sq2$p_holder), .85 * length(unique(sq2$p_holder))) -> incoop
# remove the 15% who did not join the coop
sq2 %>% 
  filter(!p_holder%in% incoop) -> outcoop

x <- f.tac.incoop.all(abc, sq2, incoop)
y <- f.tac.outcoop.all(abc, outcoop)

x <- sim.season(x)
y <- sim.season(y)

x <- f.simulation.coop(x)
y <- f.simulation.coop(y)

coop = bind_rows(x, y)

write_csv(coop, "output/80_10/all_coop_80_10.csv")


# sequential abc
# tac fed llp with co-op
# take last 5 years of status quo data
# decide who is in the coop based upon efficiency

read.csv("output/80_10_abc_seq/status_quo_80_10_seq.csv") %>% 
  filter(sim>15) -> sq2

# randomly assign 85% of fleet to cooperative
sample(unique(sq2$p_holder), .85 * length(unique(sq2$p_holder))) -> incoop
# remove the 15% who did not join the coop
sq2 %>% 
  filter(!p_holder%in% incoop) -> outcoop

x <- f.tac.incoop.all(abc.seq, sq2, incoop)
y <- f.tac.outcoop.all(abc.seq, outcoop)

x <- sim.season(x)
y <- sim.season(y)

x <- f.simulation.coop(x)
y <- f.simulation.coop(y)

coop = bind_rows(x, y) %>% mutate(rep = 1)

write_csv(coop, "output/80_10_abc_seq/all_coop_80_10_seq.csv")









f.rev(coop, 0.12, 0.8) %>% 
  bind_rows(f.rev(sq, 0.12, 0.8)) %>% 
  group_by(sim, d) %>% 
  summarise(rev = sum(n_rev/1000000)) %>% 
  group_by(d) %>% 
  mutate(mean = mean(rev)) %>% 
  ggplot(aes(sim, rev, color=factor(d))) + geom_point() + 
  geom_line(aes(sim, mean, color=factor(d)), lty=4) + 
  expand_limits(x = 0, y = 0)

# tac fed llp with co-op
# take last 5 years of status quo data

# decide who is in the coop

sq %>% 
  filter(sim>15) -> sq2

# there are not enough vessels originating from various ports for meaningful coops...
# f.boats_large() %>% 
#   left_join(sq2) %>% 
#   filter(port==d) %>% 
#   group_by(p_fshy, season, port) %>% 
#   summarise(n=length(unique(p_holder))) %>% arrange(season, port) %>% data.frame

sample(unique(sq2$p_holder), .85 * length(unique(sq2$p_holder))) -> incoop
sq2 %>% 
  filter(!p_holder%in% incoop) -> outcoop

