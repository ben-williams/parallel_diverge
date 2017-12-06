source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.state.ecs2()
x <- sim.season.p_holder(x)
x <- f.itall.ecs2(x)

write_csv(x, "output/80_10/state_equal_catch_share_80_10.csv")

# notes
# no behavior change - prob of fishing remains same
# quota is equally split across all participants/areas/ports


x %>% 
  group_by(season, area, p_holder) %>% 
  summarise(C1 = mean(C1), c1 = sum(c1),
            C2 = mean(C2), c2 = sum(c2),
            C3 = mean(C3), c3 = sum(c3)) %>% 
  ggplot(aes(C1, c1)) + geom_point() + 
  geom_abline(slope=1) +
  expand_limits(x = 0, y = 0) + 
  geom_jitter(aes(C2, c2), color = 2) + 
  geom_jitter(aes(C3, c3), color=3) 

f.rev(x, .13, FUEL) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>% 
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("llp co-op : parallel open access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3))  

  sort.list(x)
  
  x = x[order(sapply(x, '[[', i='sim'))]
  x = x[1:141]
  
  order(x,sim)
  