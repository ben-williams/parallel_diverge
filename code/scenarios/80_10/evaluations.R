
cq_oa <- read_csv('output/80_10/community_quota_open_access_80_10.csv')
sq <- read.csv("output/80_10/status_quo_80_10.csv")
state_ecs <- read.csv("output/80_10/state_equal_catch_share_80_10.csv")
state_llp <- read.csv("output/80_10/state_llp_small_vessel_80_10.csv")
state_superx <- read.csv("output/80_10/state_super_exclusive_80_10.csv")

f.rev(cq_oa, 0.12, 0.8) %>%
  bind_rows(f.rev(sq, 0.12, 0.8)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev)) %>%
  ggplot(aes(sim, rev, color=factor(d))) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0,10))

f.rev(x, 0.12, 0.7) %>%
  bind_rows(f.rev(state_ecs, 0.12, 0.7)) %>%  
  bind_rows(f.rev(sq, 0.12, 0.7)) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev)) %>%
  ggplot(aes(sim, rev, color=factor(d))) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) + 
  expand_limits(x = 0, y = c(0,10))

f.rev(x, 0.12, 0.7) %>%
  bind_rows(f.rev(state_llp, 0.12, 0.7)) %>%  
  bind_rows(f.rev(sq, 0.12, 0.7)) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev)) %>%
  ggplot(aes(sim, rev, color=factor(d))) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) + 
  expand_limits(x = 0, y = c(0,10))

f.rev(x, 0.12, 0.7) %>%
  bind_rows(f.rev(state_superx, 0.12, 0.7)) %>%  
  bind_rows(f.rev(sq, 0.12, 0.7)) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev)) %>%
  ggplot(aes(sim, rev, color=factor(d))) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) + 
  expand_limits(x = 0, y = c(0,10))  

# check to see how the catch compares to the quota
f.check(state_ecs)                
f.check(sq)   
f.check(state_llp)  
f.check.port(x)  
f.check.port(cq_oa)   
f.check(state_superx)    
