cq_fed <- read_csv('output/80_10/community_quota_fed_only_80_10.csv')
cq_oa <- read_csv('output/80_10/community_quota_open_access_80_10.csv')
sq <- read.csv("output/80_10/status_quo_80_10.csv")
state_ecs <- read.csv("output/80_10/state_equal_catch_share_80_10.csv")
state_llp <- read.csv("output/80_10/state_llp_small_vessel_80_10.csv")
state_superx <- read.csv("output/80_10/state_super_exclusive_80_10.csv")
psc <- read.csv("output/80_10/fed_psc_80_10.csv")

EXV <- 0.13
FUEL <- 0.80



f.rev(cq_oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, color=Port)) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 10)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("community quota : parallel open-access"), 
          subtitle=(paste0("ex-vessel = $", EXV, "/lb, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none")-> fig1

f.rev(cq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, color=Port)) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 10)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("community quota : llp-super exclusive"), 
          subtitle=(paste0("ex-vessel = $", EXV, "/lb, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") -> fig2

f.rev(cq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_llp, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, color=Port)) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 10)) +
  ylab('$ millions') + xlab("Simulation") +
  ggtitle(paste("community quota : llp-small vessels"), 
          subtitle=(paste0("ex-vessel = $", EXV, "/lb, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none")-> fig3

f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, color=Port)) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 10)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("prohibited species allocation : llp-super exclusive"), 
          subtitle=(paste0("ex-vessel = $", EXV, "/lb, fuel = ", FUEL,"/l"))) + 
  theme(legend.justification=c(1,0), legend.position=c(1,.45))-> fig4

f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_ecs, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, color=Port)) + geom_point() +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 10)) +
  ylab('$ millions') + xlab("Simulation") +
  ggtitle(paste("prohibited species allocation : llp-equal catch shares"), 
          subtitle=(paste0("ex-vessel = $", EXV, "/lb, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") -> fig5

grid.arrange(fig1, fig2, fig3, nrow=3)
grid.arrange(fig4, fig5, nrow=3)

# check to see how the catch compares to the quota
f.check.individual(state_ecs)                
f.check(sq)   
f.check(state_llp)  
f.check(state_superx)  
f.check(psc)   
f.check.port(cq_fed)  
f.check.port(cq_oa)   
