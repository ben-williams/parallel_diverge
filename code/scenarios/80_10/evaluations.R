
source('code/functions/helper.R')
source('code/functions/cleanup.R')

cq_fed <- read_csv('output/80_10/community_quota_fed_only_80_10.csv') %>% 
  filter(sim<41)
cq_oa <- read_csv('output/80_10/community_quota_open_access_80_10.csv') %>% 
  filter(sim<41)
sq <- read.csv("output/80_10/status_quo_80_10.csv")
state_ecs <- read.csv("output/80_10/state_equal_catch_share_80_10.csv") %>% 
  filter(sim<41)
state_llp <- read.csv("output/80_10/state_llp_small_vessel_80_10.csv") %>% 
  filter(sim<41)
state_superx <- read.csv("output/80_10/state_super_exclusive_80_10.csv") %>% 
  filter(sim<41)
psc <- read.csv("output/80_10/fed_psc_80_10.csv") %>% 
  filter(sim<41)
coop <- read.csv("output/80_10/fed_coop_80_10.csv") %>% 
  filter(sim<41)
coop.oa <- read.csv("output/80_10/all_coop_80_10.csv") %>% 
  filter(sim<41)
ifq_fed <- read.csv("output/80_10/fed_ifq_80_10.csv") %>% 
  filter(sim<41)

EXV <- 0.13
TEXV <- round(0.12 * 2204.62)
FUEL <- 0.00

status_quo = f.rev(status_quo, EXV, FUEL)
sq = f.rev(sq, EXV, FUEL)
state_ecs = f.rev(state_ecs, EXV, FUEL)
cq_fed = f.rev(cq_fed, EXV, FUEL)
cq_oa = f.rev(cq_oa, EXV, FUEL)
state_llp = f.rev(state_llp, EXV, FUEL)
state_superx = f.rev(state_superx, EXV, FUEL)
psc = f.rev(psc, EXV, FUEL)
coop = f.rev(coop, EXV, FUEL)
coop.oa = f.rev(coop.oa, EXV, FUEL)
ifq_fed = f.rev(ifq_fed, EXV, FUEL)
head(ifq_fed)


status_quo %>% 
  mutate(group='sq') -> asq
bind_rows(ifq_fed, state_superx) %>% 
  mutate(group='1c') -> a1c
bind_rows(ifq_fed, state_ecs) %>% 
  mutate(group='1d') -> a1d
cq_oa %>% 
  mutate(group='2a') -> a2a
bind_rows(cq_fed, state_llp) %>% 
  mutate(group='2b') -> a2b
bind_rows(cq_fed, state_superx) %>% 
  mutate(group='2c') -> a2c
coop.oa %>% 
  mutate(group='3a') -> a3a
bind_rows(coop, state_llp) %>% 
  mutate(group='3b') -> a3b
bind_rows(coop, state_superx) %>% 
  mutate(group='3c') -> a3c
bind_rows(psc, state_superx) %>% 
  mutate(group='4c') -> a4c
bind_rows(psc, state_ecs) %>% 
  mutate(group='4d') -> a4d

bind_rows(asq, a1c, a1d, a2a, a2b, a2c, a3a, a3b, a3c, a4c, a4d) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / mean(n_rev), rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d))  -> dat

  ggplot(dat, aes(group, rev, fill = Port)) + geom_boxplot(color='gray') + 
    theme_black()

  
  theme_black <- function (base_size = 16, base_family = ""){
    theme_minimal() + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                            lineend = "butt"), 
        rect = element_rect(fill = "white", 
                            colour = "white", size = 0.5, linetype = 1), 
        text = element_text(family = base_family, 
                            face = "plain", colour = "white", size = base_size,
                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
        plot.background = element_rect(colour = 'black', fill = 'black'),
        plot.title = element_text(size = rel(1.2)),
        panel.border = element_rect(fill = NA, colour = "white"), 
        strip.background = element_rect(fill = "grey30", colour = "white")
      )
  }














# revenue figs ----
# 1C
f.rev(ifq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("ifq : llp-super-exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig1c

# 1D
f.rev(ifq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_ecs, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("llp co-op : llp-equal catch-shares"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) +
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1)) -> fig1d

# 3A
f.rev(coop.oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("llp co-op : parallel open access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig3a

# 3B
f.rev(coop, EXV, FUEL) %>%
  bind_rows(f.rev(state_llp, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("llp co-op : llp-small vessels"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) +
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1))-> fig3b

#3C
f.rev(coop, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("Simulation") +
  ggtitle(paste("llp co-op : llp-super-exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig3c

#2A
f.rev(cq_oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("community quota : parallel open-access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig2a

#2B
f.rev(cq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("community quota : llp-super exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3))  + 
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1)) -> fig2b

#2C
f.rev(cq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_llp, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("Simulation") +
  ggtitle(paste("community quota : llp-small vessels"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig2c

# 4C
f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("") +
  ggtitle(paste("psc : llp-super exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig4c

# 4D
f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_ecs, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  filter(sim<41) %>% 
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(mean = mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, rev, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 7.5)) +
  ylab('$ millions') + xlab("Simulation") +
  ggtitle(paste("psc : llp-equal catch shares"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) + 
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1))  -> fig4d


grid.arrange( fig1c, fig1d, nrow=3)
ggsave("figs/fig_rev_1cd.png", arrangeGrob(fig1c, fig1d, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

grid.arrange( fig2a, fig2b, fig2c, nrow=3)
ggsave("figs/fig_rev_2abc.png", arrangeGrob(fig2a, fig2b, fig2c, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

grid.arrange(fig3a, fig3b, fig3c, nrow=3)
ggsave("figs/fig_rev_3abc.png", arrangeGrob(fig3a, fig3b, fig3c, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

grid.arrange(fig4c, fig4d, nrow=2)
ggsave("figs/fig_rev_4cd.png", arrangeGrob(fig4c, fig4d, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

# CV figs ----

f.rev(coop.oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("llp co-op : llp-small vessels"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig3a

# 3B
f.rev(coop, EXV, FUEL) %>%
  bind_rows(f.rev(state_llp, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("llp co-op : llp-small vessels"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3))+
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1)) -> fig3b

#3C
f.rev(coop, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("llp co-op : llp-super-exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig3c

#2A
f.rev(cq_oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("community quota : parallel open-access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig2a

#2B
f.rev(cq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("community quota : llp-super exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) +
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1)) -> fig2b

#2C
f.rev(cq_fed, EXV, FUEL) %>%
  bind_rows(f.rev(state_llp, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("community quota : llp-small vessels"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig2c

# 4C
f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("prohibited species allocation : llp-super exclusive"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 15, 2, 3)) -> fig4c

# 4D
f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_ecs, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d)) %>%
  ggplot(aes(sim, cv, shape=Port)) + geom_point(fill='gray') +
  geom_vline(xintercept=20, lty=4, alpha = 0.5) +
  expand_limits(x = 0, y = c(0, 1.5)) +
  ylab('CV') + xlab("") +
  ggtitle(paste("prohibited species allocation : llp-equal catch shares"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  theme(legend.justification=c(1,0), legend.position=c(1,.45)) +
  scale_shape_manual(values = c(21, 15, 2, 3)) +
  theme(legend.justification=c(1,0), legend.position=c(1,1)) + guides(shape = guide_legend(nrow = 1)) -> fig4d


grid.arrange(fig1c, fig1d, nrow=3)
ggsave("figs/fig_cv_1cd.png", arrangeGrob(fig1c, fig1d nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

grid.arrange(fig2a, fig2b, fig2c, nrow=3)
ggsave("figs/fig_cv_2abc.png", arrangeGrob(fig2a, fig2b, fig2c, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

grid.arrange(fig3a, fig3b, fig3c, nrow=3)
ggsave("figs/fig_cv_3abc.png", arrangeGrob(fig3a, fig3b, fig3c, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")

grid.arrange(fig4c, fig4d, nrow=2)
ggsave("figs/fig_cv_4cd.png", arrangeGrob(fig4c, fig4d, nrow=3), 
       device = 'png', width = 6.5,  height = 8, units = "in")


# stranded quota ----
bind_rows(sq, psc, state_superx) %>% 
  group_by(sim, area, season) %>% 
  mutate(diff = ifelse(area==1, mean(C1) - sum(c1),
                       if_else(area==2, mean(C2) - sum(c2), 
                               mean(C3)-sum(c3)))) %>% 
  group_by(sim, area) %>%
  summarise(diff = sum(diff)) %>%
  ggplot(aes(sim, diff, color=factor(area))) + geom_point()

bind_rows(sq, psc, state_ecs) %>% 
  group_by(sim, area, season) %>% 
  mutate(diff = ifelse(area==1, mean(C1) - sum(c1),
                          if_else(area==2, mean(C2) - sum(c2), 
                                  mean(C3)-sum(c3)))) %>% 
  group_by(sim, area) %>%
  summarise(diff = sum(diff)) %>%
  ggplot(aes(sim, diff, color=factor(area))) + geom_point()

bind_rows(sq, cq_fed, state_ecs) %>% 
  group_by(sim, area, season) %>% 
  mutate(diff = ifelse(area==1, mean(C1) - sum(c1),
                       if_else(area==2, mean(C2) - sum(c2), 
                               mean(C3)-sum(c3)))) %>% 
  group_by(sim, area) %>%
  summarise(diff = sum(diff)) %>%
  ggplot(aes(sim, diff, color=factor(area))) + geom_point()

# check to see how the catch compares to the quota
f.check.individual(state_ecs)                
f.check(sq)   
f.check(state_llp)  
f.check(state_superx)  
f.check(psc)   
f.check.port(cq_fed)  
f.check.port(cq_oa)   
f.check.individual(ifq_fed)       


# other figs
f.rev(sq, EXV, FUEL) %>% 
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d), rev = mean(rev)) %>% 
  mutate(group = factor('sq')) -> a

f.rev(ifq_fed, EXV, FUEL) %>% 
  left_join(f.rev(state_superx, EXV, FUEL)) %>% 
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d), rev = mean(rev)) %>% 
  mutate(group = factor('ifq:ecs')) %>% 
  bind_rows(a) %>% 
    ggplot(aes(group, rev, fill=Port)) + geom_violin() + 
  scale_fill_brewer(palette = "Blues")
  
f.rev(ifq_fed, EXV, FUEL) %>% 
  left_join(f.rev(state_superx, EXV, FUEL)) %>% 
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d), rev = mean(rev)) %>% 
  mutate(group = factor('ifq:sx')) %>% 
  bind_rows(a, b) %>% 
  ggplot(aes(group, rev, fill=Port)) + geom_boxplot() + 
  scale_fill_brewer(palette = "Blues")

  

f.rev(coop.oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d),
         group = factor(ifelse(sim<21,'status quo','coop:oa'), 
                        levels = c('status quo','coop:oa'))) -> a
%>% 
  left_join(f.rev(coop, EXV, FUEL)) %>% 
  left_join() %>% 
  
  ggplot(aes(group, rev)) + geom_boxplot()


# rev boxplots
f.rev(coop.oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(d) %>%
  mutate(Port = factor(d), 
         group = factor(ifelse(sim<21,'status quo','simulation'), 
                                          levels = c('status quo','simulation'))) -> s1


f.rev(coop.oa, EXV, FUEL) %>%
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d),
         group = factor(ifelse(sim<21,'status quo','simulation'), 
                        levels = c('status quo','simulation'))) %>% 
  left_join(s1) %>% 
  ggplot(aes(rev, cv, shape = group, fill=group)) + 
  geom_point(alpha=.8) +
  expand_limits(x = 0, y = c(0, 2)) +
  ylab('CV') + xlab("$ millions") +
  ggtitle(paste("llp co-op : parallel open access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 1))


# 3B
f.rev(coop, EXV, FUEL) %>%
  bind_rows(f.rev(state_llp, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>%  
  group_by(sim, d, season) %>%
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d),
         group = factor(ifelse(sim<21,'status quo','simulation'), 
                        levels = c('status quo','simulation'))) %>% 
  left_join(s1) %>% 
  ggplot(aes(rev, cv, shape = group, fill=group)) + 
  geom_point(alpha=.8) +
  expand_limits(x = 0, y = c(0, 2)) +
  ylab('CV') + xlab("$ millions") +
  ggtitle(paste("llp co-op : parallel open access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 1))

f.rev(psc, EXV, FUEL) %>%
  bind_rows(f.rev(state_superx, EXV, FUEL)) %>%  
  bind_rows(f.rev(sq, EXV, FUEL)) %>% 
  group_by(sim, d, season) %>%
  filter(sim<120) %>% 
  summarise(rev = sum(n_rev/1000000)) %>%
  group_by(sim, d) %>%
  mutate(cv = sd(rev)/mean(rev), Port = factor(d),
         group = factor(ifelse(sim<21,'status quo','simulation'), 
                        levels = c('status quo','simulation'))) %>% 
  left_join(s1) %>% 
  ggplot(aes(rev, cv, shape = group, fill=group)) + 
  geom_point(fill='black', alpha=.5) +
  expand_limits(x = 0, y = c(0, 2)) +
  ylab('CV') + xlab("$ millions") +
  ggtitle(paste("llp co-op : parallel open access"), 
          subtitle=(paste0("ex-vessel = $", TEXV, "/t, fuel = ", FUEL,"/l"))) + 
  theme(legend.position="none") +
  scale_shape_manual(values = c(21, 1))
