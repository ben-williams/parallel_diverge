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

EXV <- 0.15
TEXV <- round(EXV * 2204.62)
FUEL <- 0.70

f.rev(sq, EXV, FUEL)%>% 
  mutate(group='SQ') -> asq
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



bind_rows(ifq_fed, state_superx) %>% 
  mutate(group='1C') -> a1c
bind_rows(ifq_fed, state_ecs) %>% 
  mutate(group='1D') -> a1d
cq_oa %>% 
  mutate(group='2A') -> a2a
bind_rows(cq_fed, state_llp) %>% 
  mutate(group='2B') -> a2b
bind_rows(cq_fed, state_superx) %>% 
  mutate(group='2C') -> a2c
coop.oa %>% 
  mutate(group='3A') -> a3a
bind_rows(coop, state_llp) %>% 
  mutate(group='3B') -> a3b
bind_rows(coop, state_superx) %>% 
  mutate(group='3C') -> a3c
bind_rows(psc, state_superx) %>% 
  mutate(group='4C') -> a4c
bind_rows(psc, state_ecs) %>% 
  mutate(group='4D') -> a4d

grps <- c('SQ', '1C', '1D', '2A', '2B', '2C', '3A', '3B', '3C', '4C', '4D')

bind_rows(asq, a1c, a1d, a2a, a2b, a2c, a3a, a3b, a3c, a4c, a4d) %>% 
  asq %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / mean(n_rev), rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> dat
# Rev Figs

ggplot(dat, aes(group, rev, fill = Port)) + geom_boxplot(color='black') + 
  theme_dark() + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                                            lineend = "butt"), 
                        rect = element_rect(fill = "white", 
                                            colour = "white", size = 0.5, linetype = 1), 
                        text = element_text(face = "plain", colour = "white", size = 20,
                                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
                        plot.background = element_rect(colour = 'black', fill = 'gray50'),
                        strip.background = element_rect(fill = "grey50", colour = "white"),
                        panel.border = element_rect(fill = NA, colour = "white"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.position=c(.85, .8)) + 
  scale_fill_brewer(palette = "PuBu") + 
  geom_rect(aes(xmin = 3.5, xmax = 6.5, ymin = 0, ymax = 11), fill = 'gray50') +
  geom_rect(aes(xmin = 7.5, xmax = 9.5, ymin = 0, ymax = 11.5), fill = 'gray50') +
  ylab("Revenue") + xlab("Model") + ggtitle("Bounding scenarios")

ggplot(dat, aes(group, rev, fill = Port)) + geom_boxplot(color='black') + 
  theme_dark() + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                                            lineend = "butt"), 
                        rect = element_rect(fill = "white", 
                                            colour = "white", size = 0.5, linetype = 1), 
                        text = element_text(face = "plain", colour = "white", size = 20,
                                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
                        plot.background = element_rect(colour = 'black', fill = 'gray50'),
                        strip.background = element_rect(fill = "grey50", colour = "white"),
                        panel.border = element_rect(fill = NA, colour = "white"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.position=c(.85, .8)) + 
  scale_fill_brewer(palette = "PuBu") + 
  geom_rect(aes(xmin = 1.5, xmax = 3.5, ymin = 0, ymax = 11.5), fill = 'gray50') +
  geom_rect(aes(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 11), fill = 'gray50') +
  ylab("Revenue") + xlab("Model") + ggtitle("Likely scenarios")

ggplot(dat, aes(group, rev, fill = Port)) + geom_boxplot(color='black') + 
  theme_dark() + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                                            lineend = "butt"), 
                        rect = element_rect(fill = "white", 
                                            colour = "white", size = 0.5, linetype = 1), 
                        text = element_text(face = "plain", colour = "white", size = 20,
                                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
                        plot.background = element_rect(colour = 'black', fill = 'gray50'),
                        strip.background = element_rect(fill = "grey50", colour = "white"),
                        panel.border = element_rect(fill = NA, colour = "white"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.position=c(.85, .8)) + 
  scale_fill_brewer(palette = "PuBu") + 
  ylab("Revenue") + xlab("Model") + ggtitle("All scenarios")


# CV Figs

ggplot(dat, aes(group, cv, fill = Port)) + geom_boxplot(color='black') + 
  theme_dark() + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                                            lineend = "butt"), 
                        rect = element_rect(fill = "white", 
                                            colour = "white", size = 0.5, linetype = 1), 
                        text = element_text(face = "plain", colour = "white", size = 20,
                                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
                        plot.background = element_rect(colour = 'black', fill = 'gray50'),
                        strip.background = element_rect(fill = "grey50", colour = "white"),
                        panel.border = element_rect(fill = NA, colour = "white"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.position=c(.85, .8)) + 
  scale_fill_brewer(palette = "PuBu") + 
  geom_rect(aes(xmin = 3.5, xmax = 6.5, ymin = 0, ymax = 3), fill = 'gray50') +
  geom_rect(aes(xmin = 7.5, xmax = 9.5, ymin = 0, ymax = 3), fill = 'gray50') +
  ylab("CV") + xlab("Model") + ggtitle("Bounding scenarios")

ggplot(dat, aes(group, cv, fill = Port)) + geom_boxplot(color='black') + 
  theme_dark() + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                                            lineend = "butt"), 
                        rect = element_rect(fill = "white", 
                                            colour = "white", size = 0.5, linetype = 1), 
                        text = element_text(face = "plain", colour = "white", size = 20,
                                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
                        plot.background = element_rect(colour = 'black', fill = 'gray50'),
                        strip.background = element_rect(fill = "grey50", colour = "white"),
                        panel.border = element_rect(fill = NA, colour = "white"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.position=c(.45, .8)) + 
  scale_fill_brewer(palette = "PuBu") + 
  geom_rect(aes(xmin = 1.5, xmax = 3.5, ymin = 0, ymax = 3), fill = 'gray50') +
  geom_rect(aes(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 4), fill = 'gray50') +
  ylab("CV") + xlab("Model") + ggtitle("Likely scenarios")
  
ggplot(dat, aes(group, cv, fill = Port)) + geom_boxplot(color='black') + 
  theme_dark() + theme( panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                                            lineend = "butt"), 
                        rect = element_rect(fill = "white", 
                                            colour = "white", size = 0.5, linetype = 1), 
                        text = element_text(face = "plain", colour = "white", size = 20,
                                            angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
                        plot.background = element_rect(colour = 'black', fill = 'gray50'),
                        strip.background = element_rect(fill = "grey50", colour = "white"),
                        panel.border = element_rect(fill = NA, colour = "white"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.position=c(.85, .8)) + 
  scale_fill_brewer(palette = "PuBu") + 
  ylab("Revenue") + xlab("Model") + ggtitle("All scenarios")
