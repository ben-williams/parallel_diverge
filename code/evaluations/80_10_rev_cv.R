# load ----
source('code/functions/helper.R')
source('code/functions/cleanup.R')


# For presentation
# theme_set(theme_bw(base_size=18) + 
#             theme(panel.grid.major = element_blank(),
#                   panel.grid.minor = element_blank()))


# data ----
sq <- read.csv("output/80_10/status_quo_80_10.csv")
ifq_fed <- read.csv("output/80_10/fed_ifq_80_10.csv") 
cq_fed <- read_csv('output/80_10/community_quota_fed_80_10.csv') 
cq_oa <- read_csv('output/80_10/community_quota_all_80_10.csv')
state_ecs <- read.csv("output/80_10/state_equal_catch_share_80_10.csv") 
state_llp <- read.csv("output/80_10/state_llp_small_80_10.csv") 
state_superx <- read.csv("output/80_10/state_super_exclusive_80_10.csv") 
psc <- read.csv("output/80_10/fed_psc_80_10.csv") 
coop <- read.csv("output/80_10/fed_coop_80_10.csv") 
coop.oa <- read.csv("output/80_10/all_coop_80_10.csv") 
ifq_fed <- read.csv("output/80_10/fed_ifq_80_10.csv") 

# label scenarios
grps <- c('SQ', '1C', '1D', '2A', '2B', '2C', '3A', '3B', '3C', '4C', '4D')

# functions ----
# cretae figures
f.ggfig <- function(x){
  ggplot(data = x, aes(group, rev, fill = Port)) + geom_boxplot(color='black', outlier.alpha = .15, outlier.size = 1) + 
    guides(fill=FALSE) +
    scale_fill_brewer(palette = "PuBu") + 
    ylab("Revenue ($ millions)") + xlab("Model") + 
    theme(plot.title = element_text(size = 10))
}
f.ggfigcv <- function(x){
  ggplot(data = x, aes(group, cv, fill = Port)) + geom_boxplot(color='black', outlier.alpha = .15, outlier.size = 1) + 
    guides(fill=FALSE) +
    scale_fill_brewer(palette = "PuBu") + 
    ylab("CV") + xlab("Model") + 
    theme(plot.title = element_text(size = 10)) + ylim(0,1000) 
}

# Global Catch CV ----
EXV <- 0.05 # exvessel price
TEXV <- round(EXV * 2204.62) # exvessel per ton
FUEL <- 0.70 # fuel cost $
sq1 = f.rev(sq, EXV, FUEL) # calculate revenue for each scenario
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)

# bind scanerios to a group label
sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
  mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
  mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

# bind all groups together
bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  # calculate cv of catch
  summarise(cv = sd(catch) / abs(mean(catch)) * 100) %>%
  # name ports
  mutate(Port = factor(case_when(d==1 ~ "KOD",
                          d==2 ~ "SPT",
                          d==3 ~ "KCO",
                          d==4 ~ "AKU"), levels=c("KOD", "SPT", "KCO", "AKU"))) %>% 
  mutate(group = factor(group, levels = grps))  -> x 
  # create figure
fig <- ggplot(x, aes(group, cv, fill = Port)) + 
            geom_boxplot(color='black', outlier.alpha = .15, outlier.size = 1) + 
            scale_fill_brewer(palette = "PuBu") + 
            ylab("Catch CV") + xlab("Model") + 
            theme(plot.title = element_text(size = 10)) +
            theme(legend.position="bottom")
ggsave(plot = fig, filename = "figs/catchcv.png", width = 6.5, height = 4, unit = "in")    

# Global Catch sum ----
EXV <- 0.05
TEXV <- round(EXV * 2204.62)
FUEL <- 0.70
sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
  mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
  mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sum(catch)/1000) %>%
  # mutate(Port = factor(d)) %>% 
  mutate(Port = factor(case_when(d==1 ~ "KOD",
                                 d==2 ~ "SPT",
                                 d==3 ~ "KCO",
                                 d==4 ~ "AKU"), levels=c("KOD", "SPT", "KCO", "AKU"))) %>% 
  mutate(group = factor(group, levels = grps))  -> x 
fig <- ggplot(x, aes(group, cv, fill = Port)) + geom_boxplot(color='black', outlier.alpha = .15, outlier.size = 1) + 
  scale_fill_brewer(palette = "PuBu") + 
  ylab("Total Catch (1,000 t)") + xlab("Model") + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom")
ggsave(plot = fig, filename = "figs/totcatch.png", width = 6.5, height = 4, unit = "in")    

# $ 0.70 fuel ----------------------------------------------------
EXV <- 0.05
TEXV <- round(EXV * 2204.62)
FUEL <- 0.70

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
 ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f0570 <- f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + expand_limits(y=-4)
f0570cv <- f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) 


# ------------------------------------------------------------------------------------------------------


EXV <- 0.10
TEXV <- round(EXV * 2204.62)
FUEL <- 0.70

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k
#a1c_0590, a1d_0590, 
bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f1070 <-f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + ylim(-2, 2)
f1070cv <-f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) 


# ------------------------------------------------------------------------------------------------------
EXV <- 0.15
TEXV <- round(EXV * 2204.62)
FUEL <- 0.70

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f1570 <- f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + expand_limits(y = 6)
f1570cv <- f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) 
 

# $ 0.80 fuel ----------------------------------------------------------------------------------------

EXV <- 0.05
TEXV <- round(EXV * 2204.62)
FUEL <- 0.80

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f0580 <-f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + expand_limits(y=-6)
f0580cv <-f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l"))  


# ---------------------------------------------------------------------------------------------
EXV <- 0.10
TEXV <- round(EXV * 2204.62)
FUEL <- 0.80

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f1080 <- f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + ylim(-2, 2)
f1080cv <- f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) 
 


# ------------------------------------------------------------------------------------------
EXV <- 0.15
TEXV <- round(EXV * 2204.62)
FUEL <- 0.80

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f1580 <- f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + expand_limits(y = 6)
f1580cv <- f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) 
 

# $ 0.90 fuel ----------------------------------------------------------------------------------------

EXV <- 0.05
TEXV <- round(EXV * 2204.62)
FUEL <- 0.90

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f0590 <-f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + expand_limits(y=-6)
f0590cv <-f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l"))  
 

# ---------------------------------------------------------------------------------------------
EXV <- 0.10
TEXV <- round(EXV * 2204.62)
FUEL <- 0.90

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f1090 <-f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + ylim(-2, 2)
f1090cv <-f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l"))  
 


# ------------------------------------------------------------------------------------------
EXV <- 0.15
TEXV <- round(EXV * 2204.62)
FUEL <- 0.90

sq1 = f.rev(sq, EXV, FUEL)
ecs1 = f.rev(state_ecs, EXV, FUEL)
cqfed1 = f.rev(cq_fed, EXV, FUEL)
cqoa1 = f.rev(cq_oa, EXV, FUEL)
llp1 = f.rev(state_llp, EXV, FUEL)
sx1 = f.rev(state_superx, EXV, FUEL)
psc1 = f.rev(psc, EXV, FUEL)
coop1 = f.rev(coop, EXV, FUEL)
coopoa1 = f.rev(coop.oa, EXV, FUEL)
ifq1 = f.rev(ifq_fed, EXV, FUEL)



sq1 %>% 
  mutate(group='SQ') -> a
bind_rows(ifq1, sx1) %>%
 mutate(group='1C') -> b
bind_rows(ifq1, ecs1) %>%
 mutate(group='1D') -> c
cqoa1 %>% 
  mutate(group='2A') -> d
bind_rows(cqfed1, llp1) %>% 
  mutate(group='2B') -> e
bind_rows(cqfed1, sx1) %>% 
  mutate(group='2C') -> f
coopoa1 %>% 
  mutate(group='3A') -> g
bind_rows(coop1, llp1) %>% 
  mutate(group='3B') -> h
bind_rows(coop1, sx1) %>% 
  mutate(group='3C') -> i
bind_rows(psc1, sx1) %>% 
  mutate(group='4C') -> j
bind_rows(psc1, ecs1) %>% 
  mutate(group='4D') -> k

bind_rows(a, b, c, d, e, f, g, h, i, j, k) %>% 
  group_by(sim, d, season, group) %>%
  summarise(cv = sd(n_rev) / abs(mean(n_rev)) * 100, rev = sum(n_rev/1000000)) %>%
  mutate(Port = factor(d)) %>% 
  mutate(group = factor(group, levels = grps)) -> x
rm(a,b,c,d,e,f,g,h,i,j,k, sq1, ifq1, cqoa1, cqfed1, llp1, sx1, psc1, coop1, coopoa1)

f1590 <- f.ggfig(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + expand_limits(y = 6)
f1590cv <- f.ggfigcv(x) + ggtitle(paste0("$", TEXV, "/t, $", FUEL, "/l")) + ylim(0,1000) 


# arrange figures ----
fig <- grid.arrange(f0570, f1070, f1570, f0580, f1080, f1580, f0590, f1090, f1590, nrow = 3, as.table = FALSE)
figcv <- grid.arrange(f0570cv, f1070cv, f1570cv, f0580cv, f1080cv, f1580cv, f0590cv, f1090cv, f1590cv,  nrow = 3, as.table = FALSE)


ggsave(plot = fig, filename = "figs/rev.png", width = 10, unit = "in")      
ggsave(plot = figcv, filename = "figs/revcv.png", width = 10, unit = "in")      



