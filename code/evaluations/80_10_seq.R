# load ----
source('chapter_3/code/functions/helper.R')
source('chapter_3/code/functions/tac.R')
source('chapter_3/code/functions/cleanup.R')

# function ----
f.eval <- function(x, EXV, FUEL){
  f.rev(x, EXV, FUEL) %>% 
    group_by(sim, d, rep) %>% 
    mutate(cv = sd(n_rev)/abs(mean(n_rev))) %>% 
    summarise(rev = sum(n_rev/1000000),
              TAC = mean(C1 + C2 + C3),
              cv = mean(cv),
              exv = EXV,
              fuel = FUEL)
}

fig.func <- function(x){
  ggplot(x, aes(abc, rev, lty=factor(fuel),  col=factor(exv))) + 
    facet_wrap(~Port, labeller=labeller(Port = labels), ncol = 1, scales = "free_y") + stat_smooth(se = F) +
    scale_linetype_manual(values = ltys,
                          name = "fuel price",
                          breaks = c("0.7", "0.8", "0.9"),
                          labels = c("$ 0.7/l", "$ 0.8/l", "$ 0.9/l")) + 
    scale_color_manual(values = cols,
                       name = "ex-vessel",
                       breaks = c("0.05", "0.1", "0.15"),
                       labels = c("$ 110/mt", "$ 220/mt", "$ 330/mt"),
                       guide = guide_legend(reverse=TRUE)) +
    scale_x_continuous(breaks = seq(0,250000, 50000)) +
    theme(legend.justification=c(1,0), 
          legend.position=c(1,0), 
          legend.box = "horizontal",
          strip.text.y = element_text(size=10), strip.background = element_blank(),
          plot.title = element_text(size = 10)) +
    xlab('ABC (t)') + ylab('Simulated revenue') + 
    guides(color = FALSE, linetype = FALSE) #+
    # geom_blank(aes(y = y_min)) +
    # geom_blank(aes(y = y_max))
 
}
  
# inputs ----

ltys = c("0.7" = 'solid', "0.8" = "twodash", '0.9' = "dotted")
cols = c("0.05" = "#a1dab4", "0.1" = "#41b6c4", '0.15' = "#253494")
labels = c("1" = "Port 1", "2" =  "Port 2", "3" = "Port 3", "4" = "Port 4")


# status quo ----
x <- read_csv("chapter_3/output/80_10_abc_seq/status_quo_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("SQ") -> a


# 1C - ifq / limited entry superX ----
x =  read_csv("output/80_10_abc_seq/fed_ifq_80_10_seq.csv")
y =  read_csv("output/80_10_abc_seq/state_super_exclusive_80_10_seq.csv")

left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d)) %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("1C") -> b

# 1D - community quota / limited entry - ecs ----
x =  read_csv("output/80_10_abc_seq/fed_ifq_80_10_seq.csv")
y =  read_csv("output/80_10_abc_seq/state_equal_catch_share_80_10_seq.csv")

left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("1D") -> c

# 2A - community quota / open access ----
x =  read_csv("output/80_10_abc_seq/community_quota_all_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9))) %>% 
  mutate(Port = factor(d)) %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
  fig.func(x) + ggtitle("2A") -> d
 

# 2B - community quota / limited entry ----
x =  read.csv("output/80_10_abc_seq/community_quota_fed_80_10_seq.csv")
y =  read.csv("output/80_10_abc_seq/state_llp_small_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
  fig.func(x) + ggtitle("2B")-> e

# 2C - community quota / limited entry - super X----
x =  read.csv("chapter_3/output/80_10_abc_seq/community_quota_fed_80_10_seq.csv")
y =  read.csv("chapter_3/output/80_10_abc_seq/state_super_exclusive_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("2C")-> f
# 3A - LLP coop / open access ----
x =  read_csv("output/80_10_abc_seq/all_coop_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("3A")-> g
# 3B - LLP coop / limited entry ----
x =  read_csv("output/80_10_abc_seq/fed_coop_80_10_seq.csv")
y =  read_csv("output/80_10_abc_seq/state_llp_small_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9)) )%>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("3B")-> h
# 3C - LLP coop / limited entry - super X----
x =  read.csv("output/80_10_abc_seq/fed_coop_80_10_seq.csv")
y =  read.csv("output/80_10_abc_seq/state_super_exclusive_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("3C")-> i
# 4C - PSC / limited entry - super x ----
x =  read_csv("output/80_10_abc_seq/fed_psc_80_10_seq.csv")
y =  read_csv("output/80_10_abc_seq/state_super_exclusive_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("4C")-> j

# 4D - PSC / limited entry - ecs----
x =  read.csv("output/80_10_abc_seq/fed_psc_80_10_seq.csv")
y =  read.csv("output/80_10_abc_seq/state_equal_catch_share_80_10_seq.csv")
left_join(abc.seq,
bind_rows(f.eval(x, .05, .7),
          f.eval(x, .05, .8),
          f.eval(x, .05, .9),
          
          f.eval(x, .10, .7),
          f.eval(x, .10, .8),
          f.eval(x, .10, .9),
          
          f.eval(x, .15, .7),
          f.eval(x, .15, .8),
          f.eval(x, .15, .9),
          
          f.eval(y, .05, .7),
          f.eval(y, .05, .8),
          f.eval(y, .05, .9),
          
          f.eval(y, .10, .7),
          f.eval(y, .10, .8),
          f.eval(y, .10, .9),
          
          f.eval(y, .15, .7),
          f.eval(y, .15, .8),
          f.eval(y, .15, .9))) %>% 
  mutate(Port = factor(d))  %>% 
  group_by(Port) %>% 
  mutate(y_min = min(rev),
         y_max = max(rev)) -> x
fig.func(x) + ggtitle("4D")-> k

# all figs ----
fig <- grid.arrange(a, g,  nrow = 1, as.table = FALSE) # SQ and 3A
ggsave(plot = fig, filename = "figs/seq.png", width = 6.5, height = 8, unit = "in") 
fig1 <- grid.arrange(a, b,  nrow = 1, as.table = FALSE)
ggsave(plot = fig1, filename = "figs/seq1.png", width = 6.5, height = 8, unit = "in") 
fig2 <- grid.arrange(c, d,  nrow = 1, as.table = FALSE)
ggsave(plot = fig2, filename = "figs/seq2.png", width = 6.5, height = 8, unit = "in") 
fig3 <- grid.arrange(e, f,  nrow = 1, as.table = FALSE)
ggsave(plot = fig3, filename = "figs/seq3.png", width = 6.5, height = 8, unit = "in") 
fig4 <- grid.arrange(g, h,  nrow = 1, as.table = FALSE)
ggsave(plot = fig4, filename = "figs/seq4.png", width = 6.5, height = 8, unit = "in") 
fig5 <- grid.arrange(i, j,  nrow = 1, as.table = FALSE)
ggsave(plot = fig5, filename = "figs/seq5.png", width = 6.5, height = 8, unit = "in") 
fig6 <- grid.arrange(k,  nrow = 1, ncol = 2, as.table = FALSE)
ggsave(plot = fig6, filename = "figs/seq6.png", width = 6.5, height = 8, unit = "in") 


