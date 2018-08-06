# scenario  for community quota in federal waters only, with open access
# in state waters
# all vessels included aka parallel fishery

source('code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.port.all(abc)
x <- sim.season.port(x)
x <- f.simulation.cq(x)

write_csv(x, "output/80_10/community_quota_all_80_10.csv")

# sequential abc

x <- tac.port.all(abc.seq)
x <- sim.season.port(x)
x <- f.simulation(x) %>% mutate(rep = 1)

gc()
gc()
gc()

write_csv(x, "chapter_3/output/80_10_abc_seq/community_quota_all_80_10_seq.csv")

# to be run later
x1 <- read_csv("chapter_3/output/80_10_abc_seq/community_quota_all_80_10_seq.csv")
x2 <- f.simulation(x) %>% mutate(rep = 2)
x2 = bind_rows(x1, x2); gc(); gc(); gc(); gc(); rm(x1)
x3 <- f.simulation(x) %>% mutate(rep = 3)
x3 = bind_rows(x2, x3); gc(); gc(); gc(); gc(); rm(x2)
x4 <- f.simulation(x) %>% mutate(rep = 4)
x4 = bind_rows(x3, x4); gc(); gc(); gc(); gc(); rm(x3)
x5 <- f.simulation(x) %>% mutate(rep = 5)
aa = bind_rows(x4, x5); gc(); gc(); gc(); gc(); rm(x5)
write_csv(aa, "chapter_3/output/80_10_abc_seq/community_quota_all_80_10_seq.csv")