# scenario  for community quota in federal waters only
# large vessels only

source('chapter_3/code/functions/model.R')

# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.port.fed(abc.fed)
x <- sim.season.port(x)
x <- f.simulation.cq(x)
gc()
gc()
write_csv(x, "output/80_10/community_quota_fed_80_10.csv")


gc()
gc()
# sequential abc
x <- tac.port.fed(abc.fed.seq)
x <- sim.season.port(x)
x1 <- f.simulation(x) %>% mutate(rep = 1)
gc()
gc()

write_csv(x1, "output/80_10_abc_seq/community_quota_fed_80_10_seq.csv")

# to be run later
read_csv("output/80_10_abc_seq/community_quota_fed_80_10_seq.csv")

x2 <- f.simulation(x) %>% mutate(rep = 2)
x3 = bind_rows(x1, x2); gc(); gc(); gc(); gc(); rm(x1)
x3 <- f.simulation(x) %>% mutate(rep = 3)
x3 = bind_rows(x2, x3); gc(); gc(); gc(); gc(); rm(x2)
x4 <- f.simulation(x) %>% mutate(rep = 4)
x4 = bind_rows(x3, x4); gc(); gc(); gc(); gc(); rm(x3)
x5 <- f.simulation(x) %>% mutate(rep = 5)
aa = bind_rows(x4, x5); gc(); gc(); gc(); gc(); rm(x5)
write_csv(aa, "chapter_3/output/80_10_abc_seq/community_quota_fed_80_10_seq.csv")
