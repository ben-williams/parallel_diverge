# step 1 - source models & data
# Note that the port and delivery locatoins were kept the same for this sim!
source('code/functions/model.R')
# Define world ----
fuel_price = 0.80 # set fuel price
ex1 <- ex2 <- ex3 <- ex4 <- 0.10 # set ex-vessel

x <- tac.fed.ifq(abc.fed)
x <- sim.season.p_holder(x)
x1 <- x[1:2000]
x2 <- x[2001:4000]
x3 <- x[4001:6000]
x4 <- x[6001:8000]
x5 <- x[8001:9840]

x1 <- f.simulation.ifq(x1)
x2 <- f.simulation.ifq(x2)
x3 <- f.simulation.ifq(x3)
x4 <- f.simulation.ifq(x4)
x5 <- f.simulation.ifq(x5)

bind_rows(x1,x2,x3,x4,x5) -> aa

write_csv(aa, "output/80_10/fed_ifq_80_10.csv")


# sequential abc
gc()
gc()
gc()

x <- tac.fed.ifq(abc.fed.seq)
x <- sim.season.p_holder(x)

x1 <- x[1:2000]
x2 <- x[2001:4000]
x3 <- x[4001:6000]
x4 <- x[6001:8000]
x5 <- x[8001:9102]

x1 <- f.simulation.ifq(x1); gc()
x2 <- f.simulation.ifq(x2); gc()
x3 <- f.simulation.ifq(x3); gc()
x4 <- f.simulation.ifq(x4); gc()
x5 <- f.simulation.ifq(x5); gc()

gc()
gc()

aa <- bind_rows(x1, x2, x3, x4, x5) %>% mutate(rep = 1)

gc()
gc()
gc()
write_csv(aa, "output/80_10_abc_seq/fed_ifq_80_10_seq.csv")

# to be run later
x2 <- f.simulation.ifq(x) %>% mutate(rep = 2)
x3 <- f.simulation.ifq(x) %>% mutate(rep = 3)
x4 <- f.simulation.ifq(x) %>% mutate(rep = 4)
x5 <- f.simulation.ifq(x) %>% mutate(rep = 5)
