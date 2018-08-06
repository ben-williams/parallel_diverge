# Functions to perform the simulations

# base simulation function
# 3 parts
# 1. initial fishing period (f.sim)
# 2. search function to determine next fishing location (f.search_all)
# 3. fishing event (f.sim2)
# repeat 2 and 3 until stopped by a control (season ends, quota caught, etc.)
f.sim <- function(x){
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob), # is the vessel fishing today?
           trip = if_else(go==1, rtruncnorm(1,0,7,day,sd.day), 0), # if fishing trip duration
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140), # if fishing catch
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           
           n = if(trip==0) {1} else {trip}, # add up days fished
           c1 = if(area==1) {catch} else {0}, # which area did the catch come from
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0}) %>%
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day, 
                  sd.day, C1, C2, C3, prob, n, c1, c2, c3, trip, catch) 
}
f.sim2 <- function(x){
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1,0,7,day,sd.day), 0),
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140),
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           n = if_else(trip==0, n + 1, trip + n), # this is the only different code from f.sim
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0}) %>%
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day,
                  sd.day, C1, C2, C3, prob, n, c1, c2, c3, trip, catch)
}
fun.reps <- function(x){
  
  f.search_all <- function(x){
    # gridsearch to maximize revenue based upon anticipated returns
    s = if(c1<C1 && c2<C2 && c3<C3 && x[17]<x[7]){
      f.a123(x)
    } else if(c1<C1 && c2<C2 && c3>=C3 && x[17]<x[7]){
      f.a12(x)
    } else if(c1<C1 && c2>=C2 && c3>=C3 && x[17]<x[7]){
      f.a1(x)
    } else if(c1>=C1 && c2<C2 && c3>=C3 && x[17]<x[7]){
      f.a2(x)
    } else if(c1>=C1 && c2>=C2 && c3<C3 && x[17]<x[7]){
      f.a3(x)
    } else if(c1<C1 && c2>=C2 && c3<C3 && x[17]<x[7]){
      f.a13(x)
    } else if(c1>=C1 && c2<C2 && c3<C3 && x[17]<x[7]){
      f.a23(x)
    } else if(x[17]>=x[7]){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  l = replicate(nrow(x), vector('list',83)) # storage list
  x = f.sim(x) # trip function 
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1, i]] = x[i,]
  }
  
  # basic control levels - set catch targets low so that they aren't constantly overshot 
  C1 = as.numeric(x[1,13]) * 0.90  # limit value
  C2 = as.numeric(x[1,14]) * 0.90  # limit value
  C3 = as.numeric(x[1,15]) * 0.70  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_all))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s, y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]] = x[i,]
    }
    c1 = sum(x[,18]) + c1 # control value update
    c2 = sum(x[,19]) + c2 # control value update
    c3 = sum(x[,20]) + c3 # control value update
    y = x[,5:17] # store descriptor values
    
    s = as.data.frame(t(apply(x, 1, f.search_all))) # gridsearch
    x = bind_cols(s, y)
    if(x[1]==9) break
  }
  l
}
f.simulation <- function(x){
  # run simultaion, then cleanup
  x = lapply(x, fun.reps)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  filter(x, size<9)
}

# ECS ----
# set up the same general simulation design for ECS
# need to also add an individual catch quota (CATCH) that is based upon the tac function
# equal ctach shares - state waters - small vessels
f.sim.ecs <- function(x){
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1,0,7,day,sd.day), 0),
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140),
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           
           n = if(trip==0) {1} else {trip},
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0}) %>%
    mutate(c1 = if_else(c1>=C1, C1, c1),
           c2 = if_else(c2>=C2, C2, c2),
           c3 = if_else(c3>=C3, C3, c3)) %>% 
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day, 
                  sd.day, C1, C2, C3, prob, n, CATCH, c1, c2, c3, trip, catch) 
}
f.sim2.ecs <- function(x){
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1,0,7,day,sd.day), 0),
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140),
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           n = if_else(trip==0, n + 1, trip + n),
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0}) %>%
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day,
                  sd.day, C1, C2, C3, prob, n, CATCH, c1, c2, c3, trip, catch)
}
# equal ctach shares - state waters - small vessels
fun.reps.ecs <- function(x){
  
  f.search_all <- function(x){
    s = if(c1<C1 && c2<C2 && c3<C3 && x[17]<x[7] && catch<CATCH){
      f.a123(x)
    } else if(c1<C1 && c2<C2 && c3>=C3 && x[17]<x[7] && catch<CATCH){
      f.a12(x)
    } else if(c1<C1 && c2>=C2 && c3>=C3 && x[17]<x[7] && catch<CATCH){
      f.a1(x)
    } else if(c1>=C1 && c2<C2 && c3>=C3 && x[17]<x[7] && catch<CATCH){
      f.a2(x)
    } else if(c1>=C1 && c2>=C2 && c3<C3 && x[17]<x[7] && catch<CATCH){
      f.a3(x)
    } else if(c1<C1 && c2>=C2 && c3<C3 && x[17]<x[7] && catch<CATCH){
      f.a13(x)
    } else if(c1>=C1 && c2<C2 && c3<C3 && x[17]<x[7] && catch<CATCH){
      f.a23(x)
    } else if(x[17]>=x[7] || catch>=CATCH){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  
  l = replicate(nrow(x),vector('list', 83)) # storage list
  x = f.sim.ecs(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - reduce levels so don't "overfish"
  # need to add the CATCH control for ECS
  C1 = if(x[1,13]<100) {x[1,13] * 0.75} else {x[1,13] * 0.95}   # limit value
  C2 = if(x[1,14]<100) {x[1,14] * 0.75} else {x[1,14] * 0.95}  # limit value
  C3 = if(x[1,15]<100) {x[1,15] * 0.75} else {x[1,15] * 0.95}  # limit value
  CATCH = if(x[1,18]<100) {x[1,18] * 0.65} else {x[1,18] * 0.75}
  
  c1 = sum(x[,19]) # control value
  c2 = sum(x[,20]) # control value
  c3 = sum(x[,21]) # control value
  catch = sum(x[,23])
  
  y = x[,5:18] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_all))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s, y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2.ecs(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]] = x[i,]
    }
    c1 = sum(x[,19]) + c1 # control value update
    c2 = sum(x[,20]) + c2 # control value update
    c3 = sum(x[,21]) + c3 # control value update
    y = x[,5:18] # store descriptor values
    
    s = as.data.frame(t(apply(x, 1, f.search_all))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  l
}
f.simulation.ecs <- function(x){
  x = lapply(x, fun.reps.ecs)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'CATCH', 'c1', 'c2', 'c3', 'trip', 'catch')
  filter(x, size<9)
}

# superX ----
# super-exclusive - state waters - small vessels
fun.reps.sx <- function(x){
  
  # update the search function for superexclusive 
  f.search_sx <- function(x){
    s = if(x[2]==1 && c1<C1 && x[17]<x[7]){
      f.ase(x)
    } else if(x[2]==2 && c2<C2 && x[17]<x[7]){
      f.ase(x)
    } else if(x[2]==3 && c3<C3 && x[17]<x[7]){
      f.ase(x)
    } else if(x[17]>=x[7]){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  l = replicate(nrow(x), vector('list', 83)) # storage list
  x = f.sim(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  C1 = as.numeric(x[1,13]) * 0.90  # limit value
  C2 = as.numeric(x[1,14]) * 0.90  # limit value
  C3 = as.numeric(x[1,15]) * 0.70  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_sx))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s,y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]] = x[i,]
    }
    c1 = sum(x[,18]) + c1 # control value update
    c2 = sum(x[,19]) + c2 # control value update
    c3 = sum(x[,20]) + c3 # control value update
    y = x[,5:17] # store descriptor values
    
    s = as.data.frame(t(apply(x, 1, f.search_sx))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  l
}
f.simulation.sx <- function(x){
  x = lapply(x, fun.reps.sx)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  filter(x, size<9)
}

# PSC ----
# prohibited species catch (salmon)
f.sim.psc <- function(x){
  # add salmon catch (s1, s2, s3)
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1, 0, 7, day, sd.day), 0),
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140),
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           salmon = ifelse(area==3, 0.32 * catch, 0.66 * catch),
           n = if(trip==0) {1} else {trip},
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0},
           s1 = if(area==1) {salmon} else {0},
           s2 = if(area==2) {salmon} else {0},
           s3 = if(area==3) {salmon} else {0}) %>%
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day, 
                  sd.day, C1, C2, C3, S1, S2, S3, prob, n, c1, c2, c3, s1, s2, s3, trip, catch) 
}
f.sim2.psc <- function(x){
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1, 0, 7, day, sd.day), 0),
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140),
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           salmon = ifelse(area==3, 0.32 * catch, 0.66 * catch),
           n = if_else(trip==0, n + 1, trip + n),
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0},
           s1 = if(area==1) {salmon} else {0},
           s2 = if(area==2) {salmon} else {0},
           s3 = if(area==3) {salmon} else {0}) %>%
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day,
                  sd.day, C1, C2, C3, S1, S2, S3, prob, n, c1, c2, c3,  s1, s2, s3, trip, catch)
}
fun.reps.psc <- function(x){
  
  f.search_psc <- function(x){
    # add salmon catch to control rules
    
    s = if(c1<C1 && c2<C2 && c3<C3 && s1<S1 && s2< S2 && s3<S3 && x[20]<x[7]){
      f.a123(x)
    } else if(c1< C1 && c2< C2 && c3>=C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3< S3 ||
              c1< C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3>=S3){
      f.a12(x)
    } else if(c1< C1 && c2>=C2 && c3>=C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3< S3 || 
              c1< C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2>=S2 && s3>=S3 ||
              c1< C1 && c2< C2 && c3>=C3 && x[20]<x[7] && s1< S1 && s2>=S2 && s3< S3 ||
              c1< C1 && c2>=C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3>=S3){
      f.a1(x)
    } else if(c1>=C1 && c2>=C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3< S3 || 
              c1< C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1>=S1 && s2>=S2 && s3< S3 ||
              c1>=C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2>=S2 && s3< S3 ||
              c1< C1 && c2>=C2 && c3< C3 && x[20]<x[7] && s1>=S1 && s2< S2 && s3< S3){
      f.a2(x)
    } else if(c1>=C1 && c2>=C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3< S3 || 
              c1< C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1>=S1 && s2>=S2 && s3< S3 ||
              c1>=C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2>=S2 && s3< S3 ||
              c1< C1 && c2>=C2 && c3< C3 && x[20]<x[7] && s1>=S1 && s2< S2 && s3< S3){
      f.a3(x)
    } else if(c1< C1 && c2>=C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3< S3 || 
              c1< C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2>=S2 && s3< S3){
      f.a13(x)
    } else if(c1>=C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1< S1 && s2< S2 && s3< S3 || 
              c1< C1 && c2< C2 && c3< C3 && x[20]<x[7] && s1>=S1 && s2< S2 && s3< S3){
      f.a23(x)
    } else if(x[20]>=x[7]){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
 
  l = replicate(nrow(x), vector('list', 83)) # storage list
  x = f.sim.psc(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - work for 
  C1 = as.numeric(x[1,13]) * 0.90  # limit value
  C2 = as.numeric(x[1,14]) * 0.90  # limit value
  C3 = as.numeric(x[1,15]) * 0.70  # limit value
  # salmon controls
  S1 = as.numeric(x[1,16]) * 0.90  # limit value
  S2 = as.numeric(x[1,17]) * 0.90  # limit value
  S3 = as.numeric(x[1,18]) * 0.90  # limit value
  
  c1 = sum(x[,21]) # control value
  c2 = sum(x[,22]) # control value
  c3 = sum(x[,23]) # control value
  s1 = sum(x[,24]) # control value
  s2 = sum(x[,25]) # control value
  s3 = sum(x[,26]) # control value
  
  y = x[,5:20] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_psc))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s,y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2.psc(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]] = x[i,]
    }
    c1 = sum(x[,21]) + c1 # control value update
    c2 = sum(x[,22]) + c2 # control value update
    c3 = sum(x[,23]) + c3 # control value update
    s1 = sum(x[,24]) + s1 # control value update
    s2 = sum(x[,25]) + s2 # control value update
    s3 = sum(x[,26]) + s3 # control value update
    y = x[,5:20] # store descriptor values
    
    s = as.data.frame(t(apply(x, 1, f.search_psc))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  l
}
f.simulation.psc <- function(x){
  x = lapply(x, fun.reps.psc)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3', 'S1', 'S2', 'S3', 'prob', 'n', 'c1', 'c2', 'c3', 's1', 's2', 's3','trip', 'catch')
  filter(x, size<9)
}

# IFQ ----
# ifq based simulations
f.sim.ifq <- function(x){
  # same as f.sim - just adds a control to fish to the TAC if initial catch exceeds 
  # to available catch
  x %>%
    rowwise %>% 
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1, 0, 7, day, sd.day), 0),
           catch = if_else(trip==0, 0, 
                           ifelse(p_fshy==1, 
                                  rlnormTrunc(1, log(t), log(sd.t), 0, 140),
                                  rtruncnorm(1, 0, 140, t, sd.t))),
           
           n = if(trip==0) {1} else {trip},
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0}) %>%
    mutate(c1 = if_else(c1>=C1, C1, c1),
           c2 = if_else(c2>=C2, C2, c2),
           c3 = if_else(c3>=C3, C3, c3)) %>% 
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day, 
                  sd.day, C1, C2, C3, prob, n, c1, c2, c3, trip, catch) 
}
fun.reps.ifq <- function(x){
  # search functions were set to return to the same port they departed
  f.search_all <- function(x){
    s = if(c1<C1 && c2<C2 && c3<C3 && x[17]<x[7] ){
      f.a123pd(x)
    } else if(c1<C1 && c2<C2 && c3>=C3 && x[17]<x[7] ){
      f.a12pd(x)
    } else if(c1<C1 && c2>=C2 && c3>=C3 && x[17]<x[7] ){
      f.a1pd(x)
    } else if(c1>=C1 && c2<C2 && c3>=C3 && x[17]<x[7] ){
      f.a2pd(x)
    } else if(c1>=C1 && c2>=C2 && c3<C3 && x[17]<x[7] ){
      f.a3pd(x)
    } else if(c1<C1 && c2>=C2 && c3<C3 && x[17]<x[7] ){
      f.a13pd(x)
    } else if(c1>=C1 && c2<C2 && c3<C3 && x[17]<x[7] ){
      f.a23pd(x)
    } else if(x[17]>=x[7] ){
      c(9, 1, 1, 1)
    } else{c(9, 1, 1, 1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  
  l = replicate(nrow(x), vector('list', 83)) # storage list
  x = f.sim.ifq(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - reduce levels so don't "overfish"
  C1 = if(x[1,13]<100) {x[1,13] * 0.75} else {x[1,13] * 0.95}   # limit value
  C2 = if(x[1,14]<100) {x[1,14] * 0.75} else {x[1,14] * 0.95}  # limit value
  C3 = if(x[1,15]<100) {x[1,15] * 0.75} else {x[1,15] * 0.95}  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_all))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s,y) # create a whole dataset
  
  for(j in 2:83){
    x = f.sim2(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]]=x[i,]
    }
    c1 = sum(x[,18]) + c1 # control value update
    c2 = sum(x[,19]) + c2 # control value update
    c3 = sum(x[,20]) + c3 # control value update
    y = x[,5:17] # store descriptor values
    
    s = as.data.frame(t(apply(x,1,f.search_all))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  
  l
}
f.simulation.ifq <- function(x){
  x = lapply(x, fun.reps.ifq)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  x = filter(x, size<9)
  x
}


# Community Quota ----
fun.reps.cq <- function(x){
  # set grid search routine for CQ
  f.search_all <- function(x){
    s = if(c1<C1 && c2<C2 && c3<C3 && x[17]<x[7]){
      f.a123(x)
    } else if(c1<C1 && c2<C2 && c3>=C3 && x[17]<x[7]){
      f.a12(x)
    } else if(c1<C1 && c2>=C2 && c3>=C3 && x[17]<x[7]){
      f.a1(x)
    } else if(c1>=C1 && c2<C2 && c3>=C3 && x[17]<x[7]){
      f.a2(x)
    } else if(c1>=C1 && c2>=C2 && c3<C3 && x[17]<x[7]){
      f.a3(x)
    } else if(c1<C1 && c2>=C2 && c3<C3 && x[17]<x[7]){
      f.a13(x)
    } else if(c1>=C1 && c2<C2 && c3<C3 && x[17]<x[7]){
      f.a23(x)
    } else if(x[17]>=x[7]){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  l = replicate(nrow(x), vector('list',83)) # storage list
  x = f.sim(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1, i]] = x[i,]
  }
  
  # basic control levels - work for 
  C1 = as.numeric(x[1,13]) * 0.90  # limit value
  C2 = as.numeric(x[1,14]) * 0.90  # limit value
  C3 = as.numeric(x[1,15]) * 0.45  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_all))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s, y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]] = x[i,]
    }
    c1 = sum(x[,18]) + c1 # control value update
    c2 = sum(x[,19]) + c2 # control value update
    c3 = sum(x[,20]) + c3 # control value update
    y = x[,5:17] # store descriptor values
    
    s = as.data.frame(t(apply(x, 1, f.search_all))) # gridsearch
    x = bind_cols(s, y)
    if(x[1]==9) break
  }
  l
}
f.simulation.cq <- function(x){
  x = lapply(x, fun.reps.cq)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  filter(x, size<9)
}

# Cooperatives ----
fun.reps.coop <- function(x){
  # set cooperative seacrhc routine
  f.search_all <- function(x){
    s = if(c1<C1 && c2<C2 && c3<C3 && x[17]<x[7]){
      f.a123(x)
    } else if(c1<C1 && c2<C2 && c3>=C3 && x[17]<x[7]){
      f.a12(x)
    } else if(c1<C1 && c2>=C2 && c3>=C3 && x[17]<x[7]){
      f.a1(x)
    } else if(c1>=C1 && c2<C2 && c3>=C3 && x[17]<x[7]){
      f.a2(x)
    } else if(c1>=C1 && c2>=C2 && c3<C3 && x[17]<x[7]){
      f.a3(x)
    } else if(c1<C1 && c2>=C2 && c3<C3 && x[17]<x[7]){
      f.a13(x)
    } else if(c1>=C1 && c2<C2 && c3<C3 && x[17]<x[7]){
      f.a23(x)
    } else if(x[17]>=x[7]){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  l = replicate(nrow(x), vector('list',83)) # storage list
  x = f.sim(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1, i]] = x[i,]
  }
  
  # basic control levels - work for 
  C1 = as.numeric(x[1,13]) * 0.70  # limit value
  C2 = as.numeric(x[1,14]) * 0.80  # limit value
  C3 = as.numeric(x[1,15]) * 0.7  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x, 1, f.search_all))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s, y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]] = x[i,]
    }
    c1 = sum(x[,18]) + c1 # control value update
    c2 = sum(x[,19]) + c2 # control value update
    c3 = sum(x[,20]) + c3 # control value update
    y = x[,5:17] # store descriptor values
    
    s = as.data.frame(t(apply(x, 1, f.search_all))) # gridsearch
    x = bind_cols(s, y)
    if(x[1]==9) break
  }
  l
}
f.simulation.coop <- function(x){
  x = lapply(x, fun.reps.coop)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  filter(x, size<9)
}
