f.boats <- function(){
  pol %>% 
    group_by(p_fshy, port, season, p_holder, year) %>% 
    mutate(t = sum(ton) / n(),
           yday = min(startdt)) %>% 
    group_by(p_fshy, season, port, p_holder) %>% 
    mutate(tt = sum(t)) %>% 
    filter(tt>1) %>%
    ungroup %>% 
    group_by(p_fshy, season, p_holder) %>% 
    summarise(port = round(mean(port))) %>% 
    arrange(season, port, p_fshy) 
}
f.boats_small <- function(){
  pol %>% 
    group_by(p_fshy, port, season, p_holder, year) %>% 
    mutate(t = sum(ton) / n(),
           yday = min(startdt)) %>% 
    group_by(p_fshy, season, port, p_holder) %>% 
    mutate(tt = sum(t)) %>% 
    filter(tt>1, p_fshy==1) %>%
    ungroup %>% 
    group_by(p_fshy, season, p_holder) %>% 
    summarise(port = round(mean(port))) %>% 
    arrange(season, port, p_fshy) 
}
f.boats_small_sx <- function(){
  pol %>% 
    group_by(p_fshy, port, season, p_holder, year) %>% 
    mutate(t = sum(ton) / n(),
           yday = min(startdt)) %>% 
    group_by(p_fshy, season, port, p_holder) %>% 
    mutate(tt = sum(t)) %>% 
    filter(tt>1, p_fshy==1) %>%
    ungroup %>% 
    group_by(p_fshy, season, p_holder) %>% 
    summarise(port = round(mean(port)), area = round(mean(area))) %>% 
    arrange(season, port, p_fshy) 
}
f.boats_large <- function(){
  pol %>% 
    group_by(p_fshy, port, season, p_holder, year) %>% 
    mutate(t = sum(ton) / n(),
           yday = min(startdt)) %>% 
    group_by(p_fshy, season, port, p_holder) %>% 
    mutate(tt = sum(t)) %>% 
    filter(tt>1, p_fshy>1) %>%
    ungroup %>% 
    group_by(p_fshy, season, p_holder) %>% 
    summarise(port = round(mean(port))) %>% 
    arrange(season, port, p_fshy)  
}


f.trip_behavior <- function(){
  pol %>% 
    filter(p_holder %in% unique(f.boats()$p_holder)) %>% 
    group_by(p_holder, season, year, p_fshy) %>% 
    summarise(t = sum(ton) / n(), # ton of catch
              sd.t = sd(ton / n()), 
              trips = n(), # number of trips
              day = round(as.numeric(mean(time))) + 1, 
              sd.day = as.numeric(sd(time)), 
              tday = mean(days),
              prob = trips * day / tday) %>% 
    group_by(season, p_holder, p_fshy) %>% 
    summarise(t = mean(t), 
              sd.t = max(sd.t), 
              day = mean(day),
              trips=round(mean(trips)), # number of trips
              sd.day = mean(sd.day),
              tday = mean(tday),
              prob = mean(prob)) %>% 
    mutate(nat = mean(t, na.rm = TRUE),
           nasdt = mean(sd.t, na.rm = TRUE),
           naday = mean(day, na.rm = TRUE),
           natrips = mean(trips, na.rm = TRUE),
           nasdday = mean(sd.day, na.rm = TRUE)) %>% 
    mutate(t = ifelse(is.na(t), nat, t),
           sd.t = if_else(p_fshy==1, 3, 10),
           day = ifelse(is.na(day), naday, day),
           trips = ifelse(is.na(trips), natrips, trips),
           sd.day = ifelse(is.na(sd.day), nasdday, sd.day),
           sd.day = if_else(is.na(sd.day) | sd.day<1, 1.01,sd.day),
           tday = case_when(season == 1 ~ 51,
                            season == 2 ~ 83,
                            season == 3 ~ 38,
                            season == 4 ~ 32),
           prob = ifelse(is.na(prob), 0, if_else(prob>1, 1, prob))) %>% 
    dplyr::select(-c(nat, nasdt, naday, natrips, nasdday, trips)) 
}
f.trip_behavior_sx <- function(){
  # fleet behavior for small vessels only - 
  # each vessel is assigned a port/area by season dependent upon the 
  # max number of deliveries for each season/port/area
  pol %>% 
    filter(p_holder %in% unique(f.boats_small_sx()$p_holder)) %>% 
    group_by(p_holder, season, year, p_fshy) %>% 
    summarise(t = sum(ton) / n(), # ton of catch
              sd.t = sd(ton / n()), 
              trips = n(), # number of trips
              day = round(as.numeric(mean(time))) + 1, 
              sd.day = as.numeric(sd(time)), 
              tday = mean(days),
              prob = trips * day / tday) %>% 
    group_by(season, p_holder, p_fshy) %>% 
    summarise(t = mean(t), 
              sd.t = max(sd.t), 
              day = mean(day),
              trips=round(mean(trips)), # number of trips
              sd.day = mean(sd.day),
              tday = mean(tday),
              prob = mean(prob)) %>% 
    mutate(nat = mean(t, na.rm = TRUE),
           nasdt = mean(sd.t, na.rm = TRUE),
           naday = mean(day, na.rm = TRUE),
           natrips = mean(trips, na.rm = TRUE),
           nasdday = mean(sd.day, na.rm = TRUE)) %>% 
    mutate(t = ifelse(is.na(t), nat, t),
           sd.t = if_else(p_fshy==1, 3, 10),
           day = ifelse(is.na(day), naday, day),
           trips = ifelse(is.na(trips), natrips, trips),
           sd.day = ifelse(is.na(sd.day), nasdday, sd.day),
           sd.day = if_else(is.na(sd.day) | sd.day<1, 1.01,sd.day),
           tday = case_when(season == 1 ~ 51,
                            season == 2 ~ 83,
                            season == 3 ~ 38,
                            season == 4 ~ 32),
           prob = ifelse(is.na(prob), 0, if_else(prob>1, 1, prob))) %>% 
    dplyr::select(-c(nat, nasdt, naday, natrips, nasdday, trips)) 
}
f.trip_behavior_ifq <- function(){
  # determine who gets to fish & how much
  pol %>% 
    filter(p_holder %in% unique(f.boats_large()$p_holder)) %>% 
    group_by(port, p_holder, season, area, year) %>% 
    summarise(t = max(ton)) %>% 
    filter(t>100) %>% 
    dplyr::select(-year) %>% 
    ungroup() %>% 
    group_by(p_holder, season, area) %>% 
    summarise(t = mean(t)) %>% 
    group_by(season, area) %>% 
    mutate(tt = sum(t), perc = t/tt) -> allocation 
  
  p_holders <- unique(allocation$p_holder) # vector of vessels that have IFQ
  
  pol %>% 
    filter(p_holder %in% p_holders) %>% 
    dplyr::select(p_fshy, p_holder) %>% 
    distinct() %>% 
    left_join(allocation) %>% 
    arrange(p_holder) -> allocation
  
  # combine the vessels with TAC allocation and simulated TAC
  Catch %>% 
    mutate(area=a) %>% 
    left_join(allocation) %>% 
    dplyr::select(a, season, sim, iFed, perc, p_holder, area, p_fshy) %>% 
    mutate(tac = iFed * perc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-tac, -perc, -a) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    arrange(sim, p_holder) -> aa
  
  pol %>% 
    filter(p_fshy>1) %>% 
    left_join(allocation) %>% 
    filter(p_holder %in% p_holders) %>% 
    group_by(p_fshy, port, season, area) %>% 
    summarise(t = sum(ton)/n(), 
              sd.t = sd(ton/n()) , # add some variance to the trip catch
              day = round(as.numeric(mean(time))) + 1, 
              # add a to account for 1 day trips
              sd.day = as.numeric(sd(time, na.rm=T)) , 
              tday = mean(days)) %>% # number of days in the season
    left_join(aa) %>% 
    filter(!is.na(sim)) %>% 
    ungroup() %>% 
    mutate(prob=1, 
           deli=port,
           id = 1:n(), 
           sd.day = ifelse(sd.day<=1 | is.na(sd.day), 1.1, sd.day),
           sd.t = ifelse(sd.t<=1 | is.na(sd.t), 15, sd.t)) %>% 
    # if have ifq probability of fishing is 1 
    dplyr::select(p_fshy, area, port, deli, season, days=tday, 
                  prob, t, sd.t,day, sd.day, sim, abc=p_holder, 
                  C1, C2, C3, id) -> x
  # change abc to p_holder simply to fit the data structure
  
  x = split(x, c(x$id))
  x
}
f.port_allocation <- function(x){
  all.boat %>% 
    dplyr::select(-p_fshy) %>% 
    left_join(pol %>% 
                group_by(p_fshy, port, season, p_holder, year) %>% 
                mutate(t = sum(ton) / n(),
                       yday = min(startdt)) %>% 
                group_by(p_fshy, season, port, p_holder) %>% 
                mutate(tt = sum(t)) %>% 
                filter(tt>1) %>%
                group_by(season, port, year, area) %>% 
                summarise(catch = sum(t)) %>% 
                group_by(season, year, area) %>% 
                mutate(scatch = sum(catch)) %>% 
                group_by(season, port, year, area) %>% 
                mutate(perc_catch = catch/scatch) %>% 
                group_by(season, port, area) %>% 
                summarise(perc_catch = mean(perc_catch)) %>% 
                data.frame()) %>% 
    mutate(perc_catch = if_else(is.na(perc_catch), 0, perc_catch)) %>% 
    group_by(port, season, area) %>% 
    summarise(perc_catch = mean(perc_catch)) %>% 
    ungroup
}

f.sim <- function(x){
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
           n = if_else(trip==0, n + 1, trip + n),
           c1 = if(area==1) {catch} else {0},
           c2 = if(area==2) {catch} else {0},
           c3 = if(area==3) {catch} else {0}) %>%
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day,
                  sd.day, C1, C2, C3, prob, n, c1, c2, c3, trip, catch)
}
fun.reps <- function(x){
  
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
  
  
  l = replicate(nrow(x),vector('list',83)) # storage list
  x = f.sim(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - work for 
  C1 = as.numeric(x[1,13]) * 0.90  # limit value
  C2 = as.numeric(x[1,14]) * 0.90  # limit value
  C3 = as.numeric(x[1,15]) * 0.70  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x,1,f.search_all))) # function to run a grid search based upon the limit and controls
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
f.itall <- function(x){
  x = lapply(x, fun.reps)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  x = filter(x, size<9)
  x
}

# ECS ----
# equal ctach shares - state waters - small vessels
fun.reps.ecs <- function(x){
  
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
  
  
  l = replicate(nrow(x),vector('list',83)) # storage list
  x = f.sim(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - reduce levels so don't "overfish"
  C1 = if(x[1,14]<100) {x[1,13] * 0.65} else {x[1,13] * 0.95}   # limit value
  C2 = if(x[1,15]<100) {x[1,14] * 0.85} else {x[1,14] * 0.95}  # limit value
  C3 = if(x[1,16]<100) {x[1,15] * 0.85} else {x[1,15] * 0.95}  # limit value
  
  c1 = sum(x[,18]) # control value
  c2 = sum(x[,19]) # control value
  c3 = sum(x[,20]) # control value
  
  y = x[,5:17] # store descriptor values
  s = as.data.frame(t(apply(x,1,f.search_all))) # function to run a grid search based upon the limit and controls
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
f.itall.ecs <- function(x){
  x = lapply(x, fun.reps.ecs)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  x = filter(x, size<9)
  x
}


# ECS2 ----
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
fun.reps.ecs2 <- function(x){
  
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
    } else if(x[17]>=x[7] || catch<CATCH){
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  
  l = replicate(nrow(x),vector('list',83)) # storage list
  x = f.sim.ecs(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - reduce levels so don't "overfish"
  C1 = if(x[1,14]<100) {x[1,13] * 0.75} else {x[1,13] * 0.95}   # limit value
  C2 = if(x[1,15]<100) {x[1,14] * 0.75} else {x[1,14] * 0.95}  # limit value
  C3 = if(x[1,16]<100) {x[1,15] * 0.75} else {x[1,15] * 0.95}  # limit value
  CATCH = if(x[1,18]<100) {x[1,18] * 0.65} else {x[1,18] * 0.75}
  
  c1 = sum(x[,19]) # control value
  c2 = sum(x[,20]) # control value
  c3 = sum(x[,21]) # control value
  catch = sum(x[,23])
  
  y = x[,5:18] # store descriptor values
  s = as.data.frame(t(apply(x,1,f.search_all))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s,y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2.ecs(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]]=x[i,]
    }
    c1 = sum(x[,19]) + c1 # control value update
    c2 = sum(x[,20]) + c2 # control value update
    c3 = sum(x[,21]) + c3 # control value update
    y = x[,5:18] # store descriptor values
    
    s = as.data.frame(t(apply(x,1,f.search_all))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  l
}
f.itall.ecs2 <- function(x){
  x = lapply(x, fun.reps.ecs2)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'CATCH', 'c1', 'c2', 'c3', 'trip', 'catch')
  x = filter(x, size<9)
  x
}

# superX ----
# equal ctach shares - state waters - small vessels
fun.reps.sx <- function(x){
  
  
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
  
  l = replicate(nrow(x),vector('list',83)) # storage list
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
  s = as.data.frame(t(apply(x,1,f.search_sx))) # function to run a grid search based upon the limit and controls
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
    
    s = as.data.frame(t(apply(x,1,f.search_sx))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  l
}
f.itall.sx <- function(x){
  x = lapply(x, fun.reps.sx)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  x = filter(x, size<9)
  x
}

# PSC ----
f.sim.psc <- function(x){
  x %>%
    rowwise() %>%
    mutate(go = rbinom(1, 1, prob),
           trip = if_else(go==1, rtruncnorm(1,0,7,day,sd.day), 0),
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
           trip = if_else(go==1, rtruncnorm(1,0,7,day,sd.day), 0),
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
  
  
  l = replicate(nrow(x),vector('list',83)) # storage list
  x = f.sim.psc(x) # function (uses dplyr code to calculate a suite of variables)
  
  # add data to storage list
  for(i in 1:nrow(x)){
    l[[1,i]] = x[i,]
  }
  
  # basic control levels - work for 
  C1 = as.numeric(x[1,13]) * 0.90  # limit value
  C2 = as.numeric(x[1,14]) * 0.90  # limit value
  C3 = as.numeric(x[1,15]) * 0.70  # limit value
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
  s = as.data.frame(t(apply(x,1,f.search_psc))) # function to run a grid search based upon the limit and controls
  x = bind_cols(s,y) # create a whole dataset
  
  
  for(j in 2:83){
    x = f.sim2.psc(x) # run the second trip function (same as 1st just updates # of days)
    
    # add data to  storagelist
    for(i in 1:nrow(x)){ 
      l[[j,i]]=x[i,]
    }
    c1 = sum(x[,21]) + c1 # control value update
    c2 = sum(x[,22]) + c2 # control value update
    c3 = sum(x[,23]) + c3 # control value update
    s1 = sum(x[,24]) + s1 # control value update
    s2 = sum(x[,25]) + s2 # control value update
    s3 = sum(x[,26]) + s3 # control value update
    y = x[,5:20] # store descriptor values
    
    s = as.data.frame(t(apply(x,1,f.search_psc))) # gridsearch
    x = bind_cols(s,y)
    if(x[1]==9) break
  }
  l
}
f.itall.psc <- function(x){
  x = lapply(x, fun.reps.psc)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3', 'S1', 'S2', 'S3', 'prob', 'n', 'c1', 'c2', 'c3', 's1', 's2', 's3','trip', 'catch')
  x = filter(x, size<9)
  x
}

# IFQ ----
f.sim.ifq <- function(x){
  x %>%
    rowwise %>% 
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
           c3 = if_else(c3>=C3, C3, c3), 
           homeport=port) %>% 
    dplyr::select(p_fshy, area, port, deli, p_holder, season, days, sim, t, sd.t, day, 
                  sd.day, C1, C2, C3, prob, n, c1, c2, c3, trip, catch) 
}


fun.reps.ifq <- function(x){
  
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
      c(9,1,1,1)
    } else{c(9,1,1,1)}
    names(s) <- c('p_fshy', 'area', 'port', 'deli')
    s
  }
  
  
  l = replicate(nrow(x),vector('list',83)) # storage list
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
  s = as.data.frame(t(apply(x,1,f.search_all))) # function to run a grid search based upon the limit and controls
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
f.itall.ifq <- function(x){
  x = lapply(x, fun.reps.ifq)
  x = lapply(x, f.docall)
  x = do.call(bind_rows,x)
  names(x) = c('size', 'area', 'p', 'd', 'p_holder', 'season','days','sim', 't', 'sd.t',
               'day','sd.day', 'C1','C2', 'C3','prob', 'n', 'c1', 'c2', 'c3', 'trip', 'catch')
  x = filter(x, size<9)
  x
}


