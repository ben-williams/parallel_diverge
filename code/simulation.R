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
    arrange(season, port, p_fshy) %>% ungroup%>% data.frame()
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

f.port_allocation <- function(x){
  all.boat %>% 
    dplyr::select(-p_fshy) %>% 
    left_join(pol %>% 
                group_by(p_fshy, port, season, p_holder, year) %>% 
                mutate(t = sum(ton) / n(),
                       yday = min(startdt)) %>% 
                group_by(p_fshy, season, port, p_holder) %>% 
                mutate(tt = sum(t)) %>% 
                filter(tt>1, p_fshy>1) %>%
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
           catch = if_else(p_fshy==1, 
                           rlnormTrunc(1,log(t), log(sd.t), 0, 140),
                           rtruncnorm(1, 0, 140, t, sd.t)),
           
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
           trip = rtruncnorm(1,0,7,day,sd.day),
           catch = if_else(p_fshy==1, 
                           rlnormTrunc(1,log(t), log(sd.t), 0, 140),
                           rtruncnorm(1, 0, 140, t, sd.t)),
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
