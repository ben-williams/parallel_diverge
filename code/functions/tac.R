# tac levels by state, fed, port, ecs, ifq
tac.state <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc.state, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) -> tac
  
  
  tac %>% 
    left_join(f.boats_small()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(sim >20, p_fshy==1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob)  
}  
tac.fed <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc.fed, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) -> tac
  
  
  tac %>% 
    left_join(f.boats_large()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(sim >20, p_fshy>1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.all <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) -> tac
  
  
  tac %>% 
    left_join(f.boats()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(sim >20) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob)  
}  
tac.port.all <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) %>% 
    left_join(f.port_allocation()) %>% 
    mutate(C1 = ifelse(area==1, C1 * perc_catch, NA),
           C2 = ifelse(area==2, (C2 * perc_catch), NA),
           C3 = ifelse(area==3, (C3 * perc_catch), NA)) %>% 
    dplyr::select(-perc_catch, -area) %>% 
    ungroup -> tac
  
  tac %>% 
    filter(!is.na(C1))  %>% 
    mutate(C2 = filter(tac, !is.na(C2))$C2,
           C3 = filter(tac, !is.na(C3))$C3) %>% 
    left_join(f.boats()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(sim >20) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.port.fed <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc.fed, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) %>% 
    left_join(f.port_allocation()) %>% 
    mutate(C1 = ifelse(area==1, C1 * perc_catch, NA),
           C2 = ifelse(area==2, (C2 * perc_catch), NA),
           C3 = ifelse(area==3, (C3 * perc_catch), NA)) %>% 
    dplyr::select(-perc_catch, -area) %>% 
    ungroup -> tac
  
  tac %>% 
    filter(!is.na(C1), p_fshy>1)  %>% 
    mutate(C2 = filter(tac, !is.na(C2))$C2,
           C3 = filter(tac, !is.na(C3))$C3) %>% 
    left_join(f.boats_large()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(sim >20) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.baseline <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) -> tac
  
  
  tac %>% 
    left_join(f.boats()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(sim <21) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob)  
}  
tac.state.ecs <- function(){
  tac.state() %>% 
  group_by(season, sim, port) %>% 
  mutate(n = n(), C1 = C1/n,  C2 = C2/n, C3 = C3/n) %>% 
  dplyr::select(-n)
}
tac.state.sx <- function(){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(abc.state, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, 0),
           C2 = ifelse(area==2, tac, 0),
           C3 = ifelse(area==3, tac, 0)) %>% 
    dplyr::select(-app, -abc, -tac)   %>%  
    left_join(f.boats_small_sx()) %>%
    left_join(f.trip_behavior_sx()) %>%  
    filter(sim >20, p_fshy==1) %>% 
    mutate(deli = port) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}

# break into appropriate lists for simulations
sim.season <- function(x) { 
  x <- split(x, c(x$sim)) 
  x <- sapply(x, function(x) split(x, x$season))
  x
}
sim.season.port <- function(x){
  split(x, interaction(x$port, x$season, x$sim), drop = TRUE)
}
sim.season.p_holder <- function(x){
  split(x, interaction(x$season, x$sim, x$p_holder), drop = TRUE)
}

