
# vessels ----
# based upon CFEC dataset
# filter vessels that have not actively participated
# defined as harvesting less than 1 t over the years of data available

# Vessels start from different ports in different years/seasons
# set the starting port for vessel size classes based upon their average starting port
# this is done for different vessel size classes as various scenarios need them split out
# 1 - small
# 2-4 = large
# all vessels
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
# small vessels only
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
# small vessels in super-exclusive scenarios
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
# large vessels
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

# behavior ----
# define vessel behavior by scenario type

# behavior is established for each individual permit holder
# calculate the annual values, then take the mean (or max for SD)
# fill in any blank values with the mean 
# set the first fishing area as either Area 1 or 3 
# (ports are in either area 1 (kodiak), or area 3 (all other ports))
# we assume that all vessels will start fishing closer to their port than further

# all vessels (parallel management)
f.trip_behavior <- function(){
  pol %>% 
    filter(p_holder %in% unique(f.boats()$p_holder)) %>% # keep only vessels with permits
    group_by(p_holder, season, year, p_fshy) %>% 
    summarise(t = sum(ton) / n(), # ton of catch
              sd.t = sd(ton / n()), 
              trips = n(), # number of trips
              day = round(as.numeric(mean(time))) + 1, # average trip duration
              sd.day = as.numeric(sd(time)), 
              tday = mean(days), # days of season vessel fished
              prob = trips * day / tday) %>%  # vessel participation
    group_by(season, p_holder, p_fshy) %>% 
    # take the average across all years
    summarise(t = mean(t), 
              sd.t = max(sd.t), 
              day = mean(day),
              trips = round(mean(trips)), # number of trips
              sd.day = mean(sd.day),
              tday = mean(tday),
              prob = mean(prob)) %>% 
    # fill in any missing data with the mean
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
           tday = case_when(season == 1 ~ 51, # set season lengths
                            season == 2 ~ 83,
                            season == 3 ~ 38,
                            season == 4 ~ 32),
           prob = ifelse(is.na(prob), 0, if_else(prob>1, 1, prob))) %>% 
    dplyr::select(-c(nat, nasdt, naday, natrips, nasdday, trips)) 
}

# these functions perform the same general tasks as above, though the 
# vessel inputs change (e.g., f.boats_small_sx() instead of f.boats())
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

# f.trip_behavior_ifq <- function(){
#   # determine who gets to fish & how much
#   pol %>% 
#     filter(p_holder %in% unique(f.boats_large()$p_holder)) %>% 
#     group_by(port, p_holder, season, area, year) %>% 
#     summarise(t = max(ton)) %>% 
#     filter(t>100) %>% 
#     dplyr::select(-year) %>% 
#     ungroup() %>% 
#     group_by(p_holder, season, area) %>% 
#     summarise(t = mean(t)) %>% 
#     group_by(season, area) %>% 
#     mutate(tt = sum(t), perc = t/tt) -> allocation 
#   
#   p_holders <- unique(allocation$p_holder) # vector of vessels that have IFQ
#   
#   pol %>% 
#     filter(p_holder %in% p_holders) %>% 
#     dplyr::select(p_fshy, p_holder) %>% 
#     distinct() %>% 
#     left_join(allocation) %>% 
#     arrange(p_holder) -> allocation
#   
#   # combine the vessels with TAC allocation and simulated TAC
#   Catch %>% 
#     mutate(area=a) %>% 
#     left_join(allocation) %>% 
#     dplyr::select(a, season, sim, iFed, perc, p_holder, area, p_fshy) %>% 
#     mutate(tac = iFed * perc,
#            C1 = ifelse(area==1, tac, NA),
#            C2 = ifelse(area==2, tac, NA),
#            C3 = ifelse(area==3, tac, NA)) %>% 
#     dplyr::select(-tac, -perc, -a) %>% 
#     mutate_all(funs(replace(., is.na(.), 0))) %>% 
#     arrange(sim, p_holder) -> aa
#   
#   pol %>% 
#     filter(p_fshy>1) %>% 
#     left_join(allocation) %>% 
#     filter(p_holder %in% p_holders) %>% 
#     group_by(p_fshy, port, season, area) %>% 
#     summarise(t = sum(ton)/n(), 
#               sd.t = sd(ton/n()) , # add some variance to the trip catch
#               day = round(as.numeric(mean(time))) + 1, 
#               # add a to account for 1 day trips
#               sd.day = as.numeric(sd(time, na.rm=T)) , 
#               tday = mean(days)) %>% # number of days in the season
#     left_join(aa) %>% 
#     filter(!is.na(sim)) %>% 
#     ungroup() %>% 
#     mutate(prob=1, 
#            deli=port,
#            id = 1:n(), 
#            sd.day = ifelse(sd.day<=1 | is.na(sd.day), 1.1, sd.day),
#            sd.t = ifelse(sd.t<=1 | is.na(sd.t), 15, sd.t)) %>% 
#     # if have ifq probability of fishing is 1 
#     dplyr::select(p_fshy, area, port, deli, season, days=tday, 
#                   prob, t, sd.t,day, sd.day, sim, abc=p_holder, 
#                   C1, C2, C3, id) -> x
#   # change abc to p_holder simply to fit the data structure
#   
#   x = split(x, c(x$id))
#   x
# }

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

# abc ----
# set ABC levels
# ABC is set at 200,000
abc <- data.frame(abc = rep(50000, 40), sim = 1:40)
abc.state <- abc %>% mutate(abc = abc * 0.25)
abc.fed <- abc %>% mutate(abc = abc * 0.75)

# ABC is set at equally spaced range
abc.seq <- data.frame(abc =  seq(20000, 240000, by = 6000), sim = 1:37)
abc.state.seq <- abc.seq %>% mutate(abc = abc * 0.25)
abc.fed.seq <- abc.seq %>% mutate(abc = abc * 0.75)

# salmon ----
# salmon harvest quota
salmon.fed <- data.frame(salmon = 25000, sim = 1:40)

# tac ----
# TAC levels by state, fed, port, ecs, ifq, etc.
tac.state <- function(x){
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3, # federal fishing area
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1, # seasons
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac)  -> temp
  
  temp %>% 
    filter(C1>0)  %>% 
    mutate(C2 = filter(temp, C2>0)$C2,
           C3 = filter(temp, C3>0)$C3) -> tac
  
  # check to make sure everything looks ok
  tac %>% 
    left_join(f.boats_small()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(p_fshy==1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob)  
}  
tac.fed <- function(x){
  # assign tac for federal waters fishery
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
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
    filter(p_fshy>1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.fed.ifq <- function(x){
  # assign tac for federal IFQ fishery
  # determine who gets to fish & how much
  # vessels are filtered based upon having caught at least 100 t in a given year/season/area
  # allocation is based upon average catch over time by season and area
  pol %>%   
    filter(p_fshy!=1) %>% 
    group_by(port, p_holder, season, area, year) %>% 
    summarise(t = max(ton)) %>% 
    filter(t>100) %>% 
    dplyr::select(-year) %>% 
    ungroup() %>% 
    group_by(p_holder, season, area) %>% 
    summarise(t = mean(t)) %>% 
    group_by(season, area) %>% 
    mutate(tt = sum(t), perc = t/tt) %>% 
    dplyr::select(-t, -tt) -> allocation 
  
  p_holders <- unique(allocation$p_holder) # vector of vessels that have IFQ
  
  pol %>% 
    filter(p_holder %in% p_holders) %>% 
    dplyr::select(p_fshy, p_holder) %>% 
    distinct() %>% 
    left_join(allocation) %>% 
    arrange(p_holder) %>% 
    filter(p_fshy!=1) %>% 
    dplyr::select(-p_fshy) -> allocation
  
  
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
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
    left_join(allocation) %>% 
    mutate(C1 = ifelse(area==1, C1 * perc, 0),
           C2 = ifelse(area==2, C2 * perc, 0),
           C3 = ifelse(area==3, C3 * perc, 0)) -> temp2
  
  temp2 %>% 
    group_by(sim, season, p_holder) %>% 
    summarise(C1 = max(C1),
              C2 = max(C2),
              C3 = max(C3)) %>% 
    left_join(f.boats_large()) %>% 
    left_join(f.trip_behavior()) %>% 
    mutate(prob = 1) %>% 
    filter(p_fshy>1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) %>% 
    drop_na
}
tac.all <- function(x){
  # tac for open access fisheries/llp
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
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
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob)  
}  
tac.port.all <- function(x){
  # tac by port allocations for all vessels
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA)) %>% 
    dplyr::select(-area, -app, -abc, -tac) -> temp
  
  
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
           C3 = filter(tac, !is.na(C3))$C3,
           C1 = ifelse(C1<50, 0, C1),
           C2 = ifelse(C2<50, 0, C2),
           C3 = ifelse(C3<50, 0, C3)) %>% 
    left_join(f.boats()) %>% 
    left_join(f.trip_behavior()) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.port.fed <- function(x){
  # tac by port allocation for federal waters vessels only
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
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
           C3 = ifelse(area==3, (C3 * perc_catch), NA),
           C1 = ifelse(C1<50, 0, C1),
           C2 = ifelse(C2<50, 0, C2),
           C3 = ifelse(C3<50, 0, C3)) %>% 
    dplyr::select(-perc_catch, -area) %>% 
    ungroup -> tac
  
  tac %>% 
    filter(!is.na(C1))  %>% 
    mutate(C2 = filter(tac, !is.na(C2))$C2,
           C3 = filter(tac, !is.na(C3))$C3) %>% 
    left_join(f.boats_large()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(p_fshy>1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.baseline <- function(x){
  # tac for examining model
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
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
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob)  
}  
tac.state.ecs <- function(x){
  # state waters equal catch shares tac for small vessels
  tac.state(x) %>% 
    group_by(season, sim) %>% 
    mutate(n = n(), CATCH = sum(mean(C1), mean(C2), mean(C3))/n) %>% 
    dplyr::select(-n)
}
tac.state.sx <- function(x){
  # state waters superexclusive tac for small vessels
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, 0),
           C2 = ifelse(area==2, tac, 0),
           C3 = ifelse(area==3, tac, 0)) %>% 
    dplyr::select(-app, -abc, -tac)   %>%  
    left_join(f.boats_small_sx()) %>%
    left_join(f.trip_behavior_sx()) %>%  
    filter(p_fshy==1) %>% 
    mutate(deli = port) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
}
tac.fed.psc <- function(x){
  # federal prohibited species tac
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    merge(salmon.fed, by='sim') %>% 
    mutate(tac = app/100 * abc,
           C1 = ifelse(area==1, tac, NA),
           C2 = ifelse(area==2, tac, NA),
           C3 = ifelse(area==3, tac, NA),
           salmon = case_when(area==3 ~ salmon * 0.2674,
                              area!=3 ~ salmon * 0.7326)) %>% 
    group_by(area, sim) %>% 
    mutate(S1 = ifelse(area==1, app/sum(app) * salmon, NA),
           S2 = ifelse(area==2, app/sum(app) * salmon, NA), 
           S3 = ifelse(area==3, app/sum(app) * salmon, NA)) %>% 
    ungroup %>% 
    dplyr::select(-area, -app, -abc, -tac, -salmon)  -> temp
  
  temp %>% 
    filter(C1>0 & S1>0) %>% 
    dplyr::select(-C2, -C3, -S2, -S3) %>% 
    left_join(temp %>% 
                filter(C2>0 & S2>0) %>% 
                dplyr::select(-C1, -C3, -S1, -S3)) %>% 
    left_join(temp %>% 
                filter(C3>0 & S3>0) %>% 
                dplyr::select(-C1, -C2, -S1, -S2)) %>% 
    left_join(f.boats_large()) %>% 
    left_join(f.trip_behavior()) %>% 
    filter(p_fshy>1) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, S1, S2, S3, prob) 
}

f.tac.incoop.all <- function(x, sq2, incoop){
  # federal waters cooperative tac with open access state waters
  # take previous 5 years of status quo fishery catch
  # sort the individuals based upon their revenue returns
  # filter out lower catch
  
  sq2 %>% 
    do(f.rev(., 0.12, 0.8)) %>% 
    group_by(season, p_holder, sim) %>%
    mutate(mp = sum(catch) * (0.12 * 2204.622) - (sum(cost) / sum(catch)),
           mpi = sum(catch) * (0.12 * 2204.622) - (sum(cost) / sum(catch))) %>% 
    group_by(season, p_holder) %>%
    summarise_all(funs(mean)) %>%
    group_by(season) %>% 
    mutate(mp = mean(mp, na.rm=T),
           rent = mpi - mp,
           qlow = quantile(rent, 0.05, na.rm = T)) %>% 
    filter(rent>qlow) %>% 
    dplyr::select(season, p_holder) -> stay_in
  
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc * 0.85,
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
    left_join(stay_in) %>% 
    left_join(f.trip_behavior()) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3), prob = 1) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
  
  
}
f.tac.outcoop.all <- function(x, outcoop){
  # determine which vessels are outside of the cooperative
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc * 0.15,
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
    filter(p_holder %in% outcoop$p_holder) %>% 
    left_join(f.trip_behavior()) %>%
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
  
  
}

f.tac.incoop.fed <- function(x, sq2, incoop){
  # determine which vessels are in the cooperative
  # take previous 5 years of status quo fishery catch
  # sort the individuals based upon their revenue returns
  # filter out lower catch
  
  sq2 %>% 
    do(f.rev(., 0.12, 0.8)) %>% 
    group_by(season, p_holder, sim) %>%
    mutate(mp = sum(catch) * (0.12 * 2204.622) - (sum(cost) / sum(catch)),
           mpi = sum(catch) * (0.12 * 2204.622) - (sum(cost) / sum(catch))) %>% 
    group_by(season, p_holder) %>%
    summarise_all(funs(mean)) %>%
    group_by(season) %>% 
    mutate(mp = mean(mp, na.rm=T),
           rent = mpi - mp,
           qlow = quantile(rent, 0.05, na.rm = T)) %>% 
    filter(rent>qlow) %>% 
    dplyr::select(season, p_holder) -> stay_in
  
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc * 0.85,
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
    left_join(stay_in) %>% 
    left_join(f.trip_behavior()) %>% 
    mutate(deli = port, area = if_else(port==1, 1, 3), prob = 1) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
  
  
}
f.tac.outcoop.fed <- function(x, outcoop){
  # vessels (large, federal waters only) outside of the coop
  app %>% 
    ungroup %>% 
    mutate(area = case_when(area==610 ~ 3,
                            area==620 ~ 2,
                            area==630 ~ 1),
           season = case_when(season == 'a' ~ 1,
                              season == 'b' ~ 2, 
                              season == 'c' ~ 3,
                              season == 'd' ~ 4)) %>% 
    merge(x, by=NULL) %>% 
    mutate(tac = app/100 * abc * 0.15,
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
    filter(p_holder %in% outcoop$p_holder) %>% 
    left_join(f.trip_behavior()) %>%
    mutate(deli = port, area = if_else(port==1, 1, 3)) %>% 
    dplyr::select(p_holder, p_fshy, area, port, deli, season, days=tday, t, 
                  sd.t, day, sd.day, sim, C1, C2, C3, prob) 
  
  
}
# create lists ----
# break into appropriate lists for simulations
sim.season <- function(x) { 
  split(x, interaction(x$sim, x$season), drop = TRUE)
}
sim.season.area <- function(x) { 
  split(x, interaction(x$sim, x$season, x$area), drop = TRUE)
}
sim.season.port <- function(x){
  split(x, interaction(x$port, x$season, x$sim), drop = TRUE)
}
sim.season.p_holder <- function(x){
  split(x, interaction(x$season, x$sim, x$p_holder), drop = TRUE)
}

