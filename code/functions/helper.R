# Code for producing agent-based model simulations of
# GOA walleye pollock fleet behavior
# ben.williams@alaska.gov
# bcwilliams2@alaska.edu

# Notes: This code will be pulled in for each scenario desired

# load ----
library(tidyverse)
theme_set(theme_bw(base_size=11)+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
library(lubridate)
library(truncnorm)
library(NMOF)
library(scales)
library(splitstackshape)
library(MASS)
library(EnvStats)
library(gridExtra)
options(digits=4)
options(scipen=999)

# data ----
Catch <- read_csv('data/Catch.csv') # Simulated TAC data
pol <- read_csv('data/pol.csv') # CFEC data

app <- read_csv('data/apportion.csv')

# Calculate distributions by area and season
app %>%
  filter(area!=640) %>%  
  group_by(area, season) %>% 
  summarise(app = mean(app)) -> app # for alternative model

# create a data.frame of vessel sizes, ports, and season for simulating complete data sets
all.boat <- expand.grid(p_fshy = 1:4, port=1:4, season=1:4, area = 1:3) # dataframe for making a complete grid

# cleanup ----
#cleanup function for after simulation is run
f.docall <- function(x){do.call(bind_rows,x)}

# check results plots
f.check <- function(x){
  x %>% 
    group_by(season, area, sim) %>% 
    summarise(c1 = sum(c1), C1=mean(C1),
              c2 = sum(c2), C2=mean(C2),
              c3 = sum(c3), C3=mean(C3)) %>%
    mutate(c1 = ifelse(area==1, c1, NA),
           c2 = ifelse(area==2, c2, NA),
           c3 = ifelse(area==3, c3, NA)) %>% 
    gather(key, catch, -season, -area, -sim, -C1, -C2, -C3) %>% 
    gather(key2, TAC, -season, -area, -key, -catch, -sim) %>% 
    mutate(catch = ifelse(key=='c1' & key2=='C1', catch, 
                          ifelse(key=="c2" & key2=="C2", catch, 
                                 ifelse(key=='c3' & key2=='C3', catch, NA))),
           Area = factor(area)) %>% 
    ggplot(aes(TAC, catch, color=Area)) + geom_point() + geom_abline(slope=1, lty=4)
}

f.simcheck <- function(x, a){
  x %>% 
    group_by(sim, season, d) %>% 
    summarise(catch = sum(catch)) %>% 
    dplyr::select(sim, season, port=d, catch)-> temp
  
  pol %>% 
    filter(g_pounds>2200, year==a) %>% 
    group_by(port,  season) %>% 
    summarise(t = sum(ton)) %>% 
    left_join(temp) %>% 
    mutate(Port = factor(port)) %>% 
    ggplot(aes(t, catch, color=Port)) + geom_point() + geom_abline(slope=1, lty=4)
  
}