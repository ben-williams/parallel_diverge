# search 1 ----
# This sets the search behavior before each trip begins
# currently based upon a trip of 2.5 days
# and a catch of 50 t - the exvessel values and 
# fuel costs are set for each model independently

# Initial starting functions
f.penalty <- function(x){
  # assigns a 'penalty' in number of days travel to/from fishing port(s)
  area = x[2] 
  port = x[3] 
  deli = x[4] 
  #add travel distance onto random fishing lengths
  if(x[1]==9 | 
     area==1 & port==1 & deli==1|
     area==3 & port==2 & deli==2|
     area==3 & port==2 & deli==3|
     area==3 & port==3 & deli==2|
     area==3 & port==3 & deli==3|
     area==3 & port==4 & deli==4) {0}
  else if(area==1 & port==2 & deli==1|
          area==1 & port==3 & deli==1|
          area==2 & port==1 & deli==1) {0.5}
  else if(area==2 & port==1 & deli==2|
          area==2 & port==1 & deli==3|
          area==2 & port==2 & deli==1|
          area==2 & port==3 & deli==1) {1.5}
  else if(area==1 & port==4 & deli==1|
          area==2 & port==2 & deli==4|
          area==2 & port==3 & deli==4|
          area==2 & port==4 & deli==2|
          area==2 & port==4 & deli==3|
          area==3 & port==2 & deli==1|
          area==3 & port==3 & deli==1|
          area==3 & port==4 & deli==1) {2}
  else if(area==1 & port==4 & deli==2|
          area==1 & port==4 & deli==3|
          area==2 & port==1 & deli==4|
          area==2 & port==4 & deli==1|
          area==3 & port==1 & deli==2|
          area==3 & port==1 & deli==3|
          area==3 & port==1 & deli==4) {2.5}
  else if(area==1 & port==1 & deli==4|
          area==1 & port==2 & deli==4|
          area==1 & port==3 & deli==4|
          area==1 & port==4 & deli==4|
          area==2 & port==4 & deli==4|
          area==3 & port==1 & deli==1) {3} else {1}
}
f.gridfuel <- function(x, penalty){
  #assumed days to catch 100 t fish
  assume_day = 2.5
  #for each vessel size
  fuel = ifelse(x[1]==9, 0,
                ifelse(x[1]==1, 500 * 2.5522 * fuel_price * (assume_day + penalty),
                       ifelse(x[1]==2, 750 * 2.5522 * fuel_price * (assume_day + penalty),
                              ifelse(x[1]==3, 1000 * 2.5522 * fuel_price * (assume_day + penalty),
                                     1500 * 2.5522 * fuel_price * (assume_day + penalty)))))
  fuel
  
}
f.gridexv <- function(x){
  #exvessel value can be set seperate for each port
  if(x[4]==1) {ex1
  } else if(x[4]==2) {ex2
  } else if(x[4]==3) {ex3
  } else {ex4}
  
}
f.gridrev <- function(gridfuel, gridexv){
  #calculate revenue based upon assumed travel time and known exvessel price
  assumed_catch = 50
  rev = (assumed_catch * 2204.622 * gridexv) - 
    (gridfuel + (assumed_catch * 2204.622 * gridexv) * 0.00625)
  #(catch t converted to pounds * exvessel value) -(fuel costs + observer costs)
  target = 0 - rev
  #set target to 0 for optimization routine
} 
f.grid <- function(x){
  # roll all of the functions together
  penalty = f.penalty(x)
  gridfuel = f.gridfuel(x, penalty)
  gridexv = f.gridexv(x)
  gridrev = f.gridrev(gridfuel, gridexv)
  gridrev
}