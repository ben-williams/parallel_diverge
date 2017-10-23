f.rev <- function(x, ev, fuel_price){  
  x %>% 
    mutate(fuel = if_else(size==1, 500 * 2.5522 * fuel_price * trip,
                          if_else(size==2, 700 * 2.5522 * fuel_price * trip,
                                  if_else(size==3, 1000 * 2.5522 * fuel_price * trip, 
                                          1000 * 2.5522 * fuel_price * trip))),
           # exv = ifelse(rbinom(1,1, 0.3)==1, 0.01, ev),
           exv = ev,
           cost = (catch * 2204.622 * exv) * 0.00625 + fuel,
           g_rev = (catch * 2204.622 * exv),
           n_rev = g_rev - cost) %>% 
    dplyr::select(-t, -sd.t, -day, -sd.day, -n)
}
