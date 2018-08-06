# functions for cleaning up the simulation data and calculating the revenue generated
f.rev <- function(x, ev, fuel_price){  
  g = lazyeval::expr_text(x)
  x %>% 
    mutate(fuel = if_else(size==1, 500 * 2.5522 * fuel_price * trip,
                          if_else(size==2, 700 * 2.5522 * fuel_price * trip,
                                  if_else(size==3, 1000 * 2.5522 * fuel_price * trip, 
                                          1000 * 2.5522 * fuel_price * trip))),
           # exv = ifelse(rbinom(1,1, 0.3)==1, 0.01, ev),
           cost = (catch * 2204.622 * ev) * 0.00625 + fuel,
           g_rev = (catch * 2204.622 * ev),
           n_rev = g_rev - cost,
           model = g) %>% 
    dplyr::select(-t, -sd.t, -day, -sd.day, -n)
}


theme_black <- function (base_size = 20, base_family = ""){
  theme_minimal() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      line = element_line(colour = "white", size = 0.5, linetype = 1, 
                          lineend = "butt"), 
      rect = element_rect(fill = "white", 
                          colour = "white", size = 0.5, linetype = 1), 
      text = element_text(family = base_family, 
                          face = "plain", colour = "white", size = base_size,
                          angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
      plot.background = element_rect(colour = 'black', fill = 'gray30'),
      # plot.title = element_text(size = rel(1.2)),
      panel.border = element_rect(fill = NA, colour = "white")#, 
      # strip.background = element_rect(fill = "grey30", colour = "white")
    )
}
