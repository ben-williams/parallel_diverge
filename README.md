An agent-based model to examine parallel and divergent 
fishery management strategies for transboundary stock: 
application to the walleye pollock fishery in the Gulf of 
Alaska 

Benjamin C. Williams*, Keith R. Criddle, Gordon H. Kruse
 * ben.williams@alaska.gov

Code for reproducing model simulations

There are seven key **function** files used:

 1. helper.R - stores all lobraries necessary for the simulations, pulls in some data.
 2. tac.R - functions to determine the number of vessels in a fleet, vessel fishing behavior, allocations per management scenario, creates lists based upon the appropriate scenario structure
 3. gridsearch.R - functions to a) assign a travel penalty, b) calculate anticipated fuel costs based upon vessel size and trip duratino + penalty, c) provides for a flexible exvessel value by port, d) calculates expected revenue for each trip and sets the target to 0 for optimation.
 4. search_patterns.R - functions to define the search patterns depending upon which fishing areas are open or closed.
 5. simulation.R - base functions for the simulation, has three primary functions for each scenario - an initial fishing event (f.sim), a search function inplementation (f.search_all) that incorporates the search_patterns.R functions, and continued fishing events (f.sim2)
 6. model.R - bundles the previous functions so they can be easily called
 7. cleanup.R - is used to calculate the revenue from a trip
 
 The **scenarios** folder has the simulation runs for each management scenario. The **80_10** folder identifies the global characteristics as fuel cost $0.80/l and assumed exvessel value \$0.10/pound. Each scenario is simulated 40 times on a fixed ABC and 5 times on a sequential ABC.
 
The **evaluations** folder has folders for pulling in all the simulation results for the fixed ABC and sequential ABC models, calculating the revenue generated and plotting the results.
 