# searching patterns 
# all areas open ----
f.a123 <- function(x){
  z = gridSearch(f.grid, list(x[1], c(1:3), x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
f.a12 <- function(x){
  z=gridSearch(f.grid, list(x[1], c(1:2), x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
f.a1 <- function(x){
  z = gridSearch(f.grid, list(x[1], 1, x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
f.a2 <- function(x){
  z = gridSearch(f.grid, list(x[1], 2, x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
f.a23 <- function(x){
  z = gridSearch(f.grid, list(x[1], c(2:3), x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
f.a3 <- function(x){
  z = gridSearch(f.grid, list(x[1], 3, x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
f.a13 <- function(x){
  z = gridSearch(f.grid, list(x[1], c(1,3), x[4], c(1:4)))$minlevels
  if(z[4]==2||z[4]==3){
    z[4]=z[4]+rbinom(1,1,.5)
  }
  z
}
# return to same port -----
f.a123pd <- function(x){
  gridSearch(f.grid, list(x[1], c(1:3), x[3], x[3]))$minlevels
}
f.a12pd <- function(x){
  gridSearch(f.grid, list(x[1], c(1:2), x[3], x[3]))$minlevels
}
f.a1pd <- function(x){
  gridSearch(f.grid, list(x[1], 1, x[3], x[3]))$minlevels
}
f.a2pd <- function(x){
  gridSearch(f.grid, list(x[1], 2, x[3], x[3]))$minlevels
}
f.a23pd <- function(x){
  gridSearch(f.grid, list(x[1], c(2:3), x[3], x[3]))$minlevels
}
f.a3pd <- function(x){
  gridSearch(f.grid, list(x[1], 3, x[3],x[3]))$minlevels
}
f.a13pd <- function(x){
  gridSearch(f.grid, list(x[1], c(1,3), x[3], x[3]))$minlevels
}
# super exclusive registration ----
f.ase <- function(x){c(x[1],x[2],x[3],x[4])}

