my.library <- 'C:/Users/yakov/Documents/R/win-library/3.6'
.libPaths(my.library)

library('tidyverse')
c('reshape2', 'stringr', 'magrittr', 'plyr') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

var_save <- '../variables/'

registerDoParallel(cores=28)

num.sites = 3
m <- num.sites
plant_locations <- data.frame(
  id=1:m,
  x = runif(m,0,1) * 1,
  y = runif(m,0,1) * 1
)

plant_locations<-subset(plant_locations,select = c('x','y'))
x<-plant_locations[,1]
y<-plant_locations[,2]
assembly_locs <- assembly

distance <- function(i, j) {
  assembly_l <- assembly_locs[i, ]
  competetor_l <- competetor[j, ]
  (sqrt((assembly_l$x - competetor_l$x)^2 + (assembly_l$y - competetor_l$y)^2))
}


dmi10 <- matrix(0, nrow=nrow(assembly), ncol=nrow(plant_locations))

for(col in 1:nrow(plant_locations)){
  for(row in 1:nrow(assembly_locs)){
    dmi10[row,col]<-distance(row, col)    
  }
}

dmi100<-colSums(dmi10)
cri_new<- costParameters[1,1]*dmi100 + costParameters[1,2]*unionizationRate(plant_locations[,1],plant_locations[,2])
cri_new


dmi11 <- matrix(0, nrow=nrow(assembly), ncol=nrow(competetor))

for(col in 1:nrow(competetor)){
  for(row in 1:nrow(assembly_locs)){
    dmi11[row,col]<-distance(row, col)    
  }
}

dmi110<-colSums(dmi11)

cri_old<- costParameters[1,1]*dmi110 + costParameters[1,2]*unionizationRate(competetor[,1],competetor[,2])
cri_old

min_comp<-min(cri_old)
min_comp

max_potential<-max(cri_new)
max_potential

x=plant_locations[,1]
y=plant_locations[,2]

to_minimize<-function(x,y){
  max(costParameters[1,1]*dmi100 + costParameters[1,2]*unionizationRate(plant_locations[,1],plant_locations[,2])) 
  - min( costParameters[1,1]*dmi110 + costParameters[1,2]*unionizationRate(competetor[,1],competetor[,2]))
}


seq(1) %>%
  llply(
    function(l){
      optim(
        runif(2 * num.sites),
        fn = to_minimize,
        control=list(maxit=100)
      )
    },
    .parallel = FALSE
  )

(result <- optim(par = c(c(0,0),c(0,0)), fn = to_minimize))