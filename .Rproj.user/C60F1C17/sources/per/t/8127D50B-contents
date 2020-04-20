#Coding assignment based on the counterfactual analysis of Rosenbaum (2013):
#Construct the findBestLocation() function, which identifies the optimal foreign-owned supplier locations, given the location of the domestic-owned suppliers. 
#Let the expected cost of a supplier satesfying an assembly plant be a linear function of the union rate and the distance between the supplier and assembly plant. beta gives these multiplicative constants.

source('code/header.R')

make_data(
  assembly.count = 12,
  competetor.count = 4,
  distance = 10,
  union = 2
)

find_best_location1(
  num.sites = 6,
  assemblers = readRDS(paste0(var_save, 'assembly.rds')),
  competetors = readRDS(paste0(var_save, 'competetor.rds')),
  union.rate.fn = readRDS(paste0(var_save, 'unionizationRate.rds')),
  beta = readRDS(paste0(var_save, 'costParameters.rds')),
  num.tries = 12
) %>% 
  saveRDS(paste0(var_save, 'solution1.rds'))
