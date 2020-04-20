make_data <- function(assembly.count, competetor.count, distance, union){
  data_frame(
    x = runif(assembly.count),
    y = runif(assembly.count)
  ) %>% 
    saveRDS(paste0(var_save, 'assembly.rds'))
  
  data_frame(
    x = runif(competetor.count),
    y = runif(competetor.count)
  ) %>%
    saveRDS(paste0(var_save, 'competetor.rds'))
  
  data.frame(
    distance = distance,
    union = union
  ) %>%
    saveRDS(paste0(var_save, 'costParameters.rds'))
  
  {function(x, y) (x-1/2)^2+(y/4)^2} %>% 
    saveRDS(paste0(var_save, 'unionizationRate.rds'))
}
