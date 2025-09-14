#testing for Anova
shapiro_group_testing <- function(
    database = NULL,
    x_col = NULL,
    matched_group_col = NULL,
    ordered_group = NULL){
  
  matched_group_col <- enquo(matched_group_col)
  x_col <- enquo(x_col)
  
  group <- c()
  result <- c()
  
  for(i in ordered_group){
    dat <- database %>% 
      filter(!!matched_group_col == i) %>% 
      pull(!!x_col)
    
    test <- shapiro.test(dat)
    
    group <- c(group, i)
    result <- c(result, test$p.value)
  }
  
  out <- as.data.frame(list(group = group, p_value = result))
  out
  
  #if result P value =< 0.05 then less likely is normally distributed
}
