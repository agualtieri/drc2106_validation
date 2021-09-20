minium_standards <- function(data, group_var, items_vec){

  data.m <- data %>% select(group_var, items_vec) %>% melt()
  
  data.m %>% group_by(variable, !!sym(group_var)) %>% filter(!is.na(value)) %>%
    dplyr::summarise(., n = n()) %>% mutate(minimum_standards = ifelse(n >= 3, "requirement met", "requirement not met"))

}

