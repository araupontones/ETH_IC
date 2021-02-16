#FUnction to fuzzy merge
my_fuzzy = function(left_data,
                    right_data,
                    by,
                    match_fun  = is_name_distance_jacard_3
){
  
  fuzzy_left_join(
    left_data,
    right_data,
    by = c(by),
    match_fun  = match_fun
  ) %>%
    rename_at(vars(ends_with('.x')), funs(paste0(str_remove(.,".x"), "_field"))) %>%
    rename_at(vars(ends_with('.y')), funs(paste0(str_remove(.,".y"), "_census"))) 
  
}