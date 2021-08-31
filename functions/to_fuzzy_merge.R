#FUNCITIONS FOR FUZZY MERGE



# Calculate Tring distance------------------------------------------------------

is_name_distance_0 <- function(left, right) {
  stringdist(left, right) ==0
}


is_name_distance_below_2 <- function(left, right) {
  stringdist(left, right) < 2
}


is_name_distance_below_3 <- function(left, right) {
  stringdist(left, right) < 3
}


is_name_distance_below_4 <- function(left, right) {
  stringdist(left, right) < 4
}


is_name_distance_below_6 <- function(left, right) {
  stringdist(left, right) < 6
}

is_name_distance_jacard_3 <- function(left, right) {
  stringdist(left, right, method = "jaccard") < .3
}

is_name_distance_jacard_5 <- function(left, right) {
  stringdist(left, right, method = "jaccard") < .5
}

is_name_distance_jacard_4 <- function(left, right) {
  stringdist(left, right, method = "jaccard") < .4
}

is_name_distance_jw_1.7 <- function(left, right) {
  stringdist(left, right, method = "jw") < .17
}

is_name_distance_jw_1.5 <- function(left, right) {
  stringdist(left, right, method = "jw") < .15
}

is_name_distance_jw_1.3 <- function(left, right) {
  stringdist(left, right, method = "jw") < .13
}

is_name_distance_jw_1.2 <- function(left, right) {
  stringdist(left, right, method = "jw") < .12
}

is_name_distance_jw_1.1 <- function(left, right) {
  stringdist(left, right, method = "jw") < .11
}


#create look up table of the string to be merge -------------------------------

#' @param  db dataframe of interest
#' @param varname varname to create the lookup
#' @param keep variables to keep in the lookup
create_look_up = function(db, varname, keep, ...){
  
  data_f = db %>%
    as.tibble() %>%
    group_by({{varname}}) %>%
    slice(1) %>%
    ungroup() %>%
    select({{keep}})
  
  return(data_f)
  
}


##to fuzzy merge ---------------------------------------------------------------
#'@param pop_data 
#'@param centroid_data
#'@param fuzzy_var var to check
#'@param keep_vars vars to keep in lookup


check_diffs_ = function(left_data, 
                        right_data,
                        fuzzy_var,
                        keep_vars,
                        ...){
  
  left = create_look_up(db = left_data, varname = {{fuzzy_var}}, keep = {{keep_vars}})
  right = create_look_up(db = right_data, varname = {{fuzzy_var}}, keep = {{keep_vars}})
  
  #Check differences
  
  
  unique_left = select(left, {{fuzzy_var}}) %>% unique()
  unique_right = select(right, {{fuzzy_var}}) %>% unique()
  
  only_left = knitr::combine_words(setdiff(unique_left, unique_right))
  only_right = knitr::combine_words(setdiff(unique_right, unique_left))
  
  
  
  message(glue::glue("Observations in left: {nrow(left)}\n
                     Observations in right: {nrow(right)}\n
                     Only in left: {only_left}\n
                     Only in right: {only_right}" )
          )
  # message(paste("In left only:",only_left))
  # message(paste("In right only:",only_right))
  
  
  
  return(list("left"= left,
              "right"= right))
  
}


##fuzzy merge population and centrouds by fuzzzy variable

#'@param left_data data set to be the left_join
#'@param rigth_data data set to join to
#'@param by variables to join 
#'@match_fun matching criteria
#'@method method to calculate the distance between names 

fuzzy_join_ = function(left_data,
                       rigth_data,
                       by,
                       match_fun,
                       method = "jaccard",
                       suffix_left = "",
                       ...) {

 
  
  #create name of variable to merge
  nm <-deparse(substitute(by))
  
  nm.x = paste0(nm,".x")
  nm.y = paste0(nm, ".y")
  
  #join data sets
  data_ = fuzzy_left_join(
    left_data,rigth_data,
    by = c(nm),
    match_fun  = match_fun
  ) %>%
    mutate(distance= stringdist(get(nm.x), get(nm.y), method = method)
           ) %>%
    arrange(get(nm.y), distance) %>%
    group_by(get(nm.y)) %>%
    filter(distance == min(distance)) %>%
    ungroup() %>%
    add_count(get(nm.y)) %>%
    rename_at(vars(ends_with('.y')), funs(str_remove(.,".y"))) %>%
    rename_at(vars(ends_with('.x')), funs(paste0(str_remove(.,".x"), suffix_left)))
  
  #check for differences betwen jpint and right data set
  unique_joint = unique(data_[[paste0(nm,suffix_left)]])
  unique_right = unique(rigth_data[[nm]])
  
  dif = setdiff(unique_right, unique_joint)
  
  if(is_empty(dif)){
    
    dif = 0
   # message to help the user
    message(glue::glue("Observations in right data: {nrow(rigth_data)}\n
                       Observations in joint data: {nrow(data_)}\n
                       Not matched observations:{dif}"))
  } else {
    
    #message to help the user
    message(glue::glue("Observations in right data: {nrow(rigth_data)}\n
                       Observations in joint data: {nrow(data_)}\n
                       Not matched observations:"))
    
    print(dif)
    
    
  }
  

  
 
  
  return(data_)
}
  
  # )  








join_with_pop = function(
  to_data,
  from_data,
  join_var,
  ...
  
  
){
  
  
  #Create variable names
  var = paste0(join_var)
  var_x = paste0(join_var,".x")
  var_y = paste0(join_var,".y")
  
  
  #create join variable in toData so joining is feasible
  from_data[[var]] = from_data[[var_x]]
  
  
  
  ##check that all exist in both data bases
  print("In from data but NOT in population:")
  
  
  print(setdiff(unique(from_data[[var]]),
                unique(to_data[[var]])
  ))
  
  print("In from data but NOT in centroids:")
  print(setdiff(unique(from_data[[var_y]]), unique(centroids_int[[join_var]])))
  
  
  
  
  #join
  merge = to_data %>%
    left_join(from_data, by=(join_var)) %>%
    select(-var_x)
  
  return(merge)
  
}





