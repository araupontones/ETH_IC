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

is_name_distance_jacard_4 <- function(left, right) {
  stringdist(left, right, method = "jaccard") < .4
}



#create look up table of the string to be merge -------------------------------

#' @param  db dataframe of interest
#' @param varname varname to create the lookup
#' @param keep variables to keep in the lookup
create_look_up = function(db, varname, keep, ...){
  
  data_f = db %>%
    as.data.frame() %>%
    group_by(get(varname)) %>%
    slice(1) %>%
    ungroup() %>%
    select(all_of(keep))
  
  return(data_f)
  
  
}


##to fuzzy merge ---------------------------------------------------------------
#'@param pop_data 
#'@param centroid_data
#'@param fuzzy_var var to check
#'@param keep_vars vars to keep in lookup


check_diffs_ = function(pop_data, 
                               centroid_data,
                               fuzzy_var,
                               keep_vars,
                               ...){

pop = create_look_up(db = pop_data, varname = fuzzy_var, keep = keep_vars)
centr = create_look_up(db = centroid_data, varname = fuzzy_var, keep = keep_vars)

#Check differences
diff = knitr::combine_words(setdiff(unique(pop[[fuzzy_var]]), unique(centr[[fuzzy_var]])))

message(paste("In population only:",diff))


return(list("pop"= pop,
            "centr"= centr))

}


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
  setdiff(unique(from_data[[var_y]]), unique(centroids_int[[join_var]]))
  
  
  

  #join
  merge = to_data %>%
    left_join(from_data, by=(join_var)) %>%
    select(-var_x)
  
  return(merge)
  
}





