source("set_up.R")



zone_census = import(file.path(dir_data_reference_lkups, "Census/zones_census.xlsx"))
wereda_census = import(file.path(dir_data_reference_lkups, "Census/weredas_census.xlsx"))
kebele_census = import(file.path(dir_data_reference_lkups, "Census/kebeles_census.xlsx"))


field = import(file.path(dir_data_reference_lkups, "SNNP/field.xlsx"))
zone_field = import(file.path(dir_data_reference_lkups, "SNNP/zones_field.xlsx"))
wereda_field = import(file.path(dir_data_reference_lkups, "SNNP/weredas_field.xlsx"))
kebele_field = import(file.path(dir_data_reference_lkups, "SNNP/kebeles_field.xlsx"))






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



#Join zones
fuzzy_join_zones = my_fuzzy(left_data = zone_field, right_data  = zone_census, by = "Zone",
                            match_fun = is_name_distance_jacard_3 ) %>%
  mutate(merge_zone= case_when(is.na(Zone_census)~ "ZONE not in census",
                               T ~ "merged"))




#Weredas --------------------------------------------------------------------------
#1. fetch the Zone from the field
weredas = wereda_field %>%
  left_join(select(fuzzy_join_zones, c(Zone_census, Zone_field, merge_zone)), by = c("Zone"="Zone_field")) %>%
  #filter out zones that did not merged and missing zones 
  filter(merge_zone == "merged",
         Wereda != "missing") %>%
  mutate(to_join = paste(Region, Zone_census, Wereda, sep = "-")) %>%
  select(-c(Zone_census, merge_zone)) 


fuzzy_join_weredas = my_fuzzy(left_data = weredas, 
            right_data= wereda_census %>% mutate(to_join = paste(Region, Zone, Wereda, sep = "-")), 
            by = "to_join",
            match_fun = is_name_distance_jacard_3 ) %>%
  #calculate the distance
  mutate(distance = stringdist(Wereda_field, Wereda_census, method = "jaccard")) %>%
  relocate(starts_with("Wereda"), distance) %>%
  group_by(Wereda_field) %>%
  #keep the lower distance
  filter(distance == min(distance)) %>%
  select(-c(starts_with("to_")))




#Kebeles

kebeles = kebele_field %>%
  left_join(select(fuzzy_join_weredas, Wereda_field, Wereda_census), by=c("Wereda" = "Wereda_field")) %>%
  filter(Wereda != "missing",
         !is.na(Wereda_census)) %>%
  mutate(to_join = paste(Wereda_census, Kebele, sep = "-")) %>%
  select(-Wereda_census)


fuzzy_join_kebeles = my_fuzzy(left_data = kebeles, 
                              right_data= kebele_census %>% mutate(to_join = paste(Wereda,Kebele, sep = "-")), 
                              by = "to_join",
                              match_fun = is_name_distance_jw_1.5 ) %>%
  
  #calculate the distance
  mutate(distance = stringdist(to_join_field, to_join_census, method = "jw")) %>%
  arrange(Wereda_field, Kebele_field, distance) %>%
  relocate(starts_with("to_join"), distance) %>%
  filter(!is.na(Wereda_census)) %>%
group_by(to_join_field,) %>%
  #keep the lower distance
  filter(distance == min(distance)) %>%
  ungroup() %>%
  group_by(to_join_census) %>%
  filter(distance == min(distance)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  add_count(to_join_field, name ="n_kebele")

 

#join with field data ----------------------------------------------------------

field_joint = field %>%
  left_join(select(fuzzy_join_zones, Zone_field, Zone_census), by =c("Zone" = "Zone_field")) %>%
  mutate(merge_ = case_when(is.na(Zone_census)~"Zone not in census",
                            T ~ "merged")) %>%
  left_join(select(fuzzy_join_weredas, Wereda_field, Wereda_census), by = c("Wereda" = "Wereda_field")) %>%
  mutate(merge_2 = case_when(merge_ == "Zone not in census" ~ merge_,
                             !is.na(Wereda_census) ~ "merged",
                            is.na(Wereda_census) ~ "Wereda not in census")
  ) %>%
  left_join(select(fuzzy_join_kebeles, Kebele_field, Kebele_census), by =c("Kebele" = "Kebele_field")) %>%
  mutate(merge_3 = case_when(merge_ == "Zone not in census" ~ merge_,
                             merge_2 == "Wereda not in census" ~ merge_2,
                             !is.na(Kebele_census) ~ "merged" ,
                            T ~ "Kebele not in census")
         ) %>%
  select(-c(merge_2, merge_, Name_field)) %>%
  rename(merge_ = merge_3) %>%
  relocate(merge_, c(Region, Zone, Wereda, Kebele),c(ends_with("census")) )
  

export(field_joint, file.path(dir_data_reference_clean, "treated_snnp_merge_status.xlsx"))



