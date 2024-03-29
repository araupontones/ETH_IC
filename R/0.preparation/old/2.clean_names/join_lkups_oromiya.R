source("set_up.R")
source("functions/my_fuzzy.R") #function to fuzzy merge


is_name_distance_jw.5 <- function(left, right) {
  stringdist(left, right, method = "jw") < .05
}


is_name_distance_jw_1 <- function(left, right) {
  stringdist(left, right, method = "jw") < .1
}



zone_census = import(file.path(dir_data_reference_lkups, "Census/zones_census.xlsx"))
wereda_census = import(file.path(dir_data_reference_lkups, "Census/weredas_census.xlsx"))
kebele_census = import(file.path(dir_data_reference_lkups, "Census/kebeles_census.xlsx"))


field = import(file.path(dir_data_reference_lkups, "Oromiya/field.xlsx"))
zone_field = import(file.path(dir_data_reference_lkups, "Oromiya/zones_field.xlsx"))
wereda_field = import(file.path(dir_data_reference_lkups, "Oromiya/weredas_field.xlsx"))
kebele_field = import(file.path(dir_data_reference_lkups, "Oromiya/kebeles_field.xlsx"))






#Join zones
fuzzy_join_zones = my_fuzzy(left_data = zone_field, right_data  = zone_census, by = "Zone",
                            match_fun = is_name_distance_jacard_3 ) %>%
  mutate(merge_zone= case_when(is.na(Zone_census)~ "ZONE not in census",
                               T ~ "merged")) %>%
#calculate the distance
mutate(distance = stringdist(Zone_field, Zone_census, method = "jaccard"),
       distance = if_else(is.na(distance), 0, distance)) %>%
  relocate(starts_with("Zone"), distance) %>%
  group_by(Zone_field) %>%
  #keep the lower distance
  filter(distance == min(distance)) 




#Weredas --------------------------------------------------------------------------
#1. fetch the Zone from the field
weredas = wereda_field %>%
  left_join(select(fuzzy_join_zones, c(Zone_census, Zone_field, merge_zone)), by = c("Zone"="Zone_field")) %>%
  #filter out zones that did not merged and missing zones 
  filter(merge_zone == "merged") %>%
  mutate(to_join = paste(Region, Zone_census, Wereda, sep = "-")) %>%
  select(-c(Zone_census, merge_zone)) 




fuzzy_join_weredas = my_fuzzy(left_data = weredas, 
            right_data= wereda_census %>% mutate(to_join = paste(Region, Zone, Wereda, sep = "-")), 
            by = "to_join",
            match_fun = is_name_distance_jw.5 ) %>%
#calculate the distance
  mutate(distance = stringdist(to_join_field, to_join_census, method = "jw")) %>%
  relocate(starts_with("Wereda"), distance, starts_with("to_join")) %>%
  group_by(to_join_field) %>%
  #keep the lower distance
  filter(distance == min(distance)) %>%
  ungroup() %>%
  group_by(Wereda_census) %>%
  filter(distance == min(distance)) %>%
  select(-c(starts_with("to_")))




#Kebeles
kebeles = kebele_field %>%
  left_join(select(fuzzy_join_weredas, Wereda_field, Wereda_census, Zone_census), by=c("Wereda" = "Wereda_field")) %>%
  filter(!is.na(Wereda_census),
         !is.na(Kebele)) %>%
  mutate(to_join = paste(Zone_census,Wereda_census, Kebele, sep = "-")) %>%
  select(-Wereda_census, -Zone_census)


fuzzy_join_kebeles = my_fuzzy(left_data = kebeles, 
                              right_data= kebele_census %>% mutate(to_join = paste(Zone,Wereda,Kebele, sep = "-")), 
                              by = "to_join",
                              match_fun = is_name_distance_jw_1 ) %>%
  
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
  add_count(to_join_field, name ="n_kebele") %>%
  filter(n_kebele == 1) %>%
  mutate(to_join = paste(Wereda_field, Kebele_field, sep="-"))

 

#join with field data ----------------------------------------------------------

field_joint = field %>%
  group_by(Wereda, Kebele) %>%
  arrange(Wereda, Kebele) %>%
  slice(1) %>%
  ungroup()%>%
  left_join(select(fuzzy_join_zones, Zone_field, Zone_census), by =c("Zone" = "Zone_field")) %>%
  mutate(merge_ = case_when(is.na(Zone_census)~"Zone not in census",
                            T ~ "merged")) %>%
  left_join(select(fuzzy_join_weredas, Wereda_field, Wereda_census), by = c("Wereda" = "Wereda_field")) %>%
  mutate(merge_2 = case_when(merge_ == "Zone not in census" ~ merge_,
                             !is.na(Wereda_census) ~ "merged",
                            is.na(Wereda_census) ~ "Wereda not in census")
  ) %>%
  mutate(to_join = paste(Wereda, Kebele, sep = "-")) %>%
  left_join(select(fuzzy_join_kebeles,to_join, Kebele_census), by = c("to_join")) %>%
  mutate(merge_3 = case_when(merge_ == "Zone not in census" ~ merge_,
                             merge_2 == "Wereda not in census" ~ merge_2,
                             !is.na(Kebele_census) ~ "merged" ,
                            T ~ "Kebele not in census") 
         ) %>%
  select(-c(merge_2, merge_, to_join, Sector, Responsibility, Telefone, Position, Sex, Name)) %>%
  rename(merge_ = merge_3) %>%
  relocate(merge_, c(Region, Zone, Wereda, Kebele),c(ends_with("census")))
  

export(field_joint, file.path(dir_data_reference_clean, "treated_oromiya_merge_status.xlsx"))

table(field_joint$merge_)

check = field_joint %>%
  group_by(Zone, Wereda) %>%
  slice(1) %>%
  select(Zone, Wereda, merge_) %>%
  arrange(Zone, Wereda) %>%
  mutate(Wereda_census="") %>%
  filter(merge_== "Wereda not in census") %>%
  export(file.path(dir_data_reference_clean, "not_found_woredas_oromiya.xlsx"))

 wereda_census %>%
   filter(Zone %in% c("arsi", "east hararge","jimma","west arsi")) %>%
   export(file.path(dir_data_reference_clean, "census_woredas_oromiya.xlsx"))
 
