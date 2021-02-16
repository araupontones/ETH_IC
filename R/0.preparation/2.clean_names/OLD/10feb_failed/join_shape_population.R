#joint population census
source("Set_up.R")

clean_zone = import(file.path(dir_data_reference_cleanINT, "population_SNNPR_Oromiya_int.rds"))
centroids_names = import(file.path(dir_data_reference_cleanINT, "centroids_int.rds"))

#Zones----------------------------------------------------------------------- 

#check difference between centroinds and populations

zones_diff = check_diffs_(centroids_names,
                          clean_zone,
                          Zone,
                          Zone)


#Fuzzy joint      
joint_zones_fuzzy = fuzzy_join_(zones_diff$left,
                                zones_diff$right,
                                by = Zone,
                                match_fun = is_name_distance_jacard_3,
                                method = "jaccard",
                                suffix_left = "_shape")



#join with population, and get Z_code from centroids
joint_1_zones_clean = left_join(clean_zone, select(joint_zones_fuzzy,c(Zone, Zone_shape)),
                              by=c("Zone")) 




#WAREDA----------------------------------------------------------------------- 


#check difference and create look up tables at the wareda level

wareda_diff = check_diffs_(centroids_names %>% mutate(Zone_Wareda = paste(Region, Zone, Wareda, sep = "-")),
                           joint_1_zones_clean%>% mutate(Zone_Wareda = paste(Region,Zone_shape, Wareda, sep = "-")),
                           Zone_Wareda,
                           c(Zone_Wareda, Region, Zone, Wareda)
)


#Fuzzy joint waredas     
joint_waredas_fuzzy = fuzzy_join_(wareda_diff$left,
                                  wareda_diff$right,
                                  by = Zone_Wareda,
                                  #match_fun = is_name_distance_jacard_3,
                                  match_fun = is_name_distance_jw_1.1,
                                  method = "jw",
                                  suffix_left = "_shape") %>%
  #keep best match
  group_by(Zone_Wareda) %>%
  filter(distance == max(distance)) %>%
  ungroup()%>%
  add_count(Zone_Wareda, name = "n_", wt = 1) %>%
  add_count(Zone_Wareda_shape, name = "n_shape", wt = 1) %>%
  #arrange(desc(distance)) %>%
  relocate(Region, Zone, Wareda) %>%
  arrange(Region, Zone, Wareda) %>%
  select(-c(starts_with("Zone_Wareda"),distance, starts_with("n"), starts_with("g"), Region_shape))

#join with population

joint_2_waredas_clean = joint_1_zones_clean %>%
  left_join(joint_waredas_fuzzy, 
            by=c("Region", "Zone", "Wareda","Zone_shape")) %>%
  mutate(W_merged_= case_when(is.na(Wareda_shape) ~ "Only in census",
                              T ~ "Merged"))








unique(clean_wereda$Zone)
