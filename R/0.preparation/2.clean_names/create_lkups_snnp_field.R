#clean population file (sent by Nahom)

source("set_up.R")
source("functions/to_create_lookups.R")

#list.files(dir_data_reference_raw)

#import data
raw = import(file.path(dir_data_reference_raw, 
                       "SNNP_17.02.xlsx"),
             range = "A8:J364",
             col_names = c("Region", "Zone", "Wereda", "mig_10", "mig_11", "mig_13", 
                           "Total", "unemployment", "returnees", "Remark")
             
             
             )

             
    

raw_geo = raw %>%
  filter(Zone != "Sub Total" | is.na(Zone),
         Wereda != "Sub total" | is.na(Wereda),
         mig_10 != "Total" | is.na(mig_10)) %>%
  rename(Name_field = mig_10) %>%
  mutate(Region = zoo::na.locf0(Region),
         Zone = zoo::na.locf0(Zone)
         ) %>%
  group_by(Zone) %>%
  mutate(Wereda = zoo::na.locf0(Wereda),
         Kebele = Name_field) %>%
  mutate_at(vars(Region, Zone, Wereda, Kebele), str_trim) %>%
  mutate_at(vars(Region, Zone, Wereda, Kebele), str_to_lower) %>%
  relocate(c(Region, Zone, Wereda, Kebele)) %>%
  mutate(Region = str_replace(Region, "snnpr", "snnp"),
         Kebele = str_replace(Kebele,"ho/kuke","holugeb kuke"),
         Kebele = str_replace(Kebele,"ta/bedene","tachegnawo bedane"),
         Kebele = str_replace(Kebele,"la/bedene","laygnawo bedane"),
         Kebele = str_replace(Kebele,"mekala ha","huletegna mekala"),
         Kebele = str_replace(Kebele,"la/lenda","layegnawo lenda"),
         Kebele = str_replace(Kebele,"la/lenda","layegnawo lenda"),
         Kebele = str_replace(Kebele,"kanase","kanosi"),
         Kebele = str_replace(Kebele,"go/bete","gonichebete")
         
         
         
         
         
         
  )


  
#export raw_geo 

raw_geo %>% export(file.path(dir_data_reference_lkups, "SNNP/field.xlsx"))




#lookup for zones
my_lookup(exdir =dir_data_reference_lkups, exfile =  "SNNP/zones_field.xlsx" , Region, Zone)

#Weredas
my_lookup(exdir =dir_data_reference_lkups, exfile =  "SNNP/weredas_field.xlsx" , Region, Zone, Wereda)


my_lookup(exdir= dir_data_reference_lkups, exfile = "SNNP/kebeles_field.xlsx", Region, Zone, Wereda, Kebele)

