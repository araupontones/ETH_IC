#clean population file (sent by Nahom)

source("set_up.R")
source("functions/to_create_lookups.R")

#list.files(dir_data_reference_raw)

#import data
raw = import(file.path(dir_data_reference_raw, 
                       "Copy of 1999 EC -2007 GC kebele Population.xlsx"),
             skip = 4,
             col_names = c("Name", "Total", "Male", "Female"),
             col_types=rep("text",4)
)


#function to create region columns
create_region = function(x, suffix){
  
  zoo::na.locf0(case_when(str_detect({{x}}, suffix) ~ str_remove({{x}}, suffix),
                          T ~ NA_character_)) 
  
}



raw_geo = raw %>%
  
  mutate(
    #name as it is in the population
    Name_population = Name,
    Name = str_to_lower(Name),
    Region = create_region(Name, "-region$|region$")
    ) %>%
  group_by(Region) %>%
  mutate(Zone = create_region(Name, "-zone$")) %>%
  ungroup() %>%
  group_by(Zone) %>%
  mutate(Wereda = create_region(Name, "-wereda$")) %>%
  ungroup() %>%
  mutate_at(vars(Region, Zone, Wereda, Name), str_trim) %>%
  mutate(Region = str_replace(Region, "s.n.n.p", "snnp")) %>%
  filter(Region %in% c("oromiya", "snnp"),
         !is.na(Zone),
         !is.na(Wereda),
         !str_detect(Name, "-wereda$"),
         !str_detect(Name, "-rural$"),
         ) %>%
  relocate(c(Region, Zone, Wereda, Name, Name_population), .before = Total) %>%
  rename(Kebele = Name) %>%
  #clean kebele
  mutate(Kebele = str_replace(Kebele, "hudad 4 ena yayaotuna", "hudad 4"),
         Kebele = str_replace(Kebele, "koto baloso", "koto"),
         Kebele = str_replace(Kebele, "hudad 5 ena 6", "hudad 5-6"),
         Kebele = str_replace(Kebele, "yimerwacho 1nya", "yimerwacho 1st"),
         Kebele = str_replace(Kebele, "yimerwacho 3nya", "yimerwacho 3rd"),
         
         
         
         
         
         )


#create lookup tables

#lookup for zones
my_lookup(exdir =dir_data_reference_lkups, exfile =  "Census/zones_census.xlsx" , Region, Zone)

#Weredas
my_lookup(exdir =dir_data_reference_lkups, exfile =  "Census/weredas_census.xlsx" , Region, Zone, Wereda)


my_lookup(exdir= dir_data_reference_lkups, exfile = "Census/kebeles_census.xlsx", Region, Zone, Wereda, Kebele)


