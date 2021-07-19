source("set_up.R")

list.files(dir_data_reference_raw)
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



#create columns at the region level
raw_geo= raw %>%
  mutate(
    Name = str_to_title(Name),
    Region = create_region(Name, "-Region$|Region")
  ) %>%
  filter(Region %in% c("S.n.n.p ", "Oromiya")) %>%
  mutate(Region = str_replace(Region, "S.n.n.p ", "SNNP")) %>%
  group_by(Region) %>%
  mutate(Zone = create_region(Name, "-Zone$|Zone$"),
         Wareda = create_region(Name, "-Wereda$|Wereda$")) %>%
  ungroup() %>%
  filter(!str_detect(Name, "Region$|Region|-Zone$|Zone$|-Wereda$|Wereda$|-Rural$|Rural$")) %>%
  relocate(Region, Zone, Wareda, Name) %>%
  arrange(Region, Zone, Wareda, Name)

   
  
  unique(raw_geo$)
  
   Zone = create_region(Kebele, "-ZONE$|ZONE$"),
    Wereda = create_region(Kebele, "-WEREDA|WEREDA$"),
    Town = create_region(Kebele, "-TOWN$|TOWN$"),
    Rural = create_region(Kebele, "-RURAL$|RURAL$")
    ) %>%
  group_by(Region) %>%
  #fill Regions
  mutate(Zone = zoo::na.locf0(Zone),
         Wereda = zoo::na.locf0(Wereda),
         Rural = zoo::na.locf0(Rural),
         Town = zoo::na.locf0(Town)
         ) %>%
  ungroup() %>%
  #drop redundant observations
  filter(!str_detect(Kebele,"-REGION|-ZONE$|ZONE$|-WEREDA|WEREDA$|-TOWN$|TOWN$|-RURAL$|RURAL$")) %>%
  relocate(Region, Zone, Wereda, Town, Rural)



export(raw_geo,file.path(dir_data_reference_cleanINT, "population_SNNPR_Oromiya_int.rds"))

?zoo::na.fill0
 


names(raw)
