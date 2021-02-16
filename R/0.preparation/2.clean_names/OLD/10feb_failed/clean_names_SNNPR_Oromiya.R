#clean population file (sent by Nahom)

source("set_up.R")

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
         Wareda = create_region(Name, "-Wereda$|Wereda$")
         #Wareda = create_region(Name, "-Wereda$|Wereda$")
  ) %>%
  mutate_at(vars(Zone), str_trim) %>%
  ungroup() %>%
  #drop redundatn region and wereda observations
  filter(!str_detect(Name, "Region$|Region|-Zone$|Zone$|-Wereda$|Wereda$")) %>%
  relocate(Region, Zone, Wareda, Name) %>%
  arrange(Region, Zone, Wareda, Name)

clean_zone = raw_geo %>%
  mutate(Zone = str_replace(Zone, "Burayu Special", "Burayu"),
         Zone = str_replace(Zone, "Jimma Town Special", "Jimma Town"),
         Zone = str_replace(Zone, "Hawassa City Administration", "Awassa Town" )
  )



clean_wareda = clean_zone %>%
  mutate(Wareda = str_replace(Wareda, "Agaro\\/Town\\/", "Agaro"),
         Wareda = str_replace(Wareda, "Burayu\\/Town\\/", "Burayu"),
         Wareda = str_replace(Wareda, "Gimbi\\/Town\\/", "Gimbi"),
         Wareda = str_replace(Wareda, "Dolo\\/Town\\/", "Dolo Town"),
         Wareda = str_replace(Wareda, "Adama\\/Town\\/", "Adama Town"),
         Wareda = str_replace(Wareda, "Shambu\\/Town\\/", "Shambu Town"),
         Wareda = str_replace(Wareda, "Weliso\\/Town\\/", "Weliso Town"),
         
         Wareda = str_replace(Wareda, "Shashemene\\/Town\\/", "Shashemene Town"),          
         
         Wareda = str_replace(Wareda, "Bilo Nopha", "Bilonopa"),
         Wareda = str_replace(Wareda, "Hawassa City Administration", "Awassa Town" ),
         Wareda = str_replace(Wareda, "Basketo Special", "Basketo" ),
         Wareda = str_replace(Wareda, "Alaba Special", "Alaba" ),
         Wareda = str_replace(Wareda, "Jimma\\/Town\\/", "Jimma Spe Town" ),
         Wareda = str_replace(Wareda, "Hawassa Zuriya","Awasa Zuriya"),
         Wareda = str_replace(Wareda, "Debub Ari","South Ari"),
         Wareda = str_replace(Wareda, "Semen Ari","Gelila"),
         Wareda = str_replace(Wareda, "Debub Bench","Southern Bench"),
         Wareda = str_replace(Wareda, "Durame\\/Town\\/","Kedida Gamela")
         
         
         
         
  )


clean_kebele = clean_wareda %>%
  rename(Kebele = Name) %>%
  mutate(Total = as.numeric(Total),
         Kebele = str_to_title(Kebele),
         Kebele = str_trim(Kebele)
  ) %>%
  #drop rural and towns (because they aggregate)
  filter(!str_detect(Kebele, "-Rural$"),
         !str_detect(Kebele, "-Town$")
         ) %>%
  arrange(Region, Zone, Wareda, Kebele)

  


export(clean_kebele,file.path(dir_data_reference_cleanINT, "population_SNNPR_Oromiya_int.rds"))







names(raw)
