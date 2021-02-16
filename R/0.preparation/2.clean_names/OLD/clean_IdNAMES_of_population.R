#Clean identification variables of population
#the population_raw.rds was created at 1.create_raw/crate_raw_data_population_kebeles

source("set_up.R")

population_raw = import(file.path(dir_data_reference_raw, "kebele_population_raw.rds"))


population_names = population_raw %>%
  rename(R_NAME = Region) %>%
  #clean Region name to match centroids
  mutate(R_NAME = str_replace_all(R_NAME, "_", " "),
         
         ##KK_NAME ------------------------------------------------------------
         KK_NAME = case_when(str_detect(Kebele, "-SUB CITY") ~ Kebele,
                            T ~ NA_character_),
         KK_NAME = str_remove(KK_NAME, "-SUB CITY"),
         KK_NAME = str_to_title(KK_NAME),
         
         ##WEREDA NAME -------------------------------------------------------
         W_NAME = case_when(str_detect(Kebele, "-WEREDA") ~ Kebele,
                            T ~ NA_character_),
         W_NAME = str_remove(W_NAME, "-WEREDA"),
         W_NAME = str_to_title(W_NAME),
         W_NAME = str_replace(W_NAME,"/Town/", " Town"),
         
         ##ZONE ----------------------------------------------------------------
         Z_NAME = case_when(str_detect(Kebele, "-SUB CITY|ZONE") ~ Kebele,
                            T ~ NA_character_),
         
         Z_NAME = str_to_title(Z_NAME),
         Z_NAME = str_replace(Z_NAME, "Zone [0-9]", paste0(str_sub(Z_NAME, 1,4)," 0", str_sub(Z_NAME,6,6) )),
         Z_NAME = str_remove_all(Z_NAME, "-Zone| Zone"),
         Z_NAME = str_replace_all(Z_NAME, "-", " "),
         
         ##UK --------------------------------------------------------------------
         UK_NAME = str_remove(Kebele, "KEBELE "),
         UK_NAME = str_to_title(UK_NAME)
  )

unique(population_names$KK_NAME)      

export(population_names,file.path(dir_data_reference_cleanINT, "population_int.rds"))
       