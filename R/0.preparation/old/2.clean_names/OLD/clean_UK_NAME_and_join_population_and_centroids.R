#join centroids and population datasets
#the intermediate datasets are generated at 2.clean_names/cleanIDNAMES_*

#note: all the variables ending with .y are the names in population that merge with centroids
#note: see the pdgs stored and reference/downloads to understand the joining strategy (it differs by region)
#after cleaning, the centroids and the population tables can be join using OBJECTID or UK_NAME_joint
# UK_NAME_joint differs by region (depending on the merging strategy)

source("set_up.R")


#Import data ------------------------------------------------------------------
centroids_int = import(file.path(dir_data_reference_cleanINT, "centroids_int.rds"))
population_int = import(file.path(dir_data_reference_cleanINT, "population_int.rds"))


## clean R name -------------------------------------------------------------------

  ##create lookup tables
r_lkps = check_diffs_(population_int,
                      centroid_data = centroids_int,
                      fuzzy_var = "R_NAME",
                      keep_vars = "R_NAME")

  ##fuzzy merge population and centrouds by R Name
R_NAME = fuzzy_left_join(
  r_lkps$pop,r_lkps$centr,
  by = c("R_NAME"),
  match_fun = c("R_NAME" = is_name_distance_jacard_4)
) %>%
  filter(!is.na(R_NAME.x))


  ##join with raw population
population_R =join_with_pop(to_data = population_int,
                            from_data = R_NAME,
                            join_var = "R_NAME")



##KK NAMES (only relevant for Addis Abeba)-------------------------------------

  ##create lookup tables of KK names
kk_lkps = check_diffs_(population_R,
                       centroid_data = centroids_int,
                       fuzzy_var = "KK_NAME",
                       keep_vars = "KK_NAME")

  ## fuzzy join population and centroides 
KK_NAME = fuzzy_left_join(
  kk_lkps$pop,kk_lkps$centr,
  by = c("KK_NAME"),
  match_fun = c("KK_NAME" = is_name_distance_jacard_4)
) %>%
  filter(!is.na(KK_NAME.x))


  #join with R populations to bring KK_NAME
population_K =join_with_pop(to_data = population_R,
                            from_data = KK_NAME,
                            join_var = "KK_NAME")  %>%
  group_by(R_NAME.y) %>%
  mutate(KK_NAME.y = zoo::na.locf0(KK_NAME.y))
  


##UK_NAMES =-----------------------------------------------------------------------------

#create UK_NAMES_join (it depends by region, see the pdfs saved in reference/downloads)
# the UK_NAMES_join for the centroides is created in 2.clean_names/clean_idNAMES_of_centroids.R

population_UK = population_K %>%
  #filterout rows that serve as geographic identification in the .pdfs
  filter(!str_detect(Kebele, "-SUB CITY") #Sub city serves to identify kebeles for Addis
         ) %>% 
  #create UK_NAME_join, this depends on the region
  mutate(UK_NAME_join = case_when(
    ##FOr Addis Abeba:
    R_NAME.y == "Addis Abeba" ~ if_else(
      str_detect(Kebele, "KEBELE"), 
      paste(KK_NAME.y, UK_NAME), 
      Kebele),
    #For the rest
    T ~ UK_NAME 

   ),
   #clean UK_NAME_join (also clean in 2.clean_names/clean_idNAMES_centroids.R)
   UK_NAME_join = str_to_title(UK_NAME_join),
   UK_NAME_join = gsub('(?<=[a-z])([0-9])(?=.)', ' \\1', UK_NAME_join, perl = T),
   #clean UK_name_join
   UK_NAME_join = str_replace(UK_NAME_join,"Arada 06", "Arada 6" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Chirkos 13/14", "Cherkos 13/14" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Chirkos 15/16", "Cherkos 15/16" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Yeka 19", "19/Lokea" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Yeka 20/21", "20/21/Abado Ena Tafou" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Yeka 16/17/18", "16/17/18/Ankorcha" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Nefas Silk Lafto 16/17", "Nafasilk Lafto 16/17" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Nefas Silk Lafto 01\\(Hana,Lebu,Dertu\\)", "Nafasilk Lafto 01" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Kolfe Keraniyo 06", "Kolfe Keraniyo 01/06" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Kilinito,Feche Koye\\(17/18/19\\)", "Kilinito, Feche Roye" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Gelangora", "Golanigora" ),
   UK_NAME_join = str_replace(UK_NAME_join,"Gulele 10/18", "Gulele 18" )
   
   
  ) %>%
  
  #keep ony observations for which the strategy has bee tested
  filter(R_NAME.y %in% c("Addis Abeba"))


  #check differences of UK_NAME_join by region
uk_lkps = check_diffs_(population_UK,
                       centroid_data = filter(centroids_int, R_NAME == "Addis Abeba"),
                       fuzzy_var = "UK_NAME_join",
                      keep_vars = "UK_NAME_join")


#join fuzzy names by UK_NAME_join
UK_NAME = fuzzy_left_join(
  uk_lkps$pop,uk_lkps$centr,
  by = c("UK_NAME_join"),
  match_fun = c("UK_NAME_join" = is_name_distance_0)
) 

#join with populaiton UK
population_final =join_with_pop(to_data = population_UK,
                            from_data = UK_NAME,
                            join_var = "UK_NAME_join") %>%
  select(-UK_NAME_join.y) %>%
  ##keep only variables from population and OBJECTID to be able to merge with centroids
  left_join(select(centroids_int,UK_NAME_join, OBJECTID, geometry),
                   by = c("UK_NAME_join")) 
 

   
#so far only includes observations from Addis
export(population_final, file.path(dir_data_reference_clean, "population_and_centroids_clean.rds"))




# 
# 
# 
# #clean population raw to match IDs with centroids
# 
# population_names = population_raw %>%
#   rename(R_NAME = Region) %>%
#   #clean Region name to match centroids
#   mutate(R_NAME = str_replace_all(R_NAME, "_", " "),
#          R_NAME = str_replace(R_NAME, "Ababa", "Abeba"),
#          R_NAME = str_replace(R_NAME, "Affar", "Afar"),
#          R_NAME = str_replace(R_NAME, "Benishangu Gumuz", "Benishangul Gumuz"),
#   
#   #Define Z_NAME 
#   Z_NAME = case_when(str_detect(Kebele, "-SUB CITY|ZONE") ~ Kebele,
#                      T ~ NA_character_),
#   Z_NAME = str_remove(Z_NAME, "-SUB CITY"),
#   Z_NAME = str_to_title(Z_NAME),
#   Z_NAME = str_replace(Z_NAME, "Zone [0-9]", paste0(str_sub(Z_NAME, 1,4)," 0", str_sub(Z_NAME,6,6) )),
#   Z_NAME = str_remove_all(Z_NAME, "-Zone| Zone"),
#   Z_NAME = str_replace_all(Z_NAME, "-", " "),
#   Z_NAME = str_replace(Z_NAME, "Gojam","Gojjam"),
#   Z_NAME = str_replace(Z_NAME, "South Wello", "South Wolo"),
#   Z_NAME = str_replace(Z_NAME, "North Wello", "North Wollo"),
#   Z_NAME = str_replace(Z_NAME, "Akaki Kality", "Akaki Kaliti"),
#   Z_NAME = str_replace(Z_NAME, "South Gondar", "South Gonder"),
#   Z_NAME = str_replace(Z_NAME, "Kirkos", "Chirkos"),
#   Z_NAME = str_replace(Z_NAME, "Asosa", "Assosa"),
#   
#   #Define W_NAME 
#   W_NAME = case_when(str_detect(Kebele, "-WEREDA") ~ Kebele,
#                      T ~ NA_character_),
#   W_NAME = str_remove(W_NAME, "-WEREDA"),
#   W_NAME = str_to_title(W_NAME),
#   W_NAME = str_replace(W_NAME, "Asayita", "Asayta"),
#   W_NAME = str_replace(W_NAME, "Chifera", "Chefera"),
#   W_NAME = str_replace(W_NAME, "Erabti", "Erebti"),
#   W_NAME = str_replace(W_NAME, "Berahile", "Berehale"),
#   W_NAME = str_replace(W_NAME, "Argoba Special", "Argoba Liyu"),
#   W_NAME = str_replace(W_NAME, "Adi Arkay", "Adiarikay"),
#   W_NAME = str_replace(W_NAME, "Janamora", "Jan Amora"),
#   W_NAME = str_replace(W_NAME, "Mirab Armachiho", "Mirab Armacho"),
#   W_NAME = str_replace(W_NAME, "Lay Armacho", "Lay Armachew"),
#   W_NAME = str_replace(W_NAME, "Gonder Zuriya", "Gondar Zuriya"),
#   W_NAME = str_replace(W_NAME, "Mirab Belesa", "Merab Belesa"),
#   W_NAME = str_replace(W_NAME, "Gonder/Town/", "Gondar Town"),
#   W_NAME = str_replace(W_NAME, "Tach Armachoho", "Tach Armacho"),
#   W_NAME = str_replace(W_NAME, "Debre Brehan/Town/", "Debrebrehan Town"),
#   W_NAME = str_replace(W_NAME, "Debretabor/Town/", "Debretabor"),
#   W_NAME = str_replace(W_NAME, "Mirab Esta", "Mirab Este"),
#   W_NAME = str_replace(W_NAME, "Albuko", "Alibuko"),
#   W_NAME = str_replace(W_NAME, "Legambo", "Legamibo"),
#   W_NAME = str_replace(W_NAME, "Sayint", "Sayinit"),
#   W_NAME = str_replace(W_NAME, "Kombolcha/Town/", "Kombolcha Town"),
#   W_NAME = str_replace(W_NAME, "Dessie/Town/", "Dese Town"),
#   W_NAME = str_replace(W_NAME, "Legahida", "Legehida"),
#   W_NAME = str_replace(W_NAME, "Menz Gera Midir", "Menz Gera Meder"),
#   W_NAME = str_replace(W_NAME, "Antsokiyana Gemza", "Antsokiya Gemza"),
#   W_NAME = str_replace(W_NAME, "Efratana Gidim", "Yifratana Gidim"),
#   W_NAME = str_replace(W_NAME, "Mida Woremo", "Mida Oromo"),
#   W_NAME = str_replace(W_NAME, "Mojana Wodera", "Mojana Wedera"),
#   W_NAME = str_replace(W_NAME, "Angolala Tera", "Angolelana Tera"),
#   W_NAME = str_replace(W_NAME,  "Asagirt", "Asagert" ),
#   W_NAME = str_replace(W_NAME, "Hagere Mariam " , "Hagere Mariam" ),
#   W_NAME = str_replace(W_NAME, "Minjarna Shenkora" , "Minjar Shenkora" ),
#   W_NAME = str_replace(W_NAME, "Menz Keya Gebreal"  , "Mez Keya Geberal"),
#   W_NAME = str_replace(W_NAME,  "Saya Deberna Wayu"   , "Saya Debirna Wayu"),
#   W_NAME = str_replace(W_NAME,  "Hulet Ej Enese"   , "Huletej Enese"),
#   W_NAME = str_replace(W_NAME,  "Enebse Sar Midir"   , "Enebise Sar Midir" ),
#   W_NAME = str_replace(W_NAME,   "Enarj Enawga"   ,  "Enarj Enawuga" ),
#   W_NAME = str_replace(W_NAME,    "Debay Tilatgin"    ,  "Dibay Tilatgin"),
#   W_NAME = str_replace(W_NAME,"Debre Elias" , "Debere Elias"  ),
#   W_NAME = str_replace(W_NAME, "Debere Markos/Town/"   , "Debre Markos Town"  ),
#   W_NAME = str_replace(W_NAME, "Gonjqolola"    ,  "Gonji Kolela"   ),
#   W_NAME = str_replace(W_NAME, "Finote Selam/Town/","Finote Selam Town"),
#   W_NAME = str_replace(W_NAME, "Asosa","Assosa")
# 
#     
#  
#   
#   
#   
#   
#   
#  
#   
#   
#   
#           
#   # KK_NAME = case_when(!str_detect(Kebele, "KEBELE") ~ Kebele,
#   #                     T ~ NA_character_),
#   # KK_NAME = str_replace_all(KK_NAME, "-", " "),
#   # KK_NAME = str_remove_all(KK_NAME, "SUB CITY"),
#   # KK_NAME = str_to_title(KK_NAME),
#   # 
#   # KK_NAME = str_replace(KK_NAME, "Akaki Kality", "Akaki Kaliti"),
#   # KK_NAME = str_replace(KK_NAME,"Kilinito,Feche Koye\\(17\\/18\\/19\\)", "Akaki Kaliti"),
#   # KK_NAME = str_replace(KK_NAME,"Kilinito,Feche Koye\\(17\\/18\\/19\\)", "Akaki Kaliti")
#   # 
#   # KK_NAME = str_trim(KK_NAME)
#                               
#          ) %>%
#   #get IDs from centroids
#   mutate(R_CODE = centroids_names$R_CODE[match(.$R_NAME, centroids_names$R_NAME)])
# 
# #sort(unique(population_names$Z_NAME))
# #sort(unique(centroids_names$Z_NAME))
# 
# #sort(unique(population_names$W_NAME))
# #sort(unique(centroids_names$W_NAME))
#  
# 
# setdiff(unique(population_names$W_NAME),unique(centroids_names$W_NAME) )
# 
# sort(unique(centroids_names$W_NAME[centroids_names$R_NAME =="Amhara"]))
# 
# sort(unique(centroids_names$W_NAME[str_detect(centroids_names$W_NAME, "Jarte")]))
