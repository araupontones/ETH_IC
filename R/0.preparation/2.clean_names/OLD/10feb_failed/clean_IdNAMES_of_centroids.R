#Clean identification variables of centroids
#the kebele_centroids_raw.rds was created at 0.download/download_KEBELES_polygons

source("set_up.R")

centroids_raw = import(file.path(dir_data_reference_raw, "kebele_centroids_raw.rds"))


centroids_names = centroids_raw %>%
  rename(Region = R_NAME,
         Zone = Z_NAME,
         Wareda = W_NAME,
         Kebele = RK_NAME
  ) %>%
  mutate_at(vars(Zone, Wareda), str_to_title) %>%
  mutate(
    #Clen Zone
    Wareda = str_remove_all(Wareda, "Wereda"),
    Zone = str_remove_all(Zone, " Zone| S. Z."),
    Zone = str_replace(Zone,"Jimma Spe Town", "Jimma Town"),
    Zone = str_replace(Zone,"Qeleme Wellega", "Kelem Wellega"),
    

    Zone = case_when(Zone== "Basketo" & Wareda == "Basketo" ~ "Dawuro",
                     Zone == "Amaro Special" ~ "Bench Maji",
                     Wareda %in% c("Konta  Special") ~ "Dawuro",
                     Wareda == "Alaba" ~ "Siliti" ,
                     Wareda %in% c("Derashe Special", "Konso Special", 
                                   "Burji Special", "Yeme Special") ~ "Bench Maji",
                     T ~ Zone),
    #Clean Kebele
    Kebele = str_to_title(Kebele)
   
    
    ) %>%
  mutate_at(vars(Zone, Wareda, Kebele), str_trim) %>%
  relocate(Region, Zone, Wareda, Kebele) %>%
  ##Keep only relevant regions
  filter(Region %in% c("SNNP","Oromiya" )) %>%
  arrange(Region, Zone, Wareda, Kebele)

unique(centroids_names$Kebele)

export(centroids_names,file.path(dir_data_reference_cleanINT, "centroids_int.rds"))



#clean ID names 

names(centroids_raw)



#filter(R_NAME %in% c("SNNP","Oromiya" ))
#Clean ID variables of Centroids -------------------------------------------------------------
names(centroids_names)
centroids_names = centroids_raw %>%
  
  mutate(R_NAME = str_remove_all(R_NAME, "_"),
         R_NAME = str_to_title(R_NAME),
         R_NAME = str_replace(R_NAME, "  ", " "),
         R_NAME = str_replace(R_NAME, "Amahara", "Amhara"),
         
         #CLEAN kk_NAME
         KK_NAME = str_to_title(KK_NAME),
         
         #clean Zone Name
         
         
         # Z_NAME = case_when(Z_NAME == "01" ~ KK_NAME,
         #                    T ~ Z_NAME),
         Z_NAME = str_to_title(Z_NAME),
         Z_NAME = str_trim(Z_NAME),
         Z_NAME = str_replace_all(Z_NAME, "  ", " "),
         
         #CLEAN Wereda
         W_NAME = str_to_title(W_NAME),
         W_NAME = str_replace_all(W_NAME, "  ", " "),
         
         UK_NAME_join = case_when(
           #For Addis 
           R_NAME == "Addis Abeba" ~ 
             if_else(str_detect(UK_NAME, "[a-zA-Z]"), 
                     UK_NAME,
                     paste(KK_NAME, UK_NAME)
             ),
           T ~ UK_NAME
         ),
         
         UK_NAME_join = str_to_title(UK_NAME_join),
         UK_NAME_join = str_remove(UK_NAME_join, "Kebele"),
         UK_NAME_join = str_replace(UK_NAME_join, "Nefasilk", "Nefas Silk"),
         UK_NAME_join = gsub('(?<=[a-z])([0-9])(?=.)', ' \\1', UK_NAME_join, perl = T),
         UK_NAME_join = str_replace(UK_NAME_join, "  "," ")
         
  )

unique(centroids_names$UK_NAME_join)


export(centroids_names,file.path(dir_data_reference_cleanINT, "centroids_int.rds"))

