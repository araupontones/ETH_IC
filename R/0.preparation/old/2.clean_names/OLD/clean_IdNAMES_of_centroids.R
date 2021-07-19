#Clean identification variables of centroids
#the kebele_centroids_raw.rds was created at 0.download/download_KEBELES_polygons

source("set_up.R")

centroids_raw = import(file.path(dir_data_reference_raw, "kebele_centroids_raw.rds"))

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

