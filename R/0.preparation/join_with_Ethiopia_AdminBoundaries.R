#source("set_up.R")
source("functions/my_fuzzy.R") #function to fuzzy merge
exdir <- file.path(dir_reference, "3.joint_with_shape")
library(fuzzyjoin)
library(stringdist)

#define paths ------------------------------------------------------------
excel_kebeles <- file.path(dir_reference_clean, "treated_kebeles_official.xlsx")
shapefile_raw <- file.path(dir_data,"Ethiopia_AdminBoundaries-shp")




#read data ---------------------------------------------------------------------
sf <-sf::read_sf(shapefile_raw)
treated_kebeles <- import(excel_kebeles)


#names(sf)
#check if UK-CODE uniquely identifies

  sf %>%
    #keep regions of interest ---
  filter(`R_NAME` %in% c("SNNP", "OROMIYA", "Oromiya")) %>%
  get_dupes(`RK_CODE`) %>%
  nrow() #0 duplicates
  
 



#Make variables consistent across files------------------------------------------





t_kebeles <- treated_kebeles %>%
  mutate(across(matches("CODE"), as.character),
         across(matches("CODE"), ids_format),
         `K-CODE` = case_when(str_count(`K-CODE`)==1~ paste0("0", `K-CODE`),
                              T ~ `K-CODE`
         ),
         #this is how the ID is defined to avoid duplicates
         ID = paste0(`R-CODE`, `Z-CODE`,`W-CODE`,`T-CODE`,`K-CODE`),
         WCODE = paste0(`R-CODE`, `Z-CODE`,`W-CODE`),
         #var to fuzzy merge
         fuzzy_var = paste0(ID,WOREDA, KEBELE),
         name = paste(WOREDA, KEBELE)
  )



#nrow(t_kebeles) #518


#names(sf_clean)
sf_clean <- sf %>%
  #make core vars consistent with treated kebeles
  rename_at(vars(ends_with("CODE")), function(x){str_replace(x, "_","-")}) %>%
  mutate(across(matches("CODE"), as.character)
  ) %>%
  #names consistent with kebeles treated
  rename(
    WOREDA = W_NAME,
    REGION = R_NAME,
    ZONE = Z_NAME,
    KEBELE = RK_NAME,
    `K-CODE` = `RK-CODE`) %>%
  #keep regions of interest ---
  filter(REGION %in% c("SNNP", "OROMIYA", "Oromiya")) %>%

  mutate(
    #this uniquely identifies the kebeles in shapefile
        ID = `K-CODE`,
        #make id system consistent---
        `Z-CODE` = str_sub(`Z-CODE`,2,-1),
         `W-CODE` = str_sub(`W-CODE`,4,-1),
         `K-CODE` = str_sub(`K-CODE`,7,-1),
         #`K-CODE` = str_sub(`K-CODE`,6,-1),
         `R-CODE` = paste0("0", `R-CODE`),
         across(matches("K-CODE"), ids_format),
         #An ID cannot be created because T-CODE is missin
         WCODE = paste0(`R-CODE`, `Z-CODE`,`W-CODE`),
        #clean kebele name to ease the fuzzy merge
         KEBELE = str_trim(KEBELE),
         KEBELE = str_to_title(KEBELE),
         #var for fuzzysasd
        WOREDA = case_when(WOREDA == "Abeshege" ~ "Abashige",
                                  WOREDA == "Damot Pulasa" ~ "Damotpulasa",
                                  WOREDA == "Kindo Koyisha" ~ "Kindo Koisha",
                                  WOREDA == "Kacha Bira" ~ "Kachabira",
                                  WOREDA == "Bolossa Bonibe" ~ "Bolosobombe",
                                  WOREDA == "Hadaro Tunito" ~ "Hadaro Tunto",
                                  WOREDA == "Kokosa" ~ "Kokossa",
                                  WOREDA == "Daro lebu" ~ "Daro Labu",
                                  WOREDA == "Shashago" ~ "Shashego",
                                  WOREDA == "Midega Tole" ~ "Midaga Tola",
                                  WOREDA == "Kerisa" ~ "Kersa",
                                  WOREDA == "Deksis" ~ "Diksis",
                                  WOREDA == "Enkelo Wabe" ~ "Honkolo Wabe",
                                  WOREDA == "Guba Qoricha" ~ "Guba Koricha",
                                  WOREDA == "Haromaya" ~ "Haremaya",
                                  WOREDA == "Silite" ~ "Silti",
                                  WOREDA == "Dalocha" ~ "Dallocha",
                           WOREDA == "Anigacha" ~ "Angecha",
                           WOREDA == "Zeway Dugda" ~ "Zewaydugda",
                           WOREDA == "Wilbareg" ~ "Hulbareg",
                           WOREDA == "BOLOSSA SORE" ~ "Boloso Sore",
                           WOREDA == "BERBERE" ~ "Berbere",
                           WOREDA == "ENEMOR ENER" ~ "Enemor Ena Ener",
                           WOREDA == "Damot Woyide" ~ "Damotgale",
                           WOREDA == "DAMOT GALE" ~ "Damotgale",
                           WOREDA == "Alaba" ~ "Alaba Special",
                           
                           
                             
                                  T ~ WOREDA),
         fuzzy_var = paste0(ID,WOREDA, KEBELE),
        name = paste(WOREDA, KEBELE)
        ) %>%
  #filter(WOREDA == "Kerisa") %>%
  select(ID ,REGION, `R-CODE`, ZONE, `Z-CODE`, WOREDA, `W-CODE`, `WCODE`, `KEBELE`, `K-CODE`, fuzzy_var, name) 
  #clean WOREDA to ease the fuzy join
 

  
  
  
  

#try to join ---------------------------------------------------------------------
first_merge =  my_fuzzy(left_data = t_kebeles, right_data  = sf_clean, by = "fuzzy_var",
              match_fun = is_name_distance_jacard_5 ) %>%
  filter(!is.na(fuz_var.y_census)) %>%
  select(starts_with("ID"),starts_with("WOREDA"), starts_with("KEBELE"), starts_with("name")) %>%
  #calculate distance between names
  mutate(dist = stringdist(name_field, name_census),
         dist_jacard = stringdist(name_field, name_census, method = "jaccard"),
         dist_jw = stringdist(name_field, name_census, method = "jw"),
         
         )


#keep good fits 1 ==============================================================
first_merge_clean <- first_merge %>%
  group_by(ID_field) %>%
  filter(dist == min(dist)) %>%
  #count duplicated
  mutate(records = max(n())) %>%
  filter(dist <=3 & records == 1)

#View(first_merge_clean) 
#nrow(first_merge_clean)
###327 succes!
export(first_merge_clean, file.path(exdir, "first_merge.rds"))


#second fits (jacard method) ===================================================================

#keep good fits 2 ---------------------------------------------------------
second_merge_clean <- first_merge %>%
  anti_join(first_merge_clean, by = "ID_field") %>%
  group_by(ID_field) %>%
  filter(dist_jacard == min(dist_jacard)) %>%
  #count duplicated
  mutate(records = max(n())) %>%
  filter(dist_jacard <=.25 & records == 1)

#View(second_merge_clean)
#nrow(second_merge_clean)
#87 success!!!
export(second_merge_clean, file.path(exdir, "second_merge.rds"))


#keep good fits (jw methor) this doesnt work! =======================


# third_merge_clean <- first_merge %>%
#   anti_join(first_merge_clean, by = "ID_field") %>%
#   anti_join(second_merge_clean, by = "ID_field") %>%
#   group_by(ID_field) 
#   filter(dist_jw == min(dist_jw)) %>%
#   #count duplicated
#   mutate(records = max(n())) 
#   filter(dist_jacard <=.25 & records == 1)
# 
#   View(third_merge_clean)


#export merged and unmerged ====================================================
merged_kebeles <- rbind(first_merge_clean, second_merge_clean)

unmerged_kebeles <- t_kebeles %>%
  anti_join(merged_kebeles, by = c("ID" = "ID_field"))


export(merged_kebeles, file.path(exdir, "merged_shape_treatment.rds"))
export(unmerged_kebeles, file.path(exdir, "unmerged_kebeles.rds"))


