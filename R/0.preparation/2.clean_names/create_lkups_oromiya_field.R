#clean treated cc from Oromiya file (sent by Liyunet)

source("set_up.R")
library(readxl) #to read excel sheets

source("functions/to_create_lookups.R")


#define paths ------------------------------------------------------------
file = file.path(dir_data_reference_raw, "Targeted CC Oromia_17.02.xlsx")
sheets = excel_sheets(file)




#read raw data ------------------------------------------------------------------
read_sheets = lapply(sheets, function(sheet){import(file, sheet = sheet) %>%
    mutate(Region = "Oromiya", 
           Zone = sheet) %>%
    rename_at(vars(ends_with("ebele")), ~ "Kebele") %>%
    rename_at(vars(ends_with("osition")), ~ "Position") %>%
    rename_at(vars(ends_with("No")), ~ "S_No") %>%
    rename_at(vars(starts_with("Tele")), ~ "Telefone") %>%
    rename_at(vars(ends_with("qaa")), ~ "Name") %>%
    rename(Wereda = Woreda) %>%
    mutate(S_No = str_trim(S_No),
           S_No =  as.numeric(str_remove_all(S_No, "\\.")))
  
  })



data_raw = do.call(plyr::rbind.fill, read_sheets)

unique(data_raw$S_No)
names(data_raw)




  
#clean raw_data ----------------------------------------------------------------

raw_geo = data_raw %>%
  mutate_at(vars(Wereda, Kebele), function(x){str_trim(x)}) %>%
  mutate_at(vars(Region, Zone, Wereda, Kebele), function(x){str_to_lower(x)}) %>%
  mutate_at(vars(Wereda, Kebele), function(x){case_when(x %in% c('“', '>>', 'NA', "‘’", "zone", "woreda", "region", "reg" ) ~ NA_character_,
                                                T ~ x)}) %>%
  group_by(Region, Zone) %>%
  mutate(Wereda = zoo::na.locf0(Wereda)) %>%
  ungroup() %>%
  group_by(Region, Zone, Wereda) %>%
  mutate(Kebele = zoo::na.locf0(Kebele)) %>%
  ungroup() %>%
  group_by(Region, Zone, Wereda, Kebele) %>%
  #slice(1) %>%
  mutate(S_No = as.numeric(S_No)) %>%
  arrange(Region, Zone, S_No) %>%
  relocate(S_No,Region, Zone, Wereda, Kebele) %>%
  #Clean Weredas 
  mutate(Wereda = str_replace(Wereda, "badannoo|baddannoo","bedeno" ),
         Wereda = str_replace(Wereda, "h/mayaa", "haromaya"),
         Wereda = str_replace(Wereda, "g/odaa|a/g/odaa", "gole oda"),
         Wereda = str_replace(Wereda, "m/tolaa|midh /tolaa", "midega tola"),
         Wereda = str_replace(Wereda, "seka", "seka chekorsa"),
         Wereda = str_replace(Wereda, "diksis", "deksis"),
         Wereda = str_replace(Wereda, "h/wabe", "enkelo wabe"),
         Wereda = str_replace(Wereda, "z. dugda", "zeway dugda"),
         Wereda = str_replace(Wereda, "adodola", "dodola"),
         Wereda = str_replace(Wereda, "d/mena", "delo mena"),
         Wereda = str_replace(Wereda, "gasara", "gasera"),
         Wereda = str_replace(Wereda, "bedessa t", "bedessa town"),
         Wereda = str_replace(Wereda, "chiro", "chiro zuria"),
         Wereda = str_replace(Wereda, "d/lebu", "daro lebu"),
         Wereda = str_replace(Wereda, "g/koricha", "guba qoricha"),
         Wereda = str_replace(Wereda, "gursuum", "gursum"),
         Wereda = str_replace(Wereda, "dadar", "deder"),
         Wereda = str_replace(Wereda, "fadiis", "fedis"),
         Wereda = str_replace(Wereda, "hetosa", "hitosa"),
         Wereda = str_replace(Wereda, "m/tola", "midega tola"),
         Wereda = str_replace(Wereda, "meeta", "meta"),
         Wereda = str_replace(Wereda, "nagele", "arsi negele"),
         
         
         
         #Kebeleles
         
         Kebele = case_when(Wereda == "kokossa" & Kebele == "1" ~  "kebele 01",
                            Wereda == "seka chekorsa" & Kebele == "1" ~ "kebele 01",
                            Wereda == "seka chekorsa" & Kebele == "2" ~ "kebele 02",
                            Wereda == "daro lebu" & Kebele == "2" ~ "kebele 02",
                            T ~ Kebele)
         
         
         )
  



#export
#export raw_geo 

raw_geo %>% export(file.path(dir_data_reference_lkups, "Oromiya/field.xlsx"))




#lookup for zones
my_lookup(exdir =dir_data_reference_lkups, exfile =  "Oromiya/zones_field.xlsx" , Region, Zone)

#Weredas
my_lookup(exdir =dir_data_reference_lkups, exfile =  "Oromiya/weredas_field.xlsx" , Region, Zone, Wereda)


my_lookup(exdir= dir_data_reference_lkups, exfile = "Oromiya/kebeles_field.xlsx", Region, Zone, Wereda, Kebele)


