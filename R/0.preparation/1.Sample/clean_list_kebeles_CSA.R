#clean list of kebeles given by CSA (sent by Daniel Nigatu on July 2021)
#input: data/0.reference/1.raw/Lists of Kebeles Participated in CCP.xlsx
#output


#read administrative kebeles
kebeles <- openxlsx::read.xlsx(file.path(dir_reference_raw, "Lists of Kebeles Participated in CCP.xlsx"), cols = c(1:15)) %>%
  janitor::clean_names()





kebeles.1 <- kebeles %>%
  kebele_id() %>% #created in functions
  relocate(sn, r_code, region, z_code, zone, w_code, woreda)



export(kebeles.1, file.path(dir_reference_clean, "kebeles_cc.rds"))

kebeles_admin <- rio::import(file.path(dir_reference_clean, "kebeles_cc.rds"))

#obs
nrow(kebeles)




#Number of kebeles per zone
zones <- kebeles %>% 
  tabyl(region, zone) %>%
  pivot_longer(-region,
               names_to = "zone",
               values_to = "obs") %>%
  filter(obs > 1)



