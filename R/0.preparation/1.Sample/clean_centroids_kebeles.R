

## Clean centroids kebeles

kebele_centroids <- import(file.path(dir_reference_raw, "kebele_centroids_raw.rds")) %>%
  janitor::clean_names() %>%
  mutate(across(matches("_code"), as.character),
         r_code = case_when(str_length(r_code) == 1 ~ paste0("0", r_code),
                            T ~ r_code),
         z_code = str_sub(z_code,start = 2, end = str_length(z_code)),
         w_code = str_sub(w_code, start = 4, end = 5)) %>%
  relocate(r_code, r_name, z_code, z_name, w_code, w_name)
aco
kebele_id()

View(kebele_centroids)

sort(names(kebele_centroids))