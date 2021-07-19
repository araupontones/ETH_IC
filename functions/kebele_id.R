#'creates a unique identifier for the kebele

kebele_id <- function(.data){
  .data %>%
  mutate(across(matches("_code"), function(x){case_when(x < 10 ~ paste0("0", as.character(x)),
                                                        T ~ as.character(x))}),
         kebele_id = paste0(r_code, z_code, w_code,t_code, k_code)
  )
}
