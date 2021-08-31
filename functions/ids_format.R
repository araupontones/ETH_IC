ids_format <- function(x){
  
  y = case_when(str_count(x) == 1 ~ paste0("0", x),
                T ~ x)
  
  return(y)
  
}