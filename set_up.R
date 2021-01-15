#set up

#lLOAD LIBRARIES ----------------------------------------------------------------
  library(pacman)
  
  p_load(tidyverse, sf, httr, rio)


#DEFINE PATHS-- ----------------------------------------------------------------


#Data directory ---------------------------------------------------------------
  dir_data = "data"
    
    #reference directory
    dir_data_reference = file.path(dir_data, "0.reference")
      
      dir_data_reference_raw = file.path(dir_data_reference, "1.raw")  
      dir_data_reference_clean = file.path(dir_data_reference, "2.clean")  
      
    #sampe data directory
      dir_data_sample= file.path(dir_data, "1.sample")
    