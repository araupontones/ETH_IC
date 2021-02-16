#set up

#lLOAD LIBRARIES ----------------------------------------------------------------
  library(pacman)
  
  p_load(tidyverse, sf, httr, rio, fuzzyjoin, stringdist)



#DEFINE PATHS-- ----------------------------------------------------------------


#Data directory ---------------------------------------------------------------
  dir_data = "data"
    
    #reference directory
    dir_data_reference = file.path(dir_data, "0.reference")
      
      dir_data_reference_downloads = file.path(dir_data_reference, "0.downloads")  
      dir_data_reference_raw = file.path(dir_data_reference, "1.raw")  
      dir_data_reference_lkups = file.path(dir_data_reference, "2.look_ups")  
      
      dir_data_reference_cleanINT = file.path(dir_data_reference, "2.clean_intermediate")  
      dir_data_reference_clean = file.path(dir_data_reference, "3.clean")  
      
    #sampe data directory
      dir_data_sample= file.path(dir_data, "1.sample")
    

# functions --------------------------------------------------------------------
      
      dir_functions = "functions"
      
      #run functions
    
     
        lapply(list.files(dir_functions, recursive = T), function(file){
        
        #path of function
        runthis = file.path(dir_functions, file)
        
        #load function
        source(runthis)
      } 
      ) 
     
      
        