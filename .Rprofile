cli::cli_alert_success(glue::glue('Hey {Sys.getenv("USERNAME")}'))
cli::cli_alert_success("Welcome to Community conversations Ethiopia!!!")
cli::cli_alert_info("Have you installed all the packages for this project?")


#define libraries -------------------------------------------------------------

libraries <- c(
  #fonts
  "extrafont",
  
  #carpintery
  "gmdacr", "glue", "janitor",  "scales",
  
  #ggplot
  "grid","ggplot2","ggtextg", "ggrepel", "ggfittext",
  
  
  
  #tidyverse: 
  "tidyr", "stringr", "dplyr",
  
  #map
  
  "sf",
  
  #other
  "rio"
)

#Data directory ---------------------------------------------------------------
dir_data = "data"

#reference directory
dir_reference = file.path(dir_data, "0.reference")

  #dir_data_reference_downloads = file.path(dir_data_reference, "0.downloads")  
  dir_reference_raw = file.path(dir_reference, "1.raw")  
  dir_reference_clean = file.path(dir_reference, "2.clean")  
  #dir_data_reference_lkups = file.path(dir_data_reference, "2.look_ups")  



#sampe data directory
dir_sample= file.path(dir_data, "1.sample")


#others
dir_functions <- "functions"


#################################################################################

#load libraries ---------------------------------------------------------------
gmdacr::check.installed(libraries)

suppressWarnings({
  options(defaultPackages=c(getOption("defaultPackages"),
                            
                            libraries
  )
  )
})

gmdacr::load_functions(dir_functions)

