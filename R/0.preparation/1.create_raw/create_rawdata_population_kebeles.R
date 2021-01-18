#to read pdf files in R
library(tabulizer)
source("set_up.R")


dir_kebeles_download = file.path(dir_data_reference_downloads, "Kebeles_population")

#lisf of all pdf files
#list.files(dir_kebeles_download)


#define pages where the tables are located in each file
#Note: Dire Wara and Harari have a different format (we'd need to extract them separately)
pages_with_tables = list(Addis_Ababa= c(172,173),
                         Affar = c(333:338) ,
                         Amhara = c(313:321),
                         Benishangu_Gumuz =c(284:290) ,
                         #Dire_Dawa = c(136,136), #check thisone has a diff format
                         Gambella = c(252:255),
                         #Harari = c(137, 137) , #also different
                         #National = ,
                         Oromiya = c(985:1074)
                         #SNNPR = ,
                         #Somali = ,
                         #Tigray
                         
                           )



#read pdfs and create tables at the region level  
list_of_tables = map(list.files(dir_kebeles_download), function(region){
  
  #get the region name
  region_name = str_remove(region, ".pdf")
  #print(region_name)
  
  #skip file with these names
  if(!region_name %in% c("National", "SNNPR", "Somali", "Tigray", "Dire_Dawa", "Harari")){
    
    print(region_name)
    
    #define path to connect import pdf
    pdf_raw = file.path(dir_kebeles_download, region)
    
    #define pages where pdfs have the tables
    pages = pages_with_tables[[region_name]]
    print(pages)
    
    #read table
    table_list = extract_tables(pdf_raw,
                                pages = pages,
                                area = list(c(100.05882,  14.98522, 741.23327, 558.12116)),
                                #output = "data.frame",
                                method = "decide"
    )
    
    #Append all tables in one dataframe
    table_df = do.call(rbind, table_list) %>%
      as.tibble() %>%
      mutate(Region = region_name)
    
    #return table in
    return(table_df)
    
  }
  
 
  
  
})
  
#append tables and export to raw data
raw_data = do.call(rbind, list_of_tables)
names(raw_data)<- c("Kebele", "Population", "Male", "Female", "Number of households", "Number of household units", "Region")

export(raw_data, file.path(dir_data_reference_raw, "Kebele_population_raw.rds"))



