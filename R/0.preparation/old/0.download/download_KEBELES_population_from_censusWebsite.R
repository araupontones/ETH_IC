
# to scrap tables fro census
library(xml2)
library(rvest)
source("set_up.R")



for(script in list.files("functions", recursive = T)){
  
  source(file.path("functions",script))
}

#1. get all the links to download the data -------------------------------------


links = get_links_from_census(url = "https://www.statsethiopia.gov.et/census-2007-2/")


#2.define directory to export the pdfs

dir_output = file.path(dir_data_reference_downloads, "Kebeles_population")

if(!file.exists(dir_output)){
  
  dir.create(dir_output)
  
}


#3. dowload all pdfs contained in the census page

for(link in links$href){
  
  kebele_name = str_remove_all(str_extract(link,"[^\\/]*$"), ".pdf|_Statistical|Statistical_|Population-and-Housing-Census-2007-|-1")
  kebele_pdf = paste0(kebele_name, ".pdf")
  
  #path of the pdf file
  kebele_pdf_dir = file.path(dir_output, kebele_pdf)
  
  
  
  # Download pdf from website
  download_pdf = GET(link)
  
  print(paste("Downloading", kebele_pdf, "Status:", download_pdf$status_code))
  
  
  #open connection to write data in dir_output
  filecon <- file(kebele_pdf_dir, "wb") 
  #write data contents to download file!! 
  writeBin(download_pdf$content, filecon) 
  #close the connection
  close(filecon)
  
}


