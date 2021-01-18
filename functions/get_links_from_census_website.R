#'Function to get all the links to download .pdf files at the kebele level

#' @param url link to Census website

get_links_from_census = function(
  
  url,
  ...
) {
  
  #define url to scrap
  url = url
  
  #read website
  webpage = read_html(url)
  
  #scrap all elements containing the links to download the data
  links_xml  = webpage %>%
    html_nodes("p > a:nth-child(1)") 
  
  #convert nodes to table
  links_table = bind_rows(lapply(xml_attrs(links_xml), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  
  return(links_table)
  
}