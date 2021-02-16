#Functions to create lookup tables

#TO CREATE LOOK UP TABLES
my_lookup <- function(exdir, exfile, ...) {
  raw_geo %>%
    group_by(...) %>%
    select(...) %>%
    arrange(...) %>%
    slice(1) %>%
    export(file.path(exdir, exfile))
  
}