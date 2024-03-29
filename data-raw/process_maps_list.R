# Process maps list ------------------------------------------------------------

marawi_maps_list <- openmarawi::marawi_maps_ls()

usethis::use_data(marawi_maps_list, overwrite = TRUE, compress = "xz")
