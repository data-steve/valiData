pacman::p_install_gh("steventsimpson/valiData")

clDev::source_R_folder("~/Documents/repos/github_packages/valiData/R/")

wkd <- cl::l_drive_go("FTP/williamjewell")

file <- cl::go(wkd,"Courses/OrgUnit/Org-Unit.csv")

map_wkd <- cl::l_drive_go("/swiper/DataScience/data_quality/core_data_mapping")

col_map_loc <- cl::go(map_wkd,"column_mapping.rds")
col_map <- readRDS(col_map_loc)[["OrgUnit"]]

map_loc <- cl::go(map_wkd,"core_data_map.rds")
core_data_map <- readRDS(map_loc)[["OrgUnit"]]


#valiData::valiData(path=wkd, core_data_map=core_data_map, column_map=col_map, delete = TRUE)

validate_file(file, core_data_map=core_data_map, column_map=col_map)



