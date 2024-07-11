# PROJECT: vet-dahomey
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Configuration
# REF ID:  169c0426
# LICENSE: MIT
# DATE:    2024-06-12
# UPDATE:  2024-06-12
# NOTES:   Use OHA/SI Project folder structure

# Libraries ====

  library(magrittr)
  library(glamr)
  library(glue)

# LOCALS & SETUP ====

  # Set Params

  ref_id <- "b8140e7e"
  agency <- "USAID"

  ou <- "West Africa Region"

  ou_uid <- glamr::pepfar_country_list %>%
    dplyr::filter(operatingunit == ou) %>%
    dplyr::distinct(operatingunit, operatingunit_uid) %>%
    dplyr::pull(operatingunit_uid) %>%
    dplyr::first()

  cntry <- "Benin"

  cntry_uid <- glamr::pepfar_country_list %>%
    dplyr::filter(country == cntry) %>%
    dplyr::pull(country_uid)

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")

  # Download files [only if needed] - Extract all global & Country Specific MSD

  # grabr::pano_extract_msds(operatingunit = ou,
  #                          archive = FALSE,
  #                          dest_path = dir_mer,
  #                          username = datim_user(),
  #                          password = datim_pwd())

  # Files - MSD

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY22")
  #file_psnu1 <- si_path() %>% return_latest(glue("PSNU_IM_FY15-.*_{ou}"))
  file_psnu2 <- si_path() %>% return_latest(glue("PSNU_IM_FY22-.*_{ou}"))
  file_site1 <- si_path() %>% return_latest(glue("Site_IM_FY15-.*_{ou}"))
  file_site2 <- si_path() %>% return_latest(glue("Site_IM_FY22-.*_{ou}"))
