# PROJECT: vet-dahomey
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Identify DQA Sites and their TX_CURR Profile
# REF ID:  0fdd63e1
# LICENSE: MIT
# DATE:    2024-06-12
# UPDATE:  2024-06-12
# NOTES:

# Libraries ====

  library(tidyverse)
  library(gagglr)
  library(grabr)
  library(sf)
  library(scales, warn.conflicts = FALSE)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(lubridate)
  library(glue)
  library(openxlsx)

  source("./Scripts/00_Config.R")

# LOCALS & SETUP ====

  # Set Params

  ref_id <- "b8140e7e"

  meta <- get_metadata(file_nat)

  # Files - Geodata

  file_locs <- return_latest(
    folderpath = "./GIS",
    pattern = "SITES_.*.txt",
    recursive = T
  )

  file_water <- return_latest(
    folderpath = "./GIS",
    pattern = ".*water.*.shp",
    recursive = T
  )

  file_roads <- return_latest(
    folderpath = "./GIS",
    pattern = ".*roads.shp",
    recursive = T
  )

  file_adm0 <- return_latest(
    folderpath = "./GIS/gadm41_BEN_shp",
    pattern = ".*BEN_0.shp",
    recursive = T
  )

  file_adm1 <- return_latest(
    folderpath = "./GIS/gadm41_BEN_shp",
    pattern = ".*BEN_1.shp",
    recursive = T
  )

  file_adm2 <- return_latest(
    folderpath = "./GIS/gadm41_BEN_shp",
    pattern = ".*BEN_2.shp",
    recursive = T
  )

  file_adm3 <- return_latest(
    folderpath = "./GIS/gadm41_BEN_shp",
    pattern = ".*BEN_3.shp",
    recursive = T
  )

  # indicators

  ind_pops <- c("POP_EST", "PLHIV")
  ind_dqa <- c("HTS_TST", "HTS_TST_POS",
               "PMTCT_STAT", "PMTCT_STAT_POS",
               "TX_NEW", "TX_CURR", "PMTCT_ART")

# Functions  =====

# LOAD DATA =====

  # Geodata

  ## Raster
  ras <- dir_ras %>% get_raster()

  ## Vector
  spdf_pepfar <- dir_shp %>% get_vcpolygons()

  spdf_pepfar %>%
    filter(uid == cntry_uid) %>%
    gview()

  # Orgs
  df_orgs <- datim_orgunits(cntry = cntry,
                            username = datim_user(),
                            password = datim_pwd(),
                            reshape = T)

  # PSNUs
  spdf_psnu <- df_orgs %>%
    select(country, psnuuid, psnu) %>%
    distinct_all() %>%
    filter(str_detect(psnu, "_Mil.*", negate = T),
           str_detect(psnu, ".* is above.*level$", negate = T)) %>%
    left_join(spdf_pepfar, ., by = c("uid" = "psnuuid")) %>%
    filter(!is.na(psnu))

  spdf_psnu %>% gview

  # Community
  spdf_comm <- df_orgs %>%
    select(country, communityuid, community) %>%
    distinct_all() %>%
    filter(str_detect(community, "_Mil.*", negate = T),
           str_detect(community, ".* is above.*level$", negate = T)) %>%
    left_join(spdf_pepfar, ., by = c("uid" = "communityuid")) %>%
    filter(!is.na(community))

  spdf_comm %>% gview

  # Orgunits

  spdf_orgs <- df_orgs %>%
    filter(level == max(level)) %>%
    select(country, psnuuid, psnu,
           communityuid, community, orgunituid, orgunit) %>%
    distinct_all() %>%
    filter(str_detect(orgunit, "_Mil.*", negate = T),
           str_detect(orgunit, ".* is above.*level$", negate = T))

  # Admins

  spdf_adms <- c(file_adm0, file_adm1, file_adm2, file_adm3) %>%
    map(read_sf) %>%
    set_names(c("country", "departments", "communes", "arrondissments"))

  spdf_adms$country %>% gview
  spdf_adms$departments %>% gview
  spdf_adms$communes %>% gview
  spdf_adms$arrondissments %>% gview

  # Site locations

  spdf_sites <- file_locs %>% read_tsv()

  spdf_sites %>%
    janitor::clean_names() %>%
    spdf_points(lat = "lat", long = "long") %>%
    gview


  # Nat / Subnat

  df_nat <- file_nat %>% read_psd()

  df_nat %>% glimpse()

  df_nat <- df_nat %>%
    filter(operatingunit == ou,
           indicator %in% ind_pops | str_detect(indicator, "TX_"),
           indicatortype == "Sub-national") %>%
    select(fiscal_year, operatingunit, country, psnu, psnuuid,
           indicator, standardizeddisaggregate,
           sex, age = ageasentered, targets)

  # Site x IM

  df_msd <- file_site2 %>% read_psd()

  # df_msd <- dir_data %>%
  #   return_latest("Genie") %>%
  #   read_psd()

# MUNGE =====

  # Pops
  df_nat %>% glimpse()

  df_pops <- df_nat %>%
    summarise(across(targets, ~sum(.x, na.rm = T)),
              .by = c(fiscal_year, country, psnuuid, psnu,
                      indicator, standardizeddisaggregate, sex, age))

  df_nat <- df_nat %>%
    summarise(across(targets, ~sum(.x, na.rm = T)),
              .by = c(fiscal_year, operatingunit, country,
                      indicator, standardizeddisaggregate, sex, age)) %>%
    bind_rows(df_nat, .)

  # TX

  df_msd %>% glimpse()

  df_msd %>%
    filter(fiscal_year == meta$curr_fy, indicator %in% ind_dqa) %>%
    distinct(indicator, standardizeddisaggregate)

  df_tx_sites <- df_msd %>%
    filter(
      fiscal_year == meta$curr_fy,
      country == cntry,
      indicator %in% ind_dqa,
      standardizeddisaggregate %in% c(
        "Modality/Age/Sex/Result",
        "Age/Sex/HIVStatus",
        "Age/Sex/CD4/HIVStatus"
      ) | str_detect(standardizeddisaggregate, "Total")
    )

  df_tx_sites %>%
    distinct(indicator, standardizeddisaggregate)

  df_tx_sites <- df_tx_sites %>%
    reshape_msd() %>%
    mutate(
      ageasentered = case_when(
        #ageasentered %in% c("<01") ~ "01-04",
        ageasentered %in% c("50-54", "55-59", "60-64", "65+") ~ "50+",
        TRUE ~ ageasentered
      )
    ) %>%
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T)),
              .by = c(period, period_type,
                      country, snu2uid, snu2, psnuuid, psnu,
                      communityuid, community, orgunituid, sitename, sitetype,
                      indicator, standardizeddisaggregate,
                      sex, ageasentered)) %>%
    mutate(
      indicator = factor(indicator, levels = ind_dqa),
      period = factor(period, levels = c("FY24Q1", "FY24Q2", "FY24"), ordered = T)
    ) %>%
    relocate(period_type, .after = 1) %>%
    rename(age = ageasentered) %>%
    arrange(country, snu2, psnu, community, sitename,
            indicator, period, age, sex)


# VIZ =====

  df_nat %>%
    filter(fiscal_year == meta$curr_fy,
           is.na(psnu),
           indicator %in% ind_pops,
           standardizeddisaggregate == "Total Numerator") %>%
    pivot_wider(
      names_from = indicator,
      values_from = targets
    ) %>%
    mutate(prev = PLHIV / POP_EST) %>%
    ggplot(aes(prev, reorder(country, prev))) +
    geom_col(fill = burnt_sienna) +
    geom_text(aes(label = percent(prev, .1)),
              size = 4, hjust = 1) +
    scale_x_continuous(labels = percent, position = "top") +
    labs(x = "", y = "",
         title = glue("WAR - {meta$curr_fy} HIV PREVALENCE BY COUNTRY"),
         subtitle = glue("**{cntry}** is ranked 5th, with **0.7% PLHIV** or **39K** HIV+ out of **5.6M** People"),
         caption = glue("{meta$caption} | **Note**: Only pop within PEPFAR Supported PSNUs were included in the calculation of prevalence.")) +
    si_style() +
    theme(plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown())

  df_nat %>%
    filter(fiscal_year == meta$curr_fy,
           country == cntry,
           !is.na(psnu),
           indicator %in% ind_pops,
           standardizeddisaggregate == "Total Numerator") %>%
    pivot_wider(
      names_from = indicator,
      values_from = targets
    ) %>%
    mutate(prev = PLHIV / POP_EST) %>%
    ggplot(aes(prev, reorder(psnu, prev))) +
    geom_col(fill = burnt_sienna) +
    geom_text(aes(label = percent(prev, .1)),
              size = 6, hjust = 1.1) +
    geom_text(aes(label = paste0("\n PLHIV\n ", comma(PLHIV),
                                 "\n POP_EST\n ", comma(POP_EST))),
              size = 3, hjust = 0) +
    scale_x_continuous(labels = percent, position = "top") +
    labs(x = "", y = "",
         title = glue("{str_to_upper(cntry)} - {meta$curr_fy} HIV PREVALENCE BY PSNU"),
         subtitle = glue("**Littoral Department** is ranked 1st, with **2.6% PLHIV** or more than half of the country PLHIV Burden."),
         caption = glue("{meta$caption} | **Note**: Only pop within PEPFAR Supported PSNUs were included in the calculation of prevalence.")) +
    si_style() +
    theme(plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown())


# OUTPUTS =====

  #file_tx_outputs <- "FY24_WAR_Benin_TX_Data.xlsx"
  file_tx_outputs <- "FY24_WAR_Benin_TX_Data_pmtct_stat.xlsx"

  tx_outputs <- list(
    "TARGETS" = df_tx_sites %>%
      filter(period == meta$curr_fy_lab, period_type == "targets") %>%
      select(-c(communityuid, community, orgunituid, sitename, sitetype)) %>%
      arrange(psnu, age, sex),
    "RESUTLS" = df_tx_sites %>%
      filter(period_type != "targets") %>%
      arrange(psnu, community, sitename, age, sex, period)
  )

  write.xlsx(
    x = tx_outputs,
    file = file.path(dir_dataout, file_tx_outputs)
  )

  open_path(file.path(dir_dataout, file_tx_outputs))
