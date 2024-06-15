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

  # indicators

  ind_pops <- c("POP_EST", "PLHIV")
  ind_dqa <- c("HTS_TST_POS", "TX_NEW", "TX_CURR")

# Functions  =====

# LOAD DATA =====

  #
  df_nat <- file_nat %>% read_psd()

  df_nat <- df_nat %>%
    filter(operatingunit == ou,
           indicator %in% ind_pops | str_detect(indicator, "TX_"))

  df_msd <- file_site2 %>% read_psd()

# MUNGE =====

  # Pops
  df_nat %>% glimpse()

  df_nat %>%
     distinct(indicator, standardizeddisaggregate)

  df_pops <- df_nat %>%
    summarise(across(targets, ~sum(.x, na.rm = T)),
              .by = c(fiscal_year, country, psnuuid, psnu,
                      indicator, standardizeddisaggregate, sex, target_age_2024))


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
        "Age/Sex/HIVStatus"
      )
    ) %>%
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T)),
              .by = c(fiscal_year, country, snu2uid, snu2, psnuuid, psnu,
                      communityuid, community, orgunituid, sitename, sitetype,
                      indicator, sex, ageasentered)) %>%
    reshape_msd() %>%
    filter(!(period_type == "targets" & value == 0)) %>%
    mutate(
      indicator = factor(indicator, levels = ind_dqa),
      period = factor(period, levels = c("FY24Q1", "FY24Q2", "FY24"))
    ) %>%
    relocate(period_type, .after = 1) %>%
    arrange(country, snu2, psnu, community, sitename,
            indicator, period, ageasentered, sex)


# VIZ =====



# OUTPUTS =====

  file_tx_outputs <- "FY24_WAR_Benin_TX_Data.xlsx"

  tx_outputs <- list(
    "TARGETS" = df_tx_sites %>%
      filter(period == meta$curr_fy_lab, period_type == "targets") %>%
      select(-c(communityuid, community, orgunituid, sitename, sitetype)) %>%
      arrange(psnu, ageasentered, sex),
    "RESUTLS" = df_tx_sites %>%
      filter(period_type != "targets") %>%
      arrange(psnu, community, sitename, ageasentered, sex)
  )

  write.xlsx(
    x = tx_outputs,
    file = file.path(dir_dataout, file_tx_outputs)
  )
