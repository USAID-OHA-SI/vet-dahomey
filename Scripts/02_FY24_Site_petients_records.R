# PROJECT: vet-dahomey
# PURPOSE: Extract Latest Patients Records
# AUTHOR: Baboyma Kagniniwa | USAID - SI
# LICENSE: MIT
# REF. ID: 4af8eea5
# CREATED: 2024-07-10
# UPDATED: 2024-07-10
# NOTES:

# Libraries ====

  library(tidyverse)
  library(gagglr)
  library(grabr)
  library(sf)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(janitor)
  library(googledrive)
  library(googlesheets4)
  library(lubridate)

# Set paths  ====

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_datim <- glamr::si_path("path_datim")

# Params ====

  ref_id <- "4af8eea5"
  cntry <- "Nigeria"
  agency <- "USAID"

  dqa_pd <- "FY24Q2"

  fy_start_date <- ymd("2023-10-01")
  dqa_pd_first_date <- ymd("2024-01-01")
  dqa_pd_last_date <- ymd("2024-03-31")

  file_cnhu_url <- rstudioapi::askForPassword(prompt = "Enter Patients Events data googlesheet link:")

  file_cnhu_id <- file_cnhu_url |> as_sheets_id()

  file_cnhu_id |> drive_browse()

  file_name <- file_cnhu_id |>
    as_sheets_id() |>
    drive_get() |>
    pull(name) |>
    unlist()

  file_date <- file_name |>
    str_extract("[^_]*$") |>
    str_replace_all("\\.", "-") |>
    dmy()

  file_cnhu_shts <- file_cnhu_id |>
    googlesheets4::sheet_names()

  file_cnhu_shts_lbls <- file_cnhu_shts |>
    str_to_lower() |>
    str_extract("[^\\s]*$") # chars after last space


# Functions ====


# LOAD DATA ====

  ## Get Metadata + clean columns
  df_cnhu_meta <- file_cnhu_shts |>
    map_dfr(function(.sheet){
      read_sheet(ss = file_cnhu_id, sheet = .sheet) |>
        names() |>
        as_tibble() |>
        rename(label = value) |>
        mutate(
          filename = file_name,
          sheet = .sheet,
          .before = 1
        ) |>
        mutate(
          label_clean = str_replace(label, "Ã©|a©", "e"),
          label_clean = str_replace(label_clean, "Ã|â", "a"),
          label_clean = str_replace(label_clean, "â€™", "'"),
          column = make_clean_names(label_clean)
        )
    })

  df_cnhu_meta |> glimpse()

  df_cnhu_meta |> head()

  Sys.getlocale()

  chr_fr <- "Sexe assignÃ© Ã  la naissance"

  Encoding(chr_fr)

  stringi::stri_enc_detect(chr_fr)

  ## Read Data
  dfs_cnhu <- file_cnhu_shts |>
    map(function(.sheet){
      read_sheet(ss = file_cnhu_id, sheet = .sheet) |>
        rename_with(.cols = everything(),
                    .fn = ~str_replace(.x, "Ã©|a©", "e") |>
                      str_replace("Ã|â", "a") |>
                      str_replace("â€™", "'") |>
                      make_clean_names())
    }) |>
    set_names(nm = file_cnhu_shts_lbls)


# MUNGE ====

  ## ART Pickup for Q2

  dfs_cnhu$art |> glimpse()

  df_art <- dfs_cnhu$art |>
    select(
      orgunit = organisation_unit_name,
      client_hiv_uic = hiv_program_uic,
      #client_art_num = hiv_art_art_number,
      #client_art_num2 = hiv_art_number,
      client_status = hiv_art_patient_status,
      #client_art_status = hiv_art_statut_du_traitement,
      #client_art_prev_status = hiv_art_situation_anterieure_arv,
      client_pop_group = hiv_type_of_population,
      client_pregnant = hiv_art_is_client_pregnant,
      client_sex = sexe_assigne_a_la_naissance,
      client_dob = hiv_age,
      #client_age = NA_integer_,
      date_of_hiv_diagnosis = hiv_date_hiv_status_diagnosed,
      date_of_art_init = hiv_art_date_de_mise_sous_arv,
      date_of_curr_visit = date_of_visit,
      date_of_next_visit = hiv_art_prochaine_date_de_suivi_date_du_prochain_rdv_2,
      arv_days = hiv_art_nombre_de_jours_de_tar_dispenses,
      arv_curr_regiment = regime_actuel
    )

  df_art <- df_art |>
    mutate(across(contains("date|dob"), ~ymd(.x))) |>
    mutate(
      client_age = as.period(
        interval(start = client_dob, end = file_date), unit = "years")$year,
      .after = client_dob
    ) |>
    filter(
      date_of_curr_visit >= dqa_pd_first_date,
      date_of_curr_visit <= dqa_pd_last_date
    ) |> #glimpse() |>
    group_by(orgunit, client_hiv_uic, client_pop_group, client_dob) |>
    arrange(desc(date_of_curr_visit)) |>
    mutate(
      last_record = case_when(
        row_number() == 1 ~ 1,
        TRUE ~ 0
      ),
      art_init_processing_days = case_when(
        all(!is.na(c(date_of_hiv_diagnosis, date_of_art_init))) ~
          as.period(
            interval(start = date_of_hiv_diagnosis,
                     end = date_of_art_init),
            unit = "days")$day,
        TRUE ~ NA_integer_
      )
    ) |>
    ungroup() |>
    filter(last_record == 1) |>
    select(-last_record)

  ## VL Testing for Q2

  #dfs_cnhu$vl |> glimpse()

  df_vl <- dfs_cnhu$vl |>
    select(
      orgunit = organisation_unit_name,
      client_hiv_uic = hiv_program_uic,
      client_pop_group = hiv_type_of_population,
      client_sex = sexe_assigne_a_la_naissance,
      client_dob = hiv_age,
      #client_age = NA_integer_,
      vl_collection_date = date_of_sample_collection,
      vl_transmission_date = hiv_vl_date_echantillon_envoya_c_au_laboratoire,
      vl_results_date = hiv_vl_date_de_reception_du_ra_c_sultat,
      vl_test_results = hiv_vl_viral_load_test_results
    )

  df_vl <- df_vl |>
    filter(
      #vl_collection_date >= dqa_pd_first_date,
      vl_collection_date <= dqa_pd_last_date
    ) |>
    group_by(orgunit, client_hiv_uic, client_pop_group, client_sex, client_dob) |>
    filter(!is.na(vl_results_date)) |>
    arrange(desc(vl_collection_date), desc(vl_results_date)) |>
    mutate(
      last_record = case_when(
        row_number() == 1 ~ 1,
        TRUE ~ 0
      ),
      vl_transmitted_same_day = vl_collection_date == vl_transmission_date,
      vl_test_processing_days = as.period(
        interval(start = vl_collection_date, end = vl_results_date),
        unit = "days")$day
    ) |>
    ungroup() |>
    filter(last_record == 1) |>
    select(-last_record)

  ## Merge ARV & VL

  df_ref <- df_art |>
    left_join(df_vl, by = c("orgunit", "client_hiv_uic", "client_pop_group",
                            "client_sex", "client_dob")) |>
    select(-c(art_init_processing_days,
              vl_transmitted_same_day,
              vl_test_processing_days))

# VIZ ====

  #

# EXPORT ====

  df_ref |>
    write_csv(file = file.path(dir_dataout, paste0("DQA - ", dqa_pd, " - ", file_name, ".csv")))
