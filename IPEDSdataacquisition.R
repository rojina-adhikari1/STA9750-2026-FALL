```{r}
#| label: data-acquisition
#| message: false

acquire_ipeds_data <- function(start_year = 2015, end_year = 2024){
  library(tidyverse)
  library(glue)
  
  data_dir <- file.path("data", "mp01")
  if(!dir.exists(data_dir)){
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  YEARS <- seq(start_year, end_year)
  
  EFA_ALL <- map(YEARS, function(yy){
    ef_url  <- glue("https://nces.ed.gov/ipeds/datacenter/data/EF{yy}A.zip")
    ef_file <- file.path(data_dir, glue("ef{yy}a.csv.zip"))
    
    if(!file.exists(ef_file)){
      message(glue("Downloading Enrollment Data for {yy} from {ef_url}"))
      download.file(ef_url, destfile = ef_file, quiet = TRUE, mode = "wb")
    }
    
    read_csv(ef_file, show_col_types = FALSE) |>
      mutate(year = yy,
             enrollment_m_aian = EFAIANM, enrollment_f_aian = EFAIANW,
             enrollment_m_asia = EFASIAM, enrollment_f_asia = EFASIAW,
             enrollment_m_bkaa = EFBKAAM, enrollment_f_bkaa = EFBKAAW,
             enrollment_m_hisp = EFHISPM, enrollment_f_hisp = EFHISPW,
             enrollment_m_nhpi = EFNHPIM, enrollment_f_nhpi = EFNHPIW,
             enrollment_m_whit = EFWHITM, enrollment_f_whit = EFWHITW,
             enrollment_m_2mor = EF2MORM, enrollment_f_2mor = EF2MORW,
             enrollment_m_unkn = EFUNKNM, enrollment_f_unkn = EFUNKNW,
             enrollment_m_nral = EFNRALM, enrollment_f_nral = EFNRALW) |>
      filter((EFALEVEL %in% c(2, 12)) | (LINE %in% c(1, 15))) |>
      mutate(level = case_when(
        EFALEVEL == 2  ~ "all undergrad",
        EFALEVEL == 12 ~ "all graduate",
        LINE %in% c(1, 15) ~ "first year undergrad")) |>
      select(institution_id = UNITID, year, level, starts_with("enrollment_")) |>
      group_by(institution_id, year, level) |>
      summarize(across(starts_with("enrollment_"), sum), .groups = "drop")
  }) |> bind_rows()
  
  DESC_ALL <- map(YEARS, function(yy){
    hd_url  <- glue("https://nces.ed.gov/ipeds/datacenter/data/HD{yy}.zip")
    hd_file <- file.path(data_dir, glue("hd{yy}.csv.zip"))
    
    if(!file.exists(hd_file)){
      message(glue("Downloading Institutional Descriptions for {yy} from {hd_url}"))
      download.file(hd_url, destfile = hd_file, quiet = TRUE, mode = "wb")
    }
    
    suppressWarnings(
      read_csv(hd_file, show_col_types = FALSE,
               locale = locale(encoding = if_else(yy >= 2024, "utf-8", "windows-1252"))) |>
        select(institution_id = UNITID,
               institution_name = INSTNM,
               state = STABBR) |>
        mutate(year = yy)
    )
  }) |> bind_rows()
  
  inner_join(EFA_ALL, DESC_ALL, join_by(institution_id, year))
}

IPEDS <- acquire_ipeds_data(start_year = 2015, end_year = 2024)
```