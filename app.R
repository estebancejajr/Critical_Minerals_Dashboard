###############################################
#### Nuclear Energy Critical Minerals Dashboard
###############################################

# ---------------------------------------------------------------------------

rm(list = ls())

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(countrycode)
library(scales)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

safe_chr <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x[is.na(x)] <- ""
  x
}

clean_colnames <- function(x) {
  safe_chr(x) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

pick_column <- function(data_names, candidates) {
  exact_match <- candidates[candidates %in% data_names]
  if (length(exact_match) > 0) return(exact_match[1])
  
  partial_match <- data_names[str_detect(data_names, paste(candidates, collapse = "|"))]
  if (length(partial_match) > 0) return(partial_match[1])
  
  NA_character_
}

find_existing_file <- function(possible_paths) {
  existing <- possible_paths[file.exists(possible_paths)]
  if (length(existing) == 0) return(NA_character_)
  existing[1]
}

empty_project_data <- function() {
  tibble(
    row_id = integer(),
    source = character(),
    mineral = character(),
    country = character(),
    country_clean = character(),
    iso3 = character(),
    year = integer(),
    value = numeric(),
    unit = character(),
    commodity = character(),
    statistic = character(),
    source_note = character(),
    data_type = character(),
    value_standard = numeric(),
    standard_unit = character(),
    duplicate_group_n = integer(),
    selected_for_dashboard = logical(),
    duplicate_flag = character()
  )
}

empty_import_data <- function() {
  tibble(
    source = character(),
    mineral = character(),
    year = integer(),
    importer_country = character(),
    importer_iso3 = character(),
    exporter_country = character(),
    exporter_iso3 = character(),
    import_value_usd = numeric(),
    quantity = numeric(),
    quantity_unit = character(),
    hs_code = character(),
    hs_description = character()
  )
}

standardize_unit_value <- function(value, unit) {
  unit_lower <- str_to_lower(safe_chr(unit))
  multiplier <- case_when(
    str_detect(unit_lower, "thousand") ~ 1000,
    str_detect(unit_lower, "million") ~ 1000000,
    TRUE ~ 1
  )
  as.numeric(value) * multiplier
}

classify_data_type <- function(statistic) {
  stat_lower <- str_to_lower(safe_chr(statistic))
  case_when(
    str_detect(stat_lower, "reserve") ~ "Reserves",
    str_detect(stat_lower, "production|mine production") ~ "Production",
    TRUE ~ "Other"
  )
}

format_number_for_table <- function(x) {
  ifelse(is.na(x), NA_character_, comma(x, accuracy = 1))
}

format_dollar_for_table <- function(x) {
  ifelse(is.na(x), NA_character_, dollar(x, accuracy = 1))
}

normalize_mineral_name <- function(x) {
  x <- str_squish(safe_chr(x))
  case_when(
    str_to_lower(x) == "rare earths" ~ "Rare Earths",
    TRUE ~ x
  )
}

repair_common_mojibake <- function(x) {
  x <- safe_chr(x)
  fixed <- tryCatch(
    iconv(x, from = "UTF-8", to = "latin1"),
    error = function(e) x
  )
  fixed <- ifelse(is.na(fixed), x, fixed)
  fixed <- enc2utf8(fixed)
  ifelse(str_detect(x, "Ã|Â"), fixed, x)
}

pretty_country_from_iso3 <- function(iso3) {
  iso3 <- str_to_upper(str_squish(safe_chr(iso3)))
  iso3[iso3 == ""] <- NA_character_
  countrycode(
    iso3,
    origin = "iso3c",
    destination = "country.name",
    warn = FALSE,
    custom_match = c(
      "ABW" = "Aruba",
      "BOL" = "Bolivia",
      "COD" = "Democratic Republic of the Congo",
      "COG" = "Republic of the Congo",
      "CIV" = "Côte d'Ivoire",
      "HKG" = "Hong Kong",
      "IRN" = "Iran",
      "KOR" = "South Korea",
      "LAO" = "Laos",
      "MAC" = "Macao",
      "MDA" = "Moldova",
      "MKD" = "North Macedonia",
      "PRK" = "North Korea",
      "PSE" = "Palestine",
      "RUS" = "Russia",
      "SYR" = "Syria",
      "TUR" = "Turkey",
      "TWN" = "Taiwan",
      "TZA" = "Tanzania",
      "USA" = "United States",
      "VEN" = "Venezuela",
      "VNM" = "Vietnam",
      "XKX" = "Kosovo"
    )
  )
}

clean_country_display_name <- function(country, iso3 = NULL) {
  country <- repair_common_mojibake(country)
  country <- str_squish(country)
  
  if (!is.null(iso3)) {
    iso_name <- pretty_country_from_iso3(iso3)
    country <- ifelse(!is.na(iso_name) & iso_name != "", iso_name, country)
  }
  
  country %>%
    str_replace_all(fixed("Bolivia (Plurinational State of)"), "Bolivia") %>%
    str_replace_all(fixed("Iran (Islamic Republic of)"), "Iran") %>%
    str_replace_all(fixed("Venezuela (Bolivarian Republic of)"), "Venezuela") %>%
    str_replace_all(fixed("United States of America"), "United States") %>%
    str_replace_all(fixed("Russian Federation"), "Russia") %>%
    str_replace_all(fixed("Republic of Korea"), "South Korea") %>%
    str_replace_all(fixed("Korea, Republic of"), "South Korea") %>%
    str_replace_all(fixed("Dem. People's Rep. of Korea"), "North Korea") %>%
    str_replace_all(fixed("Democratic People's Republic of Korea"), "North Korea") %>%
    str_replace_all(fixed("Lao People's Democratic Republic"), "Laos") %>%
    str_replace_all(fixed("Viet Nam"), "Vietnam") %>%
    str_replace_all(fixed("Syrian Arab Republic"), "Syria") %>%
    str_replace_all(fixed("Türkiye"), "Turkey") %>%
    str_replace_all(fixed("TÃ¼rkiye"), "Turkey") %>%
    str_replace_all(fixed("United Rep. of Tanzania"), "Tanzania") %>%
    str_replace_all(fixed("United Republic of Tanzania"), "Tanzania") %>%
    str_replace_all(fixed("Moldova, Republic of"), "Moldova") %>%
    str_replace_all(fixed("China, Hong Kong SAR"), "Hong Kong") %>%
    str_replace_all(fixed("China, Macao SAR"), "Macao") %>%
    str_replace_all(fixed("Taipei, Chinese"), "Taiwan") %>%
    str_replace_all(fixed("Czech Republic"), "Czechia") %>%
    str_replace_all(fixed("Brunei Darussalam"), "Brunei") %>%
    str_squish()
}

axis_scale_details <- function(values, base_unit) {
  values <- suppressWarnings(as.numeric(values))
  max_value <- max(abs(values), na.rm = TRUE)
  
  if (!is.finite(max_value) || max_value == 0) {
    return(list(
      divisor = 1,
      unit_label = base_unit,
      accuracy = 1
    ))
  }
  
  if (max_value >= 1000000000) {
    divisor <- 1000000000
    unit_label <- paste("billions of", base_unit)
  } else if (max_value >= 1000000) {
    divisor <- 1000000
    unit_label <- paste("millions of", base_unit)
  } else if (max_value >= 1000) {
    divisor <- 1000
    unit_label <- paste("thousands of", base_unit)
  } else {
    divisor <- 1
    unit_label <- base_unit
  }
  
  scaled_max <- max_value / divisor
  accuracy <- ifelse(scaled_max < 10, 0.1, 1)
  
  list(
    divisor = divisor,
    unit_label = unit_label,
    accuracy = accuracy
  )
}

axis_label_function <- function(axis_details) {
  label_number(accuracy = axis_details$accuracy, big.mark = ",")
}

build_country_centroids <- function() {
  if (!requireNamespace("maps", quietly = TRUE)) {
    return(tibble())
  }
  
  cities <- as_tibble(maps::world.cities) %>%
    mutate(
      country_name = safe_chr(country.etc),
      pop_weight = if_else(is.na(pop) | pop <= 0, 1, as.numeric(pop)),
      iso3 = countrycode(
        country_name,
        origin = "country.name",
        destination = "iso3c",
        warn = FALSE,
        custom_match = c(
          "USA" = "USA",
          "UK" = "GBR",
          "Russia" = "RUS",
          "South Korea" = "KOR",
          "North Korea" = "PRK",
          "Congo Democratic Republic" = "COD",
          "Congo Republic" = "COG"
        )
      )
    ) %>%
    filter(!is.na(iso3), !is.na(long), !is.na(lat))
  
  cities %>%
    group_by(iso3) %>%
    summarise(
      lon = weighted.mean(long, pop_weight, na.rm = TRUE),
      lat = weighted.mean(lat, pop_weight, na.rm = TRUE),
      centroid_label = country_name[which.max(pop_weight)][1],
      .groups = "drop"
    )
}

metric_note <- function(data_type) {
  ifelse(
    data_type == "Production",
    "Production values are standardized to metric tons for ranking and comparison.",
    "Reserve values are standardized to metric tons for ranking and comparison."
  )
}

# ---------------------------------------------------------------------------
# Load and prepare project data
# ---------------------------------------------------------------------------

project_data_path <- find_existing_file(c(
  "data/project_data.csv",
  "project_data.csv",
  "/mnt/data/project_data.csv"
))

if (is.na(project_data_path)) {
  stop("Could not find project_data.csv. Put it in data/project_data.csv or the app folder.")
}

raw_project_data <- readr::read_csv(
  project_data_path,
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
) %>%
  mutate(across(where(is.character), safe_chr)) %>%
  mutate(row_id = row_number())

required_project_cols <- c(
  "source", "mineral", "country", "country_clean", "iso3", "year",
  "value", "unit", "commodity", "statistic", "source_note"
)

missing_project_cols <- setdiff(required_project_cols, names(raw_project_data))
if (length(missing_project_cols) > 0) {
  stop(paste(
    "project_data.csv is missing required columns:",
    paste(missing_project_cols, collapse = ", ")
  ))
}

project_raw_augmented <- raw_project_data %>%
  mutate(
    year = as.integer(year),
    value = as.numeric(value),
    data_type = classify_data_type(statistic),
    value_standard = standardize_unit_value(value, unit),
    standard_unit = "metric tons",
    mineral = normalize_mineral_name(mineral),
    country = clean_country_display_name(country, iso3),
    country_clean = clean_country_display_name(country_clean, iso3),
    iso3 = str_squish(safe_chr(iso3)),
    unit = str_squish(safe_chr(unit)),
    statistic = str_squish(safe_chr(statistic)),
    commodity = str_squish(safe_chr(commodity)),
    source = str_squish(safe_chr(source)),
    source_note = str_squish(safe_chr(source_note))
  ) %>%
  filter(
    !is.na(year),
    !is.na(value_standard),
    value_standard > 0,
    data_type %in% c("Production", "Reserves"),
    !is.na(mineral), mineral != "",
    !is.na(country_clean), country_clean != "",
    !is.na(iso3), iso3 != ""
  ) %>%
  group_by(mineral, data_type, year, country_clean, iso3) %>%
  mutate(
    duplicate_group_n = n(),
    duplicate_flag = if_else(
      duplicate_group_n > 1,
      paste0(
        "Duplicate country-year records detected; dashboard keeps the largest standardized value from ",
        duplicate_group_n,
        " rows."
      ),
      "No duplicate flag"
    )
  ) %>%
  ungroup()

# Dashboard data keeps one country-year-mineral-data_type record. If duplicates
# exist, it keeps the largest standardized value. If standardized values tie,
# it keeps the first row after sorting by raw value and row_id.
project_data <- project_raw_augmented %>%
  arrange(
    mineral,
    data_type,
    year,
    country_clean,
    desc(value_standard),
    desc(value),
    row_id
  ) %>%
  group_by(mineral, data_type, year, country_clean, iso3) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(selected_for_dashboard = TRUE)

selected_row_ids <- project_data$row_id

project_raw_augmented <- project_raw_augmented %>%
  mutate(selected_for_dashboard = row_id %in% selected_row_ids)

mineral_choices <- c("Boron", "Rare Earths", "Uranium", "Zirconium")
data_type_choices <- c("Production", "Reserves")
year_choices <- as.character(sort(unique(project_raw_augmented$year)))
if (length(year_choices) == 0) year_choices <- as.character(2021:2025)

# ---------------------------------------------------------------------------
# Optional static bilateral import data
# ---------------------------------------------------------------------------

import_data_path <- find_existing_file(c(
  "data/import_data.csv",
  "import_data.csv",
  "/mnt/data/import_data.csv"
))

standardize_import_data <- function(raw_import_data) {
  if (is.null(raw_import_data) || nrow(raw_import_data) == 0) {
    return(empty_import_data())
  }
  
  df <- raw_import_data
  names(df) <- clean_colnames(names(df))
  data_names <- names(df)
  
  mineral_col <- pick_column(data_names, c("mineral", "commodity", "material"))
  year_col <- pick_column(data_names, c("year", "data_year"))
  importer_country_col <- pick_column(data_names, c(
    "importer_country", "importing_country", "reporter_country",
    "reporter_name", "reporter", "country_name"
  ))
  importer_iso3_col <- pick_column(data_names, c(
    "importer_iso3", "importer_iso", "reporter_iso3", "reporter_iso",
    "reporter_code_iso3"
  ))
  exporter_country_col <- pick_column(data_names, c(
    "exporter_country", "exporting_country", "partner_country",
    "partner_name", "partner", "origin_country", "source_country"
  ))
  exporter_iso3_col <- pick_column(data_names, c(
    "exporter_iso3", "exporter_iso", "partner_iso3", "partner_iso",
    "partner_code_iso3", "origin_iso3"
  ))
  value_col <- pick_column(data_names, c(
    "import_value_usd", "trade_value_usd", "value_usd", "customs_value_usd",
    "primary_value", "trade_value", "value"
  ))
  quantity_col <- pick_column(data_names, c("quantity", "qty", "netweight", "net_weight", "weight"))
  quantity_unit_col <- pick_column(data_names, c("quantity_unit", "qty_unit", "unit", "units"))
  hs_code_col <- pick_column(data_names, c("hs_code", "cmd_code", "commodity_code", "product_code"))
  hs_description_col <- pick_column(data_names, c(
    "hs_description", "cmd_desc", "commodity_description", "product_description", "description"
  ))
  source_col <- pick_column(data_names, c("source", "data_source"))
  
  required_import_cols <- c(
    mineral_col, year_col, importer_country_col, exporter_country_col, value_col
  )
  
  if (any(is.na(required_import_cols))) {
    return(empty_import_data())
  }
  
  out <- df %>%
    transmute(
      source = {
        if (!is.na(source_col)) str_squish(safe_chr(.data[[source_col]])) else "Static import CSV"
      },
      mineral = normalize_mineral_name(.data[[mineral_col]]),
      year = as.integer(suppressWarnings(readr::parse_number(safe_chr(.data[[year_col]])))),
      importer_country = str_squish(safe_chr(.data[[importer_country_col]])),
      importer_iso3 = {
        if (!is.na(importer_iso3_col)) {
          str_squish(safe_chr(.data[[importer_iso3_col]]))
        } else {
          countrycode(
            str_squish(safe_chr(.data[[importer_country_col]])),
            origin = "country.name",
            destination = "iso3c",
            warn = FALSE
          )
        }
      },
      exporter_country = str_squish(safe_chr(.data[[exporter_country_col]])),
      exporter_iso3 = {
        if (!is.na(exporter_iso3_col)) {
          str_squish(safe_chr(.data[[exporter_iso3_col]]))
        } else {
          countrycode(
            str_squish(safe_chr(.data[[exporter_country_col]])),
            origin = "country.name",
            destination = "iso3c",
            warn = FALSE
          )
        }
      },
      import_value_usd = suppressWarnings(readr::parse_number(safe_chr(.data[[value_col]]))),
      quantity = {
        if (!is.na(quantity_col)) {
          suppressWarnings(readr::parse_number(safe_chr(.data[[quantity_col]])))
        } else {
          NA_real_
        }
      },
      quantity_unit = {
        if (!is.na(quantity_unit_col)) str_squish(safe_chr(.data[[quantity_unit_col]])) else "Not reported"
      },
      hs_code = {
        if (!is.na(hs_code_col)) str_squish(safe_chr(.data[[hs_code_col]])) else "Not reported"
      },
      hs_description = {
        if (!is.na(hs_description_col)) str_squish(safe_chr(.data[[hs_description_col]])) else "Not reported"
      }
    ) %>%
    filter(
      mineral %in% mineral_choices,
      !is.na(year),
      !is.na(import_value_usd),
      import_value_usd > 0,
      importer_country != "",
      exporter_country != ""
    ) %>%
    mutate(
      importer_country = clean_country_display_name(importer_country, importer_iso3),
      exporter_country = clean_country_display_name(exporter_country, exporter_iso3)
    )
  
  out
}

if (!is.na(import_data_path)) {
  raw_import_data <- readr::read_csv(
    import_data_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  ) %>%
    mutate(across(where(is.character), safe_chr))
  
  import_data <- standardize_import_data(raw_import_data)
} else {
  import_data <- empty_import_data()
}

country_centroids <- build_country_centroids()

get_focus_country_data <- function(focus_iso3) {
  if (is.null(focus_iso3) || length(focus_iso3) == 0 || is.na(focus_iso3) || focus_iso3 == "" || focus_iso3 == "World View") {
    return(tibble())
  }
  if (nrow(country_centroids) == 0) {
    return(tibble())
  }
  country_centroids %>%
    filter(iso3 == focus_iso3) %>%
    slice_head(n = 1) %>%
    mutate(country = clean_country_display_name(centroid_label, iso3))
}

# ---------------------------------------------------------------------------
# User interface
# ---------------------------------------------------------------------------

ui <- page_sidebar(
  title = div(
    class = "app-title",
    span(class = "app-title-main", "Nuclear Energy Critical Minerals Dashboard"),
    span(class = "app-title-sub", "Production, Reserves, Concentration, Trade, and Data Coverage")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#102a43",
    secondary = "#334155",
    success = "#16a34a",
    info = "#2563eb",
    warning = "#d97706",
    danger = "#dc2626",
    base_font = "system-ui"
  ),
  sidebar = sidebar(
    helpText("Use this dashboard to compare nuclear-energy mineral supply, producer concentration, producer exports, and bilateral import exposure."),
    
    selectInput(
      inputId = "mineral",
      label = "Mineral",
      choices = mineral_choices,
      selected = "Boron"
    ),
    
    selectInput(
      inputId = "data_type",
      label = "Metric",
      choices = data_type_choices,
      selected = "Production"
    ),
    
    selectInput(
      inputId = "year",
      label = "Year",
      choices = year_choices,
      selected = max(year_choices)
    ),
    
    selectizeInput(
      inputId = "hs_codes",
      label = "HS Codes for Trade Tabs",
      choices = c("All HS Codes" = "All HS Codes"),
      selected = "All HS Codes",
      multiple = TRUE,
      options = list(
        placeholder = "All HS Codes",
        create = FALSE,
        plugins = list("remove_button")
      )
    ),
    helpText("This filter applies only to CEPII/BACI trade tabs, maps, and country profiles."),
    
    numericInput(
      inputId = "top_n",
      label = "Countries in ranking",
      value = 10,
      min = 3,
      max = 25,
      step = 1
    ),
    
    hr(),
    helpText("Duplicate country-year rows are handled transparently: rankings keep the largest standardized value, and all duplicate rows are flagged in Data & Sources.")
  ),
  
  tags$head(
    tags$style(HTML("
      :root {
        --dash-navy: #102a43;
        --dash-blue: #1d4ed8;
        --dash-teal: #0f766e;
        --dash-slate: #334155;
        --dash-muted: #64748b;
        --dash-border: #dbe4ee;
        --dash-card: #ffffff;
        --dash-soft: #f8fafc;
        --dash-soft-blue: #eff6ff;
        --dash-warning-bg: #fffbeb;
        --dash-warning-text: #7c2d12;
        --dash-danger-bg: #fef2f2;
        --dash-danger-text: #7f1d1d;
      }

      body {
        background: #f3f6fa;
        color: #0f172a;
        -webkit-font-smoothing: antialiased;
      }

      .bslib-sidebar-layout > .sidebar {
        background: #ffffff;
        border-right: 1px solid var(--dash-border);
        box-shadow: 8px 0 24px rgba(15, 23, 42, 0.04);
      }

      .app-title {
        display: flex;
        flex-direction: column;
        gap: 0.15rem;
        line-height: 1.1;
      }
      .app-title-main {
        font-weight: 800;
        letter-spacing: -0.025em;
        font-size: 1.2rem;
      }
      .app-title-sub {
        font-weight: 500;
        font-size: 0.78rem;
        opacity: 0.72;
      }

      .dashboard-intro {
        background: linear-gradient(135deg, #102a43 0%, #0f766e 100%);
        color: white;
        border-radius: 20px;
        padding: 1.35rem 1.5rem;
        margin-bottom: 1.1rem;
        box-shadow: 0 18px 36px rgba(15, 23, 42, 0.18);
        border: 1px solid rgba(255, 255, 255, 0.18);
      }
      .dashboard-intro h2 {
        margin: 0 0 0.35rem 0;
        font-size: 1.45rem;
        font-weight: 820;
        letter-spacing: -0.03em;
      }
      .dashboard-intro p {
        margin: 0;
        opacity: 0.94;
        max-width: 76rem;
        font-size: 0.98rem;
      }

      .card {
        border: 1px solid var(--dash-border);
        border-radius: 18px;
        box-shadow: 0 10px 26px rgba(15, 23, 42, 0.07);
        overflow: hidden;
        background: var(--dash-card);
      }
      .card-header {
        background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
        color: #0f172a;
        font-weight: 780;
        letter-spacing: -0.01em;
        border-bottom: 1px solid var(--dash-border);
        padding-top: 0.85rem;
        padding-bottom: 0.85rem;
      }

      .value-box, .bslib-value-box {
        border-radius: 18px !important;
        border: 1px solid var(--dash-border) !important;
        box-shadow: 0 10px 26px rgba(15, 23, 42, 0.07) !important;
      }
      .bslib-value-box .value-box-title {
        color: var(--dash-muted);
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        font-size: 0.78rem;
      }

      .nav-tabs {
        border-bottom: 0;
        gap: 0.4rem;
        margin-bottom: 1rem;
      }
      .nav-tabs .nav-link {
        border: 1px solid var(--dash-border);
        border-radius: 999px;
        background: #ffffff;
        color: var(--dash-slate);
        font-weight: 730;
        padding: 0.55rem 0.95rem;
        box-shadow: 0 4px 12px rgba(15, 23, 42, 0.04);
        transition: all 140ms ease-in-out;
      }
      .nav-tabs .nav-link:hover {
        color: var(--dash-navy);
        border-color: #b6c4d2;
        transform: translateY(-1px);
      }
      .nav-tabs .nav-link.active {
        color: white !important;
        background: var(--dash-navy) !important;
        border-color: var(--dash-navy) !important;
        box-shadow: 0 10px 22px rgba(15, 23, 42, 0.16);
        transform: translateY(-1px);
      }

      .form-label, label {
        font-weight: 720;
        color: #0f172a;
      }
      .form-control, .form-select, .selectize-input, .selectize-dropdown, .btn {
        border-radius: 12px !important;
      }
      .selectize-input, .form-control, .form-select {
        border-color: #cbd5e1 !important;
        box-shadow: none !important;
      }
      .selectize-input.focus, .form-control:focus, .form-select:focus {
        border-color: var(--dash-teal) !important;
        box-shadow: 0 0 0 0.18rem rgba(15, 118, 110, 0.16) !important;
      }
      .btn {
        font-weight: 720;
        border: 0;
        box-shadow: 0 6px 14px rgba(15, 23, 42, 0.10);
      }

      .country-profile-search .selectize-control.single .selectize-input {
        min-height: 42px;
        display: flex;
        align-items: center;
        cursor: text;
      }
      .country-profile-search .selectize-control.single .selectize-input input {
        min-width: 220px !important;
        opacity: 1 !important;
      }
      .country-profile-search .selectize-control.single .selectize-input.input-active input {
        width: 100% !important;
        min-width: 220px !important;
      }
      .country-profile-search .selectize-dropdown { z-index: 10000; }
      .country-profile-search-hint {
        font-size: 0.86rem;
        color: var(--dash-muted);
        margin-top: -0.35rem;
        margin-bottom: 0.75rem;
      }

      .selection-alert {
        border-radius: 14px;
        padding: 0.85rem 1rem;
        margin-bottom: 1rem;
        font-weight: 650;
        box-shadow: 0 6px 16px rgba(15, 23, 42, 0.06);
      }
      .selection-alert.warning {
        color: var(--dash-warning-text);
        background: var(--dash-warning-bg);
        border: 1px solid #fed7aa;
      }
      .selection-alert.danger {
        color: var(--dash-danger-text);
        background: var(--dash-danger-bg);
        border: 1px solid #fecaca;
      }

      .dataTables_wrapper {
        font-size: 0.92rem;
      }
      table.dataTable thead th {
        background: #f1f5f9;
        color: #0f172a;
        border-bottom: 1px solid var(--dash-border) !important;
        font-weight: 780;
      }
      table.dataTable tbody td {
        vertical-align: middle;
      }
      code {
        color: #0f766e;
        background: #ecfdf5;
        padding: 0.15rem 0.3rem;
        border-radius: 0.35rem;
      }

      .method-callout {
        background: #f8fafc;
        border: 1px solid var(--dash-border);
        border-left: 5px solid var(--dash-teal);
        border-radius: 14px;
        padding: 1rem 1.1rem;
        margin-bottom: 1rem;
      }
      .method-callout h4 {
        margin-top: 0;
        font-weight: 800;
        color: #0f172a;
      }
      .map-caption {
        color: var(--dash-muted);
        font-size: 0.9rem;
        margin: 0.75rem 0 0 0;
        border-top: 1px solid var(--dash-border);
        padding-top: 0.75rem;
      }
      .coverage-list {
        margin-bottom: 0;
      }
      .coverage-list li {
        margin-bottom: 0.35rem;
      }
      .source-footer {
        color: #475569;
        background: #ffffff;
        border: 1px solid var(--dash-border);
        border-radius: 14px;
        padding: 0.9rem 1rem;
        margin-top: 1rem;
        font-size: 0.92rem;
        box-shadow: 0 4px 12px rgba(15, 23, 42, 0.05);
      }
      .download-row {
        display: flex;
        gap: 0.75rem;
        flex-wrap: wrap;
        margin-bottom: 1rem;
      }
    "))
  ),
  
  div(
    class = "dashboard-intro",
    h2("Nuclear Energy Critical Minerals Dashboard"),
    p("Compare production, reserves, concentration, trade exposure, and data coverage for selected minerals important to nuclear-energy supply chains.")
  ),
  
  navset_tab(
    nav_panel(
          "Overview",
          uiOutput("selection_status_overview"),
          card(
            card_header("How to Use This Dashboard"),
            p("Start with Overview to identify the leading producer or reserve holder for the selected mineral, metric, and year."),
            p("Use Concentration to assess whether supply is concentrated, Rankings to compare countries, Country Profile to examine one country, and Trade Dependence to evaluate whether production concentration appears in bilateral trade exposure."),
            p("Use Methods & Coverage and Data & Sources to check whether the selected mineral-year combination has enough reported coverage for interpretation.")
          ),
          layout_columns(
            value_box(
              title = "Countries shown",
              value = textOutput("country_count")
            ),
            value_box(
              title = "Largest country",
              value = textOutput("top_country")
            ),
            value_box(
              title = "Largest share",
              value = textOutput("top_share")
            ),
            value_box(
              title = "Top 3 share",
              value = textOutput("top3_share")
            ),
            col_widths = c(3, 3, 3, 3)
          ),
          layout_columns(
            card(
              card_header("What this selection shows"),
              textOutput("summary_text"),
              br(),
              textOutput("coverage_note"),
              br(),
              textOutput("unit_note")
            ),
            card(
              card_header("Key metrics"),
              DTOutput("selection_summary_table")
            ),
            col_widths = c(6, 6)
          )
        ),
    
    nav_panel(
          "Country Profile",
          uiOutput("selection_status_country_profile"),
          layout_columns(
            card(
              card_header("Country Profile Controls"),
              div(
                class = "country-profile-search",
                selectizeInput(
                  inputId = "country_profile_country",
                  label = "Search for a Country",
                  choices = c("Loading country list..." = ""),
                  selected = "",
                  multiple = FALSE,
                  options = list(
                    placeholder = "Type a country name or ISO3 code",
                    create = FALSE,
                    openOnFocus = TRUE,
                    maxOptions = 10000,
                    maxItems = 1,
                    labelField = "label",
                    valueField = "value",
                    searchField = c("label", "value"),
                    selectOnTab = TRUE,
                    closeAfterSelect = TRUE,
                    plugins = list("clear_button")
                  )
                ),
                div(
                  class = "country-profile-search-hint",
                  "Click in the box and start typing. The country list will narrow as you type."
                )
              ),
              radioButtons(
                inputId = "country_profile_view",
                label = "Trade View",
                choices = c("Importer View" = "Imports", "Exporter View" = "Exports"),
                selected = "Imports"
              ),
              numericInput(
                inputId = "country_profile_map_links",
                label = "Trade links to show on country map",
                value = 25,
                min = 5,
                max = 75,
                step = 5
              ),
              helpText("This tab uses the selected mineral and year from the sidebar. Switch between import-side and export-side flows for one country."),
              htmlOutput("hs_code_status_country")
            ),
            card(
              card_header("Country Summary"),
              htmlOutput("country_profile_status"),
              DTOutput("country_profile_summary_table")
            ),
            col_widths = c(4, 8)
          ),
          card(
            card_header("Country Trade Flow Map"),
            plotlyOutput("country_profile_trade_map", height = "620px"),
            p(class = "map-caption", "Lines show selected BACI exporter-to-importer flows for the chosen country, mineral, year, and HS-code filter. Direction appears in the hover label and flow table.")
          ),
          layout_columns(
            card(
              card_header("Top Trading Partners"),
              plotOutput("country_profile_partner_chart", height = "500px")
            ),
            card(
              card_header("Production and Reserves Context"),
              DTOutput("country_profile_supply_table")
            ),
            col_widths = c(7, 5)
          ),
          card(
            card_header("Bilateral Country Flows"),
            DTOutput("country_profile_flow_table")
          ),
          card(
            card_header("Country Interpretation"),
            htmlOutput("country_profile_interpretation")
          ),
          card(
            card_header("Interpretation Note"),
            p("The country profile reads CEPII/BACI bilateral flows from either the importer side or the exporter side."),
            p("These HS6 mineral-related flows should be interpreted as trade exposure, not as a perfect trace of physical mine output.")
          )
        ),
    
    nav_panel(
          "Country Comparison",
          uiOutput("selection_status_compare"),
          layout_columns(
            card(
              card_header("Choose comparison countries"),
              selectizeInput(
                inputId = "compare_countries",
                label = "Choose countries to compare",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(maxItems = 8, placeholder = "Select up to 8 countries")
              ),
              helpText("Country trends use the selected mineral and data type. Values are standardized to metric tons.")
            ),
            card(
              card_header("Country comparison over time"),
              plotOutput("country_trend", height = "420px")
            ),
            col_widths = c(4, 8)
          ),
          layout_columns(
            card(
              card_header("Ranks, values, and mapped shares"),
              DTOutput("country_comparison_table")
            ),
            col_widths = c(12)
          )
        ),
    
    nav_panel(
          "Concentration",
          uiOutput("selection_status_concentration"),
          layout_columns(
            card(
              card_header("How concentrated is supply over time?"),
              plotOutput("concentration_trend", height = "450px")
            ),
            card(
              card_header("Year-by-year concentration summary"),
              DTOutput("concentration_table")
            ),
            col_widths = c(7, 5)
          ),
          card(
            card_header("How to read the concentration metrics"),
            p("Top-1, Top-3, and Top-5 shares show how much of the selected mapped total is controlled by the largest countries."),
            p("HHI is the Herfindahl-Hirschman Index, calculated from country shares. Higher values mean supply is more concentrated and therefore potentially more exposed to country-specific disruptions.")
          )
        ),
    
    nav_panel(
          "Rankings",
          uiOutput("selection_status_rankings"),
          layout_columns(
            card(
              card_header("Largest countries in this view"),
              plotOutput("bar_chart", height = "600px")
            ),
            card(
              card_header("Ranking details"),
              DTOutput("top_country_table")
            ),
            col_widths = c(7, 5)
          )
        ),
    
    nav_panel(
          "Trade Dependence",
          uiOutput("selection_status_trade"),
          card(
            card_header("Trade Dependence Controls"),
            htmlOutput("import_status"),
            numericInput(
              inputId = "producer_count",
              label = "Main producer countries to screen",
              value = 5,
              min = 1,
              max = 15,
              step = 1
            ),
            checkboxInput(
              inputId = "only_top_producer_imports",
              label = "Focus on imports from these producer countries",
              value = TRUE
            ),
            htmlOutput("hs_code_status_trade")
          ),
          card(
            card_header("Producer Countries Used for Trade Screen"),
            DTOutput("producer_screen_table")
          ),
          card(
            card_header("Trade Dependence Interpretation"),
            htmlOutput("trade_interpretation")
          ),
          layout_columns(
            card(
              card_header("Top Importers From Major Producers"),
              plotOutput("importer_bar_chart", height = "500px")
            ),
            card(
              card_header("Major Producers Dominating Import Flows"),
              plotOutput("exporter_import_chart", height = "500px")
            ),
            col_widths = c(6, 6)
          ),
          layout_columns(
            card(
              card_header("Do Top Producers Also Dominate Exports?"),
              plotOutput("producer_trade_alignment_chart", height = "450px")
            ),
            card(
              card_header("Production Rank vs Export-Flow Rank"),
              DTOutput("producer_trade_alignment_table")
            ),
            col_widths = c(6, 6)
          ),
          card(
            card_header("Bilateral Trade Links"),
            DTOutput("import_dependence_table")
          ),
          card(
            card_header("Interpretation Note"),
            p("This tab screens bilateral CEPII/BACI trade flows against the main producer countries identified in the production data."),
            p("These are HS6 mineral-related trade categories. They should be interpreted as trade exposure, not direct physical mine-output shipments."),
            p("The checkbox above lets you focus trade exposure on the selected top producer countries or view the broader trade universe for the selected mineral and HS codes.")
          ),
          card(
            card_header("Static Import CSV Requirements"),
            DTOutput("import_schema_table")
          )
        ),

    nav_panel(
          "Global Map",
          uiOutput("selection_status_map"),
          card(
            card_header("Where this mineral is reported"),
            plotlyOutput("world_map", height = "650px"),
            p(class = "map-caption", "Darker countries indicate larger reported production or reserves for the selected mineral, metric, and displayed data year.")
          )
        ),

    nav_panel(
          "Trade Scale Map",
          uiOutput("selection_status_importer_exporter_map"),
          layout_columns(
            card(
              card_header("Trade Scale Map Controls"),
              htmlOutput("import_status_map"),
              radioButtons(
                inputId = "trade_party_map_role",
                label = "Trade View",
                choices = c("Importer View" = "Importers", "Exporter View" = "Exporters"),
                selected = "Importers"
              ),
              htmlOutput("hs_code_status_map")
            ),
            card(
              card_header("Importer/Exporter Country Scale Map"),
              plotlyOutput("trade_party_map", height = "650px"),
              p(class = "map-caption", "Darker countries indicate larger selected BACI mineral-related trade values for the chosen year and HS-code filter.")
            ),
            col_widths = c(3, 9)
          ),
          card(
            card_header("How to Read the Trade Scale Map"),
            p("Importer View aggregates selected BACI trade flows by importing country."),
            p("Exporter View aggregates selected BACI trade flows by exporting country."),
            p("Darker countries indicate larger selected mineral-related trade values for the chosen year and HS-code filter.")
          )
        ),

    nav_panel(
          "Trade Flows Map",
          uiOutput("selection_status_trade_map"),
          layout_columns(
            card(
              card_header("Map Controls"),
              htmlOutput("import_status_map"),
              numericInput(
                inputId = "trade_map_links",
                label = "Trade links to show on map",
                value = 25,
                min = 5,
                max = 75,
                step = 5
              ),
              htmlOutput("hs_code_status_map")
            ),
            card(
              card_header("Bilateral Trade Flows"),
              plotlyOutput("trade_flow_map", height = "650px"),
              p(class = "map-caption", "Line thickness represents selected BACI trade value. Each line runs from exporter/source country to importer/destination country.")
            ),
            col_widths = c(3, 9)
          ),
          card(
            card_header("How to Read the Trade Flows Map"),
            p("Line thickness represents trade value in the selected BACI HS6 categories."),
            p("Lines are directional: exporter/source country → importer/destination country. Plotly does not reliably support arrowheads on geo lines, so direction appears in the hover label and table."),
            p("These flows represent mineral-related HS6 trade categories, not direct physical mine-output shipments.")
          ),
          card(
            card_header("Mapped Trade Links"),
            DTOutput("trade_flow_map_table")
          )
        ),

    nav_panel(
          "Methods & Coverage",
          uiOutput("selection_status_methods"),
          layout_columns(
            card(
              card_header("What the Dashboard Measures"),
              div(class = "method-callout",
                  h4("Production and Reserves"),
                  p("Production and reserve views use the prepared project_data.csv file. Values are standardized to metric tons so countries can be ranked consistently within each mineral and metric."),
                  p("Production and reserves are intentionally kept separate because they measure different supply concepts: current output versus longer-run resource control."),
                  p("Uranium uses BGS World Mineral Statistics in this dashboard because the prepared uranium coverage was built from BGS country-year tables, while Boron, Rare Earths, and Zirconium were prepared from USGS Mineral Commodity Summaries.")),
              div(class = "method-callout",
                  h4("Trade Dependence"),
                  p("Trade tabs use a prepared static CEPII/BACI file. BACI records bilateral HS6 trade flows by year, exporter, importer, and product."),
                  p("The dashboard treats these as mineral-related trade exposure measures. They should not be interpreted as exact physical mine-output shipments.")),
              div(class = "method-callout",
                  h4("Rare Earths"),
                  p("Rare Earths are shown as an aggregated category because source data and HS6 trade codes usually report rare-earth groups or compounds rather than individual elements.")),
              div(class = "method-callout",
                  h4("Zirconium and Hafnium"),
                  p("Zirconium and hafnium are treated together in the methodological notes because USGS describes zircon as the principal economic source of zirconium and the primary source of all hafnium."),
                  p("USGS also notes that zirconium and hafnium are contained together in zircon and that zircon is commonly recovered as a coproduct or byproduct of heavy-mineral-sands mining and processing. For this dashboard, the combined treatment avoids implying that the two elements have fully separate mining supply chains at the source-data level."))
            ),
            card(
              card_header("HS Codes Included in Current Trade Filter"),
              DTOutput("hs_code_table")
            ),
            col_widths = c(7, 5)
          ),
          layout_columns(
            card(
              card_header("Mineral Uses in Nuclear Energy"),
              DTOutput("mineral_uses_table")
            ),
            col_widths = c(12)
          ),
          layout_columns(
            card(
              card_header("Data Coverage by Mineral and Year"),
              DTOutput("coverage_table")
            ),
            col_widths = c(12)
          ),
          layout_columns(
            card(
              card_header("Known Limitations"),
              tags$ul(
                class = "coverage-list",
                tags$li("Source coverage differs by mineral, metric, and year."),
                tags$li("Rare Earths are aggregated rather than element-specific."),
                tags$li("Zirconium and hafnium are discussed together because they share a zircon-linked supply chain in the USGS source documentation."),
                tags$li("BACI HS6 trade flows measure mineral-related trade exposure, not direct mine-to-end-user supply chains."),
                tags$li("Missing values may reflect non-reporting, aggregation, withheld data, or source limitations rather than true zero production or reserves.")
              )
            ),
            col_widths = c(12)
          ),
          layout_columns(
            card(
              card_header("Download Filtered Data"),
              div(class = "download-row",
                  downloadButton("download_dashboard_data", "Download Production/Reserve Data"),
                  downloadButton("download_trade_data", "Download Filtered Trade Data")),
              p("Downloads use the current mineral, year, metric, and HS-code selections where applicable.")
            ),
            col_widths = c(12)
          )
        ),
    
    nav_panel(
          "Data & Sources",
          uiOutput("selection_status_data"),
          layout_columns(
            card(
              card_header("Simplified Data Availability by Mineral, Metric, and Year"),
              p("This summary shows which minerals, metrics, and years are available in the prepared dashboard data. It is intended as a quick coverage check before interpreting rankings or maps."),
              DTOutput("data_availability_table")
            ),
            col_widths = c(12)
          ),
          layout_columns(
            card(
              card_header("Download Current Data"),
              div(class = "download-row",
                  downloadButton("download_dashboard_data_sources", "Download Production/Reserve Data"),
                  downloadButton("download_trade_data_sources", "Download Filtered Trade Data")),
              p("These downloads mirror the current sidebar filters.")
            ),
            col_widths = c(12)
          ),
          layout_columns(
            card(
              card_header("Dashboard-ready records"),
              DTOutput("dashboard_data_table")
            ),
            col_widths = c(12)
          ),
          layout_columns(
            card(
              card_header("Raw records and duplicate flags"),
              DTOutput("raw_data_table")
            ),
            col_widths = c(12)
          ),
          card(
            card_header("Data Version and Source Notes"),
            p("Data files used: project_data.csv and import_data.csv. BACI trade data are from HS17 V202601. Production and reserve extracts are prepared from USGS and BGS source tables."),
            p("Boron, Rare Earths, and Zirconium records come from the prepared USGS Mineral Commodity Summaries extract; uranium records come from the prepared BGS World Mineral Statistics extract. Uranium uses BGS here because the prepared uranium coverage was built from BGS country-year tables."),
            p("The dashboard standardizes production and reserve values to metric tons before ranking countries, calculating mapped shares, and estimating concentration metrics."),
            p("Rare Earths are aggregated, and zirconium/hafnium are discussed together in Methods & Coverage, to avoid implying more source-level precision than the underlying data support.")
          )
        )
  ),
  div(
    class = "source-footer",
    HTML("<strong>Sources:</strong> Prepared USGS and BGS production/reserve extracts; CEPII BACI HS17 V202601 bilateral trade data. See Methods & Coverage and Data & Sources for coverage notes and limitations.")
  )
)

# ---------------------------------------------------------------------------
# Server logic
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  selected_year <- reactive({
    as.integer(input$year)
  })
  
  selected_base_data <- reactive({
    req(input$mineral, input$data_type)
    
    project_data %>%
      filter(
        mineral == input$mineral,
        data_type == input$data_type
      )
  })
  
  selected_data <- reactive({
    df_all <- selected_base_data()
    
    if (nrow(df_all) == 0) {
      return(empty_project_data())
    }
    
    available_years <- sort(unique(df_all$year))
    usable_years <- available_years[available_years <= selected_year()]
    
    if (length(usable_years) == 0) {
      return(empty_project_data())
    }
    
    actual_year <- max(usable_years)
    
    df_all %>%
      filter(year == actual_year) %>%
      arrange(desc(value_standard))
  })
  
  actual_data_year <- reactive({
    df <- selected_data()
    if (nrow(df) == 0) return(NA_integer_)
    unique(df$year)[1]
  })
  
  import_available <- reactive({
    nrow(import_data) > 0
  })
  
  hs_code_filter <- reactive({
    selected <- input$hs_codes
    if (!import_available() || is.null(selected) || length(selected) == 0) return(NULL)
    if ("All HS Codes" %in% selected) return(NULL)
    selected
  })
  
  filtered_import_data <- reactive({
    if (!import_available()) return(empty_import_data())
    df <- import_data %>% filter(mineral == input$mineral)
    codes <- hs_code_filter()
    if (!is.null(codes)) {
      df <- df %>% filter(hs_code %in% codes)
    }
    df
  })
  
  selected_hs_code_label <- reactive({
    codes <- hs_code_filter()
    if (is.null(codes)) return("All HS Codes")
    paste(codes, collapse = ", ")
  })
  
  observe({
    if (!import_available()) {
      updateSelectizeInput(
        session,
        "hs_codes",
        choices = c("All HS Codes" = "All HS Codes"),
        selected = "All HS Codes",
        server = TRUE
      )
      return()
    }
    
    code_choices <- import_data %>%
      filter(mineral == input$mineral) %>%
      distinct(hs_code, hs_description) %>%
      filter(!is.na(hs_code), hs_code != "", hs_code != "Not reported") %>%
      arrange(hs_code) %>%
      mutate(
        label = paste0(hs_code, " — ", str_trunc(hs_description, width = 85))
      )
    
    if (nrow(code_choices) == 0) {
      choice_values <- c("All HS Codes" = "All HS Codes")
    } else {
      choice_values <- c("All HS Codes" = "All HS Codes", code_choices$hs_code)
      names(choice_values)[-1] <- code_choices$label
    }
    
    current_choice <- isolate(input$hs_codes)
    selected_choice <- "All HS Codes"
    if (!is.null(current_choice)) {
      current_valid <- current_choice[current_choice %in% choice_values]
      if (length(current_valid) > 0) selected_choice <- current_valid
    }
    
    updateSelectizeInput(
      session,
      "hs_codes",
      choices = choice_values,
      selected = selected_choice,
      server = TRUE
    )
  })
  
  hs_status_html <- reactive({
    paste0(
      "<p><strong>HS-code filter:</strong> ", selected_hs_code_label(), "</p>"
    )
  })
  
  for (one_id in c("hs_code_status_exports", "hs_code_status_trade", "hs_code_status_map", "hs_code_status_country")) {
    local({
      this_id <- one_id
      output[[this_id]] <- renderUI({ HTML(hs_status_html()) })
      outputOptions(output, this_id, suspendWhenHidden = FALSE)
    })
  }
  
  selection_status <- reactive({
    req(input$mineral, input$data_type, input$year)
    notes <- character()
    status_class <- "warning"
    
    exact_main_rows <- project_data %>%
      filter(
        mineral == input$mineral,
        data_type == input$data_type,
        year == selected_year()
      ) %>%
      nrow()
    
    if (nrow(selected_base_data()) == 0 || nrow(selected_data()) == 0) {
      status_class <- "danger"
      notes <- c(
        notes,
        paste0(
          "No ", input$data_type, " data are available for ", input$mineral,
          " in the prepared project data."
        )
      )
    } else if (exact_main_rows == 0) {
      notes <- c(
        notes,
        paste0(
          "No ", input$data_type, " data are available for ", input$mineral,
          " in ", selected_year(), ". Showing the latest available year instead: ",
          actual_data_year(), "."
        )
      )
    }
    
    if (nrow(selected_data()) > 0 && nrow(selected_data()) <= 2) {
      notes <- c(
        notes,
        paste0(
          "Sparse coverage warning: this selection has only ", nrow(selected_data()),
          " reported country record(s). Rankings, maps, and concentration metrics may reflect source-data availability rather than complete global supply."
        )
      )
    }
    
    if (import_available()) {
      exact_trade_rows <- filtered_import_data() %>%
        filter(year == selected_year()) %>%
        nrow()
      
      if (exact_trade_rows == 0) {
        available_trade_years <- filtered_import_data() %>%
          pull(year) %>%
          unique() %>%
          sort()
        usable_trade_years <- available_trade_years[available_trade_years <= selected_year()]
        
        if (length(usable_trade_years) > 0) {
          notes <- c(
            notes,
            paste0(
              "CEPII/BACI trade data are not available for ", input$mineral,
              " in ", selected_year(), ". Trade tabs use ", max(usable_trade_years),
              " instead."
            )
          )
        } else if (length(available_trade_years) > 0) {
          notes <- c(
            notes,
            paste0(
              "CEPII/BACI trade data are not available for ", input$mineral,
              " at or before ", selected_year(), "."
            )
          )
        }
      }
    }
    
    if (length(notes) == 0) return(NULL)
    list(class = status_class, html = paste(notes, collapse = "<br>"))
  })
  
  render_selection_status <- function() {
    renderUI({
      status <- selection_status()
      if (is.null(status)) return(NULL)
      div(
        class = paste("selection-alert", status$class),
        HTML(status$html)
      )
    })
  }
  
  status_output_ids <- c(
    "selection_status_overview",
    "selection_status_map",
    "selection_status_rankings",
    "selection_status_compare",
    "selection_status_concentration",
    "selection_status_trade",
    "selection_status_trade_map",
    "selection_status_importer_exporter_map",
    "selection_status_country_profile",
    "selection_status_methods",
    "selection_status_data"
  )
  
  for (one_id in status_output_ids) {
    local({
      this_id <- one_id
      output[[this_id]] <- render_selection_status()
      outputOptions(output, this_id, suspendWhenHidden = FALSE)
    })
  }
  
  observe({
    req(input$mineral, input$data_type)
    
    all_country_pool <- selected_base_data() %>%
      arrange(country_clean) %>%
      pull(country_clean) %>%
      unique()
    
    default_countries <- selected_data() %>%
      slice_max(order_by = value_standard, n = 5, with_ties = FALSE) %>%
      pull(country_clean)
    
    updateSelectizeInput(
      session,
      "compare_countries",
      choices = all_country_pool,
      selected = default_countries,
      server = TRUE
    )
  })
  

  concentration_for_data <- function(df) {
    if (nrow(df) == 0 || sum(df$value_standard, na.rm = TRUE) <= 0) {
      return(tibble())
    }
    
    df_ranked <- df %>%
      arrange(desc(value_standard)) %>%
      mutate(share = value_standard / sum(value_standard, na.rm = TRUE))
    
    tibble(
      countries = nrow(df_ranked),
      top_1_share = sum(head(df_ranked$share, 1), na.rm = TRUE),
      top_3_share = sum(head(df_ranked$share, 3), na.rm = TRUE),
      top_5_share = sum(head(df_ranked$share, 5), na.rm = TRUE),
      hhi = sum((df_ranked$share * 100)^2, na.rm = TRUE)
    )
  }
  
  output$country_count <- renderText({
    nrow(selected_data())
  })
  
  output$top_country <- renderText({
    df <- selected_data()
    if (nrow(df) == 0) return("No data")
    df$country_clean[1]
  })
  
  output$top_share <- renderText({
    df <- selected_data()
    if (nrow(df) == 0 || sum(df$value_standard, na.rm = TRUE) == 0) return("No data")
    percent(df$value_standard[1] / sum(df$value_standard, na.rm = TRUE), accuracy = 0.1)
  })
  
  output$top3_share <- renderText({
    metrics <- concentration_for_data(selected_data())
    if (nrow(metrics) == 0) return("No data")
    percent(metrics$top_3_share, accuracy = 0.1)
  })
  
  output$summary_text <- renderText({
    df <- selected_data()
    if (nrow(df) == 0) {
      return(paste(
        "No", input$data_type,
        "records were found for", input$mineral,
        "at or before", selected_year(),
        "in project_data.csv."
      ))
    }
    
    total_value <- sum(df$value_standard, na.rm = TRUE)
    top_value <- df$value_standard[1]
    top_share <- top_value / total_value
    source_label <- paste(unique(df$source), collapse = "; ")
    
    year_note <- ifelse(
      selected_year() != actual_data_year(),
      paste0(" You selected ", selected_year(), ", but the latest available year at or before that selection is ", actual_data_year(), "."),
      ""
    )
    
    paste0(
      "The dashboard is showing ", input$data_type, " records for ", input$mineral,
      " using ", actual_data_year(), " data from ", source_label, ".",
      year_note,
      " The selected dataset has ", nrow(df), " mapped country records. ",
      "The largest reported country is ", df$country_clean[1],
      ", with ", comma(top_value), " standardized metric tons, or about ",
      percent(top_share, accuracy = 0.1),
      " of the mapped total in this dataset."
    )
  })
  
  output$coverage_note <- renderText({
    df <- selected_data()
    
    if (nrow(df) == 0) {
      return("Coverage note: this mineral-data-type-year combination has no usable records in the prepared CSV.")
    }
    
    duplicate_n <- sum(df$duplicate_group_n > 1, na.rm = TRUE)
    duplicate_note <- ifelse(
      duplicate_n > 0,
      paste0(" ", duplicate_n, " displayed country records had duplicate raw rows; the largest standardized value is kept and the raw rows are flagged in Data & Sources."),
      ""
    )
    
    rare_earth_note <- ifelse(
      input$mineral == "Rare Earths",
      " Rare Earths are aggregated rather than shown element-by-element.",
      ""
    )
    
    sparse_note <- ifelse(
      nrow(df) <= 2,
      " Sparse coverage warning: this view has very limited reported country coverage. Rankings and concentration metrics may reflect source-data availability rather than complete global supply.",
      " Missing countries may indicate no reported production/reserves, unavailable data, withheld data, or countries grouped into an 'Other' category."
    )
    
    paste0("Coverage note:", rare_earth_note, sparse_note, duplicate_note)
  })
  
  output$unit_note <- renderText({
    metric_note(input$data_type)
  })
  
  output$selection_summary_table <- renderDT({
    df <- selected_data()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No selected records found."), rownames = FALSE))
    }
    
    metrics <- concentration_for_data(df)
    
    tibble(
      metric = c(
        "Mineral", "Data type", "Displayed data year", "Countries",
        "Top country", "Top country share", "Top 3 share", "Top 5 share", "HHI"
      ),
      value = c(
        input$mineral,
        input$data_type,
        as.character(actual_data_year()),
        as.character(nrow(df)),
        df$country_clean[1],
        percent(metrics$top_1_share, accuracy = 0.1),
        percent(metrics$top_3_share, accuracy = 0.1),
        percent(metrics$top_5_share, accuracy = 0.1),
        comma(metrics$hhi, accuracy = 1)
      )
    ) %>%
      datatable(
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      )
  })
  
  output$world_map <- renderPlotly({
    df <- selected_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "No country-level records found for this selection.")
    )
    
    axis_info <- axis_scale_details(df$value_standard, "metric tons")
    df <- df %>% mutate(value_axis = value_standard / axis_info$divisor)
    p <- plot_ly(
      data = df,
      type = "choropleth",
      locations = ~iso3,
      z = ~value_axis,
      text = ~paste0(
        country_clean,
        "<br>Standardized value: ", comma(value_standard), " metric tons",
        "<br>Original value: ", comma(value), " ", unit,
        "<br>Data type: ", data_type,
        "<br>Data year used: ", year,
        "<br>Source: ", source,
        "<br>Commodity: ", commodity,
        "<br>Record: ", statistic,
        "<br>Duplicate flag: ", duplicate_flag
      ),
      hoverinfo = "text",
      colorscale = "Viridis",
      marker = list(line = list(color = "white", width = 0.4)),
      colorbar = list(title = axis_info$unit_label)
    )
    
    p %>%
      layout(
        title = paste(input$mineral, str_to_lower(input$data_type), "map", "(", actual_data_year(), ")"),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth")
        ),
        margin = list(l = 0, r = 0, t = 50, b = 0)
      )
  })
  
  output$bar_chart <- renderPlot({
    df <- selected_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "No country-level records found for this selection.")
    )
    
    axis_info <- axis_scale_details(df$value_standard, "metric tons")
    
    top_df <- df %>%
      slice_max(order_by = value_standard, n = input$top_n, with_ties = FALSE) %>%
      mutate(
        country_clean = fct_reorder(country_clean, value_standard),
        value_axis = value_standard / axis_info$divisor
      )
    
    ggplot(top_df, aes(x = country_clean, y = value_axis)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = axis_label_function(axis_info)) +
      labs(
        title = paste("Largest", input$top_n, "countries by", str_to_lower(input$data_type), "-", input$mineral, actual_data_year()),
        subtitle = ifelse(
          selected_year() != actual_data_year(),
          paste("Selected", selected_year(), "but latest available year at or before that selection is", actual_data_year()),
          metric_note(input$data_type)
        ),
        x = "Country",
        y = paste0("Standardized value (", axis_info$unit_label, ")")
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$top_country_table <- renderDT({
    df <- selected_data()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No selected records found."), rownames = FALSE))
    }
    
    total_value <- sum(df$value_standard, na.rm = TRUE)
    
    df %>%
      arrange(desc(value_standard)) %>%
      mutate(
        rank = row_number(),
        share = value_standard / total_value,
        duplicate = if_else(duplicate_group_n > 1, "Flagged", "No")
      ) %>%
      select(
        rank,
        country = country_clean,
        iso3,
        year,
        value_metric_tons = value_standard,
        share,
        source,
        statistic,
        duplicate
      ) %>%
      mutate(
        value_metric_tons = format_number_for_table(value_metric_tons),
        share = percent(share, accuracy = 0.1)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  comparison_data <- reactive({
    req(input$compare_countries)
    
    # Shares and ranks are calculated against all mapped countries for the
    # selected mineral and data type, not just against the displayed countries.
    selected_base_data() %>%
      group_by(year) %>%
      mutate(
        year_total = sum(value_standard, na.rm = TRUE),
        rank = dense_rank(desc(value_standard)),
        share = value_standard / year_total
      ) %>%
      ungroup() %>%
      filter(country_clean %in% input$compare_countries)
  })
  
  output$country_trend <- renderPlot({
    df <- comparison_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select at least one country with available records.")
    )
    
    axis_info <- axis_scale_details(df$value_standard, "metric tons")
    df <- df %>% mutate(value_axis = value_standard / axis_info$divisor)
    
    ggplot(df, aes(x = year, y = value_axis, group = country_clean, color = country_clean)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = axis_label_function(axis_info)) +
      scale_x_continuous(breaks = sort(unique(df$year))) +
      labs(
        title = paste("Country comparison:", input$mineral, str_to_lower(input$data_type)),
        x = "Year",
        y = paste0("Standardized value (", axis_info$unit_label, ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$country_comparison_table <- renderDT({
    df <- comparison_data()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No comparison records found."), rownames = FALSE))
    }
    
    df %>%
      arrange(year, rank, country_clean) %>%
      select(
        year,
        country = country_clean,
        iso3,
        rank,
        value_metric_tons = value_standard,
        share,
        original_value = value,
        original_unit = unit,
        duplicate_flag
      ) %>%
      mutate(
        value_metric_tons = format_number_for_table(value_metric_tons),
        share = percent(share, accuracy = 0.1),
        original_value = format_number_for_table(original_value)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  concentration_all_years <- reactive({
    df <- selected_base_data()
    
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      group_by(year) %>%
      arrange(desc(value_standard), .by_group = TRUE) %>%
      mutate(
        year_total = sum(value_standard, na.rm = TRUE),
        share = value_standard / year_total,
        rank = row_number()
      ) %>%
      summarise(
        countries = n(),
        total_metric_tons = sum(value_standard, na.rm = TRUE),
        top_1_country = country_clean[rank == 1][1],
        top_1_share = sum(share[rank <= 1], na.rm = TRUE),
        top_3_share = sum(share[rank <= 3], na.rm = TRUE),
        top_5_share = sum(share[rank <= 5], na.rm = TRUE),
        hhi = sum((share * 100)^2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  output$concentration_trend <- renderPlot({
    df <- concentration_all_years()
    shiny::validate(
      shiny::need(nrow(df) > 0, "No concentration metrics available for this selection.")
    )
    
    trend_df <- df %>%
      select(year, top_1_share, top_3_share, top_5_share) %>%
      pivot_longer(
        cols = c(top_1_share, top_3_share, top_5_share),
        names_to = "metric",
        values_to = "share"
      ) %>%
      mutate(
        metric = recode(
          metric,
          top_1_share = "Top 1 share",
          top_3_share = "Top 3 share",
          top_5_share = "Top 5 share"
        )
      )
    
    ggplot(trend_df, aes(x = year, y = share, group = metric, color = metric)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_x_continuous(breaks = sort(unique(trend_df$year))) +
      labs(
        title = paste("Supply concentration over time:", input$mineral, str_to_lower(input$data_type)),
        x = "Year",
        y = "Share of mapped total"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$concentration_table <- renderDT({
    df <- concentration_all_years()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No concentration records found."), rownames = FALSE))
    }
    
    df %>%
      mutate(
        total_metric_tons = format_number_for_table(total_metric_tons),
        top_1_share = percent(top_1_share, accuracy = 0.1),
        top_3_share = percent(top_3_share, accuracy = 0.1),
        top_5_share = percent(top_5_share, accuracy = 0.1),
        hhi = comma(hhi, accuracy = 1)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  # -------------------------------------------------------------------------
  # Shared bilateral trade-data helper
  # -------------------------------------------------------------------------
  
  import_available <- reactive({
    nrow(import_data) > 0
  })
  
  # -------------------------------------------------------------------------
  # Producer exports tab
  # -------------------------------------------------------------------------
  
  export_year <- reactive({
    if (!import_available()) return(NA_integer_)
    available_years <- filtered_import_data() %>%
      pull(year) %>%
      unique() %>%
      sort()
    usable_years <- available_years[available_years <= selected_year()]
    if (length(usable_years) == 0) return(NA_integer_)
    max(usable_years)
  })
  
  main_producers_for_exports <- reactive({
    req(input$mineral)
    
    production_pool <- project_data %>%
      filter(
        mineral == input$mineral,
        data_type == "Production"
      )
    
    if (nrow(production_pool) == 0) return(empty_project_data())
    
    available_years <- sort(unique(production_pool$year))
    usable_years <- available_years[available_years <= selected_year()]
    
    if (length(usable_years) == 0) return(empty_project_data())
    
    producer_year <- max(usable_years)
    n_to_use <- ifelse(is.null(input$export_producer_count), 5, input$export_producer_count)
    
    production_pool %>%
      filter(year == producer_year) %>%
      arrange(desc(value_standard)) %>%
      slice_head(n = n_to_use)
  })
  
  selected_export_data <- reactive({
    if (!import_available() || is.na(export_year())) return(empty_import_data())
    
    producers <- main_producers_for_exports() %>%
      pull(iso3) %>%
      unique()
    
    if (length(producers) == 0) return(empty_import_data())
    
    filtered_import_data() %>%
      filter(
        year == export_year(),
        exporter_iso3 %in% producers
      )
  })
  
  output$export_status <- renderUI({
    if (!import_available()) {
      HTML(paste0(
        "<p><strong>No import_data.csv found or the file could not be standardized.</strong></p>",
        "<p>Add the static BACI extract at <code>data/import_data.csv</code>. ",
        "This tab will then show export destinations for the highest-producing countries in the production dataset.</p>"
      ))
    } else {
      HTML(paste0(
        "<p>Current trade year used: ", ifelse(is.na(export_year()), "No matching year", export_year()), ".</p>",
        "<p>Exporters are screened from the top production countries for ", input$mineral, ".</p>"
      ))
    }
  })
  
  output$export_producer_screen_table <- renderDT({
    df <- main_producers_for_exports()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No production records available for this mineral/year."), rownames = FALSE))
    }
    
    full_year_total <- project_data %>%
      filter(
        mineral == input$mineral,
        data_type == "Production",
        year == unique(df$year)[1]
      ) %>%
      summarise(total = sum(value_standard, na.rm = TRUE)) %>%
      pull(total)
    
    df %>%
      mutate(
        producer_rank = row_number(),
        share_of_mapped_production = value_standard / full_year_total
      ) %>%
      select(
        producer_rank,
        producer_country = country_clean,
        producer_iso3 = iso3,
        production_year = year,
        production_metric_tons = value_standard,
        share_of_mapped_production
      ) %>%
      mutate(
        production_metric_tons = format_number_for_table(production_metric_tons),
        share_of_mapped_production = percent(share_of_mapped_production, accuracy = 0.1)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$export_destination_chart <- renderPlot({
    df <- selected_export_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this chart."),
      shiny::need(nrow(df) > 0, "No export flows found from the selected top producers for this mineral/year.")
    )
    
    top_destinations_raw <- df %>%
      group_by(importer_country) %>%
      summarise(
        export_value_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(export_value_usd)) %>%
      slice_head(n = input$top_n)
    
    axis_info <- axis_scale_details(top_destinations_raw$export_value_usd, "USD")
    top_destinations <- top_destinations_raw %>%
      mutate(
        importer_country = fct_reorder(importer_country, export_value_usd),
        export_value_axis = export_value_usd / axis_info$divisor
      )
    
    ggplot(top_destinations, aes(x = importer_country, y = export_value_axis)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = axis_label_function(axis_info)) +
      labs(
        title = paste("Largest destinations for exports from top", input$mineral, "producers"),
        subtitle = paste("Trade data year:", export_year(), "| values shown in", axis_info$unit_label),
        x = "Destination/importing country",
        y = paste0("Export flow value (", axis_info$unit_label, ")")
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$producer_export_chart <- renderPlot({
    df <- selected_export_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this chart."),
      shiny::need(nrow(df) > 0, "No export flows found from the selected top producers for this mineral/year.")
    )
    
    producer_exports_raw <- df %>%
      group_by(exporter_country) %>%
      summarise(
        export_value_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(share = export_value_usd / sum(export_value_usd, na.rm = TRUE)) %>%
      arrange(desc(export_value_usd))
    
    axis_info <- axis_scale_details(producer_exports_raw$export_value_usd, "USD")
    producer_exports <- producer_exports_raw %>%
      mutate(
        exporter_country = fct_reorder(exporter_country, export_value_usd),
        export_value_axis = export_value_usd / axis_info$divisor
      )
    
    ggplot(producer_exports, aes(x = exporter_country, y = export_value_axis)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = axis_label_function(axis_info)) +
      labs(
        title = paste("Export value from top", input$mineral, "producer countries"),
        subtitle = paste("Trade data year:", export_year(), "| values shown in", axis_info$unit_label),
        x = "Producer/exporting country",
        y = paste0("Export flow value (", axis_info$unit_label, ")")
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$producer_export_table <- renderDT({
    df <- selected_export_data()
    
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to activate the producer export-flow table."), rownames = FALSE))
    }
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No export flows found from the selected top producers for this mineral/year."), rownames = FALSE))
    }
    
    exporter_totals <- df %>%
      group_by(exporter_country) %>%
      summarise(
        producer_total_exports_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      )
    
    df %>%
      group_by(
        exporter_country,
        exporter_iso3,
        importer_country,
        importer_iso3,
        mineral,
        year,
        hs_code,
        hs_description,
        source
      ) %>%
      summarise(
        export_value_usd = sum(import_value_usd, na.rm = TRUE),
        quantity = sum(quantity, na.rm = TRUE),
        quantity_unit = paste(unique(quantity_unit), collapse = "; "),
        .groups = "drop"
      ) %>%
      left_join(exporter_totals, by = "exporter_country") %>%
      mutate(
        share_of_producer_exports = export_value_usd / producer_total_exports_usd
      ) %>%
      arrange(desc(export_value_usd)) %>%
      mutate(
        export_value_usd = format_dollar_for_table(export_value_usd),
        producer_total_exports_usd = format_dollar_for_table(producer_total_exports_usd),
        share_of_producer_exports = percent(share_of_producer_exports, accuracy = 0.1),
        quantity = ifelse(quantity == 0, NA_character_, format_number_for_table(quantity))
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  # -------------------------------------------------------------------------
  # Import-dependence tab
  # -------------------------------------------------------------------------
  
  import_year <- reactive({
    if (!import_available()) return(NA_integer_)
    available_years <- filtered_import_data() %>%
      pull(year) %>%
      unique() %>%
      sort()
    usable_years <- available_years[available_years <= selected_year()]
    if (length(usable_years) == 0) return(NA_integer_)
    max(usable_years)
  })
  
  main_producers_for_imports <- reactive({
    req(input$mineral)
    
    production_pool <- project_data %>%
      filter(
        mineral == input$mineral,
        data_type == "Production"
      )
    
    if (nrow(production_pool) == 0) return(empty_project_data())
    
    available_years <- sort(unique(production_pool$year))
    usable_years <- available_years[available_years <= selected_year()]
    
    if (length(usable_years) == 0) return(empty_project_data())
    
    producer_year <- max(usable_years)
    
    production_pool %>%
      filter(year == producer_year) %>%
      arrange(desc(value_standard)) %>%
      slice_head(n = input$producer_count)
  })
  
  selected_import_data <- reactive({
    if (!import_available() || is.na(import_year())) return(empty_import_data())
    
    df <- filtered_import_data() %>%
      filter(year == import_year())
    
    if (isTRUE(input$only_top_producer_imports)) {
      producers <- main_producers_for_imports() %>%
        pull(iso3) %>%
        unique()
      df <- df %>% filter(exporter_iso3 %in% producers)
    }
    
    df
  })
  
  trade_flow_map_data <- reactive({
    if (!import_available() || is.na(import_year())) return(empty_import_data())
    
    # The standalone Trade Flow Map should show global bilateral flows for
    # the selected mineral/year/HS code filter. It intentionally ignores the
    # Trade Dependence tab's "Focus on imports from these producer countries"
    # checkbox so opening the map tab starts from the world-flow view.
    filtered_import_data() %>%
      filter(year == import_year())
  })
  
  output$import_status <- renderUI({
    if (!import_available()) {
      HTML(paste0(
        "<p><strong>No import_data.csv found or the file could not be standardized.</strong></p>",
        "<p>Add a static bilateral trade file at <code>data/import_data.csv</code>. ",
        "The app expects one row per importer-exporter-mineral-product-year flow. ",
        "Once added, this tab will show which countries import from the main producers identified in the production data.</p>"
      ))
    } else {
      HTML(paste0(
        "<p>Current import year used: ", ifelse(is.na(import_year()), "No matching year", import_year()), ".</p>",
        "<p>Trade flows use the selected mineral, year, and HS-code filter.</p>"
      ))
    }
  })
  
  output$import_status_map <- renderUI({
    if (!import_available()) {
      HTML(paste0(
        "<p><strong>No import_data.csv found or the file could not be standardized.</strong></p>",
        "<p>Add a static bilateral trade file at <code>data/import_data.csv</code> to activate the map.</p>"
      ))
    } else {
      HTML(paste0(
        "<p>Current trade year used: ", ifelse(is.na(import_year()), "No matching year", import_year()), ".</p>",
        "<p>Map shows global bilateral flows for the selected mineral, year, and HS-code filter.</p>"
      ))
    }
  })
  
  output$producer_screen_table <- renderDT({
    df <- main_producers_for_imports()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No production records available for this mineral/year."), rownames = FALSE))
    }
    
    total_value <- sum(df$value_standard, na.rm = TRUE)
    
    df %>%
      mutate(
        producer_rank = row_number(),
        producer_screen_share = value_standard / total_value
      ) %>%
      select(
        producer_rank,
        exporter_country = country_clean,
        exporter_iso3 = iso3,
        production_year = year,
        production_metric_tons = value_standard,
        producer_screen_share
      ) %>%
      mutate(
        production_metric_tons = format_number_for_table(production_metric_tons),
        producer_screen_share = percent(producer_screen_share, accuracy = 0.1)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$importer_bar_chart <- renderPlot({
    df <- selected_import_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this chart."),
      shiny::need(nrow(df) > 0, "No trade flows found for this mineral/year/filter.")
    )
    
    top_importers_raw <- df %>%
      group_by(importer_country) %>%
      summarise(
        import_value_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(import_value_usd)) %>%
      slice_head(n = input$top_n)
    
    axis_info <- axis_scale_details(top_importers_raw$import_value_usd, "USD")
    top_importers <- top_importers_raw %>%
      mutate(
        importer_country = fct_reorder(importer_country, import_value_usd),
        import_value_axis = import_value_usd / axis_info$divisor
      )
    
    ggplot(top_importers, aes(x = importer_country, y = import_value_axis)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = axis_label_function(axis_info)) +
      labs(
        title = paste("Largest importers from selected", input$mineral, "producer countries"),
        subtitle = paste("Import data year:", import_year(), "| values shown in", axis_info$unit_label),
        x = "Importing country",
        y = paste0("Import value (", axis_info$unit_label, ")")
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$exporter_import_chart <- renderPlot({
    df <- selected_import_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this chart."),
      shiny::need(nrow(df) > 0, "No global trade flows found for this mineral/year/HS-code filter.")
    )
    
    exporter_df <- df %>%
      group_by(exporter_country) %>%
      summarise(
        import_value_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(share = import_value_usd / sum(import_value_usd, na.rm = TRUE)) %>%
      arrange(desc(import_value_usd)) %>%
      slice_head(n = input$top_n) %>%
      mutate(exporter_country = fct_reorder(exporter_country, import_value_usd))
    
    ggplot(exporter_df, aes(x = exporter_country, y = share)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste("Producer-source concentration in", input$mineral, "import flows"),
        subtitle = paste("Import data year:", import_year()),
        x = "Exporting/source country",
        y = "Share of selected import value"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$trade_flow_map <- renderPlotly({
    df <- trade_flow_map_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this map."),
      shiny::need(nrow(country_centroids) > 0, "Install the maps package to enable country centroids for the trade-flow map: install.packages('maps')."),
      shiny::need(nrow(df) > 0, "No global trade flows found for this mineral/year/HS-code filter.")
    )
    
    n_links <- ifelse(is.null(input$trade_map_links), 25, input$trade_map_links)
    
    flows <- df %>%
      group_by(
        exporter_country, exporter_iso3,
        importer_country, importer_iso3
      ) %>%
      summarise(
        trade_value_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(trade_value_usd)) %>%
      slice_head(n = n_links) %>%
      left_join(
        country_centroids %>%
          select(exporter_iso3 = iso3, exporter_lon = lon, exporter_lat = lat),
        by = "exporter_iso3"
      ) %>%
      left_join(
        country_centroids %>%
          select(importer_iso3 = iso3, importer_lon = lon, importer_lat = lat),
        by = "importer_iso3"
      ) %>%
      filter(
        !is.na(exporter_lon), !is.na(exporter_lat),
        !is.na(importer_lon), !is.na(importer_lat)
      )
    
    shiny::validate(
      shiny::need(nrow(flows) > 0, "The selected trade flows do not have enough country coordinates to draw lines.")
    )
    
    max_value <- max(flows$trade_value_usd, na.rm = TRUE)
    axis_info <- axis_scale_details(flows$trade_value_usd, "USD")
    flows <- flows %>%
      mutate(
        line_width = 1.2 + 7 * sqrt(trade_value_usd / max_value),
        hover_text = paste0(
          exporter_country, " → ", importer_country,
          "<br>Trade value: $", comma(trade_value_usd, accuracy = 1),
          "<br>Year: ", import_year(),
          "<br>Mineral category: ", input$mineral
        )
      )
    
    endpoints <- bind_rows(
      flows %>%
        transmute(
          country = exporter_country,
          iso3 = exporter_iso3,
          lon = exporter_lon,
          lat = exporter_lat,
          role = "Exporter/source"
        ),
      flows %>%
        transmute(
          country = importer_country,
          iso3 = importer_iso3,
          lon = importer_lon,
          lat = importer_lat,
          role = "Importer/destination"
        )
    ) %>%
      distinct(country, iso3, lon, lat, role) %>%
      mutate(
        hover_text = paste0(country, "<br>", role)
      )
    
    p <- plot_geo()
    
    for (i in seq_len(nrow(flows))) {
      p <- p %>%
        add_trace(
          type = "scattergeo",
          mode = "lines",
          lon = c(flows$exporter_lon[i], flows$importer_lon[i]),
          lat = c(flows$exporter_lat[i], flows$importer_lat[i]),
          text = c(flows$hover_text[i], flows$hover_text[i]),
          hoverinfo = "text",
          line = list(width = flows$line_width[i], color = "rgba(37, 99, 235, 0.45)"),
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>%
      add_trace(
        data = endpoints,
        type = "scattergeo",
        mode = "markers",
        lon = ~lon,
        lat = ~lat,
        text = ~hover_text,
        hoverinfo = "skip",
        hovertemplate = NULL,
        marker = list(size = 7, color = "#0f766e", line = list(width = 0.6, color = "white")),
        showlegend = FALSE
      )
    
    p %>%
      layout(
        title = paste0(
          "Top bilateral ", input$mineral, " trade links, ", import_year(),
          " (values shown by line thickness; hover for USD value)"
        ),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          showcountries = TRUE,
          projection = list(type = "natural earth")
        ),
        margin = list(l = 0, r = 0, t = 60, b = 0)
      )
  })
  
  output$trade_party_map <- renderPlotly({
    df <- trade_flow_map_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this map."),
      shiny::need(nrow(df) > 0, "No global trade flows found for this mineral/year/HS-code filter.")
    )
    
    map_role <- ifelse(is.null(input$trade_party_map_role), "Importers", input$trade_party_map_role)
    
    if (map_role == "Exporters") {
      map_df <- df %>%
        group_by(country = exporter_country, iso3 = exporter_iso3) %>%
        summarise(trade_value_usd = sum(import_value_usd, na.rm = TRUE), .groups = "drop")
      role_label <- "Exporters"
      value_label <- "Export value"
    } else {
      map_df <- df %>%
        group_by(country = importer_country, iso3 = importer_iso3) %>%
        summarise(trade_value_usd = sum(import_value_usd, na.rm = TRUE), .groups = "drop")
      role_label <- "Importers"
      value_label <- "Import value"
    }
    
    map_df <- map_df %>%
      filter(!is.na(iso3), iso3 != "", !is.na(trade_value_usd), trade_value_usd > 0)
    
    shiny::validate(
      shiny::need(nrow(map_df) > 0, "No importer/exporter country values are available for this selection.")
    )
    
    axis_info <- axis_scale_details(map_df$trade_value_usd, "USD")
    map_df <- map_df %>%
      mutate(
        trade_value_axis = trade_value_usd / axis_info$divisor,
        hover_text = paste0(
          country,
          "<br>", value_label, ": $", comma(trade_value_usd, accuracy = 1),
          "<br>Year: ", import_year(),
          "<br>Mineral category: ", input$mineral,
          "<br>HS-code filter: ", selected_hs_code_label()
        )
      )
    
    p <- plot_ly(
      data = map_df,
      type = "choropleth",
      locations = ~iso3,
      z = ~trade_value_axis,
      text = ~hover_text,
      hoverinfo = "text",
      colorscale = "Viridis",
      marker = list(line = list(color = "white", width = 0.4)),
      colorbar = list(title = axis_info$unit_label)
    )
    
    p %>%
      layout(
        title = paste0(role_label, " by selected ", input$mineral, " trade value, ", import_year(), " (", axis_info$unit_label, ")"),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          showcountries = TRUE,
          projection = list(type = "natural earth")
        ),
        margin = list(l = 0, r = 0, t = 60, b = 0)
      )
  })
  
  output$trade_flow_map_table <- renderDT({
    df <- trade_flow_map_data()
    
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to activate the trade-flow map table."), rownames = FALSE))
    }
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No global trade flows found for this mineral/year/HS-code filter."), rownames = FALSE))
    }
    
    n_links <- ifelse(is.null(input$trade_map_links), 25, input$trade_map_links)
    
    df %>%
      group_by(
        exporter_country, exporter_iso3,
        importer_country, importer_iso3
      ) %>%
      summarise(
        trade_value_usd = sum(import_value_usd, na.rm = TRUE),
        quantity = sum(quantity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(trade_value_usd)) %>%
      slice_head(n = n_links) %>%
      mutate(
        trade_value_usd = format_dollar_for_table(trade_value_usd),
        quantity = ifelse(quantity == 0, NA_character_, format_number_for_table(quantity))
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$import_dependence_table <- renderDT({
    df <- selected_import_data()
    
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to activate the bilateral import-dependence table."), rownames = FALSE))
    }
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No import flows found for this mineral/year/filter."), rownames = FALSE))
    }
    
    importer_totals <- filtered_import_data() %>%
      filter(year == import_year()) %>%
      group_by(importer_country) %>%
      summarise(
        importer_total_imports_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      )
    
    df %>%
      group_by(
        importer_country,
        importer_iso3,
        exporter_country,
        exporter_iso3,
        mineral,
        year,
        hs_code,
        hs_description,
        source
      ) %>%
      summarise(
        import_value_usd = sum(import_value_usd, na.rm = TRUE),
        quantity = sum(quantity, na.rm = TRUE),
        quantity_unit = paste(unique(quantity_unit), collapse = "; "),
        .groups = "drop"
      ) %>%
      left_join(importer_totals, by = "importer_country") %>%
      mutate(
        share_of_importer_mineral_imports = import_value_usd / importer_total_imports_usd
      ) %>%
      arrange(desc(import_value_usd)) %>%
      mutate(
        import_value_usd = format_dollar_for_table(import_value_usd),
        importer_total_imports_usd = format_dollar_for_table(importer_total_imports_usd),
        share_of_importer_mineral_imports = percent(share_of_importer_mineral_imports, accuracy = 0.1),
        quantity = ifelse(quantity == 0, NA_character_, format_number_for_table(quantity))
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$import_schema_table <- renderDT({
    tibble(
      column = c(
        "mineral", "year", "importer_country", "importer_iso3",
        "exporter_country", "exporter_iso3", "import_value_usd",
        "quantity", "quantity_unit", "hs_code", "hs_description", "source"
      ),
      required = c(
        "Yes", "Yes", "Yes", "Recommended",
        "Yes", "Recommended", "Yes",
        "No", "No", "Recommended", "Recommended", "Recommended"
      ),
      example = c(
        "Rare Earths", "2024", "Japan", "JPN", "China", "CHN",
        "125000000", "3200", "metric tons", "284690", "Rare-earth compounds", "UN Comtrade static extract"
      ),
      note = c(
        "Must match one of Boron, Rare Earths, Uranium, Zirconium.",
        "Use calendar year.",
        "Importing/reporting country.",
        "If missing, app tries to infer ISO3 from country name.",
        "Exporting/partner/source country.",
        "If missing, app tries to infer ISO3 from country name.",
        "Use U.S. dollars for comparability.",
        "Optional physical quantity.",
        "Optional physical unit.",
        "Use HS or other product code used in the static trade extract.",
        "Short product description.",
        "Source label for transparency."
      )
    ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  # -------------------------------------------------------------------------
  # Individual country profile tab
  # -------------------------------------------------------------------------
  
  observe({
    if (!import_available()) {
      updateSelectizeInput(
        session,
        "country_profile_country",
        choices = c("No countries available" = ""),
        selected = "",
        server = TRUE
      )
      return()
    }
    
    country_choices <- filtered_import_data() %>%
      transmute(country = importer_country, iso3 = importer_iso3) %>%
      bind_rows(
        filtered_import_data() %>%
          transmute(country = exporter_country, iso3 = exporter_iso3)
      ) %>%
      filter(!is.na(iso3), iso3 != "", !is.na(country), country != "") %>%
      arrange(country) %>%
      group_by(iso3) %>%
      summarise(country = first(country), .groups = "drop") %>%
      arrange(country)
    
    if (nrow(country_choices) == 0) {
      updateSelectizeInput(
        session,
        "country_profile_country",
        choices = c("No countries available" = ""),
        selected = "",
        server = TRUE
      )
      return()
    }
    
    choice_values <- country_choices$iso3
    names(choice_values) <- paste0(country_choices$country, " (", country_choices$iso3, ")")
    
    top_country_iso <- selected_data() %>%
      slice_head(n = 1) %>%
      pull(iso3)
    
    default_country <- ifelse(
      length(top_country_iso) > 0 && top_country_iso %in% choice_values,
      top_country_iso,
      choice_values[1]
    )
    
    current_choice <- isolate(input$country_profile_country)
    if (!is.null(current_choice) && current_choice %in% choice_values) {
      default_country <- current_choice
    }
    
    updateSelectizeInput(
      session,
      "country_profile_country",
      choices = choice_values,
      selected = default_country,
      options = list(
        placeholder = "Type a country name or ISO3 code",
        create = FALSE,
        openOnFocus = TRUE,
        maxOptions = 10000,
        maxItems = 1,
        searchField = c("label", "value"),
        selectOnTab = TRUE,
        closeAfterSelect = TRUE,
        plugins = list("clear_button")
      ),
      server = FALSE
    )
  })
  
  country_profile_name <- reactive({
    req(input$country_profile_country)
    name_from_trade <- filtered_import_data() %>%
      filter(
        importer_iso3 == input$country_profile_country |
          exporter_iso3 == input$country_profile_country
      ) %>%
      transmute(country = if_else(
        importer_iso3 == input$country_profile_country,
        importer_country,
        exporter_country
      )) %>%
      filter(!is.na(country), country != "") %>%
      distinct(country) %>%
      slice_head(n = 1) %>%
      pull(country)
    
    if (length(name_from_trade) > 0) return(name_from_trade)
    
    name_from_project <- project_data %>%
      filter(iso3 == input$country_profile_country) %>%
      pull(country_clean) %>%
      unique()
    
    if (length(name_from_project) > 0) return(name_from_project[1])
    input$country_profile_country
  })
  
  selected_country_profile_trade <- reactive({
    req(input$country_profile_country, input$country_profile_view)
    
    if (!import_available() || is.na(import_year())) return(empty_import_data())
    
    df <- filtered_import_data() %>%
      filter(year == import_year())
    
    if (input$country_profile_view == "Imports") {
      df <- df %>% filter(importer_iso3 == input$country_profile_country)
    } else {
      df <- df %>% filter(exporter_iso3 == input$country_profile_country)
    }
    
    df
  })
  
  output$country_profile_status <- renderUI({
    if (!import_available()) {
      return(HTML(paste0(
        "<p><strong>No import_data.csv found or the file could not be standardized.</strong></p>",
        "<p>Add <code>data/import_data.csv</code> to activate the country profile.</p>"
      )))
    }
    
    if (is.null(input$country_profile_country) || input$country_profile_country == "") {
      return(HTML("<p><strong>Choose a country to view its bilateral trade profile.</strong></p>"))
    }
    
    trade_year_label <- ifelse(is.na(import_year()), "No matching trade year", import_year())
    view_label <- ifelse(input$country_profile_view == "Imports", "imports into", "exports from")
    
    HTML(paste0(
      "<p><strong>", country_profile_name(), "</strong></p>",
      "<p>Showing ", view_label, " this country for <strong>", input$mineral,
      "</strong> using CEPII/BACI trade year <strong>", trade_year_label, "</strong>.</p>"
    ))
  })
  
  output$country_profile_summary_table <- renderDT({
    df <- selected_country_profile_trade()
    
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to activate the country profile."), rownames = FALSE))
    }
    
    if (is.null(input$country_profile_country) || input$country_profile_country == "") {
      return(datatable(tibble(note = "Choose a country to view its profile."), rownames = FALSE))
    }
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = paste("No", str_to_lower(input$country_profile_view), "flows found for this country, mineral, and selected year.")), rownames = FALSE))
    }
    
    partner_col <- ifelse(input$country_profile_view == "Imports", "exporter_country", "importer_country")
    partner_iso_col <- ifelse(input$country_profile_view == "Imports", "exporter_iso3", "importer_iso3")
    flow_label <- ifelse(input$country_profile_view == "Imports", "Total import value", "Total export value")
    partner_label <- ifelse(input$country_profile_view == "Imports", "Source countries", "Destination countries")
    
    partner_totals <- df %>%
      group_by(.data[[partner_col]]) %>%
      summarise(value_usd = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value_usd))
    
    total_value <- sum(df$import_value_usd, na.rm = TRUE)
    top_partner <- partner_totals[[partner_col]][1]
    top_partner_share <- partner_totals$value_usd[1] / total_value
    
    tibble(
      metric = c(
        "Country", "Mineral", "Trade view", "Trade year", flow_label,
        partner_label, "Top partner", "Top partner share", "HS6 product categories"
      ),
      value = c(
        country_profile_name(),
        input$mineral,
        input$country_profile_view,
        as.character(import_year()),
        dollar(total_value, accuracy = 1),
        as.character(n_distinct(df[[partner_iso_col]])),
        top_partner,
        percent(top_partner_share, accuracy = 0.1),
        as.character(n_distinct(df$hs_code))
      )
    ) %>%
      datatable(
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      )
  })
  
  output$country_profile_partner_chart <- renderPlot({
    df <- selected_country_profile_trade()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this chart."),
      shiny::need(!is.null(input$country_profile_country) && input$country_profile_country != "", "Choose a country to view its profile."),
      shiny::need(nrow(df) > 0, "No flows found for this country, mineral, and selected year.")
    )
    
    if (input$country_profile_view == "Imports") {
      partner_df <- df %>%
        group_by(partner_country = exporter_country) %>%
        summarise(value_usd = sum(import_value_usd, na.rm = TRUE), .groups = "drop")
      title_text <- paste("Top source countries for", country_profile_name(), input$mineral, "imports")
      x_label <- "Source/exporting country"
      y_label_prefix <- "Import value"
    } else {
      partner_df <- df %>%
        group_by(partner_country = importer_country) %>%
        summarise(value_usd = sum(import_value_usd, na.rm = TRUE), .groups = "drop")
      title_text <- paste("Top destination countries for", country_profile_name(), input$mineral, "exports")
      x_label <- "Destination/importing country"
      y_label_prefix <- "Export value"
    }
    
    partner_df <- partner_df %>%
      arrange(desc(value_usd)) %>%
      slice_head(n = input$top_n)
    
    axis_info <- axis_scale_details(partner_df$value_usd, "USD")
    partner_df <- partner_df %>%
      mutate(
        partner_country = fct_reorder(partner_country, value_usd),
        value_axis = value_usd / axis_info$divisor
      )
    
    ggplot(partner_df, aes(x = partner_country, y = value_axis)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = axis_label_function(axis_info)) +
      labs(
        title = title_text,
        subtitle = paste("Trade data year:", import_year(), "| values shown in", axis_info$unit_label),
        x = x_label,
        y = paste0(y_label_prefix, " (", axis_info$unit_label, ")")
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$country_profile_supply_table <- renderDT({
    req(input$country_profile_country)
    
    context <- project_data %>%
      filter(mineral == input$mineral) %>%
      group_by(data_type, year) %>%
      mutate(
        year_total = sum(value_standard, na.rm = TRUE),
        rank = dense_rank(desc(value_standard)),
        share = value_standard / year_total
      ) %>%
      ungroup() %>%
      filter(iso3 == input$country_profile_country) %>%
      arrange(data_type, year)
    
    if (nrow(context) == 0) {
      return(datatable(tibble(note = paste("No production or reserve records found for", country_profile_name(), "and", input$mineral, "in project_data.csv.")), rownames = FALSE))
    }
    
    context %>%
      select(
        mineral,
        country = country_clean,
        data_type,
        year,
        rank,
        share,
        value_metric_tons = value_standard,
        original_value = value,
        original_unit = unit,
        source,
        duplicate_flag
      ) %>%
      mutate(
        share = percent(share, accuracy = 0.1),
        value_metric_tons = format_number_for_table(value_metric_tons),
        original_value = format_number_for_table(original_value)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$country_profile_trade_map <- renderPlotly({
    df <- selected_country_profile_trade()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this map."),
      shiny::need(nrow(country_centroids) > 0, "Install the maps package to enable country centroids for the country trade-flow map: install.packages('maps')."),
      shiny::need(!is.null(input$country_profile_country) && input$country_profile_country != "", "Choose a country to view its trade-flow map."),
      shiny::need(nrow(df) > 0, "No flows found for this country, mineral, and selected year.")
    )
    
    n_links <- ifelse(is.null(input$country_profile_map_links), 25, input$country_profile_map_links)
    view_label <- ifelse(input$country_profile_view == "Imports", "imports into", "exports from")
    
    flows <- df %>%
      group_by(
        exporter_country, exporter_iso3,
        importer_country, importer_iso3
      ) %>%
      summarise(
        trade_value_usd = sum(import_value_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(trade_value_usd)) %>%
      slice_head(n = n_links) %>%
      left_join(
        country_centroids %>%
          select(exporter_iso3 = iso3, exporter_lon = lon, exporter_lat = lat),
        by = "exporter_iso3"
      ) %>%
      left_join(
        country_centroids %>%
          select(importer_iso3 = iso3, importer_lon = lon, importer_lat = lat),
        by = "importer_iso3"
      ) %>%
      filter(
        !is.na(exporter_lon), !is.na(exporter_lat),
        !is.na(importer_lon), !is.na(importer_lat)
      )
    
    shiny::validate(
      shiny::need(nrow(flows) > 0, "The selected country flows do not have enough country coordinates to draw lines.")
    )
    
    max_value <- max(flows$trade_value_usd, na.rm = TRUE)
    flows <- flows %>%
      mutate(
        line_width = 1.2 + 7 * sqrt(trade_value_usd / max_value),
        hover_text = paste0(
          exporter_country, " → ", importer_country,
          "<br>Trade value: $", comma(trade_value_usd, accuracy = 1),
          "<br>Year: ", import_year(),
          "<br>Mineral category: ", input$mineral,
          "<br>Country profile view: ", input$country_profile_view
        )
      )
    
    endpoints <- bind_rows(
      flows %>%
        transmute(
          country = exporter_country,
          iso3 = exporter_iso3,
          lon = exporter_lon,
          lat = exporter_lat,
          role = "Exporter/source"
        ),
      flows %>%
        transmute(
          country = importer_country,
          iso3 = importer_iso3,
          lon = importer_lon,
          lat = importer_lat,
          role = "Importer/destination"
        )
    ) %>%
      distinct(country, iso3, lon, lat, role) %>%
      mutate(
        point_size = if_else(iso3 == input$country_profile_country, 11, 7),
        hover_text = paste0(country, "<br>", role)
      )
    
    p <- plot_geo()
    
    for (i in seq_len(nrow(flows))) {
      p <- p %>%
        add_trace(
          type = "scattergeo",
          mode = "lines",
          lon = c(flows$exporter_lon[i], flows$importer_lon[i]),
          lat = c(flows$exporter_lat[i], flows$importer_lat[i]),
          text = c(flows$hover_text[i], flows$hover_text[i]),
          hoverinfo = "text",
          line = list(width = flows$line_width[i], color = "rgba(124, 58, 237, 0.45)"),
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p %>%
      add_trace(
        data = endpoints,
        type = "scattergeo",
        mode = "markers",
        lon = ~lon,
        lat = ~lat,
        text = ~hover_text,
        hoverinfo = "skip",
        hovertemplate = NULL,
        marker = list(size = ~point_size, color = "#0f766e", line = list(width = 0.7, color = "white")),
        showlegend = FALSE
      ) %>%
      layout(
        title = paste0(
          "Top ", input$mineral, " trade flows for ", country_profile_name(),
          " (", view_label, " country), ", import_year()
        ),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          showcountries = TRUE,
          projection = list(type = "natural earth")
        ),
        margin = list(l = 0, r = 0, t = 60, b = 0)
      )
  })
  
  output$country_profile_flow_table <- renderDT({
    df <- selected_country_profile_trade()
    
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to activate the country flow table."), rownames = FALSE))
    }
    
    if (is.null(input$country_profile_country) || input$country_profile_country == "") {
      return(datatable(tibble(note = "Choose a country to view its bilateral flows."), rownames = FALSE))
    }
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = paste("No", str_to_lower(input$country_profile_view), "flows found for this country, mineral, and selected year.")), rownames = FALSE))
    }
    
    if (input$country_profile_view == "Imports") {
      out <- df %>%
        group_by(
          source_country = exporter_country,
          source_iso3 = exporter_iso3,
          destination_country = importer_country,
          destination_iso3 = importer_iso3,
          mineral,
          year,
          hs_code,
          hs_description,
          source
        ) %>%
        summarise(
          trade_value_usd = sum(import_value_usd, na.rm = TRUE),
          quantity = sum(quantity, na.rm = TRUE),
          quantity_unit = paste(unique(quantity_unit), collapse = "; "),
          .groups = "drop"
        )
    } else {
      out <- df %>%
        group_by(
          source_country = exporter_country,
          source_iso3 = exporter_iso3,
          destination_country = importer_country,
          destination_iso3 = importer_iso3,
          mineral,
          year,
          hs_code,
          hs_description,
          source
        ) %>%
        summarise(
          trade_value_usd = sum(import_value_usd, na.rm = TRUE),
          quantity = sum(quantity, na.rm = TRUE),
          quantity_unit = paste(unique(quantity_unit), collapse = "; "),
          .groups = "drop"
        )
    }
    
    total_value <- sum(out$trade_value_usd, na.rm = TRUE)
    
    out %>%
      mutate(share_of_country_profile = trade_value_usd / total_value) %>%
      arrange(desc(trade_value_usd)) %>%
      mutate(
        trade_value_usd = format_dollar_for_table(trade_value_usd),
        share_of_country_profile = percent(share_of_country_profile, accuracy = 0.1),
        quantity = ifelse(quantity == 0, NA_character_, format_number_for_table(quantity))
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  # -------------------------------------------------------------------------
  # Additional methods, interpretation, alignment, and download outputs
  # -------------------------------------------------------------------------
  
  producer_trade_alignment_data <- reactive({
    req(input$mineral)
    
    production_pool <- project_data %>%
      filter(
        mineral == input$mineral,
        data_type == "Production"
      )
    
    if (nrow(production_pool) == 0) return(tibble())
    
    available_years <- sort(unique(production_pool$year))
    usable_years <- available_years[available_years <= selected_year()]
    if (length(usable_years) == 0) return(tibble())
    
    producer_year <- max(usable_years)
    production_ranks <- production_pool %>%
      filter(year == producer_year) %>%
      arrange(desc(value_standard)) %>%
      mutate(
        production_rank = row_number(),
        production_share = value_standard / sum(value_standard, na.rm = TRUE)
      )
    
    export_ranks <- filtered_import_data() %>%
      filter(year == export_year()) %>%
      group_by(exporter_iso3, exporter_country) %>%
      summarise(export_value_usd = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(export_value_usd)) %>%
      mutate(
        export_rank = row_number(),
        export_share = export_value_usd / sum(export_value_usd, na.rm = TRUE)
      )
    
    production_ranks %>%
      left_join(
        export_ranks,
        by = c("iso3" = "exporter_iso3")
      ) %>%
      mutate(
        exporter_country = if_else(is.na(exporter_country), country_clean, exporter_country),
        export_value_usd = replace_na(export_value_usd, 0),
        export_share = replace_na(export_share, 0),
        export_rank = if_else(is.na(export_rank), NA_integer_, as.integer(export_rank))
      ) %>%
      arrange(production_rank) %>%
      slice_head(n = input$top_n)
  })
  
  output$producer_trade_alignment_chart <- renderPlot({
    df <- producer_trade_alignment_data()
    shiny::validate(
      shiny::need(import_available(), "Add data/import_data.csv to activate this comparison."),
      shiny::need(nrow(df) > 0, "No producer/export alignment data available for this selection.")
    )
    
    plot_df <- df %>%
      select(country_clean, production_share, export_share) %>%
      pivot_longer(
        cols = c(production_share, export_share),
        names_to = "metric",
        values_to = "share"
      ) %>%
      mutate(
        metric = recode(
          metric,
          production_share = "Share of mapped production",
          export_share = "Share of selected export value"
        ),
        country_clean = fct_reorder(country_clean, share, .fun = max)
      )
    
    ggplot(plot_df, aes(x = country_clean, y = share, fill = metric)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste("Production share vs export-flow share:", input$mineral),
        subtitle = paste("Production year:", actual_data_year(), "| Trade year:", export_year(), "| HS filter:", selected_hs_code_label()),
        x = "Country",
        y = "Share",
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$producer_trade_alignment_table <- renderDT({
    df <- producer_trade_alignment_data()
    
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to activate the producer/export alignment table."), rownames = FALSE))
    }
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No producer/export alignment data available for this selection."), rownames = FALSE))
    }
    
    df %>%
      select(
        country = country_clean,
        iso3,
        production_year = year,
        production_rank,
        production_share,
        export_rank,
        export_share,
        export_value_usd
      ) %>%
      mutate(
        production_share = percent(production_share, accuracy = 0.1),
        export_share = percent(export_share, accuracy = 0.1),
        export_value_usd = format_dollar_for_table(export_value_usd),
        export_rank = ifelse(is.na(export_rank), "No selected exports", as.character(export_rank))
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$export_interpretation <- renderUI({
    df <- selected_export_data()
    if (!import_available()) return(HTML("<p>Add <code>data/import_data.csv</code> to activate export interpretation.</p>"))
    if (nrow(df) == 0) return(HTML("<p>No export flows are available for the current mineral, year, producer screen, and HS-code filter.</p>"))
    
    top_destination <- df %>%
      group_by(importer_country) %>%
      summarise(value = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value)) %>%
      slice_head(n = 1)
    top_exporter <- df %>%
      group_by(exporter_country) %>%
      summarise(value = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value)) %>%
      slice_head(n = 1)
    total_value <- sum(df$import_value_usd, na.rm = TRUE)
    
    HTML(paste0(
      "<p>Using ", export_year(), " BACI trade data and HS-code filter <strong>", selected_hs_code_label(), "</strong>, ",
      "the largest destination for export flows from the screened top producers is <strong>", top_destination$importer_country[1], "</strong> ",
      "with ", dollar(top_destination$value[1], accuracy = 1), " (", percent(top_destination$value[1] / total_value, accuracy = 0.1), " of selected export value). ",
      "The largest exporting/source country in this selected view is <strong>", top_exporter$exporter_country[1], "</strong>.</p>"
    ))
  })
  
  output$trade_interpretation <- renderUI({
    df <- selected_import_data()
    if (!import_available()) return(HTML("<p>Add <code>data/import_data.csv</code> to activate trade-dependence interpretation.</p>"))
    if (nrow(df) == 0) return(HTML("<p>No import flows are available for the current mineral, year, producer screen, and HS-code filter.</p>"))
    
    top_importer <- df %>%
      group_by(importer_country) %>%
      summarise(value = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value)) %>%
      slice_head(n = 1)
    top_source <- df %>%
      group_by(exporter_country) %>%
      summarise(value = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value)) %>%
      slice_head(n = 1)
    total_value <- sum(df$import_value_usd, na.rm = TRUE)
    
    HTML(paste0(
      "<p>Using ", import_year(), " BACI trade data and HS-code filter <strong>", selected_hs_code_label(), "</strong>, ",
      "the largest importer in the selected view is <strong>", top_importer$importer_country[1], "</strong> ",
      "with ", dollar(top_importer$value[1], accuracy = 1), " (", percent(top_importer$value[1] / total_value, accuracy = 0.1), " of selected import value). ",
      "The largest source/exporter in the selected view is <strong>", top_source$exporter_country[1], "</strong>.</p>"
    ))
  })
  
  output$country_profile_interpretation <- renderUI({
    df <- selected_country_profile_trade()
    if (!import_available()) return(HTML("<p>Add <code>data/import_data.csv</code> to activate country-level interpretation.</p>"))
    if (is.null(input$country_profile_country) || input$country_profile_country == "") return(HTML("<p>Choose a country to generate an interpretation.</p>"))
    if (nrow(df) == 0) return(HTML("<p>No bilateral flows are available for this country under the current mineral, year, view, and HS-code filter.</p>"))
    
    if (input$country_profile_view == "Imports") {
      partner <- df %>%
        group_by(partner = exporter_country) %>%
        summarise(value = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = 1)
      view_phrase <- "source country for imports into"
    } else {
      partner <- df %>%
        group_by(partner = importer_country) %>%
        summarise(value = sum(import_value_usd, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = 1)
      view_phrase <- "destination country for exports from"
    }
    total_value <- sum(df$import_value_usd, na.rm = TRUE)
    
    HTML(paste0(
      "<p>For <strong>", country_profile_name(), "</strong>, the largest ", view_phrase, " this country is <strong>", partner$partner[1], "</strong>. ",
      "That flow accounts for ", dollar(partner$value[1], accuracy = 1), " or ", percent(partner$value[1] / total_value, accuracy = 0.1),
      " of the selected country-profile trade value under HS-code filter <strong>", selected_hs_code_label(), "</strong>.</p>"
    ))
  })
  
  output$mineral_uses_table <- renderDT({
    mineral_uses <- tibble(
      `Mineral` = c(
        "Boron / Borates",
        "Rare Earths",
        "Uranium",
        "Zirconium / Hafnium"
      ),
      `Nuclear Energy Relevance` = c(
        "Boron compounds are relevant for neutron absorption, reactor control, shielding, and boron carbide applications.",
        "Rare Earths are not treated here as a single direct reactor input. They are included as a strategic mineral group used in magnets, batteries, phosphors, lasers, electronics, and advanced manufacturing connected to broader energy and defense supply chains.",
        "Uranium is the primary mined mineral input for nuclear fuel after milling, conversion, enrichment where required, and fuel fabrication.",
        "Zirconium alloys are used in nuclear fuel cladding. Hafnium is linked to zirconium supply chains and is used in nuclear control rods."
      ),
      `Dashboard Interpretation` = c(
        "Concentrated boron supply may matter for reactor-control and shielding-related input security.",
        "Rare-earth concentration should be interpreted as broader strategic industrial exposure rather than reactor-fuel dependence.",
        "Uranium concentration is directly relevant to nuclear fuel security and upstream fuel-cycle access.",
        "Zirconium and hafnium concentration matters for reactor-material and control-system supply chains, while the dashboard treats them cautiously because USGS links both to zircon."
      )
    )
    
    mineral_uses %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 4,
          scrollX = TRUE,
          dom = "t",
          columnDefs = list(
            list(width = "18%", targets = 0),
            list(width = "42%", targets = 1),
            list(width = "40%", targets = 2)
          )
        )
      )
  })
  
  output$coverage_table <- renderDT({
    production_coverage <- project_data %>%
      group_by(mineral, data_type) %>%
      summarise(
        years_available = paste(sort(unique(year)), collapse = ", "),
        country_year_records = n(),
        max_countries_one_year = max(as.integer(table(year)), na.rm = TRUE),
        .groups = "drop"
      )
    
    trade_coverage <- import_data %>%
      group_by(mineral) %>%
      summarise(
        data_type = "BACI trade flows",
        years_available = paste(sort(unique(year)), collapse = ", "),
        country_year_records = n(),
        max_countries_one_year = max(as.integer(table(year)), na.rm = TRUE),
        .groups = "drop"
      )
    
    bind_rows(production_coverage, trade_coverage) %>%
      arrange(mineral, data_type) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$hs_code_table <- renderDT({
    if (!import_available()) {
      return(datatable(tibble(note = "Add data/import_data.csv to view HS-code coverage."), rownames = FALSE))
    }
    
    df <- filtered_import_data() %>%
      distinct(mineral, hs_code, hs_description) %>%
      arrange(mineral, hs_code)
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No HS codes are available under the current mineral/filter selection."), rownames = FALSE))
    }
    
    df %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  filtered_dashboard_download <- reactive({
    selected_base_data() %>%
      arrange(year, country_clean, data_type) %>%
      select(
        mineral, data_type, country = country_clean, iso3, year,
        value_metric_tons = value_standard, original_value = value,
        original_unit = unit, source, commodity, statistic,
        duplicate_flag, source_note
      )
  })
  
  filtered_trade_download <- reactive({
    if (!import_available()) return(empty_import_data())
    df <- filtered_import_data()
    if (!is.na(import_year())) df <- df %>% filter(year == import_year())
    df %>% arrange(year, exporter_country, importer_country, hs_code)
  })
  
  make_dashboard_download <- function() {
    downloadHandler(
      filename = function() {
        paste0("dashboard_project_data_", str_replace_all(input$mineral, " ", "_"), "_", input$data_type, ".csv")
      },
      content = function(file) {
        readr::write_csv(filtered_dashboard_download(), file)
      }
    )
  }
  
  make_trade_download <- function() {
    downloadHandler(
      filename = function() {
        paste0("dashboard_trade_data_", str_replace_all(input$mineral, " ", "_"), "_", selected_hs_code_label() %>% str_replace_all("[^A-Za-z0-9]+", "_"), ".csv")
      },
      content = function(file) {
        readr::write_csv(filtered_trade_download(), file)
      }
    )
  }
  
  output$download_dashboard_data <- make_dashboard_download()
  output$download_trade_data <- make_trade_download()
  output$download_dashboard_data_sources <- make_dashboard_download()
  output$download_trade_data_sources <- make_trade_download()
  
  # -------------------------------------------------------------------------
  # Data & Sources tab
  # -------------------------------------------------------------------------
  
  output$data_availability_table <- renderDT({
    dashboard_availability <- project_data %>%
      group_by(mineral, metric = data_type) %>%
      summarise(
        years_available = paste(sort(unique(year)), collapse = ", "),
        first_year = min(year, na.rm = TRUE),
        latest_year = max(year, na.rm = TRUE),
        country_year_records = n(),
        countries_latest_year = n_distinct(country_clean[year == max(year, na.rm = TRUE)]),
        source = paste(sort(unique(source)), collapse = "; "),
        .groups = "drop"
      )
    
    trade_availability <- if (import_available()) {
      import_data %>%
        group_by(mineral) %>%
        summarise(
          metric = "BACI trade flows",
          years_available = paste(sort(unique(year)), collapse = ", "),
          first_year = min(year, na.rm = TRUE),
          latest_year = max(year, na.rm = TRUE),
          country_year_records = n(),
          countries_latest_year = n_distinct(importer_country[year == max(year, na.rm = TRUE)]),
          source = paste(sort(unique(source)), collapse = "; "),
          .groups = "drop"
        )
    } else {
      tibble(
        mineral = character(),
        metric = character(),
        years_available = character(),
        first_year = integer(),
        latest_year = integer(),
        country_year_records = integer(),
        countries_latest_year = integer(),
        source = character()
      )
    }
    
    df <- bind_rows(dashboard_availability, trade_availability) %>%
      arrange(mineral, metric) %>%
      mutate(
        country_year_records = comma(country_year_records),
        countries_latest_year = comma(countries_latest_year)
      ) %>%
      rename(
        Mineral = mineral,
        Metric = metric,
        `Years Available` = years_available,
        `First Year` = first_year,
        `Latest Year` = latest_year,
        `Reported Country-Year Observations` = country_year_records,
        `Countries in Latest Year` = countries_latest_year,
        Source = source
      )
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No availability records found."), rownames = FALSE))
    }
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
    )
  })
  
  output$dashboard_data_table <- renderDT({
    df <- project_data %>%
      filter(
        mineral == input$mineral,
        data_type == input$data_type
      ) %>%
      arrange(year, desc(value_standard)) %>%
      select(
        mineral,
        data_type,
        country = country_clean,
        iso3,
        year,
        value_metric_tons = value_standard,
        original_value = value,
        original_unit = unit,
        source,
        commodity,
        statistic,
        duplicate_flag,
        source_note
      ) %>%
      mutate(
        value_metric_tons = format_number_for_table(value_metric_tons),
        original_value = format_number_for_table(original_value)
      )
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No dashboard records found."), rownames = FALSE))
    }
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$raw_data_table <- renderDT({
    df <- project_raw_augmented %>%
      filter(
        mineral == input$mineral,
        data_type == input$data_type
      ) %>%
      arrange(year, country_clean, desc(value_standard), row_id) %>%
      select(
        selected_for_dashboard,
        row_id,
        mineral,
        data_type,
        country = country_clean,
        iso3,
        year,
        value_metric_tons = value_standard,
        original_value = value,
        original_unit = unit,
        source,
        commodity,
        statistic,
        duplicate_group_n,
        duplicate_flag,
        source_note
      ) %>%
      mutate(
        selected_for_dashboard = if_else(selected_for_dashboard, "Yes", "No"),
        value_metric_tons = format_number_for_table(value_metric_tons),
        original_value = format_number_for_table(original_value)
      )
    
    if (nrow(df) == 0) {
      return(datatable(tibble(note = "No raw records found."), rownames = FALSE))
    }
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

# ---------------------------------------------------------------------------
# Launch app
# ---------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
