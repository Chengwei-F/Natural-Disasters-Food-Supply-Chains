# Natural Disasters and U.S. Food Supply Chains
root <- ""
# enter your root
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(dbscan)
library(patchwork)
library(readxl)
library(haven)
options(dplyr.summarise.inform = FALSE)
sf::sf_use_s2(TRUE)

if (!dir.exists(root)) stop("can't find files：", root)
output_dir <- file.path(root, "replication_output")
data_dir <- file.path(output_dir, "data")
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
check_dir <- file.path(output_dir, "checks")
invisible(lapply(c(data_dir, figure_dir, table_dir, check_dir), dir.create,
                 recursive = TRUE, showWarnings = FALSE))
INPUT_FILES <- list.files(root, recursive = TRUE, full.names = TRUE)
INPUT_FILES <- INPUT_FILES[!startsWith(normalizePath(INPUT_FILES, winslash = "/", mustWork = FALSE),
                                      paste0(normalizePath(output_dir, winslash = "/"), "/"))]

run_status <- data.frame(id = character(), status = character(), detail = character())
used_inputs <- data.frame(file = character(), path = character(), bytes = double())
input_subfolders <- c(
  "Gvkey-Zip.csv" = "disaster raw data", "Zip-County.csv" = "disaster raw data",
  "merged_company_info.xlsx" = "source", "food_company_branch.csv" = "industry raw data",
  "food_company_longandlai.csv" = "industry raw data",
  "food_company_branch_cluster_final.csv" = "industry raw data",
  "food_company_branch_cluster_percentage.csv" = "industry raw data",
  "food_company_branch_cluster_percentage_final.csv" = "industry raw data")
register_output <- function(id, status, detail) {
  run_status <<- rbind(run_status, data.frame(id = id, status = status, detail = detail))
  message("[", id, "] ", status, ": ", detail)
  invisible(NULL)
}
find_input <- function(filename, required = FALSE) {
  direct <- file.path(root, filename)
  subfolder <- unname(input_subfolders[filename])
  known_path <- if (!is.na(subfolder)) file.path(root, subfolder, filename) else NA_character_
  found <- if (file.exists(direct)) direct else if (!is.na(known_path) && file.exists(known_path)) {
    known_path
  } else INPUT_FILES[tolower(basename(INPUT_FILES)) == tolower(filename)]
  if (length(found) > 1L) {
    stop("problem：", filename,
         "\n", paste(found, collapse = "\n"))
  }
  if (!length(found)) {
    if (required) stop("lack of input：", filename)
    return(NA_character_)
  }
  used_inputs <<- unique(rbind(used_inputs, data.frame(file = filename, path = found,
                                                      bytes = file.info(found)$size)))
  found
}
read_csv_text <- function(path, select = NULL) {
  as.data.frame(data.table::fread(path, select = select, colClasses = "character",
    na.strings = c("", "NA", "NaN"), strip.white = FALSE,
    check.names = FALSE, data.table = FALSE, showProgress = interactive()))
}
num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  result <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & is.na(result))) {
    stop("cannot convert into numeric：", paste(head(unique(x[!is.na(x) & is.na(result)]), 5), collapse = ", "))
  }
  result
}
pad_id <- function(x, width) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  # Excel/R
  x <- sub("\\.0+$", "", x)
  scientific <- !is.na(x) & grepl("^[0-9.]+[eE][+-]?[0-9]+$", x)
  if (any(scientific)) x[scientific] <- format(num(x[scientific]), scientific = FALSE, trim = TRUE)
  present <- !is.na(x)
  x[present] <- paste0(vapply(pmax(0L, width - nchar(x[present])),
                             function(n) paste(rep("0", n), collapse = ""), character(1)), x[present])
  x
}
save_data <- function(df, name) {
  data.table::fwrite(as.data.frame(df), file.path(data_dir, name), na = "NA", quote = "auto")
  invisible(df)
}
save_table <- function(df, name) {
  data.table::fwrite(as.data.frame(df), file.path(table_dir, name), na = "NA", quote = "auto")
  invisible(df)
}
save_plot <- function(plot, name, width = 9, height = 5.5) {
  ggplot2::ggsave(file.path(figure_dir, paste0(name, ".pdf")), plot = plot,
                  width = width, height = height, device = "pdf")
  ggplot2::ggsave(file.path(figure_dir, paste0(name, ".png")), plot = plot,
                  width = width, height = height, dpi = 300)
  invisible(plot)
}


# ============================================================================
# 1. Disaster input, Figures 1 and 3, Table A2, and disaster exposures
# ============================================================================

disaster_source <- disaster_monthly <- disaster_12 <- NULL
disaster_year <- disaster_year_allhazards <- mainland_us_counties <- NULL

# Figures 1, 2 and A3 use the ORIGINAL 2018 Census county boundaries.
# Use a local shapefile/ZIP first; otherwise download this exact public release.
load_county_boundaries <- function() {
  stem <- "cb_2018_us_county_500k"
  url <- paste0("https://www2.census.gov/geo/tiger/GENZ2018/shp/", stem, ".zip")
  cache <- file.path(root, "map_data", stem)
  county_path <- find_input(paste0(stem, ".shp"), required = FALSE)
  source <- "Local 2018 Census shapefile"
  extracted <- FALSE
  if (is.na(county_path)) {
    zip_path <- find_input(paste0(stem, ".zip"), required = FALSE)
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    if (is.na(zip_path)) {
      zip_path <- file.path(cache, paste0(stem, ".zip"))
      if (!file.exists(zip_path)) {
        pending <- tempfile(fileext = ".zip")
        on.exit(unlink(pending), add = TRUE)
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout), add = TRUE)
        options(timeout = max(300, old_timeout))
        status <- utils::download.file(url, pending, mode = "wb", quiet = FALSE)
        if (status != 0L || !file.exists(pending) || file.info(pending)$size == 0)
          stop("2018 Census county download failed: ", url)
        if (!file.copy(pending, zip_path, overwrite = TRUE)) stop("Cannot save county ZIP: ", zip_path)
      }
      source <- paste("Official Census 2018 download/cache:", url)
    } else source <- paste("Local 2018 Census ZIP:", zip_path)
    stage <- tempfile("county_2018_")
    dir.create(stage)
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    zip_members <- utils::unzip(zip_path, list = TRUE)$Name
    if (any(grepl("(^[/\\\\]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", zip_members)))
      stop("County ZIP contains an unsafe entry.")
    utils::unzip(zip_path, exdir = stage)
    county_path <- list.files(stage, recursive = TRUE, full.names = TRUE)
    county_path <- county_path[tolower(basename(county_path)) == paste0(stem, ".shp")]
    if (length(county_path) != 1L) stop("County ZIP must contain one ", stem, ".shp")
    extracted <- TRUE
  }
  companions <- paste0(tools::file_path_sans_ext(county_path), c(".shp", ".shx", ".dbf", ".prj"))
  if (!all(file.exists(companions)))
    stop("County map is missing companion files: ", paste(basename(companions[!file.exists(companions)]), collapse = ", "))
  county_map <- sf::st_read(county_path, quiet = TRUE)
  if (is.na(sf::st_crs(county_map))) stop("County map has no readable CRS; check its .prj file.")
  if (!all(c("STATEFP", "COUNTYFP") %in% names(county_map)))
    stop("County shapefile requires STATEFP and COUNTYFP.")
  county_map$STATEFP <- pad_id(county_map$STATEFP, 2)
  county_map$COUNTYFP <- pad_id(county_map$COUNTYFP, 3)
  county_map$County_FIPS <- paste0(county_map$STATEFP, county_map$COUNTYFP)
  if (any(!grepl("^[0-9]{5}$", county_map$County_FIPS))) stop("Invalid county FIPS in county map.")
  if ("GEOID" %in% names(county_map) &&
      any(is.na(county_map$GEOID) | pad_id(county_map$GEOID, 5) != county_map$County_FIPS))
    stop("County map GEOID does not agree with STATEFP/COUNTYFP.")
  county_map$GEOID <- county_map$County_FIPS
  if (extracted) {
    if (!all(file.copy(companions, cache, overwrite = TRUE))) stop("Cannot cache county shapefile.")
    county_path <- file.path(cache, paste0(stem, ".shp"))
  }
  used_inputs <<- unique(rbind(used_inputs, data.frame(
    file = basename(county_path), path = county_path, bytes = file.info(county_path)$size)))
  register_output("County map", "loaded", paste(source, "|", county_path))
  county_map %>% filter(STATEFP <= "56", !STATEFP %in% c("02", "15"))
}
mainland_us_counties <- tryCatch(load_county_boundaries(), error = function(e) {
  register_output("County map", "skipped", paste(conditionMessage(e),
    "Disaster data construction continues; maps require the 2018 county shapefile or ZIP."))
  NULL
})

disaster_path <- find_input("disaster_final_filtered.csv", required = FALSE)
if (is.na(disaster_path)) {
  register_output("Disaster inputs", "skipped", "Missing disaster_final_filtered.csv")
} else {
  disaster_input <- read_csv_text(disaster_path)
  disaster_required <- c("County FIPS", "Hazard", "Year", "Month",
                         "PropertyDmg", "PropertyDmg(ADJ 2023)")
  disaster_missing <- setdiff(disaster_required, names(disaster_input))
  if (length(disaster_missing)) {
    register_output("Disaster inputs", "skipped",
                    paste("Missing columns:", paste(disaster_missing, collapse = ", ")))
  } else {
    disaster_source <- as.data.frame(disaster_input) %>%
      transmute(
        County_FIPS = pad_id(gsub("['\"]", "", `County FIPS`), 5),
        Hazard = as.character(Hazard), Year = as.integer(num(Year)),
        Month = as.integer(num(Month)), PropertyDmg = num(PropertyDmg),
        damage_2023 = num(`PropertyDmg(ADJ 2023)`)
      )
    bad_keys <- with(disaster_source,
                     is.na(County_FIPS) | !grepl("^[0-9]{5}$", County_FIPS) |
                       is.na(Hazard) | Hazard == "" | is.na(Year) |
                       is.na(Month) | !(Month %in% 1:12))
    if (any(bad_keys)) {
      save_table(disaster_source[bad_keys, ], "disaster_invalid_keys.csv")
      register_output("Disaster inputs", "skipped",
                      "Invalid county/year/month/hazard keys; see disaster_invalid_keys.csv")
      disaster_source <- NULL
    }
  }
}

if (!is.null(disaster_source)) {
  disaster_monthly <- disaster_source %>%
    group_by(County_FIPS, Year, Month, Hazard) %>%
    summarise(Frequency = n(), PropertyDmg = sum(PropertyDmg, na.rm = TRUE),
              damage_2023 = sum(damage_2023, na.rm = TRUE), .groups = "drop")
  save_data(disaster_monthly, "disaster_county_month_observed.csv")

  # ============================================================================
  # Figure 1: Frequency of All Disasters and Large Disasters
  # ============================================================================
  damaged_types_original <- c("Tornado", "Hurricane/Tropical Storm", "Flooding",
                              "Earthquake", "Tsunami/Seiche",
                              "Severe Storm/Thunder Storm")
  figure_1a_data <- disaster_source %>% count(County_FIPS, name = "records")
  figure_1b_data <- disaster_source %>%
    filter(Hazard %in% damaged_types_original) %>% count(County_FIPS, name = "records")
  figure_1b_large_data <- disaster_source %>%
    filter(damage_2023 > 1000000) %>% count(County_FIPS, name = "records")
  save_table(figure_1a_data, "Figure_1a_county_counts.csv")
  save_table(figure_1b_data, "Figure_1b_original_code_county_counts.csv")
  save_table(figure_1b_large_data, "Figure_1b_large_caption_diagnostic_counts.csv")

  if (!is.null(mainland_us_counties)) {
    map_breaks <- c(0, 50, 150, 250, 350, 400)
    map_bins <- c(0, unlist(lapply(seq_len(length(map_breaks) - 1L), function(i) {
      seq(map_breaks[i], map_breaks[i + 1L], length.out = 21)[-1]
    })))
    map_palette <- grDevices::colorRampPalette(
      c("#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929",
        "#EC7014", "#CC4C02", "#993404", "#662506"), space = "rgb")(101)
    map_palette <- map_palette[round(seq(1, 101, length.out = 100))]
    plot_disaster_counties <- function(counts, caption) {
      map_data <- mainland_us_counties %>% left_join(counts, by = "County_FIPS")
      map_data$color_bin <- findInterval(map_data$records, map_bins, all.inside = TRUE)
      ggplot(map_data) +
        geom_sf(aes(fill = color_bin), colour = "grey40", linewidth = 0.12) +
        scale_fill_gradientn(
          colours = map_palette, values = seq(0, 1, length.out = 100),
          limits = c(1, 100), breaks = c(1, 20, 40, 60, 80, 100),
          labels = map_breaks, na.value = "grey75", name = NULL,
          guide = guide_colourbar(direction = "vertical", reverse = TRUE)) +
        coord_sf(datum = NA) + labs(caption = caption) +
        theme_void(base_size = 11) +
        theme(
          legend.position = "inside", legend.position.inside = c(0.02, 0.02),
          legend.justification = c(0, 0),
          legend.key.width = grid::unit(2.5, "mm"),
          legend.key.height = grid::unit(22, "mm"),
          legend.text = element_text(size = 7),
          legend.background = element_rect(
            fill = grDevices::adjustcolor("white", alpha.f = 0.1), colour = NA),
          legend.margin = margin(0, 0, 0, 0),
          plot.caption = element_text(hjust = 0.5, size = 11, margin = margin(t = 6)),
          plot.background = element_rect(fill = "white", colour = NA))
    }
    fig1a <- plot_disaster_counties(figure_1a_data, "(a) All Disasters")
    fig1b <- plot_disaster_counties(figure_1b_data, "(b) Damaged Disasters")
    fig1b_large <- plot_disaster_counties(figure_1b_large_data,
                                        "(b) Damage > $1 million")
    save_plot(fig1a, "Figure_1a", width = 9, height = 5.5)
    save_plot(fig1b, "Figure_1b_original_code", width = 9, height = 5.5)
    save_plot(patchwork::wrap_plots(fig1a, fig1b, ncol = 2),
              "Figure_1_original_code", width = 14, height = 5)
    save_plot(fig1b_large, "Figure_1b_large_caption_diagnostic", width = 9, height = 5.5)
    register_output("Figure 1", "generated_with_difference",
                    "Drawn with ggplot2/sf using the original county counts")
  } else {
    register_output("Figure 1", "skipped",
                    "County counts saved; map requires cb_2018_us_county_500k.shp and companion files.")
  }

  # ============================================================================
  # Figure 3: Trends in All Disasters and Large Disasters
  # ============================================================================
  events_year_all <- disaster_source %>% count(Year, name = "total_events_all")
  year_hazard <- disaster_source %>%
    group_by(Year, Hazard) %>%
    summarise(total_damage = sum(damage_2023, na.rm = TRUE), .groups = "drop") %>%
    mutate(total_damage = na_if(total_damage, 0))
  top_haz <- year_hazard %>% group_by(Hazard) %>%
    summarise(dmg_all_years = sum(total_damage, na.rm = TRUE), .groups = "drop") %>%
    slice_max(dmg_all_years, n = 6) %>% pull(Hazard)
  year_hazard_top <- year_hazard %>% filter(Hazard %in% top_haz)
  save_table(events_year_all, "Figure_3_annual_frequency.csv")
  save_table(year_hazard_top, "Figure_3_annual_damage_top6.csv")
  damage_max <- max(year_hazard_top$total_damage, na.rm = TRUE)
  if (is.finite(damage_max) && damage_max > 0) {
    scale_factor <- max(events_year_all$total_events_all, na.rm = TRUE) / damage_max
    fig3 <- ggplot() +
      geom_line(data = events_year_all, aes(x = Year, y = total_events_all),
                linewidth = 1.1, colour = "black") +
      geom_point(data = events_year_all, aes(x = Year, y = total_events_all), size = 2) +
      geom_line(data = year_hazard_top,
                aes(x = Year, y = total_damage * scale_factor, colour = Hazard),
                linewidth = 1, na.rm = TRUE) +
      geom_point(data = year_hazard_top,
                 aes(x = Year, y = total_damage * scale_factor, colour = Hazard),
                 size = 1.6, na.rm = TRUE) +
      scale_y_continuous(name = "Total number of disaster records",
                         sec.axis = sec_axis(~ . / scale_factor,
                                             name = "Property damage (2023 USD; top 6 hazards)")) +
      scale_x_continuous(breaks = pretty(events_year_all$Year)) +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom") +
      labs(x = "Year", colour = "Hazard")
    save_plot(fig3, "Figure_3", width = 11, height = 6.5)
    register_output("Figure 3", "generated_with_difference",
                    "Original annual-count/top-six-damage logic, using supplied disaster version; exact paper match is not established.")
  } else {
    register_output("Figure 3", "skipped", "No positive finite annual damage to scale the second axis.")
  }

  # ============================================================================
  # Table A2: Frequency and Property Damage of Natural Disasters by Disaster Type, 1976–2023
  # ============================================================================
  selected_hazards <- c("Tornado", "Hurricane/Tropical Storm", "Flooding",
                        "Earthquake", "Tsunami/Seiche", "Severe Storm/Thunder Storm", "Heat")
  table_A2 <- disaster_monthly %>%
    mutate(Hazard_group = if_else(Hazard %in% selected_hazards, Hazard, "Other hazards")) %>%
    group_by(Hazard_group) %>%
    summarise(total_frequency = sum(Frequency),
              total_damage_2023_USD = sum(damage_2023), .groups = "drop") %>%
    mutate(share_frequency = total_frequency / sum(total_frequency),
           share_damage = total_damage_2023_USD / sum(total_damage_2023_USD))
  table_A2 <- bind_rows(table_A2,
                        table_A2 %>% summarise(Hazard_group = "All hazards",
                                               total_frequency = sum(total_frequency),
                                               total_damage_2023_USD = sum(total_damage_2023_USD),
                                               share_frequency = 1, share_damage = 1)) %>%
    mutate(Hazard_group = factor(Hazard_group,
                                 levels = c(selected_hazards, "Other hazards", "All hazards"))) %>%
    arrange(Hazard_group) %>%
    mutate(Hazard_group = as.character(Hazard_group),
           total_damage_billion_2023_USD = total_damage_2023_USD / 1e9)
  save_table(table_A2, "Table_A2.csv")
  paper_A2 <- data.frame(
    Hazard_group = c(selected_hazards, "Other hazards", "All hazards"),
    paper_frequency = c(28090, 6277, 70629, 71, 60, 166669, 7636, 392208, 671640),
    paper_damage_billion = c(63.06, 287.98, 297.68, 59.64, .08, 29.76, .59, 201.89, 940.69))
  table_A2_check <- table_A2 %>% left_join(paper_A2, by = "Hazard_group") %>%
    mutate(frequency_difference = total_frequency - paper_frequency,
           damage_difference_vs_rounded_paper = total_damage_billion_2023_USD - paper_damage_billion)
  save_table(table_A2_check, "Table_A2_comparison_with_paper.csv")
  register_output("Table A2", "generated_with_difference",
                  paste0("Supplied source: ", nrow(disaster_source),
                         " records; published total: 671640. See Table_A2_comparison_with_paper.csv."))

  # ----- Exposure construction: complete 1976--2023 county-month grid. -----
  frequency_order <- c("Flooding", "Avalanche", "Tornado", "Drought",
                        "Severe Storm/Thunder Storm", "Wind", "Hurricane/Tropical Storm",
                        "Winter Weather", "Lightning", "Heat", "Hail", "Fog", "Coastal",
                        "Wildfire", "Landslide", "Earthquake", "Tsunami/Seiche", "Volcano")
  large_order <- c("Tornado", "Drought", "Avalanche", "Coastal", "Severe Storm/Thunder Storm",
                    "Wind", "Winter Weather", "Earthquake", "Heat", "Lightning",
                    "Tsunami/Seiche", "Fog", "Hail", "Volcano", "Flooding", "Landslide",
                    "Wildfire", "Hurricane/Tropical Storm")
  unexpected_hazards <- setdiff(unique(disaster_monthly$Hazard), frequency_order)
  if (length(unexpected_hazards)) {
    register_output("Disaster exposures", "skipped",
                    paste("Unrecognized hazard names:", paste(unexpected_hazards, collapse = ", ")))
  } else {
    county_month_grid <- data.table::CJ(County_FIPS = sort(unique(disaster_monthly$County_FIPS)),
                                       Year = 1976:2023, Month = 1:12, unique = TRUE)
    disaster_12 <- data.table::copy(county_month_grid)
    disaster_year_allhazards <- unique(county_month_grid[, c("County_FIPS", "Year"), with = FALSE])
    monthly_dt <- data.table::as.data.table(disaster_monthly)
    for (hazard_name in frequency_order) {
      hazard_months <- monthly_dt[Hazard == hazard_name,
                                  c("County_FIPS", "Year", "Month", "Frequency", "damage_2023"),
                                  with = FALSE]
      full_months <- merge(county_month_grid, hazard_months,
                           by = c("County_FIPS", "Year", "Month"), all.x = TRUE, sort = FALSE)
      data.table::setorder(full_months, County_FIPS, Year, Month)
      full_months[is.na(Frequency), Frequency := 0L]
      full_months[is.na(damage_2023), damage_2023 := 0]
      full_months[, Large := as.integer(damage_2023 > 1000000)]
      rolled <- full_months[, list(
        frequency12 = data.table::frollsum(Frequency, n = 12L, align = "right", fill = NA_real_),
        large12 = data.table::frollsum(Large, n = 12L, align = "right", fill = NA_real_)
      ), by = County_FIPS]
      data.table::set(disaster_12, j = paste0("12_all_", hazard_name), value = rolled$frequency12)
      data.table::set(disaster_12, j = paste0("12_Large_", hazard_name), value = rolled$large12)
      annual <- full_months[, list(Frequency = sum(Frequency), Large = sum(Large)),
                            by = c("County_FIPS", "Year")]
      data.table::set(disaster_year_allhazards, j = paste0("Frequency_", hazard_name), value = annual$Frequency)
      data.table::set(disaster_year_allhazards, j = paste0("Large_", hazard_name), value = annual$Large)
    }
    data.table::setnames(disaster_12, c("Year", "Month"), c("year_srcdate", "month"))
    data.table::setcolorder(disaster_12,
                           c("County_FIPS", "year_srcdate", "month",
                             paste0("12_all_", frequency_order), paste0("12_Large_", large_order)))
    data.table::setcolorder(disaster_year_allhazards,
                           c("County_FIPS", "Year", paste0("Frequency_", frequency_order),
                             paste0("Large_", frequency_order)))
    year_hazards <- c("Hurricane/Tropical Storm", "Flooding", "Drought", "Heat",
                      "Winter Weather", "Wildfire", "Tsunami/Seiche")
    year_short <- c("Hurricane", "Flooding", "Drought", "Heat",
                    "WinterWeather", "Wildfire", "TsunamiSeiche")
    year_columns <- as.vector(rbind(paste0("Frequency_", year_hazards), paste0("Large_", year_hazards)))
    disaster_year <- data.table::copy(disaster_year_allhazards[, c("County_FIPS", "Year", year_columns), with = FALSE])
    data.table::setnames(disaster_year, year_columns,
                         as.vector(rbind(paste0("Frequency_", year_short), paste0("Large_", year_short))))
    save_data(disaster_12, "disaster_12month.csv")
    save_data(disaster_year, "disaster_year_dataaxle.csv")
    save_data(disaster_year_allhazards, "disaster_year_allhazards.csv")
    register_output("Disaster exposures", "generated",
                    "1976--2023 grid; right-aligned 12 months; annual sums; monthly adjusted damage > $1m. Data Axle Large_TsunamiSeiche will differ from 26 rows of supplied final.")
    rm(county_month_grid, monthly_dt, full_months, hazard_months, rolled, annual)
  }
}


# ============================================================================
# 2. Supply-chain customer panel, concentration, segments, and financial controls
# ============================================================================
panel_food_customer_network <- customer_base <- customer_C <- customer_S <- NULL
food_sumconcentration_network <- vd_BUSSEG <- vd_GEOSEG <- NULL
financial_information <- gvkey_countyFIPS <- NULL

cs_attempt <- function(id, code) {
  tryCatch(force(code), error = function(e) {
    register_output(id, "error", conditionMessage(e)); invisible(NULL)
  })
}
cs_need <- function(x, fields, source) {
  absent <- setdiff(fields, names(x))
  if (length(absent)) stop(source, ": missing columns: ", paste(absent, collapse = ", "))
}
cs_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  ans <- as.Date(rep(NA_character_, length(x)))
  for (fmt in c("%Y/%m/%d", "%Y-%m-%d", "%m/%d/%Y")) {
    j <- is.na(ans) & !is.na(x)
    ans[j] <- suppressWarnings(as.Date(x[j], format = fmt))
  }
  if (any(!is.na(x) & is.na(ans))) stop("Unrecognised date values: ",
    paste(head(unique(x[!is.na(x) & is.na(ans)]), 5), collapse = ", "))
  ans
}
cs_unique <- function(x, keys, diagnostic) {
  duplicate_rows <- x %>% group_by(across(all_of(keys))) %>%
    filter(n() > 1) %>% ungroup()
  if (nrow(duplicate_rows)) {
    save_table(duplicate_rows, paste0(diagnostic, ".csv"))
    stop("Non-unique ", paste(keys, collapse = "+"), "; see ", diagnostic,
         ".csv. No first-row location/control selection was imposed.")
  }
  invisible(TRUE)
}
cs_report_source_duplicates <- function(x, keys, diagnostic, id) {
  duplicate_rows <- x %>% group_by(across(all_of(keys))) %>%
    filter(n() > 1) %>% ungroup()
  save_table(duplicate_rows, paste0(diagnostic, ".csv"))
  if (nrow(duplicate_rows)) register_output(id, "source_duplicates_retained",
    paste(nrow(duplicate_rows), "source rows have repeated firm-year keys;",
          "all retained as in the original R; actual food-customer joins are checked separately"))
  invisible(NULL)
}
cs_financial_columns <- c("Age", "Dividend_Yield", "EV", "EVM_Sales",
  "EVM_EBITDA", "PS_Ratio", "Gross_Profit_Margin", "Operating_Profit_Margin_AD",
  "ROA", "ROE", "Debt_to_Assets", "RD_to_Sales", "Size", "Cash_Holdings",
  "Tobins_q", "Leverage", "Operating_Leverage")
cs_basic_columns <- c("gvkey", "cid", "year_srcdate", "cnms", "ctype", "gareac",
  "gareat", "salecs", "sid", "stype", "srcdate", "conm", "tic", "cusip",
  "cik", "sic", "naics")
cs_food_sic <- c(sprintf("%04d", c(100:199, 200:299, 700:799, 910:919)),
  "2048", sprintf("%04d", c(2000:2009, 2010:2019, 2020:2029, 2030:2039,
  2040:2046, 2050:2059, 2060:2063, 2070:2079, 2090:2092)),
  "2095", "2098", "2099", sprintf("%04d", 2064:2068), "2086", "2087",
  "2096", "2097", "2080", sprintf("%04d", c(2082:2085, 2100:2199,
  5140:5149, 5150:5159, 5180:5182)))

# 2.1 Headquarters county.
cs_attempt("CS_headquarters", {
  p <- find_input("gvkey_countyFIPS.csv")
  if (!is.na(p)) {
    hq <- read_csv_text(p)
    cs_need(hq, c("gvkey", "County_FIPS", "fyear"), basename(p))
    hq <- hq %>% transmute(gvkey = pad_id(gvkey, 6),
      County_FIPS = pad_id(County_FIPS, 5), fyear = num(fyear)) %>% distinct()
  } else {
    pp <- vapply(c("Gvkey-Zip.csv", "merged_company_info.xlsx", "zipcodecensus.dta"),
                 find_input, character(1))
    if (anyNA(pp)) {
      register_output("CS_headquarters", "missing_input",
        paste("Need gvkey_countyFIPS.csv, or all three:", paste(names(pp), collapse = ", ")))
      hq <- NULL
    } else {
      gz <- read_csv_text(pp[1])
      cs_need(gz, c("gvkey", "addzip", "indfmt", "fyear", "cik"), "Gvkey-Zip.csv")
      gz <- gz %>% mutate(gvkey = pad_id(gvkey, 6), fyear = num(fyear), cik = num(cik)) %>%
        filter(!grepl("^[A-Za-z]", addzip), !grepl("^\\d{6}$", addzip), indfmt != "FS") %>%
        mutate(addzip = num(sub("([0-9]{5}).*", "\\1", addzip))) %>%
        group_by(gvkey) %>% complete(fyear = 1976:2023) %>% ungroup()
      sec <- as.data.frame(readxl::read_excel(pp[2]))
      cs_need(sec, c("cik", "acceptanceDateTime", "ZIP"), "merged_company_info.xlsx")
      sec_date <- cs_date(sec$acceptanceDateTime)
      sec <- sec %>% transmute(cik = num(cik), ZIP = as.character(ZIP),
        fyear = as.integer(format(sec_date, "%Y")) -
          as.integer(as.integer(format(sec_date, "%m")) <= 6)) %>%
        filter(!grepl("^[A-Za-z]", ZIP), !grepl("^\\d{6}$", ZIP),
          !is.na(ZIP), !ZIP %in% c("0", "000", "0000", "00000", "-", "000-000")) %>%
        mutate(ZIP = sub("([0-9]{5}).*", "\\1", ZIP))
      gz <- gz %>% left_join(sec, by = c("cik", "fyear")) %>%
        group_by(gvkey) %>% fill(ZIP, .direction = "down") %>%
        fill(ZIP, .direction = "updown") %>% ungroup() %>%
        mutate(ZIP = ifelse(is.na(ZIP) & !is.na(addzip), as.character(addzip), ZIP)) %>%
        group_by(gvkey) %>% fill(ZIP, .direction = "downup") %>% ungroup()
      zc <- as.data.frame(haven::read_dta(pp[3]))
      cs_need(zc, c("zipcode", "fips"), "zipcodecensus.dta")
      zc <- zc %>% transmute(zipcode = pad_id(zipcode, 5), County_FIPS = pad_id(fips, 5))
      hq <- gz %>% left_join(zc, by = c("ZIP" = "zipcode")) %>%
        select(gvkey, County_FIPS, fyear) %>% distinct()
      short_zip <- gz %>% filter(!is.na(ZIP), nchar(ZIP) < 5)
      if (nrow(short_zip)) save_table(short_zip, "HQ_short_ZIP_original_rule.csv")
    }
  }
  if (!is.null(hq)) {
    cs_report_source_duplicates(hq, c("gvkey", "fyear"),
      "HQ_multiple_counties_same_firm_year", "CS_headquarters_duplicates")
    gvkey_countyFIPS <- hq
    save_data(gvkey_countyFIPS, "gvkey_countyFIPS.csv")
    register_output("CS_headquarters", "created", paste(nrow(hq), "firm-year rows"))
  }
})

# 2.2 Financial ratios. Compute before C/S joins, unlike the old script's order.
cs_attempt("CS_financial", {
  p <- find_input("some fundament.csv")
  q <- find_input("financial_information.csv")
  if (!is.na(p)) {
    ff <- read_csv_text(p)
    fields <- c("fyear", "dvpsp_c", "prcc_c", "mkvalt", "dt", "che", "sale",
      "ebitda", "gp", "oiadp", "ni", "at", "teq", "xrd", "dltt", "dlc", "cogs", "xsga")
    cs_need(ff, c("gvkey", fields), basename(p))
    ff <- ff %>% mutate(gvkey = pad_id(gvkey, 6), across(all_of(fields), num))
    fi <- ff %>% group_by(gvkey) %>%
      mutate(first_fyear = min(fyear), Age = fyear - first_fyear + 1) %>% ungroup() %>%
      mutate(Dividend_Yield = dvpsp_c / prcc_c, EV = mkvalt + dt - che,
        EVM_Sales = EV / sale, EVM_EBITDA = EV / ebitda, PS_Ratio = mkvalt / sale,
        Gross_Profit_Margin = gp / sale, Operating_Profit_Margin_AD = oiadp / sale,
        ROA = ni / at, ROE = ni / teq, Debt_to_Assets = dt / at, RD_to_Sales = xrd / sale,
        Size = log(at), Cash_Holdings = che / at, Tobins_q = mkvalt / at,
        Leverage = (dltt + dlc) / at, Operating_Leverage = (cogs + xsga) / at) %>%
      select(gvkey, fyear, all_of(cs_financial_columns))
  } else if (!is.na(q)) {
    fi <- read_csv_text(q)
    cs_need(fi, c("gvkey", "fyear", cs_financial_columns), basename(q))
    fi <- fi %>% mutate(gvkey = pad_id(gvkey, 6),
      across(all_of(c("fyear", cs_financial_columns)), num)) %>%
      select(gvkey, fyear, all_of(cs_financial_columns))
  } else {
    fi <- NULL
    register_output("CS_financial", "missing_input", "some fundament.csv or financial_information.csv")
  }
  if (!is.null(fi)) {
    cs_report_source_duplicates(fi, c("gvkey", "fyear"),
      "financial_duplicate_firm_year", "CS_financial_duplicates")
    financial_information <- fi
    save_data(fi, "financial_information.csv")
    register_output("CS_financial", "created", "Original 17 financial variables; no trimming or imputation")
  }
})

# ============================================================================
# Figure A1: Trends in Sales Concentration
# ============================================================================
cs_attempt("CS_concentration_and_Figure_A1", {
  p <- find_input("company sale sic.csv"); q <- find_input("supply chain network.csv")
  if (is.na(p) || is.na(q)) {
    register_output("CS_concentration_and_Figure_A1", "missing_input",
                    "company sale sic.csv and supply chain network.csv")
  } else {
    sale <- read_csv_text(p); links <- read_csv_text(q)
    cs_need(sale, c("gvkey", "sic", "datadate", "sale"), basename(p))
    cs_need(links, c("gvkey", "srcdate", "salecs"), basename(q))
    sale <- sale %>% mutate(gvkey = pad_id(gvkey, 6), sic = pad_id(sic, 4)) %>%
      filter(sic %in% cs_food_sic) %>% transmute(gvkey,
        year_srcdate = as.integer(format(cs_date(datadate), "%Y")), sale = num(sale))
    links <- links %>% transmute(gvkey = pad_id(gvkey, 6),
      year_srcdate = as.integer(format(cs_date(srcdate), "%Y")), salecs = num(salecs))
    concentration <- sale %>% inner_join(links, by = c("gvkey", "year_srcdate")) %>%
      group_by(gvkey, year_srcdate, sale) %>%
      summarise(main_salecs = sum(salecs, na.rm = FALSE), .groups = "drop") %>%
      mutate(mainsale_ratio = main_salecs / sale * 100)
    dup <- concentration %>% group_by(gvkey, year_srcdate) %>% filter(n() > 1) %>% ungroup()
    if (nrow(dup)) save_table(dup, "concentration_multiple_sales_original_distinct.csv")
    food_sumconcentration_network <- concentration %>% distinct(gvkey, year_srcdate, .keep_all = TRUE)
    save_data(food_sumconcentration_network, "food_sumconcentration_network.csv")
    yr <- food_sumconcentration_network %>% group_by(year_srcdate) %>%
      summarise(mean_mainsale_ratio = mean(mainsale_ratio, na.rm = TRUE),
        sd_mainsale_ratio = sd(mainsale_ratio, na.rm = TRUE), .groups = "drop") %>%
      mutate(lower_bound = mean_mainsale_ratio - sd_mainsale_ratio,
        upper_bound = mean_mainsale_ratio + sd_mainsale_ratio)
    save_table(yr, "Figure_A1_customer_concentration_data.csv")
    plt <- ggplot(yr, aes(x = year_srcdate)) +
      geom_line(aes(y = mean_mainsale_ratio), colour = "black", linewidth = 0.75) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "blue", alpha = 0.1) +
      labs(x = "Year", y = "Average Concentration Sale Ratio") + theme_minimal()
    save_plot(plt, "Figure_A1_customer_concentration", 8, 5)
    register_output("CS_concentration_and_Figure_A1", "created", "Full source sample; band is mean +/- SD")
  }
})

# ============================================================================
# Figure A2: Trends in Business and International Geographical Diversification
# ============================================================================
# 2.4 Business/geographical diversity:
cs_attempt("CS_segments_and_Figure_A2", {
  p <- find_input("food_segment.csv")
  if (!is.na(p)) {
    seg <- read_csv_text(p)
    cs_need(seg, c("gvkey", "stype", "sales"), basename(p))
    if (!"year_srcdate" %in% names(seg)) {
      cs_need(seg, "datadate", basename(p))
      seg$year_srcdate <- as.integer(format(cs_date(seg$datadate), "%Y"))
    }
    seg <- seg %>% mutate(gvkey = pad_id(gvkey, 6), year_srcdate = num(year_srcdate),
      sales = num(sales)) %>% filter(sales > 0 | is.na(sales))
    bus <- seg %>% filter(stype == "BUSSEG"); geo <- seg %>% filter(stype == "GEOSEG")
  } else {
    pb <- find_input("food_segment_BUSSEG.csv"); pg <- find_input("food_segment_GEOSEG.csv")
    read_segment <- function(path) {
      if (is.na(path)) return(NULL)
      z <- read_csv_text(path)
      cs_need(z, c("gvkey", "year_srcdate", "sales"), basename(path))
      z %>% mutate(gvkey = pad_id(gvkey, 6), year_srcdate = num(year_srcdate), sales = num(sales)) %>%
        filter(sales > 0 | is.na(sales))
    }
    bus <- read_segment(pb); geo <- read_segment(pg)
  }
  cs_vd <- function(sales) 100 * (1 - sum((sales / sum(sales, na.rm = TRUE))^2, na.rm = TRUE))
  if (!is.null(bus)) {
    vd_BUSSEG <- bus %>% group_by(gvkey, year_srcdate) %>%
      summarise(vd_BUSSEG = cs_vd(sales), .groups = "drop")
    save_data(vd_BUSSEG, "vd_BUSSEG.csv")
  }
  if (!is.null(geo)) {
    vd_GEOSEG <- geo %>% group_by(gvkey, year_srcdate) %>%
      summarise(vd_GEOSEG = cs_vd(sales), .groups = "drop")
    save_data(vd_GEOSEG, "vd_GEOSEG.csv")
  }
  if (is.null(vd_BUSSEG) || is.null(vd_GEOSEG)) {
    register_output("CS_segments_and_Figure_A2", "missing_input",
      "food_segment.csv, or both food_segment_BUSSEG.csv and food_segment_GEOSEG.csv")
  } else {
    segment_trend <- function(z, field) z %>% group_by(year_srcdate) %>%
      summarise(mean = mean(.data[[field]], na.rm = TRUE),
        sd = sd(.data[[field]], na.rm = TRUE), .groups = "drop") %>%
      mutate(lower_bound = mean - sd, upper_bound = mean + sd)
    by <- segment_trend(vd_BUSSEG, "vd_BUSSEG"); gy <- segment_trend(vd_GEOSEG, "vd_GEOSEG")
    save_table(by, "Figure_A2_BUSSEG_data.csv"); save_table(gy, "Figure_A2_GEOSEG_data.csv")
    seg_plot <- function(z, ylab, caption) ggplot(z, aes(x = year_srcdate)) +
      geom_line(aes(y = mean), colour = "black", linewidth = 0.75) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "blue", alpha = 0.1) +
      labs(x = "Year", y = ylab, caption = caption) + theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5, vjust = -1))
    a <- seg_plot(by, "Average Business Diversification", "(a) Trends in Business Diversification")
    b <- seg_plot(gy, "Average Geographical Diversification", "(b) Trends in International Diversification")
    save_plot(a + b + patchwork::plot_layout(ncol = 2), "Figure_A2_segment_diversification", 12, 5)
    register_output("CS_segments_and_Figure_A2", "created", "Full segment source sample; bands are mean +/- SD")
  }
})

# ============================================================================
# Figure 4: Impact of All and Large Disasters on Sales Growth
# Table A5: Current and Lead Effects of Natural Disasters on Food Firm Sales Growth
# ============================================================================
# 2.5 Customer panel.
cs_attempt("CS_customer_base", {
  p <- find_input("food_customer_network.csv")
  if (is.na(p)) {
    register_output("CS_customer_base", "missing_input", "food_customer_network.csv")
  } else {
    fc <- read_csv_text(p)
    cs_need(fc, cs_basic_columns, basename(p))
    fc <- fc %>% select(all_of(cs_basic_columns)) %>%
      mutate(gvkey = pad_id(gvkey, 6), year_srcdate = num(year_srcdate), salecs = num(salecs))
    cs_unique(fc, c("gvkey", "cid", "year_srcdate"), "customer_duplicate_pair_year")
    dates <- cs_date(fc$srcdate)
    bad_year <- !is.na(dates) & as.integer(format(dates, "%Y")) != fc$year_srcdate
    if (any(bad_year, na.rm = TRUE)) stop("srcdate year differs from year_srcdate in customer input")
    if (any(!fc$year_srcdate %in% 1976:2023)) stop("Customer years outside original 1976:2023 template")
    link <- fc %>% distinct(gvkey, cid)
    template <- link %>% group_by(gvkey, cid) %>% expand(year_srcdate = 1976:2023) %>% ungroup()
    panel_food_customer_network <- template %>%
      left_join(fc, by = c("gvkey", "cid", "year_srcdate")) %>%
      arrange(gvkey, cid, year_srcdate) %>% group_by(gvkey, cid) %>%
      mutate(sales_growth = (salecs - dplyr::lag(salecs)) / dplyr::lag(salecs),
        month = as.integer(format(cs_date(srcdate), "%m"))) %>% ungroup()
    growth_gaps <- panel_food_customer_network %>% group_by(gvkey, cid) %>%
      mutate(previous_sale = dplyr::lag(salecs)) %>% ungroup() %>%
      filter(!is.na(srcdate), !is.na(salecs), is.na(previous_sale))
    save_table(growth_gaps, "customer_observations_without_previous_year_sale.csv")
    known_gap <- growth_gaps %>% filter(gvkey == "014501", cid == "27", year_srcdate == 2019)
    if (nrow(known_gap)) register_output("CS_growth_known_gap", "limitation",
      "014501 / cid 27 / 2019: 2018 input record absent; recomputed growth remains NA, unlike old final")
    if (is.null(gvkey_countyFIPS) || is.null(disaster_12)) {
      register_output("CS_customer_base", "missing_input",
        "Customer growth constructed; need headquarters crosswalk and disaster_12 to merge")
    } else {
      panel_food_customer_network <- panel_food_customer_network %>%
        left_join(gvkey_countyFIPS, by = c("gvkey", "year_srcdate" = "fyear")) %>%
        select(all_of(cs_basic_columns), sales_growth, month, County_FIPS)
      actual <- panel_food_customer_network %>% filter(!is.na(srcdate))
      missed <- actual %>% anti_join(disaster_12, by = c("County_FIPS", "year_srcdate", "month"))
      if (nrow(missed)) save_table(missed, "customer_unmatched_disaster_keys.csv")
      cs_unique(disaster_12, c("County_FIPS", "year_srcdate", "month"), "disaster12_duplicate_keys")
      customer_base_candidate <- panel_food_customer_network %>%
        inner_join(disaster_12, by = c("County_FIPS", "year_srcdate", "month")) %>%
        arrange(gvkey, cid, year_srcdate)
      cs_unique(customer_base_candidate, c("gvkey", "cid", "year_srcdate"),
                "customer_base_duplicate_pair_year_after_HQ_merge")
      customer_base <- customer_base_candidate
      save_data(customer_base, "merged_disaster_food_customer_12.csv")
      register_output("CS_customer_base", "created",
        paste(nrow(customer_base), "observed rows after original inner join;",
              nrow(missed), "observed input rows unmatched; growth was recomputed"))
    }
  }
})

# ============================================================================
# Table 2: Change in Sale Concentration Moderation Effect
# Table A6: Robustness Check for Sale Concentration Moderation Effect
# ============================================================================
cs_attempt("CS_final_C", {
  if (is.null(customer_base) || is.null(food_sumconcentration_network) || is.null(financial_information)) {
    register_output("CS_final_C", "missing_input", "Need customer base, concentration sources and financial controls")
  } else {
    customer_C_candidate <- customer_base %>% left_join(food_sumconcentration_network,
        by = c("gvkey", "year_srcdate")) %>% arrange(gvkey, cid, year_srcdate) %>%
      group_by(gvkey, cid) %>% mutate(
        growth_mainsale_ratio = (mainsale_ratio - dplyr::lag(mainsale_ratio)) / dplyr::lag(mainsale_ratio),
        growth_mainsale = (main_salecs - dplyr::lag(main_salecs)) / dplyr::lag(main_salecs),
        allhurri_mainratio = `12_all_Hurricane/Tropical Storm` * growth_mainsale_ratio,
        allhurri_main = `12_all_Hurricane/Tropical Storm` * growth_mainsale) %>% ungroup() %>%
      left_join(financial_information, by = c("gvkey", "year_srcdate" = "fyear"))
    cs_unique(customer_C_candidate, c("gvkey", "cid", "year_srcdate"),
              "customer_C_duplicate_pair_year_after_financial_merge")
    customer_C <- customer_C_candidate
    save_data(customer_C, "merged_disaster_food_customer_12C.csv")
    register_output("CS_final_C", "created", paste(nrow(customer_C), "rows; comparison with reference final is still required"))
  }
})

# ============================================================================
# Table 3: Segment Sale Moderation Effect
# Table A7: Robustness Check for Segment Sale Moderation Effect
# ============================================================================
cs_attempt("CS_final_S", {
  if (is.null(customer_base) || is.null(vd_BUSSEG) || is.null(vd_GEOSEG) || is.null(financial_information)) {
    register_output("CS_final_S", "missing_input", "Need customer base, both segment sources and financial controls")
  } else {
    customer_S_candidate <- customer_base %>% left_join(vd_BUSSEG, by = c("gvkey", "year_srcdate")) %>%
      left_join(vd_GEOSEG, by = c("gvkey", "year_srcdate")) %>%
      left_join(financial_information, by = c("gvkey", "year_srcdate" = "fyear"))
    cs_unique(customer_S_candidate, c("gvkey", "cid", "year_srcdate"),
              "customer_S_duplicate_pair_year_after_financial_merge")
    customer_S <- customer_S_candidate
    save_data(customer_S, "merged_disaster_food_customer_12S.csv")
    register_output("CS_final_S", "created", paste(nrow(customer_S), "rows; comparison with reference final is still required"))
  }
})


# ============================================================================
# 3. Data Axle: food_company_branch -> clusters -> parent-year panels
# ============================================================================
branch_candidate <- branch_currentR <- branch_network <- NULL
food_industry_candidate <- NULL
branch_candidate_ready <- food_industry_candidate_ready <- FALSE

branch_path <- find_input("food_company_branch.csv", required = FALSE)
if (is.na(branch_path)) {
  branch_path <- find_input("food_company_branch_replication.csv.gz", required = FALSE)
}
if (is.na(branch_path)) {
  register_output("Data Axle / Figures 2, A3, A4", "missing_input",
                  "Need food_company_branch.csv (or the 13-column .csv.gz copy).")
} else {
  tryCatch({
    branch_cols <- c("abi", "archive_version_year", "parent_number", "company",
                     "fips_code", "primary_sic_code", "year_established",
                     "employee_size_location", "sales_volume_location",
                     "parent_actual_employee_size", "parent_actual_sales_volume",
                     "latitude", "longitude")
    header <- names(data.table::fread(branch_path, nrows = 0, check.names = FALSE))
    absent <- setdiff(branch_cols, header)
    if (length(absent)) stop("Missing branch columns: ", paste(absent, collapse = ", "))
    b <- read_csv_text(branch_path, select = branch_cols)
    b$source_row_order <- seq_len(nrow(b))
    b$abi <- pad_id(b$abi, 9)
    b$parent_number <- pad_id(b$parent_number, 9)
    b$fips_code <- pad_id(b$fips_code, 5)
    numeric_cols <- c("archive_version_year", "year_established",
                      "employee_size_location", "sales_volume_location",
                      "parent_actual_employee_size", "parent_actual_sales_volume",
                      "latitude", "longitude")
    b[numeric_cols] <- lapply(b[numeric_cols], num)
    b$primary_sic_code[b$primary_sic_code %in% c("", "NA")] <- NA_character_
    b$company[b$company %in% c("", "NA")] <- NA_character_
    branch_input_n <- nrow(b)

    # ========================================================================
    # Figure 2: Spatial Distribution of Food-Industry Establishments in the Mainland United States
    # ========================================================================
    if (!is.null(mainland_us_counties)) {
      tryCatch(local({
        figure_2_data <- b %>% 
          filter(archive_version_year == 2022,
                 !is.na(parent_number),
                 parent_number != "000000000") %>%
          filter(substr(fips_code, 1, 2) %in% mainland_us_counties$STATEFP,
                 is.finite(longitude), is.finite(latitude),
                 between(longitude, -180, 180), between(latitude, -90, 90)) %>%
          distinct(abi, longitude, latitude)
        if (!nrow(figure_2_data)) stop("No mainland establishment locations with valid coordinates.")
        save_table(figure_2_data, "Figure_2_plot_data.csv")
        figure_2_points <- sf::st_as_sf(figure_2_data,
          coords = c("longitude", "latitude"), crs = 4326)
        map_bounds <- sf::st_bbox(mainland_us_counties)
        p2 <- ggplot() +
          geom_sf(data = mainland_us_counties, fill = "grey95",
                  colour = "white", linewidth = 0.1) +
          geom_sf(data = figure_2_points, colour = "red", size = 0.25,
                  alpha = 0.5, show.legend = FALSE) +
          coord_sf(xlim = unname(map_bounds[c("xmin", "xmax")]),
                   ylim = unname(map_bounds[c("ymin", "ymax")]),
                   datum = NA, expand = FALSE) +
          theme_void() +
          theme(plot.background = element_rect(fill = "white", colour = NA))
        save_plot(p2, "Figure_2", width = 12, height = 6)
        register_output("Figure 2", "generated",
          paste(nrow(figure_2_data),
            "distinct abi/longitude/latitude locations across all source years"))
      }), error = function(e) register_output("Figure 2", "error", conditionMessage(e)))
    } else {
      register_output("Figure 2", "missing_input", "Need mainland county shapefile.")
    }

    parent_rows <- b %>% filter(!is.na(parent_number), parent_number != "000000000")

    spatial_rows <- parent_rows %>%
      filter(!grepl("^02|^15", fips_code), fips_code < "57",
             !is.na(longitude), !is.na(latitude))
    if (any(!is.finite(spatial_rows$longitude) | !is.finite(spatial_rows$latitude))) {
      stop("Non-finite coordinates found; inspect the source rather than silently dropping rows.")
    }
    branch_quality <- data.frame(
      item = c("input_rows", "nonzero_parent_rows", "clustering_rows",
               "missing_coordinate_parent_rows", "duplicate_abi_year_rows"),
      value = c(branch_input_n, nrow(parent_rows), nrow(spatial_rows),
                sum(is.na(parent_rows$longitude) | is.na(parent_rows$latitude)),
                sum(duplicated(parent_rows[c("abi", "archive_version_year")]))))
    save_table(branch_quality, "DataAxle_input_checks.csv")
    if (!nrow(spatial_rows)) stop("No mainland parent-linked rows with coordinates.")

    cl <- data.table::as.data.table(spatial_rows)
    cl[, cluster := {
      if (.N >= 4L) dbscan::hdbscan(cbind(longitude, latitude), minPts = 4L)$cluster
      else rep(NA_integer_, .N)
    }, by = c("parent_number", "archive_version_year")]
    cl <- as.data.frame(cl) %>% arrange(parent_number, archive_version_year)
    choose_parent_name <- function(company, abi, parent_number) {
      hq <- company[!is.na(abi) & abi == parent_number & !is.na(company)]
      if (length(hq)) return(hq[1])
      counts <- sort(table(company), decreasing = TRUE)
      if (length(counts)) names(counts)[1] else NA_character_
    }
    cl <- cl %>% group_by(parent_number) %>%
      mutate(company = choose_parent_name(company, abi, first(parent_number))) %>% ungroup()
    branch_network <- cl %>% group_by(parent_number, archive_version_year) %>%
      summarise(total_branches = n(),
                num_clusters = n_distinct(cluster[!is.na(cluster) & cluster != 0]),
                branches_in_clusters = sum(!is.na(cluster) & cluster != 0),
                company = first(company), .groups = "drop")
    save_data(branch_network, "food_company_branch_cluster_percentage_final.csv")
    save_data(cl %>% select(abi, parent_number, archive_version_year,
                            longitude, latitude, cluster, company),
              "food_company_branch_clusters_rebuilt.csv")
    register_output("Data Axle clustering", "generated",
                    "Original HDBSCAN minPts=4 on longitude/latitude degrees.")

    # ============================================================================
    # Figure A4: Trends in Branches and Clusters
    # ============================================================================
    mean_cluster <- branch_network %>% group_by(archive_version_year) %>%
      summarise(mean_total_branches = mean(total_branches, na.rm = TRUE),
                mean_num_clusters = mean(num_clusters, na.rm = TRUE),
                mean_branches_in_clusters = mean(branches_in_clusters, na.rm = TRUE),
                .groups = "drop")
    save_table(mean_cluster, "Figure_A4_plot_data.csv")
    p_a4 <- ggplot(mean_cluster, aes(x = archive_version_year)) +
      geom_line(aes(y = mean_total_branches), color = "black") +
      geom_point(aes(y = mean_total_branches, shape = "Total Branches"), size = 2) +
      geom_line(aes(y = mean_branches_in_clusters), color = "gray50") +
      geom_point(aes(y = mean_branches_in_clusters, shape = "Clustered Branches"),
                 color = "gray50", size = 2) +
      geom_line(aes(y = mean_num_clusters * 17 / 2), color = "blue", linetype = "dashed") +
      geom_point(aes(y = mean_num_clusters * 17 / 2, shape = "Number of Clusters"),
                 color = "blue", size = 2) +
      scale_y_continuous(name = "Number of Branches", limits = c(0, 17),
                         sec.axis = sec_axis(~ . * 2 / 17, name = "Number of Clusters")) +
      scale_shape_manual(values = c("Total Branches" = 15, "Clustered Branches" = 16,
                                    "Number of Clusters" = 17),
                         guide = guide_legend(override.aes = list(linetype = 0))) +
      labs(x = "Year") + theme_minimal() +
      theme(axis.title.y.right = element_text(color = "blue"),
            panel.grid.minor = element_blank(), legend.title = element_blank(),
            legend.position = "bottom")
    save_plot(p_a4, "Figure_A4", width = 8, height = 5)
    register_output("Figure A4", "generated", "Original full-network yearly means and dual-axis scaling.")

    # ============================================================================
    # Figure A3: Comparison of Different Cluster Scenarios
    # ============================================================================
    if (!is.null(mainland_us_counties)) {
      tryCatch({
        cluster_map <- function(parent, year, caption) {
          z <- cl %>% filter(parent_number == parent, archive_version_year == year)
          if (!nrow(z)) stop("Missing Figure A3 example: ", parent, "/", year)
          z <- sf::st_as_sf(z, coords = c("longitude", "latitude"), crs = 4326)
          clustered <- z %>% filter(!is.na(cluster), cluster != 0)
          noise <- z %>% filter(cluster == 0)
          hull <- clustered %>% group_by(cluster) %>%
            summarise(geometry = sf::st_convex_hull(sf::st_union(geometry)), .groups = "drop")
          ggplot() + geom_sf(data = mainland_us_counties, fill = "white",
                             color = "gray75", linewidth = 0.1) +
            geom_sf(data = hull, fill = "lightblue", color = "blue", alpha = 0.4) +
            geom_sf(data = clustered, color = "black", size = 0.7) +
            geom_sf(data = noise, color = "red", size = 0.5) +
            labs(caption = caption) + theme_void() +
            theme(plot.caption = element_text(hjust = 0.5))
        }
        p_a3a <- cluster_map("441371168", 1998, "(a) Loosely clustered branches")
        p_a3b <- cluster_map("000558247", 2016, "(b) Tightly clustered branches")
        save_plot(patchwork::wrap_plots(p_a3a, p_a3b, ncol = 2), "Figure_A3",
                  width = 12, height = 5)
        register_output("Figure A3", "generated",
                        "Original two parent-year examples; convex hulls, clustered points and noise.")
      }, error = function(e) register_output("Figure A3", "error", conditionMessage(e)))
    } else {
      register_output("Figure A3", "missing_input", "Need mainland county shapefile.")
    }

    # ============================================================================
    # Table 4: Cluster Scenario Moderation Effect
    # Table A8: Robustness Check for Cluster Scenario Moderation Effect
    # ============================================================================
    prepare_parent <- function(x, old_parent_sales = TRUE) {
      if (old_parent_sales) {
        # industry(3).R lines 487-522: commented earlier parent-sales rule.
        x <- x %>% arrange(parent_number, archive_version_year) %>%
          group_by(parent_number, archive_version_year) %>%
          fill(parent_actual_sales_volume, .direction = "downup") %>%
          mutate(parent_actual_sales_volume = ifelse(
            is.na(parent_actual_sales_volume),
            if (all(is.na(sales_volume_location))) NA_real_
            else sum(sales_volume_location, na.rm = TRUE), parent_actual_sales_volume)) %>%
          fill(parent_actual_employee_size, .direction = "downup") %>% ungroup()
      } else {
        # Active code lines 524-567: fills across years and adds location sales.
        x <- x %>% arrange(parent_number, archive_version_year) %>%
          group_by(parent_number) %>%
          fill(parent_actual_employee_size, parent_actual_sales_volume, .direction = "downup") %>%
          ungroup() %>% arrange(abi, archive_version_year) %>% group_by(abi) %>%
          fill(employee_size_location, sales_volume_location, .direction = "downup") %>% ungroup()
      }
      x <- x %>% group_by(parent_number) %>%
        mutate(year_established = ifelse(is.na(year_established) | abi != parent_number,
                                         year_established[abi == parent_number][1], year_established)) %>%
        fill(year_established, .direction = if (old_parent_sales) "down" else "downup") %>%
        mutate(primary_sic_code = ifelse(is.na(primary_sic_code) | abi != parent_number,
                                         primary_sic_code[abi == parent_number][1], primary_sic_code)) %>%
        fill(primary_sic_code, .direction = "downup") %>%
        group_by(parent_number, archive_version_year) %>%
        mutate(total_employee = sum(employee_size_location, na.rm = TRUE)) %>% ungroup()
      if (!old_parent_sales) {
        x <- x %>% group_by(parent_number, archive_version_year) %>%
          mutate(actual_sales_volume = parent_actual_sales_volume + sum(sales_volume_location, na.rm = TRUE)) %>%
          ungroup()
      }
      x
    }
    make_parent_panel <- function(x, sale_col) {
      panel <- tidyr::expand(x %>% distinct(parent_number), parent_number,
                             archive_version_year = 1997:2023)
      # Preserve the original headquarters join/fill, fallback county, and first
      # parent-year record rule. Duplicate HQ records are not silently discarded.
      hq_county <- panel %>% left_join(
        x %>% filter(abi == parent_number) %>%
          select(parent_number, archive_version_year, fips_code),
        by = c("parent_number", "archive_version_year")) %>%
        group_by(parent_number) %>% arrange(archive_version_year, .by_group = TRUE) %>%
        fill(fips_code, .direction = "downup") %>%
        rename(County_FIPS = fips_code) %>% ungroup()
      panel %>% left_join(x %>% distinct(parent_number, archive_version_year, .keep_all = TRUE),
                          by = c("parent_number", "archive_version_year")) %>%
        left_join(hq_county, by = c("parent_number", "archive_version_year")) %>%
        mutate(County_FIPS = ifelse(is.na(County_FIPS), fips_code, County_FIPS)) %>%
        select(parent_number, archive_version_year, all_of(sale_col), County_FIPS,
               primary_sic_code, year_established, parent_actual_employee_size, total_employee) %>%
        group_by(parent_number) %>%
        mutate(sales_growth = (.data[[sale_col]] - lag(.data[[sale_col]])) / lag(.data[[sale_col]])) %>%
        ungroup() %>% left_join(branch_network, by = c("parent_number", "archive_version_year")) %>%
        distinct(parent_number, archive_version_year, .keep_all = TRUE)
    }
    parent_old <- prepare_parent(parent_rows, old_parent_sales = TRUE)
    branch_candidate <- make_parent_panel(parent_old, "parent_actual_sales_volume")
    rm(parent_old)
    parent_active <- prepare_parent(parent_rows, old_parent_sales = FALSE)
    branch_currentR <- make_parent_panel(parent_active, "actual_sales_volume")
    rm(parent_active)
    save_data(branch_candidate, "dataaxle_parent_sales_before_disaster_candidate.csv")
    save_data(branch_currentR, "dataaxle2_before_disaster_from_original_R.csv")
    if (!is.null(disaster_year)) {
      branch_candidate <- branch_candidate %>% inner_join(
        disaster_year, by = c("County_FIPS", "archive_version_year" = "Year"))
      # The currently active R also exports Earthquake, SevereStorm and Tornado.
      active_hazards <- c("Hurricane/Tropical Storm", "Flooding", "Drought", "Heat",
                          "Winter Weather", "Wildfire", "Tsunami/Seiche", "Earthquake",
                          "Severe Storm/Thunder Storm", "Tornado")
      active_labels <- c("Hurricane", "Flooding", "Drought", "Heat", "WinterWeather",
                         "Wildfire", "TsunamiSeiche", "Earthquake", "SevereStorm", "Tornado")
      active_disaster <- disaster_year_allhazards %>% select(County_FIPS, Year)
      for (i in seq_along(active_hazards)) {
        for (prefix in c("Frequency_", "Large_")) {
          active_disaster[[paste0(prefix, active_labels[i])]] <-
            disaster_year_allhazards[[paste0(prefix, active_hazards[i])]]
        }
      }
      branch_currentR <- branch_currentR %>% inner_join(
        active_disaster, by = c("County_FIPS", "archive_version_year" = "Year"))
      save_data(branch_candidate, "merged_disaster_food_customer_12dataaxle_candidate.csv")
      branch_candidate_ready <- TRUE
      save_data(branch_currentR, "dataaxle2_from_original_R.csv")
      register_output("Table 4 / A8 input", "candidate_needs_comparison",
                      "Parent-sales path follows earlier commented R rules")
    } else {
      register_output("Data Axle disaster merge", "missing_input", "Need disaster_final_filtered.csv.")
    }
    # Diagnostic only: retained final counts select a small set of problem groups.
    # Never use these reference values to change clusters or construct the panel.
    # Requires b$source_row_order <- seq_len(nrow(b)) immediately after reading b.
    tryCatch(local({
      reference_path <- find_input("merged_disaster_food_customer_12dataaxle.csv")
      if (!is.na(reference_path) && !is.null(branch_candidate)) {
        debug_keys <- c("parent_number", "archive_version_year")
        debug_values <- c("total_branches", "num_clusters", "branches_in_clusters")
        reference <- read_csv_text(reference_path,
          select = c(debug_keys, debug_values, "sales_growth"))
        reference$parent_number <- pad_id(reference$parent_number, 9)
        reference$archive_version_year <- num(reference$archive_version_year)
        reference[c(debug_values, "sales_growth")] <-
          lapply(reference[c(debug_values, "sales_growth")], num)
        fresh <- branch_candidate %>%
          select(all_of(c(debug_keys, debug_values, "sales_growth")))
        fresh$parent_number <- pad_id(fresh$parent_number, 9)
        fresh$archive_version_year <- num(fresh$archive_version_year)
        if (anyDuplicated(reference[debug_keys]) || anyDuplicated(fresh[debug_keys])) {
          stop("Cluster diagnostic needs unique parent-year keys.")
        }
        paired <- inner_join(reference, fresh, by = debug_keys,
                             suffix = c("_reference", "_rebuilt"))
        different_count <- function(a, b) {
          xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
        }
        paired$changed_num_clusters <- different_count(
          paired$num_clusters_reference, paired$num_clusters_rebuilt)
        paired$changed_branches_in_clusters <- different_count(
          paired$branches_in_clusters_reference, paired$branches_in_clusters_rebuilt)
        paired <- paired %>%
          filter(changed_num_clusters | changed_branches_in_clusters) %>%
          arrange(total_branches_rebuilt, parent_number, archive_version_year) %>%
          slice_head(n = 12L)
        paired$passes_growth_range <- !is.na(paired$sales_growth_rebuilt) &
          paired$sales_growth_rebuilt >= -1 & paired$sales_growth_rebuilt <= 1
        paired$dbscan_version <- rep(as.character(utils::packageVersion("dbscan")), nrow(paired))
        paired$R_version <- rep(R.version.string, nrow(paired))
        paired$branch_input <- rep(branch_path, nrow(paired))
        # cl still contains each source row; exact within-group order is recorded.
        debug_rows <- cl %>% semi_join(paired[debug_keys], by = debug_keys) %>%
          select(abi, parent_number, archive_version_year, fips_code,
                 longitude, latitude, cluster, source_row_order) %>%
          arrange(parent_number, archive_version_year, source_row_order)
        data.table::fwrite(paired,
          file.path(check_dir, "DataAxle_cluster_debug_summary.csv"), na = "NA", quote = "auto")
        data.table::fwrite(debug_rows,
          file.path(check_dir, "DataAxle_cluster_debug_rows.csv"), na = "NA", quote = "auto")
        legacy_attempt <- function(label, code) tryCatch(force(code), error = function(e)
          register_output(label, "diagnostic_error", conditionMessage(e)))
        legacy_write <- function(x, name) data.table::fwrite(x,
          file.path(check_dir, name), na = "NA", quote = "auto")
        legacy_normalize <- function(x) {
          x$parent_number <- pad_id(x$parent_number, 9)
          x$archive_version_year <- num(x$archive_version_year)
          x
        }
        legacy_attempt("Data Axle old cluster rows", {
          legacy_path <- find_input("food_company_branch_cluster_final.csv")
          if (!is.na(legacy_path)) {
            needed <- c("abi", debug_keys, "cluster")
            legacy_header <- names(data.table::fread(legacy_path, nrows = 0))
            if (!all(needed %in% legacy_header)) stop("Old cluster CSV lacks: ",
              paste(setdiff(needed, legacy_header), collapse = ", "))
            old_rows <- legacy_normalize(read_csv_text(legacy_path, select = needed))
            old_rows$abi <- pad_id(old_rows$abi, 9)
            old_rows$cluster <- num(old_rows$cluster)
            old_rows$legacy_file_row_order <- seq_len(nrow(old_rows))
            old_rows <- semi_join(old_rows, paired[debug_keys], by = debug_keys)
            legacy_write(old_rows, "DataAxle_cluster_legacy_rows.csv")
            old_coord_path <- find_input("food_company_longandlai.csv")
            if (!is.na(old_coord_path)) {
              coord_keys <- c("abi", "archive_version_year")
              needed_coord <- c(coord_keys, "longitude", "latitude")
              coord_header <- names(data.table::fread(old_coord_path, nrows = 0))
              if (!all(needed_coord %in% coord_header)) stop("Old coordinate CSV lacks: ",
                paste(setdiff(needed_coord, coord_header), collapse = ", "))
              old_coords <- read_csv_text(old_coord_path, select = needed_coord)
              old_coords$abi <- pad_id(old_coords$abi, 9)
              old_coords$archive_version_year <- num(old_coords$archive_version_year)
              old_coords$longitude <- num(old_coords$longitude)
              old_coords$latitude <- num(old_coords$latitude)
              old_coords$legacy_coordinate_row_order <- seq_len(nrow(old_coords))
              old_coords <- semi_join(old_coords, old_rows[coord_keys], by = coord_keys)
              legacy_write(old_coords, "DataAxle_cluster_legacy_coordinates.csv")
              counts <- old_coords %>% count(abi, archive_version_year,
                                             name = "legacy_coordinate_matches")
              old_rows <- left_join(old_rows, counts, by = coord_keys)
              ambiguous <- counts %>% filter(legacy_coordinate_matches > 1L)
              legacy_write(semi_join(old_coords, ambiguous, by = coord_keys),
                           "DataAxle_cluster_legacy_coordinate_duplicates.csv")
              unique_coords <- old_coords %>% anti_join(ambiguous, by = coord_keys)
              old_rows <- left_join(old_rows, unique_coords, by = coord_keys)
            }
            legacy_write(old_rows, "DataAxle_cluster_legacy_rows.csv")
            register_output("Data Axle old cluster rows", "diagnostic_generated",
              paste0(nrow(old_rows), " old cluster rows for selected parent-years; ",
                     "coordinates, when available, are joined only on unique abi-year keys. ",
                     "Old clusters have not replaced the new HDBSCAN calculation."))
          }
        })
        legacy_attempt("Data Axle old cluster totals", {
          old_total_path <- find_input("food_company_branch_cluster_percentage_final.csv")
          if (!is.na(old_total_path)) {
            needed <- c(debug_keys, debug_values)
            old_header <- names(data.table::fread(old_total_path, nrows = 0))
            if (!all(needed %in% old_header)) stop("Old cluster totals lack: ",
              paste(setdiff(needed, old_header), collapse = ", "))
            old_total <- legacy_normalize(read_csv_text(old_total_path, select = needed))
            old_total[debug_values] <- lapply(old_total[debug_values], num)
            dup <- duplicated(old_total[debug_keys]) | duplicated(old_total[debug_keys], fromLast = TRUE)
            if (any(dup)) {
              legacy_write(old_total[dup, ], "DataAxle_cluster_legacy_total_duplicates.csv")
              stop("Old totals contain duplicate parent-year keys; saved all duplicate rows without choosing one.")
            }
            old_total$legacy_present <- TRUE
            rebuilt_total <- branch_network %>% select(all_of(needed))
            rebuilt_total$rebuilt_present <- TRUE
            combined <- full_join(old_total, rebuilt_total, by = debug_keys,
                                  suffix = c("_legacy", "_rebuilt"))
            retained_total <- reference %>% select(all_of(needed))
            names(retained_total)[match(debug_values, names(retained_total))] <-
              paste0(debug_values, "_reference")
            retained_total$reference_present <- TRUE
            combined <- full_join(combined, retained_total, by = debug_keys)
            for (v in debug_values) {
              combined[[paste0(v, "_legacy_vs_rebuilt")]] <-
                different_count(combined[[paste0(v, "_legacy")]], combined[[paste0(v, "_rebuilt")]])
              combined[[paste0(v, "_legacy_vs_reference")]] <-
                different_count(combined[[paste0(v, "_legacy")]], combined[[paste0(v, "_reference")]])
            }
            legacy_write(combined, "DataAxle_cluster_legacy_total_comparison.csv")
            register_output("Data Axle old cluster totals", "diagnostic_generated",
              "Compared old saved totals with newly calculated totals and retained final data; presence columns distinguish absent keys from missing values.")
          }
        })

        register_output("Data Axle cluster diagnostic", "generated",
          paste0("Exported ", nrow(paired), " small differing parent-year groups and ",
                 nrow(debug_rows), " source-coordinate rows; dbscan ",
                 as.character(utils::packageVersion("dbscan")), ". Reference used only to select diagnostic cases."))
      }
    }), error = function(e) register_output("Data Axle cluster diagnostic", "error", conditionMessage(e)))

    rm(b, parent_rows, spatial_rows, cl)
  }, error = function(e) register_output("Data Axle", "error", conditionMessage(e)))
}

# ============================================================================
# Table 1: Impact of Major Damaging Disasters on Food Industry Establishment and Employment Change
# 4. Tapestry: supplied food county-year input -> disaster merge
# ============================================================================
tryCatch({
  tap_cols <- c("area_fips", "year", "tap_estabs_count", "tap_wages_est_5", "tap_emplvl_est_5")
  industry_path <- find_input("food_industry_county_year_input.csv", required = TRUE)
  industry_processed <- read_csv_text(industry_path)
  tap_missing <- setdiff(tap_cols, names(industry_processed))
  if (length(tap_missing)) {
    stop("food_industry_county_year_input.csv：", paste(tap_missing, collapse = ", "))
  }
  food_industry <- industry_processed[tap_cols]
  food_industry$area_fips <- pad_id(food_industry$area_fips, 5)
  food_industry[tap_cols[2:5]] <- lapply(food_industry[tap_cols[2:5]], num)
  if (any(is.na(food_industry$area_fips) |
          !grepl("^[0-9]{5}$", food_industry$area_fips)) ||
      any(!is.finite(food_industry$year) | food_industry$year != floor(food_industry$year))) {
    stop("food_industry_county_year_input.csv")
  }
  if (anyDuplicated(food_industry[c("area_fips", "year")])) {
    stop("food_industry_county_year_input.csv 的 area_fips/year")
  }
  register_output("Tapestry county-year input", "processed_input",
                  paste("Directly loaded", nrow(food_industry), "county-years from", industry_path,
                        "; annual industry CSVs are not read and NAICS totals are not regrouped."))
  rm(industry_processed)
  save_data(food_industry, "food_industry_county_year_input.csv")
  if (is.null(disaster_year_allhazards)) {
    register_output("Table 1 disaster merge", "missing_input", "Need disaster_final_filtered.csv.")
  } else {
    tap_hazards <- c("Flooding", "Tornado", "Drought", "Severe Storm/Thunder Storm",
                     "Wind", "Hurricane/Tropical Storm", "Winter Weather", "Lightning",
                     "Heat", "Hail", "Fog", "Coastal", "Landslide", "Avalanche",
                     "Earthquake", "Tsunami/Seiche", "Volcano")
    tap_large_2022 <- disaster_monthly %>%
      filter(Year < 2023, Hazard %in% tap_hazards) %>%
      mutate(Large = as.integer(damage_2023 / 1.034 > 1000000)) %>%
      group_by(County_FIPS, Year, Hazard) %>%
      summarise(Large = sum(Large, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Hazard, values_from = Large,
                  names_prefix = "Large_", values_fill = 0L)
    tap_disaster <- as.data.frame(disaster_year_allhazards) %>%
      select(County_FIPS, Year, all_of(paste0("Frequency_", tap_hazards))) %>%
      left_join(tap_large_2022, by = c("County_FIPS", "Year")) %>%
      mutate(across(all_of(paste0("Large_", tap_hazards)),
                    ~ if_else(Year < 2023, coalesce(.x, 0L), NA_integer_))) %>%
      select(County_FIPS, Year, all_of(paste0("Frequency_", tap_hazards)),
             all_of(paste0("Large_", tap_hazards)))
    food_industry_candidate <- food_industry %>% left_join(
      tap_disaster, by = c("area_fips" = "County_FIPS", "year" = "Year"))
    tap_names <- c("flooding", "tornado", "drought", "severestormthunderstorm", "wind",
                   "hurricanetropicalstorm", "winterweather", "lightning", "heat", "hail",
                   "fog", "coastal", "landslide", "avalanche", "earthquake", "tsunamiseiche", "volcano")
    new_names <- c(paste0("frequency_", tap_names), paste0("large_", tap_names))
    new_names[new_names == "frequency_severestormthunderstorm"] <- "frequency_severestormthunderstor"
    names(food_industry_candidate)[-(1:5)] <- new_names
    save_data(food_industry_candidate, "food_industry_disaster_candidate.csv")
    food_industry_candidate_ready <- TRUE
    register_output("Table 1 input", "candidate_needs_comparison",
                    "Annual frequencies regenerated.")
  }
}, error = function(e) register_output("Tapestry", "error", conditionMessage(e)))


comparison_summary <- data.frame()
compare_final <- function(rebuilt, reference_name, keys, label) {
  tryCatch({
    ref_path <- find_input(reference_name)
    if (is.null(rebuilt) || is.na(ref_path)) {
      register_output(paste0("Check ", label), "not_run", "Rebuilt data or retained reference final is unavailable")
      return(FALSE)
    }
    old <- read_csv_text(ref_path)
    fresh <- as.data.frame(rebuilt)
    if (!all(keys %in% names(old)) || !all(keys %in% names(fresh))) stop("Missing comparison keys")
    for (k in keys) {
      old[[k]] <- as.character(old[[k]])
      fresh[[k]] <- as.character(fresh[[k]])
      if (k == "gvkey") { old[[k]] <- pad_id(old[[k]], 6); fresh[[k]] <- pad_id(fresh[[k]], 6) }
      if (k == "parent_number") { old[[k]] <- pad_id(old[[k]], 9); fresh[[k]] <- pad_id(fresh[[k]], 9) }
      if (k %in% c("County_FIPS", "area_fips")) {
        old[[k]] <- pad_id(old[[k]], 5); fresh[[k]] <- pad_id(fresh[[k]], 5)
      }
    }
    if (anyDuplicated(old[keys]) || anyDuplicated(fresh[keys])) {
      stop("Comparison requires unique keys: ", paste(keys, collapse = "+"))
    }
    old_only <- dplyr::anti_join(old, fresh[keys], by = keys)
    new_only <- dplyr::anti_join(fresh, old[keys], by = keys)
    data.table::fwrite(old_only, file.path(check_dir, paste0(label, "_reference_only_rows.csv")), na = "NA")
    data.table::fwrite(new_only, file.path(check_dir, paste0(label, "_rebuilt_only_rows.csv")), na = "NA")
    key_string <- function(d) do.call(paste, c(d[keys], sep = "\r"))
    match_index <- match(key_string(old), key_string(fresh))
    ok <- !is.na(match_index)
    a <- old[ok, , drop = FALSE]; b <- fresh[match_index[ok], , drop = FALSE]
    common <- setdiff(intersect(names(a), names(b)), keys)
    # Keep identifiers and descriptions textual. Other genuinely numeric fields
    # are compared numerically, so CSV formatting alone does not create a failure.
    text_fields <- c("gvkey", "cid", "parent_number", "abi", "cnms", "ctype", "gareac",
      "gareat", "sid", "stype", "srcdate", "conm", "tic", "cusip", "cik", "sic", "naics",
      "company", "County_FIPS", "area_fips", "primary_sic_code")
    differences <- list(); variable_checks <- list()
    for (v in common) {
      av <- as.character(a[[v]]); bv <- as.character(b[[v]])
      av[av %in% c("", "NA", "NaN")] <- NA_character_
      bv[bv %in% c("", "NA", "NaN")] <- NA_character_
      if (v %in% c("County_FIPS", "area_fips")) { av <- pad_id(av, 5); bv <- pad_id(bv, 5) }
      if (v == "primary_sic_code") { av <- pad_id(av, 4); bv <- pad_id(bv, 4) }
      an <- suppressWarnings(as.numeric(av)); bn <- suppressWarnings(as.numeric(bv))
      numeric_field <- !v %in% text_fields &&
        all(is.na(av) | !is.na(an)) && all(is.na(bv) | !is.na(bn))
      equal <- (is.na(av) & is.na(bv))
      if (numeric_field) {
        finite <- is.finite(an) & is.finite(bn)
        equal[finite] <- abs(an[finite] - bn[finite]) <= 1e-10 * pmax(1, abs(an[finite]), abs(bn[finite]))
        infinite <- is.infinite(an) & is.infinite(bn)
        equal[infinite] <- an[infinite] == bn[infinite]
      } else {
        present <- !is.na(av) & !is.na(bv)
        equal[present] <- av[present] == bv[present]
      }
      equal[is.na(equal)] <- FALSE
      variable_checks[[v]] <- data.frame(variable = v, matched_rows = nrow(a),
        equal_values = sum(equal), different_values = sum(!equal), numeric_comparison = numeric_field)
      if (any(!equal)) {
        z <- a[!equal, keys, drop = FALSE]
        z$variable <- v; z$reference_value <- av[!equal]; z$rebuilt_value <- bv[!equal]
        differences[[v]] <- z
      }
    }
    vars <- dplyr::bind_rows(variable_checks)
    if (length(differences)) diff <- dplyr::bind_rows(differences) else {
      diff <- a[FALSE, keys, drop = FALSE]
      diff$variable <- character(); diff$reference_value <- character(); diff$rebuilt_value <- character()
    }
    data.table::fwrite(vars, file.path(check_dir, paste0(label, "_variable_checks.csv")), na = "NA")
    data.table::fwrite(diff, file.path(check_dir, paste0(label, "_value_differences.csv")), na = "NA")
    missing_cols <- setdiff(names(old), names(fresh)); extra_cols <- setdiff(names(fresh), names(old))
    identical_order <- identical(names(old), names(fresh))
    passed <- !nrow(old_only) && !nrow(new_only) && !nrow(diff) &&
      !length(missing_cols) && !length(extra_cols) && identical_order
    summary <- data.frame(dataset = label, reference_rows = nrow(old), rebuilt_rows = nrow(fresh),
      matched_keys = nrow(a), reference_only_rows = nrow(old_only), rebuilt_only_rows = nrow(new_only),
      different_values = nrow(diff), missing_columns = paste(missing_cols, collapse = ";"),
      extra_columns = paste(extra_cols, collapse = ";"), identical_column_order = identical_order,
      passed = passed)
    comparison_summary <<- dplyr::bind_rows(comparison_summary, summary)
    register_output(paste0("Check ", label), if (passed) "matched" else "differences_found",
      paste(nrow(diff), "different values;", nrow(old_only), "reference-only rows;",
            nrow(new_only), "rebuilt-only rows. See checks/", sep = " "))
    passed
  }, error = function(e) {
    register_output(paste0("Check ", label), "error", conditionMessage(e)); FALSE
  })
}

compare_final(customer_base, "merged_disaster_food_customer_12.csv",
              c("gvkey", "cid", "year_srcdate"), "Customer_base")
compare_final(customer_C, "merged_disaster_food_customer_12C.csv",
              c("gvkey", "cid", "year_srcdate"), "Customer_C")
compare_final(customer_S, "merged_disaster_food_customer_12S.csv",
              c("gvkey", "cid", "year_srcdate"), "Customer_S")
branch_matched <- compare_final(branch_candidate, "merged_disaster_food_customer_12dataaxle.csv",
                               c("parent_number", "archive_version_year"), "DataAxle_parent_sales_candidate")
if (isTRUE(branch_candidate_ready)) {
  save_data(branch_candidate, "merged_disaster_food_customer_12dataaxle.csv")
  register_output("Stata input: Data Axle", if (branch_matched) "matched" else "exported_unverified",
    "Rebuilt parent-sales data saved under the canonical name in replication_output/data; use checks and the same Stata do-file to validate results. Original root input is unchanged.")
}
industry_matched <- compare_final(food_industry_candidate, "food_industry_disaster.csv",
                                 c("area_fips", "year"), "Tapestry")
if (isTRUE(food_industry_candidate_ready)) {
  save_data(food_industry_candidate, "food_industry_disaster.csv")
  register_output("Stata input: Tapestry", if (industry_matched) "matched" else "exported_unverified",
    "Rebuilt county-year data saved under the canonical name in replication_output/data; export alone does not establish replication of Table 1.")
}
if (nrow(comparison_summary)) data.table::fwrite(comparison_summary,
  file.path(check_dir, "final_dataset_comparison.csv"), na = "NA")

# ============================================================================
# Table A1: Descriptive Statistics
# Three paper panels; statistics use rebuilt data, not the retained final files.
# Customer/C/S: sales_growth <= p99; DataAxle: -1 <= sales_growth <= 1.
# County changes: adjacent calendar years, with no trimming for this summary.
# ============================================================================
local({
  a1_notes <- c(
    "Obs. counts finite, nonmissing values separately for each variable.",
    "Percentiles use R quantile(type = 2), matching Stata's default definition.",
    "Num. customers counts distinct customers per supplier-year within the selected customer sample, repeated over its supplier-customer-year observations.",
    "The original do-file's cumulative counter matches the paper's customer mean 3.449 but is not a customer total. Its comparison is saved in checks/Table_A1_customer_count_comparison.csv.",
    "In the retained original customer sample, sales-growth p99 is 2.88211382; the paper prints 0.882. Statistics here are calculated from the current data."
  )
  a1_rules <- data.frame(Source = character(), Rule = character(), Rows = integer())
  a1_rule <- function(source, rule, d) {
    a1_rules <<- rbind(a1_rules, data.frame(Source = source, Rule = rule,
      Rows = if (is.null(d)) NA_integer_ else nrow(d)))
  }
  a1_get <- function(d, variable) {
    if (is.null(d)) return(numeric())
    if (!variable %in% names(d)) return(rep(NA_real_, nrow(d)))
    num(d[[variable]])
  }
  a1_stats <- function(d, variable, label, panel, multiplier = 1) {
    available <- !is.null(d) && variable %in% names(d)
    x <- a1_get(d, variable) * multiplier
    x <- x[is.finite(x)]
    q <- if (length(x)) quantile(x, c(.01, .5, .99), type = 2, names = FALSE)
         else rep(NA_real_, 3)
    data.frame(Panel = panel, Variable = label,
      "Obs." = if (available) length(x) else NA_integer_,
      Mean = if (length(x)) mean(x) else NA_real_,
      "Std.Dev" = if (length(x) > 1L) sd(x) else NA_real_,
      p1 = q[1], p50 = q[2], p99 = q[3], check.names = FALSE)
  }
  a1_growth <- a1_get(customer_base, "sales_growth")
  a1_growth <- a1_growth[is.finite(a1_growth)]
  a1_p99 <- if (length(a1_growth))
    unname(quantile(a1_growth, .99, type = 2)) else NA_real_
  a1_trim_customer <- function(d) {
    if (is.null(d) || !is.finite(a1_p99)) return(NULL)
    x <- a1_get(d, "sales_growth")
    d[is.finite(x) & x <= a1_p99, , drop = FALSE]
  }
  a1_customer <- a1_trim_customer(customer_base)
  a1_C <- a1_trim_customer(customer_C)
  a1_S <- a1_trim_customer(customer_S)
  # Original < 5.416667 equals <= exact p99 (5.41666666666667) in the old data.
  # Calculate that percentile from the rebuilt customer base for this run.
  a1_customer_rule <- paste0("Finite sales_growth <= base-sample p99 = ",
                             format(a1_p99, digits = 16))
  a1_rule("merged_disaster_food_customer_12.csv", a1_customer_rule, a1_customer)
  a1_rule("merged_disaster_food_customer_12C.csv", a1_customer_rule, a1_C)
  a1_rule("merged_disaster_food_customer_12S.csv", a1_customer_rule, a1_S)

  # Customer totals and the historical cumulative counter are separate measures.
  if (!is.null(a1_customer)) {
    cs_need(a1_customer, c("gvkey", "year_srcdate", "cid"), "Table A1 customers")
    a1_customer$num_customers <- a1_customer$legacy_customer_counter <- NA_real_
    valid <- !is.na(a1_customer$gvkey) & !is.na(a1_customer$year_srcdate) &
             !is.na(a1_customer$cid)
    groups <- split(which(valid), paste(a1_customer$gvkey[valid],
                                        a1_customer$year_srcdate[valid], sep = "|"))
    for (i in groups) {
      customer_ids <- sort(unique(as.character(a1_customer$cid[i])))
      a1_customer$num_customers[i] <- length(customer_ids)
      a1_customer$legacy_customer_counter[i] <-
        match(as.character(a1_customer$cid[i]), customer_ids)
    }
    customer_comparison <- rbind(
      a1_stats(a1_customer, "num_customers", "Actual distinct customer count", "Panel A"),
      a1_stats(a1_customer, "legacy_customer_counter", "Original cumulative counter", "Panel A"))
    utils::write.csv(customer_comparison,
      file.path(check_dir, "Table_A1_customer_count_comparison.csv"), row.names = FALSE, na = "NA")
  }

  # Original food-firm financial input; never summarize the all-industry source.
  a1_financial <- NULL
  a1_financial_source <- "Food financial input unavailable"
  tryCatch({
    p_food <- find_input("financial_information_food.csv")
    if (!is.na(p_food)) {
      a1_financial <- read_csv_text(p_food)
      a1_financial_source <- "financial_information_food.csv"
    } else {
      # The main pipeline drops sic after computing financial ratios. Recover
      # its food-row indicator from the same source without changing that panel.
      p_full <- find_input("some fundament.csv")
      if (is.na(p_full)) p_full <- find_input("financial_information.csv")
      if (!is.na(p_full) && !is.null(financial_information)) {
        h <- names(data.table::fread(p_full, nrows = 0, check.names = FALSE))
        if (all(c("gvkey", "fyear", "sic") %in% h)) {
          keys <- read_csv_text(p_full, select = c("gvkey", "fyear", "sic"))
          keys$gvkey <- pad_id(keys$gvkey, 6)
          keys$fyear <- num(keys$fyear)
          if (nrow(keys) != nrow(financial_information) ||
              !identical(keys$gvkey, as.character(financial_information$gvkey)) ||
              !identical(keys$fyear, num(financial_information$fyear))) {
            stop("Financial source rows do not align; provide financial_information_food.csv.")
          }
          a1_financial <- financial_information[
            pad_id(keys$sic, 4) %in% cs_food_sic, , drop = FALSE]
          a1_financial_source <- paste(basename(p_full), "with original food SIC filter")
        }
      }
    }
    if (!is.null(a1_financial)) cs_need(a1_financial,
      c("ROA", "Size", "RD_to_Sales", "Gross_Profit_Margin", "Operating_Leverage", "Cash_Holdings"),
      "Table A1 food financial input")
  }, error = function(e) {
    a1_financial <<- NULL
    a1_notes <<- c(a1_notes, paste("Financial input:", conditionMessage(e)))
  })
  if (is.null(a1_financial)) {
    a1_notes <- c(a1_notes,
      "Financial rows are unavailable: supply financial_information_food.csv, or the original financial source with its sic column. NA does not mean zero.")
    register_output("Table A1 financial rows", "missing_input",
      "Need food-only financial ratios; all-industry financial_information is not substituted.")
  }
  a1_rule(a1_financial_source, "Food firm-year records; no additional A1 trimming", a1_financial)

  # Annual changes match Stata's L. operator: a missing prior year yields NA.
  a1_industry <- food_industry_candidate
  if (!is.null(a1_industry)) {
    cs_need(a1_industry, c("area_fips", "year", "tap_estabs_count", "tap_emplvl_est_5"),
            "Table A1 county panel")
    key <- paste(a1_industry$area_fips, a1_industry$year, sep = "|")
    if (anyDuplicated(key)) stop("Table A1 county-year keys are not unique.")
    previous <- match(paste(a1_industry$area_fips, num(a1_industry$year) - 1,
                           sep = "|"), key)
    a1_industry$dtap_estabs_count <- a1_get(a1_industry, "tap_estabs_count") -
      a1_get(a1_industry, "tap_estabs_count")[previous]
    a1_industry$dtap_emplvl_est_5 <- a1_get(a1_industry, "tap_emplvl_est_5") -
      a1_get(a1_industry, "tap_emplvl_est_5")[previous]
  }
  a1_rule("food_industry_disaster.csv", "Untrimmed county-years; t minus t-1 for changes", a1_industry)
  a1_branch <- branch_candidate
  if (!is.null(a1_branch)) {
    x <- a1_get(a1_branch, "sales_growth")
    a1_branch <- a1_branch[is.finite(x) & x >= -1 & x <= 1, , drop = FALSE]
    total <- a1_get(a1_branch, "total_branches")
    a1_branch$p_branch_cluster <- ifelse(is.finite(total) & total > 0,
      a1_get(a1_branch, "branches_in_clusters") / total * 100, NA_real_)
  }
  a1_rule("merged_disaster_food_customer_12dataaxle.csv", "Finite sales_growth in [-1,1]", a1_branch)

  A <- "Panel A: Firm Sales"
  B <- "Panel B: Establishment Data"
  C <- "Panel C: Branch Data"
  table_A1 <- do.call(rbind, list(
    a1_stats(a1_customer, "sales_growth", "Sales growth", A),
    a1_stats(a1_customer, "num_customers", "Num. customers", A),
    a1_stats(a1_customer, "12_all_Hurricane/Tropical Storm", "Hurricane shock", A),
    a1_stats(a1_customer, "12_Large_Hurricane/Tropical Storm", "Large hurricane", A),
    a1_stats(a1_customer, "12_all_Flooding", "Flooding shock", A),
    a1_stats(a1_customer, "12_Large_Flooding", "Large flooding", A),
    a1_stats(a1_C, "mainsale_ratio", "Concentration sale ratio", A),
    a1_stats(a1_C, "growth_mainsale_ratio", "Concentration sale ratio growth", A, 100),
    a1_stats(a1_S, "vd_BUSSEG", "BUSSEG diversification index", A),
    a1_stats(a1_S, "vd_GEOSEG", "GEOSEG diversification index", A),
    a1_stats(a1_financial, "ROA", "ROA", A),
    a1_stats(a1_financial, "Size", "Size", A),
    a1_stats(a1_financial, "RD_to_Sales", "R&D to Sales", A),
    a1_stats(a1_financial, "Gross_Profit_Margin", "Gross Profit Margin", A),
    a1_stats(a1_financial, "Operating_Leverage", "Operating Leverage", A),
    a1_stats(a1_financial, "Cash_Holdings", "Cash Holdings", A),
    a1_stats(a1_industry, "dtap_estabs_count", "Difference of num.establishment", B),
    a1_stats(a1_industry, "dtap_emplvl_est_5", "Difference of num.employment", B),
    a1_stats(a1_industry, "frequency_hurricanetropicalstorm", "e.g. Hurricane shock", B),
    a1_stats(a1_industry, "large_hurricanetropicalstorm", "e.g. Large hurricane", B),
    a1_stats(a1_industry, "frequency_flooding", "e.g. Flooding shock", B),
    a1_stats(a1_industry, "large_flooding", "e.g. Large flooding", B),
    a1_stats(a1_branch, "sales_growth", "Sales growth", C),
    a1_stats(a1_branch, "num_clusters", "Num. clusters", C),
    a1_stats(a1_branch, "total_branches", "Num. branches", C),
    a1_stats(a1_branch, "p_branch_cluster", "Clustered branches percentage", C),
    a1_stats(a1_branch, "Frequency_Hurricane", "Hurricane shock", C),
    a1_stats(a1_branch, "Large_Hurricane", "Large hurricane", C)
  ))
  save_table(table_A1, "Table_A1.csv")
  utils::write.csv(a1_rules, file.path(check_dir, "Table_A1_sample_rules.csv"), row.names = FALSE, na = "NA")
  writeLines(a1_notes, file.path(table_dir, "Table_A1_notes.txt"), useBytes = TRUE)

  # A readable table with paper panel headings; no extra rendering packages.
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  html <- c('<!doctype html><html><head><meta charset="UTF-8">',
    '<title>Table A1: Descriptive Statistics</title>',
    '<style>body{font-family:"Times New Roman",serif;margin:28px auto;max-width:1000px;padding:0 18px}table{width:100%;border-collapse:collapse}caption{font-size:18px;padding:10px}th,td{padding:5px 10px;text-align:right}th:first-child,td:first-child{text-align:left}.panel td{border-top:1px solid black;border-bottom:1px solid black;text-align:left}.sub td{text-align:left}thead{border-top:2px solid black;border-bottom:1px solid black}tbody{border-bottom:2px solid black}.notes{font-size:13px;margin-top:16px}h2{font-size:15px}</style></head><body>',
    '<table><caption>Table A1: Descriptive Statistics</caption><thead><tr>',
    paste0('<th>', c('Variable', 'Obs.', 'Mean', 'Std.Dev', 'p1', 'p50', 'p99'), '</th>', collapse = ''),
    '</tr></thead><tbody>')
  last_panel <- ""
  for (i in seq_len(nrow(table_A1))) {
    z <- table_A1[i, ]
    if (z$Panel != last_panel) {
      html <- c(html, paste0('<tr class="panel"><td colspan="7">', esc(z$Panel), '</td></tr>'))
      last_panel <- z$Panel
    }
    if (i == 11L) html <- c(html,
      '<tr class="sub"><td colspan="7">Food Company Financial Information:</td></tr>')
    values <- c(if (is.na(z[["Obs."]])) "NA" else format(z[["Obs."]], big.mark = ",", trim = TRUE),
      vapply(z[c("Mean", "Std.Dev", "p1", "p50", "p99")], function(v)
        if (is.na(v)) "NA" else sprintf("%.3f", v), character(1)))
    html <- c(html, paste0('<tr><td>', esc(z$Variable), '</td>',
      paste0('<td>', values, '</td>', collapse = ''), '</tr>'))
  }
  html <- c(html, '</tbody></table><div class="notes"><h2>Notes and replication checks</h2>',
    paste0('<p>', esc(a1_notes), '</p>'), '</div></body></html>')
  writeLines(html, file.path(table_dir, "Table_A1.html"), useBytes = TRUE)
  register_output("Table A1", "generated_needs_comparison",
    "28 paper variables in three panels: Table_A1.csv and Table_A1.html. See notes for corrected customer counts, the printed sales p99 discrepancy and financial-input availability.")
})

register_output("Figure 4; Tables 1-4, A5-A8", "Stata_step",
  "Run the author's already-organized do-files on checked data. This R file does not invent new models, weights or trimming rules.")
register_output("Figures 5, A5", "Stata_cached_coefficients",
  "Existing heterogeneity hazard to sector.csv allows redraw; the original coefficient-generation regressions were not recovered.")
# The folder tree revealed additional sources. Inspect headers only until their
# contents and original transformations can be checked; do not switch samples.
additional_names <- c("nonfood_transaction_disaster_frequency.csv", "Customer.csv",
  "customer_.csv", "customer_ _.csv", "food_company_branch.csv",
  "food_company_longandlai.csv", "food_company_branch_cluster_final.csv",
  "food_company_branch_cluster_percentage_final.csv", "complete_disaster.csv")
additional_paths <- INPUT_FILES[tolower(basename(INPUT_FILES)) %in% tolower(additional_names)]
additional_headers <- bind_rows(lapply(additional_paths, function(p) {
  header_error <- ""
  header <- tryCatch(names(data.table::fread(p, nrows = 0, check.names = FALSE)),
    error = function(e) { header_error <<- conditionMessage(e); character() })
  data.frame(file = basename(p), path = p, bytes = file.info(p)$size,
             column_count = length(header), columns = paste(header, collapse = ";"),
             error = header_error)
}))
if (nrow(additional_headers)) data.table::fwrite(additional_headers,
  file.path(check_dir, "available_source_headers.csv"), na = "NA")

# ============================================================================
# Table A4: Impact of Main Damaging Disasters on Non-food Firm Sale Growth
# ============================================================================
if (any(tolower(basename(additional_paths)) == "nonfood_transaction_disaster_frequency.csv")) {
  register_output("Table A4", "input_found_unverified",
    "nonfood_transaction_disaster_frequency.csv exists; see available_source_headers.csv. Its sample, definitions and original estimation code still need verification.")
} else register_output("Table A4", "missing_nonfood_input",
  "Food-only customer input cannot reconstruct the nonfood comparison; no replacement sample is invented.")

# ============================================================================
# Figure 5: Heatmap for the Frequency of Disasters Affecting Agricultural Sectors - Headquarters
# Figure A5: Robust Heatmap for the Frequency of Disasters Affecting Agricultural Sectors - Headquarters
# ============================================================================
# Keep the existing auxiliary Stata inputs available beside checked rebuilt data.
for (name in c("states_list.csv", "heterogeneity hazard to sector.csv")) {
  p <- find_input(name)
  if (!is.na(p)) file.copy(p, file.path(data_dir, name), overwrite = TRUE)
}


# ============================================================================
# 6. Paper output inventory and static SIC appendix
# Table A3 below is transcribed from the supplied manuscript, not used as the
# ============================================================================

# ============================================================================
# Table A3: SIC codes used to define food-sector firms
# ============================================================================
table_A3 <- utils::read.csv(text = "sic,title
0100,Agricultural Production Crops
0110,Cash Grains
0111,Wheat
0112,Rice
0115,Corn
0116,Soybeans
0119,\"Cash Grains, Not Elsewhere Classified\"
0130,\"Field Crops, Except Cash Grains\"
0131,Cotton
0132,Tobacco
0133,Sugarcane and Sugar Beets
0134,Irish Potatoes
0139,\"Field Crops, Except Cash Grains, Not Elsewhere Classified\"
0160,Vegetables and Melons
0161,Vegetables and Melons
0170,Fruits and Tree Nuts
0171,Berry Crops
0172,Grapes
0173,Tree Nuts
0174,Citrus Fruits
0175,Deciduous Tree Fruits
0179,\"Fruits and Tree Nuts, Not Elsewhere Classified\"
0180,Horticultural Specialties
0181,Ornamental Floriculture and Nursery Products
0182,Food Crops Grown Under Cover
0190,\"General Farms, Primarily Crop\"
0191,\"General Farms, Primarily Crop\"
0200,Agricultural Production Livestock and Animal Specialties
0210,\"Livestock, Except Dairy and Poultry\"
0211,Beef Cattle Feedlots
0212,\"Beef Cattle, Except Feedlots\"
0213,Hogs
0214,Sheep and Goats
0219,\"General Livestock, Except Dairy and Poultry\"
0240,Dairy Farms
0241,Dairy Farms
0250,Poultry and Eggs
0251,\"Broiler, Fryer, and Roaster Chickens\"
0252,Chicken Eggs
0253,Turkeys and Turkey Eggs
0254,Poultry Hatcheries
0259,\"Poultry and Eggs, Not Elsewhere Classified\"
0270,Animal Specialties
0271,Fur-bearing Animals and Rabbits
0272,Horses and Other Equines
0273,Animal Aquaculture
0279,\"Animal Specialties, Not Elsewhere Classified\"
0290,\"General Farms, Primarily Livestock and Animal Specialties\"
0291,\"General Farms, Primarily Livestock and Animal Specialties\"
0700,Agricultural Services
0710,Soil Preparation Services
0711,Soil Preparation Services
0720,Crop Services
0721,\"Crop Planting, Cultivating, and Protecting\"
0722,\"Crop Harvesting, Primarily By Machine\"
0723,\"Crop Preparation Services For Market, Except Cotton Ginning\"
0724,Cotton Ginning
0740,Veterinary Services
0741,Veterinary Services For Livestock
0742,Veterinary Services For Animal Specialties
0750,\"Animal Services, Except Veterinary\"
0751,\"Livestock Services, Except Veterinary\"
0752,\"Animal Specialty Services, Except Veterinary\"
0760,Farm Labor and Management Services
0761,Farm Labor Contractors and Crew Leaders
0762,Farm Management Services
0780,Landscape and Horticultural Services
0782,Lawn and Garden Services
0783,Ornamental Shrub and Tree Services
0910,Commercial Fishing
0912,Finfish
0913,Shellfish
0919,Miscellaneous Marine Products
2000,Food and Kindred Products
2010,Meat Products
2011,Meat Packing Plants
2013,Sausages and Other Prepared Meat Products
2015,Poultry Slaughtering and Processing
2020,Dairy Products
2021,Creamery Butter
2022,\"Natural, Processed, and Imitation Cheese\"
2023,\"Dry, Condensed, and Evaporated Dairy Products\"
2024,Ice Cream and Frozen Desserts
2026,Fluid Milk
2030,\"Canned, Frozen, and Preserved Fruits, Vegetables, and Food Specialties\"
2032,Canned Specialties
2033,\"Canned Fruits, Vegetables, Preserves, Jams, and Jellies\"
2034,\"Dried and Dehydrated Fruits, Vegetables, and Soup Mixes\"
2035,\"Pickled Fruits and Vegetables, Vegetable Sauces and Seasonings, and Salad Dressings\"
2037,\"Frozen Fruits, Fruit Juices, and Vegetables\"
2038,\"Frozen Specialties, Not Elsewhere Classified\"
2040,Grain Mill Products
2041,Flour and Other Grain Mill Products
2043,Cereal Breakfast Foods
2044,Rice Milling
2045,Prepared Flour Mixes and Doughs
2046,Wet Corn Milling
2047,Dog and Cat Food
2048,\"Prepared Feeds and Feed Ingredients for Animals and Fowls, Except Dogs and Cats\"
2050,Bakery Products
2051,\"Bread and Other Bakery Products, Except Cookies and Crackers\"
2052,Cookies and Crackers
2053,\"Frozen Bakery Products, Except Bread\"
2060,Sugar and Confectionery Products
2061,\"Cane Sugar, Except Refining\"
2062,Cane Sugar Refining
2063,Beet Sugar
2064,Candy and Other Confectionery Products
2066,Chocolate and Cocoa Products
2067,Chewing Gum
2068,Salted and Roasted Nuts and Seeds
2070,Fats and Oils
2074,Cottonseed Oil Mills
2075,Soybean Oil Mills
2076,\"Vegetable Oil Mills, Except Corn, Cottonseed, and Soybean\"
2077,Animal and Marine Fats and Oils
2079,\"Shortening, Table Oils, Margarine, and Other Edible Fats and Oils\"
2080,Beverages
2082,Malt Beverages
2083,Malt
2084,\"Wines, Brandy, and Brandy Spirits\"
2085,Distilled and Blended Liquors
2086,Bottled and Canned Soft Drinks and Carbonated Waters
2087,\"Flavoring Extracts and Flavoring Syrups, Not Elsewhere Classified\"
2090,Miscellaneous Food Preparations and Kindred Products
2091,Canned and Cured Fish and Seafoods
2092,Prepared Fresh or Frozen Fish and Seafoods
2095,Roasted Coffee
2096,\"Potato Chips, Corn Chips, and Similar Snacks\"
2097,Manufactured Ice
2098,\"Macaroni, Spaghetti, Vermicelli, and Noodles\"
2099,\"Food Preparations, Not Elsewhere Classified\"
2100,Tobacco Products
2110,Cigarettes
2111,Cigarettes
2120,Cigars
2121,Cigars
2130,Chewing and Smoking Tobacco and Snuff
2131,Chewing and Smoking Tobacco and Snuff
2140,Tobacco Stemming and Redrying
2141,Tobacco Stemming and Redrying
5140,Groceries and Related Products
5141,\"Groceries, General Line\"
5142,Packaged Frozen Foods
5143,\"Dairy Products, Except Dried or Canned\"
5144,Poultry and Poultry Products
5145,Confectionery
5146,Fish and Seafoods
5147,Meats and Meat Products
5148,Fresh Fruits and Vegetables
5149,\"Groceries and Related Products, Not Elsewhere Classified\"
5150,Farm-Product Raw Materials
5153,Grain and Field Beans
5154,Livestock
5159,\"Farm-Product Raw Materials, Not Elsewhere Classified\"
5180,\"Beer, Wine, and Distilled Alcoholic Beverages\"
5181,Beer and Ale
5182,Wine and Distilled Alcoholic Beverages
",
  colClasses = "character", check.names = FALSE)
save_table(table_A3, "Table_A3_SIC_codes.csv")
register_output("Table A3", "static_manuscript_table", "SIC dictionary from manuscript; original R selection list retained separately, including its omission of 2047")

paper_outputs <- utils::read.csv(text = "ID,title,executor,input,notes
Figure 1,Frequency of All Disasters and Large Disasters,R,disaster_final_filtered.csv; 
Figure 2,Spatial Distribution of Food-Industry Establishments in the Mainland United States,R,food_company_branch.csv (or reduced .csv.gz); county shapefile,
Figure 3,Trends in All Disasters and Large Disasters,R,disaster_final_filtered.csv
Figure 4,Impact of All and Large Disasters on Sales Growth,Stata; upstream construction in R,merged_disaster_food_customer_12.csv
Figure 5,Heatmap for the Frequency of Disasters Affecting Agricultural Sectors - Headquarters,Stata cached-coefficient plot,heterogeneity hazard to sector.csv
Table 1,Impact of Major Damaging Disasters on Food Industry Establishment and Employment Change,Stata; upstream construction in R,food_industry_county_year_input.csv
Table 2,Change in Sale Concentration Moderation Effect,Stata; upstream construction in R,food_customer_network.csv
Table 3,Segment Sale Moderation Effect,Stata; upstream construction in R,food_customer_network.csv; gvkey/county/year link; food_segment.csv; financial inputs; disaster_final_filtered.csv
Table 4,Cluster Scenario Moderation Effect,Stata; upstream construction in R,food_company_branch.csv (or reduced .csv.gz); disaster_final_filtered.csv,
Figure A1,Trends in Sales Concentration,R,supply chain network.csv; company sale sic.csv; connect sic.csv if needed
Figure A2,Trends in Business and International Geographical Diversification,R,food_segment.csv
Figure A3,Comparison of Different Cluster Scenarios,R,food_company_branch.csv (or reduced .csv.gz)
Figure A4,Trends in Branches and Clusters,R,food_company_branch.csv (or reduced .csv.gz)
Figure A5,Robust Heatmap for the Frequency of Disasters Affecting Agricultural Sectors - Headquarters,Stata cached-coefficient plot,heterogeneity hazard to sector.csv
Table A1,Descriptive Statistics,R descriptive statistics; see Table_A1_notes.txt,\"Rebuilt customer/C/S panels; food_industry_disaster; DataAxle; financial_information_food.csv\"
Table A2,\"Frequency and Property Damage of Natural Disasters by Disaster Type, 1976–2023\",R,disaster_final_filtered.csv
Table A3,SIC codes used to define food-sector firms,Static reference exported by R,Table_A3_SIC_codes.
Table A4,Impact of Main Damaging Disasters on Non-food Firm Sale Growth,Unresolved: non-food data and original estimation code,nonfood_transaction_disaster_frequency.csv 
Table A5,Current and Lead Effects of Natural Disasters on Food Firm Sales Growth,Stata; upstream construction in R,merged_disaster_food_customer_12.csv; states_list.csv
Table A6,Robustness Check for Sale Concentration Moderation Effect,User-completed Stata; upstream construction in R,merged_disaster_food_customer_12C.csv
Table A7,Robustness Check for Segment Sale Moderation Effect,User-completed Stata; upstream construction in R,merged_disaster_food_customer_12S.csv
Table A8,Robustness Check for Cluster Scenario Moderation Effect,User-completed Stata; upstream construction in R,merged_disaster_food_customer_12dataaxle.csv,\"
",
  colClasses = "character", check.names = FALSE)
data.table::fwrite(paper_outputs, file.path(check_dir, "paper_output_manifest.csv"), na = "NA")
data.table::fwrite(run_status, file.path(check_dir, "run_status.csv"), na = "NA")
data.table::fwrite(used_inputs, file.path(check_dir, "used_inputs.csv"), na = "NA")
writeLines(capture.output(sessionInfo()), file.path(check_dir, "R_session_info.txt"))
message("", output_dir)
