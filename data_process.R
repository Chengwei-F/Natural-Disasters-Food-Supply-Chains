root <- ""
input_dir <- figure_dir <- table_dir <- NULL

load_packages <- function() {
  packages <- c("data.table", "dplyr", "ggplot2", "sf", "patchwork")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  for (package in packages) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }
  options(dplyr.summarise.inform = FALSE)
  sf::sf_use_s2(TRUE)
  invisible(packages)
}

configure_project <- function(project_root, data_dir = file.path(project_root, "inputs")) {
  if (length(project_root) != 1L || is.na(project_root) || !nzchar(project_root)) {
    stop("Set root to your project folder, then call run_all(root).", call. = FALSE)
  }
  if (!dir.exists(project_root)) stop("Project folder does not exist.", call. = FALSE)
  root <<- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  input_dir <<- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  figure_dir <<- file.path(root, "replication_output", "figures")
  table_dir <<- file.path(root, "replication_output", "tables")
  for (directory in c(figure_dir, table_dir)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(directory)) stop("Cannot create an output folder.", call. = FALSE)
  }
  invisible(root)
}

missing_input <- function(name) {
  stop("Missing input: ", name, call. = FALSE)
}

need_columns <- function(x, columns) {
  if (!is.data.frame(x)) stop("Expected a data frame.", call. = FALSE)
  if (anyNA(names(x)) || any(names(x) == "") || anyDuplicated(names(x))) {
    stop("Input column names must be nonempty and unique.", call. = FALSE)
  }
  absent <- setdiff(columns, names(x))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "), call. = FALSE)
  invisible(x)
}

num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  text <- trimws(as.character(x))
  text[text %in% c("", "NA", "NaN")] <- NA_character_
  value <- suppressWarnings(as.numeric(text))
  if (any(!is.na(text) & is.na(value))) {
    stop("A nonmissing input value is not numeric.", call. = FALSE)
  }
  value
}

assert_unique <- function(x, keys) {
  need_columns(x, keys)
  if (!length(keys)) stop("At least one key is required.", call. = FALSE)
  values <- as.data.frame(x)[keys]
  if (anyNA(values)) stop("Input keys contain missing values.", call. = FALSE)
  if (anyDuplicated(values)) stop("Input keys are not unique.", call. = FALSE)
  invisible(x)
}

assert_nonnegative <- function(x, columns, allow_missing = FALSE) {
  need_columns(x, columns)
  for (column in columns) {
    values <- num(x[[column]])
    if (!allow_missing && anyNA(values)) stop("Missing values in: ", column, call. = FALSE)
    present <- values[!is.na(values)]
    if (any(!is.finite(present) | present < 0)) {
      stop("Expected finite, nonnegative values in: ", column, call. = FALSE)
    }
  }
  invisible(x)
}

input_csv <- function(name, required, numeric_cols = character(),
                      keys = NULL, nonempty = TRUE) {
  if (is.null(input_dir)) stop("Configure the project first.", call. = FALSE)
  if (basename(name) != name) stop("Use an input filename without a directory.", call. = FALSE)
  path <- file.path(input_dir, name)
  if (!file.exists(path)) missing_input(name)
  x <- data.table::fread(
    path, colClasses = "character", na.strings = c("", "NA", "NaN"),
    strip.white = FALSE, check.names = FALSE, data.table = FALSE
  )
  need_columns(x, unique(c(required, numeric_cols)))
  if (nonempty && !nrow(x)) stop("Empty input: ", name, call. = FALSE)
  x[numeric_cols] <- lapply(x[numeric_cols], num)
  if (!is.null(keys)) assert_unique(x, keys)
  x
}

validate_year <- function(x, column) {
  need_columns(x, column)
  year <- num(x[[column]])
  if (any(!is.finite(year) | year != floor(year))) {
    stop("Year values must be finite integers.", call. = FALSE)
  }
  invisible(x)
}

validate_coordinates <- function(x) {
  need_columns(x, c("longitude", "latitude"))
  if (any(!is.finite(x$longitude) | !is.finite(x$latitude)) ||
      any(abs(x$longitude) > 180 | abs(x$latitude) > 90)) {
    stop("Invalid geographic coordinates.", call. = FALSE)
  }
  invisible(x)
}

output_file <- function(directory, name) {
  if (is.null(directory)) stop("Configure the project first.", call. = FALSE)
  if (length(name) != 1L || is.na(name) || basename(name) != name ||
      grepl("[/\\\\]", name)) stop("Invalid output filename.", call. = FALSE)
  file.path(directory, name)
}

save_table <- function(x, name) {
  data.table::fwrite(as.data.frame(x), output_file(table_dir, name),
                    na = "NA", quote = "auto")
  invisible(x)
}

save_text <- function(lines, name) {
  writeLines(lines, output_file(table_dir, name), useBytes = TRUE)
  invisible(name)
}

save_plot <- function(plot, name, width = 9, height = 5.5) {
  ggplot2::ggsave(output_file(figure_dir, paste0(name, ".pdf")), plot = plot,
                  width = width, height = height, units = "in", device = "pdf")
  ggplot2::ggsave(output_file(figure_dir, paste0(name, ".png")), plot = plot,
                  width = width, height = height, units = "in", dpi = 300)
  invisible(plot)
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

load_county_boundaries <- function() {
  stem <- "cb_2018_us_county_500k"
  shapefile <- file.path(root, "map_data", paste0(stem, ".shp"))
  files <- file.path(root, "map_data", paste0(stem, c(".shp", ".shx", ".dbf", ".prj")))
  if (!all(file.exists(files))) missing_input(paste0("map_data/", stem, ".shp and companions"))
  counties <- sf::st_read(shapefile, quiet = TRUE)
  need_columns(counties, c("STATEFP", "COUNTYFP"))
  if (is.na(sf::st_crs(counties))) stop("County boundaries have no CRS.", call. = FALSE)
  counties$STATEFP <- sprintf("%02d", as.integer(num(counties$STATEFP)))
  counties$COUNTYFP <- sprintf("%03d", as.integer(num(counties$COUNTYFP)))
  counties$County_FIPS <- paste0(counties$STATEFP, counties$COUNTYFP)
  if (any(!grepl("^[0-9]{5}$", counties$County_FIPS))) {
    stop("County boundaries contain invalid geographic identifiers.", call. = FALSE)
  }
  counties <- counties %>% filter(STATEFP <= "56", !STATEFP %in% c("02", "15"))
  assert_unique(sf::st_drop_geometry(counties), "County_FIPS")
  sf::st_transform(counties, 4326)
}

read_county_counts <- function(name) {
  x <- input_csv(name, c("County_FIPS", "records"), "records", "County_FIPS")
  if (any(!grepl("^[0-9]{5}$", x$County_FIPS))) {
    stop("County identifiers must be five-digit strings.", call. = FALSE)
  }
  assert_nonnegative(x, "records")
  if (any(x$records != floor(x$records))) stop("Disaster counts must be integers.", call. = FALSE)
  x %>% select(County_FIPS, records)
}

read_annual_series <- function(name, year_column, value_columns,
                               allow_missing = FALSE, nonnegative_columns = value_columns) {
  fields <- c(year_column, value_columns)
  x <- input_csv(name, fields, fields, year_column)
  validate_year(x, year_column)
  for (column in value_columns) {
    values <- x[[column]]
    if ((!allow_missing && anyNA(values)) || any(!is.finite(values[!is.na(values)]))) {
      stop("Invalid annual values in: ", column, call. = FALSE)
    }
  }
  assert_nonnegative(x, nonnegative_columns, allow_missing = allow_missing)
  x %>% select(all_of(fields)) %>% arrange(.data[[year_column]])
}

plot_disaster_counties <- function(counties, counts, caption) {
  map_breaks <- c(0, 50, 150, 250, 350, 400)
  map_bins <- c(0, unlist(lapply(seq_len(length(map_breaks) - 1L), function(i) {
    seq(map_breaks[i], map_breaks[i + 1L], length.out = 21)[-1]
  })))
  palette <- grDevices::colorRampPalette(
    c("#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929",
      "#EC7014", "#CC4C02", "#993404", "#662506"), space = "rgb"
  )(101)
  palette <- palette[round(seq(1, 101, length.out = 100))]
  map_data <- counties %>% left_join(counts, by = "County_FIPS")
  if (!any(!is.na(map_data$records))) stop("No county counts match the map.", call. = FALSE)
  map_data$color_bin <- findInterval(map_data$records, map_bins, all.inside = TRUE)
  ggplot(map_data) +
    geom_sf(aes(fill = color_bin), colour = "grey40", linewidth = 0.12) +
    scale_fill_gradientn(
      colours = palette, values = seq(0, 1, length.out = 100),
      limits = c(1, 100), breaks = c(1, 20, 40, 60, 80, 100),
      labels = map_breaks, na.value = "grey75", name = NULL,
      guide = guide_colourbar(direction = "vertical", reverse = TRUE)
    ) +
    coord_sf(datum = NA) +
    labs(caption = caption) +
    theme_void(base_size = 11) +
    theme(
      legend.position = "inside", legend.position.inside = c(0.02, 0.02),
      legend.justification = c(0, 0),
      legend.key.width = grid::unit(2.5, "mm"),
      legend.key.height = grid::unit(22, "mm"),
      legend.text = element_text(size = 7),
      legend.background = element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.1), colour = NA
      ),
      legend.margin = margin(0, 0, 0, 0),
      plot.caption = element_text(hjust = 0.5, size = 11, margin = margin(t = 6)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

run_figure_1 <- function() {
  counties <- load_county_boundaries()
  counts_all <- read_county_counts("Figure_1a_county_counts.csv")
  counts_large <- read_county_counts("Figure_1b_large_caption_diagnostic_counts.csv")
  fig1a <- plot_disaster_counties(counties, counts_all, "(a) All Disasters")
  fig1b <- plot_disaster_counties(counties, counts_large, "(b) Large Disasters")
  save_plot(fig1a, "Figure_1a", 9, 5.5)
  save_plot(fig1b, "Figure_1b_large_caption_diagnostic", 9, 5.5)
  save_plot(patchwork::wrap_plots(fig1a, fig1b, ncol = 2), "Figure_1", 14, 5)
  save_table(counts_all, "Figure_1a_county_counts.csv")
  save_table(counts_large, "Figure_1b_large_caption_diagnostic_counts.csv")
  invisible(list(all = fig1a, large = fig1b))
}

run_figure_2 <- function() {
  counties <- load_county_boundaries()
  locations <- input_csv(
    "Figure_2_plot_data.csv", c("longitude", "latitude"),
    c("longitude", "latitude")
  )
  validate_coordinates(locations)
  points <- locations %>% select(longitude, latitude)
  points <- sf::st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)
  map_bounds <- sf::st_bbox(counties)
  plot <- ggplot() +
    geom_sf(data = counties, fill = "grey95", colour = "white", linewidth = 0.1) +
    geom_sf(data = points, colour = "red", size = 0.25,
            alpha = 0.5, show.legend = FALSE) +
    coord_sf(
      xlim = unname(map_bounds[c("xmin", "xmax")]),
      ylim = unname(map_bounds[c("ymin", "ymax")]),
      datum = NA, expand = FALSE
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA))
  save_plot(plot, "Figure_2", 12, 6)
  invisible(plot)
}

run_figure_3 <- function() {
  frequency <- read_annual_series(
    "Figure_3_annual_frequency.csv", "Year", "total_events_all"
  )
  damage <- input_csv(
    "Figure_3_annual_damage_top6.csv", c("Year", "Hazard", "total_damage"),
    c("Year", "total_damage"), c("Year", "Hazard")
  )
  validate_year(damage, "Year")
  assert_nonnegative(damage, "total_damage", allow_missing = TRUE)
  if (any(!damage$Year %in% frequency$Year)) {
    stop("Damage series contains years absent from the frequency input.", call. = FALSE)
  }
  plot_data <- damage %>%
    select(Year, Hazard, total_damage) %>%
    left_join(frequency, by = "Year") %>%
    arrange(Hazard, Year)
  positive_damage <- plot_data$total_damage[is.finite(plot_data$total_damage)]
  if (!length(positive_damage) || max(positive_damage) <= 0 ||
      max(frequency$total_events_all) <= 0) {
    stop("Positive frequency and damage are required for the dual axis.", call. = FALSE)
  }
  scale_factor <- max(frequency$total_events_all) / max(positive_damage)
  plot <- ggplot() +
    geom_line(data = frequency, aes(x = Year, y = total_events_all),
              linewidth = 1.1, colour = "black") +
    geom_point(data = frequency, aes(x = Year, y = total_events_all), size = 2) +
    geom_line(data = plot_data,
              aes(x = Year, y = total_damage * scale_factor, colour = Hazard),
              linewidth = 1, na.rm = TRUE) +
    geom_point(data = plot_data,
               aes(x = Year, y = total_damage * scale_factor, colour = Hazard),
               size = 1.6, na.rm = TRUE) +
    scale_y_continuous(
      name = "Total number of disaster records",
      sec.axis = sec_axis(~ . / scale_factor,
                          name = "Property damage (2023 USD; selected hazards)")
    ) +
    scale_x_continuous(breaks = pretty(frequency$Year)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom") +
    labs(x = "Year", colour = "Hazard")
  save_plot(plot, "Figure_3", 11, 6.5)
  save_table(frequency, "Figure_3_annual_frequency.csv")
  save_table(damage %>% select(Year, Hazard, total_damage), "Figure_3_annual_damage_top6.csv")
  invisible(plot)
}

run_figure_a1 <- function() {
  annual <- read_annual_series(
    "Figure_A1_customer_concentration_data.csv", "year_srcdate",
    c("mean_mainsale_ratio", "sd_mainsale_ratio"),
    allow_missing = TRUE, nonnegative_columns = "sd_mainsale_ratio"
  )
  if (!any(is.finite(annual$mean_mainsale_ratio))) stop("No finite concentration means.", call. = FALSE)
  annual <- annual %>% mutate(
    lower_bound = mean_mainsale_ratio - sd_mainsale_ratio,
    upper_bound = mean_mainsale_ratio + sd_mainsale_ratio
  )
  plot <- ggplot(annual, aes(x = year_srcdate)) +
    geom_line(aes(y = mean_mainsale_ratio), colour = "black", linewidth = 0.75) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                fill = "blue", alpha = 0.1) +
    labs(x = "Year", y = "Average Concentration Sale Ratio") +
    theme_minimal()
  save_table(annual, "Figure_A1_customer_concentration_data.csv")
  save_plot(plot, "Figure_A1_customer_concentration", 8, 5)
  invisible(plot)
}

segment_trend_plot <- function(annual, y_label, caption) {
  ggplot(annual, aes(x = year_srcdate)) +
    geom_line(aes(y = mean), colour = "black", linewidth = 0.75) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                fill = "blue", alpha = 0.1) +
    labs(x = "Year", y = y_label, caption = caption) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0.5, vjust = -1))
}

run_figure_a2 <- function() {
  business <- read_annual_series(
    "Figure_A2_BUSSEG_data.csv", "year_srcdate", c("mean", "sd"),
    allow_missing = TRUE, nonnegative_columns = "sd"
  )
  geographical <- read_annual_series(
    "Figure_A2_GEOSEG_data.csv", "year_srcdate", c("mean", "sd"),
    allow_missing = TRUE, nonnegative_columns = "sd"
  )
  if (!any(is.finite(business$mean)) || !any(is.finite(geographical$mean))) {
    stop("Each diversification series needs a finite mean.", call. = FALSE)
  }
  joined <- business %>%
    rename(mean_business = mean, sd_business = sd) %>%
    full_join(
      geographical %>% rename(mean_geographical = mean, sd_geographical = sd),
      by = "year_srcdate"
    ) %>% arrange(year_srcdate)
  business_plot <- joined %>%
    filter(year_srcdate %in% business$year_srcdate) %>%
    transmute(year_srcdate, mean = mean_business, sd = sd_business,
               lower_bound = mean - sd, upper_bound = mean + sd)
  geographical_plot <- joined %>%
    filter(year_srcdate %in% geographical$year_srcdate) %>%
    transmute(year_srcdate, mean = mean_geographical, sd = sd_geographical,
               lower_bound = mean - sd, upper_bound = mean + sd)
  first <- segment_trend_plot(
    business_plot, "Average Business Diversification", "(a) Trends in Business Diversification"
  )
  second <- segment_trend_plot(
    geographical_plot, "Average Geographic Diversification",
    "(b) Trends in Geographic Diversification"
  )
  combined <- patchwork::wrap_plots(first, second, ncol = 2)
  save_table(business_plot, "Figure_A2_BUSSEG_data.csv")
  save_table(geographical_plot, "Figure_A2_GEOSEG_data.csv")
  save_plot(combined, "Figure_A2_segment_diversification", 12, 5)
  invisible(combined)
}

read_cluster_examples <- function() {
  points <- input_csv(
    "Figure_A3_points.csv",
    c("panel", "longitude", "latitude", "cluster_id", "is_clustered"),
    c("longitude", "latitude")
  )
  panels <- input_csv(
    "Figure_A3_panels.csv", c("panel", "caption", "display_order"),
    "display_order", "panel"
  )
  if (nrow(panels) != 2L) stop("Figure A3 requires two panels.", call. = FALSE)
  assert_unique(panels, "display_order")
  if (any(!is.finite(panels$display_order))) stop("Panel order must be finite.", call. = FALSE)
  if (anyNA(points$panel) || any(!points$panel %in% panels$panel) ||
      any(!panels$panel %in% points$panel)) {
    stop("Example points and panel descriptions do not correspond.", call. = FALSE)
  }
  if (anyNA(panels$caption)) stop("Panel captions are missing.", call. = FALSE)
  flags <- tolower(trimws(points$is_clustered))
  if (anyNA(flags) || any(!flags %in% c("true", "false", "1", "0"))) {
    stop("is_clustered must contain explicit TRUE/FALSE values.", call. = FALSE)
  }
  points$is_clustered <- flags %in% c("true", "1")
  if (any(points$is_clustered & (is.na(points$cluster_id) | points$cluster_id == ""))) {
    stop("Clustered points require a cluster identifier.", call. = FALSE)
  }
  validate_coordinates(points)
  list(points = points, panels = panels %>% arrange(display_order))
}

plot_cluster_panel <- function(counties, points, caption) {
  spatial <- sf::st_as_sf(
    points %>% select(longitude, latitude, cluster_id, is_clustered),
    coords = c("longitude", "latitude"), crs = 4326
  )
  clustered <- spatial %>% filter(is_clustered)
  unclustered <- spatial %>% filter(!is_clustered)
  hulls <- if (nrow(clustered)) {
    clustered %>% group_by(cluster_id) %>%
      summarise(geometry = sf::st_convex_hull(sf::st_union(geometry)), .groups = "drop")
  } else clustered
  plot <- ggplot() +
    geom_sf(data = counties, fill = "white", color = "gray75", linewidth = 0.1)
  if (nrow(hulls)) {
    plot <- plot + geom_sf(data = hulls, fill = "lightblue", color = "blue", alpha = 0.4)
  }
  plot +
    geom_sf(data = clustered, color = "black", size = 0.7) +
    geom_sf(data = unclustered, color = "red", size = 0.5) +
    labs(caption = caption) +
    theme_void() +
    theme(plot.caption = element_text(hjust = 0.5))
}

run_figure_a3 <- function() {
  counties <- load_county_boundaries()
  examples <- read_cluster_examples()
  panels <- examples$panels
  plots <- lapply(seq_len(nrow(panels)), function(i) {
    points <- examples$points %>% filter(panel == panels$panel[i])
    plot_cluster_panel(counties, points, panels$caption[i])
  })
  combined <- patchwork::wrap_plots(plots, ncol = 2)
  save_plot(combined, "Figure_A3", 12, 5)
  invisible(combined)
}

run_figure_a4 <- function() {
  annual <- read_annual_series(
    "Figure_A4_plot_data.csv", "archive_version_year",
    c("mean_total_branches", "mean_num_clusters", "mean_branches_in_clusters")
  )
  if (any(annual$mean_branches_in_clusters > annual$mean_total_branches)) {
    stop("Clustered branch means exceed total branch means.", call. = FALSE)
  }
  displayed <- c(annual$mean_total_branches, annual$mean_branches_in_clusters,
                 annual$mean_num_clusters * 17 / 2)
  if (any(displayed > 17)) {
    stop("Values exceed the Figure A4 display range.", call. = FALSE)
  }
  plot <- ggplot(annual, aes(x = archive_version_year)) +
    geom_line(aes(y = mean_total_branches), color = "black") +
    geom_point(aes(y = mean_total_branches, shape = "Total Branches"), size = 2) +
    geom_line(aes(y = mean_branches_in_clusters), color = "gray50") +
    geom_point(aes(y = mean_branches_in_clusters, shape = "Clustered Branches"),
               color = "gray50", size = 2) +
    geom_line(aes(y = mean_num_clusters * 17 / 2), color = "blue", linetype = "dashed") +
    geom_point(aes(y = mean_num_clusters * 17 / 2, shape = "Number of Clusters"),
               color = "blue", size = 2) +
    scale_y_continuous(
      name = "Number of Branches", limits = c(0, 17),
      sec.axis = sec_axis(~ . * 2 / 17, name = "Number of Clusters")
    ) +
    scale_shape_manual(
      values = c("Total Branches" = 15, "Clustered Branches" = 16, "Number of Clusters" = 17),
      guide = guide_legend(override.aes = list(linetype = 0))
    ) +
    labs(x = "Year") +
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "blue"),
      panel.grid.minor = element_blank(), legend.title = element_blank(),
      legend.position = "bottom"
    )
  save_table(annual, "Figure_A4_plot_data.csv")
  save_plot(plot, "Figure_A4", 8, 5)
  invisible(plot)
}

table_page <- function(title, headers, rows) {
  style <- paste(
    'body{font-family:"Times New Roman",serif;margin:28px auto;',
    'max-width:1100px;padding:0 18px;color:#161616}',
    'table{width:100%;border-collapse:collapse}',
    'caption{font-size:18px;padding:10px}',
    'th,td{padding:5px 10px;text-align:right;vertical-align:top}',
    'th:first-child,td:first-child{text-align:left}',
    '.text{white-space:normal;text-align:left}',
    '.panel td{border-top:1px solid black;border-bottom:1px solid black;',
    'text-align:left;font-weight:bold}',
    '.sub td{text-align:left;font-style:italic}',
    'thead{border-top:2px solid black;border-bottom:1px solid black}',
    'tbody{border-bottom:2px solid black}',
    '@media print{body{margin:0;max-width:none}thead{display:table-header-group}',
    'tr{break-inside:avoid}}', sep = ""
  )
  c('<!doctype html><html><head><meta charset="UTF-8">',
    paste0('<title>', escape_html(title), '</title>'),
    paste0('<style>', style, '</style></head><body>'),
    paste0('<table><caption>', escape_html(title), '</caption><thead><tr>'),
    paste0('<th scope="col">', escape_html(headers), '</th>', collapse = ""),
    '</tr></thead><tbody>', rows, '</tbody></table>',
    '</body></html>')
}

table_numbers <- function(x, digits = 3L) {
  out <- rep("NA", length(x))
  present <- !is.na(x)
  out[present] <- formatC(x[present], format = "f", digits = digits,
                          big.mark = ",")
  out
}

run_table_a1 <- function() {
  statistics <- c("Obs.", "Mean", "Std.Dev", "p1", "p50", "p99")
  columns <- c("Panel", "Variable", statistics)
  x <- input_csv("Table_A1.csv", required = columns,
                 numeric_cols = statistics)
  x <- x[, columns, drop = FALSE]
  labels_ok <- vapply(x[c("Panel", "Variable")], function(z) {
    !anyNA(z) && all(nzchar(trimws(z)))
  }, logical(1))
  if (!all(labels_ok)) stop("Table A1 requires nonempty panel and variable labels.")
  assert_unique(x, c("Panel", "Variable"))
  for (name in statistics) {
    if (any(!is.na(x[[name]]) & !is.finite(x[[name]]))) {
      stop("Table A1 contains a nonfinite statistic in ", name, ".")
    }
  }
  observations <- x[["Obs."]]
  if (any(!is.na(observations) &
          (observations < 0 | observations != floor(observations)))) {
    stop("Table A1 observation counts must be nonnegative integers or missing.")
  }
  if (any(x[["Std.Dev"]] < 0, na.rm = TRUE)) {
    stop("Table A1 standard deviations cannot be negative.")
  }
  ordered_quantiles <- (is.na(x$p1) | is.na(x$p50) | x$p1 <= x$p50) &
    (is.na(x$p50) | is.na(x$p99) | x$p50 <= x$p99) &
    (is.na(x$p1) | is.na(x$p99) | x$p1 <= x$p99)
  if (!all(ordered_quantiles)) stop("Table A1 supplied percentiles are not ordered.")

  rows <- character()
  previous_panel <- NULL
  for (i in seq_len(nrow(x))) {
    if (!identical(x$Panel[i], previous_panel)) {
      rows <- c(rows, paste0('<tr class="panel"><td colspan="7">',
                             escape_html(x$Panel[i]), '</td></tr>'))
      previous_panel <- x$Panel[i]
    }
    if (identical(x$Variable[i], "ROA")) {
      rows <- c(rows, paste0('<tr class="sub"><td colspan="7">',
                             'Food Company Financial Information:</td></tr>'))
    }
    values <- c(table_numbers(observations[i], digits = 0L),
      vapply(x[i, statistics[-1], drop = FALSE], table_numbers, character(1)))
    rows <- c(rows, paste0('<tr><td>', escape_html(x$Variable[i]), '</td>',
      paste0('<td>', escape_html(values), '</td>', collapse = ""), '</tr>'))
  }
  html <- table_page("Table A1: Descriptive Statistics",
    c("Variable", statistics), rows)
  save_table(x, "Table_A1.csv")
  save_text(html, "Table_A1.html")
  invisible(x)
}

run_table_a2 <- function() {
  totals <- c("total_frequency", "total_damage_2023_USD")
  x <- input_csv("Table_A2.csv", required = c("Hazard_group", totals),
                 numeric_cols = totals)
  x <- x[, c("Hazard_group", totals), drop = FALSE]
  if (anyNA(x$Hazard_group) || any(!nzchar(trimws(x$Hazard_group)))) {
    stop("Table A2 requires nonempty hazard-group labels.")
  }
  assert_unique(x, "Hazard_group")
  for (name in totals) {
    if (any(!is.finite(x[[name]]) | x[[name]] < 0)) {
      stop("Table A2 totals must be finite and nonnegative: ", name, ".")
    }
  }
  if (any(x$total_frequency != floor(x$total_frequency))) {
    stop("Table A2 frequencies must be integer counts.")
  }
  is_total <- tolower(trimws(x$Hazard_group)) == "all hazards"
  if (sum(is_total) > 1L) stop("Table A2 contains multiple All hazards rows.")
  if (all(is_total)) stop("Table A2 needs at least one individual hazard group.")
  calculated <- vapply(x[!is_total, totals, drop = FALSE], sum, numeric(1))
  if (any(!is.finite(calculated))) stop("Table A2 summed totals are not finite.")
  supplied_total <- any(is_total)
  if (supplied_total) {
    for (name in totals) {
      stated <- x[[name]][is_total]
      tolerance <- if (name == "total_frequency") 0 else
        .Machine$double.eps * max(8, nrow(x)) * max(1, abs(stated), abs(calculated[[name]]))
      if (abs(stated - calculated[[name]]) > tolerance) {
        stop("Table A2 All hazards does not equal the sum of supplied groups: ",
             name, ".")
      }
    }
  } else {
    x <- rbind(x, data.frame(Hazard_group = "All hazards",
      total_frequency = calculated[["total_frequency"]],
      total_damage_2023_USD = calculated[["total_damage_2023_USD"]],
      stringsAsFactors = FALSE))
    is_total <- c(is_total, TRUE)
  }
  frequency_total <- x$total_frequency[is_total]
  damage_total <- x$total_damage_2023_USD[is_total]
  x$share_frequency <- if (frequency_total > 0) x$total_frequency / frequency_total else NA_real_
  x$share_damage <- if (damage_total > 0) x$total_damage_2023_USD / damage_total else NA_real_
  x$total_damage_billion_2023_USD <- x$total_damage_2023_USD / 1e9
  rows <- vapply(seq_len(nrow(x)), function(i) {
    values <- c(table_numbers(x$total_frequency[i], 0L),
      table_numbers(x$total_damage_billion_2023_USD[i]),
      table_numbers(100 * x$share_frequency[i]),
      table_numbers(100 * x$share_damage[i]))
    paste0('<tr><td>', escape_html(x$Hazard_group[i]), '</td>',
      paste0('<td>', escape_html(values), '</td>', collapse = ""), '</tr>')
  }, character(1))
  html <- table_page("Table A2: Frequency and Property Damage by Disaster Type",
    c("Disaster type", "Frequency", "Damage (billion 2023 USD)",
      "Frequency share (%)", "Damage share (%)"), rows)
  save_table(x, "Table_A2.csv")
  save_text(html, "Table_A2.html")
  invisible(x)
}

run_table_a3 <- function() {
  x <- input_csv("Table_A3_SIC_codes.csv", required = c("sic", "title"))
  x <- x[, c("sic", "title"), drop = FALSE]
  if (!is.character(x$sic)) stop("Table A3 SIC codes must be read as text.")
  if (anyNA(x$sic) || any(!nzchar(trimws(x$sic))) ||
      anyNA(x$title) || any(!nzchar(trimws(x$title)))) {
    stop("Table A3 requires nonempty SIC codes and titles.")
  }
  assert_unique(x, "sic")
  rows <- vapply(seq_len(nrow(x)), function(i) {
    paste0('<tr><td>', escape_html(x$sic[i]), '</td><td class="text">',
      escape_html(x$title[i]), '</td></tr>')
  }, character(1))
  html <- table_page("Table A3: SIC Codes Used to Define Food-Sector Firms",
    c("SIC", "Title"), rows)
  save_table(x, "Table_A3_SIC_codes.csv")
  save_text(html, "Table_A3.html")
  invisible(x)
}

available_tasks <- function() {
  list(
    "Figure 1" = run_figure_1,
    "Figure 2" = run_figure_2,
    "Figure 3" = run_figure_3,
    "Figure A1" = run_figure_a1,
    "Figure A2" = run_figure_a2,
    "Figure A3" = run_figure_a3,
    "Figure A4" = run_figure_a4,
    "Table A1" = run_table_a1,
    "Table A2" = run_table_a2,
    "Table A3" = run_table_a3
  )
}

run_all <- function(project_root = root, items = NULL,
                    data_dir = file.path(project_root, "inputs")) {
  tasks <- available_tasks()
  if (is.null(items)) items <- names(tasks)
  if (!is.character(items) || !length(items) || anyNA(items) ||
      any(!items %in% names(tasks))) {
    stop("Unknown figure or table identifier.", call. = FALSE)
  }
  load_packages()
  configure_project(project_root, data_dir)
  for (id in unique(items)) {
    message(id)
    tasks[[id]]()
  }
  invisible(unique(items))
}

if (nzchar(root)) run_all(root)
