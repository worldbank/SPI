

# SPI revision analysis: global + pillars + regional aggregates
# Author: [Your Name]
# Description: Compare SPI_df_v1 (original) vs SPI_df_new (revised) on
#   (i) global average SPI.INDEX by year,
#   (ii) global average of pillars (SPI.INDEX.PIL1..PIL5) by year,
#   (iii) regional aggregates by year for SPI.INDEX and each pillar,
# and visualize each comparison with simple time-series charts.
# Notes:
# * All averages are SIMPLE (unweighted) means across countries.
# * For regional pillar trends, one chart per region–pillar is produced to avoid crowding.
# * Nothing is saved to disk; plots are printed to the device.

# --- Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(collapse)   # fast grouping/summarising
  library(ggplot2)    # plotting
  library(dplyr)      # light use for joins/select (kept minimal)
  library(tidyr)      # pivot_longer for tidy reshaping
  library(stringr)    # small string helpers
  library(tidyverse)
})

SPI_df_v1  <- read_csv('https://raw.githubusercontent.com/worldbank/SPI/master/03_output_data/SPI_index.csv')
SPI_df_new <- read_csv('https://raw.githubusercontent.com/worldbank/SPI/SPI_2024/03_output_data/SPI_index.csv')


# --- Inputs -------------------------------------------------------------------
# Expect two data frames in memory with identical schema:
#   SPI_df_v1  : original
#   SPI_df_new : revised

# Defensive harmonization of minimal columns used below
required_cols <- c("iso3c", "date", "region", "SPI.INDEX",
                   paste0("SPI.INDEX.PIL", 1:5))

stopifnot(all(required_cols %in% names(SPI_df_v1)),
          all(required_cols %in% names(SPI_df_new)))

# Ensure types are consistent
SPI_df_v1  <- fmutate(SPI_df_v1,  iso3c = as.character(iso3c),
                      region = as.character(region),
                      date = as.integer(date))
SPI_df_new <- fmutate(SPI_df_new, iso3c = as.character(iso3c),
                      region = as.character(region),
                      date = as.integer(date))

pillar_vars <- paste0("SPI.INDEX.PIL", 1:5)

# Helper to tag dataset version for later stacking/plotting
.tag_version <- function(x, tag) fmutate(x, .version = tag)

# --- (I) GLOBAL AVERAGE: SPI.INDEX by year -----------------------------------
agg_global_index <- function(df, label) {
  # simple mean across all countries by year
  df |>
    fselect(date, SPI.INDEX) |>
    fgroup_by(date) |>
    fsummarise(mean_spi_index = fmean(SPI.INDEX, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

global_v1  <- agg_global_index(SPI_df_v1,  "v1 (original)")
global_new <- agg_global_index(SPI_df_new, "v2 (revised)")

# Comparison data frame (wide)
global_cmp_df <-
  full_join(global_v1, global_new,
            by = "date",
            suffix = c("_v1", "_v2")) |>
  select(date,
         mean_spi_index_v1 = mean_spi_index_v1,
         mean_spi_index_v2 = mean_spi_index_v2)

# Long for plotting
global_plot_df <-
  bind_rows(global_v1, global_new) |>
  fselect(date, dataset, mean_spi_index)

# --- Plot: Global SPI.INDEX ---------------------------------------------------
ggplot(global_plot_df, aes(x = date, y = mean_spi_index, color = dataset)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(title = "Global Average of SPI.INDEX (Simple mean across countries)",
       x = NULL, y = "Average SPI.INDEX") +
  scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                "v2 (revised)"  = "#bf3f34")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# --- (II) GLOBAL AVERAGES: Pillars by year -----------------------------------
agg_global_pillars <- function(df, label) {
  # reshape pillars long, then mean across countries by year & pillar
  df |>
    select(date, all_of(pillar_vars)) |>
    as_tibble() |>
    pivot_longer(cols = all_of(pillar_vars),
                 names_to = "pillar", values_to = "value") |>
    fgroup_by(date, pillar) |>
    fsummarise(mean_pillar = fmean(value, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

globpill_v1  <- agg_global_pillars(SPI_df_v1,  "v1 (original)")
globpill_new <- agg_global_pillars(SPI_df_new, "v2 (revised)")

# Comparison data frame (wide)
globpill_cmp_df <-
  full_join(globpill_v1, globpill_new,
            by = c("date", "pillar"),
            suffix = c("_v1", "_v2")) |>
  select(date, pillar,
         mean_pillar_v1 = mean_pillar_v1,
         mean_pillar_v2 = mean_pillar_v2)

# Long for plotting
globpill_plot_df <- bind_rows(globpill_v1, globpill_new)

# --- Plot: Global pillar averages (faceted by pillar) -------------------------
ggplot(globpill_plot_df,
       aes(x = date, y = mean_pillar, color = dataset)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ pillar, ncol = 2, scales = "free_y") +
  labs(title = "Global Average of SPI Pillars (Simple mean across countries)",
       x = NULL, y = "Average pillar score") +
  scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                "v2 (revised)"  = "#bf3f34")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# --- (III) REGIONAL AGGREGATES -----------------------------------------------
# (A) Regional SPI.INDEX by year (one chart per region)
agg_regional_index <- function(df, label) {
  df |>
    fselect(region, date, SPI.INDEX) |>
    fgroup_by(region, date) |>
    fsummarise(mean_spi_index = fmean(SPI.INDEX, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

regidx_v1  <- agg_regional_index(SPI_df_v1,  "v1 (original)")
regidx_new <- agg_regional_index(SPI_df_new, "v2 (revised)")

# Comparison data frame (wide)
regidx_cmp_df <-
  full_join(regidx_v1, regidx_new,
            by = c("region", "date"),
            suffix = c("_v1", "_v2")) |>
  select(region, date,
         mean_spi_index_v1 = mean_spi_index_v1,
         mean_spi_index_v2 = mean_spi_index_v2)

# Regions list
regions <- sort(unique(c(regidx_v1$region, regidx_new$region)))

# Plot function: one chart per region for SPI.INDEX
plot_region_index <- function(rgn) {
  d <-
    bind_rows(regidx_v1, regidx_new) |>
    fsubset(region == rgn)
  
  ggplot(d, aes(x = date, y = mean_spi_index, color = dataset)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    labs(title = paste0("Regional Average SPI.INDEX: ", rgn),
         x = NULL, y = "Average SPI.INDEX") +
    scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                  "v2 (revised)"  = "#bf3f34")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# Print all region-level index plots
invisible(lapply(regions, function(r) print(plot_region_index(r))))




# --- Aggregate regional pillar means (simple averages across countries) -------
agg_regional_pillars <- function(df, label) {
  df |>
    fselect(c("region", "date", pillar_vars)) |>
    as_tibble() |>
    pivot_longer(cols = dplyr::all_of(pillar_vars),
                 names_to = "pillar", values_to = "value") |>
    fgroup_by(region, date, pillar) |>
    fsummarise(mean_pillar = fmean(value, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

regpill_v1  <- agg_regional_pillars(SPI_df_v1,  "v1 (original)")
regpill_new <- agg_regional_pillars(SPI_df_new, "v2 (revised)")

# Optional: comparison table (wide) for inspection
regpill_cmp_df <-
  full_join(regpill_v1, regpill_new,
            by = c("region","date","pillar"),
            suffix = c("_v1","_v2")) |>
  transmute(
    region, date, pillar,
    mean_pillar_v1 = mean_pillar_v1,
    mean_pillar_v2 = mean_pillar_v2,
    delta = mean_pillar_v2 - mean_pillar_v1
  )

# --- Plot helper: one plot per region, pillars faceted ------------------------
plot_region_pillars_faceted <- function(rgn, free_y = FALSE, ncol = 2) {
  d <- bind_rows(regpill_v1, regpill_new) |> fsubset(region == rgn)
  ggplot(d, aes(x = date, y = mean_pillar, color = dataset)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    facet_wrap(~ pillar, ncol = ncol, scales = if (free_y) "free_y" else "fixed") +
    labs(
      title = paste0("Regional Average SPI Pillars: ", rgn),
      x = NULL, y = "Average pillar score"
    ) +
    scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                  "v2 (revised)"  = "#bf3f34")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# --- Render all regions (faceted by pillar) -----------------------------------
regions <- sort(unique(c(regpill_v1$region, regpill_new$region)))

# Set free_y = TRUE if pillar scales differ a lot within regions.
invisible(lapply(regions, function(r) print(plot_region_pillars_faceted(r, free_y = FALSE, ncol = 2))))






















# --- Inputs assumed in memory --------------------------------------------------
# SPI_df_v1  : original
# SPI_df_new : revised

# --- Specs --------------------------------------------------------------------
dim4_vars <- c("SPI.DIM4.1.CEN.INDEX", "SPI.DIM4.1.SVY.INDEX",
               "SPI.DIM4.2.INDEX",     "SPI.DIM4.3.INDEX")

ind4_vars <- c("SPI.D4.1.1.POPU", "SPI.D4.1.2.AGRI", "SPI.D4.1.3.BIZZ",
               "SPI.D4.1.4.HOUS", "SPI.D4.1.5.AGSVY", "SPI.D4.1.6.LABR",
               "SPI.D4.1.7.HLTH", "SPI.D4.1.8.BZSVY", "SPI.D4.2.3.CRVS",
               "SPI.D4.3.GEO.first.admin.level")

# --- Light validation and harmonization ---------------------------------------
.required_cols <- function(extra) unique(c("date","region", extra))
.check_cols <- function(df, vars) {
  miss <- setdiff(vars, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

SPI_df_v1  <- fmutate(SPI_df_v1,  date = as.integer(date), region = as.character(region))
SPI_df_new <- fmutate(SPI_df_new, date = as.integer(date), region = as.character(region))

.check_cols(SPI_df_v1,  .required_cols(c(dim4_vars, ind4_vars)))
.check_cols(SPI_df_new, .required_cols(c(dim4_vars, ind4_vars)))

# --- Generic helpers -----------------------------------------------------------
# Aggregate globals (simple mean across countries) for an arbitrary vector of series
agg_global_multi <- function(df, label, series_vars) {
  df |>
    fselect(c("date", series_vars)) |>
    as_tibble() |>
    pivot_longer(cols = all_of(series_vars),
                 names_to = "series", values_to = "value") |>
    fgroup_by(date, series) |>
    fsummarise(mean_value = fmean(value, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

# Aggregate regional (simple mean across countries) for an arbitrary vector of series
agg_regional_multi <- function(df, label, series_vars) {
  df |>
    fselect(c("region", "date", series_vars)) |>
    as_tibble() |>
    pivot_longer(cols = all_of(series_vars),
                 names_to = "series", values_to = "value") |>
    fgroup_by(region, date, series) |>
    fsummarise(mean_value = fmean(value, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

# Plot: global, one chart per series
plot_global_series <- function(dat, series_name, ylab = "Global average (simple mean)") {
  d <- fsubset(dat, series == series_name)
  ggplot(d, aes(x = date, y = mean_value, color = dataset)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    labs(title = paste0("Global: ", series_name),
         x = NULL, y = ylab) +
    scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                  "v2 (revised)"  = "#bf3f34")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# Plot: regional, one chart per region–series
plot_regional_series <- function(dat, region_name, series_name,
                                 ylab = "Regional average (simple mean)") {
  d <- fsubset(dat, region == region_name & series == series_name)
  ggplot(d, aes(x = date, y = mean_value, color = dataset)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    labs(title = paste0(region_name, " — ", series_name),
         x = NULL, y = ylab) +
    scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                  "v2 (revised)"  = "#bf3f34")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}



# --- A1. GLOBALS --------------------------------------------------------------
dim4_glob_v1  <- agg_global_multi(SPI_df_v1,  "v1 (original)", dim4_vars)
dim4_glob_v2  <- agg_global_multi(SPI_df_new, "v2 (revised)",  dim4_vars)

# Comparison table (wide)
dim4_global_cmp_df <-
  full_join(dim4_glob_v1, dim4_glob_v2,
            by = c("date","series"),
            suffix = c("_v1","_v2")) |>
  transmute(
    date, series,
    mean_value_v1 = mean_value_v1,
    mean_value_v2 = mean_value_v2,
    delta = mean_value_v2 - mean_value_v1
  )

# Plot: one chart per dimension series (global)
dim4_series <- unique(dim4_global_cmp_df$series)
invisible(lapply(dim4_series, function(s) {
  print(plot_global_series(bind_rows(dim4_glob_v1, dim4_glob_v2), s,
                           ylab = "Global average (dimension index)"))
}))













# -------------------------------------------------------------------
# Faceted regional chart: SPI.INDEX over time, facets = regions
# -------------------------------------------------------------------


# --- Aggregate regional SPI.INDEX means (simple averages across countries) ----
agg_regional_index <- function(df, label) {
  df |>
    fselect(region, date, SPI.INDEX) |>
    fgroup_by(region, date) |>
    fsummarise(mean_spi_index = fmean(SPI.INDEX, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

regidx_v1  <- agg_regional_index(SPI_df_v1,  "v1 (original)")
regidx_new <- agg_regional_index(SPI_df_new, "v2 (revised)")

# Wide comparison (kept for inspection)
regidx_cmp_df <-
  full_join(regidx_v1, regidx_new,
            by = c("region", "date"),
            suffix = c("_v1", "_v2")) |>
  transmute(
    region, date,
    mean_spi_index_v1 = mean_spi_index_v1,
    mean_spi_index_v2 = mean_spi_index_v2,
    delta = mean_spi_index_v2 - mean_spi_index_v1
  )

# Long for plotting (stack the two datasets)
regidx_plot_df <- bind_rows(regidx_v1, regidx_new) |>
  # optional: order facets nicely (alphabetical)
  fmutate(region = factor(region, levels = sort(unique(region))))

# --- Faceted plot: SPI.INDEX, facets = regions --------------------------------
# Set ncol to control the facet grid width; set free_y = TRUE if scales differ a lot.
plot_spi_index_faceted_by_region <- function(ncol = 3, free_y = FALSE) {
  ggplot(regidx_plot_df, aes(x = date, y = mean_spi_index, color = dataset)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    facet_wrap(~ region, ncol = ncol, scales = if (free_y) "free_y" else "fixed") +
    labs(
      title = "Regional Average SPI.INDEX (simple mean across countries)",
      x = NULL, y = "Average SPI.INDEX"
    ) +
    scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                  "v2 (revised)"  = "#bf3f34")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# --- Print the faceted plot ----------------------------------------------------
print(plot_spi_index_faceted_by_region(ncol = 3, free_y = FALSE))















# (B) Regional Pillars by year (one chart per region–pillar)
agg_regional_pillars <- function(df, label) {
  df |>
    fselect(region, date, all_of(pillar_vars)) |>
    as_tibble() |>
    pivot_longer(cols = all_of(pillar_vars),
                 names_to = "pillar", values_to = "value") |>
    fgroup_by(region, date, pillar) |>
    fsummarise(mean_pillar = fmean(value, na.rm = TRUE)) |>
    fungroup() |>
    fmutate(dataset = label)
}

regpill_v1  <- agg_regional_pillars(SPI_df_v1,  "v1 (original)")
regpill_new <- agg_regional_pillars(SPI_df_new, "v2 (revised)")

# Comparison data frame (wide)
regpill_cmp_df <-
  full_join(regpill_v1, regpill_new,
            by = c("region", "date", "pillar"),
            suffix = c("_v1", "_v2")) |>
  select(region, date, pillar,
         mean_pillar_v1 = mean_pillar_v1,
         mean_pillar_v2 = mean_pillar_v2)

# Unique region–pillar combos
reg_pillars <- regpill_cmp_df |>
  distinct(region, pillar) |>
  arrange(region, pillar) |>
  as_tibble()

# Plot function: one chart per region–pillar
plot_region_pillar <- function(rgn, pil) {
  d <-
    bind_rows(regpill_v1, regpill_new) |>
    fsubset(region == rgn & pillar == pil)
  
  ggplot(d, aes(x = date, y = mean_pillar, color = dataset)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    labs(title = paste0("Regional Average ", pil, ": ", rgn),
         x = NULL, y = "Average pillar score") +
    scale_color_manual(values = c("v1 (original)" = "#1b6ca8",
                                  "v2 (revised)"  = "#bf3f34")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# Print all region–pillar plots (this will produce a sequence of charts)
invisible(
  apply(reg_pillars, 1, function(row) {
    print(plot_region_pillar(rgn = row[["region"]], pil = row[["pillar"]]))
  })
)

# --- (IV) Outputs of comparison tables (in-memory data frames) ----------------
# You can inspect these in the console / environment:
#   global_cmp_df   : date, mean_spi_index_v1, mean_spi_index_v2
#   globpill_cmp_df : date, pillar, mean_pillar_v1, mean_pillar_v2
#   regidx_cmp_df   : region, date, mean_spi_index_v1, mean_spi_index_v2
#   regpill_cmp_df  : region, date, pillar, mean_pillar_v1, mean_pillar_v2

# --- (V) Optional: quick deltas (revised - original) --------------------------
# Global overall index deltas by year
global_deltas <- global_cmp_df |>
  fmutate(delta = mean_spi_index_v2 - mean_spi_index_v1)

# Global pillar deltas by year
globpill_deltas <- globpill_cmp_df |>
  fmutate(delta = mean_pillar_v2 - mean_pillar_v1)

# Regional index deltas by year
regidx_deltas <- regidx_cmp_df |>
  fmutate(delta = mean_spi_index_v2 - mean_spi_index_v1)

# Regional pillar deltas by year
regpill_deltas <- regpill_cmp_df |>
  fmutate(delta = mean_pillar_v2 - mean_pillar_v1)
