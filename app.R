# =============================================================================
# US Solar PV Fleet: operating plants and development pipeline
# -----------------------------------------------------------------------------
# Data:  U.S. EIA Form EIA-860M, "Preliminary Monthly Electric Generator
#        Inventory" (https://www.eia.gov/electricity/data/eia860m/).
#        The app downloads the latest monthly workbook straight from eia.gov
#        and reads two sheets:
#          - Operating : units in service (OP, SB, OS, OA)
#          - Planned   : pipeline units (P, L, T, U, V, TS) with planned COD
#        Filtered to energy source SUN + prime mover PV (excludes CSP).
#        Array details (tracking type, bifacial, DC capacity, ILR, module
#        material, tilt/azimuth) come from the annual Form EIA-860,
#        Schedule 3_3 Solar, joined on plant + generator ID.
#        No API key needed.
# Map:   OpenStreetMap tiles (dark-filtered for the Linear look, plus the
#        standard OSM style). Light use only, per the OSM tile usage policy:
#        https://operations.osmfoundation.org/policies/tiles/
# Theme: Linear-style dark UI (near-black surfaces, hairline borders,
#        indigo accent, Inter with tabular numerals).
#
# install.packages(c("shiny","bslib","httr2","readxl","dplyr","tidyr",
#                    "leaflet","plotly","DT","htmltools"))
# shiny::runApp()
#
# The parsed snapshots are cached next to this file: eia860m_solar.rds
# (checked for a newer month after 24 h; EIA publishes monthly, ~2 month lag)
# and eia860_solar_arrays.rds (checked for a newer annual release after 30 days).
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# ---- Data layer -------------------------------------------------------------

EIA_XLS    <- "https://www.eia.gov/electricity/data/eia860m/xls/%s_generator%d.xlsx"
CACHE_FILE <- "eia860m_solar.rds"
CACHE_TTL  <- 24 * 3600

STAGES <- c("Operating", "Under construction", "Permitted",
            "Pending approval", "Early planning")
STAGE_COLS <- c(
  "Operating"          = "#5E6AD2",  # Linear indigo
  "Under construction" = "#F2994A",  # orange
  "Permitted"          = "#F2C94C",  # yellow
  "Pending approval"   = "#B59AF8",  # lavender
  "Early planning"     = "#8A8F98"   # grey
)
PIPELINE <- STAGES[-1]

# Walk back from the current month until EIA has a workbook (usually ~2 months).
# With `newer_than`, only months after that date are tried and NULL is
# returned when EIA has nothing newer.
download_latest_860m <- function(max_back = 9, newer_than = NULL) {
  d0 <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  for (k in 0:max_back) {
    d   <- seq(d0, by = "-1 month", length.out = k + 1)[k + 1]
    if (!is.null(newer_than) && d <= newer_than) return(NULL)
    m   <- as.integer(format(d, "%m")); y <- as.integer(format(d, "%Y"))
    url <- sprintf(EIA_XLS, tolower(month.name[m]), y)
    tmp <- tempfile(fileext = ".xlsx")
    ok  <- tryCatch({
      resp <- request(url) |>
        req_user_agent("shiny-solar-fleet-dashboard (R/httr2)") |>
        req_timeout(180) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform(path = tmp)
      resp_status(resp) == 200 &&
        all(c("Operating", "Planned") %in% excel_sheets(tmp))
    }, error = \(e) FALSE)
    if (isTRUE(ok)) {
      message("EIA-860M: using ", url)
      return(list(path = tmp, url = url, period = d))
    }
  }
  stop("Could not find an EIA-860M workbook in the last ", max_back, " months.",
       call. = FALSE)
}

clean_names <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

# The sheets carry a title block above the header and notes below the data,
# so locate the header row instead of trusting a fixed skip.
read_860m_sheet <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  hdr <- which(apply(raw[seq_len(min(15, nrow(raw))), ], 1,
                     \(r) any(trimws(r) %in% c("Plant ID", "Entity ID"))))[1]
  if (is.na(hdr)) stop("Header row not found in sheet ", sheet, call. = FALSE)
  df <- raw[-seq_len(hdr), ]
  names(df) <- make.unique(clean_names(unlist(raw[hdr, ])))
  df[!is.na(suppressWarnings(as.numeric(df$plant_id))), ]
}

pick <- function(d, ...) {
  nm <- intersect(c(...), names(d))
  if (length(nm)) d[[nm[1]]] else rep(NA_character_, nrow(d))
}
num <- function(x) suppressWarnings(as.numeric(x))

standardise <- function(d, sheet_name) {
  # Resolve sheet-specific columns first: inside tibble(), a column named
  # `sheet` would shadow the argument and break the scalar if().
  is_op    <- identical(sheet_name, "Operating")
  year_raw <- if (is_op) pick(d, "operating_year")  else pick(d, "planned_operation_year")
  mon_raw  <- if (is_op) pick(d, "operating_month") else pick(d, "planned_operation_month")
  tibble(
    sheet    = sheet_name,
    plant_id = pick(d, "plant_id"),
    plant    = pick(d, "plant_name"),
    owner    = pick(d, "entity_name"),
    state    = pick(d, "plant_state", "state"),
    county   = pick(d, "county"),
    sector   = pick(d, "sector", "sector_name"),
    ba       = pick(d, "balancing_authority_code"),
    gen_id   = pick(d, "generator_id"),
    esc      = pick(d, "energy_source_code"),
    pm       = pick(d, "prime_mover_code"),
    status   = pick(d, "status"),
    lat      = num(pick(d, "latitude")),
    lon      = num(pick(d, "longitude")),
    mw       = num(pick(d, "nameplate_capacity_mw")),
    year     = as.integer(num(year_raw)),
    month    = as.integer(num(mon_raw))
  )
}

stage_from_status <- function(status, sheet) {
  code <- toupper(sub("^\\s*\\(?([A-Za-z]+)\\)?.*$", "\\1", status))
  out  <- case_when(
    sheet == "Operating"         ~ "Operating",
    code %in% c("U", "V", "TS")  ~ "Under construction",
    code == "T"                  ~ "Permitted",
    code == "L"                  ~ "Pending approval",
    TRUE                         ~ "Early planning"
  )
  factor(out, levels = STAGES)
}

build_solar <- function(src = download_latest_860m()) {
  gens <- bind_rows(
    standardise(read_860m_sheet(src$path, "Operating"), "Operating"),
    standardise(read_860m_sheet(src$path, "Planned"),   "Planned")
  ) |>
    filter(toupper(esc) == "SUN", toupper(pm) == "PV",
           !is.na(mw), mw > 0, !is.na(lat), !is.na(lon)) |>
    mutate(stage = stage_from_status(status, sheet),
           status = sub("^\\s*\\([A-Za-z]+\\)\\s*", "", status))
  list(gens = gens, url = src$url, period = src$period, fetched = Sys.time())
}

# Cache logic:
#  - no cache        -> download the latest workbook
#  - cache < 24 h    -> use it as is
#  - cache >= 24 h   -> only ask EIA for a NEWER month (a couple of quick 404s
#                       when there is none); keep the cache and reset its clock
load_solar <- function() {
  if (!file.exists(CACHE_FILE)) {
    out <- build_solar()
    saveRDS(out, CACHE_FILE)
    return(out)
  }
  cached <- readRDS(CACHE_FILE)
  age <- difftime(Sys.time(), file.mtime(CACHE_FILE), units = "secs")
  if (age < CACHE_TTL) return(cached)

  out <- tryCatch({
    src <- download_latest_860m(newer_than = cached$period)
    if (is.null(src)) NULL else build_solar(src)
  }, error = function(e) {
    warning("EIA check failed, keeping cached data: ", conditionMessage(e))
    NULL
  })
  if (is.null(out)) {
    Sys.setFileTime(CACHE_FILE, Sys.time())
    return(cached)
  }
  saveRDS(out, CACHE_FILE)
  out
}

# ---- Array details: annual Form EIA-860, Schedule 3_3 Solar -------------------
# The monthly 860M has no mounting or module information. The annual EIA-860
# solar sheet does, per generator: single-axis / dual-axis / fixed tilt /
# east-west tracking flags, bifacial, tilt and azimuth, DC capacity and module
# material. It runs about a year behind, so recent plants and the pipeline
# show as "not yet reported". Optional: the app works without it.

EIA860_ZIPS <- c(
  final   = "https://www.eia.gov/electricity/data/eia860/xls/eia860%d.zip",
  early   = "https://www.eia.gov/electricity/data/eia860/xls/eia860%dER.zip",
  archive = "https://www.eia.gov/electricity/data/eia860/archive/xls/eia860%d.zip"
)
ARRAY_CACHE <- "eia860_solar_arrays.rds"
ARRAY_TTL   <- 30 * 24 * 3600  # annual data: look for a newer release monthly

# Newest first: each data year's final release, then its early release.
# With `newer_than` (a previous result), only strictly newer releases are tried.
download_latest_860 <- function(newer_than = NULL, years_back = 4) {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  for (y in (cy - 1):(cy - years_back)) {
    for (kind in c("final", "early")) {
      if (!is.null(newer_than)) {
        newer <- y > newer_than$year ||
          (y == newer_than$year && kind == "final" && newer_than$release == "early")
        if (!newer) return(NULL)
      }
      urls <- if (kind == "final") EIA860_ZIPS[c("final", "archive")] else EIA860_ZIPS["early"]
      for (u in sprintf(urls, y)) {
        tmp <- tempfile(fileext = ".zip")
        ok <- tryCatch({
          resp <- request(u) |>
            req_user_agent("shiny-solar-fleet-dashboard (R/httr2)") |>
            req_timeout(300) |>
            req_error(is_error = \(r) FALSE) |>
            req_perform(path = tmp)
          resp_status(resp) == 200 && any(grepl("3_3_Solar", unzip(tmp, list = TRUE)$Name))
        }, error = \(e) FALSE)
        if (isTRUE(ok)) {
          message("EIA-860 (", kind, " ", y, "): using ", u)
          return(list(path = tmp, url = u, year = y, release = kind))
        }
      }
    }
  }
  stop("Could not find an EIA-860 annual release in the last ", years_back, " years.",
       call. = FALSE)
}

yn <- function(x) {
  x <- toupper(trimws(x))
  ifelse(x %in% c("Y", "YES", "TRUE"), TRUE, ifelse(x %in% c("N", "NO", "FALSE"), FALSE, NA))
}

# First column whose cleaned name matches a regex (layouts shift between years)
pick_rx <- function(d, rx) {
  nm <- grep(rx, names(d), value = TRUE)
  if (length(nm)) d[[nm[1]]] else rep(NA_character_, nrow(d))
}

norm_plant <- function(x) as.character(as.integer(num(x)))
norm_gen   <- function(x) toupper(trimws(x))

build_arrays <- function(src = download_latest_860()) {
  inner <- grep("3_3_Solar.*\\.xlsx?$", unzip(src$path, list = TRUE)$Name, value = TRUE)[1]
  exdir <- tempfile("eia860_"); dir.create(exdir)
  xlsx  <- unzip(src$path, files = inner, exdir = exdir, junkpaths = TRUE)
  sheet <- grep("^operable", excel_sheets(xlsx), ignore.case = TRUE, value = TRUE)[1]
  raw <- read_excel(xlsx, sheet = sheet, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  hdr <- which(apply(raw[seq_len(min(15, nrow(raw))), ], 1,
                     \(r) any(trimws(r) %in% c("Plant Code", "Plant ID"))))[1]
  if (is.na(hdr)) stop("Header row not found in ", inner, call. = FALSE)
  d <- raw[-seq_len(hdr), ]
  names(d) <- make.unique(clean_names(unlist(raw[hdr, ])))

  sat  <- yn(pick_rx(d, "^single_axis"));  dat <- yn(pick_rx(d, "^dual_axis"))
  fix  <- yn(pick_rx(d, "^fixed_tilt"));   ew  <- yn(pick_rx(d, "^east_west"))
  csi  <- yn(pick_rx(d, "crystalline"));   cdte <- yn(pick_rx(d, "cdte"))
  thin <- Reduce(`|`, lapply(grep("^thin_film_(?!cdte)", names(d), value = TRUE, perl = TRUE),
                             \(n) coalesce(yn(d[[n]]), FALSE)), FALSE)
  n_mount <- rowSums(cbind(sat, dat, fix, ew), na.rm = TRUE)

  arrays <- tibble(
    plant_id   = norm_plant(pick_rx(d, "^plant_(code|id)$")),
    gen_id     = norm_gen(pick_rx(d, "^generator_id$")),
    array_type = case_when(
      n_mount > 1         ~ "Mixed",
      coalesce(sat, FALSE) ~ "Single-axis tracker",
      coalesce(dat, FALSE) ~ "Dual-axis tracker",
      coalesce(ew, FALSE)  ~ "East-west fixed",
      coalesce(fix, FALSE) ~ "Fixed tilt",
      TRUE                 ~ NA_character_),
    bifacial   = yn(pick_rx(d, "bifacial")),
    module     = case_when(
      coalesce(csi, FALSE) & (coalesce(cdte, FALSE) | thin) ~ "c-Si + thin film",
      coalesce(csi, FALSE)  ~ "c-Si",
      coalesce(cdte, FALSE) ~ "CdTe thin film",
      thin                  ~ "Thin film",
      TRUE                  ~ NA_character_),
    tilt       = num(pick_rx(d, "^tilt_angle")),
    azimuth    = num(pick_rx(d, "^azimuth")),
    mwdc       = num(pick_rx(d, "^dc_net_capacity|^dc_capacity"))
  ) |>
    filter(!is.na(plant_id), !is.na(gen_id)) |>
    distinct(plant_id, gen_id, .keep_all = TRUE)

  list(arrays = arrays, url = src$url, year = src$year, release = src$release,
       fetched = Sys.time())
}

# Same pattern as load_solar(), but never fatal: on any failure without a
# cache the dashboard simply shows array details as unavailable.
load_arrays <- function() {
  cached <- if (file.exists(ARRAY_CACHE)) readRDS(ARRAY_CACHE) else NULL
  if (!is.null(cached) &&
      difftime(Sys.time(), file.mtime(ARRAY_CACHE), units = "secs") < ARRAY_TTL) {
    return(cached)
  }
  out <- tryCatch({
    src <- download_latest_860(newer_than = cached)
    if (is.null(src)) NULL else build_arrays(src)
  }, error = function(e) {
    warning("EIA-860 array data unavailable: ", conditionMessage(e))
    NULL
  })
  if (is.null(out)) {
    if (!is.null(cached)) Sys.setFileTime(ARRAY_CACHE, Sys.time())
    return(cached)
  }
  saveRDS(out, ARRAY_CACHE)
  out
}

array_label <- function(a) {
  if (is.null(a)) return("EIA-860 (unavailable)")
  sprintf("EIA-860 %d%s", a$year, if (a$release == "early") " early release" else "")
}

ARRAY_COLS <- c("Single-axis tracker" = "#7C85E4", "Dual-axis tracker" = "#B59AF8",
                "Fixed tilt" = "#F2C94C", "East-west fixed" = "#F2994A", "Mixed" = "#8A8F98")
ARRAY_FILTER <- c(names(ARRAY_COLS), "Not reported")  # sidebar choices
ARRAY_SHORT <- c("Single-axis tracker" = "single-axis", "Dual-axis tracker" = "dual-axis",
                 "Fixed tilt" = "fixed tilt", "East-west fixed" = "east-west",
                 "Mixed" = "mixed")

# Site-level roll-ups across a site's generators, weighted by AC MW
site_array <- function(type, mw) {
  k <- !is.na(type)
  if (!any(k)) return(NA_character_)
  t <- tapply(mw[k], type[k], sum)
  if (length(t) == 1) return(names(t))
  sprintf("Mixed, mostly %s", ARRAY_SHORT[[names(t)[which.max(t)]]])
}
site_bifacial <- function(b, mw) {
  k <- !is.na(b)
  if (!any(k)) return(NA_character_)
  if (all(b[k])) "Yes" else if (!any(b[k])) "No" else "Partial"
}
wmean <- function(x, w) {
  k <- !is.na(x) & !is.na(w) & w > 0
  if (!any(k)) NA_real_ else sum(x[k] * w[k]) / sum(w[k])
}

CY    <- as.integer(format(Sys.Date(), "%Y"))

# Everything the app derives from a snapshot, so a refresh can swap it whole
prep_solar <- function(solar, arrays = NULL) {
  # Pipeline units whose planned COD has already passed are shown as "this year"
  G <- solar$gens |>
    mutate(year_plot = ifelse(stage != "Operating" & !is.na(year) & year < CY, CY, year),
           plant_key = norm_plant(plant_id), gen_key = norm_gen(gen_id))
  arr_cols <- tibble(plant_id = character(), gen_id = character(),
                     array_type = character(), bifacial = logical(), module = character(),
                     tilt = numeric(), azimuth = numeric(), mwdc = numeric())
  A <- if (is.null(arrays)) arr_cols else arrays$arrays
  G <- G |>
    left_join(rename(A, plant_key = plant_id, gen_key = gen_id),
              by = c("plant_key", "gen_key")) |>
    select(-plant_key, -gen_key)
  list(
    raw          = solar,
    arrays       = arrays,
    array_label  = array_label(arrays),
    G            = G,
    period_label = format(solar$period, "%B %Y"),
    yr_rng       = range(G$year_plot, na.rm = TRUE),
    total_op     = sum(G$mw[G$stage == "Operating"]),
    site_max     = G |> group_by(plant_id, stage) |>
                     summarise(mw = sum(mw), .groups = "drop") |>
                     pull(mw) |> max() |> ceiling()
  )
}

# Prepped snapshot, memoised on download time so sessions share one copy
current_data <- local({
  key <- NULL; val <- NULL
  function() {
    s <- load_solar()
    a <- load_arrays()
    k <- paste(format(s$fetched, "%Y-%m-%d %H:%M:%OS3"),
               if (is.null(a)) "none" else format(a$fetched, "%Y-%m-%d %H:%M:%OS3"))
    if (!identical(k, key)) { val <<- prep_solar(s, a); key <<- k }
    val
  }
})
invisible(current_data())  # fail fast at startup if EIA is unreachable and no cache

# ---- Linear design tokens ---------------------------------------------------

lin <- list(
  bg = "#08090A", surface = "#0F1011", raised = "#141516",
  border = "rgba(255,255,255,0.08)", text = "#F7F8F8",
  muted = "#8A8F98", faint = "#62666D", accent = "#5E6AD2"
)

SIZE_BREAKS <- c(0, 5, 20, 100, 250, Inf)
SIZE_LABELS <- c("< 5 MW", "5 to 20", "20 to 100", "100 to 250", "> 250 MW")

# Bubble area proportional to MWac (radius ~ sqrt(MW)), pixel radius
bubble_r <- function(mw) pmin(pmax(sqrt(mw) * 0.95, 2.5), 34)

theme <- bs_theme(
  version = 5, bg = lin$bg, fg = lin$text, primary = lin$accent,
  secondary = lin$raised,
  base_font    = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  font_scale = 0.88,
  `border-radius` = "6px", `border-color` = lin$border,
  `card-bg` = lin$surface, `card-border-color` = lin$border,
  `input-bg` = lin$raised, `input-border-color` = lin$border
) |>
  bs_add_rules(sprintf("
  body { font-feature-settings:'cv11','ss01','tnum'; letter-spacing:-0.005em; }
  .navbar { background:%1$s !important; border-bottom:1px solid %4$s; }
  .brand { display:flex; align-items:center; gap:10px; font-weight:600; font-size:0.95rem; }
  .brand .logo { width:18px; height:18px; border-radius:5px;
                 background:linear-gradient(135deg,%5$s 0%%,#A5ABF2 100%%); }
  .brand .tag { color:%2$s; font-weight:400; font-size:0.78rem;
                border:1px solid %4$s; border-radius:999px; padding:1px 8px; }
  .bslib-sidebar-layout > .sidebar { border-right:1px solid %4$s !important; }
  .sec { color:%3$s; font-size:0.7rem; font-weight:500; text-transform:uppercase;
         letter-spacing:0.06em; margin:4px 0 6px; }
  .card { box-shadow:none; }
  .card-header { background:transparent; border-bottom:1px solid %4$s; font-size:0.8rem;
                 font-weight:500; color:%6$s; padding:10px 14px; }
  .card-header .sub { color:%3$s; font-weight:400; margin-left:6px; }
  .kpi { background:%7$s; border:1px solid %4$s; border-radius:8px; padding:14px 16px; height:100%%; }
  .kpi .lbl { color:%2$s; font-size:0.72rem; margin-bottom:6px; display:flex; align-items:center; gap:6px; }
  .kpi .dot { width:7px; height:7px; border-radius:50%%; display:inline-block; }
  .kpi .val { font-size:1.55rem; font-weight:600; letter-spacing:-0.02em; line-height:1.1; }
  .kpi .foot { color:%3$s; font-size:0.72rem; margin-top:6px; white-space:nowrap;
               overflow:hidden; text-overflow:ellipsis; }
  .form-label, .control-label { color:%2$s; font-size:0.75rem; font-weight:500; }
  .form-control, .selectize-input { font-size:0.82rem; }
  .selectize-input, .selectize-dropdown { background:#141516 !important; border-color:%4$s !important; }
  .selectize-input .item { background:rgba(94,106,210,0.18) !important; color:#C9CDFB !important;
                           border:0 !important; border-radius:4px !important; }
  .form-check-input { background-color:#141516; border-color:rgba(255,255,255,0.18); }
  .form-check-input:checked { background-color:%5$s; border-color:%5$s; }
  .form-check-label { font-size:0.8rem; }
  .stage-sw { display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:6px; }
  .irs--shiny .irs-bar { background:%5$s; border-color:%5$s; }
  .irs--shiny .irs-line { background:#1C1D1F; border-color:%4$s; }
  .irs--shiny .irs-handle { background:%6$s; border:0; box-shadow:0 0 0 3px rgba(94,106,210,0.35); }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:%5$s; font-size:0.7rem; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background:#1C1D1F; color:%2$s; font-size:0.68rem; }
  .btn-lin { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.78rem; }
  .btn-lin:hover { background:#1C1D1F; color:%6$s; }
  .btn-dl { padding:2px 10px; font-size:0.72rem; line-height:1.4; white-space:nowrap; }
  .meta { color:%3$s; font-size:0.72rem; line-height:1.5; }
  .meta a { color:%2$s; }
  /* OpenStreetMap standard tiles, recoloured to sit in the dark UI */
  .osm-dark { filter: invert(100%%) hue-rotate(180deg) grayscale(55%%) brightness(0.82) contrast(1.05); }
  .leaflet-container { background:%1$s; font-family:Inter, sans-serif; }
  .leaflet-popup-content-wrapper, .leaflet-popup-tip { background:#141516; color:%6$s;
       border:1px solid %4$s; box-shadow:0 8px 24px rgba(0,0,0,0.5); }
  .leaflet-popup-content { margin:12px 14px; font-size:0.78rem; line-height:1.55; }
  .pop-k { color:%2$s; }
  .pop-na { color:%3$s; font-style:italic; }
  .pop-stage { display:inline-block; font-size:0.68rem; padding:1px 7px; border-radius:999px;
               border:1px solid currentColor; margin-bottom:4px; }
  .info.legend, .size-legend { background:rgba(15,16,17,0.92) !important; color:%6$s !important;
       border:1px solid %4$s; border-radius:6px; font-size:0.72rem; box-shadow:none; padding:8px 10px; }
  .size-legend .t { color:%2$s; margin-bottom:4px; }
  .leaflet-bar a, .leaflet-control-layers { background:#141516; color:%6$s; border-color:%4$s; }
  .leaflet-control-layers-expanded { font-size:0.75rem; }
  .leaflet-control-attribution { background:rgba(8,9,10,0.75) !important; color:%3$s; }
  .leaflet-control-attribution a { color:%2$s; }
  table.dataTable { font-size:0.78rem; }
  table.dataTable thead th { color:%2$s; font-weight:500; border-bottom:1px solid %4$s !important; }
  table.dataTable tbody td { border-top:1px solid rgba(255,255,255,0.04) !important; }
  table.dataTable tbody tr:hover > * { background:rgba(255,255,255,0.03) !important; }
  table.dataTable tbody tr.selected > * { box-shadow:inset 0 0 0 9999px rgba(94,106,210,0.22) !important;
       color:%6$s !important; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_filter label { color:%3$s; font-size:0.74rem; }
  .dt-buttons .btn { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.74rem; padding:3px 10px; }
  .page-link { background:transparent; border-color:%4$s; color:%2$s; }
  .active > .page-link { background:%5$s; border-color:%5$s; }
  ", lin$bg, lin$muted, lin$faint, lin$border, lin$accent, lin$text, lin$surface))

kpi <- function(label, color, value_id, foot_id) {
  div(class = "kpi",
      div(class = "lbl", span(class = "dot", style = paste0("background:", color)), label),
      div(class = "val", textOutput(value_id, inline = TRUE)),
      div(class = "foot", textOutput(foot_id, inline = TRUE)))
}

plotly_lin <- function(p, ...) {
  base <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, sans-serif", color = lin$muted, size = 11),
    margin = list(l = 8, r = 8, t = 6, b = 6),
    hoverlabel = list(bgcolor = lin$raised, bordercolor = "#2A2B2E",
                      font = list(color = lin$text, family = "Inter")),
    xaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 linecolor = lin$border, tickfont = list(color = lin$faint)),
    yaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 tickfont = list(color = lin$faint)),
    showlegend = FALSE
  )
  do.call(layout, c(list(p), modifyList(base, list(...)))) |>
    config(displayModeBar = FALSE)
}

fmt_mw <- function(x) {
  ifelse(x >= 1000, sprintf("%.1f GW", x / 1000),
         paste(formatC(x, format = "f", digits = 1, big.mark = ","), "MW"))
}

stage_labels <- lapply(STAGES, \(s) HTML(sprintf(
  "<span class='stage-sw' style='background:%s'></span>%s", STAGE_COLS[[s]], s)))


size_legend_html <- function() {
  refs <- c(10, 100, 500)
  r <- bubble_r(refs); h <- 2 * max(r) + 4
  circles <- paste0(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' stroke='#8A8F98' stroke-width='1'/>
     <text x='%.1f' y='%.1f' fill='#8A8F98' font-size='10' text-anchor='middle'>%s</text>",
    max(r) + 2, h - r - 2, r, max(r) + 2, h - 2 * r - 5,
    c("10", "100", "500 MW")), collapse = "")
  sprintf("<div class='size-legend'><div class='t'>Bubble area = MWac</div>
          <svg width='%d' height='%d'>%s</svg></div>", ceiling(2 * max(r) + 30), ceiling(h + 12), circles)
}

# Card header with a CSV download button on the right
table_header <- function(title, sub, dl_id) {
  card_header(
    class = "d-flex align-items-center gap-2",
    span(title, span(class = "sub", sub)),
    div(class = "ms-auto",
        downloadButton(dl_id, "CSV", icon = icon("download"), class = "btn-lin btn-dl"))
  )
}

# ---- UI ---------------------------------------------------------------------

# UI is a function so every new visitor gets controls built from the newest
# snapshot (e.g. after someone else refreshed, or the daily check found a month)
ui <- function(req) {
d <- current_data()
G <- d$G; yr_rng <- d$yr_rng; SITE_MW_MAX <- d$site_max

page_sidebar(
  theme = theme, fillable = FALSE, window_title = "US Solar PV Fleet",
  title = div(class = "brand", span(class = "logo"), "US Solar PV Fleet",
              span(class = "tag", "EIA-860M ·", textOutput("period_tag", inline = TRUE))),
  sidebar = sidebar(
    width = 290, bg = lin$bg,
    div(class = "sec", "Layers"),
    checkboxGroupInput("stages", NULL, choiceNames = stage_labels,
                       choiceValues = STAGES,
                       selected = c("Operating", "Under construction", "Permitted")),
    div(class = "sec", "Filters"),
    selectizeInput("state", "State", choices = sort(unique(G$state)), multiple = TRUE,
                   options = list(placeholder = "All states")),
    selectizeInput("sector", "Sector", choices = sort(unique(na.omit(G$sector))),
                   multiple = TRUE, options = list(placeholder = "All sectors")),
    selectizeInput("array", "Array type", choices = ARRAY_FILTER, multiple = TRUE,
                   options = list(placeholder = "All array types")),
    sliderInput("mw", "Site size (MWac)", min = 0, max = SITE_MW_MAX,
                value = c(1, SITE_MW_MAX), step = 1, ticks = FALSE),
    sliderInput("yr", "Online year (actual or planned)", min = yr_rng[1], max = yr_rng[2],
                value = yr_rng, step = 1, sep = "", ticks = FALSE),
    textInput("q", "Search site or owner", placeholder = "e.g. NextEra"),
    actionButton("reset", "Reset filters", class = "btn-lin w-100"),
    hr(style = sprintf("border-color:%s; opacity:1; margin:14px 0 10px;", lin$border)),
    div(class = "sec", "Data"),
    actionButton("refresh", "Check EIA for newer data",
                 icon = icon("rotate"), class = "btn-lin w-100"),
    div(style = "height:10px"),
    uiOutput("meta")
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    kpi("Operating",          STAGE_COLS[["Operating"]],          "k_op",  "k_op_f"),
    kpi("Under construction", STAGE_COLS[["Under construction"]], "k_uc",  "k_uc_f"),
    kpi("Full pipeline",      STAGE_COLS[["Permitted"]],          "k_pl",  "k_pl_f"),
    kpi("Largest operating site", "#D4D8FF",                      "k_big", "k_big_f")
  ),

  card(
    full_screen = TRUE, height = "600px",
    card_header("Sites", span(class = "sub",
      "one bubble per site and stage · filled = operating, dashed ring = pipeline")),
    card_body(padding = 0, leafletOutput("map", height = "100%"))
  ),

  card(
    full_screen = TRUE, height = "430px",
    card_header("Capacity trajectory", span(class = "sub",
      "annual additions by year online · cumulative fleet if pipeline stages complete")),
    card_body(plotlyOutput("trajectory", height = "100%"))
  ),

  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(height = "400px", full_screen = TRUE,
         card_header("Capacity by state", span(class = "sub", "top 15, operating + pipeline")),
         card_body(plotlyOutput("by_state", height = "100%"))),
    card(height = "400px", full_screen = TRUE,
         card_header("Site size distribution", span(class = "sub", "count of sites by stage")),
         card_body(plotlyOutput("by_size", height = "100%")))
  ),

  card(
    full_screen = TRUE,
    table_header("Capacity by owner",
                 "EIA entity (owner/operator) · array mix from EIA-860 · respects all filters",
                 "dl_owners"),
    card_body(DTOutput("owners"))
  ),

  card(
    full_screen = TRUE,
    table_header("Sites", "select a row to fly to it on the map", "dl_sites"),
    card_body(DTOutput("sites_tbl"))
  )
)
}

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # Current snapshot for this session. Starts from the cache/startup load;
  # the refresh button swaps it for a newer EIA month without a restart.
  # Another session that started earlier keeps its data until it refreshes too.
  D <- reactiveVal(current_data())

  output$period_tag <- renderText(D()$period_label)

  output$meta <- renderUI({
    s <- D()$raw
    div(class = "meta",
        "Source: U.S. EIA, ", a("Form EIA-860M", href = s$url, target = "_blank"),
        " (", D()$period_label, "). Energy source SUN, prime mover PV, units of 1 MW and up. ",
        "Capacity is nameplate MWac, rolled up to one bubble per site and stage. ",
        "Pipeline units with a planned COD already in the past are shown in ", CY, ".",
        br(), br(),
        "Array type, bifacial, DC capacity, module and angles: ",
        if (is.null(D()$arrays)) "annual EIA-860 not available right now." else
          tagList(a(D()$array_label, href = D()$arrays$url, target = "_blank"),
                  " (Schedule 3_3 Solar), matched by plant and generator ID. ",
                  "Newer plants and the pipeline show as not yet reported."),
        br(), br(),
        "Map data © ", a("OpenStreetMap", href = "https://www.openstreetmap.org/copyright",
                         target = "_blank"), " contributors.",
        br(), sprintf("Downloaded %s", format(s$fetched, "%Y-%m-%d %H:%M")))
  })

  # Re-sync filter controls to a new snapshot, keeping selections that still exist
  sync_inputs <- function(d) {
    updateSelectizeInput(session, "state", choices = sort(unique(d$G$state)),
                         selected = intersect(input$state, d$G$state))
    updateSelectizeInput(session, "sector", choices = sort(unique(na.omit(d$G$sector))),
                         selected = intersect(input$sector, d$G$sector))
    updateSliderInput(session, "mw", max = d$site_max,
                      value = c(min(input$mw[1], d$site_max), d$site_max))
    updateSliderInput(session, "yr", min = d$yr_rng[1], max = d$yr_rng[2], value = d$yr_rng)
  }

  # Checks both sources: a newer EIA-860M month and a newer annual EIA-860
  observeEvent(input$refresh, {
    id <- showNotification(sprintf("Checking eia.gov for data newer than %s / %s…",
                                   D()$period_label, D()$array_label),
                           duration = NULL, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    updateActionButton(session, "refresh", label = "Checking…")
    on.exit(updateActionButton(session, "refresh", label = "Check EIA for newer data"),
            add = TRUE)

    monthly <- tryCatch({
      src <- download_latest_860m(newer_than = D()$raw$period)
      if (is.null(src)) NULL else build_solar(src)
    }, error = function(e) e)
    annual <- tryCatch({
      src <- download_latest_860(newer_than = D()$arrays)
      if (is.null(src)) NULL else build_arrays(src)
    }, error = function(e) e)

    msgs <- character()
    solar  <- D()$raw
    arrays <- D()$arrays
    if (inherits(monthly, "error")) {
      msgs <- c(msgs, paste("EIA-860M check failed:", conditionMessage(monthly)))
    } else if (!is.null(monthly)) {
      saveRDS(monthly, CACHE_FILE); solar <- monthly
    }
    if (inherits(annual, "error")) {
      msgs <- c(msgs, paste("EIA-860 check failed:", conditionMessage(annual)))
    } else if (!is.null(annual)) {
      saveRDS(annual, ARRAY_CACHE); arrays <- annual
    }

    changed <- !identical(solar, D()$raw) || !identical(arrays, D()$arrays)
    if (changed) {
      new <- prep_solar(solar, arrays)
      D(new)
      sync_inputs(new)
      showNotification(sprintf("Updated: EIA-860M %s, %s.", new$period_label, new$array_label),
                       type = "message", duration = 6)
    } else if (!length(msgs)) {
      showNotification(sprintf("Already on the latest releases (EIA-860M %s, %s).",
                               D()$period_label, D()$array_label),
                       type = "message", duration = 5)
    }
    for (m in msgs) showNotification(m, type = "error", duration = 8)
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "stages",
                             selected = c("Operating", "Under construction", "Permitted"))
    updateSelectizeInput(session, "state", selected = character(0))
    updateSelectizeInput(session, "sector", selected = character(0))
    updateSelectizeInput(session, "array", selected = character(0))
    updateSliderInput(session, "mw", value = c(1, D()$site_max))
    updateSliderInput(session, "yr", value = D()$yr_rng)
    updateTextInput(session, "q", value = "")
  })

  q_d <- debounce(reactive(input$q), 300)

  # Generator-level rows after attribute filters (all stages)
  gens <- reactive({
    d <- D()$G |> filter(is.na(year_plot) | (year_plot >= input$yr[1] & year_plot <= input$yr[2]))
    if (length(input$state))  d <- filter(d, state %in% input$state)
    if (length(input$sector)) d <- filter(d, sector %in% input$sector)
    # Generator-level, so a site with mixed mounting keeps only the matching
    # units and every chart and table shows capacity of the chosen array types
    if (length(input$array)) d <- filter(d, coalesce(array_type, "Not reported") %in% input$array)
    q <- trimws(q_d())
    if (nzchar(q)) d <- filter(d, grepl(q, plant, ignore.case = TRUE) |
                                  grepl(q, owner, ignore.case = TRUE))
    d
  })

  # One row per site and stage: the bubble map unit
  sites_all <- reactive({
    gens() |>
      arrange(year_plot, month) |>
      group_by(plant_id, stage) |>
      summarise(
        plant = first(plant), owner = first(owner), state = first(state),
        county = first(county), sector = first(sector), ba = first(ba),
        status = paste(unique(status), collapse = "; "),
        lat = first(lat), lon = first(lon),
        yr_first = first(year), mo_first = first(month),
        yr_last = suppressWarnings(max(year, na.rm = TRUE)),
        # array details from the annual EIA-860, rolled up by AC MW
        array    = site_array(array_type, mw),
        bifacial = site_bifacial(bifacial, mw),
        module   = if (all(is.na(module))) NA_character_ else
                     paste(sort(unique(na.omit(module))), collapse = " + "),
        tilt     = wmean(tilt, mw),
        azimuth  = wmean(azimuth, mw),
        mwdc     = if (all(is.na(mwdc))) NA_real_ else sum(mwdc, na.rm = TRUE),
        mwac_dc  = sum(mw[!is.na(mwdc)]),
        known_mw   = sum(mw[!is.na(array_type)]),
        tracker_mw = sum(mw[array_type %in% c("Single-axis tracker", "Dual-axis tracker")]),
        fixed_mw   = sum(mw[array_type %in% c("Fixed tilt", "East-west fixed")]),
        bif_mw     = sum(mw[bifacial %in% TRUE]),
        bif_known_mw = sum(mw[!is.na(bifacial)]),
        mw = sum(mw), n_gen = n(),
        .groups = "drop") |>
      mutate(ilr = ifelse(!is.na(mwdc) & mwac_dc > 0, mwdc / mwac_dc, NA_real_)) |>
      mutate(yr_last = ifelse(is.finite(yr_last), yr_last, NA_integer_)) |>
      filter(mw >= input$mw[1], mw <= input$mw[2])
  })

  sites <- reactive({
    sites_all() |>
      filter(stage %in% input$stages) |>
      mutate(size_class = cut(mw, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
             radius = bubble_r(mw),
             online = ifelse(is.na(yr_first), "-",
                        ifelse(!is.na(mo_first),
                               paste(month.abb[pmin(pmax(mo_first, 1), 12)], yr_first),
                               as.character(yr_first)))) |>
      arrange(desc(mw))
  })

  # KPIs (stage cards ignore the layer toggles so they always tell the full story)
  stage_sum <- function(st) {
    s <- sites_all() |> filter(stage %in% st)
    list(mw = sum(s$mw), n = nrow(s))
  }
  output$k_op   <- renderText(fmt_mw(stage_sum("Operating")$mw))
  output$k_op_f <- renderText(sprintf("%s sites · %.0f%% of US PV fleet",
    format(stage_sum("Operating")$n, big.mark = ","),
    100 * stage_sum("Operating")$mw / D()$total_op))
  output$k_uc   <- renderText(fmt_mw(stage_sum("Under construction")$mw))
  output$k_uc_f <- renderText({
    s <- sites_all() |> filter(stage == "Under construction")
    if (!nrow(s)) return("no sites")
    sprintf("%s sites · online %s to %s", nrow(s),
            min(pmax(s$yr_first, CY), na.rm = TRUE), max(s$yr_last, na.rm = TRUE))
  })
  output$k_pl   <- renderText(fmt_mw(stage_sum(PIPELINE)$mw))
  output$k_pl_f <- renderText({
    s <- stage_sum(PIPELINE); op <- stage_sum("Operating")$mw
    sprintf("%s sites · %s of today's fleet", format(s$n, big.mark = ","),
            if (op > 0) sprintf("%.0f%%", 100 * s$mw / op) else "-")
  })
  big <- reactive(sites_all() |> filter(stage == "Operating") |> slice_max(mw, n = 1, with_ties = FALSE))
  output$k_big   <- renderText(if (nrow(big())) fmt_mw(big()$mw) else "-")
  output$k_big_f <- renderText(if (nrow(big())) sprintf("%s, %s", big()$plant, big()$state) else "")

  # ---- Bubble map (OpenStreetMap) ----
  output$map <- renderLeaflet({
    osm_attr <- '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    osm_url  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
    leaflet(options = leafletOptions(preferCanvas = TRUE, worldCopyJump = TRUE)) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM dark",
               options = tileOptions(maxZoom = 19, className = "osm-dark")) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM standard",
               options = tileOptions(maxZoom = 19)) |>
      addLayersControl(baseGroups = c("OSM dark", "OSM standard"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(-98.5, 39.5, zoom = 4) |>
      addLegend("bottomright", colors = unname(STAGE_COLS), labels = STAGES,
                title = "Stage", opacity = 1) |>
      addControl(html = HTML(size_legend_html()), position = "bottomleft",
                 className = "size-legend-wrap")
  })

  observe({
    d <- sites() |> mutate(stage = as.character(stage))
    proxy <- leafletProxy("map") |> clearGroup("sites") |> clearGroup("focus")
    if (!nrow(d)) return()
    col <- unname(STAGE_COLS[as.character(d$stage)])
    # Array block from the annual EIA-860 (one to three lines, only what's known)
    arr_line <- ifelse(
      is.na(d$array),
      sprintf("<span class='pop-k'>Array</span> <span class='pop-na'>%s</span><br>",
              ifelse(d$stage == "Operating",
                     paste("not in", D()$array_label), "reported once operating")),
      sprintf("<span class='pop-k'>Array</span> %s%s<br>", htmlEscape(d$array),
              ifelse(is.na(d$bifacial), "",
                     paste0(" &middot; ", ifelse(d$bifacial == "Yes", "bifacial",
                                          ifelse(d$bifacial == "No", "monofacial",
                                                 "partly bifacial"))))))
    dc_line <- ifelse(is.na(d$mwdc), "",
      sprintf("<span class='pop-k'>DC</span> %s MWdc%s<br>",
              formatC(d$mwdc, format = "f", digits = 1, big.mark = ","),
              ifelse(is.na(d$ilr), "", sprintf(" &middot; <span class='pop-k'>ILR</span> %.2f", d$ilr))))
    ang <- ifelse(is.na(d$tilt) & is.na(d$azimuth), NA_character_,
                  sprintf("tilt %s&deg; / az %s&deg;",
                          ifelse(is.na(d$tilt), "-", round(d$tilt)),
                          ifelse(is.na(d$azimuth), "-", round(d$azimuth))))
    mod_line <- ifelse(is.na(d$module) & is.na(ang), "",
      sprintf("<span class='pop-k'>Module</span> %s<br>",
              paste0(ifelse(is.na(d$module), "", htmlEscape(d$module)),
                     ifelse(!is.na(d$module) & !is.na(ang), " &middot; ", ""),
                     ifelse(is.na(ang), "", ang))))

    pop <- sprintf(
      "<span class='pop-stage' style='color:%s'>%s</span><br>
       <b>%s</b><br>
       <span class='pop-k'>Owner</span> %s<br>
       <span class='pop-k'>Location</span> %s, %s<br>
       <span class='pop-k'>Capacity</span> %s MWac &middot; %d unit%s<br>
       %s%s%s
       <span class='pop-k'>%s</span> %s<br>
       <span class='pop-k'>Status</span> %s<br>
       <span class='pop-k'>Sector</span> %s &middot; <span class='pop-k'>BA</span> %s &middot;
       <span class='pop-k'>EIA ID</span> %s",
      col, d$stage, htmlEscape(d$plant), htmlEscape(d$owner),
      htmlEscape(ifelse(is.na(d$county), "-", d$county)), d$state,
      formatC(d$mw, format = "f", digits = 1, big.mark = ","), d$n_gen,
      ifelse(d$n_gen == 1, "", "s"),
      arr_line, dc_line, mod_line,
      ifelse(d$stage == "Operating", "In service", "Planned online"), d$online,
      htmlEscape(d$status), htmlEscape(d$sector), d$ba, d$plant_id)
    lab <- sprintf("%s · %s MW · %s", d$plant,
                   formatC(d$mw, format = "f", digits = 1), d$stage)

    is_op <- d$stage == "Operating"
    # Operating: solid bubbles. Pipeline: dashed rings drawn on top so
    # expansions at an existing site stay visible.
    if (any(is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[is_op], lat = d$lat[is_op], radius = d$radius[is_op],
        group = "sites", layerId = paste0(d$plant_id[is_op], "_op"),
        stroke = TRUE, weight = 0.8, color = col[is_op], opacity = 0.9,
        fillColor = col[is_op], fillOpacity = 0.45,
        popup = pop[is_op], label = lab[is_op])
    }
    if (any(!is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[!is_op], lat = d$lat[!is_op], radius = d$radius[!is_op],
        group = "sites", layerId = paste0(d$plant_id[!is_op], "_", d$stage[!is_op]),
        stroke = TRUE, weight = 1.4, dashArray = "3,3", color = col[!is_op], opacity = 0.95,
        fillColor = col[!is_op], fillOpacity = 0.10,
        popup = pop[!is_op], label = lab[!is_op])
    }
  })

  # ---- Capacity trajectory: history + pipeline ----
  output$trajectory <- renderPlotly({
    g <- gens() |> filter(!is.na(year_plot), stage %in% input$stages)
    validate(need(nrow(g) > 0, "No capacity for the current filters."))

    yrs <- seq(min(g$year_plot), max(c(g$year_plot, CY)))
    annual <- g |>
      group_by(year_plot, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop") |>
      complete(year_plot = yrs, stage = factor(STAGES, STAGES), fill = list(gw = 0))

    p <- plot_ly()
    for (s in intersect(STAGES, input$stages)) {
      a <- filter(annual, stage == s, gw > 0)
      if (!nrow(a)) next
      p <- p |> add_bars(data = a, x = ~year_plot, y = ~gw, name = s,
                         marker = list(color = STAGE_COLS[[s]],
                                       opacity = if (s == "Operating") 1 else 0.85),
                         hovertemplate = paste0(s, " · %{x}: +%{y:.2f} GW<extra></extra>"))
    }

    # Cumulative lines: solid history, then dashed scenarios stacking stages
    # in order of maturity (construction first, early planning last).
    op <- annual |> filter(stage == "Operating") |> arrange(year_plot) |>
      mutate(cum = cumsum(gw))
    hist_end <- max(c(g$year_plot[g$stage == "Operating"], CY - 1), na.rm = TRUE)
    base <- sum(op$gw)
    if ("Operating" %in% input$stages) {
      h <- filter(op, year_plot <= hist_end)
      p <- p |> add_lines(data = h, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = "Operating fleet (cumulative)",
                          line = list(color = "#D4D8FF", width = 2),
                          hovertemplate = "%{x}: %{y:.1f} GW installed<extra></extra>")
    } else base <- 0

    fut_years <- yrs[yrs >= hist_end]
    pipe_on <- intersect(PIPELINE, input$stages)
    scen <- list(c("Under construction"),
                 c("Under construction", "Permitted"),
                 PIPELINE)
    scen_name <- c("+ under construction", "+ permitted", "+ full pipeline")
    dash <- c("dash", "dashdot", "dot")
    for (i in seq_along(scen)) {
      st <- intersect(scen[[i]], pipe_on)
      if (!length(st) || (i > 1 && setequal(st, intersect(scen[[i - 1]], pipe_on)))) next
      add <- annual |> filter(stage %in% st, year_plot >= hist_end) |>
        group_by(year_plot) |> summarise(gw = sum(gw), .groups = "drop") |>
        right_join(tibble(year_plot = fut_years), by = "year_plot") |>
        mutate(gw = coalesce(gw, 0)) |> arrange(year_plot) |>
        mutate(cum = base + cumsum(gw))
      add <- bind_rows(tibble(year_plot = hist_end, cum = base), add)
      p <- p |> add_lines(data = add, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = scen_name[i],
                          line = list(color = STAGE_COLS[[st[length(st)]]], width = 1.6,
                                      dash = dash[i]),
                          hovertemplate = paste0(scen_name[i], " · %{x}: %{y:.1f} GW<extra></extra>"))
    }

    p |> plotly_lin(
      barmode = "stack", bargap = 0.22, showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10, color = lin$muted),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(title = "", dtick = 2, tickfont = list(color = lin$faint),
                   range = c(max(min(yrs), CY - 16) - 0.5, max(yrs) + 0.5)),
      yaxis = list(title = list(text = "GWac added per year", font = list(size = 10)),
                   gridcolor = "rgba(255,255,255,0.05)", tickfont = list(color = lin$faint)),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = "cumulative GWac", font = list(size = 10)),
                    tickfont = list(color = lin$faint)),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = CY - 0.5, x1 = CY - 0.5, y0 = 0, y1 = 1,
                         line = list(color = "rgba(255,255,255,0.25)", width = 1, dash = "dot"))),
      annotations = list(list(x = CY - 0.5, y = 1, xref = "x", yref = "paper",
                              text = paste(CY, "→ pipeline"), showarrow = FALSE,
                              xanchor = "left", yanchor = "top", xshift = 4,
                              font = list(size = 10, color = lin$faint))),
      margin = list(l = 8, r = 44, t = 34, b = 6))
  })

  output$by_state <- renderPlotly({
    s <- sites_all() |> filter(stage %in% input$stages) |>
      group_by(state, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop")
    validate(need(nrow(s) > 0, "No sites for the current filters."))
    top <- s |> group_by(state) |> summarise(t = sum(gw)) |> slice_max(t, n = 15) |> arrange(t)
    s <- filter(s, state %in% top$state) |> mutate(state = factor(state, top$state))
    p <- plot_ly()
    for (st in intersect(STAGES, unique(as.character(s$stage)))) {
      p <- p |> add_bars(data = filter(s, stage == st), x = ~gw, y = ~state,
                         orientation = "h", name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{y}: %{x:.2f} GW<extra></extra>"))
    }
    p |> plotly_lin(barmode = "stack", bargap = 0.3,
                    xaxis = list(title = list(text = "GWac", font = list(size = 10))),
                    yaxis = list(title = "", tickfont = list(color = lin$muted)))
  })

  output$by_size <- renderPlotly({
    z <- sites() |> count(size_class, stage, .drop = FALSE) |>
      filter(stage %in% input$stages)
    p <- plot_ly()
    for (st in intersect(STAGES, input$stages)) {
      p <- p |> add_bars(data = filter(z, stage == st), x = ~size_class, y = ~n, name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{x}: %{y:,} sites<extra></extra>"))
    }
    p |> plotly_lin(barmode = "group", bargap = 0.25, bargroupgap = 0.08,
                    xaxis = list(title = "", tickfont = list(color = lin$muted)),
                    yaxis = list(title = ""))
  })

  # ---- Capacity by owner ----
  # Shares of operating capacity are over MWac that has array data, so a
  # missing EIA-860 record doesn't read as "fixed" or "monofacial"
  share <- function(num, den) ifelse(den > 0, num / den, NA_real_)

  owner_tbl <- reactive({
    s <- sites_all() |> mutate(op = stage == "Operating")
    s |>
      group_by(Owner = owner) |>
      summarise(
        `Operating sites` = sum(op),
        `Operating MWac`  = sum(mw[op]),
        `Operating MWdc`  = if (any(op & !is.na(mwdc))) sum(mwdc[op & !is.na(mwdc)]) else NA_real_,
        ac_dc     = sum(mwac_dc[op]),
        known     = sum(known_mw[op]),
        tracker   = sum(tracker_mw[op]),
        fixed     = sum(fixed_mw[op]),
        bif       = sum(bif_mw[op]),
        bif_known = sum(bif_known_mw[op]),
        `Under constr. MWac` = sum(mw[stage == "Under construction"]),
        `Permitted MWac`  = sum(mw[stage == "Permitted"]),
        `Earlier-stage MWac` = sum(mw[stage %in% c("Pending approval", "Early planning")]),
        States = n_distinct(state),
        `Largest site` = if (any(op)) sprintf("%s (%s MW)", plant[op][which.max(mw[op])],
                                              formatC(max(mw[op]), format = "f", digits = 0))
                         else "-",
        .groups = "drop") |>
      mutate(
        `Share of operating` = `Operating MWac` / max(sum(`Operating MWac`), 1),
        `Fleet ILR`          = ifelse(ac_dc > 0, `Operating MWdc` / ac_dc, NA_real_),
        `Tracker share`      = share(tracker, known),
        `Fixed-tilt share`   = share(fixed, known),
        `Bifacial share`     = share(bif, bif_known),
        `Array data coverage`= share(known, `Operating MWac`),
        `Total incl. pipeline` = `Operating MWac` + `Under constr. MWac` +
                                 `Permitted MWac` + `Earlier-stage MWac`) |>
      select(Owner, `Operating sites`, `Operating MWac`, `Share of operating`,
             `Operating MWdc`, `Fleet ILR`, `Tracker share`, `Fixed-tilt share`,
             `Bifacial share`, `Array data coverage`, `Under constr. MWac`,
             `Permitted MWac`, `Earlier-stage MWac`, `Total incl. pipeline`,
             States, `Largest site`) |>
      arrange(desc(`Operating MWac`), desc(`Total incl. pipeline`))
  })

  OWNER_PCT <- c("Share of operating", "Tracker share", "Fixed-tilt share",
                 "Bifacial share", "Array data coverage")

  output$owners <- renderDT({
    d <- owner_tbl()
    num_cols <- which(vapply(d, is.numeric, logical(1))) - 1
    datatable(
      d, rownames = FALSE, selection = "none",
      style = "bootstrap5", class = "compact hover",
      options = list(pageLength = 15, dom = "ftip", scrollX = TRUE,
                     order = list(list(2, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = unname(num_cols))))
    ) |>
      formatRound(c("Operating MWac", "Operating MWdc", "Under constr. MWac", "Permitted MWac",
                    "Earlier-stage MWac", "Total incl. pipeline"), digits = 0) |>
      formatRound("Fleet ILR", digits = 2) |>
      formatPercentage(OWNER_PCT, digits = 1) |>
      formatStyle("Operating MWac",
                  background = styleColorBar(c(0, max(d$`Operating MWac`, 1)),
                                             "rgba(94,106,210,0.35)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Under constr. MWac",
                  background = styleColorBar(c(0, max(d$`Under constr. MWac`, 1)),
                                             "rgba(242,153,74,0.30)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$dl_owners <- downloadHandler(
    filename = function() sprintf("us_solar_pv_owners_%s.csv", Sys.Date()),
    content  = function(file) {
      d <- owner_tbl() |>
        mutate(across(all_of(OWNER_PCT), ~ round(100 * .x, 1)),
               across(c(`Operating MWac`, `Operating MWdc`, `Under constr. MWac`,
                        `Permitted MWac`, `Earlier-stage MWac`, `Total incl. pipeline`),
                      ~ round(.x, 1)),
               `Fleet ILR` = round(`Fleet ILR`, 3)) |>
        rename_with(~ paste(.x, "(%)"), all_of(OWNER_PCT))
      write.csv(d, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  # ---- Site table ----
  # One frame feeds both the on-screen table and the CSV, in the same row order
  # as sites() so a selected row maps back to the right bubble
  sites_tbl_data <- reactive({
    sites() |> transmute(
      Site = plant, Stage = as.character(stage), Owner = owner, State = state,
      County = county, MWac = round(mw, 1), MWdc = round(mwdc, 1),
      ILR = round(ilr, 2), Array = array, Bifacial = bifacial, Module = module,
      `Tilt °` = round(tilt), `Azimuth °` = round(azimuth),
      Units = n_gen, Online = online, Status = status, Sector = sector, BA = ba,
      Lat = round(lat, 4), Lon = round(lon, 4), `EIA ID` = plant_id)
  })

  output$dl_sites <- downloadHandler(
    filename = function() sprintf("us_solar_pv_sites_%s.csv", Sys.Date()),
    content  = function(file) {
      d <- sites_tbl_data() |> rename(`Tilt (deg)` = `Tilt °`, `Azimuth (deg)` = `Azimuth °`)
      write.csv(d, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$sites_tbl <- renderDT({
    d <- sites_tbl_data()
    num_cols <- which(vapply(d, is.numeric, logical(1))) - 1
    datatable(
      d,
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      options = list(pageLength = 12, dom = "ftip", order = list(list(5, "desc")),
                     scrollX = TRUE,
                     columnDefs = list(list(className = "dt-right", targets = unname(num_cols))))
    ) |>
      formatStyle("Stage", color = styleEqual(STAGES, unname(STAGE_COLS))) |>
      formatStyle("Array", color = styleEqual(names(ARRAY_COLS), unname(ARRAY_COLS)))
  })

  observeEvent(input$sites_tbl_rows_selected, {
    r <- sites()[input$sites_tbl_rows_selected, ]
    leafletProxy("map") |>
      flyTo(r$lon, r$lat, zoom = 12, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$lon, lat = r$lat, radius = r$radius + 6, group = "focus",
                       stroke = TRUE, color = "#FFFFFF", weight = 1.5, opacity = 0.9,
                       fill = FALSE)
  })
}

shinyApp(ui, server)
