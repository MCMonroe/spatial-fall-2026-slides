# ==============================================================================
# econ895_setup_test.R
# ECON 895 Spatial Techniques in Empirical Economics -- Fall 2026
#
# Run this before the term starts, and again whenever a week adds a package.
# It checks that your machine can do everything the course needs, and it tells
# you plainly what to fix if it cannot.
#
# HOW TO RUN
#   1. Open RStudio.
#   2. File > Open File, and choose this script.
#   3. Click "Source" (top right of the editor pane), or press Cmd/Ctrl+Shift+S.
#   4. Read the summary at the bottom of the console.
#
# If anything reports FAIL, email njohnsoL@gmu.edu before the next meeting. Do
# not wait until Monday at 7 pm.
#
# Nothing here touches your files or installs anything. It only reads and
# reports.
# ==============================================================================


# ------------------------------------------------------------------------------
# Small helpers so the output is readable
# ------------------------------------------------------------------------------

results <- list()

check <- function(label, expr, fix = NULL) {
  outcome <- tryCatch(
    {
      value <- force(expr)
      if (isTRUE(value)) list(ok = TRUE, note = "") else list(ok = FALSE, note = as.character(value))
    },
    error   = function(e) list(ok = FALSE, note = conditionMessage(e)),
    warning = function(w) list(ok = FALSE, note = conditionMessage(w))
  )

  tag <- if (outcome$ok) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", tag, label))
  if (!outcome$ok) {
    if (nzchar(outcome$note)) cat("       reason: ", outcome$note, "\n", sep = "")
    if (!is.null(fix)) cat("       fix:    ", fix, "\n", sep = "")
  }

  results[[label]] <<- outcome$ok
  invisible(outcome$ok)
}

section <- function(title) cat("\n---- ", title, " ----\n", sep = "")


cat("\n==============================================================\n")
cat(" ECON 895 setup test\n")
cat("==============================================================\n")


# ------------------------------------------------------------------------------
# 1. R itself
# ------------------------------------------------------------------------------

section("R version")

cat("       You are running: ", R.version.string, "\n", sep = "")

check(
  "R is version 4.2.0 or newer",
  getRversion() >= "4.2.0",
  fix = "Download the current version of R from https://cran.r-project.org and reinstall."
)


# ------------------------------------------------------------------------------
# 2. Packages
# ------------------------------------------------------------------------------

section("Required packages")

# AER is here for the Week 2 instrumental-variables slides, and sandwich and
# lmtest for the Week 3 regression discontinuity, where the standard errors
# come from coeftest() and vcovHC(). Unlike units, haven and readxl, which
# arrive as dependencies of sf and the tidyverse, these three need their own
# install.
required <- c("tidyverse", "sf", "terra", "AER", "sandwich", "lmtest")

for (pkg in required) {
  check(
    sprintf("package '%s' is installed", pkg),
    requireNamespace(pkg, quietly = TRUE),
    fix = sprintf('Run:  install.packages("%s")', pkg)
  )
}

# Stop early if the spatial packages are missing. The tests below need them.
if (!all(vapply(c("sf", "terra"), requireNamespace, logical(1), quietly = TRUE))) {
  cat("\n==============================================================\n")
  cat(" STOPPING EARLY. Install sf and terra, then run this again.\n")
  cat("\n On macOS or Linux these two packages need system libraries")
  cat("\n (GDAL, GEOS, PROJ). If install.packages() fails with an error")
  cat("\n mentioning one of those names, send me the full error message.\n")
  cat("==============================================================\n\n")
  knitr_exit <- TRUE
} else {
  knitr_exit <- FALSE
}


if (!knitr_exit) {

  suppressPackageStartupMessages({
    library(sf)
    library(terra)
  })

  # ----------------------------------------------------------------------------
  # 3. The geospatial system libraries underneath sf
  # ----------------------------------------------------------------------------

  section("Geospatial libraries")

  cat("       sf is linked against:\n")
  cat("       ", paste(sf::sf_extSoftVersion()[c("GEOS", "GDAL", "PROJ")],
                       collapse = " / "), "  (GEOS / GDAL / PROJ)\n", sep = "")

  check(
    "sf can talk to GDAL, GEOS, and PROJ",
    all(nzchar(sf::sf_extSoftVersion()[c("GEOS", "GDAL", "PROJ")])),
    fix = "Reinstall sf. If that fails, send me the output of sf::sf_extSoftVersion()."
  )


  # ----------------------------------------------------------------------------
  # 4. Vector data: build geometry, check the CRS, project, measure
  #
  #    This is the whole discipline of the course in one test. We make three
  #    points in longitude and latitude, look at what CRS they carry, project
  #    them to a metric CRS, and confirm the distance comes back in meters.
  # ----------------------------------------------------------------------------

  section("Vector data with sf")

  # Three US cities, longitude first, then latitude. EPSG:4326 is degrees.
  cities <- sf::st_as_sf(
    data.frame(
      name = c("Fairfax", "Boston", "Chicago"),
      lon  = c(-77.3064, -71.0589, -87.6298),
      lat  = c( 38.8462,  42.3601,  41.8781)
    ),
    coords = c("lon", "lat"),
    crs    = 4326
  )

  check(
    "sf can build a point layer",
    inherits(cities, "sf") && nrow(cities) == 3,
    fix = "Reinstall sf."
  )

  check(
    "the layer carries the CRS we asked for (EPSG:4326)",
    sf::st_crs(cities)$epsg == 4326,
    fix = "Reinstall sf."
  )

  # Project to EPSG:5070, Albers Equal Area for the continental US, whose
  # units are meters. Measuring before projecting is the error this course
  # spends fourteen weeks teaching you to catch.
  cities_proj <- sf::st_transform(cities, 5070)

  check(
    "sf can reproject (EPSG:4326 to EPSG:5070)",
    sf::st_crs(cities_proj)$epsg == 5070,
    fix = "Reinstall the PROJ system library, then reinstall sf."
  )

  # Fairfax to Boston is roughly 650 km on the ground. If the projection and
  # the units are working, we should land near that.
  d_m <- as.numeric(sf::st_distance(cities_proj[1, ], cities_proj[2, ]))
  d_km <- d_m / 1000

  cat(sprintf("       Fairfax to Boston measured at %.0f km (expected roughly 650)\n", d_km))

  check(
    "distances come back in meters and are plausible",
    d_km > 550 && d_km < 720,
    fix = "Send me the number printed above. Something is wrong with PROJ."
  )

  check(
    "geometric operations work (buffer, intersects)",
    {
      buf <- sf::st_buffer(cities_proj[1, ], dist = 50000)  # 50 km, in meters
      length(sf::st_intersects(buf, cities_proj)[[1]]) >= 1
    },
    fix = "Reinstall the GEOS system library, then reinstall sf."
  )


  # ----------------------------------------------------------------------------
  # 5. Raster data with terra
  # ----------------------------------------------------------------------------

  section("Raster data with terra")

  check(
    "terra can build a raster and do algebra on it",
    {
      r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10,
                       ymin = 0, ymax = 10, crs = "EPSG:4326")
      terra::values(r) <- seq_len(terra::ncell(r))
      r2 <- r * 2
      terra::global(r2, "max", na.rm = TRUE)[[1]] == 200
    },
    fix = "Reinstall terra."
  )

  check(
    "terra can extract raster values at point locations",
    {
      r <- terra::rast(nrows = 10, ncols = 10, xmin = -180, xmax = 180,
                       ymin = -90, ymax = 90, crs = "EPSG:4326")
      terra::values(r) <- seq_len(terra::ncell(r))
      vals <- terra::extract(r, terra::vect(cities))
      nrow(vals) == 3 && !all(is.na(vals[[2]]))
    },
    fix = "Reinstall terra."
  )


  # ----------------------------------------------------------------------------
  # 6. Reading a real spatial file from disk
  # ----------------------------------------------------------------------------

  section("Reading spatial files")

  check(
    "sf can read a shapefile from disk",
    {
      nc_path <- system.file("shape/nc.shp", package = "sf")
      nc <- sf::st_read(nc_path, quiet = TRUE)
      nrow(nc) == 100
    },
    fix = "Reinstall sf. The test file ships with the package."
  )


  # ----------------------------------------------------------------------------
  # 7. Plotting
  # ----------------------------------------------------------------------------

  section("Plotting")

  check(
    "a map can be drawn without error",
    {
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 400, height = 400)
      plot(sf::st_geometry(cities_proj), pch = 19)
      dev.off()
      ok <- file.exists(tmp) && file.info(tmp)$size > 0
      unlink(tmp)
      ok
    },
    fix = "Send me the error message. This is usually a graphics device problem."
  )


  # ----------------------------------------------------------------------------
  # Summary
  # ----------------------------------------------------------------------------

  passed <- sum(unlist(results))
  total  <- length(results)

  cat("\n==============================================================\n")
  if (passed == total) {
    cat(sprintf(" ALL CLEAR. %d of %d checks passed.\n", passed, total))
    cat(" Your machine is ready for Week 2. See you Monday.\n")
  } else {
    cat(sprintf(" %d of %d checks passed. %d need attention.\n",
                passed, total, total - passed))
   cat("\n Failed checks:\n")
    for (nm in names(results)) {
      if (!isTRUE(results[[nm]])) cat("   - ", nm, "\n", sep = "")
    }
    cat("\n Read the 'fix:' lines above. If you are stuck, email me the\n")
    cat(" full console output BEFORE Sunday.\n")
  }
  cat("==============================================================\n\n")

  cat(" One thing worth noticing. That distance came back in meters, and it\n")
  cat(" would have come back in meters without the projection too, because\n")
  cat(" sf measures on the sphere by default now. The old warning that\n")
  cat(" unprojected distances arrive in degrees stopped being true in 2021,\n")
  cat(" and it sat in this script until someone ran it and checked.\n\n")
  cat(" The trap that is still live is the projected one. Ask a CRS whose\n")
  cat(" units are US survey feet for a 10000 meter buffer and it hands you\n")
  cat(" 3047 meters without a word of complaint. So run st_crs(x)$units\n")
  cat(" before you measure anything. Not is it projected, which everyone\n")
  cat(" asks. What are the units. That is the course in one line.\n\n")
}


# end code