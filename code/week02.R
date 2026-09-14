# ==============================================================================
# ECON 895  Spatial Techniques in Empirical Economics
# Week 2, 31 August 2026.  Vector Data, Distance, and IVs
#
# HOW TO USE THIS FILE
#   1. Open ECON895.Rproj first. That sets the working directory to the repo
#      root, which is why every path below starts with data/ and not ../data/
#      or a path from your own machine.
#   2. Work down the file. The heading on each block gives the slide it goes
#      with, so you can read the deck and the code side by side.
#   3. Blocks marked PREDICT are the ones we voted on in class. Commit to an
#      answer before you run them. Being wrong is the point.
#
# One new package this week. AER, for the instrumental-variables slides:
#   install.packages("AER")
# ==============================================================================

library(tidyverse)
library(sf)
library(units)

cps08 <- read_csv("data/cps08.csv", show_col_types = FALSE)

cities_sf <- read_csv("data/europe_cities_1801.csv", show_col_types = FALSE) %>%
  filter(!(is.na(longitude) | is.na(latitude))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# The .prj on this shapefile is complete, and it is on ED50 rather than WGS84.
# Transform it. Do not relabel it. Relabelling moves every border about 355 m.
europe_sf <- st_read("data/europe_modern.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()

mainz <- cities_sf %>% filter(city_jjk == "MAINZ")


# ---- Slide 8. Why not base R, and library() vs require() ---------------------
packageVersion("dplyr"); packageVersion("sf")

# The slide says library() fails loudly and require() does not. Run these two
# one at a time and watch the difference.
require(notarealpackage)       # returns FALSE, warns, and CARRIES ON
library(notarealpackage)       # errors and stops

# A script that opens with require() will sail past a missing package and fail
# forty lines later on something that looks unrelated.


# ---- Slide 9. A tibble is a data frame with better manners -------------------
class(cps08)
print(cps08, n = 3)

nrow(cps08)             # 7,711. A base data frame would have printed all of it.

class(cps08["ahe"])     # tibble, one column
class(cps08[["ahe"]])   # numeric vector


# ---- Slide 11. Subsetting dplyr-style ----------------------------------------
new <- select(cps08, age, ahe)
filter(cps08, age == 33)

nrow(cps08); nrow(filter(cps08, age == 33))   # count both sides, always


# ---- Slide 12. Sorting, renaming, and changing variables ---------------------
# Notice what it costs to use three verbs together without a pipe.
tmp <- rename(cps08, wage = ahe)
tmp <- mutate(tmp, lwage = log(wage))
head(arrange(tmp, desc(lwage)), 3)


# ---- Slide 13. The pipe operator ---------------------------------------------
cps08 %>%
  rename(wage = ahe) %>%
  mutate(lwage = log(wage)) %>%
  arrange(desc(lwage)) %>%
  select(age, wage, lwage) %>%
  print(n = 3)

# Same answer, no tmp. Check that rather than take it on faith:
identical(head(arrange(tmp, desc(lwage)), 3),
          cps08 %>% rename(wage = ahe) %>% mutate(lwage = log(wage)) %>%
            arrange(desc(lwage)) %>% head(3))

# The dot. %>% hands the left side to the FIRST argument on the right. Where you
# need it somewhere else, . stands in for it. You need this later tonight.
cps08 %>% nrow()
cps08 %>% nrow(.)


# ---- Slide 14. Grouping and summarizing --------------------------------------
cps08 %>%
  group_by(female, bachelor) %>%
  summarize(mean_ahe = mean(ahe), .groups = "drop")
#.groups=drop same as ungroup command

## R will remember if you grouped data, once you get summary stat you are 
## looking for be sure to ungroup

# .groups controls what grouping is left ON THE RESULT. The default is
# "drop_last", which peels one level and leaves the rest attached, silently.
g <- cps08 %>% group_by(female) %>% mutate(m = mean(ahe))
group_vars(g)                                  # still grouped

nrow(g %>% summarize(n = n()))                 # 2. one row per group.
nrow(g %>% ungroup() %>% summarize(n = n()))   # 1. what you meant.

# Neither errors. The wrong one is a perfectly reasonable-looking number.


# ---- Slide 15. PREDICT. The join that eats your data -------------------------
# How many rows come back? Write your answer down before you run it.
lookup <- tibble(age = 25:30, cohort = "prime")
print(lookup)

nrow(cps08)                                    # before
joined <- inner_join(cps08, lookup, by = "age")
nrow(joined)                                   # after

# Count rows on both sides of every join, and then look at what you kept.
# Week 1's Tanzania join is why the second half of that sentence matters.


# ---- Slide 16. Getting data in and out ---------------------------------------
saveRDS(cps08, "data/cps08.rds")
cps08 <- readRDS("data/cps08.rds")
unlink("data/cps08.rds")            # tidy up; this file is not tracked

##RDS allows you to recall 

# haven reads Stata, SPSS and SAS. readxl reads Excel. Both install with the
# tidyverse. Real example rather than the placeholder name on the slide:
## Tell haven to treat periods from Dat file as NA
library(haven)
head(read_dta(system.file("examples", "iris.dta", package = "haven")), 2)

# The slide says a CSV cannot carry a CRS. Do not take that on faith.
pts <- cities_sf %>% select(city_jjk, countryname)
class(pts)                          # "sf"

f <- tempfile(fileext = ".csv")
write_csv(pts, f)                   # NO error. It writes a file quite happily.
back <- read_csv(f, show_col_types = FALSE)
inherits(back, "sf")                # FALSE. The geometry is now text.
names(back)

f2 <- tempfile(fileext = ".rds")
saveRDS(pts, f2)
inherits(readRDS(f2), "sf")         # TRUE
st_crs(readRDS(f2))$epsg            # 4326, still there
unlink(c(f, f2))

# write_csv() succeeded. It just quietly stopped being spatial.


# ---- Slide 19. What is underneath sf -----------------------------------------
sf_extSoftVersion()[c("GEOS", "GDAL", "PROJ")]

nrow(st_drivers())        # file formats GDAL exposes on your machine

# Your versions will differ from mine. That is the point: pinning sf means
# pinning GDAL, GEOS and PROJ too, and their error messages are not R's.


# ---- Slide 20. Basic simple features -----------------------------------------
st_point(c(8.27, 50.00))                                  # Mainz
st_linestring(rbind(c(8.27, 50.0), c(2.35, 48.86)))       # Mainz to Paris
st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))


# ---- Slide 21. Anatomy of an sf object ---------------------------------------
# The three colours on that figure are three real classes. Build them upward.
p  <- st_point(c(8.27, 50.0))               # one geometry
class(p)

pc <- st_sfc(p, crs = 4326)                 # a geometry COLUMN
class(pc)
st_crs(pc)$epsg                             # the CRS lives on the column

f  <- st_sf(city = "Mainz", geometry = pc)  # a tibble with that column
class(f)
f

# An sf object is a data frame whose geometry column knows what a coordinate
# reference system is. Nothing more exotic than that.


# ---- Slide 23. Loading vector data -------------------------------------------
europe_raw <- st_read("data/europe_modern.shp")   # read what it prints

# Driver, geometry type, bounding box, CRS. The CRS line is the one that
# matters and the one people skip. Note this is NOT called europe_sf: the
# object you read is not yet the object you analyze.


# ---- Slide 24. Learning about this data I ------------------------------------
class(cities_sf)
st_crs(cities_sf)$epsg
st_bbox(cities_sf)


# ---- Slide 25. Learning about this data II -----------------------------------
dim(cities_sf); names(cities_sf)   # note the geometry column

cities_sf %>% st_drop_geometry() %>% count(countryname, sort = TRUE) %>% head(3)


# ---- Slide 26. Plotting what we loaded ---------------------------------------
# xlim/ylim matter. Portugal's polygon reaches the Azores at 31W and Spain's the
# Canaries at 27N, so the default window is mostly empty Atlantic.
par(mar = c(0, 0, 0, 0))
plot(st_geometry(europe_sf), col = "grey93", border = "grey65",
     xlim = c(-11, 31.5), ylim = c(35, 71.5))
plot(st_geometry(cities_sf), add = TRUE,
     pch = 20, cex = 0.35, col = "#005293")

# Your eye catches a bad projection faster than a summary statistic does. Same
# continent, two projections:
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
plot(st_geometry(st_transform(europe_sf, 3035)), col = "grey90",
     border = "grey60", main = "EPSG:3035, equal area")
plot(st_geometry(st_transform(europe_sf, 3857)), col = "grey90",
     border = "grey60", main = "EPSG:3857, Web Mercator")
par(mfrow = c(1, 1))

swe <- europe_sf %>% filter(CntryName == "Sweden")
ita <- europe_sf %>% filter(CntryName == "Italy")
ratio <- function(crs) as.numeric(st_area(st_transform(swe, crs)) /
                                  st_area(st_transform(ita, crs)))
c(equal_area = ratio(3035), mercator = ratio(3857))


# ---- Slide 27. Reading points from CSV files ---------------------------------
cities_csv <- read_csv("data/europe_cities_1801.csv", show_col_types = FALSE)
cities_csv <- cities_csv %>%
  filter(!(is.na(longitude) | is.na(latitude)))          # note the OR, not AND

cities_sf <- st_as_sf(cities_csv, remove = FALSE,
                      coords = c("longitude", "latitude"), crs = 4326)
nrow(cities_sf)

# The slide says reversing longitude and latitude drops your European cities
# into the Indian Ocean. Swap the two names. Nothing complains.
oops <- st_as_sf(cities_csv, coords = c("latitude", "longitude"), crs = 4326)

st_bbox(cities_sf)    # 9.7W to 18.6E,  36.0N to 63.4N.  Europe.
st_bbox(oops)         # 36.0E to 63.4E,  9.7S to 18.6N.  Not Europe.

africa <- st_read("data/africa_scale.shp", quiet = TRUE)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(africa), col = "grey94", border = "grey70",
     xlim = c(-15, 70), ylim = c(-12, 68))
plot(st_geometry(europe_sf), col = "grey94", border = "grey70", add = TRUE)
plot(st_geometry(oops),      add = TRUE, pch = 20, cex = 0.3, col = "firebrick")
plot(st_geometry(cities_sf), add = TRUE, pch = 20, cex = 0.3, col = "#005293")

# Blue is where they belong. Red is the Arabian Sea. No error, no warning, and
# every distance you compute afterwards is measured between the red dots.


# ---- Slide 31. PREDICT. Watch it happen --------------------------------------
# Both objects below claim EPSG:3857. Predict what each one prints.
nairobi <- st_as_sf(tibble(lon = 36.82241, lat = -1.287822),
                    coords = c("lon", "lat"), crs = 4326)

a <- nairobi %>% st_set_crs(3857)
b <- nairobi %>% st_transform(3857)

st_coordinates(a)     # 36.82  -1.29   still degrees, now claiming meters
st_coordinates(b)     # 4099052  -143372

# st_set_crs changes the label. st_transform changes the coordinates. Use the
# first where you needed the second and your data ends up in the ocean.


# ---- Slide 32. A tale of two cities ------------------------------------------
cty1 <- tibble(name = "Nairobi", lon = 36.82241, lat = -1.287822) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cty2 <- tibble(name = "Kinshasa", x = 1531775, y = -531896.9) %>%
  st_as_sf(coords = c("x", "y"), crs = "+proj=moll")

st_crs(cty1)$epsg
st_is_longlat(cty2)

try(rbind(cty1, cty2))       # R refuses. It will not always be so helpful.


# ---- Slide 33. Always transform data in different CRSs -----------------------
cty2  <- cty2 %>% st_transform(4326)
ctys  <- rbind(cty1, cty2)
st_coordinates(ctys)

# "Albers" is ambiguous and EPSG:5070 is not. Two CRSs, both honestly called
# Albers, one point, and look where it lands.
fx <- st_sfc(st_point(c(-77.31, 38.85)), crs = 4326)     # Fairfax
st_crs(5070)$Name; st_coordinates(st_transform(fx, 5070))
st_crs(3005)$Name; st_coordinates(st_transform(fx, 3005))


# ---- Slide 34. Distances redux -----------------------------------------------
st_distance(cty1, cty2)

# Units are the smaller question. Whether the projection suits the measurement
# is the larger one. Same five degrees of longitude, walked north:
gap <- function(lat) {
  a <- st_sfc(st_point(c(0, lat)), crs = 4326)
  b <- st_sfc(st_point(c(5, lat)), crs = 4326)
  c(lat      = lat,
    geodetic = round(as.numeric(st_distance(a, b)) / 1000),
    mercator = round(as.numeric(st_distance(st_transform(a, 3857),
                                            st_transform(b, 3857))) / 1000))
}
t(sapply(c(0, 30, 50, 60), gap))

# Mercator says 557 km at every latitude. At 60N the truth is half that.


# ---- Slide 35. Areas and length ----------------------------------------------
europe_sf %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  arrange(desc(area_km2)) %>%
  select(CntryName, area_km2) %>%
  head(4)

head(europe_sf$name, 3)    # "Cntry Name:" -- a header read in as data


# ---- Slide 36. PREDICT. The area that is two and a half times too big --------
# Germany, measured three ways. Which one is wrong, and by how much?
de <- europe_sf %>% filter(CntryName == "Germany")

as.numeric(st_area(de)) / 1e6                       # 4326, geodetic
as.numeric(st_area(st_transform(de, 3035))) / 1e6   # equal-area
as.numeric(st_area(st_transform(de, 3857))) / 1e6   # Web Mercator

# Germany is about 357,600 km2. Web Mercator is off by a factor of sec^2(lat):
lat <- st_coordinates(st_centroid(st_geometry(de)))[2]
1 / cos(lat * pi / 180)^2

# The wrong number is correctly labelled as square metres. Units would not have
# caught this. Equal-area before st_area(): EPSG:3035 in Europe, 5070 in the US.


# ---- Slide 37. An aside on units ---------------------------------------------
a_to_b <- st_distance(cty1, cty2)
set_units(a_to_b, km)

try(set_units(a_to_b, "km") + set_units(5, "kg"))   # refuses. Good.
as.numeric(a_to_b)                                  # what you give up


# ---- Slide 40. You already have everything you need --------------------------
# The payoff. Two lines build the instrument from a top-five paper.
mainz <- cities_sf %>% filter(city_jjk == "MAINZ")
st_coordinates(mainz)
##Check to make sure same geo system

cities_sf <- cities_sf %>%
  mutate(km_mainz = as.numeric(st_distance(., mainz)) / 1000)


# ---- Slide 41. PREDICT. Nearest neighbors of the press -----------------------
# Which city is closest to Mainz? Take a guess before you run this.
cities_sf %>% st_drop_geometry() %>%
  arrange(km_mainz) %>%
  transmute(city_jjk, countryname, km = round(km_mainz)) %>%
  slice(2:6)
# Mainz itself is row one at zero kilometers, so we start at two.


# ---- Slide 42. The instrument, drawn -----------------------------------------
pal <- colorRampPalette(c("#b40000", "#f0a000", "#2f6fa8"))(100)
idx <- cut(cities_sf$km_mainz, breaks = 100, labels = FALSE)
par(mar = c(0, 0, 1.4, 0))
plot(st_geometry(europe_sf), col = "grey97", border = "grey75",
     xlim = c(-11, 31.5), ylim = c(35, 71.5),
     main = "Distance from Mainz, 1,801 cities")
plot(st_geometry(cities_sf), add = TRUE, pch = 20, cex = 0.5, col = pal[idx])
plot(st_geometry(mainz), add = TRUE, pch = 21, cex = 1.7, bg = "white", lwd = 2)
legend("topleft", bty = "n", cex = 0.75, pch = 20, col = pal[c(1, 50, 100)],
       legend = c("0 km", "1,000 km", "2,000 km"), title = "from Mainz")


# ---- Slide 45. The first stage -----------------------------------------------
press <- read_csv("data/europe_press_1500.csv", show_col_types = FALSE)

est <- cities_sf %>% st_drop_geometry() %>%
  filter(!city_jjk %in% c("ALBA", "HALLE")) %>%  # one name, two cities
  inner_join(press, by = "city_jjk") %>%
  filter(pop1500 > 0, pop1600 > 0) %>%           # log(0) is -Inf, not NA
  mutate(press1500 = as.integer(!is.na(print_date) & print_date <= 1500),
         growth    = log(pop1600) - log(pop1500))

fs <- lm(press1500 ~ km_mainz + log(pop1500), data = est)
round(summary(fs)$coefficients, 6)

# A hundred kilometers closer to Mainz raises the probability of a press by
# about 1.4 points, t = -5.2. Relevance holds.


# ---- Slide 46. PREDICT. Now estimate the thing you care about ----------------
# OLS says printing cities grew 14 log points faster. Predict the sign of the
# IV estimate before you run this.
ols <- lm(growth ~ press1500 + log(pop1500), data = est)
rf  <- lm(growth ~ km_mainz  + log(pop1500), data = est)

c(OLS = coef(ols)["press1500"],               # both estimates
  IV  = coef(rf)["km_mainz"] / coef(fs)["km_mainz"])

# Just-identified, so the IV estimate is exactly the ratio of the reduced form
# to the first stage. That is what 2SLS is. The standard error is not a ratio,
# though, so for that we need the real thing:
library(AER)
iv <- ivreg(growth ~ press1500 + log(pop1500) | km_mainz + log(pop1500),
            data = est)
round(summary(iv)$coefficients["press1500", ], 3)

# Read the standard error, not just the estimate. The interval runs from about
# -0.95 to +0.04. It excludes neither zero nor a large negative effect. The
# instrument did not overturn OLS. It declined to confirm it.


# ---- Slide 47. Where is the variation actually coming from? ------------------
# Distance from Mainz IS position. Watch it dissolve as the regression is told
# where each city sits.
f1 <- lm(press1500 ~ km_mainz + log(pop1500), data = est)
f2 <- update(f1, . ~ . + longitude + latitude)
f3 <- update(f1, . ~ . + poly(longitude, 2) + poly(latitude, 2))

round(sapply(list(f1, f2, f3),
       function(m) summary(m)$coefficients["km_mainz", "t value"]^2), 1)

# 27.1, 15.2, 0.0. Strong, weak, gone. Dittmar conditions on latitude,
# longitude and their interaction, so his identifying variation is what
# survives that. Ours is the raw continental gradient. Different variation,
# not a worse computation.


# ---- Slide 49. The biggest move came from a table note -----------------------
est <- est %>% mutate(ldist = log(pmax(km_mainz, 1)))
wald <- function(dat, inst, ctrl) {
  fs <- lm(reformulate(c(inst, ctrl), "press1500"), data = dat)
  rf <- lm(reformulate(c(inst, ctrl), "growth"),    data = dat)
  c(IV = unname(coef(rf)[inst] / coef(fs)[inst]), n = nobs(fs))
}
his <- est %>% filter(pop1500 >= 5, pop1700 > 0, pop1800 > 0)
geo <- c("log(pop1500)", "latitude", "longitude", "I(latitude*longitude)",
         "rivers_10", "DAnyRomRoad", "Bcapital", "Bcommune")

rbind(`ours, linear distance`    = wald(est, "km_mainz", "log(pop1500)"),
      `+ his sample restriction` = wald(his, "ldist",    "log(pop1500)"),
      `+ his controls`           = wald(his, "ldist",    geo),
      `Dittmar (2011) Table VII` = c(0.58, 410)) %>% round(3)

# The largest single move is the middle row, and it is not a modelling choice.
# It is one sentence in his table note restricting the sample. We never reach
# his number, and the honest reason is that our print cities grew nine
# percentage points faster than the rest where he reports about twenty. This
# is our two-line version of his design, not a replication of it.
