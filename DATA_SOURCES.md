# Where the data came from

A course that argues you should know the provenance of every number owes you this
page. Each dataset below says what it is, where it came from, and what it is doing
in the course.

If you spot an attribution here that is wrong or incomplete, tell me. That is a
contribution, not a nuisance.

---

### `africa_scale.shp` — African country polygons

Natural Earth, small-scale (1:110m) admin-0 boundaries, in WGS84 (EPSG:4326).
Natural Earth is in the public domain. https://www.naturalearthdata.com/

Used in Week 1 for the opening map, the CRS checks, and the spatial join.

### `africa_cities.csv` — African city points

A gazetteer of African cities with coordinates and population. Used in Week 1 as
the points side of the spatial join, and as the source of the Tanzania name
mismatch we walked through in class.

### `europe_cities_1801.csv` — European cities circa 1801

From the Johnson and Koyama city-growth data, built for research on European city
growth. `city_jjk` is the city identifier in that dataset. Mainz is in here, which
is the point: it is where Week 2 ends up.

See Johnson and Koyama (2017), "Jewish communities and city growth in preindustrial
Europe", *Journal of Development Economics* 127, 339–354.

### `europe_modern.shp` — modern European country polygons

A shapefile from a published replication package, kept deliberately in the state it
arrived in. It carries a custom equidistant conic projection with **no EPSG code**,
which is exactly why it is in Week 1. `NA` from `st_crs()$epsg` means
somebody built a projection the registry does not name; the file itself is complete.

### `europe_press_1500.csv` — press adoption dates and city populations

Printing press adoption dates, 1450--1500, and city populations at 1500, 1600,
1700 and 1800, for 1,019 European cities. Keyed on `city_jjk` so it joins to
`europe_cities_1801.csv`. Four extra columns carry rivers, Roman roads, capital
status and communal government, which the Week 2 replication attempt uses.

Extracted from Noel's work in progress on plague, print, and persecution.
Population is Bairoch and Chandler as assembled in the Johnson and Koyama city
data, which is the same source Dittmar (2011) uses. The adoption dates descend
from the standard incunabula lists.

Used in Week 2 for the first stage, the IV estimate, and the attempt to
reproduce Dittmar's Table VII.

Two names in this file, ALBA and HALLE, each denote two different cities. Week 2
drops them by hand rather than let a one-to-many join duplicate rows silently.

### `cps08.csv` — Current Population Survey extract, 2008

The teaching extract distributed with Stock and Watson, *Introduction to
Econometrics*. Hourly earnings, education, sex, and age. Used in Week 2 only, as a
plain rectangular dataset for the tidyverse material, before any geometry appears.

### `mita_boundary.shp` — the boundary of the Peruvian mita

Two linestrings in WGS84 (EPSG:4326), cut from the replication package for Melissa
Dell (2010), "The Persistent Effects of Peru's Mining Mita", *Econometrica* 78(6),
1863--1903. This is the line the colonial state drew in 1573 to mark which districts
owed forced labor to the mines at Potosi and Huancavelica. It stopped binding in 1812.

Used in Week 3 to build the running variable. The whole design turns on the fact that
`st_distance()` from a district to the mita *polygon* returns zero, because a polygon
contains its own interior, so you need `st_boundary()` and a sign.

### `peru_districts.shp` — district polygons for the study region

299 district polygons keyed on `ubigeo`, from the same replication package. These are
the units Dell georeferences and the ones the running variable is measured from.

### `peru_mita_districts.csv` — the district-level analysis file

299 rows carrying the mita dummy, childhood stunting, the number of children, elevation,
slope, the boundary-segment fixed effects, and Dell's own `d_bnd` so you can check your
running variable against hers rather than take mine on faith.

**The latitudes and longitudes in this file are stored unsigned.** Peru is south and
west, so every one of them has the wrong sign until you fix it. That is how Dell stores
them and it is shipped that way on purpose. It is the Week 3 lesson about looking at
your coordinates before you measure anything with them. Correct it, and notice what you did.

### `abramson_states_1340.shp` and `abramson_states_1425.shp` — European polities

Two snapshots of European polity borders, from Scott F. Abramson's polygon data
underlying *The Economic Origins of the Territorial State* (2017). Abramson traced the
borders of roughly two hundred and seventy polities at five-year intervals across six
centuries.

Used at the end of Week 3 for the contrast that makes the mita design work. The French
border moved substantially in the 85 years around the start of the Hundred Years' War,
while the mita line moved not at all in 239 years. A boundary that moves, follows rivers and
mountains, and sorts people for centuries is cheap to compute and impossible to defend
as a discontinuity. The same files then carry the frame where the boundary is the
outcome rather than the design.


---

## Reusing any of this

The R code in `code/` is mine and you may reuse it freely, with attribution.

Several of the datasets belong to other scholars, as the entries above make clear. They are here as
teaching extracts. If you want to build a paper on one of them, go to the original
source, read its terms, and cite it properly rather than citing this repository.
