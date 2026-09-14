# ==============================================================================
# ECON 895  Spatial Techniques in Empirical Economics
# Week 3, 14 September 2026.  Buffers and Regression Discontinuity
#
# HOW TO USE THIS FILE
#   1. Open ECON895.Rproj first. That sets the working directory to the repo
#      root, which is why every path below starts with data/ and not a path
#      from someone else's machine.
#   2. Work DOWN the file, in order. Objects built in one block are used by the
#      next, so skipping around will produce "object not found".
#   3. The heading on each block gives the slide it goes with, so you can read
#      the deck and the code side by side.
#   4. Blocks marked PREDICT are the ones we voted on in class. The question is
#      in the comment and the answer is not. Commit to an answer, then run.
#      Being wrong is the point, and it is also the assignment.
#
# Two new packages this week, for the regression discontinuity standard errors:
#   install.packages(c("sandwich", "lmtest"))
#
# The data is Melissa Dell's replication package for "The Persistent Effects of
# Peru's Mining Mita" (Econometrica, 2010), cut down to a teaching extract, plus
# two snapshots of European polity borders from Scott Abramson's data. See
# DATA_SOURCES.md.
# ==============================================================================

library(tidyverse)
library(sf)
library(sandwich)
library(lmtest)

districts <- st_read("data/peru_districts.shp", quiet = TRUE)
boundary  <- st_read("data/mita_boundary.shp",  quiet = TRUE)
mita      <- read_csv("data/peru_mita_districts.csv", show_col_types = FALSE)

# UTM 18S, in meters. Slide 23 is where we work out that this is the zone Dell
# used, and also why matching her is not the same as being accurate.
UTM <- 32718
d18 <- st_transform(districts, UTM)
b18 <- st_transform(boundary,  UTM)

# Dissolve the subject districts into one region. Pulling the id vector out
# first is deliberate: written inline, `mita$ubigeo` is evaluated inside
# filter()'s data mask, and if the shapefile ever carried a column called
# `mita` this would silently return nothing at all.
mita_ids    <- mita$ubigeo[mita$mita == 1]
mita_region <- d18 %>% filter(ubigeo %in% mita_ids) %>% st_union()

stopifnot(sum(d18$ubigeo %in% mita$ubigeo) == nrow(mita),
          length(mita_region) == 1, !st_is_empty(mita_region))

# The sign correction. Slide 19 is where this line gets earned; if you are
# reading ahead, do not skip to it.
pts <- mita %>% mutate(lat = -abs(lat), lon = -abs(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
p18 <- st_transform(pts, UTM)

options(width = 66)


# ---- Slide 6. The line, and who was inside it --------------------------------
par(mar = c(0, 0, 0, 0))
plot(st_geometry(d18), col = "grey95", border = "grey80")
plot(st_geometry(d18[d18$ubigeo %in% mita_ids, ]),
     col = "#b40000", border = "grey70", add = TRUE)
plot(st_geometry(b18), col = "grey15", lwd = 2.2, add = TRUE)
legend("bottomleft", bty = "n", cex = 0.9, fill = c("#b40000", "grey95"),
       legend = c("subject to the mita", "not subject"), border = "grey70")


# ---- Slide 10. Buffers, and the one that goes inward -------------------------
one <- d18[1, ]
c(original = as.numeric(st_area(one)),
  grown    = as.numeric(st_area(st_buffer(one,  5000))),
  shrunk   = as.numeric(st_area(st_buffer(one, -5000)))) / 1e6

# A negative buffer is the interior band. That is how RD samples get drawn, and
# it is the machinery behind the placebo ladder at the end of the evening.
plot(st_geometry(st_buffer(one, 5000)), col = "grey92", border = "grey60")
plot(st_geometry(one), col = "grey80", border = "grey40", add = TRUE)
plot(st_geometry(st_buffer(one, -5000)), col = "#b40000", border = NA, add = TRUE)

# Note these areas come out of UTM, which is conformal rather than equal-area,
# so they are off by a fraction of a percent. At district scale that is
# invisible and we only want the ratio. At continental scale it is Week 1's
# Germany, where the same mistake was a factor of 2.5.


# ---- Slide 11. PREDICT. A centroid is not always in its own polygon ----------
# PREDICT: of 299 Peruvian districts, how many have a centroid that falls
# outside the district itself? Write a number down before you run this.
cen <- st_centroid(st_geometry(d18))
pos <- st_point_on_surface(st_geometry(d18))

cen_inside <- sapply(seq_len(nrow(d18)), function(i)
  st_within(cen[i], st_geometry(d18)[i], sparse = FALSE)[1, 1])
sum(!cen_inside)

# Look at one. It is a district wrapped around a valley.
bad <- which(!cen_inside)[1]
plot(st_geometry(d18[bad, ]), col = "grey92", border = "grey50")
plot(cen[bad], add = TRUE, pch = 4,  cex = 2, lwd = 2, col = "#b40000")
plot(pos[bad], add = TRUE, pch = 20, cex = 2, col = "#2f6fa8")

# Red cross is st_centroid, outside its own polygon. Blue dot is
# st_point_on_surface, which is guaranteed to land inside a VALID polygon.
# Dell sidesteps all of this by using district capitals, which are real towns.
# Build your own points from polygons and you do not.


# ---- Slide 12. Name the predicate --------------------------------------------
c(cap_within  = sum(lengths(st_within(p18, mita_region)) > 0),
  cap_touch   = sum(lengths(st_intersects(p18, mita_region)) > 0),
  dist_within = sum(lengths(st_within(d18, mita_region)) > 0),
  dist_touch  = sum(lengths(st_intersects(d18, mita_region)) > 0))

# On the capitals the two agree, because an interior point is never on the
# line. On the districts they part, and the gap is every polygon that straddles
# the boundary. st_intersects adds the control districts that merely share an
# edge with the region. No district straddles the line, because treatment was
# assigned district by district. And st_within returns 195 where it should
# return 205, because ten mita districts do not test as contained in a dissolve
# of themselves. Floating point, not geography. Hold that until slide 15.
#
# The number on its own never tells you which question you asked, which is why
# the writeup names the predicate.


# ---- Slide 13. The gap is three different things -----------------------------
# The three categories behind the 41, then the picture. .gap/.pat/.odd
# come first because the plot classifies off them.
.ids  <- mita$ubigeo[mita$mita == 1]
.w    <- lengths(st_within(d18, mita_region)) > 0
.i    <- lengths(st_intersects(d18, mita_region)) > 0
.gap  <- d18[.i & !.w, ]
.pat  <- as.character(st_relate(.gap, mita_region))
.pure <- sum(.pat == "FF2F11212")                        # shared edge, zero area
.over <- sum(.pat != "FF2F11212" & !(.gap$ubigeo %in% .ids))   # real overlap
.sliv <- sum(.gap$ubigeo %in% .ids)                      # mita, sub-m2 slivers
.odd  <- .gap[.pat != "FF2F11212" & !(.gap$ubigeo %in% .ids), ]
.oshare <- 100 * as.numeric(st_area(st_intersection(st_geometry(.odd), mita_region))) /
                 as.numeric(st_area(st_geometry(.odd)))

.gi   <- d18$ubigeo %in% .gap$ubigeo[.pat == "FF2F11212"]          # shared edge
.gs   <- d18$ubigeo %in% .gap$ubigeo[.gap$ubigeo %in% .ids]        # sliver failures
.go   <- d18$ubigeo %in% .odd$ubigeo                               # real overlap
par(mar = c(0, 0, 0, 0))
plot(st_geometry(d18), col = "grey96", border = "grey85", lwd = 0.4)
plot(st_geometry(d18[.w, ]),  col = "#e9c4c4", border = "grey70", lwd = 0.4, add = TRUE)
plot(st_geometry(d18[.gi, ]), col = "#2f6fa8", border = "#2f6fa8", lwd = 0.4, add = TRUE)
plot(st_geometry(d18[.gs, ]), col = "#f2a900", border = "#f2a900", lwd = 0.4, add = TRUE)
plot(st_geometry(d18[.go, ]), col = "#15803d", border = "#15803d", lwd = 0.4, add = TRUE)
plot(st_geometry(mita_region), border = "#b40000", lwd = 2.2, add = TRUE)
legend("bottomleft", bty = "o", box.col = "white", bg = "white", cex = 0.72,
       fill = c("#e9c4c4", "#2f6fa8", "#f2a900", "#15803d", "grey96"),
       border = c("grey70", "#2f6fa8", "#f2a900", "#15803d", "grey85"),
       legend = c(paste0("st_within  (", sum(.w), ")"),
                  paste0("shares an edge  (", .pure, ")"),
                  paste0("sliver failure  (", .sliv, ")"),
                  paste0("real overlap  (", .over, ")"),
                  paste0("neither  (", sum(!.i), ")")))


# ---- Slide 14. PREDICT. Clipping, and what it does quietly -------------------
# PREDICT: we clip 299 districts to the western half of the study region. How
# many districts come back? And that is the easy half of the question.
bb  <- st_bbox(d18)
win <- st_as_sfc(st_bbox(c(xmin = unname(bb["xmin"]),
                           ymin = unname(bb["ymin"]),
                           xmax = unname(mean(bb[c("xmin", "xmax")])),
                           ymax = unname(bb["ymax"])), crs = st_crs(d18)))

clipped <- st_intersection(d18, win)
nrow(d18); nrow(clipped)

# sf warns here that attributes are assumed spatially constant. Read it. That
# warning is the whole point of the next slide.


# ---- Slide 15. What the clip actually did -------------------------------------
# The dashed outline is what the cut districts looked like before.
.bb  <- st_bbox(d18)
.win <- st_as_sfc(st_bbox(c(xmin = unname(.bb["xmin"]), ymin = unname(.bb["ymin"]),
                            xmax = unname(mean(.bb[c("xmin", "xmax")])),
                            ymax = unname(.bb["ymax"])), crs = st_crs(d18)))
.cl  <- suppressWarnings(st_intersection(d18, .win))
.o   <- d18   %>% mutate(ao = as.numeric(st_area(.))) %>% st_drop_geometry() %>% select(ubigeo, ao)
.n   <- .cl   %>% mutate(an = as.numeric(st_area(.))) %>% st_drop_geometry() %>% select(ubigeo, an)
.cut <- inner_join(.o, .n, by = "ubigeo") %>% filter(an / ao < 0.999) %>% pull(ubigeo)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(d18), col = "grey97", border = "grey88", lwd = 0.4)
plot(st_geometry(d18[d18$ubigeo %in% .cut, ]), col = "#f2d0d0", border = "#b40000",
     lwd = 0.9, lty = 2, add = TRUE)
plot(st_geometry(.cl[!(.cl$ubigeo %in% .cut), ]), col = "grey78", border = "grey60",
     lwd = 0.4, add = TRUE)
plot(st_geometry(.cl[.cl$ubigeo %in% .cut, ]), col = "#b40000", border = "#b40000",
     lwd = 0.4, add = TRUE)
plot(st_geometry(.win), border = "#2f6fa8", lwd = 2.2, add = TRUE)
legend("bottomleft", bty = "o", box.col = "white", bg = "white", cex = 0.85,
       fill = c("grey78", "#b40000", "#f2d0d0", "grey97"),
       border = c("grey60", "#b40000", "#b40000", "grey88"),
       legend = c(paste0("survived whole  (", nrow(.cl) - length(.cut), ")"),
                  paste0("kept after the cut  (", length(.cut), ")"),
                  "what those districts lost",
                  paste0("dropped  (", nrow(d18) - nrow(.cl), ")")))


# ---- Slide 16. The district that kept its name and lost its land -------------
a1 <- d18     %>% mutate(a1 = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>% select(ubigeo, a1)
a2 <- clipped %>% mutate(a2 = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>% select(ubigeo, a2)

cmp <- inner_join(a1, a2, by = "ubigeo") %>% mutate(kept = a2 / a1)

sum(cmp$kept < 0.999)                 # survived the clip and came out smaller
cmp %>% slice_min(kept, n = 1)        # the worst of them

# It still has its code, its name and its row, and a fraction of its area.
# Anything you compute per square kilometer after this is wrong for that
# district and nothing warned you. The habit, upgraded: around a clip, count
# rows AND compare areas. Week 1 said count rows. Polygons need both halves.


# ---- Slide 17. Simplifying breaks the seams ----------------------------------
s <- st_simplify(d18, dTolerance = 2000)
sum(st_is_empty(s))                                  # districts that vanished

overlaps <- function(x) {              # real overlap, not shared edges
  p <- suppressWarnings(st_intersection(x, x))
  p <- p[p$ubigeo != p$ubigeo.1, ]
  sum(as.numeric(st_area(p)) > 0) / 2
}
c(before = overlaps(d18), after = overlaps(s))

(sum(as.numeric(st_area(d18))) - sum(as.numeric(st_area(s)))) / 1e6

# A district layer is a partition, so the before number ought to be zero.
# Careful with the count: st_intersection returns a record for ANY non-empty
# intersection, and neighbors share edges, so counting records rather than
# positive-area overlaps measures ordinary adjacency.

all(st_is_valid(s[!st_is_empty(s), ]))

# Every survivor is still "valid", because validity is a property of one
# polygon at a time and says nothing about its neighbors. st_simplify(
# preserveTopology = TRUE) does not fix this either, for the same reason. The
# tool that does is rmapshaper::ms_simplify, which we are not installing.


# ======================== BREAK ===============================================


# ---- Slide 18. What simplification did to the seams --------------------------
# One seam, two neighbors thinned separately. Red where the new edges
# cross, orange where they pull apart, all along every border.
.sk <- s[!st_is_empty(s), ]
.zw <- st_as_sfc(st_bbox(c(xmin = 910954 - 26000, ymin = 8361851 - 17000,
                           xmax = 910954 + 26000, ymax = 8361851 + 17000),
                         crs = st_crs(d18)))
.oz <- suppressWarnings(st_intersection(st_geometry(d18),  .zw))
.sz <- suppressWarnings(st_intersection(st_geometry(.sk),  .zw))
.ov <- suppressWarnings(st_intersection(.sk, .sk))
.ov <- .ov[.ov$ubigeo != .ov$ubigeo.1, ]
.ov <- .ov[as.numeric(st_area(.ov)) > 0, ]
.ovz <- suppressWarnings(st_intersection(st_geometry(.ov), .zw))
.gpz <- suppressWarnings(st_intersection(
          st_geometry(st_difference(st_union(d18), st_union(.sk))), .zw))
par(mar = c(0, 0, 0, 0))
plot(st_geometry(.oz), col = "grey94", border = "grey70", lwd = 0.8, reset = FALSE)
plot(.gpz, col = "#f2a900", border = NA, add = TRUE)
plot(.ovz, col = "#b40000", border = NA, add = TRUE)
plot(.sz,  col = NA, border = "#2f6fa8", lwd = 1.8, add = TRUE)
legend("bottomleft", inset = c(0.015, 0.02), bty = "o", box.col = "grey80",
       bg = "white", cex = 0.72,
       fill = c("grey94", "#f2a900", "#b40000", NA),
       border = c("grey70", "#f2a900", "#b40000", NA),
       lty = c(NA, NA, NA, 1), col = c(NA, NA, NA, "#2f6fa8"),
       lwd = c(NA, NA, NA, 1.6), seg.len = 1.2,
       legend = c("districts as drawn", "gap opened", "overlap created",
                  "simplified edge"))


# ---- Slide 22. PREDICT. Before anything else, look at the coordinates --------
# PREDICT: here are the latitudes and longitudes in Dell's file. Peru is at
# about 13 south and 75 west. What is wrong with these?
range(mita$lat)
range(mita$lon)

# Build points from them as they stand and see where the districts land.
oops <- mita %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
st_bbox(oops)

# This is Dell's own file, exactly as she distributes it. It is the ordinary
# condition of archival data.
#
# The fix is at the top of this script. DO NOT rerun that line after slide 22.
# it rebuilds pts from the CSV and drops km, inside and run along the way.


# ---- Slide 23. PREDICT. How far is this district from the mita region? -------
# PREDICT: take a district INSIDE the mita region. How far is it from the mita
# region? Commit to a number.
inside_one <- p18 %>% filter(mita == 1) %>% slice(1)

st_distance(inside_one, mita_region)


# ---- Slide 24. st_boundary() is the whole trick ------------------------------
edge <- st_boundary(mita_region)

as.numeric(st_distance(inside_one, edge)) / 1000     # kilometers

# A polygon contains its own interior, so the distance from a point inside it
# to the polygon is zero and nothing errors. st_boundary() casts the polygon to
# its outline, and distance to THAT is the number you wanted. One function, and
# now you have something you can regress on.
#
# In practice you use the boundary file Dell ships, because the region's own
# outer edge includes coastline and the far frontier, which are not the
# discontinuity. That choice turns out to matter; see slide 33.


# ---- Slide 25. Distance, and then a sign -------------------------------------
pts$km     <- as.numeric(st_distance(p18, st_union(b18))) / 1000
pts$inside <- lengths(st_within(p18, mita_region)) > 0
pts$run    <- ifelse(pts$inside, pts$km, -pts$km)

sum(pts$inside == (mita$mita == 1))      # does st_within recover her dummy?

# Read what 299 of 299 establishes, because it is less than it looks. Look
# back at how mita_region was built, by dissolving the districts her dummy
# marks. So this cannot be an independent reconstruction of treatment. What it
# checks is that every district capital sits inside its own polygon, which is
# what lets a point-in-polygon test stand in for a district-level dummy at all.
# Slide 11, where three centroids fell outside theirs, is why that needed
# checking. The distance is the genuinely independent half; the side is not.


# ---- Slide 26. Now check it against hers -------------------------------------
cor(pts$km, mita$dell_d_bnd)
max(abs(pts$km - mita$dell_d_bnd)) * 1000            # meters

# Her file ships d_bnd and we did not look at it until now. Rebuilding a
# published variable from the raw geometry and comparing is the whole method of
# this course in one line.

# The zone matters. The same thing again in UTM 19S rather than 18S:
p19 <- as.numeric(st_distance(st_transform(pts, 32719),
                              st_union(st_transform(boundary, 32719)))) / 1000
max(abs(p19 - mita$dell_d_bnd)) * 1000

# That is how you work out which projection someone used. You try them.
#
# But matching her does not make 18S correct. Zone 18S runs 78 to 72 degrees
# west, and a good share of these capitals sit east of that, where the scale
# factor runs a few tenths of a percent high. Careful with the comparison.
# These longitudes are still stored unsigned, so east of 72 west is lon < 72.
sum(mita$lon < 72)

# Reproduction and accuracy are different goals. Tonight we wanted reproduction.


# ---- Slide 27. The running variable, drawn -----------------------------------
pal <- colorRampPalette(c("#2f6fa8", "grey92", "#b40000"))(100)
rv  <- pts$run                        # same quantity, named for the plot
idx <- cut(rv, breaks = 100, labels = FALSE)

par(mar = c(0, 0, 0, 0))
plot(st_geometry(d18), col = "grey96", border = "grey85")
plot(st_geometry(p18), add = TRUE, pch = 20, cex = 0.9, col = pal[idx])
plot(st_geometry(b18), col = "grey15", lwd = 2, add = TRUE)
legend("bottomleft", bty = "n", cex = 0.85, pch = 20,
       col = pal[c(5, 50, 95)], title = "signed distance",
       legend = c("outside", "at the line", "inside"))


# ---- Slide 30. Look before you estimate --------------------------------------
dd <- pts %>% st_drop_geometry() %>% filter(cusco != 1, !is.na(stunted))
c(before = nrow(pts), after = nrow(dd))              # count both sides

# Dell drops Cusco for its relative prosperity today, which traces to its
# pre-mita past as the Inca capital. Her own note records that including it
# makes the estimated mita effect LARGER, so the restriction runs against her
# own result. A sample restriction you can defend is worth more than one you
# can hide, and this is why the count goes on the screen.

bins <- dd %>% filter(abs(run) <= 75) %>%
  mutate(b = cut(run, breaks = seq(-75, 75, by = 10))) %>%
  group_by(b) %>%
  summarize(x = mean(run), y = weighted.mean(stunted, n_children),
            n = sum(n_children), .groups = "drop")

par(mar = c(4, 4, 0.5, 0.5))
plot(bins$x, bins$y, pch = 21, bg = ifelse(bins$x > 0, "#b40000", "#2f6fa8"),
     cex = 1 + 1.6 * bins$n / max(bins$n),
     xlab = "km from the mita boundary", ylab = "share of children stunted",
     ylim = range(bins$y) + c(-0.02, 0.02))
abline(v = 0, lty = 2, col = "grey40")

RS <- dd %>% filter(run > 0, run <= 75)
LS <- dd %>% filter(run < 0, run >= -75)
lines(sort(RS$run),
      fitted(lm(stunted ~ poly(run, 2), RS, weights = n_children))[order(RS$run)],
      col = "#b40000", lwd = 2)
lines(sort(LS$run),
      fitted(lm(stunted ~ poly(run, 2), LS, weights = n_children))[order(LS$run)],
      col = "#2f6fa8", lwd = 2)

# This picture is raw and unconditional, binned at 75 km. The estimate in the
# next block is conditional and runs at 100 km, so do not expect the visible
# jump here to equal the coefficient there.


# ---- Slide 31. The estimate --------------------------------------------------
est <- dd %>% mutate(s = km / 100, s2 = s^2, s3 = s^3) %>% filter(km < 100)

m <- lm(stunted ~ mita + s + s2 + s3 + elevation + slope +
          bfe4_1 + bfe4_2 + bfe4_3, data = est, weights = n_children)

round(coeftest(m, vcov. = vcovHC(m, type = "HC1"))["mita", ], 3)

# Compare this against Dell's Table II, Panel C, column 4. Coefficient,
# standard error, cluster count and number of children, out of a shapefile and
# nine lines of R.
#
# The estimand is local. It compares districts just inside the line with
# districts just outside, and says nothing about a district deep inside the
# catchment. bfe4_1 to bfe4_3 are the boundary-segment fixed effects, three
# dummies for four segments, and they are there because the effect need not be
# the same along every stretch of the line.
#
# The polynomial is in km, the unsigned distance, which is Dell's
# specification. That constrains the trend to be symmetric in the two
# directions away from the line, and the dummy carries the level shift. Putting
# the SIGNED variable in is the textbook RD form instead, and imposes a common
# slope across the line. A different restriction.
# Try it and see how little moves:
#
#   lm(stunted ~ mita + I(run/100) + I((run/100)^2) + I((run/100)^3) +
#        elevation + slope + bfe4_1 + bfe4_2 + bfe4_3,
#      data = est, weights = n_children)
#
# These are HC1 on district means. Dell's Table II reports robust standard
# errors clustered by district, which at one row per district is the same
# estimator, and that is why the standard error matches too. Conley spatial
# standard errors appear in her Table I, the balance table, and nowhere else.
# Spatially robust inference is the Week 9 digression.


# ---- Slide 32. PREDICT. The choice nobody writes down ------------------------
# PREDICT: the same regression on district averages, rather than weighted by
# how many children each district contributed. Bigger, smaller, or the same?
mu <- lm(stunted ~ mita + s + s2 + s3 + elevation + slope +
           bfe4_1 + bfe4_2 + bfe4_3, data = est)      # no weights

c(weighted = unname(coef(m)["mita"]), unweighted = unname(coef(mu)["mita"]))

# Weighting each district average by its number of children reproduces Dell,
# who runs it on the children. Unweighted, a district of two hundred children
# counts as much as one of nine thousand. That is a claim about what you are
# estimating, and nothing in the equation says which.
# Narrow the window to 50 km and the same contrast gets wider still.


# ---- Slide 33. How wide is the window? ---------------------------------------
band <- function(cutoff) {
  sub <- est %>% filter(km < cutoff)
  mm <- lm(stunted ~ mita + s + s2 + s3 + elevation + slope +
             bfe4_1 + bfe4_2 + bfe4_3, data = sub, weights = n_children)
  cc <- coeftest(mm, vcov. = vcovHC(mm, type = "HC1"))["mita", ]
  c(km = cutoff, estimate = unname(cc[1]), se = unname(cc[2]),
    districts = nobs(mm))
}
round(t(sapply(c(100, 75, 50), band)), 3)

# These are Dell's Table II Panel C columns 4, 5 and 6. The estimate is stable,
# but each row is a different sample, so these are not nested comparisons of
# one population. The bandwidth is an assumption about where the comparison is
# credible, and it belongs in the paper rather than in a footnote.


# ---- Slide 34. PREDICT. Does geography jump too? -----------------------------
# PREDICT: run the same RD with elevation on the left, then slope. If the
# design is clean both should be flat at the boundary. Are they?
bal <- function(v) {
  mm <- lm(as.formula(paste(v, "~ mita + s + s2 + s3 + bfe4_1 + bfe4_2 + bfe4_3")),
           data = est, weights = n_children)
  cc <- coeftest(mm, vcov. = vcovHC(mm, type = "HC1"))["mita", ]
  c(estimate = unname(cc[1]), se = unname(cc[2]), p = unname(cc[4]))
}
round(rbind(elevation = bal("elevation"), slope = bal("slope")), 3)

# Units matter for reading these. Elevation is in thousands of meters and slope
# is in degrees, against a mean slope of about 8.
mean(est$slope)

# These are HC1 again. Dell reports Conley spatial standard errors for exactly
# these two variables, roughly twice as wide, and on that basis calls elevation
# statistically identical across the boundary.
#
# A predetermined covariate that
# jumps at the cutoff is evidence AGAINST continuity. Putting it on the
# right-hand side assumes a functional form for that imbalance rather than
# restoring the design.


# ---- Slide 35. PREDICT. Move the line, and the effect should die -------------
# PREDICT: we shift the boundary 25 km inward with the negative buffer from the
# start of the evening and rerun everything at the fake line. What should
# happen if the design is real?
#
# Nothing prints from this block. It only defines the function; the answer is
# in the next one.
rd_at <- function(region) {
  km_new <- as.numeric(st_distance(p18, st_boundary(region))) / 1000
  t_new  <- as.integer(lengths(st_within(p18, region)) > 0)
  # New names on purpose. Write km = km[...] inside mutate() and the km on the
  # right is dd's OLD column, not this function's vector. That is dplyr data
  # masking, and the wrong version still "fades" convincingly while quietly
  # reusing the true running variable.
  d <- dd %>% mutate(treat = t_new[match(ubigeo, pts$ubigeo)],
                     km    = km_new[match(ubigeo, pts$ubigeo)],
                     s = km / 100, s2 = s^2, s3 = s^3) %>%
    filter(km < 100)
  mm <- lm(stunted ~ treat + s + s2 + s3 + elevation + slope +
             bfe4_1 + bfe4_2 + bfe4_3, data = d, weights = n_children)
  coeftest(mm, vcov. = vcovHC(mm, type = "HC1"))["treat", 1:2]
}


# ---- Slide 36. Where the fake lines actually go ------------------------------
# Each fake line is a buffer of the real one, so every treated set is
# nested in the one outside it. Watch the treated count collapse.
.nt <- function(r) sum(lengths(st_within(p18, r)) > 0)
.i25 <- st_buffer(mita_region, -25000)
.o25 <- st_buffer(mita_region,  25000)
.i50 <- st_buffer(mita_region, -50000)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(d18), col = "grey96", border = "grey88", lwd = 0.3, reset = FALSE)
plot(mita_region, col = "#f2d0d0", border = NA, add = TRUE)
plot(st_boundary(.o25),        col = "#2f6fa8", lwd = 2.0, add = TRUE)
plot(st_boundary(mita_region), col = "#b40000", lwd = 2.4, add = TRUE)
plot(st_boundary(.i25),        col = "#f2a900", lwd = 2.0, add = TRUE)
plot(st_boundary(.i50),        col = "#15803d", lwd = 2.0, add = TRUE)
legend("bottomleft", inset = c(0.01, 0.02), bty = "o", box.col = "grey80",
       bg = "white", cex = 0.75, lty = 1, lwd = 2.2,
       col = c("#2f6fa8", "#b40000", "#f2a900", "#15803d"),
       legend = c(paste0("25 km outward   treated ", .nt(.o25)),
                  paste0("the true line   treated ", .nt(mita_region)),
                  paste0("25 km inward    treated ", .nt(.i25)),
                  paste0("50 km inward    treated ", .nt(.i50))))


# ---- Slide 37. The placebo ladder --------------------------------------------
round(rbind(`true boundary` = rd_at(mita_region),
            `25 km inward`  = rd_at(st_buffer(mita_region, -25000)),
            `25 km outward` = rd_at(st_buffer(mita_region,  25000)),
            `50 km inward`  = rd_at(st_buffer(mita_region, -50000))), 3)

# Read this carefully. A buffer drawn inside the real
# region gives a treated set that is NESTED in the true one, so attenuation is
# what both the null and the alternative predict. This is a dose-response
# check rather than a falsification test. The falsification you want moves
# the line sideways, onto a stretch of the Andes the mita never touched.
#
# Note also that the true-boundary row here does not equal the estimate from
# slide 28. This ladder measures distance to the REGION's own outer edge, which
# includes the frontier, whereas slide 28 used Dell's boundary file. Two
# defensible edges, two different answers, nothing else changed. Which edge
# you mean is never a detail. Write it down before you run the regression.


# ---- Slide 38. Three ways to write f(.) --------------------------------------
sp <- function(f) {
  mm <- lm(f, data = est, weights = n_children)
  cc <- coeftest(mm, vcov. = vcovHC(mm, type = "HC1"))["mita", ]
  c(estimate = unname(cc[1]), se = unname(cc[2]))
}
ctrl <- "+ elevation + slope + bfe4_1 + bfe4_2 + bfe4_3"

rbind(
  `cubic in distance` = sp(as.formula(paste("stunted ~ mita + s + s2 + s3", ctrl))),
  `linear in distance` = sp(as.formula(paste("stunted ~ mita + s", ctrl))),
  `cubic in lat and lon` = sp(as.formula(paste(
      "stunted ~ mita + lon + lat + I(lon*lat) +",
      "I(lon^2) + I(lat^2) + I(lon^3) + I(lat^3) +",
      "I(lon^2*lat) + I(lon*lat^2)", ctrl))),
  `linear own slopes` = sp(as.formula(paste("stunted ~ mita*s", ctrl))),
  `quadratic own slopes` = sp(as.formula(paste("stunted ~ mita*(s + s2)", ctrl)))) %>%
  round(3)

# The last two rows are the modern shape. Interacting the polynomial with the
# dummy lets the trend have its own slope on each side of the line, which is
# what local linear RD does by default. Raising the ORDER of the polynomial
# does nothing here, 0.072 to 0.073 from linear to cubic. Letting the SLOPE
# differ doubles the standard error and takes the result out of significance.
#
# Every one of these five rows is in Dell's own Table III, column 4, and ours
# match hers to the third decimal. Gelman and Imbens (2019, JBES) argue global
# high-order polynomials should not be used in RD at all, because the weight
# they put on the cutoff is driven by observations far away from it. Her paper
# predates that consensus, which is the ordinary condition of a 2010 design
# rather than a defect in it. The tool the literature uses now is rdrobust,
# and we are not installing it tonight.

# The third row reproduces Dell's footnote 15 exactly, nine terms including
# the two cross terms that are easy to leave out. Drop I(lon^2*lat) and
# I(lon*lat^2) and you get a different answer from hers.
#
# The standard error nearly doubles on that row, and degrees of freedom do not
# explain it. Three terms to ten costs six residual df, about one percent. The
# catchment is a region in lon/lat, so a flexible enough surface traces the
# boundary and absorbs the discontinuity itself. Overfitting at the cutoff.
#
# Note whose preference is whose. Dell calls the multidimensional polynomial
# preferable where power allows it, and calls distance to the boundary the
# least informative on its own, because nothing historical says that distance
# matters. We led with the one tonight's geometry builds. Her third f is a
# cubic in distance to Potosi, which this extract cannot run.


# ---- Slide 39. So do you believe it? -----------------------------------------
# No code on this one. The verdict frame. What survived tonight, what did not,
# and the test that would settle it: move the line sideways, onto a stretch of
# the Andes the mita never touched, and run the same regression. That is four
# lines of the geometry in this file.


# ---- Slide 42. Boundaries that move ------------------------------------------
a1340 <- st_read("data/abramson_states_1340.shp", quiet = TRUE) %>% st_make_valid()
a1425 <- st_read("data/abramson_states_1425.shp", quiet = TRUE) %>% st_make_valid()

# These arrive in an equal-area conic with no EPSG code, which is why st_area
# and st_sym_difference are trustworthy on them and would not be in Web
# Mercator. Check before you measure, every time:
st_crs(a1340)$units
st_crs(a1340)$epsg          # NA here means unregistered, and the file is complete.

fr1 <- a1340 %>% filter(Name == "France") %>% st_union()
fr2 <- a1425 %>% filter(Name == "France") %>% st_union()

churn <- as.numeric(st_area(st_sym_difference(fr1, fr2)))
share <- churn / max(as.numeric(st_area(fr1)), as.numeric(st_area(fr2)))
share

par(mar = c(0, 0, 1.2, 0), mfrow = c(1, 2))
bbf <- st_bbox(st_union(fr1, fr2))
plot(st_geometry(a1340), col = "grey96", border = "grey75",
     xlim = bbf[c(1, 3)], ylim = bbf[c(2, 4)], main = "1340")
plot(st_geometry(fr1), col = "#b4000055", border = "#b40000", add = TRUE)
plot(st_geometry(a1425), col = "grey96", border = "grey75",
     xlim = bbf[c(1, 3)], ylim = bbf[c(2, 4)], main = "1425")
plot(st_geometry(fr2), col = "#2f6fa855", border = "#2f6fa8", add = TRUE)
par(mfrow = c(1, 1))

# France across the first phase of the Hundred Years' War, measured with one
# geometry verb. The mita line moved zero percent in 239 years. That contrast
# is the identification argument. Every boundary is cheap to compute and
# expensive to defend, and a line that moves, follows rivers and mountains, and
# sorts people for centuries will not carry a discontinuity design.


# ---- Slide 43. When the boundary is the outcome -------------------------------
# Here the boundary is the dependent variable.
g <- st_make_grid(a1340, cellsize = 200000) %>% st_as_sf() %>%
  mutate(cell = row_number())
g$n1340 <- lengths(st_intersects(g, a1340))
g$n1425 <- lengths(st_intersects(g, a1425))

land <- g %>% filter(n1340 > 0 | n1425 > 0)
table(sign(land$n1425 - land$n1340))

# Printed in the order -1, 0, 1: consolidated, unchanged, fragmented.
#
# 200 km is a choice and the tally moves with it. Halve the cell and you get a
# different table, which is the modifiable areal unit problem wearing its
# research-design hat. Anything built on an aggregation you chose owes the
# reader the same table at another resolution.
#
# Grids return next week as rasters, and interpolating a mortality surface is
# Week 8.


# ---- Before next week --------------------------------------------------------
# 1. Read Nunn and Qian (2011) for Week 4, rasters and difference-in-differences.
# 2. Work this script by hand. Rebuild the running variable from the shapefile
#    without looking at the slides. That is three lines and it is the exam.
# 3. The lab exam in Week 6 is closed book and closed model.


# ---- Slide 44. The outcome, drawn --------------------------------------------
# The dependent variable, one cell at a time. 200 km is a choice, and the
# table moves with it. That is the modifiable areal unit problem.
.chg <- sign(land$n1425 - land$n1340)
.pal <- c("-1" = "#b40000", "0" = "grey88", "1" = "#2f6fa8")
par(mar = c(0, 0, 0, 0))
plot(st_geometry(land), col = .pal[as.character(.chg)], border = "white",
     lwd = 0.5, reset = FALSE)
plot(st_geometry(st_union(a1340)), col = NA, border = "grey35", lwd = 0.9, add = TRUE)
legend("topright", inset = c(0.02, 0.03), bty = "o", box.col = "grey80",
       bg = "white", cex = 0.78,
       fill = c("#b40000", "grey88", "#2f6fa8"), border = "white",
       legend = c(paste0("consolidated  (", sum(.chg == -1), ")"),
                  paste0("unchanged  (",    sum(.chg ==  0), ")"),
                  paste0("fragmented  (",   sum(.chg ==  1), ")")))
