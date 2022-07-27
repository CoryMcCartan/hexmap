library(sf)
library(rmapshaper)
library(dplyr)

url <- "https://projects.fivethirtyeight.com/redistricting-2022-maps/approved-districts-us.json"
path <- "data-raw/districts_2020_538.json"
download.file(url, path)

d_2020 = read_sf(path) |>
    st_make_valid()

d_usa = tigris::states(cb=TRUE, resolution="20m", year=2020) |>
    filter(STUSPS != "PR")

geom_usa = d_usa |>
    filter(STUSPS != "AK", STUSPS != "HI") |>
    arrange(STUSPS) |>
    st_transform(5070) |>
    pull(geometry) |>
    st_convex_hull() |>
    st_cast("MULTILINESTRING") |>
    st_cast("LINESTRING")

geom_usa_538 = d_2020 |>
    st_buffer(1) |>
    filter(state != "AK", state != "HI") |>
    arrange(state) |>
    group_by(state) |>
    summarize() |>
    pull(geometry) |>
    st_buffer(-1) |>
    st_convex_hull() |>
    st_cast("MULTILINESTRING") |>
    st_cast("LINESTRING")

st_crs(geom_usa_538) = 5070
ctr_538 = st_centroid(st_union(geom_usa_538))

x1 = st_coordinates(st_centroid(geom_usa))[, 1:2]
x2 = st_coordinates(st_centroid(geom_usa_538))[, 1:2]
m = coef(lm(x1 ~ x2))

d_2020$geometry = d_2020$geometry * m[-1, ] + m[1, ]

d_2020 = d_2020 |>
    select(-id) |>
    st_set_crs(5070)

saveRDS(d_2020, "data-raw/districts_2020_538.rds", compress="xz")
