devtools::load_all(".")
library(ggplot2)

d_2020 = readRDS("data-raw/districts_2020_538.rds")
d_usa = tigris::states(cb=TRUE, resolution="20m", year=2020) |>
    dplyr::filter(!STUSPS %in% c("PR", "DC")) |>
    tigris::shift_geometry()

# Make hexmap for a state -----
hex_per_distr = 5
STATE = "MA"

d_state = dplyr::filter(d_2020, state == STATE) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(district = as.integer(district))
shp = d_2020$geometry[d_2020$state == STATE]
if (STATE %in% c("CA", "NY")) {
    # shp_raw = alarmdata::alarm_50state_map(STATE)
    shp_raw = tigris::congressional_districts(STATE, cb=TRUE, year=2020)
    shp = dplyr::as_tibble(shp_raw) |>
        sf::st_as_sf() |>
        dplyr::group_by(CD116FP) |>
        dplyr::summarize() |>
        dplyr::pull(geometry)
}
outline = d_usa$geometry[d_usa$STUSPS == STATE]

res = make_hex_grid(shp, outline, hex_per_district=hex_per_distr)

out = place_districts(res)

dplyr::left_join(out, d_state, by="district") |>
    ggplot(aes(fill=0.5+pvi/200)) +
    geom_sf(color="black") +
    # geom_sf_text(aes(label=paste0(STATE, "-", district), geometry=geom_label),
    geom_sf_text(aes(label=district, geometry=geom_label),
                 data=out, inherit.aes=FALSE,
                 size=5, color="#00000077", fontface="bold") +
    ggredist::scale_fill_party_c(limits=c(0.3, 0.7)) +
    guides(fill="none") +
    ggredist::theme_map()

# Position and rescale states -----

rescale = 1e6
geom_usa = sf::st_union(d_usa$geometry) |>
    sf::st_transform(5070)
ctr_usa = sf::st_centroid(geom_usa)
{
d_states = dplyr::select(d_usa, state=STUSPS) |>
    dplyr::left_join(dplyr::count(dplyr::as_tibble(d_2020), state), by="state") |>
    sf::st_transform(5070)
d_states$area = sf::st_area(d_states)
d_states$area = 435 * as.numeric(d_states$area / sum(d_states$area))
coords = sf::st_coordinates(sf::st_centroid(d_states$geometry)) / rescale

ctr_state = sf::st_centroid(d_states$geometry)
# dist_edge = sf::st_nearest_points(ctr_state, sf::st_cast(geom_usa, "MULTILINESTRING")) |>
#     sf::st_length() |>
#     as.numeric()
# dist_edge = 1 + 0.5 * exp(-1 * dist_edge / max(dist_edge))
# ctr_state = (ctr_state - ctr_usa) * dist_edge + ctr_usa
# sf::st_crs(ctr_state) = 5070
# ctr_state = sf::st_nearest_points(ctr_state, d_states$geometry, pairwise=TRUE) |>
#     sf::st_cast("POINT")
# ctr_state = (sf::st_centroid(d_states$geometry) + ctr_state[seq(2, length(ctr_state), 2)]) * 0.5
}

m_scale = with(d_states, sqrt(n / area))
# m_scale = m_scale / max(m_scale)
m_scale = m_scale * 0.7
for (i in seq_len(nrow(d_states))) {
    geom_i = d_states$geometry[i]
    d_states$geometry[i] = (geom_i - ctr_state[i]) * m_scale[i] + ctr_state[i]
}

{
plot(geom_usa)
# plot(sf::st_transform(d_usa$geometry, 5070))
plot(d_states['n'], add=T)
# plot(ctr_state, cex=dist_edge, add=T)
}

area_adj = with(d_states, sqrt(n / area))
adj = sf::st_buffer(d_usa, 50e3) |>
    geomander::adjacency() |>
    lapply(\(x) x + 1L)
m_adj = 0 * diag(nrow(coords))
for (i in seq_len(nrow(coords))) {
    m_adj[i, adj[[i]]] = 1
    m_adj[adj[[i]], i] = 1
}
areas = as.numeric(sqrt(sf::st_area(sf::st_convex_hull(d_states)))/1e6)
pair_area = outer(areas, areas, "+")
dx0 = outer(coords[, 1], coords[, 1], "-")
dy0 = outer(coords[, 2], coords[, 2], "-")
angle0 = atan2(dy0, dx0)

plot(sf::st_transform(d_usa$geometry, 5070))
plot(ctr_state, cex=8*areas, add=T)
plot(coords, cex=8*areas)
# plot(d_us$geometry)

{
k = list(spring = 1.0 / mean(lengths(adj)) / mean(abs(log(area_adj))),
         loc = 0.25,
         ctr = 0.00,
         rel = 2.50,
         repel = 0.000 / sum(area_adj))

energy = function(x) {
    x = coords + matrix(x, ncol=2)
    dm = as.matrix(dist(x))
    dx = outer(x[, 1], x[, 1], "-")
    dy = outer(x[, 2], x[, 2], "-")
    adj_dist = dm - 0.5*pair_area
    adj_dist[adj_dist < 0.1] = 0.1

    ctr_dist = -0.25 + (dm - 0.5*pair_area)
    e_attract = 0.5 * k$spring * rowSums(m_adj * ctr_dist^2) * abs(log(area_adj))
    e_loc = k$loc * rowSums((x - coords)^2)
    e_ctr = k$ctr * mean(matrixStats::rowMaxs(dm))
    # e_rel = k$rel * rowSums(pnorm(-dx*dx0) + pnorm(-dy*dy0))
    angle_diff = (pi + atan2(dy, dx) - angle0) %% (2*pi) - pi
    e_rel = k$rel * rowSums(angle_diff^2)
    e_repel = k$repel * rowSums(area_adj / adj_dist^3)
    e_attract + e_rel + e_loc + e_ctr + e_repel
}

opt = optim(rep(0, length(coords)), \(x) mean(energy(x)),
            method="L-BFGS-B", control=list(factr=1e10))
shift = matrix(opt$par, ncol=2)

geom2 = d_states$geometry
for (i in seq_along(geom2)) {
    geom2[i] = geom2[i] + shift[i, ]*rescale
}
ggplot(d_states, aes(fill=energy(shift), geometry=geom2)) + geom_sf(lwd=0.4) + theme_void()
}


ggplot(d_states, aes(fill=energy(shift))) + geom_sf(lwd=0.4) + theme_void()
ggplot(d_states, aes(fill=abs(log(area_adj)))) + geom_sf(lwd=0.4) + theme_void()
