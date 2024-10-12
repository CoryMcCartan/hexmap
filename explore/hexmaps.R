devtools::load_all(".")
library(ggplot2)
library(ggredist)

d_2020 = readRDS("data-out/districts_2020_alarm.rds")
d_usa = tigris::states(cb=TRUE, resolution="20m", year=2020) |>
    dplyr::filter(!STUSPS %in% c("PR", "DC"))

# Make state hexmaps ----------
state_hex = list()
for (abbr in setdiff(state.abb, names(state_hex))) {
    state_hex[[abbr]] = make_hex_map(abbr, d_2020, d_usa)
}

state_hex$IN |>
    sf::st_transform(4269) |>
    dplyr::mutate(
        disp = sf::st_distance(
            sf::st_centroid(geometry),
            sf::st_centroid(d_2020$geometry[d_2020$state == state[1]]),
            by_element=TRUE),
        disp = as.numeric(disp) / 1609) |>
ggplot(aes(fill=disp)) +
    geom_sf(lwd=0.3, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district),
                 fontface="bold", size=3.0) +
    wacolors::scale_fill_wa_c("puget") +
    theme_map()

saveRDS(state_hex, "data-raw/hex1.rds", compress="gzip")
# state_hex = readRDS("data-raw/hex1.rds")

state_hex = dplyr::bind_rows(state_hex)

shifted_labels = state_hex |>
    sf::st_set_geometry("geom_label") |>
    sf::st_transform(4269) |>
    tigris::shift_geometry() |>
    dplyr::pull(geom_label)

state_hex = state_hex |>
    sf::st_transform(4269) |>
    tigris::shift_geometry() |>
    dplyr::mutate(geom_label = shifted_labels) |>
    dplyr::left_join(sf::st_drop_geometry(d_2020),
                     by=c("state", "district"="cd_2020"))


# Position states -----

## calculate rescaling -------
d_states = state_hex |>
    dplyr::group_by(state) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::mutate(ctr = sf::st_centroid(geometry),
                  area = sf::st_area(geometry),
                  rescale = 435 * as.numeric(area / sum(area)))
hex_scale = sqrt(median(as.numeric(d_states$area)))
scale_fac = with(d_states, sqrt(n / rescale))
names(scale_fac) = d_states$state

## load DK map and rescale ----------
{
dk_states = sf::read_sf("data-raw/HexSTv20/HexSTv20.shp") |>
    dplyr::select(state=STATEAB) |>
    sf::st_set_crs(3857) |>
    suppressWarnings() |>
    dplyr::mutate(area = as.numeric(sf::st_area(geometry)))

dk_scale = sqrt(median(dk_states$area))
m_scale = diag(2) / dk_scale
dk_ctr = sf::st_centroid(sf::st_union(dk_states$geometry))
dk_states$area = dk_states$area * m_scale[1, 1]
dk_states$geometry = sf::st_centroid((dk_states$geometry - dk_ctr) * m_scale)

# adjustments
idx = which(dk_states$state == "DE")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(-0.02, 0.00)
idx = which(dk_states$state == "NJ")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(-0.09, 0.00)
idx = which(dk_states$state == "NY")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.08, -0.05)
idx = which(dk_states$state %in% c("CT", "RI"))
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.00, 0.02)
idx = which(dk_states$state == "MT")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.00, -0.03)
idx = which(dk_states$state == "OR")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.13, -0.01)
idx = which(dk_states$state == "WA")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.10, 0.03)
idx = which(dk_states$state %in% c("NJ", "NY", "CT", "MA", "RI", "VT", "NH", "ME"))
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.06, -0.14)
idx = which(dk_states$state == "FL")
dk_states$geometry[idx] = dk_states$geometry[idx] + c(0.00, -0.07)
}

## scale and move -------
state_hex2 = state_hex
for (i in seq_len(nrow(d_states))) {
    abbr = d_states$state[i]
    idx = which(state_hex2$state == abbr)
    sc = scale_fac[d_states$state[i]]
    # sc = 0.8
    new_ctr = dk_states$geometry[dk_states$state == abbr] * (diag(2) * hex_scale * 0.88)
    state_hex2$geometry[idx] = (state_hex2$geometry[idx] - d_states$ctr[i]) * sc + new_ctr
    state_hex2$geom_label[idx] = (state_hex2$geom_label[idx] - d_states$ctr[i]) * sc + new_ctr
    if (abbr == "AK") {
        state_hex2$geometry[idx] = rmapshaper::ms_simplify(state_hex2$geometry[idx], 0.02)
    }
}
plot(state_hex2$geometry)

saveRDS(state_hex2, "data-out/districts_2020_hex.rds", compress="gzip")

d_sum = d_2020 |>
    dplyr::as_tibble() |>
    dplyr::transmute(state=state,
                     district=cd_2020,
                     area = as.numeric(sf::st_area(sf::st_make_valid(geometry))) / 1609^2,
                     dens = pop / area)

state_hex2 |>
    dplyr::left_join(d_sum, by=c("state", "district")) |>
    dplyr::mutate(dem_16 = pre_16_dem_cli / (pre_16_dem_cli + pre_16_rep_tru),
                  dem_18 = adv_18 / (adv_18 + arv_18),
                  dem_20 = pre_20_dem_bid / (pre_20_dem_bid + pre_20_rep_tru),
                  dem = ndv / (ndv + nrv)) |>
# ggplot(aes(fill=dens)) +
ggplot(aes(fill=dem)) +
# ggplot(aes(fill=vap_aian/vap)) +
# ggplot(aes(fill=dem_20-dem_16)) +
    geom_sf(lwd=0.3, color="black") +
    # geom_sf_text(aes(geometry=geom_label, label=district),
                 # fontface="bold", size=2.4, color="#ffffff88") +
    # wacolors::scale_fill_wa_c(trans="sqrt") +
    # scale_fill_party_b(limits=c(0.35, 0.65)) +
    # scale_fill_party_c(midpoint=0, limits=c(-0.1, 0.1), labels=scales::percent) +
    scale_fill_party_c(limits=c(0.3, 0.7), labels=scales::percent) +
    # wacolors::scale_fill_wa_b("ferries", which=1:15, reverse=T, trans="log10",
    #                           name="People / sq. mi.",
    #                           breaks=round(0.1 * 10^seq(1.6, 4, length.out=4))*10,
    #                           labels=\(x) scales::comma(x, 1), oob=scales::squish) +
    theme_map() +
    theme(legend.position=c(0.9, 0.4))
