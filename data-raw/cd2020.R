library(sf)
library(tidyverse)
library(tigris)
library(alarmdata)

maps = map(state.abb, alarm_50state_map)
names(maps) = state.abb

d_usa = nation(year=2020)

d_2020 = imap_dfr(maps, function(m, abbr) {
    cat(abbr, "\n")
    as_tibble(m) |>
        relocate(any_of(c("GEOID", "state", "county", "cd_2010", "cd_2020")),
                 .before=everything()) |>
        st_as_sf() |>
        st_make_valid() |>
        select(cd_2020, pop:vap_two, any_of(c(
            "pre_16_dem_cli", "pre_16_rep_tru", "pre_20_dem_bid", "pre_20_rep_tru",
            "arv_16", "adv_16", "arv_18", "adv_18", "arv_20", "adv_20", "nrv", "ndv")),
            geometry) |>
        group_by(cd_2020) %>%
        summarize(across(pop:ndv, sum),
                  is_coverage = (abbr != "FL")) |>
        st_transform(4269) |>
        st_intersection(d_usa$geometry) |>
        suppressWarnings() |>
        mutate(state = abbr,
               .before=cd_2020)
}) |>
    arrange(state, cd_2020)

shift_geometry(d_2020) |>
ggplot(aes(fill=pre_16_dem_cli / (pre_16_dem_cli + pre_16_rep_tru))) +
    geom_sf(lwd=0.15, color='black') +
    ggredist::scale_fill_party_b(name="2016 President",
                                 limits=c(0.3, 0.7), na.value="black") +
    ggredist::theme_map() +
    theme(legend.position=c(0.1, 0.75))

saveRDS(d_2020, "data-out/districts_2020_alarm_full.rds", compress="xz")


