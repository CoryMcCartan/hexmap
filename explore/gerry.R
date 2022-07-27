library(tidyverse)
library(redist)
library(ggredist)
library(alarmdata)

# Load maps and plans --------

if (!file.exists(path <- "~/Desktop/maps.rds")) {
    maps = map(state.abb, alarm_50state_map)
    names(maps) = state.abb
    write_rds(maps, path, compress="xz")
} else {
    maps = read_rds(path)
}

plans$AR = plans$AR |>
    subset_sampled() |>
    alarm_add_plan(maps$AR$cd_2020, plans=_, map=maps$AR, name="cd_2020") |>
    mutate(draw = fct_inorder(draw))


if (!file.exists(path <- "~/Desktop/plans.rds")) {
    plans = map(state.abb, alarm_50state_plans, stats=TRUE)
    names(plans) = state.abb
    write_rds(plans, path, compress="gz")
} else {
    plans = read_rds(path)
}

hex = read_rds("data-out/districts_2020_hex.rds")

# Partisan gerrymandering --------

d_gerry = map_dfr(names(plans), function(abbr) {
    cat(abbr, "\n")
    if (mean(is.na(plans[[abbr]]$pr_dem))) {
        x = alarm_50state_stats(abbr)
        plans[[abbr]]$pr_dem = x$pr_dem
    }
    pr_samp = avg_by_prec(plans[[abbr]], pr_dem, draws=NA)
    pr_enac = avg_by_prec(plans[[abbr]], pr_dem, draws="cd_2020")
    as_tibble(maps[[abbr]]) |>
        mutate(pr_enac = pr_enac,
               diff = pr_samp - pr_enac) |>
        group_by(state, cd_2020) |>
        summarize(pr_enac = pr_enac[1],
                  gerry = weighted.mean(diff, ndv+nrv))
})
d_gerry$gerry[d_gerry$state == "SD"] = 0
d_gerry$pr_enac[d_gerry$state == "SD"] = 0



# Racial gerrymandering --------

d_race = map_dfr(names(plans), function(abbr) {
    cat(abbr, "\n")
    if (mean(is.na(plans[[abbr]]$total_vap))) {
        x = alarm_50state_stats(abbr)
        plans[[abbr]]$vap_black = x$vap_black
        plans[[abbr]]$vap_white = x$vap_white
        plans[[abbr]]$vap_hisp = x$vap_hisp
        plans[[abbr]]$total_vap = x$total_vap
    }
    black_samp = avg_by_prec(plans[[abbr]], vap_black/total_vap, draws=NA)
    black_enac = avg_by_prec(plans[[abbr]], vap_black/total_vap, draws="cd_2020")
    white_samp = avg_by_prec(plans[[abbr]], vap_white/total_vap, draws=NA)
    white_enac = avg_by_prec(plans[[abbr]], vap_white/total_vap, draws="cd_2020")
    hisp_samp = avg_by_prec(plans[[abbr]], vap_hisp/total_vap, draws=NA)
    hisp_enac = avg_by_prec(plans[[abbr]], vap_hisp/total_vap, draws="cd_2020")
    as_tibble(maps[[abbr]]) |>
        mutate(diff_black = black_samp - black_enac,
               diff_white = white_samp - white_enac,
               diff_hisp = hisp_samp - hisp_enac) |>
        group_by(state, cd_2020) |>
        summarize(gerry_black = weighted.mean(diff_black, vap),
                  gerry_white = weighted.mean(diff_white, vap),
                  gerry_hisp = weighted.mean(diff_hisp, vap))
})

# Plots ---------
{

p = hex |>
    left_join(d_gerry, by=c("state", "district"="cd_2020")) |>
ggplot(aes(fill=pr_enac)) +
    geom_sf(lwd=0.3, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district),
                 size=2.0, fontface="bold", color="#000000aa") +
    scale_fill_party_c("Win prob.\nfor enacted", labels=scales::percent) +
    theme_map() +
    theme(legend.position=c(0.875, 0.375))
ggsave("~/Desktop/hex_prob.pdf", plot=p, height=8.5, width=11)

p = hex |>
    left_join(d_gerry, by=c("state", "district"="cd_2020")) |>
ggplot(aes(fill=gerry)) +
    geom_sf(lwd=0.3, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district),
                 size=2.0, fontface="bold", color="#000000aa") +
    scale_fill_party_c("Change in win prob.\nfrom baseline", midpoint=0, limits=c(-0.5, 0.5),
                       reverse=TRUE, labels=label_party_margin(midpoint=0.0, reverse=TRUE)) +
    theme_map() +
    theme(legend.position=c(0.875, 0.375))
ggsave("~/Desktop/hex_party.pdf", plot=p, height=8.5, width=11)

p = hex |>
    left_join(d_race, by=c("state", "district"="cd_2020")) |>
    ggplot(aes(fill=-gerry_black)) +
    geom_sf(lwd=0.3, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district),
                 size=2.0, fontface="bold", color="#000000aa") +
    wacolors::scale_fill_wa_c("vantage", midpoint=0, limits=c(-0.25, 0.25),
                              oob=scales::squish,
                              labels=\(x) scales::number(100*x, 1, suffix="pp"),
                              name="Change in Black share\nfrom baseline") +
    theme_map() +
    theme(legend.position=c(0.85, 0.375))
ggsave("~/Desktop/hex_black.pdf", plot=p, height=8.5, width=11)

p = hex |>
    left_join(d_race, by=c("state", "district"="cd_2020")) |>
    ggplot(aes(fill=-gerry_white)) +
    geom_sf(lwd=0.3, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district),
                 size=2.0, fontface="bold", color="#000000aa") +
    wacolors::scale_fill_wa_c("vantage", midpoint=0, limits=c(-0.25, 0.25),
                              oob=scales::squish,
                              labels=\(x) scales::number(100*x, 1, suffix="pp"),
                              name="Change in White share\nfrom baseline") +
    theme_map() +
    theme(legend.position=c(0.85, 0.375))
ggsave("~/Desktop/hex_white.pdf", plot=p, height=8.5, width=11)

p = hex |>
    left_join(d_race, by=c("state", "district"="cd_2020")) |>
    ggplot(aes(fill=-gerry_hisp)) +
    geom_sf(lwd=0.3, color="black") +
    geom_sf_text(aes(geometry=geom_label, label=district),
                 size=2.0, fontface="bold", color="#000000aa") +
    wacolors::scale_fill_wa_c("vantage", midpoint=0, limits=c(-0.25, 0.25),
                              oob=scales::squish,
                              labels=\(x) scales::number(100*x, 1, suffix="pp"),
                              name="Change in Hispanic\nshare from baseline") +
    theme_map() +
    theme(legend.position=c(0.85, 0.375))
ggsave("~/Desktop/hex_hisp.pdf", plot=p, height=8.5, width=11)

}

