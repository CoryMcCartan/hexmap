make_hex_map = function(state, d_2020, d_usa, hex_per_district=5) {
    d_state = dplyr::filter(d_2020, .data$state == .env$state) |>
        sf::st_drop_geometry() |>
        dplyr::rename(district = cd_2020)

    if (nrow(d_state) == 1) {
        d_state = dplyr::filter(d_2020, .data$state == .env$state) |>
            dplyr::select(state, district=cd_2020) |>
            sf::st_transform(5070)
        d_state$geom_label = geomander::st_circle_center(d_state)$geometry
        return(d_state)
    }

    shp = d_2020$geometry[d_2020$state == state]
    outline = d_usa$geometry[d_usa$STUSPS == state]

    cli::cli_h1(paste("Making map for", state))
    cli::cli_process_start("Making hexagonal grid")

    res = make_hex_grid(shp, outline, hex_per_district=hex_per_district)

    cli::cli_process_done()

    place_districts(res) |>
        dplyr::mutate(state = state, .before=.data$district)
}


make_hex_grid = function(shp, outline, hex_per_district=5, infl=1.05) {
    shp = sf::st_transform(shp, 3857)
    outline = sf::st_transform(outline, 3857)
    outline = outline - sf::st_centroid(outline) + sf::st_centroid(sf::st_union(shp))
    sf::st_crs(outline) = 3857

    bbox = sf::st_bbox(shp)
    shp_area = as.numeric(sum(sf::st_area(sf::st_buffer(shp, 5e3))))
    bbox_area = diff(bbox[c(2, 4)]) * diff(bbox[c(1, 3)])
    shp_frac = min(shp_area / bbox_area, 1)

    a_ratio = diff(bbox[c(2, 4)]) / diff(bbox[c(1, 3)])
    n_hex = round(length(shp) * hex_per_district * infl)

    hex = data.frame()
    cuml_infl = 0.5
    base_area = NULL
    while (nrow(hex) <= n_hex) {
        n_dim = floor(sqrt(cuml_infl * n_hex * c(1/a_ratio, a_ratio)))

        hex = sf::st_make_grid(shp, n=n_dim, square=FALSE, flat_topped=TRUE)
        hex = sf::st_filter(sf::st_sf(geometry=hex), shp)
        base_area = median(as.numeric(sf::st_area(hex)))
        hex = sf::st_intersection(hex, outline) |>
            dplyr::filter(as.numeric(sf::st_area(.data$geometry)) / base_area >= 0.25)

        cuml_infl = cuml_infl * 1.1
    }


    hex = hex |>
        sf::st_centroid() |>
        sf::st_union() |>
        sf::st_voronoi(outline) |>
        sf::st_collection_extract(type="POLYGON") |>
        sf::st_intersection(outline) |>
        sf::st_sf(geometry=_) |>
        sf::st_transform(5070)
    hex$adj = geomander::adjacency(hex)

    connect = geomander::suggest_component_connection(hex, hex$adj)
    for (i in seq_len(nrow(connect))) {
        hex$adj = geomander::add_edge(hex$adj, connect$x[i], connect$y[i])
    }

    shp_adj = sf::st_buffer(shp, 2e3) |>
        sf::st_sf(geometry=_) |>
        geomander::adjacency()

    shp_out = sf::st_transform(shp, 5070) |>
        sf::st_centroid() |>
        sf::st_coordinates() |>
        dplyr::as_tibble() |>
        dplyr::mutate(adj = shp_adj)

    # hexagon size
    base_size = round(sf::st_area(hex) / 3e8) |>
        table() |>
        which.max() |>
        names() |>
        as.numeric()

    list(n_distr=length(shp),
         n_hex=n_hex,
         base_size=base_size,
         hex=hex,
         distr=shp_out)
}


place_districts = function(res, n_runs=50L,
                           max_bursts=300 + round(sqrt(res$n_distr)*25),
                           silent=FALSE) {
    map = redist::redist_map(res$hex, pop=1, ndists=res$n_distr,
                             pop_tol=1.3 * res$n_distr^1.075 / nrow(res$hex),
                             adj=res$hex$adj)

    sc_close = scorer_close(res)
    scorer = redist::scorer_frac_kept(map) + 2*sc_close

    if (!silent) cli::cli_process_start("Initializing districts")
    inits = redist::redist_smc(map, max(round(3 * sqrt(n_runs*res$n_distr)), n_runs),
                               resample=FALSE, pop_temper=0.005, seq_alpha=0.95,
                               ncores=1, silent=TRUE) |>
        as.matrix()
    if (!silent) cli::cli_process_done()

    if (!silent)
    looper = seq_len(n_runs)
    if (!silent) looper = cli::cli_progress_along(looper, "Optimizing", clear=FALSE)
    opt = do.call(rbind, lapply(looper, function(i) {
        redist::redist_shortburst(map, scorer, init_plan=inits[, i],
                                  burst_size=round(2 * sqrt(res$n_distr)),
                                  max_bursts=max_bursts, return_all=FALSE, verbose=FALSE)
    })) |>
        dplyr::filter(score == max(.data$score))
    if (!silent) cli::cli_progress_done()

    pl = redist::last_plan(opt)
    matcher = attr(sc_close, "hungarian")(pl)
    pl = matcher$pairs[pl, 2]

    out = res$hex |>
        dplyr::group_by(district = pl) |>
        dplyr::summarize()
    out$geom_label = geomander::st_circle_center(out)$geometry

    out
}


scorer_close = function(res) {
    m_coord_hex = sf::st_centroid(res$hex$geometry) |>
        sf::st_coordinates() |>
        dplyr::as_tibble()
    m_coord_shp = as.matrix(res$distr[, 1:2])
    areas = as.numeric(sf::st_area(res$hex$geometry))
    areas = areas / sum(areas)
    idx1 = seq_len(res$n_distr)
    idx2 = seq_len(res$n_distr) + res$n_distr
    tot_links = lengths(res$distr$adj)

    fn_hungarian = function(pl) {
        center_x = tapply(m_coord_hex$X, pl, mean)
        center_y = tapply(m_coord_hex$Y, pl, mean)
        m_coords = 1e-6 * rbind(cbind(center_x, center_y), m_coord_shp)
        m_dist = as.matrix(dist(m_coords))[idx1, idx2]
        RcppHungarian::HungarianSolver(m_dist^2)
    }

    fn <- function(plans) {
        apply(plans, 2, function(pl) {
            matcher = fn_hungarian(pl)
            distr_adj = redist:::coarsen_adjacency(res$hex$adj, matcher$pairs[pl, 2] - 1L)
            shared_links = sapply(seq_len(res$n_distr), function(i) {
                length(intersect(distr_adj[[i]], res$distr$adj[[i]]))
            })
            mean(shared_links / tot_links) - 6*matcher$cost/res$n_distr - 12*sd(tapply(areas, pl, sum))
        })
    }

    class(fn) <- c("redist_scorer", "function")
    attr(fn, "hungarian") = fn_hungarian
    fn
}
