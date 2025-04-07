#' Download LODES OD tables by census unit.
#'
#' @param states Character vector of state abbreviations.
#' @param year Integer. Year of LODES data.
#' @param census_unit One of c("bg", "tract", "county", "state").
#'
#' @returns A dataframe of LODES files.
#' 
lodes_get_data <- function(states, 
                      year, 
                      census_unit) {
  results <- list()
  for (s in states) {
    message(glue::glue("Getting LODES data for {s}."))
    results[[s]] <- lehdr::grab_lodes(
      state = s,
      year = year,
      version = "LODES8",
      job_type = "JT01",
      segment = "S000",
      state_part = "main",
      agg_geo = census_unit,
      use_cache = TRUE
    ) |>
      dplyr::bind_rows(
        lehdr::grab_lodes(
          state = s,
          year = year,
          version = "LODES8",
          job_type = "JT01",
          segment = "S000",
          state_part = "aux",
          agg_geo = census_unit,
          use_cache = TRUE
        )
      )
  }
  dplyr::bind_rows(results) |>
    dplyr::distinct()
}

lodes_prep <- function(od,
                       census_unit) {
  h_col <- stringr::str_c("h", census_unit, sep = "_")
  w_col <- stringr::str_c("w", census_unit, sep = "_")
  od |>
    dplyr::rename(
      w_unit = {{w_col}},
      h_unit = {{h_col}}
    )
}

lodes_to_census_units <- function(df, 
                                  census_units_geo,
                                  census_unit) {
  
  census_units_geo <- census_units_geo |>
    st_multi_type_center() |>
    sf::st_drop_geometry() |>
    dplyr::select(-c("state", "pl_name"))
  
  df |>
    dplyr::left_join(
      census_units_geo |> 
        dplyr::rename(
          x_w = "x",
          y_w = "y",
          x_pl_w = "x_pl",
          y_pl_w = "y_pl",
          pl_n_w = "pl_id",
          dplyr::any_of(c(selected_w = "selected"))
        ), 
      by = c("w_unit" = "unit_id")
    ) |>
    dplyr::left_join(
      census_units_geo |> 
        dplyr::rename(
          x_h = "x",
          y_h = "y",
          x_pl_h = "x_pl",
          y_pl_h = "y_pl",
          pl_n_h = "pl_id",
          dplyr::any_of(c(selected_h = "selected"))
        ),
      by = c("h_unit" = "unit_id")
    )
}

lodes_workers_in_unit <- function(prox) {
  prox |>
    dplyr::group_by(.data$in_unit, unit_id = .data$w_unit) |>
    dplyr::summarize(
      count = sum(.data$S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = "unit_id",
      names_from = "in_unit",
      names_prefix = "in_unit_",
      values_from = "count",
      values_fill = 0
    ) |>
    dplyr::rename(
      w_in_unit = "in_unit_TRUE"
    ) |>
    dplyr::mutate(
      w_tot_in_unit = .data$in_unit_FALSE + .data$w_in_unit,
      pct_w_in_unit = .data$w_in_unit / .data$w_tot_in_unit * 100
    ) |>
    dplyr::select(-c("in_unit_FALSE"))
}

lodes_workers_in_town <- function(prox) {
  prox |>
    dplyr::group_by(.data$in_town, unit_id = .data$w_unit) |>
    dplyr::summarize(
      count = sum(.data$S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = "unit_id",
      names_from = "in_town",
      names_prefix = "in_town_",
      values_from = "count",
      values_fill = 0
    ) |>
    dplyr::rename(
      w_in_town = "in_town_TRUE"
    ) |>
    dplyr::mutate(
      w_tot_in_town = .data$in_town_FALSE + .data$w_in_town,
      pct_w_in_town = .data$w_in_town / .data$w_tot_in_town * 100
    ) |>
    dplyr::select(-c("in_town_FALSE"))
}

lodes_residents_in_unit <- function(prox) {
  # What % of working residents work in tract?
  prox |>
    dplyr::group_by(.data$in_unit, unit_id = .data$h_unit) |>
    dplyr::summarize(
      count = sum(.data$S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = "unit_id",
      names_from = "in_unit",
      names_prefix = "in_unit_",
      values_from = "count",
      values_fill = 0
    ) |>
    dplyr::rename(
      h_in_unit = "in_unit_TRUE"
    ) |>
    dplyr::mutate(
      h_tot_in_unit = .data$in_unit_FALSE + .data$h_in_unit,
      pct_h_in_unit = .data$h_in_unit / .data$h_tot_in_unit * 100
    ) |>
    dplyr::select(-c("in_unit_FALSE"))
}

lodes_residents_in_town <- function(prox) {
  prox |>
    dplyr::group_by(.data$in_town, unit_id = .data$h_unit) |>
    dplyr::summarize(
      count = sum(.data$S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = "unit_id",
      names_from = "in_town",
      names_prefix = "in_town_",
      values_from = "count",
      values_fill = 0
    ) |>
    dplyr::rename(
      h_in_town = "in_town_TRUE"
    ) |>
    dplyr::mutate(
      h_tot_in_town = .data$in_town_FALSE + .data$h_in_town,
      pct_h_in_town = .data$h_in_town / .data$h_tot_in_town * 100
    ) |>
    dplyr::select(-c("in_town_FALSE"))
}

lodes_proximity_measures <- function(od_census_units) {
  prox <- od_census_units |>
    dplyr::mutate(
      in_unit = .data$w_unit == .data$h_unit,
      in_town = .data$pl_n_h == .data$pl_n_w
    ) |>
    tidyr::replace_na(
      list(
        in_town = FALSE
      )
    )
  
  # Commence hacky copypaste...
  # TODO: Fight with dplyr programming.
  # What % of jobs in tract are held by people in that tract?
  lodes_workers_in_unit(prox) |>
    dplyr::full_join(lodes_workers_in_town(prox), by = "unit_id") |>
    dplyr::full_join(lodes_residents_in_unit(prox), by = "unit_id") |>
    dplyr::full_join(lodes_residents_in_town(prox), by = "unit_id")
}

lodes_selected_ods_poly <- function(od_census_units) {
  sel_workers <- od_census_units |>
    dplyr::filter(.data$selected_w) |>
    dplyr::group_by(unit_id = .data$h_unit, .data$pl_n_w) |>
    dplyr::summarize(
      work_res = sum(.data$S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = "unit_id",
      names_from = "pl_n_w",
      names_prefix = "work_res_",
      values_from = "work_res",
      values_fill = 0
    )
  
  sel_residents <- od_census_units |>
    dplyr::filter(.data$selected_h) |>
    dplyr::group_by(unit_id = .data$w_unit, .data$pl_n_h) |>
    dplyr::summarize(
      res_work = sum(.data$S000)
    ) |>
    tidyr::pivot_wider(
      id_cols = "unit_id",
      names_from = "pl_n_h",
      names_prefix = "res_work_",
      values_from = "res_work",
      values_fill = 0
    )
  
  sel_workers |>
    dplyr::full_join(sel_residents, by = "unit_id")
}

lodes_ods_lines <- function(od_census_units, crs) {
  if (
    ("selected_h" %in% names(od_census_units)) & ("selected_w" %in% names(od_census_units))
    ) {
    od_census_units <- od_census_units |>
      dplyr::filter(.data$selected_h | .data$selected_w)
  }
  od_census_units |>
    tidyr::drop_na("x_h","y_h", "x_w", "y_w") |>
    dplyr::rename(
      count = .data$S000
    ) |>
    dplyr::select(
      "w_unit",
      "selected_w",
      "h_unit",
      "selected_h",
      "count",
      "x_h",
      "y_h",
      "x_w",
      "y_w"
    ) |>
    st_xyxy_to_lines()
}

lodes_ods_lines_place_agg <- function(od_census_units) {
  if (
      ("selected_h" %in% names(od_census_units)) & ("selected_w" %in% names(od_census_units))
    ) {
    od_census_units <- od_census_units |>
      dplyr::filter(.data$selected_h | .data$selected_w)
  }
  od_census_units |>
    tidyr::drop_na("x_h", "y_h", "x_w", "y_w") |>
    dplyr::group_by(
      "pl_n_h", 
      "selected_h",
      "pl_n_w", 
      "selected_w",
      x_h = "x_pl_h", 
      y_h = "y_pl_h", 
      x_w = "x_pl_w",
      y_w = "y_pl_w"
    ) |>
    dplyr::summarize(
      count = sum(.data$S000)
    ) |>
    dplyr::ungroup() |>
    st_xyxy_to_lines()
}