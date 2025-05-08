#' Download LODES OD tables by census unit.
#'
#' @param states Character vector of state abbreviations.
#' @param year Integer. Year of LODES data.
#' @param census_unit One of c("bg", "tract", "county", "state").
#' @param use_cache Whether LODES data should be loaded from the `lehdr` cache.
#'
#' @returns A dataframe of LODES files.
#' 
lodes_get_data <- function(states, 
                      year, 
                      census_unit,
                      use_cache = TRUE) {
  lehdr::grab_lodes(
    state = states,
    year = year,
    agg_geo = census_unit,
    use_cache = use_cache,
    state_part = "main",
    version = "LODES8",
    job_type = "JT01",
    segment = "S000"
  ) |>
    dplyr::bind_rows(
      lehdr::grab_lodes(
        state = states,
        year = year,
        agg_geo = census_unit,
        use_cache = use_cache,
        state_part = "aux",
        version = "LODES8",
        job_type = "JT01",
        segment = "S000"
      )
    )
}

# lodes_to_census_units <- function(df, 
#                                   census_units_geo,
#                                   census_unit) {
#   df |>
#     dplyr::left_join(
#       census_units_geo |> 
#         dplyr::rename(
#           x_w = x,
#           y_w = y,
#           x_pl_w = x_pl,
#           y_pl_w = y_pl,
#           pl_n_w = pl_id,
#           dplyr::any_of(c(selected_w = "selected"))
#         ), 
#       by = c("w_unit" = "unit_id")
#     ) |>
#     dplyr::left_join(
#       census_units_geo |> 
#         dplyr::rename(
#           x_h = "x",
#           y_h = "y",
#           x_pl_h = "x_pl",
#           y_pl_h = "y_pl",
#           pl_n_h = "pl_id",
#           dplyr::any_of(c(selected_h = "selected"))
#         ),
#       by = c("h_unit" = "unit_id")
#     )
# }

lodes_type_in_geo <- function(df, type = "w", geo = "bg", segment = "S000") {
  if (type == "w") {
    other <- "h"
  } else if (type == "h") {
    other <- "w"
  } else {
    stop("Invalid work/home type.")
  }
  
  a_id <- stringr::str_c(type, geo, sep="_")
  b_id <- stringr::str_c(other, geo, sep="_")
  
  df |>
    dplyr::mutate(
      in_unit = .data[[a_id]] == .data[[b_id]]
    ) |>
    dplyr::group_by(.data$in_unit, id = .data[[a_id]]) |>
    dplyr::summarize(
      count = sum(.data[[segment]])
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = "id",
      names_from = "in_unit",
      values_from = "count",
      values_fill = 0
    ) |>
    dplyr::rename(
      !!stringr::str_c(type, "in", "unit", sep = "_") := "TRUE",
      !!stringr::str_c(type, "out", "unit", sep = "_") := "FALSE"
    )
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