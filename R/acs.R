#' Get 5-Year ACS Estimates Based on Variable List
#'
#' @param vars Character vector of variables (see `tidycensus::load_variables`).
#' @param states Character vector of state abbreviations.
#' @param year Integer. Final year of ACS estimates.
#' @param census_unit Census unit for which to download tables.
#' @param crs Target coordinate reference system: object of class `crs`.
#' @param county Character. County name.
#' @param geometry Boolean. If `TRUE` (default) retains Tiger/LINE geography.
#' @param drop_moe Boolean. If `TRUE` (default) drops margin of error estimates.
#'
#' @returns `sf` object if `geometry` is `TRUE`. Else data frame.
#' @export
acs_get_vars <- function(vars,
                         states,
                         year,
                         census_unit,
                         crs = 4326,
                         county = NULL,
                         geometry = TRUE,
                         drop_moe = TRUE) {
  var_values <- unname(vars)
  df <- tidycensus::get_acs(
    geography = census_unit,
    variables = var_values, 
    year = year,
    state = states,
    county = county,
    geometry = geometry,
    cache_table = TRUE,
    output = "wide"
  ) |>
    dplyr::rename_with(~stringr::str_remove(.x, "E$"))
  if (geometry) {
    df <- df |>
      sf::st_transform(crs)
  }
  if (drop_moe) {
    df <- df |>
      dplyr::select(
        -dplyr::ends_with("M")
      )
  }
  if(!is.null(names(vars))) {
    df <- df |>
      dplyr::rename(
        dplyr::all_of(vars)
      )
  }
  df |>
    dplyr::rename(
      geoid = "GEOID"
    )
}

#' Get 5-Year ACS Estimates Based on Variable List
#'
#' @inheritParams acs_get_vars
#' @param table The ACS table for which you would like to request all 
#' variables.
#' @param var_match Character. Retain only variables matching this string.
#' @param var_suffix Boolean. If `TRUE` (default), adjusts for variable 
#' suffixes.
#'
#' @returns `sf` object if `geometry` is `TRUE`. Else data frame.
#' @export
acs_get_table <- function(table, 
                          states,
                          year,
                          census_unit,
                          crs = 4326,
                          county = NULL,
                          geometry = TRUE,
                          var_match = "",
                          var_suffix = TRUE) {
  df <- tidycensus::get_acs(
      geography = census_unit,
      table = table, 
      year = year,
      state = states,
      county = county,
      geometry = geometry,
      cache_table = TRUE
    ) |>
    dplyr::filter(
      !stringr::str_detect(.data$variable, "_C0[2-9]_")
    ) |>
    dplyr::mutate(
      "prefix" = stringr::str_c(
        "B",
        stringr::str_sub(table, 2),
        sep = ""
      ),
      "prefix" = dplyr::case_when(
        var_suffix ~ stringr::str_c(
          .data$prefix,
          "1",
          sep = ""
        ),
        .default = .data$prefix
      ),
      "variable" = stringr::str_c(
        .data$prefix,
        stringr::str_extract(.data$variable, "(?<=_)\\d+$"),
        sep="_"
      )
    ) |>
    dplyr::select(-"prefix") |>
    dplyr::left_join(
      tidycensus::load_variables(
        year = year,
        dataset = "acs5",
        cache = TRUE
      ) |>
        dplyr::select("name", "label"),
      by = c("variable" = "name")
    ) |>
    dplyr::mutate(
      "level" = stringr::str_count(.data$label, "!!") - 1,
      "label" = stringi::stri_trans_general(.data$label, id="Latin-ASCII"),
      "label" = stringr::str_extract(
        .data$label,
        "(?<=!!)[0-9A-Za-z\\s,()-]+(?=(?:$|:$))"
      )
    )

  if (nchar(var_match) > 0) {
    df <- df |>
      dplyr::filter(
        stringr::str_detect(.data$variable, pattern = var_match)
      )
  }
  if (geometry) {
    df <- df |>
      sf::st_transform(crs)
  }
  df
}

#' Process Nested Data Structures in ACS Estimates.
#'
#' @param df A dataframe.
#'
#' @returns A dataframe.
#' @export
acs_process_nested <- function(df) {
  max_level <- max(df$level)
  
  df |>
    dplyr::mutate(
      levels_flag = dplyr::case_when(
        (.data$level == dplyr::lead(.data$level)) ~ TRUE,
        (.data$level == dplyr::lag(.data$level)) & (.data$level > dplyr::lead(.data$level)) ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      levels = dplyr::case_when(
        .data$levels_flag ~ list(seq.int(.data$level, max_level, 1)),
        .default = list(.data$level)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label = stringr::str_replace_all(
        .data$label,
        c("[(),:]" = "", "\\-" = " ")
      )
    ) 
  # |>
  #   dplyr::rename(
  #     geoid = .data$GEOID
  #   )
}

#' Calculate Proportions from Long-Formatted ACS Subsets by Population
#'
#' @param df A dataframe.
#' @param unique_col Character or integer. Column containing unique values of
#' aggregating unit (i.e., the FIPS code).
#' @param percent If `TRUE` (default), calculates a percentage. Otherwise, returns a
#' proportion.
#'
#' @returns A dataframe.
#' @export
acs_pct_transform <- function(df, unique_col, percent = TRUE) {
  mult <- ifelse(percent, 100, 1)
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(unique_col))) |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        .data$estimate == max(.data$estimate) ~ mult,
        .default = .data$estimate / max(.data$estimate) * mult
      )
    ) |>
    dplyr::ungroup()
}

#' Pivot ACS Estimates Long to Wide
#'
#' @param df A dataframe.
#' @param percent Whether to calculate percentages from long-formatted table.
#' Assumes that one value of `label` is a total.
#'
#' @returns A dataframe.
#' @export
acs_pivot <- function(df, percent = TRUE) {
  depths <- unique(df$level)
  depths_nonzero <- depths[ !depths == 0 ]
  
  if (percent) {
    df <- df |>
      acs_pct_transform("geoid") |>
      dplyr::mutate(
        type = "pct"
      ) |>
      dplyr::filter(.data$label != "Total") |>
      dplyr::bind_rows(
        df |>
          dplyr::mutate(
            type = "count"
          )
      )
  } else {
    df <- df |>
      dplyr::mutate(
        type = "count"
      )
  }
  
  for (d in depths_nonzero) {
    df_out <- df |>
      dplyr::rowwise() |>
      dplyr::filter(
        d %in% .data$levels || 0 %in% .data$levels
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        label = stringi::stri_trans_general(.data$label, id="Latin-ASCII"),
      ) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of("geoid"),
        names_from = c(.data$type, .data$label),
        names_glue = "{.data$type}_{.data$label}",
        values_from = .data$estimate
      )
  }
}

#' Get ACS Variables by Census Unit
#' @name get_acs
#' 
#' @description
#' `acs_get_industries()` obtains data on employment by industry from ACS table [S2403 (Industry by Sex for the Civilian 
#' Employed Population 16 Years and Over)](https://data.census.gov/table/ACSST5Y2022.S2401).
#' 
#' `acs_get_occupations()` obtains data on civilian occupation from ACS table 
#' [S2401 (Occupation by Sex for the Civilan Employed Population 16 Years 
#' and Over)](https://data.census.gov/table/ACSST5Y2022.S2401).
#' 
#' `acs_get_ancestry()` obtains data on reported ancestry from ACS table 
#' [B04006 (People Reporting Ancestry)](https://data.census.gov/table/ACSDT5Y2023.B04006).
#' 
#' `acs_get_place_of_birth()` obtains data on place of birth for migrant population from ACS table
#' [B05006 (Place of Birth for the Foreign-Born Population in the United States)](https://data.census.gov/table/ACSDT5Y2023.B05006).
#' 
#' `acs_get_housing()`obtains data on tenure, housing type, and gross rent from ACS tables
#' [B25042 (Tenure by Bedrooms)](https://data.census.gov/table/ACSDT5Y2023.B25042), [B25024 (Units in Structure)](https://data.census.gov/table/ACSDT5Y2023.B25024), and
#' [B25031 (Median Gross Rent by Bedrooms)](https://data.census.gov/table/ACSDT5Y2023.B25031).
#' 
#' `acs_get_race()` Obtains data on race and ethnicity, including median household and per-capita income measures, from tables 
#' [B03002 (Hispanic or Latino Origin by Race)](https://data.census.gov/table/ACSDT5Y2023.B03002), [B19301B-H (Per capita income in the past 12 months)](https://data.census.gov/table/ACSDT5Y2023.B19301B), 
#' and [B19013B-H (Median household income in the past 12 months)](https://data.census.gov/table/ACSDT5Y2023.B19013B).
#' 
#' `acs_get_age()` obtains data on age, sliced by sex, from ACS table [B01001 (Sex by Age)](https://data.census.gov/table/ACSDT5Y2023.B01001).
#' 
#' @inheritParams acs_get_vars
#'
#' @returns A tibble or sf tibble of ACS data.
#' @export
#'
acs_get_ind <- function(states,
                        year,
                        census_unit,
                        county = NULL,
                        crs = 4326,
                        geometry = FALSE) {
  acs_get_table("S2403", 
                 census_unit = census_unit,
                 year = year,
                 states = states,
                 county = county,
                 geometry = geometry) |>
    acs_process_nested() 
  # |>
  #   acs_pivot()
}

#' @name get_acs
#' @export
acs_get_occ <- function(states,
                            year,
                            census_unit,
                            county = NULL,
                            crs = 4326,
                            geometry = FALSE) {
  acs_get_table("S2401", 
                census_unit = census_unit,
                year = year,
                states = states,
                county = county,
                geometry = geometry) |>
    acs_process_nested()
}

#' @name get_acs
#' @export
acs_get_ancestry <- function(states,
                             year,
                             census_unit,
                             county = NULL,
                             crs = 4326,
                             geometry = FALSE) {
  acs_get_table("B04006",
                census_unit = census_unit,
                year = year,
                states = states,
                county = county,
                geometry = geometry,
                var_suffix = FALSE)
}

#' @name get_acs
#' @export
acs_get_place_of_birth <- function(states,
                                   year,
                                   census_unit,
                                   county = NULL,
                                   crs = 4326,
                                   geometry = FALSE) {
  acs_get_table("B05006",
                census_unit = census_unit,
                year = year,
                states = states,
                county = county,
                geometry = geometry,
                var_suffix = FALSE)
}

#' @name get_acs
#' @export
acs_get_housing <- function(states,
                            year,
                            census_unit,
                            county = NULL,
                            crs = 4326,
                            geometry = FALSE) {
  vars <- c("hsg_unts" = "B25024_001",
            # Tenure by bedrooms.
            # Owner-occupied...
            "oo" = "B25042_002",
            "oo0br" = "B25042_003",
            "oo1br" = "B25042_004",
            "oo2br" = "B25042_005",
            "oo3br" = "B25042_006",
            "oo4br" = "B25042_007",
            "oogt5br" = "B25042_008",
            # Renter-occupied...
            "ro" = "B25042_009",
            "ro0br" = "B25042_010",
            "ro1br" = "B25042_011",
            "ro2br" = "B25042_012",
            "ro3br" = "B25042_013",
            "ro4br" = "B25042_014",
            "rogt5br" = "B25042_015",
            # Units in Structure
            "unt1d" = "B25024_002",
            "unt1a" = "B25024_003",
            "unt2" = "B25024_004",
            "unt3_4" = "B25024_005",
            "unt5_9" = "B25024_006",
            "unt10_19" = "B25024_007",
            "unt20_49" = "B25024_008",
            "unt50up" = "B25024_009",
            "untmbl" = "B25024_010",
            "untbtrv" = "B25024_011",
            # Median Gross Rent by Bedrooms
            "mgr" = "B25031_001",
            "mgr0br" = "B25031_002",
            "mgr1br" = "B25031_003",
            "mgr2br" = "B25031_004",
            "mgr3br" = "B25031_005",
            "mgr4br" = "B25031_006",
            "mgrgt5br" = "B25031_007"
            )
  acs_get_vars(vars, 
               states = states,
               year = year,
               county = county,
               census_unit = census_unit,
               geometry = geometry
               )
}

#' @name get_acs
#' @export
acs_get_race <- function(states,
                         year,
                         census_unit,
                         county = NULL,
                         crs = 4326,
                         geometry = FALSE) {
  vars <- c(
    "tot" = "B03002_001",
    "white" = "B03002_003",
    "black" = "B03002_004",
    "hisp_lat" = "B03002_012",
    "indig" = "B03002_005",
    "asian" = "B03002_006",
    "hw_pi" = "B03002_007",
    "other" = "B03002_008",
    "multi" = "B03002_009",
    "multi" = "B03002_009",
    # PER-CAPITA INCOME
    # Non-Hispanic
    "wht_pci" = "B19301H_001",
    # Following include Hispanic
    "blk_pci" = "B19301B_001",
    "ind_pci" = "B19301C_001",
    "asn_pci" = "B19301D_001",
    # Not available at sub-county level
    # "hwp_pci" = "B19301E_001",
    "oth_pci" = "B19301F_001",
    "mti_pci" = "B19301G_001",
    "hslt_pci" = "B19301I_001",
    # MEDIAN HOUSEHOLD INCOME
    # Non-Hispanic
    "wht_mhi" = "B19013A_001",
    # Following include Hispanic
    "blk_mhi" = "B19013B_001",
    "ind_mhi" = "B19013C_001",
    "asn_mhi" = "B19013D_001",
    # Not available at sub-county level
    # "hwp_pci" = "B19301E_001",
    "oth_mhi" = "B19013F_001",
    "mti_mhi" = "B19013G_001",
    "hslt_mhi" = "B19013I_001"
  )
  acs_get_vars(vars, 
               states = states,
               year = year,
               county = county,
               census_unit = census_unit,
               geometry = geometry
  )
}

#' @name get_acs
#' @export
acs_get_age <- function(states,
                        year,
                        census_unit,
                        county = NULL,
                        crs = 4326,
                        geometry = FALSE) {
  vars <- c(
    "tot" = "B01001_001",
    "mtot" = "B01001_002",
    "mlt5" = "B01001_003",
    "m5_9" = "B01001_004",
    "m10_14" = "B01001_005",
    "m15_17" = "B01001_006",
    "m18_19" = "B01001_007",
    "m20_24" = "B01001_008",
    "m25_29" = "B01001_009",
    "m30_34" = "B01001_010",
    "m35_44" = "B01001_011",
    "m45_54" = "B01001_012",
    "m55_64" = "B01001_013",
    "m65_74" = "B01001_014",
    "m75_84" = "B01001_015",
    "mgt85" = "B01001_016",
    "ftot" = "B01001_017",
    "flt5" = "B01001_018",
    "f5_9" = "B01001_019",
    "f10_14" = "B01001_020",
    "f15_17" = "B01001_021",
    "f18_19" = "B01001_022",
    "f20_24" = "B01001_023",
    "f25_29" = "B01001_024",
    "f30_34" = "B01001_025",
    "f35_44" = "B01001_026",
    "f45_54" = "B01001_027",
    "f55_64" = "B01001_028",
    "f65_74" = "B01001_029",
    "f75_84" = "B01001_030",
    "fgt85" = "B01001_031")
  
  acs_get_vars(vars, 
               states = states,
               year = year,
               county = county,
               census_unit = census_unit,
               geometry = geometry
    )|>
    dplyr::mutate(
      tlt5 = rowSums(dplyr::across(dplyr::matches("lt5")), na.rm = TRUE),
      t5_9 = rowSums(dplyr::across(dplyr::matches("5_9")), na.rm = TRUE),
      t10_14 = rowSums(dplyr::across(dplyr::matches("10_14")), na.rm = TRUE),
      t15_17 = rowSums(dplyr::across(dplyr::matches("15_17")), na.rm = TRUE),
      t18_19 = rowSums(dplyr::across(dplyr::matches("18_19")), na.rm = TRUE),
      t20_24 = rowSums(dplyr::across(dplyr::matches("20_24")), na.rm = TRUE),
      t25_29 = rowSums(dplyr::across(dplyr::matches("25_29")), na.rm = TRUE),
      t30_34 = rowSums(dplyr::across(dplyr::matches("30_34")), na.rm = TRUE),
      t35_44 = rowSums(dplyr::across(dplyr::matches("35_44")), na.rm = TRUE),
      t45_54 = rowSums(dplyr::across(dplyr::matches("45_54")), na.rm = TRUE),
      t55_64 = rowSums(dplyr::across(dplyr::matches("55_64")), na.rm = TRUE),
      t65_74 = rowSums(dplyr::across(dplyr::matches("65_74")), na.rm = TRUE),
      t75_84 = rowSums(dplyr::across(dplyr::matches("75_84")), na.rm = TRUE),
      tgt85 = rowSums(dplyr::across(dplyr::matches("gt85")), na.rm = TRUE),
      f5_17 = rowSums(dplyr::across(c("f5_9", "f10_14", "f15_17")), na.rm = TRUE),
      f18_24 = rowSums(dplyr::across(c("f18_19", "f20_24")), na.rm = TRUE),
      f25_34 = rowSums(dplyr::across(c("f25_29", "f30_34")), na.rm = TRUE),
      fgt65 = rowSums(dplyr::across(c("f65_74", "f75_84", "fgt85")), na.rm = TRUE),
      m5_17 = rowSums(dplyr::across(c("m5_9", "m10_14", "m15_17")), na.rm = TRUE),
      m18_24 = rowSums(dplyr::across(c("m18_19", "m20_24")), na.rm = TRUE),
      m25_34 = rowSums(dplyr::across(c("m25_29", "m30_34")), na.rm = TRUE),
      mgt65 = rowSums(dplyr::across(c("m65_74", "m75_84", "mgt85")), na.rm = TRUE)
    ) |>
    dplyr::select(
       "geoid",
      dplyr::starts_with("t"), 
      dplyr::starts_with("f"),
      dplyr::starts_with("m")
    )
}

acs_router <- function(var, geography, states, county = NULL, year = 2022) {
  if (var == "ind") {
    # data <- acs_get_ind(census_unit = geography, states = states, county = county, year = year)
  } else if (var == "occ") {
    # data <- acs_get_occ(census_unit = geography, states = states, county = county, year = year)
  } else if (var == "ancestry") {
    # data <- acs_get_ancestry(census_unit = geography, states = states, county = county, year = year)
  } else if (var == "place_of_birth") {
    # data <- acs_get_place_of_birth(census_unit = geography, states = states, county = county, year = year)
  } else if (var == "housing") {
    data <- acs_get_housing(census_unit = geography, states = states, county = county, year = year)
  } else if (var == "race") {
    data <- acs_get_race(census_unit = geography, states = states, county = county, year = year)
  } else if (var == "age") {
    data <- acs_get_age(census_unit = geography, states = states, county = county, year = year)
  } else {
    message(glue::glue("ACS variable {var} is invalid. Skipping"))
  }
}

acs_get_multi <- function(vars, places, year, geography) {
  if (geography == "block_group") {
    geo_name <- "block_groups"
  } else if (geography == "tract") {
    geo_name <- "tracts"
  } else if (geography == "county") {
    geo_name <- "counties"
  }
  vars_out_names <- stringr::str_c(geo_name, vars, sep="_")
  is_county <- utils_is_county(places)
  if (is_county) {
    data <- vars |>
      purrr::map(
        \(x) purrr::map(
          utils_parse_place(places),
          \(y) acs_router(
          var = x,
          geography = geography, 
          states = y[2], 
          county = y[1], 
          year = year
          )
        ) |>
          dplyr::bind_rows()
      ) |>
      purrr::set_names(vars_out_names)
  } else {
    data <- vars |>
      purrr::map(
        \(x) purrr::map(
          utils_place_states(places),
          \(y) acs_router(
            var = x,
            geography = geography, 
            states = y,
            year = year
          )
        ) |>
          dplyr::bind_rows()
      ) |>
      purrr::set_names(vars_out_names)
  }
  data
}
