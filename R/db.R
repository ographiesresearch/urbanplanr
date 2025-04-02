#' Test Whether Database Exists
#'
#' @param conn Database connection like that created by `RPostgres::dbConnect()`.
#' @param dbname Character. Name of database.
#'
#' @returns Boolean.
#' @export
#'
db_exists <- function(conn, dbname) {
  exists <- dbname %in% RPostgres::dbGetQuery(
    conn, 
    glue::glue("SELECT datname FROM pg_database WHERE datname = '{dbname}';")
  )$datname
  if (exists) {
    message(glue::glue("Database '{dbname}' exists!"))
  } else {
    message(glue::glue("Database '{dbname}' does not exist."))
  }
  exists
}

#' Create PostGIS extension.
#'
#' @inheritParams db_exists
#'
#' @returns NULL
#' @export
#'
db_create_postgis <- function(conn, dbname) {
  RPostgres::dbExecute(conn, "CREATE EXTENSION postgis;")
  message(
    glue::glue("Created PostGIS extension on database '{dbname}'.")
  )
  NULL
}

#' Drop Database If It Exists
#'
#' @inheritParams db_exists
#'
#' @returns NULL
#' @export
db_drop <- function(conn, dbname) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("DROP DATABASE IF EXISTS {dbname};")
  )
  NULL
}

#' Create Database
#'
#' @inheritParams db_exists
#'
#' @returns NULL
#' @export
#'
db_create <- function(conn, dbname) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("CREATE DATABASE {dbname};")
  )
  
  message(
    glue::glue("Created database '{dbname}'.")
  )
  
  NULL
}

#' Test Whether Role Exists
#'
#' @inheritParams db_exists
#' @param role Character. Name of role.
#'
#' @returns Boolean
#' @export
#'
db_role_exists <- function(conn, role) {
  
  exists <- role %in% RPostgres::dbGetQuery(
    conn, 
    glue::glue("SELECT rolname FROM pg_roles WHERE rolname = '{role}';")
  )$rolname
  
  if (exists) {
    message(glue::glue("Role '{role}' exists!"))
  } else {
    message(glue::glue("Role '{role}' does not exist."))
  }
  exists
}

#' Create Role With a Password
#'
#' @inheritParams db_role_exists
#' @param pass Character. Password
#'
#' @returns NULL
#' @export
#'
db_role_create <- function(conn, role, pass) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("CREATE ROLE {role} WITH LOGIN PASSWORD '{pass}';")
  )
  message(
    glue::glue("Role '{role}' created.")
  )
  NULL
}

#' Grant DB Acess to Role
#'
#' @inheritParams db_exists
#' @inheritParams db_role_exists
#'
#' @returns NULL
#' @export
#'
db_grant_access <- function(conn, dbname, role) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("GRANT CONNECT ON DATABASE {dbname} TO {role};")
  )
  message(
    glue::glue("Granted CONNECT privilege on database '{dbname}' to role '{role}'.")
  )
  NULL
}


#' Set Default Access for Role on Database
#'
#' @inheritParams db_grant_access
#'
#' @returns NULL
#' @export
#'
db_set_defaults <- function(conn, dbname, role) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO {role};")
  )
  message(
    glue::glue("Set default SELECT privileges in '{dbname}' to role '{role}'.")
  )
  NULL
}

#' Create DB and User, Set Access
#'
#' @inheritParams db_grant_access
#' @inheritParams db_role_create
#'
#' @returns NULL
#' @export
#'
db_create_if <- function(conn, dbname, role, pass) {
  if (db_exists(dbname)) {
    overwrite <- utils_prompt_check("Would you like to overwrite the database?")
    if (overwrite) {
      db_drop(conn, dbname)
      db_create(conn, dbname)
      db_create_postgis(conn, dbname)
    } else {
      message(
        glue::glue("User chose to not overwrite database '{dbname}'.")
      )
    }
  } else {
    db_create(conn, dbname)
    db_create_postgis(conn, dbname)
  }
  
  if (!db_role_exists(role)) {
    db_role_create(conn, role, pass)
    db_grant_access(conn, dbname, role)
    db_set_defaults(conn, dbname, role)
  } else {
    db_grant_access(conn, dbname, role)
    db_set_defaults(conn, dbname, role)
  }
}