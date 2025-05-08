#' Test Whether Database Exists
#'
#' @param conn Database connection like that created by `RPostgres::dbConnect()`.
#' @param dbname Character. Name of database.
#'
#' @returns Boolean.
#' @export
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
#' Test Whether Extension Exists
#'
#' @inheritParams db_exists
#' @param ext Character. Name of extension, e.g., "postgis."
#'
#' @returns Boolean.
#' @export
db_extension_exists <- function(conn, ext) {
   ext %in% RPostgres::dbGetQuery(
    conn,
    glue::glue("SELECT extname FROM pg_extension WHERE extname = '{ext}'")
  )$extname
}

#' Create Extension
#'
#' @inheritParams db_extension_exists
#'
#' @returns conn
#' @export
db_create_extension <- function(conn, ext) {
  current <- RPostgres::dbGetInfo(conn)
  if(!db_extension_exists(conn, ext)) {
    RPostgres::dbExecute(conn, glue::glue("CREATE EXTENSION {ext}"))
    message(
      glue::glue("Created '{ext}' extension on database '{current$dbname}'.")
    )
  } else {
    message(
      glue::glue("'{ext}' already exists on database '{current$dbname}'.")
    )
  }
  conn
}

#' Create extensions.
#'
#' @inheritParams db_exists
#' @param exts Character vector. Name of extensions to create.
#'
#' @returns conn
#' @export
db_create_extensions <- function(conn, 
                                 exts = c("postgis", "postgis_raster")) {
  purrr::walk(exts, \(x) db_create_extension(conn, x))
  conn
}

#' Drop Database If It Exists
#'
#' @inheritParams db_exists
#'
#' @returns conn
#' @export
db_drop <- function(conn, dbname) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("DROP DATABASE IF EXISTS {dbname};")
  )
  conn
}

#' Create Database
#'
#' @inheritParams db_exists
#'
#' @returns conn
#' @export
db_create <- function(conn, dbname) {
  RPostgres::dbExecute(
    conn, 
    glue::glue("CREATE DATABASE {dbname};")
  )
  
  message(
    glue::glue("Created database '{dbname}'.")
  )
  
  conn
}

#' Test Whether Role Exists
#'
#' @inheritParams db_exists
#' @param role Character. Name of role.
#'
#' @returns Boolean
#' @export
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
#' @returns conn
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
  conn
}

#' Grant DB Acess to Role
#'
#' @inheritParams db_exists
#' @inheritParams db_role_exists
#'
#' @returns conn
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
  conn
}


#' Set Default Access for Role on Database
#'
#' @inheritParams db_grant_access
#'
#' @returns conn
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
  conn
}

#' Create connection to DB
#' 
#' Thin wrapper around RPostgres::dbConnect().
#'
#' @inheritParams db_grant_access
#' @inheritParams db_role_create
#' @param pass Character. Password.
#' @param host Character. Host address. Defaults to "localhost".
#' @param port Integer. Port number. Defaults to 5432.
#'
#' @returns conn
#' @export
db_conn <- function(dbname, role, pass, host = "localhost", port = 5432) {
  RPostgres::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = dbname,
    user = role,
    password = pass,
    host = host,
    port = port
    )
}

#' Create DB and User, Set Access
#'
#' @inheritParams db_grant_access
#' @inheritParams db_role_create
#' @inheritParams db_create_extensions
#' 
#' @returns conn
#' @export
db_create_flow <- function(conn,
                           dbname,
                           role,
                           role_pass,
                           admin_pass,
                           exts = c("postgis", "postgis_raster")
                           ) {
  if (db_exists(conn, dbname)) {
    overwrite <- utils_prompt_check("Would you like to overwrite the database?")
    overwrite <- ifelse(
      overwrite, 
      utils_prompt_check("Are you sure?"), 
      FALSE
      )
    if (overwrite) {
      conn |>
        db_drop(dbname) |>
        db_create(dbname)
    }
  } else {
    db_create(conn, dbname)
  }
  
  current <- RPostgres::dbGetInfo(conn)
  
  new_conn <- db_conn(
    dbname = dbname,
    role = current$username,
    pass = admin_pass,
    host = current$host,
    port = current$port
  )
  
  on.exit(RPostgres::dbDisconnect(new_conn))
  
  if (!db_role_exists(new_conn, role)) {
    new_conn |>
      db_role_create(role, role_pass)
  }

  new_conn |>
    db_grant_access(dbname, role) |>
    db_set_defaults(dbname, role) |>
    db_create_extensions(exts)

  conn
}
