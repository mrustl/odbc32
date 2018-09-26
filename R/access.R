#' export data from Rdata file into MS Access db via separate 32-bit Rscript
#' @export
write_access_db <- function(rdata_file) {

  system2(command = file.path(R.home(), "bin", "i386", "Rscript.exe"),
          args    =
            # c(system.file("Rscripts", "write_access_db.R",
            c(system.file("Rscripts", "write_access_db_rodbc.R",
                          package = "odbc32",
                          mustWork = FALSE),
              rdata_file))
}


#' export data from an R object to MS Access db via separate 32-bit Rscript
#' @details
#' \describe{
#'   \item{overwrite}{logical; Attempt to drop any existing table of the name before writing data?}
#'   \item{convert_dates}{character; Convert dates as text before writing into database.}
#' }
#' @export
data2access <- function(data, accdb_file, ...) {

  # save any options as a list
  params = list(accdb_file = accdb_file, ...)

  # create a temporary file for writing the data on disk
  tmpf <- tempfile("rdata")
  on.exit(unlink(tmpf))

  save(data, params, file = tmpf)

  # call R script to write data from temporary file into an Access database
  write_access_db(rdata_file = tmpf)

  return(TRUE)
}





#' Start r2r server connected to an Access database over 32-bit ODBC driver
#' @export
start_server_access2007 <- function(
  accdb_fp,
  invisible = FALSE,
  wait      = FALSE,
  address   = "tcp://localhost",
  port      = "5555",
  Rbin      = file.path(R.home(), "bin", "i386", "Rscript.exe")
) {

  stopifnot(file.exists(accdb_fp))
  stopifnot(file.exists(Rbin))

  # start 32-bit R with r2r server
  system2(command = Rbin,
          args    =
            # c(system.file("Rscripts", "write_access_db.R",
            c(system.file("Rscripts", "start_remote_odbc_server_access.R",
                          package = "odbc32",
                          mustWork = FALSE),
              accdb_fp,
              sub(pattern     = "localhost",
                  replacement = "*",
                  x           = address),
              port),
          invisible = invisible,
          wait = wait)

  # connect to the r2r server
  r2r::connect(address = address, port = port)
}


#' Start a new R session with a running r2r server
#'
#' @param invisible
#' @param address
#' @param port
#' @param Rbin
#'
#' @return
#' @export
#'
#' @examples
start_server <- function(
  address   = "tcp://localhost",
  port      = "5555",
  Rbin      = file.path(R.home(), "bin", "i386", "Rscript.exe"),
  invisible = FALSE,
  wait      = FALSE
) {
  stopifnot(file.exists(Rbin))

  cmd <- sprintf('"library(r2r);library(RODBC);r2r::server(debug = TRUE)"')
              # accdb_fp,
              # sub(pattern     = "localhost",
              #     replacement = "*",
              #     x           = address),
              #   port),

  system2(command = Rbin,
          args    = c("-e", cmd),
          invisible = invisible,
          wait = wait)
}

# r2r::server(debug = TRUE)
# start_server()
#
# r2r::connect()

#' Connect to an Access 2007 database
#'
#' @param access.file
#' @param uid
#' @param pwd
#' @param quote
#'
#' @return
#' @export
#'
#' @examples
odbcConnectAccess2007_r2r <- function(access.file, uid = "", pwd = "", quote = TRUE) {
  r2r::eval_remote(
    .access_con <- odbcConnectAccess2007(access.file = access.file, uid = uid, pwd = pwd),
    data = list(access.file = access.file, uid = uid, pwd = pwd)
  )
}


#' @export
stop_remote_odbc_server_access <- function() {
  r2r::stop_remote()
}

#' Query over remote connection
#'
#' @param ... is passed to r2r::do.call_remote
#'
#' @return whatever the call on the remote R session returns
#' @export
#'
#' @examples
remote_access_query <- function(...) {
  r2r::do.call_remote(..., args_remote = list(channel = .access_con))
}


#' @export
sqlSave_r2r <- function(data, name = NULL, ...) {
  # RODBC::sqlSave()
  remote_access_query(
    what       = "sqlSave",
    args_local =
      c(
        list(dat       = data,
             tablename = name),
        list(...)
      )
  )
}

#' @export
sqlDrop_r2r <- function(name, ...) {
  # RODBC::sqlDrop()
  remote_access_query(
    what       = "sqlDrop",
    args_local =
      c(
        list(sqtable = name),
        list(...)
      )
  )
}

#' @export
sqlQuery_r2r <- function(query, ...) {
  # RODBC::sqlQuery()
  remote_access_query(
    what       = "sqlQuery",
    args_local =
      c(
        list(query = query),
        list(...)
      )
  )
}


#' @export
sqlTables_r2r <- function(...) {
  # RODBC::sqlTables()
  remote_access_query(
    what       = "sqlTables",
    args_local = list(...)
  )
}


#' @export
sqlGetResults_r2r <- function(...) {
  # RODBC::sqlGetResults()
  remote_access_query(
    what       = "sqlGetResults",
    args_local = list(...)
  )
}


#' @export
sqlUpdate_r2r <- function(data, name = NULL, ...) {
  remote_access_query(
    what       = "sqlQuery",
    args_local =
      c(
        list(dat       = data,
             tablename = name),
        list(...)
      )
  )
}

odbcDataSources_r2r <- function(type = c("all", "user", "system")) {
  remote_access_query(
    what       = "odbcDataSources",
    args_local = list(type = type)
  )
}


sqlFetch_r2r <- function(data, name = NULL, ...) {
  remote_access_query(
    what       = "sqlQuery",
    args_local =
      c(
        list(dat       = data,
             tablename = name),
        list(...)
      )
  )
}

