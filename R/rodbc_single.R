
#' Start r2r server connected to an Access database over 32-bit ODBC driver
#' @export
start_server_access2007 <- function(
  accdb_fp,
  invisible = FALSE,
  wait      = FALSE,
  address   = "tcp://localhost",
  port      = "5555",
  arch      = "i386",
  Rbin      = file.path(R.home(), "bin", arch, "Rscript.exe")
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
#' #TBA
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
    what       = "sqlUpdate",
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

