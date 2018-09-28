#' Start a new R session with a running r2r server
#'
#' @param address host address
#' @param port    port
#' @param arch    architecture
#' @param Rbin    path to R binary
#' @param invisible logical; visibility of the new session window
#'
#' @return socket connection (invisible)
#' @export
#' @rdname connect
#' @examples
#' #TBA
start_server <- function(
  address   = "tcp://localhost",
  port      = "5555",
  arch      = "i386",
  Rbin      = file.path(R.home(), "bin", arch, "Rscript.exe"),
  invisible = FALSE,
  wait      = FALSE,
  global    = TRUE
) {
  stopifnot(file.exists(Rbin))

  cmd <-
    sprintf(
      '"library(r2r);library(RODBC);library(odbc32);cons <- list();r2r::server(debug = TRUE)"'
    )

  # accdb_fp,
  # sub(pattern     = "localhost",
  #     replacement = "*",
  #     x           = address),
  #   port),

  system2(
    command   = Rbin,
    args      = c("-e", cmd),
    invisible = invisible,
    wait      = wait
  )

  socket <-
    r2r::connect(
      address = address,
      port    = port
    )

  if (global) r2r::save_socket(socket)

  return(invisible(socket))
}


#' Stop r2r server running on a remote R session
#'
#' @param ... args passed to r2r::stop_remote()
#'
#' @return logical
#' @export
#'
#' @examples
#' #TBA
stop_server <- function(...) {
  r2r::stop_remote(...)
}



#' Connect to an Access 2007 database
#'
#' @param access.file path to .accdb file
#' @param uid user name
#' @param pwd password
#'
#' @return odbc32 object
#' @export
#'
#' @examples
#' #TBA
odbcConnectAccess2007 <- function(
  access.file,
  uid    = "",
  pwd    = "",
  socket = .GlobalEnv$.r2r_socket) {

  ref <-
    r2r::eval_remote(
      expr = .append_con(
        RODBC::odbcConnectAccess2007(
          access.file = access.file,
          uid         = uid,
          pwd         = pwd)
      ),
      data = list(
        access.file = access.file,
        uid         = uid,
        pwd         = pwd
      ),
      socket = socket
    )

  new_con <-
    list(
      socket = socket,
      ref    = ref
    )

  class(new_con) <- "odbc32"

  return(new_con)
}

#' @export
odbcConnect <- function(
  dsn,
  uid = "",
  pwd = "",
  socket = .GlobalEnv$.r2r_socket) {

  ref <-
    r2r::eval_remote(
      expr = .append_con(
        RODBC::odbcConnect(
          dsn = dsn,
          uid = uid,
          pwd = pwd)
      ),
      data = list(
        dsn = dsn,
        uid = uid,
        pwd = pwd
      ),
      socket = socket
    )

  new_con <-
    list(
      socket = socket,
      ref    = ref
    )

  class(new_con) <- "odbc32"

  return(new_con)
}


#' @export
get_con <- function(x) {
  UseMethod("get_con", x)
}


#' @export
get_con.odbc32 <- function(con) {
  r2r::eval_remote(
    .get_con(ref),
    data = list(ref = con$ref))
}


#' print method for odbc32
#'
#' @param x odbc32 object
#'
#' @return printed odbc32 object
#' @export
#'
#' @examples
#' #TBA
print.odbc32 <- function(x) {
  cat("ODBC connection on ")
  print(x$socket)
  print(get_con(x))
  print(x$ref)
}


#' @export
odbcClose <- function(con) {
  r2r::eval_remote(
    RODBC::odbcClose(channel = .GlobalEnv$cons[[ref]]),
    data   = list(ref = con$ref),
    socket = con$socket
  )
}

#' Closes odbc32 connection
#'
#' @param con
#'
#' @return
#' @export
#'
#' @examples
#' TBA
close.odbc32 <- function(con) {
  odbcClose(con)
}


#' @export
sqlSave <- function(con, data, name = NULL, ...) {
  r2r::do.call_remote(
    RODBC::sqlSave,
    args_local = c(
      list(
        dat       = data,
        tablename = name
      ),
      list(...)
    ),
    args_remote = list(
      channel = .GlobalEnv$cons[[ref]]
    ),
    data = list(
      ref = con$ref
    ),
    quote = TRUE,
    socket = con$socket
  )
}


#' @export
sqlDrop <- function(con, name) {
  r2r::do.call_remote(
    RODBC::sqlDrop,
    args_local = list(
      sqtable = name
    ),
    args_remote = list(
      channel = .GlobalEnv$cons[[ref]]
    ),
    data = list(
      ref = con$ref
    ),
    quote = TRUE,
    socket = con$socket
  )
}


#' @export
sqlTables <- function(con, ...) {
  r2r::do.call_remote(
    RODBC::sqlTables,
    args_remote =
      list(
        channel = .GlobalEnv$cons[[ref]]
      ),
    args_local = list(...),
    data = list(
      ref = con$ref
    ),
    quote = TRUE,
    socket = con$socket
  )
}
