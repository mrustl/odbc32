#' Start a new R session with a running r2r server
#'
#' @param address host address
#' @param port    port
#' @param arch    architecture
#' @param Rbin    path to Rscript binary
#' @param r_args  further arguments to Rscript command
#' @param invisible logical; visibility of the new session window
#'
#' @return socket connection (invisible)
#' @export
#' @rdname connect
#' @examples
#' #TBA
start_server <- function(
  address   = "tcp://localhost",
  port      = pbdZMQ::random_open_port(),
  arch      = "i386",
  Rbin      = file.path(R.home(), "bin", arch, "Rscript.exe"),
  r_args    = NULL, # --vanilla
  invisible = FALSE,
  wait      = FALSE,
  global    = TRUE,
  debug     = FALSE
) {
  stopifnot(file.exists(Rbin))

  cmd <-
    sprintf(
      '"library(r2r);library(RODBC);library(odbc32);cons <- list();r2r::server(port = %i, debug = %s)"', as.integer(port), as.character(debug)
    )

  system2(
    command   = Rbin,
    args      = c(r_args, "-e", cmd),
    invisible = invisible,
    wait      = wait
  )

  socket <-
    r2r::connect(
      address = address,
      port    = as.integer(port)
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

  arguments <- list(access.file = access.file, uid = uid, pwd = pwd)

  ref <- r2r::eval_remote(
    expr = .append_con(do.call(RODBC::odbcConnectAccess2007, arguments)),
    data = arguments,
    socket = socket
  )

  create_odbc32_object(socket, ref)
}

#' establish ODBC connection defined by DSN name
#' @param dsn dsn
#' @param uid user name
#' @param pwd password
#' @param socket socket
#'
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

  create_odbc32_object(socket, ref)
}

#' establish ODBC connection based on a connection string
#' @param ... other args passed to RODBC::odbcDriverConnect()
#' @param socket
#'
#' @export
odbcDriverConnect <- function(
  ...,
  socket = .GlobalEnv$.r2r_socket) {

  # RODBC::odbcDriverConnect()
  ref <-
    r2r::eval_remote(
      expr = .append_con(
        do.call(
          RODBC::odbcDriverConnect,
          args = argslist
        )
      ),
      data = list(
        argslist = list(...)
      ),
      socket = socket
    )

  create_odbc32_object(socket, ref)
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


#' close odbc32 connection
#' @param con odbc32 connection object
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
#' @param con odbc32 connection object
#'
#' @return logical
#' @export
#'
#' @examples
#' TBA
close.odbc32 <- function(con) {
  odbcClose(con)
}


#' list data sources
#' @param type see type param of RODBC::odbcDataSources()
#' @export
odbcDataSources <- function(
  type = c("all", "user", "system"),
  socket = .GlobalEnv$.r2r_socket) {

  r2r::do.call_remote(
    RODBC::odbcDataSources,
    args_local  = list(type = type),
    quote = TRUE,
    socket = con$socket
  )
}




#' drop a db table
#' @param con odbc32 connection object
#' @param name name of db table to be dropped
#'
#' @export
sqlDrop <- function(con, name) {
  result <-
    r2r::do.call_remote(
      RODBC::sqlDrop,
      args_local = list(
        sqtable = name,
        errors = FALSE
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
  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}

#' clear/empty a db table
#' @param con odbc32 connection object
#' @param name name of db table to be dropped
#'
#' @export
sqlClear <- function(con, name) {
  # RODBC::sqlClear()
  result <-
    r2r::do.call_remote(
      RODBC::sqlClear,
      args_local = list(
        sqtable = name,
        errors = FALSE
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
  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}


#' clear/empty a db table
#' @param con odbc32 connection object
#' @param commit logical
#'
#' @export
odbcEndTran <- function(con, commit) {
  # RODBC::odbcEndTran()
  result <-
    r2r::do.call_remote(
      RODBC::odbcEndTran,
      args_local = list(
        commit = commit
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
  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}



#' list tables in DB
#' @param con odbc32 connection object
#' @param errors ignored
#' @param ... other args passed to RODBC::sqlTables() on remote
#'
#' @export
sqlTables <- function(con, errors = NULL, ...) {
  # RODBC::sqlTables
  result <-
    r2r::do.call_remote(
      RODBC::sqlTables,
      args_remote =
        list(
          channel = .GlobalEnv$cons[[ref]]
        ),
      args_local = c(
        list(errors = FALSE),
        list(...)
      ),
      data = list(
        ref = con$ref
      ),
      quote = TRUE,
      socket = con$socket
    )
  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}


#' send query to a db and get results
#' @param con odbc32 connection object
#' @param query text of the query
#' @param ... other args passed to RODBC::sqlQuery() on remote
#'
#' @export
sqlQuery <- function(con, query, errors = NULL, ...) {
  # RODBC::sqlQuery()
  result <-
    r2r::do.call_remote(
      RODBC::sqlQuery,
      args_remote =
        list(
          channel = .GlobalEnv$cons[[ref]]
        ),
      args_local = c(
        list(
          query = query,
          errors = FALSE
        ),
        list(...)
      ),
      data = list(
        ref = con$ref
      ),
      quote = TRUE,
      socket = con$socket
    )

  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}


#' fetch db table
#'
#' @param con odbc32 connection object
#' @param name name of db table to be dropped
#' @param ... other args passed to RODBC::sqlFetch() on remote
#'
#' @return data.frame
#' @export
sqlFetch <- function(con, name, ...) {
  # RODBC::sqlFetch()
  result <-
    r2r::do.call_remote(
      RODBC::sqlFetch,
      args_remote =
        list(
          channel = .GlobalEnv$cons[[ref]]
        ),
      args_local = c(
        list(sqtable = name),
        list(...)
      ),
      data = list(
        ref = con$ref
      ),
      quote = TRUE,
      socket = con$socket
    )
  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}

#' @export
#' @rdname sqlFetch
sqlFetchMore <- function(con, ...) {
  # RODBC::sqlFetchMore()
  result <-
    r2r::do.call_remote(
      RODBC::sqlFetchMore,
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
  if (is.numeric(result) && result == -1) {
    stop(odbcGetErrMsg(con))
  } else
    return(result)
}



#' update db table
#' @param con odbc32 connection object
#' @param data tabular data object
#' @param name name of the resulting db table
#' @param ... other args passed to RODBC::sqlUpdate() on remote
#'
#' @export
sqlUpdate <- function(con, data, name = NULL, ...) {
  # RODBC::sqlUpdate()
  r2r::do.call_remote(
    RODBC::sqlUpdate,
    args_remote =
      list(
        channel = .GlobalEnv$cons[[ref]]
      ),
    args_local = c(
      list(dat       = data,
           tablename = name),
      list(...)
    ),
    data = list(
      ref = con$ref
    ),
    quote = TRUE,
    socket = con$socket
  )
}



#' @title upload table to a db
#'
#' @param con odbc32 object
#' @param data tabular data
#' @param name name of the resulting db table
#' @param ... other args passed to RODBC::sqlSave() on remote
#'
#' @export
sqlSave <- function(con, data, name = NULL, ...) {
  # RODBC::sqlSave()
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


#' Information on an ODBC connection.
#'
#' @description see RODBC::odbcGetInfo()
#'
#' @param con odbc32 connection object
#' @export
odbcGetInfo <- function(con) {
  r2r::do.call_remote(
    RODBC::odbcGetInfo,
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

#' @rdname setSqlTypeInfo
#' @export
#' @examples
getSqlTypeInfo <- function(con, driver) {
  r2r::do.call_remote(
    RODBC::getSqlTypeInfo,
    args_local = list(
      driver = driver
    ),
    quote = TRUE,
    socket = con$socket
  )
}


#' Specify or retrieve a mapping of R types to DBMS datatypes.
#'
#' @param con odbc32 object
#' @param driver
#' @param value passed to \code{RODBC::setSqlTypeInfo}
#'
#' @return
#' For setSqlTypeInfo none.
#' For getSqlTypeInfo with an argument, a named list. Without an argument, a data frame.
#' @export
setSqlTypeInfo <- function(con, driver, value) {
  r2r::do.call_remote(
    RODBC::setSqlTypeInfo,
    args_local = list(
      driver = driver,
      value = value
    ),
    quote = TRUE,
    socket = con$socket
  )
}

#' @rdname setSqlTypeInfo
#' @details \code{setSqlTypeInfo2} is a custom implementation for cases when setSqlTypeInfo fails on "locked environment" error
#' @export
setSqlTypeInfo2 <- function(con, driver, value) {
  r2r::do.call_remote(
    what = assign,
    args_remote = list(
      envir = RODBC:::typesR2DBMS
    ),
    args_local = list(
      x     = driver,
      value = value
    ),
    quote = TRUE,
    socket = con$socket)
}



#' Get error message
#'
#' @param con odbc32 object
#'
#' @export
odbcGetErrMsg <- function(con) {
  r2r::do.call_remote(
    RODBC::odbcGetErrMsg,
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
