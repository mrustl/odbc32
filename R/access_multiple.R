#' Start a new R session with a running r2r server
#'
#' @param invisible
#' @param address
#' @param port
#' @param Rbin
#'
#' @return
#' @export
#' @rdname connect
#' @examples
start_server <- function(
  address   = "tcp://localhost",
  port      = "5555",
  Rbin      = file.path(R.home(), "bin", "i386", "Rscript.exe"),
  invisible = FALSE,
  wait      = FALSE,
  global    = TRUE
) {
  stopifnot(file.exists(Rbin))

  cmd <- sprintf('"library(r2r);library(RODBC);cons <- list();r2r::server(debug = TRUE)"')
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
odbcConnectAccess2007_r2r <- function(
  access.file,
  uid = "",
  pwd = "",
  quote = TRUE,
  socket = NULL) {

  r2r::eval_remote(
    .access_con <<- odbcConnectAccess2007(access.file = access.file, uid = uid, pwd = pwd),
    data = list(access.file = access.file, uid = uid, pwd = pwd)
  )
}
