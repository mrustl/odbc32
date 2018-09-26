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
