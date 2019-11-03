#' export data from Rdata file into MS Access db via separate 32-bit Rscript
#' @export
write_access_db <- function(rdata_file) {

  system2(command = rscript_path("i386"),
          args    = c(
            # package_file("Rscripts", "write_access_db.R)",
            package_file("Rscripts", "write_access_db_rodbc.R"),
            rdata_file))
}

#' @title writing data into MS Access via 32bit R session script
#' @description Procedure for easier exporting of data from an R into MS Access database via separate 32-bit R session script
#'
#' @param data a named list of tables
#' @param accdb_file path to accdb file
#' @param overwrite logical; TRUE for an attempt to drop each table first.
#' @param fast passed to RDOBC::sqlSave
#' @param safer passed to RDOBC::sqlSave
#' @param rownames passed to RDOBC::sqlSave
#' @param ... passed to RDOBC::sqlSave
#'
#' @details
#' \describe{
#'   \item{overwrite}{logical; Attempt to drop any existing table of the name before writing data?}
#'   \item{convert_dates}{character; Convert dates as text before writing into database.}
#' }
#' @export
data2access <- function(
  data,
  accdb_file,
  overwrite = FALSE,
  fast      = TRUE,
  safer     = TRUE,
  rownames  = FALSE,
  ...) {

  # check arguments
  if (!is.list(data)) stop(substitute(data), " needs to be a named list!")
  if (sum(names(data) != "") != length(data)) stop(substitute(data), " needs to be a named list!")

  # save any options as a list
  params = list(
    accdb_file = accdb_file,
    overwrite  = overwrite,
    fast       = fast,
    safer      = safer,
    rownames   = rownames,
    ...)

  # create a temporary file for writing the data on disk
  tmpf <- tempfile("rdata")
  on.exit(unlink(tmpf))

  save(data, params, file = tmpf)

  # call R script to write data from temporary file into an Access database
  write_access_db(rdata_file = tmpf)

  return(TRUE)
}
