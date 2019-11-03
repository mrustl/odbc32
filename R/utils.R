#' Path to Rscript.exe (on Windows)
#'
#' @param arch architecture: "i386" for 32 bit or "x64" for 64 bit
#' @return absolute file path to "Rscript.exe"
#'
rscript_path <- function(arch = "i386")
{
  match.arg(arch, c("i386", "x64"))

  file.path(R.home(), "bin", arch, "Rscript.exe")
}

#' Path to File in This Package
#'
#' @param \dots passed to \code{\link{system.file}}
#' @param mustWork passed to \code{\link{system.file}}
#' @return absolute file path to a file in the installation folder of this
#'   package
package_file <- function(..., mustWork = FALSE)
{
  system.file(..., package = "odbc32", mustWork = mustWork)
}
