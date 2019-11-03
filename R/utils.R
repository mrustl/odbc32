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
