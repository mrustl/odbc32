#' @exportClass odbc32
"_PACKAGE"

#' Create Object of Class "odbc32"
#'
#' @param socket socket
#' @param ref reference returned by \code{\link[r2r]{eval_remote}}
#'
create_odbc32_object <- function(socket, ref) {

  result <- list(socket = socket, ref = ref)

  class(result) <- "odbc32"

  result
}
