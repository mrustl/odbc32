
#' Append a (new) connection to a global list of connections
#'
#' @param con
#'
#' @return a reference (index) number to the list of all connections
#' @export
#'
#' @examples
#' #TBA
.append_con <- function(con) {
  .GlobalEnv$cons[[length(cons) + 1]] <- con

  return(length(.GlobalEnv$cons))
}


#' @export
.get_con <- function(ref) {
  .GlobalEnv$cons[[ref]]
}

