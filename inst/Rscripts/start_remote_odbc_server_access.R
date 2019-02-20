# ========================================================= #
# Title: r2r server for IO to access db files
# Author:  Vaclav Hausenblas
# DO: ...
# TODO:
# ========================================================= #

# INIT --------------------------------------------------------------------

library(RODBC)
library(stringr)
library(r2r)


# read and check args ---------------------------------------------------------------
script_args <- commandArgs(trailingOnly = TRUE)

# db file name argument
if (length(script_args) >= 1) {
  accdb_file  <- script_args[1]
} else {
  stop("You have to provide accdb file name!")
}

if (!file.exists(accdb_file)) stop(str_interp("Output/access file '${accdb_file}' does not exist!"))

# additional arguments
address <- if (length(script_args) >= 2) script_args[2] else "tcp://*"
port    <- if (length(script_args) >= 3) script_args[3] else pbdZMQ::random_open_port()

# open access db ----------------------------------------------------------

message(str_interp("Connectiong to '${accdb_file}'"))
.access_con <- odbcConnectAccess2007(access.file = accdb_file)


# MAIN: start r2r server ------------------------------------
r2r::server(
  address = address,
  port    = port,
  debug   = TRUE
)



# EXIT --------------------------------------------------------------------

# close connection
odbcClose(.access_con)

# wait for user to see the log
Sys.sleep(time = 15)

message("Bye!")


