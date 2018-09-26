library(DBI)
library(stringr)


# read and check args ---------------------------------------------------------------

script_args <- commandArgs(trailingOnly = TRUE)


if (length(script_args) >= 4) {
  in_file  <- script_args[1]
  obj_name <- script_args[2]
  out_file <- script_args[3]
  tab_name <- script_args[4]

  if (length(script_args) > 4) drop_first <- as.logical(script_args[5]) else drop_first <- FALSE

} else {
  stop("All four arguments needs to be supplied: 1) input RData file; 2) object name 3) output MS Access file 4) table name!")
}


if (!file.exists(in_file))  stop(str_interp("Source/RData file '${in_file}' does not exist!"))
if (!file.exists(out_file)) stop(str_interp("Output/access file '${out_file}' does not exist!"))



# load data ---------------------------------------------------------------

data_env <- new.env()

load(in_file, envir = data_env)

if (!exists(obj_name, envir = data_env)) stop(str_interp("Object '${obj_name}' not found in '${in_file}'!"))


# open access db ----------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                 dbq = out_file)



# remove if required ------------------------------------------------------

if (drop_first && dbExistsTable(con, tab_name)) {
  message(str_interp("Dropping '${tab_name}' table..."))
  DBI::dbRemoveTable(conn = con, name = tab_name)
}


# write --------------------------------------------------------------------
message(str_interp("Writing '${obj_name}' data from '${in_file}' into '${out_file}' as a '${tab_name}' table..."))

DBI::dbWriteTable(
  conn = con,
  name = tab_name,
  value = as.data.frame(data_env[[obj_name]]),
  row.names = FALSE,
  field.types = "SQL_DOUBLE")


dbDisconnect(conn = con)

#
# save(tmp, file = "U:\\ODO586\\300\\DW\\ICD\\tmp.RData")
message("Done!")


