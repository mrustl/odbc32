library(RODBC)
library(stringr)


# read and check args ---------------------------------------------------------------

script_args <- commandArgs(trailingOnly = TRUE)


if (length(script_args) >= 1) {
  in_file  <- script_args[1]
} else {
  stop("You have to provide input RData file name!")
}


if (!file.exists(in_file))  stop(str_interp("Source/RData file '${params$in_file}' does not exist!"))


# load and check data ---------------------------------------------------------------

load(in_file)

stopifnot(exists("data"))
stopifnot(is.list(data))

stopifnot(exists("params"))
stopifnot(is.list(params))

if (!file.exists(params$accdb_file)) stop(str_interp("Output/access file '${params$accdb_file}' does not exist!"))

# quit early if the data list is empty
if (length(data) == 0) quit()


# open access db ----------------------------------------------------------

.access_con <- odbcConnectAccess2007(access.file = params$accdb_file)

# write --------------------------------------------------------------------

message(str_interp("Writing into '${params$accdb_file}'"))

for (tab_name in names(data)) {

  if (length(params$overwrite) && params$overwrite) {
    message(str_interp("Trying to drop '${tab_name}'"))
    try(RODBC::sqlDrop(.access_con, tab_name))
  }

  # convert Date columns as characters?
  if (!is.null(params$convert_dates)) {
    is_date <- sapply(X = data[[tab_name]], FUN = methods::is, class2 = "Date")
    if (any(is_date)) {
      for (i in which(is_date)) {
        try(data[[tab_name]][[i]] <- as.character.Date(data[[tab_name]][[i]], format = params$convert_dates)) # typically "%Y%m%d" or "%d%m%Y"
      }
    }
    # try(RODBC::sqlDrop(.access_con, tab_name))
  }

  message(str_interp("Writing '${tab_name}'"))

  try(
    do.call(
      what = RODBC::sqlSave,
      args = c(
        list(
          channel   = .access_con,
          dat       = as.data.frame(data[[tab_name]]),
          tablename = tab_name
        ),
        params[setdiff(names(params), c("convert_dates", "accdb_file", "overwrite"))]
      )
    )
  )
}

odbcClose(.access_con)

#
# save(tmp, file = "U:\\ODO586\\300\\DW\\ICD\\tmp.RData")
message("Done!")


