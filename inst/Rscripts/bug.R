
DBI::dbRemoveTable(con, "df")
DBI::dbCreateTable(con, "df", fields = c("A" = "VARCHAR(255)"))
DBI::dbWriteTable(
  conn = con,
  name = "df",
  value = df,
  append = TRUE)
