

# client ------------------------------------------------------------------

library(r2r)
library(odbc32)

odbc32::start_remote_odbc_server_access(accdb_fp = "N:\\temp.accdb")

# r2r::connect(port = 5555)

r2r::eval_remote(RODBC::sqlTables(channel = .access_con))

remote_access_query(
  what       = "sqlSave",
  args_local = list(dat = data.frame(a = 1:10),
                    tablename = "tabulka"))

remote_access_query(
  what       = "sqlQuery",
  args_local = list(query = "select * from tabulka"))

remote_access_query(
  what       = "sqlDrop",
  args_local = list(sqtable = "tabulka"))

remote_access_query(what = "sqlTables")


dtFA <-
  remote_access_query(
    what       = "sqlQuery",
    args_local = list(query = "select * from FA_S"))

library(data.table)
setDT(dtFA)
tables()


# r2r:::break_remote()
# r2r::stop_remote()
odbc32::stop_remote_odbc_server_access()
