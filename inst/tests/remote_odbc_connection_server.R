library(RODBC)
access_con <- odbcConnectAccess2007(access.file = "N:\\temp_access.accdb")

odbc_server(db_con = access_con)