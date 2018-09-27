# odbc32

The goal of odbc32 package is to allow connections via 32-bit ODBC drivers on a 64-bit R session.


## Installation

You can install the released version of odbc32 from [GitHub](https://github.com/vh-d/odbc32) with:

``` r
devtools::install_github("vh-d/odbc32")
```

## Features

`odbc32` is currently based on the functionality of RODBC package.


## Example

Assuming existence of a DB.accdb file in your current working directory.

``` r
# > sessionInfo()
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1

library(odbc32)
start_server()

con1 <- odbc32::odbcConnectAccess2007("DB.accdb")
sqlTables(con1)
sqlSave(con1, data.frame(id = 1:10, value = rnorm(10), name = "table1"))
sqlTables(con1)
sqlDrop(con1, "table1")
```

