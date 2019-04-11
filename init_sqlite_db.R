# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# Utility file containing the code to initialize a new results database 
# and to extract data from it.
#
# DO NOT SOURCE THIS FILE! 
# Instead, pick the components you need.
# ------------------------------------------------------------------------------


library(tidyverse)
library(DBI)
library(lubridate)

# Create empty data base
con <- dbConnect(RSQLite::SQLite(), "croom_exp.sqlite3")
dbCreateTable(con, "answers", c("sys_time" = "text", 
                                "full_cost" = "integers", 
                                "price" = "double", 
                                "time" = "double"))
dbDisconnect(con)

# Read data from data base
con <- dbConnect(RSQLite::SQLite(), "croom_exp.sqlite3")
res <- dbSendQuery(con, "SELECT * FROM answers")
df <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con)

# Store answers within a certain time frame in a new data base for evaluation
answers <- df %>%
  filter(as_datetime(sys_time) < as_datetime("2019-04-11 07:25:00"))
new_con <- dbConnect(RSQLite::SQLite(), "croom_exp_response.sqlite3")
dbWriteTable(new_con, "answers", answers)
DBI::dbListTables(new_con)
dbDisconnect(new_con)


