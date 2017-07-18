source("functions.R")
host <- Sys.getenv("MONTAGU_DB_HOST", "localhost")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 8888))
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = host,
                      port = port,
                      password = "changeme",
                      user = "vimc")
import_demography(con)
