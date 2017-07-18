source("functions.R")
host <- Sys.getenv("MONTAGU_DB_HOST", "support.montagu.dide.ic.ac.uk")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 6543))
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = host,
                      port = port,
                      password = "changeme",
                      user = "vimc")
import_demography(con)
