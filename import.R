source("functions.R")
host <- Sys.getenv("MONTAGU_DB_HOST", "support.montagu.dide.ic.ac.uk")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 8888))
host <- "localhost"
db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = "montagu",
                     host = host,
                     port = port,
                     password = "changeme",
                     user = "vimc")
import_demography(db)
