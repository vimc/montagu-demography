source("functions.R")

download_data()

host <- Sys.getenv("MONTAGU_DB_HOST", "support.montagu.dide.ic.ac.uk")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 8888))
host <- "localhost"

db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = "montagu",
                     host = host,
                     port = port,
                     password = "changeme",
                     user = "vimc")

## # Delete all the rows from the tables:
#empty_tables(db)
## # Only do this on test DB if country table isn't already populated.
## init_country_table(db,iso3166)

empty_tables(db)
init_tables(db)
process_all_interpolated_population(db)
