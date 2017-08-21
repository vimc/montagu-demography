source("functions.R")

host <- Sys.getenv("MONTAGU_DB_HOST", "localhost")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 8888))
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = host,
                      port = port,
                      password = "changeme",
                      user = "vimc")




#import_demography(con, test_code = "july28_test")
import_demography(con, test_code = "")

# Create the Neonatal Mortality dataset out of WPP IMR * (CM NMR/IMR ratio)
# Depends on UNWPP demographic indicators already being present...
create_nmr(con, 
           wpp_source = "unwpp_2017", 
           cm_file = "data/cm2015/ratesdeaths_allindicators.xlsx",
           cm_sheet = "Rates and Deaths",
           new_source = "unwpp2017_cm2015_hybrid",
           new_variant = "wpp_cm_hybrid")

