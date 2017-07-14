source("functions.R")

countries_keep <- readLines("countries_keep.txt")

download_data()

host <- Sys.getenv("MONTAGU_DB_HOST", "support.montagu.dide.ic.ac.uk")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 6543))

iso3166 <- xml2::read_xml("data/iso3166.xml")
xml_countries <- xml2::xml_find_all(iso3166, "//c")
iso3166_df<- data.frame(id = as.numeric(xml2::xml_attr(xml_countries, "n3")),
                        code = xml2::xml_attr(xml_countries, "c3"),
                        stringsAsFactors = FALSE)

# Filter to only include countries we care about.

iso3166_df_97 <- iso3166_df[iso3166_df$code %in% countries_keep, ]

db <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = host,
                      port = port,
                      password = "changeme",
                      user = "vimc")

## # Delete all the rows from the tables:
empty_tables(db)

## # Initialise the enum tables
init_tables(db)

## # Only do this on test DB if country table isn't already populated.
init_country_table(db,iso3166_df)

variant_names <- c("ESTIMATES","MEDIUM_VARIANT")
sheet_names_2015 <- c("ESTIMATES","MEDIUM VARIANT")
sheet_names_2012 <- c("ESTIMATES","MEDIUM FERTILITY")

#2015
#ans <- process_interpolated_population(db,
#                                  "data/wpp2015/WPP2015_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLS",
#                                  "BOTH", sheet_names_2015, variant_names, "UNWPP_2015", iso3166_df)

Sys.time()
process_all_interpolated_population(db, iso3166_df_97)
Sys.time()

dbGetQuery(db, "SELECT * from demographic_statistic where id<100")

rs <- dbSendQuery(db, "SELECT * from demographic_statistic")
dbFetch(rs)
