
# init_country_table populates an empty country table with 
# the 3-digit alpha code for both name and country, just so
# the demographic statistic "country" field has something to
# link to.

init_country_table <- function(db,iso3166) {
  DBI::dbExecute(db,"DELETE FROM country")
  country <- data.frame(id = iso3166$code,
                        name = iso3166$code,
                        stringsAsFactors = FALSE)
  DBI::dbWriteTable(db, "country", country, append = TRUE)
}

# For dev only:
# Code to empty the tables. (Not drop them)

empty_tables <- function(db) {
  rs<-DBI::dbSendStatement(db,"DELETE FROM demographic_statistic");
  rs<-dbClearResult(rs)
  rs<-DBI::dbSendStatement(db,"DELETE FROM gender");  
  rs<-dbClearResult(rs)
  rs<-DBI::dbSendStatement(db,"DELETE FROM projection_variant");
  rs<-dbClearResult(rs)
  rs<-DBI::dbSendStatement(db,"DELETE FROM source");
  rs<-dbClearResult(rs)
  rs<-DBI::dbSendStatement(db,"DELETE FROM demographic_statistic_type");
  rs<-dbClearResult(rs)
}

# init_tables
# Adds identifiers for the gender, projection, demographic_statistic_type
# and source tables.

init_tables <- function(db) {
  gender_table <- data.frame(c("BOTH","MALE","FEMALE"),
                             c("Both","Male","Female"))
  colnames(gender_table) <- c("id","name")
  DBI::dbWriteTable(db,"gender",gender_table, append=TRUE)



  projection_table <- data.frame(c("UNWPP_ESTIMATES","UNWPP_MEDIUM_VARIANT","UNWPP_HIGH_VARIANT",
                                   "UNWPP_LOW_VARIANT","UNWPP_CONSTANT_FERTILITY","UNWPP_INSTANT_REPLACEMENT",
                                   "UNWPP_MOMENTUM","UNWPP_ZERO_MIGRATION","UNWPP_CONSTANT_MORTALITY",
                                   "UNWPP_NO_CHANGE"),
                                 c("UNWPP Estimates","UNWPP Medium Variant","UNWPP High Variant",
                                   "UNWPP Low Variant","UNWPP Constant Fertility","UNWPP Instant Replacement",
                                   "UNWPP Momentum","UNWPP Zero Migration","UNWPP Constant Mortality",
                                   "UNWPP No Change"))
  colnames(projection_table) <- c("id","name")

  DBI::dbWriteTable(db,"projection_variant",projection_table, append=TRUE)


  demographic_statistic_type_table <- data.frame(c("INT_POP"),c("Age (years)"),c("Interpolated Population"))
  colnames(demographic_statistic_type_table) <- c("id","age_interpretation","name")

  DBI::dbWriteTable(db,"demographic_statistic_type",demographic_statistic_type_table, append=TRUE)


  source_table <- data.frame(c("UNWPP_2012","UNWPP_2015","UNWPP_2017"),
                             c("UNWPP 2012","UNWPP 2015","UNWPP 2017"))
  colnames(source_table) <- c("id","name")

  DBI::dbWriteTable(db,"source",source_table, append=TRUE)
}


download_single <- function(url, dest) {
  if (!file.exists(dest)) download.file(url,dest,method='libcurl', mode="wb")
}

download_data <- function() {
  if (!file.exists('data')) dir.create('data')
  if (!file.exists('data/wpp2012')) dir.create('data/wpp2012')
  if (!file.exists('data/wpp2015')) dir.create('data/wpp2015')
  if (!file.exists('data/wpp2017')) dir.create('data/wpp2017')
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2012/WPP2012_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xls","data/wpp2012/WPP2012_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLS")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2012/WPP2012_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xls","data/wpp2012/WPP2012_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLS")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2012/WPP2012_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xls","data/wpp2012/WPP2012_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2015/WPP2015_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLS","data/wpp2015/WPP2015_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLS")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2015/WPP2015_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLS","data/wpp2015/WPP2015_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLS")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2015/WPP2015_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS","data/wpp2015/WPP2015_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2017/WPP2017_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx","data/wpp2017/WPP2017_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLSX")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2017/WPP2017_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx","data/wpp2017/WPP2017_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLSX")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/unwpp/wpp2017/WPP2017_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx","data/wpp2017/WPP2017_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLSX")
  download_single("https://mrcdata.dide.ic.ac.uk/resources/iso3166.xml","data/iso3166.xml")
}

process_interpolated_population <- function(db, xlfile, gender, sheets,
                                            variant_names,source, iso3166) {
  reshape <- function(x, cols) {
    age_to <- age_from <- seq_along(cols) - 1L
    age_to[length(age_to)] <- 120L
    nr <- nrow(x)
    nc <- length(cols)
    if (nrow(x) == 0) {
      ## This avoids indexing by non-existant columns
      value <- numeric(0)
    } else {
      value <- unlist(x[cols], use.names = FALSE)
    }
    data.frame(age_from = rep(age_from, each = nr),
               age_to = rep(age_to, each = nr),
               year = rep(x$year, nc),
               country = x$iso3,
               value = value)
  }
  read_sheet <- function(sheet, variant) {
    
    message(sprintf("Reading %s:%s", xlfile, sheet))
    t <- -(as.numeric(Sys.time()))
    xl <- read_excel(xlfile, sheet = sheet, skip = 16, col_names = TRUE,
                     na = c("", "…"))
    t <- t+(as.numeric(Sys.time()))
    message(sprintf("E-Time: %f",t*1000))
    
    t <- -(as.numeric(Sys.time()))
    age_cols_pre_1990 <- as.character(c(0:79,"80+"))
    
    # Column "100+" has been renamed to "100" in UNWPP 2017.
    if ("100+" %in% colnames(xl)) {
      age_cols_from_1990 <- as.character(c(0:99,"100+"))
    } else {
      age_cols_from_1990 <- as.character(c(0:100))
    }
    
    
    message("...processing")
    xl$iso3 <- iso3166$code[match(xl$"Country code", iso3166$id)]
    xl <- as.data.frame(xl[!is.na(xl$iso3), ])
    xl$year <- xl[[6]]

    res <- rbind(reshape(xl[xl$year < 1990, ], age_cols_pre_1990),
                 reshape(xl[xl$year >= 1990, ], age_cols_from_1990))
    row.names(res) <- NULL
    res$projection_variant <- variant
    res$date_start <- sprintf("%d-07-01", res$year)
    res$date_end <- sprintf("%d-06-30", res$year + 1)
    res$year<-NULL
    res$gender<-gender
    res$source<-source
    res$demographic_statistic_type<-"INT_POP"
    t <- t+(as.numeric(Sys.time()))
    message(sprintf("P-Time: %f",t*1000))
    res
    
  }

  t <- -(as.numeric(Sys.time()))
  
  for (i in seq_along(sheets)) {
    x <- read_sheet(sheets[[i]], variant_names[[i]])
    dbWriteTable(db, "demographic_statistic", x, append = TRUE)
  }
  
  t <- t+(as.numeric(Sys.time()))
  message(sprintf("DBTime: %f",t*1000))
  
}

process_interpolated_population_2012 <- function(db, iso3166) {

  variant_names <- c("UNWPP_ESTIMATES","UNWPP_MEDIUM_VARIANT")
  sheet_names <- c("ESTIMATES","MEDIUM FERTILITY")
  
  process_interpolated_population(db,
                      "data/wpp2012/WPP2012_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLS",
                      "BOTH",sheet_names, variant_names, "UNWPP_2012",iso3166)
  
  process_interpolated_population(db,
                      "data/wpp2012/WPP2012_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLS",
                      "MALE",sheet_names, variant_names, "UNWPP_2012",iso3166)
  
  process_interpolated_population(db,
                      "data/wpp2012/WPP2012_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS",
                      "FEMALE",sheet_names, variant_names, "UNWPP_2012",iso3166)
}

process_interpolated_population_2015 <- function(db, iso3166) {
  
  variant_names <- c("UNWPP_ESTIMATES","UNWPP_MEDIUM_VARIANT")
  sheet_names <- c("ESTIMATES","MEDIUM VARIANT")
  
  process_interpolated_population(db,
                      "data/wpp2015/WPP2015_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLS",
                      "BOTH", sheet_names, variant_names, "UNWPP_2015", iso3166)
  
  process_interpolated_population(db,
                      "data/wpp2015/WPP2015_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLS",
                      "MALE", sheet_names, variant_names, "UNWPP_2015",iso3166)
  
  process_interpolated_population(db,
                      "data/wpp2015/WPP2015_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS",
                      "FEMALE", sheet_names, variant_names, "UNWPP_2015",iso3166)
}
  
process_interpolated_population_2017 <- function(db, iso3166) {
  
  variant_names <- c("UNWPP_ESTIMATES","UNWPP_MEDIUM_VARIANT")
  sheet_names <- c("ESTIMATES","MEDIUM VARIANT")
  
  process_interpolated_population(db,
                      "data/wpp2017/WPP2017_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.XLSX",
                      "BOTH", sheet_names, variant_names, "UNWPP_2017", iso3166)
  
  process_interpolated_population(db,
                      "data/wpp2017/WPP2017_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.XLSX",
                      "MALE", sheet_names,variant_names, "UNWPP_2017", iso3166)
  
  process_interpolated_population(db,
                      "data/wpp2017/WPP2017_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLSX",
                      "FEMALE", sheet_names, variant_names, "UNWPP_2017", iso3166)
}

process_all_interpolated_population <- function(db, iso3166) {
  
  process_interpolated_population_2012(db,iso3166);
  process_interpolated_population_2015(db,iso3166);
  process_interpolated_population_2017(db,iso3166);

}

read_excel <- function(...) {
  oo <- options(warnPartialMatchArgs = FALSE)
  if (!is.null(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  readxl::read_excel(...)
}
