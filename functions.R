import_demography <- function(con, clear_first = TRUE, test_code = "") {
  download_data(test_code)
  if (clear_first) {
    empty_tables(con)
  }
  
  init_tables(con)
  process_all_population(con, test_code = test_code)
  DBI::dbExecute(con, "VACUUM")
}

# For dev only:
# Code to empty the tables. (Not drop them)
empty_tables <- function(con) {
  tables <- c("demographic_statistic", "demographic_value_unit", 
              "touchstone_demographic_source",
              "gender", "demographic_variant", "demographic_source",
              "demographic_statistic_type", "demographic_statistic_type_variant")
  tables_str <- paste(tables, collapse = ", ")
  sql <- paste("TRUNCATE", tables_str)
  message(sprintf("Clearing %s", tables_str))
  DBI::dbExecute(con, sql)
}

table_is_empty <- function(con, tbl) {
  nrow(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 1", tbl))) == 0L
}

# init_tables
# Adds identifiers for the gender, demographic_variant, demographic_statistic_type,
# demographic_source tables, demographic_statistic_type_variant and demographic_value_unit tables.
init_tables <- function(con) {
  tables <- c("gender", "demographic_value_unit", "demographic_variant",
              "demographic_source")
  for (table in tables) {
    upload_csv(con, file.path("meta", paste0(table, ".csv")))
  }
  upload_csv(con, "montagu-db/minimal/common/country.csv")
  
  # Special for demographic_statistic type, to deal with foreign keys
  # for default_variant field.
  
  dst_csv <- read_csv("meta/demographic_statistic_type.csv")
  db_dv <- DBI::dbGetQuery(con, "SELECT * from demographic_variant")
  db_unit <- DBI::dbGetQuery(con, "SELECT * from demographic_value_unit")
  dst_csv$default_variant <- db_dv$id[match(dst_csv$default_variant, db_dv$code)]
  dst_csv$demographic_value_unit <- db_unit$id[match(dst_csv$demographic_value_unit, db_unit$name)]  
  DBI::dbWriteTable(con, "demographic_statistic_type", dst_csv, append = TRUE)
  
  # Same for demographic_statistic_type_variant
  
  dstv_csv <- read_csv("meta/demographic_statistic_type_variant.csv")
  db_dst <- DBI::dbGetQuery(con, "SELECT * from demographic_statistic_type")
  dstv_csv$demographic_statistic_type <- db_dst$id[match(dstv_csv$demographic_statistic_type,db_dst$code)]
  dstv_csv$demographic_variant <- db_dv$id[match(dstv_csv$demographic_variant, db_dv$code)]
  DBI::dbWriteTable(con, "demographic_statistic_type_variant", dstv_csv, append = TRUE)
  
}

## NOTE: this does not necessarily do error handling properly:
download_single <- function(url, dest) {
  if (!file.exists(dest)) {
    download.file(url, dest, method = 'libcurl', mode = "wb")
  }
}

download_data <- function(test_code = "") {
  if (test_code=="") {
    files <- read.csv("meta/files.csv", stringsAsFactors = FALSE)
  } else if (test_code=="july28_test") {
    files <- read.csv("meta/files_july28test.csv", stringsAsFactors = FALSE)
  } else {
    stop(sprintf("test_code %s not recognised", test_code))
  }
  
  for (p in unique(dirname(files$filename))) {
    dir.create(p, FALSE, TRUE)
  }
  for (i in seq_len(nrow(files))) {
    download_single(files$url[[i]], files$filename[[i]])
  }
}

process_population <- function(con, xlfile, gender, sheet_names,
                               remove_year, variant_names, dsource, data_type,
                               country_tr, test_code = "") {

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
  
  reshape_qq <- function(x, cols) {
    
    cols_rename_last <- cols
    
    if (!is.na(match("80+",cols_rename_last))) {
      cols_rename_last[match("80+",cols_rename_last)] <- "80-120"
    }
    
    if (!is.na(match("100+",cols_rename_last))) {
      cols_rename_last[match("100+",cols_rename_last)] <- "100-120"
    }
    
    
    age_from <- unlist( lapply( strsplit( cols_rename_last,"-"), `[[`, 1))
    age_to <- unlist( lapply( strsplit( cols_rename_last,"-"), `[[`, 2))
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
  
  read_sheet <- function(sheet, skip= 16, country_col = "Country code") {
    message(sprintf("Reading %s:%s", xlfile, sheet))
    xl <- read_excel(xlfile, sheet = sheet, skip = skip, col_names = TRUE, na = c("", "…"))
    xl$iso3 <- country_tr$id[match(xl[[country_col]], country_tr$code)]
    as.data.frame(xl[!is.na(xl$iso3), ])
  }

  process_life_table <- function(xl, variant, gender, col_names, var_names, dsource) {
    # Remove unwanted columns
    
    cols_wanted <- c("Period","iso3", "Age (x)", "Age interval (n)", col_names)
    xl<-xl[match(cols_wanted, names(xl))]
    data_cols <- match(col_names, names(xl))
    
    age_from<-as.numeric(xl$"Age (x)")
    age_int<-as.numeric(xl$"Age interval (n)")
    age_to<-age_from+(age_int-1)
                  

    res<- data.frame(
      year = as.numeric(substring(xl$"Period",1,4)),
      gender = gender,
      demographic_variant = variant,
      demographic_source = dsource,
      demographic_statistic_type = rep(var_names, each=nrow(xl)),
      age_from = age_from,
      age_to = age_to,
      country = xl$iso3,
      value = unlist(xl[data_cols]),
      stringsAsFactors = FALSE
    )
    row.names(res) <- NULL
    
    # An issue with the "..." single character these files contain for missing data...
    # Can't seem to make R compare to it, to remove those entries. Here is a hacked
    # workaround...
    
    res<-res[ suppressWarnings( !is.na( as.numeric(res$value))) , ]
    
    res
  }

  process_annual_indicators <- function(xl, variant, col_names, genders, var_names, transforms, dsource, remove_year) {
    
    # Rename absurd date column.
    
    names(xl)[match("Reference date (1 January - 31 December)",names(xl))]<-"year"
    
    # Remove unwanted columns
    
    cols_wanted <- c("year","iso3",col_names)
    xl<-xl[match(cols_wanted, names(xl))]
    data_cols <- match(col_names, names(xl))
    
    # Remove unwanted years
    
    if (as.numeric(remove_year) > 0) {
      xl<-xl[!(xl$year %in% remove_year), ]
    }
    
    # Apply data transforms (1000s to units)
    
    xl[data_cols]<-mapply(`*`,xl[data_cols],transforms)

    res<- data.frame(
      year = as.numeric(xl$year),
      gender = rep(genders, each = nrow(xl)),
      demographic_variant = variant,
      demographic_source = dsource,
      demographic_statistic_type = rep(var_names, each=nrow(xl)),
      age_from = 0,
      age_to = 0,
      country = xl$iso3,
      value = unlist(xl[data_cols]),
      stringsAsFactors = FALSE
    )
    res$age_to[res$demographic_statistic_type=="unwpp_u5mr"] <- 4
    row.names(res) <- NULL
    res
  }
  
  
  process_age_specific_mortality_sheet <- function(xl, variant, data_type) {
    
    # Replace eg. "95+" with "95-120"
    
    last_age_index <- grep("[+]", names(xl))

    if (length(last_age_index) == 1) {
      new_colname <- gsub("\\+", "-120", names(xl)[last_age_index])
      names(xl)[last_age_index] <- new_colname
    }
    
    age_indexes <- c(as.numeric(grep(RE_AGE_SPAN, names(xl))))
    age_cols <- names(xl)[age_indexes]
    
    if (age_indexes[[1L]] != 7L) {
      stop("Unexpected data format!")
    }
    
    # Seperate ages into {age_from} - {age_to}
    
    age_from <- unlist( lapply( strsplit( age_cols,"-"), `[[`, 1))
    age_to <- unlist( lapply( strsplit( age_cols,"-"), `[[`, 2))
    
    res<- data.frame(
      year = as.numeric( substring( unique( xl$"Period"), 1, 4)),
      age_from = rep(age_from, each = nrow(xl)),
      age_to = rep(age_to, each = nrow(xl)),
      value = unlist(xl[age_cols])*1000,
      country = xl$iso3,
      stringsAsFactors = FALSE
    )
    process_shared(res, gender, dsource, variant, data_type)
  }
  
  process_age_specific_fertility_sheet <- function(xl, variant, data_type) {
    #Column 6 Title Period (yyyy-yyyy)
    #Column 7 onwards: age (aa-bb)
    
    age_indexes <- as.numeric(grep(RE_AGE_SPAN, names(xl)))
    if (age_indexes[[1L]] != 7L) {
      stop("Unexpected data format!")
    }
    
    age_cols <- names(xl)[age_indexes]
    start_age <- as.integer(substr(age_cols,1,2))
    
    start_years = as.numeric(substring(unique(xl$"Period"),1,4))
    no_countries <- length(unique(xl$iso3))
    no_ages <- length(age_cols)
    no_years <- length(start_years)
    
    res <- data.frame(
      year = rep(start_years, no_ages * no_countries),
      country = rep( rep(unique(xl$iso3), each=no_years), no_ages),
      value = unlist(xl[age_cols])/1000,
      age_from = rep(start_age, each=no_countries * no_years),
      age_to = rep(start_age+4, each=no_countries * no_years),
      stringsAsFactors = FALSE)
    process_shared(res, gender, dsource, variant, data_type)
  }

  process_5yearly_single_sheet <- function(xl, variant, data_type, mult_value) {
    year_indexes <- as.numeric(grep(RE_YEAR_SPAN, names(xl)))
    if (year_indexes[[1L]] != 6L) {
      stop("Unexpected data format!")
    }
    
    year_cols <- names(xl)[year_indexes]
    start_years <- as.integer(substr(year_cols,1,4))
    
    
    res <- data.frame(
      year = rep(start_years, each = length(unique(xl$iso3))),
      country = rep(xl$iso3, length(year_cols)),
      value = unlist(xl[year_cols]),
      stringsAsFactors = FALSE)
    
    res$age_from <- 0
    res$age_to   <- 0
    res$value    <- res$value * mult_value
    
    process_shared(res, gender, dsource, variant, data_type)
  }
  
  process_interpolated_population_sheet <- function(xl, variant, data_type, remove_year, test_code = "") {
    
    
    if (test_code == "") {
      age_cols_pre_1990 <- as.character(c(0:79, "80+"))
   
      # Column "100+" has been renamed to "100" in UNWPP 2017 interpolated data.
   
      if ("100+" %in% colnames(xl)) {
        age_cols_from_1990 <- as.character(c(0:99, "100+"))
      } else {
        age_cols_from_1990 <- as.character(c(0:100))
      }
    
    } else if (test_code == "july28_test") {
      age_cols_pre_1990 <- c("0","1","10")
      age_cols_from_1990 <- c("0","1","10")
    
    } else {
      stop(sprintf("test_code %s not recognised", test_code))
    }
    
    xl$year <- xl[[6]]
    if (as.numeric(remove_year)>0) {
      xl<-xl[!(xl$year %in% remove_year), ]
    }
    

    res <- rbind(reshape(xl[xl$year <  1990, ], age_cols_pre_1990),
                 reshape(xl[xl$year >= 1990, ], age_cols_from_1990))
    
    
    
    res$value <- res$value * 1000
    
    process_shared(res, gender, dsource, variant, data_type)
  }
  
  process_qq_population_sheet <- function(xl, variant, data_type, remove_year) {
    
    # 5-yearly time and age data, but multi-variant.
    #
    # ESTIMATES page has years 1950..2015 step 5.
    #   For years up to 1985, ages 0..4 until 75..79 and 80+
    #   For years from 1990, age 0..4 until 95.99 and 100+
    #
    # ALL OTHER VARIANT PAGES: years 2015..2100 step 5. (Note dup 2015)
    #   Years 0..4 until 95..99 and 100+ - and "80+" column is absent.
    
    # Rename year column, and remove duplicate year rows:
    
    names(xl)[match("Reference date (as of 1 July)",names(xl))]<-"year"
    if (as.numeric(remove_year)>0){
      xl<-xl[!(xl$year %in% remove_year), ]
    }
    
    # Expected age columns:-
    
    age_start_pre_1990 <- seq(0,75,5)
    age_end_pre_1990 <- seq(4,79,5)
    age_cols_pre_1990 <- c(paste(age_start_pre_1990,age_end_pre_1990,sep="-"),"80+")
    
    age_start_from_1990 <- seq(0,95,5)
    age_end_from_1990 <- seq(4,99,5)
    age_cols_from_1990 <- c(paste(age_start_from_1990,age_end_from_1990,sep="-"),"100+")
    
    res <- rbind(reshape_qq(xl[xl$year <  1990, ], age_cols_pre_1990),
                 reshape_qq(xl[xl$year >= 1990, ], age_cols_from_1990))
    
    res$value <- res$value * 1000
    process_shared(res, gender, dsource, variant, data_type)
  }
  
  process_total_population_sheet <- function(xl, variant, data_type, remove_year) {
    if (as.numeric(remove_year)>0){
      xl[[remove_year]]<-NULL
    }
    
    i <- grep(RE_YEAR, names(xl))
    if (i[[1L]] != 6L) {
      stop("Unexpected data format!")
    }
    year_cols <- names(xl)[i]

    res <- data.frame(
      year = rep(as.integer(year_cols), each = length(unique(xl$iso3))),
      country = rep(xl$iso3, length(year_cols)),
      value = unlist(xl[year_cols]),
      stringsAsFactors = FALSE)
    res$age_from <- 0
    res$age_to   <- 120
    res$value <- res$value * 1000
    process_shared(res, gender, dsource, variant, data_type)
  }

  ## Common behaviour for all process functions
  process_shared <- function(res, gender, dsource, variant, data_type) {
    row.names(res) <- NULL    
    res$demographic_variant <- variant
    res$gender <- gender
    res$demographic_source <- dsource
    res$demographic_statistic_type <- data_type
    res
  }
  
  upload_data <- function(d) {
    ## TODO: this can be done a bit more efficiently, but this is OK for now
    ## Manually satisfy FK constraints:
    fks <- c("gender", "demographic_source", "demographic_variant",
             "demographic_statistic_type")
    meta <- setNames(lapply(fks, function(x) DBI::dbReadTable(con, x)), fks)
    for (fk in names(meta)) {
      i <- match(d[[fk]], meta[[fk]]$code)
      if (any(is.na(i))) {
        stop("Foreign key violation for ", fk)
      }
      d[[fk]] <- meta[[fk]]$id[i]
    }
    if (!all(d[["country"]] %in% DBI::dbReadTable(con, "country")$id)) {
      stop("Foreign key violation for country")
    }

    message(sprintf("...uploading %d rows", nrow(d)))

    DBI::dbBegin(con)
    on.exit(DBI::dbRollback(con))
    DBI::dbExecute(con, "ALTER TABLE demographic_statistic DISABLE TRIGGER ALL")
    DBI::dbWriteTable(con, "demographic_statistic", d, append = TRUE)
    DBI::dbExecute(con, "ALTER TABLE demographic_statistic ENABLE TRIGGER ALL")
    DBI::dbCommit(con)
    on.exit()
  }

  is_done <- function(dsource, demographic_variant, gender, data_type) {
    sql <- paste("SELECT *",
                 "  FROM demographic_statistic",
                 "  JOIN demographic_source",
                 "    ON demographic_source.id = demographic_statistic.demographic_source",
                 "  JOIN demographic_variant",
                 "    ON demographic_variant.id =",
                 "       demographic_statistic.demographic_variant",
                 "  JOIN gender",
                 "    ON gender.id = demographic_statistic.gender",
                 "  JOIN demographic_statistic_type",
                 "    ON demographic_statistic_type.id =",
                 "       demographic_statistic.demographic_statistic_type",
                 " WHERE demographic_source.code = $1",
                 "   AND demographic_variant.code = $2",
                 "   AND gender.code = $3",
                 "   AND demographic_statistic_type.code = $4",
                 " LIMIT 1",
                 sep = "\n")
    pars <- list(dsource, demographic_variant, gender, data_type)
    nrow(DBI::dbGetQuery(con, sql, pars)) > 0L
  }

  for (i in seq_along(sheet_names)) {
    # Issue: some medium variant data is spread across two sheets, so 
    # the is_done call causes the second sheet to be ignored.
    
    #if (is_done(dsource, variant_names[[i]], gender, data_type)) {
    #  message(sprintf("skipping %s / %s / %s / %s",
    #                  dsource, variant_names[[i]], gender, data_type))
    #} else {
      
      if (data_type == 'int_pop') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_interpolated_population_sheet(xl, variant_names[[i]],
                                                data_type, remove_year[[i]], test_code), "process")
        
      } else if (data_type == 'qq_pop') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_qq_population_sheet(xl, variant_names[[i]],
                                                data_type, remove_year[[i]]), "process")
      
      } else if (data_type == 'tot_pop') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_total_population_sheet(xl, variant_names[[i]], data_type, remove_year[[i]]),
          "process")
        
      } else if (data_type == 'birth_mf') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_5yearly_single_sheet(xl, variant_names[[i]], data_type, mult_value = 1),
          "process")
      
      } else if (data_type == 'qq_births') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_5yearly_single_sheet(xl, variant_names[[i]], data_type, mult_value = 1000),
          "process")
        
        
      } else if (data_type == 'net_mig_rate') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_5yearly_single_sheet(xl, variant_names[[i]], data_type, mult_value = 0.001),
          "process")
        
            
      } else if (data_type == 'as_fert') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_age_specific_fertility_sheet(xl, variant_names[[i]], data_type),
          "process")
        
      } else if (data_type == 'mort_age') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        d <- report_time(
          process_age_specific_mortality_sheet(xl, variant_names[[i]], data_type),
          "process")
        
      } else if (data_type == 'life_table') {
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        
        col_names = c("Probability of dying q(x,n)",
                      "Number of survivors l(x)",
                      "Expectation of life e(x)")
        
        var_names <- c("p_dying","n_survivors","life_ex")
        
        d<- report_time(process_life_table(xl, variant_names[[i]], gender, col_names, 
                                                  var_names, dsource),
                        "process")

      } else if (data_type == 'ann_int_ind') {
        
        xl <- report_time(read_sheet(sheet_names[[i]]), "read")
        
        col_names = c("Deaths (thousands)",
                      "Male deaths (thousands)",
                      "Female deaths (thousands)",
                      "Crude death rate (deaths per 1,000 population)",
                      "Life expectancy at birth, both sexes combined (years)",
                      "Life expectancy at birth, males (years)",
                      "Life expectancy at birth, females (years)",
                      "Births (thousands)",
                      "Crude birth rate (births per 1,000 population)",
                      "Total fertility (live births per woman)",
                      "Infant mortality rate (infant deaths per 1,000 live births)",
                      "Under-five mortality (deaths under age 5 per 1,000 live births)")

        transforms = c(1000,1000,1000,0.001,1,1,1,1000,0.001,1,0.001,0.001)
                      
        genders <- c("both","male","female","both","both","male",
                     "female","both","both","both","both","both")
        
        var_names <- c("mort_tot","mort_tot","mort_tot","cdr","lx0",
                       "lx0","lx0","births","cbr","fert_tot","unwpp_imr","unwpp_u5mr")
        
        d<- report_time(process_annual_indicators(xl,variant_names[[i]],col_names, genders, 
                                                  var_names, transforms, dsource, remove_year[[i]]),
                        "process")
        

      } else {
        stop(sprintf("data type %s not recognised", data_type))
      }
      report_time(upload_data(d), "upload")
    #}
  }
}

process_all_population <- function(con, test_code = "") {
  country_tr <- read_iso_countries()
  country_tr <- filter_iso_countries(country_tr)
  
  if (test_code=="") {
    info <- read_csv("meta/process.csv")
  } else if (test_code=="july28_test") {
    info <- read_csv("meta/process_july28test.csv")
  }

  for (i in seq_len(nrow(info))) {
    x <- info[i, ]
    sheet_names <- strsplit(x$sheet_names, ";\\s*")[[1]]
    variant_names <- strsplit(x$variant_names, ";\\s*")[[1]]
    remove_year <- strsplit(x$remove_year, ";\\s*")[[1]]
    process_population(con, x$filename, x$gender, sheet_names, remove_year, 
                       variant_names, x$source, x$data_type, country_tr, test_code = test_code)
  }
  
}

read_excel <- function(...) {
  oo <- options(warnPartialMatchArgs = FALSE)
  if (!is.null(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  readxl::read_excel(...)
}

read_csv <- function(...) {
  read.csv(..., stringsAsFactors = FALSE)
}

report_time <- function(expr, label) {
  t <- system.time(res <- force(expr))
  message(sprintf("...%s time: %2.2f s (%2.2f s wall)",
                  label, summary(t)[["user"]], t[["elapsed"]]))
  res
}

upload_csv <- function(con, filename) {
  table <- sub("\\.csv$", "", basename(filename))
  if (table_is_empty(con, table)) {
    message(sprintf("Uploading %s => '%s'", filename, table))
    data <- read_csv(filename)
    DBI::dbWriteTable(con, table, data, append = TRUE)
  }
}

read_iso_countries <- function() {
  iso3166 <- xml2::read_xml("data/iso3166.xml")
  xml_countries <- xml2::xml_find_all(iso3166, "//c")
  data.frame(code = as.numeric(xml2::xml_attr(xml_countries, "n3")),
             name = xml2::xml_attr(xml_countries, "n"),
             id = xml2::xml_attr(xml_countries, "c3"),
             stringsAsFactors = FALSE)
}

filter_iso_countries <- function(ret) {
  ret[ret$id %in% readLines("meta/countries_keep.txt"), ]
}


RE_YEAR <- "^[0-9]{4}$"
RE_YEAR_SPAN <- "^[0-9]{4}-[0-9]{4}$"
RE_AGE_SPAN <- "^[0-9]{1,2}-[0-9]{1,3}$"

#-------------------------------------------------------------------------------------
#
# Create Neonatal Mortality data from UNWPP IMR, and CM-NMR/IMR
#
#


create_nmr <- function(con, wpp_source, cm_file, cm_sheet, new_source, new_variant) {
  
  read_cm_sheet <- function(xlfile, sheet, skip = 6, country_col = "ISO Code") {
    message(sprintf("Reading %s:%s", xlfile, sheet))
    xl <- read_excel(xlfile, sheet = sheet, skip = skip, col_names = TRUE, na = c("", "…"))
    xl$iso3 <- country_tr$id[match(xl[[country_col]], country_tr$id)]
    as.data.frame(xl[!is.na(xl$iso3), ])
  }
  
  # Get the country set...
  
  country_tr <- read_iso_countries()
  country_tr <- filter_iso_countries(country_tr)
  
  # Load the XL sheet.
  
  xl <- read_cm_sheet(cm_file, cm_sheet)
  
  # Get db foreign keys
  # Lookup both gender
  
  db_gender <- DBI::dbGetQuery(con, "SELECT * from gender")
  id_both_gender <- db_gender$id[db_gender$code=='both']
  
  # Lookup variant ids
  
  db_dv <- DBI::dbGetQuery(con, "SELECT * from demographic_variant")
  id_extrap <- db_dv$id[db_dv$code == new_variant]
  id_wpp_med <- db_dv$id[db_dv$code == 'unwpp_medium_variant']
  id_wpp_est <- db_dv$id[db_dv$code == 'unwpp_estimates']
  
  # Lookup type ids
  
  db_types <- DBI::dbGetQuery(con, "SELECT * from demographic_statistic_type")
  id_wpp_imr <- db_types$id[db_types$code == 'unwpp_imr']
  id_wpp_u5mr <- db_types$id[db_types$code == 'unwpp_u5mr']
  id_nmr <- db_types$id[db_types$code == 'unwpp_cm_nmr']
  
  # Lookup demographic source
  db_src <- DBI::dbGetQuery(con, "SELECT * from demographic_source")
  id_wpp_source <- db_src$id[db_src$code == wpp_source]
  id_extrap_source <- db_src$id[db_src$code == new_source]
  
  # Spreadsheet into data frame
  
  rightString <- function(s,n) {
    substr(s, (nchar(s)-n)+1, nchar(s))
  }
  
  select_cols <- sort(c(
    grep("^IMR.[0-9]{4}$", names(xl)),
    grep("^U5MR.[0-9]{4}$", names(xl)),
    grep("^NMR.[0-9]{4}$", names(xl))
  ))
  
  no_countries <- length(unique(xl$iso3))
  variants <- c("cm_lower","cm_median","cm_upper")
  dtypes <- c("cm_u5mr","cm_imr","cm_nmr")
  years <- rightString( colnames(xl)[select_cols], 4)
  no_years <- length(years)
  no_variants <- length(variants)
  no_variables <- length(dtypes)
    
  res <- data.frame(
    year = rep(years, each = (no_countries * no_variants)),
    value = unlist(xl[select_cols]),
    country = xl$iso3,
    demographic_statistic_type = rep(dtypes, each = no_countries * no_variants * length(unique(years))),
    demographic_variant = rep(variants, no_countries * no_years),
    stringsAsFactors = FALSE
  )
  res <- res[res$demographic_variant=='cm_median',]
  res$demographic_variant <- NULL
  res <- res[!is.na(res$value), ]
  row.names(res) <- NULL
  
  # Create new data for each country.
  for (country in country_tr$id) {
    # Calculate CM nmr/imr ratio
    
    cm_data <- res[res$country==country,]
    cm_data$country <- NULL
    
    nmr_data <- cm_data[cm_data$demographic_statistic_type=='cm_nmr',]
    nmr_data$demographic_statistic_type <- NULL
    nmr_data<-nmr_data[!is.na(nmr_data$value),]
    nmr_data$year <- as.numeric(nmr_data$year)
    
    imr_data <- cm_data[cm_data$demographic_statistic_type=='cm_imr',]
    imr_data$demographic_statistic_type <- NULL
    imr_data<-imr_data[!is.na(imr_data$value),]
    imr_data$year <- as.numeric(imr_data$year)
    
    first_year_nmr <- min(nmr_data$year)   # Expected to be variable..            
    last_year_nmr <- max(nmr_data$year)    # Expected to be 2015
    first_year_imr <- min(imr_data$year)   # Expected to be variable..            
    last_year_imr <- max(imr_data$year)    # Expected to be 2015
    first_year <- max(first_year_nmr, first_year_imr)
    last_year <- min(last_year_nmr, last_year_imr)
    
    imr_data <- imr_data[imr_data$year>=first_year,]
    nmr_data <- nmr_data[nmr_data$year>=first_year,]
    imr_data <- imr_data[imr_data$year<=last_year,]
    nmr_data <- nmr_data[nmr_data$year<=last_year,]
    
    first_ratio <- nmr_data$value[nmr_data$year == first_year]/
                   imr_data$value[imr_data$year == first_year]
    
    last_ratio <- nmr_data$value[nmr_data$year == last_year]/
      imr_data$value[imr_data$year == last_year]
    
    
    
    nmr_imr_ratio <- data.frame(
      year = 1950:2099,
      value = c(rep(first_ratio,first_year-1950),
                nmr_data$value/imr_data$value,
                rep(last_ratio, 2099-last_year))
    
    )
    
    # Now get WPP IMR from db.
    
    wpp_imr <- DBI::dbGetQuery(con, paste("SELECT year, value FROM demographic_statistic WHERE ",
                                          "demographic_statistic_type = ",id_wpp_imr," AND ",
                                          "(demographic_variant = ",id_wpp_est," OR demographic_variant = ",id_wpp_med,") AND ",
                                          "country = '",country,"'",sep=""))
    
    # Build new table multiplying by NMR/IMR ratio.
    
    if (nrow(wpp_imr)>0) {
      new_umr <- data.frame(
        age_from = 0,
        age_to = 0,
        value = wpp_imr$value * nmr_imr_ratio$value,
        year = wpp_imr$year,
        demographic_variant = id_extrap,
        gender = id_both_gender,
        country = country,
        demographic_source = id_extrap_source,
        demographic_statistic_type = id_nmr,
        stringsAsFactors = FALSE
      )
      DBI::dbWriteTable(con, "demographic_statistic", new_umr, append = TRUE)
    }
  }
}
  
  
