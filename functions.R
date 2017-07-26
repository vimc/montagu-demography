import_demography <- function(con, clear_first = TRUE) {
  download_data()
  if (clear_first) {
    empty_tables(con)
  }
  
  init_tables(con)
  process_all_population(con)
  DBI::dbExecute(con, "VACUUM")
}

# For dev only:
# Code to empty the tables. (Not drop them)
empty_tables <- function(con) {
  tables <- c("demographic_statistic",
              "touchstone_demographic_source",
              "gender", "demographic_variant", "demographic_source",
              "demographic_statistic_type")
  tables_str <- paste(tables, collapse = ", ")
  sql <- paste("TRUNCATE", tables_str)
  message(sprintf("Clearing %s", tables_str))
  DBI::dbExecute(con, sql)
}

table_is_empty <- function(con, tbl) {
  nrow(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 1", tbl))) == 0L
}

# init_tables
# Adds identifiers for the gender, demographic_variant, demographic_statistic_type
# and demographic_source tables.
init_tables <- function(con) {
  tables <- c("gender", "demographic_variant", "demographic_statistic_type",
              "demographic_source")
  for (table in tables) {
    upload_csv(con, file.path("meta", paste0(table, ".csv")))
  }
  upload_csv(con, "montagu-db/minimal/common/country.csv")
}

## NOTE: this does not necessarily do error handling properly:
download_single <- function(url, dest) {
  if (!file.exists(dest)) {
    download.file(url, dest, method = 'libcurl', mode = "wb")
  }
}

download_data <- function() {
  files <- read.csv("meta/files.csv", stringsAsFactors = FALSE)
  for (p in unique(dirname(files$filename))) {
    dir.create(p, FALSE, TRUE)
  }
  for (i in seq_len(nrow(files))) {
    download_single(files$url[[i]], files$filename[[i]])
  }
}

process_population <- function(con, xlfile, gender, sheet_names,
                               remove_year, variant_names, dsource, data_type,
                               country_tr) {

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
  
  read_sheet <- function(sheet, skip, country_col, iso_numeric_to_alpha) {
    message(sprintf("Reading %s:%s", xlfile, sheet))
    xl <- read_excel(xlfile, sheet = sheet, skip = skip, col_names = TRUE, na = c("", "…"))
    
    if (iso_numeric_to_alpha) {
      xl$iso3 <- country_tr$id[match(xl[[country_col]], country_tr$code)]
    } else {
      ## ISO is already alpha (eg, ChildMortality)
      xl$iso3 <- country_tr$id[match(xl[[country_col]], country_tr$id)]
    }
    
    as.data.frame(xl[!is.na(xl$iso3), ])
  }
  
  read_sheet_unwpp <- function(sheet) {
    read_sheet(sheet, skip = 16, country_col = "Country code", iso_numeric_to_alpha = TRUE)
  }
  
  read_sheet_cm <- function(sheet) {
    read_sheet(sheet, skip=6, country_col = "ISO Code", iso_numeric_to_alpha = FALSE)
  }


  process_child_mortality <- function(xl, dsource) {
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
      age_from = 0,
      age_to = 0,
      gender = "both",
      country = xl$iso3,
      demographic_statistic_type = rep(dtypes, each = no_countries * no_variants * length(unique(years))),
      demographic_variant = rep(variants, no_countries * no_years),
      demographic_source = dsource,
      stringsAsFactors = FALSE
    )
    res <- res[!is.na(res$value), ]
    res$value <- res$value/1000.0
    row.names(res) <- NULL
    res
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
      year = as.numeric(substring(unique(xl$"Period"),1,4)),
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

  process_birth_gender_sheet <- function(xl, variant, data_type) {
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
    process_shared(res, gender, dsource, variant, data_type)
  }
  
  process_interpolated_population_sheet <- function(xl, variant, data_type, remove_year) {
    age_cols_pre_1990 <- as.character(c(0:79, "80+"))
    # Column "100+" has been renamed to "100" in UNWPP 2017.
    if ("100+" %in% colnames(xl)) {
      age_cols_from_1990 <- as.character(c(0:99, "100+"))
    } else {
      age_cols_from_1990 <- as.character(c(0:100))
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
    if (is_done(dsource, variant_names[[i]], gender, data_type)) {
      message(sprintf("skipping %s / %s / %s / %s",
                      dsource, variant_names[[i]], gender, data_type))
    } else {
      
      if (data_type == 'int_pop') {
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        d <- report_time(
          process_interpolated_population_sheet(xl, variant_names[[i]],
                                                data_type, remove_year[[i]]), "process")
        
      } else if (data_type == 'tot_pop') {
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        d <- report_time(
          process_total_population_sheet(xl, variant_names[[i]], data_type, remove_year[[i]]),
          "process")
        
      } else if (data_type == 'birth_mf') {
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        d <- report_time(
          process_birth_gender_sheet(xl, variant_names[[i]], data_type),
          "process")
      
      } else if (data_type == 'as_fert') {
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        d <- report_time(
          process_age_specific_fertility_sheet(xl, variant_names[[i]], data_type),
          "process")
        
      } else if (data_type == 'mort_age') {
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        d <- report_time(
          process_age_specific_mortality_sheet(xl, variant_names[[i]], data_type),
          "process")
        
        
      } else if (data_type == 'cm_2015') {
        xl <- report_time(read_sheet_cm(sheet_names[[i]]), "read")
        d <- report_time(
          process_child_mortality(xl, dsource),
          "process")
        
      } else if (data_type == 'life_table') {
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        
        col_names = c("Probability of dying q(x,n)",
                      "Number of survivors l(x)",
                      "Expectation of life e(x)")
        
        var_names <- c("p_dying","n_survivors","life_ex")
        
        d<- report_time(process_life_table(xl, variant_names[[i]], gender, col_names, 
                                                  var_names, dsource),
                        "process")
        
      

      } else if (data_type == 'ann_int_ind') {
        
        xl <- report_time(read_sheet_unwpp(sheet_names[[i]]), "read")
        
        col_names = c("Deaths (thousands)",
                      "Male deaths (thousands)",
                      "Female deaths (thousands)",
                      "Crude death rate (deaths per 1,000 population)",
                      "Life expectancy at birth, both sexes combined (years)",
                      "Life expectancy at birth, males (years)",
                      "Life expectancy at birth, females (years)",
                      "Births (thousands)",
                      "Crude birth rate (births per 1,000 population)",
                      "Total fertility (live births per woman)")
        
        transforms = c(1000,1000,1000,0.001,1,1,1,1000,0.001,1)
                      
        genders <- c("both","male","female","both","both","male","female","both","both","both")
        
        var_names <- c("mort_age","mort_age","mort_age","cdr","lx0","lx0","lx0","births","cbr","fert_tot")
        
        d<- report_time(process_annual_indicators(xl,variant_names[[i]],col_names, genders, 
                                                  var_names, transforms, dsource, remove_year[[i]]),
                        "process")
        

      } else {
        stop(sprintf("data type %s not recognised", data_type))
      }
      report_time(upload_data(d), "upload")
    }
  }
}

process_all_population <- function(con) {
  country_tr <- read_iso_countries()
  country_tr <- filter_iso_countries(country_tr)

  info <- read_csv("meta/process.csv")

  for (i in seq_len(nrow(info))) {
    x <- info[i, ]
    sheet_names <- strsplit(x$sheet_names, ";\\s*")[[1]]
    variant_names <- strsplit(x$variant_names, ";\\s*")[[1]]
    remove_year <- strsplit(x$remove_year, ";\\s*")[[1]]
    process_population(con, x$filename, x$gender, sheet_names, remove_year, 
                       variant_names, x$source, x$data_type, country_tr)
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

