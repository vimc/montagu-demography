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
              "gender", "projection_variant", "source",
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
# Adds identifiers for the gender, projection, demographic_statistic_type
# and source tables.
init_tables <- function(con) {
  tables <- c("gender", "projection_variant", "demographic_statistic_type",
              "source")
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
                               remove_year, variant_names, source, data_type,
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
  read_sheet <- function(sheet) {
    message(sprintf("Reading %s:%s", xlfile, sheet))
    xl <- read_excel(xlfile, sheet = sheet, skip = 16, col_names = TRUE,
                     na = c("", "…"))
    xl$iso3 <- country_tr$id[match(xl$"Country code", country_tr$code)]
    as.data.frame(xl[!is.na(xl$iso3), ])
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
      value = unlist(xl[age_cols]),
      age_from = rep(start_age, each=no_countries * no_years),
      age_to = rep(start_age+4, each=no_countries * no_years),
      stringsAsFactors = FALSE)
    process_shared(res, gender, source, variant, data_type, year_span = 5)
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
    process_shared(res, gender, source, variant, data_type, year_span = 5)
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
    process_shared(res, gender, source, variant, data_type, year_span = 1)
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
    process_shared(res, gender, source, variant, data_type, year_span = 1)
  }

  ## This converts columns to the format we want them on montagu
  ## (date_start, date_end) and sets the metadata columns
  process_shared <- function(res, gender, source, variant, data_type, year_span) {
    row.names(res) <- NULL    
    res$date_start <- sprintf("%d-07-01", res$year)
    res$date_end <- sprintf("%d-06-30", res$year + year_span)
    res$year <- NULL

    res$projection_variant <- variant
    res$gender <- gender
    res$source <- source
    res$demographic_statistic_type <- data_type
    res
  }
  
  upload_data <- function(d) {
    ## TODO: this can be done a bit more efficiently, but this is OK for now
    ## Manually satisfy FK constraints:
    fks <- c("gender", "source", "projection_variant",
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

  is_done <- function(source, projection_variant, gender, data_type) {
    sql <- paste("SELECT *",
                 "  FROM demographic_statistic",
                 "  JOIN source",
                 "    ON source.id = demographic_statistic.source",
                 "  JOIN projection_variant",
                 "    ON projection_variant.id =",
                 "       demographic_statistic.projection_variant",
                 "  JOIN gender",
                 "    ON gender.id = demographic_statistic.gender",
                 "  JOIN demographic_statistic_type",
                 "    ON demographic_statistic_type.id =",
                 "       demographic_statistic.demographic_statistic_type",
                 " WHERE source.code = $1",
                 "   AND projection_variant.code = $2",
                 "   AND gender.code = $3",
                 "   AND demographic_statistic_type.code = $4",
                 " LIMIT 1",
                 sep = "\n")
    pars <- list(source, projection_variant, gender, data_type)
    nrow(DBI::dbGetQuery(con, sql, pars)) > 0L
  }

  for (i in seq_along(sheet_names)) {
    if (is_done(source, variant_names[[i]], gender, data_type)) {
      message(sprintf("skipping %s / %s / %s / %s",
                      source, variant_names[[i]], gender, data_type))
    } else {
      xl <- report_time(read_sheet(sheet_names[[i]]), "read")
      if (data_type == 'int_pop') {
        d <- report_time(
          process_interpolated_population_sheet(xl, variant_names[[i]],
                                                data_type, remove_year[[i]]), "process")
      } else if (data_type == 'tot_pop') {
        d <- report_time(
          process_total_population_sheet(xl, variant_names[[i]], data_type, remove_year[[i]]),
          "process")
        
      } else if (data_type == 'birth_mf') {
        d <- report_time(
          process_birth_gender_sheet(xl, variant_names[[i]], data_type),
          "process")
      
      } else if (data_type == 'as_fert') {
        d <- report_time(
          process_age_specific_fertility_sheet(xl, variant_names[[i]], data_type),
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
RE_AGE_SPAN <- "^[0-9]{2}-[0-9]{2}$"
