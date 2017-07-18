import_demography <- function(con) {
  download_data()
  empty_tables(con)
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
  for (name in tables) {
    if (table_is_empty(con, name)) {
      message(sprintf("Uploading '%s'", name))
      data <- read_csv(sprintf("meta/%s.csv", name))
      DBI::dbWriteTable(con, name, data, append = TRUE)
    }
  }

  if (table_is_empty(con, "country")) {
    message(sprintf("Uploading '%s'", "country"))
    iso3166 <- read_iso_countries()
    country <- data.frame(id = iso3166$code,
                          name = iso3166$code,
                          stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "country", country, append = TRUE)
  }
}

## NOTE: this does not necessarily do error handling properly:
download_single <- function(url, dest) {
  if (!file.exists(dest)) {
    download.file(url, dest, method='libcurl', mode="wb")
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

read_iso_countries <- function(filter = TRUE) {
  iso3166 <- xml2::read_xml("data/iso3166.xml")
  xml_countries <- xml2::xml_find_all(iso3166, "//c")
  ret <- data.frame(id = as.numeric(xml2::xml_attr(xml_countries, "n3")),
                    code = xml2::xml_attr(xml_countries, "c3"),
                    stringsAsFactors = FALSE)
  if (filter) {
    ret <- ret[ret$code %in% readLines("meta/countries_keep.txt"), ]
  }
  ret
}

process_population <- function(con, xlfile, gender, sheet_names,
                                  variant_names, source, iso3166, data_type) {
  
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
    xl
  }
  
  process_interpolated_population_sheet <- function(xl, variant, data_type) {
    age_cols_pre_1990 <- as.character(c(0:79,"80+"))

    # Column "100+" has been renamed to "100" in UNWPP 2017.
    if ("100+" %in% colnames(xl)) {
      age_cols_from_1990 <- as.character(c(0:99,"100+"))
    } else {
      age_cols_from_1990 <- as.character(c(0:100))
    }

    xl$iso3 <- iso3166$code[match(xl$"Country code", iso3166$id)]
    xl <- as.data.frame(xl[!is.na(xl$iso3), ])
    xl$year <- xl[[6]]

    res <- rbind(reshape(xl[xl$year < 1990, ], age_cols_pre_1990),
                 reshape(xl[xl$year >= 1990, ], age_cols_from_1990))
    row.names(res) <- NULL
    res$projection_variant <- variant
    res$date_start <- sprintf("%d-07-01", res$year)
    res$date_end <- sprintf("%d-06-30", res$year + 1)
    res$year <- NULL
    res$gender <- gender
    res$source <- source
    res$demographic_statistic_type <- data_type
    res
  }
  
  process_total_population_sheet <- function(xl, variant, data_type) {
    
    first_year   <- as.integer(colnames(xl)[6])
    last_year    <- as.integer(colnames(xl)[length(colnames(xl))])
    year_cols    <- first_year:last_year
    no_years     <- length(year_cols)
    
    xl$iso3 <- iso3166$code[match(xl$"Country code", iso3166$id)]
    xl <- as.data.frame(xl[!is.na(xl$iso3), ])
    no_countries <- length(unique(xl$iso3))
    
    res <- data.frame(  year = rep(year_cols,each=no_countries),
                        country = rep(xl$iso3,no_years),
                        value = unlist(xl[6:(length(colnames(xl))-1)])
    )
    row.names(res) <- NULL    
    res$age_from <- '0'
    res$age_to   <- '120'
    res$projection_variant <- variant
    res$date_start <- sprintf("%d-07-01", res$year)
    res$date_end <- sprintf("%d-06-30", res$year+1)
    res$gender   <- gender
    res$source   <- source
    res$demographic_statistic_type <- data_type
    res$year <- NULL
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

  sql <- paste("SELECT id FROM demographic_statistic",
               " WHERE source = $1 AND projection_variant = $2",
               " LIMIT 1",
               sep = "\n")
  is_done <- function(source, projection_variant, gender) {
    sql <- paste("SELECT id FROM demographic_statistic",
                 " WHERE source = $1",
                 "   AND projection_variant = $2",
                 "   AND gender = $3",
                 " LIMIT 1",
                 sep = "\n")
    pars <- list(source, projection_variant, gender)
    nrow(DBI::dbGetQuery(con, sql, pars)) > 0L
  }

  for (i in seq_along(sheet_names)) {
    if (FALSE && is_done(source, variant_names[[i]], gender)) {
      message(sprintf("skipping %s / %s / %s",
                      source, variant_names[[i]], gender))
    } else {
      xl <- report_time(read_sheet(sheet_names[[i]]), "read")
      if (data_type=='int_pop') {
        d <- report_time(process_interpolated_population_sheet(xl, variant_names[[i]], data_type), "process")
      } else if (data_type=='tot_pop') {
        d <- report_time(process_total_population_sheet(xl, variant_names[[i]], data_type), "process")
      } else {
        stop(sprintf("data type %s not recognised",data_type))
      }
      report_time(upload_data(d), "upload")
    }
  }
}

process_all_population <- function(con) {
  iso3166 <- read_iso_countries()
  info <- read_csv("meta/process.csv")

  for (i in seq_len(nrow(info))) {
    x <- info[i, ]
    sheet_names <- strsplit(x$sheet_names, ";\\s*")[[1]]
    variant_names <- strsplit(x$variant_names, ";\\s*")[[1]]
    process_population(con, x$filename, x$gender, sheet_names,
                                  variant_names, x$source, iso3166, x$data_type)
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
