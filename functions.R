# init_country_table populates an empty country table with
# the 3-digit alpha code for both name and country, just so
# the demographic statistic "country" field has something to
# link to.
init_country_table <- function(db, iso3166) {
  DBI::dbExecute(db,"DELETE FROM country")
  country <- data.frame(id = iso3166$code,
                        name = iso3166$code,
                        stringsAsFactors = FALSE)
  DBI::dbWriteTable(db, "country", country, append = TRUE)
}

# For dev only:
# Code to empty the tables. (Not drop them)
empty_tables <- function(db) {
  DBI::dbBegin(db)
  on.exit(DBI::dbRollback(db))
  DBI::dbExecute(db, "ALTER TABLE demographic_statistic DISABLE TRIGGER ALL")

  DBI::dbExecute(db, "DELETE FROM demographic_statistic")
  DBI::dbExecute(db, "DELETE FROM gender")
  DBI::dbExecute(db, "DELETE FROM projection_variant")
  DBI::dbExecute(db, "DELETE FROM source")
  DBI::dbExecute(db, "DELETE FROM demographic_statistic_type")

  DBI::dbExecute(db, "ALTER TABLE demographic_statistic ENABLE TRIGGER ALL")
  DBI::dbCommit(db)
  on.exit()
  DBI::dbExecute(db, "VACUUM")
}

# init_tables
# Adds identifiers for the gender, projection, demographic_statistic_type
# and source tables.
init_tables <- function(db) {
  init1 <- function(name) {
    data <- read_csv(sprintf("meta/%s.csv", name))
    DBI::dbWriteTable(db, name, data, append = TRUE)
  }
  tables <- c("gender", "projection_variant", "demographic_statistic_type",
              "source")
  for (t in tables) {
    init1(t)
  }
}

download_single <- function(url, dest) {
  if (!file.exists(dest)) {
    download.file(url,dest,method='libcurl', mode="wb")
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

process_interpolated_population <- function(db, xlfile, gender, sheet_names,
                                            variant_names, source, iso3166) {
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
  process_sheet <- function(xl, variant) {
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
    res$demographic_statistic_type <- "int_pop"
    res
  }
  upload_data <- function(d) {
    ## Manually satisfy FK constraints:
    fks <- c("gender", "source", "country", "projection_variant",
             "demographic_statistic_type")
    for (fk in fks) {
      if (!all(d[[fk]] %in% DBI::dbReadTable(db, fk)$id)) {
        stop("Foreign key violation for ", fk)
      }
    }

    DBI::dbBegin(db)
    on.exit(DBI::dbRollback(db))
    DBI::dbExecute(db, "ALTER TABLE demographic_statistic DISABLE TRIGGER ALL")
    DBI::dbWriteTable(db, "demographic_statistic", d, append = TRUE)
    DBI::dbExecute(db, "ALTER TABLE demographic_statistic ENABLE TRIGGER ALL")
    DBI::dbCommit(db)
    on.exit()
  }

  for (i in seq_along(sheet_names)) {
    xl <- report_time(read_sheet(sheet_names[[i]]), "read")
    d <- report_time(process_sheet(xl, variant_names[[i]]), "process")
    report_time(upload_data(d), "upload")
  }
}


process_all_interpolated_population <- function(db) {
  iso3166 <- read_iso_countries()
  info <- read_csv("meta/process.csv")

  for (i in seq_len(nrow(info))) {
    x <- info[i, ]
    sheet_names <- strsplit(x$sheet_names, ";\\s*")[[1]]
    variant_names <- strsplit(x$variant_names, ";\\s*")[[1]]
    process_interpolated_population(db, x$filename, x$gender, sheet_names,
                                    variant_names, x$source, iso3166)
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
                  label, t[["elapsed"]], summary(t)[["user"]]))
  res
}
