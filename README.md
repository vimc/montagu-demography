# Montagu demography

This repo contains R scripts to import data from UNWPP spreadsheets into the Montagu Database.

## Dependencies and pre-requesites:

The script requires the following packages:

* `readxl`
* `RPostgres`

Which are all found in the `montagu-db-import` docker image.

The script requires that the following tables exist according to the latest schema.

* `demographic_statistic_type`
* `gender`
* `projection_variant`
* `source`
* `country` - and the script further requires this table to be populated already.

Two environment variables set the behaviour:-

* `MONTAGU_DB_HOST` (default: support.montagu.dide.ic.ac.uk)
* `MONTAGU_DB_PORT` (default: 6543)

It is easiest to create a file called `.Renviron` in this directory that sets these appropriately, such as:

```sh
MONTAGU_DB_HOST=localhost
MONTAGU_DB_PORT=8888
```

(`.Renviron` is excluded from git by `.gitignore`).

## Usage

### Integrated use

 Running import.R does the following steps:

* Loads the list of 97 relevant countries from `meta/countries_keep.txt`
* Creates a `data/` directory, and downloads UNWPP data from `mrcdata.dide.ic.ac.uk`. (See data below)
* (note that files are only downloaded if the destination doesn't exist)
* Populate `demographic_statistic_type`, `gender`, `projection_variant` and `source` with the constants.
* Populate `country` with `montagu-db` country information
* Populate `demographic_statistic` table with the demographic data.

### Development/Debugging

Two extra functions may be useful for debugging.

* `empty_tables(db)` empties the `demographic_statistic_type`, `gender`, `projection_variant`, `source`, and `demographic_statistic` tables.

## Data Supported

### Current datasets

* Interpolated Population, WPP 2012 revision.
* Interpolated Population, WPP 2015 revision.
* Interpolated Population, WPP 2017 revision.
* Total Population, WPP 2012 revision.
* Total Population, WPP 2015 revision.
* Total Population, WPP 2017 revision.

### Steps for adding new datasets (to be confirmed)

* New spreadsheets should be put somewhere in `C:\xampp\htdocs\mrcdata\resources` on `fi--didex1.dide.ic.ac.uk` - these will be downloaded on demand
* Add the new sheets to `meta/files.csv` arrange downloading
* May need to add values in `meta/demographic_statistic_type.csv`, `meta/source.csv`, and/or `meta/projection_variant.csv`
* Write the function to import the data. (see `process_interpolated_population`)
