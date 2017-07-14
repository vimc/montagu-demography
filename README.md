# Montagu demography

This repo contains R scripts to import data from UNWPP spreadsheets into the Montagu Database.

## Dependencies and pre-requesites:

The script requires the following packages:

* `readxl`
* `xml2`
* `RPostgres`

The script requires that the following tables exist according to the latest schema.

* demographic_statistic_type
* gender
* projection_variant
* source
* country - and the script further requires this table to be populated already.

Two environment variables set the behaviour:-

* `MONTAGU_DB_HOST` (default: support.montagu.dide.ic.ac.uk)
* `MONTAGU_DB_PORT` (default: 8888)

## Usage

### Integrated use

 Running import.R does the following steps:

* Loads the list of 97 relevant countries from `countries_keep.txt`
* Creates a `data/` directory, and downloads UNWPP data from `mrcdata.dide.ic.ac.uk`. (See data below)
* (note that files are only downloaded if the destination doesn't exist)
* Parse `iso3166.xml` (also downloaded from mrcdata), creating lookup from 3-digit numeric code (UNWPP), to 3-digit alpha-code.
* Populate demographic_statistic_type, gender, projection_variant and source with the constants.
* Populate the demographic_statistic table with the demographic data.

### Development/Debugging

Two extra functions may be useful for debugging.

* `empty_tables(db)` empties the demographic_statistic_type, gender, projection_variant, source, and demographic_statistic tables.
* `init_country_table(db, iso3166)` initialises an empty country table.

## Data Supported

### Current datasets

* Interpolated Population, WPP 2012 revision.
* Interpolated Population, WPP 2015 revision.
* Interpolated Population, WPP 2017 revision.

### Steps for adding new datasets (to be confirmed)

* New spreadsheets should be put somewhere in `C:\xampp\htdocs\mrcdata\resources` on `fi--didex1.dide.ic.ac.uk`
* Update the download_data function - consider whether new directories should be made.
* May need to add rows in demographic_statistic_type, sources, and/or projection_variant.
* Write function to import the data. (see process_interpolated_population, which called by process_interpolated_population_{2012,2015,2017}.

