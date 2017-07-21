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
* `demographic_variant`
* `demographic_source`
* `country`

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
* Populate `demographic_statistic_type`, `gender`, `demographic_variant` and `demographic_source` with the constants. (See meta/ directory)
* Populate `country` with `montagu-db` country information. (montagu-db/minimal/common/country.csv)
* Populate `demographic_statistic` table with the demographic data.

### Development/Debugging

* `empty_tables(db)` empties the `demographic_statistic_type`, `gender`, `demographic_variant`, `demographic_source`, and `demographic_statistic` tables.

## Data Supported

### Current datasets

|Source      | Data                        | Units               | Years [Step]  | Ages [Step]        | Gender |
|------------|-----------------------------|---------------------|---------------|--------------------|--------|
| WPP_2012   | Interpolated Population     | People              | 1950-2100 [1] | 0-80+/100+ [1]     | M/F/B  |
| WPP_2015   | Interpolated Population     | People              | 1950-2100 [1] | 0-80+/100+ [1]     | M/F/B  |
| WPP_2017   | Interpolated Population     | People              | 1950-2100 [1] | 0-80+/100+ [1]     | M/F/B  |
| WPP_2012   | Total Population            | People              | 1950-2100 [1] |         -          | M/F/B  |
| WPP_2015   | Total Population            | People              | 1950-2100 [1] |         -          | M/F/B  |
| WPP_2017   | Total Population            | People              | 1950-2100 [1] |         -          | M/F/B  |
| WPP_2017   | Sex Ratio At Birth          | m. per f. births    | 1950-2100 [5] |         -          |   B    |
| WPP_2017   | Age-specific fertility      | Births              | 1950-2100 [5] | (mother) 15-49 [5] |   F    |
| CM_2015    | U5MR rtality rate           | deaths / live birth |~1950-2015 [1] |         -          |   B    |
| CM_2015    | IMR Infant mortality rate   | deaths / live birth |~1950-2015 [1] |         -          |   B    |
| CM_2015    | NMR Neonatal mortality rate | deaths / live birth |~1950-2015 [1] |         -          |   B    |

Note 1: Demographic statistics are given in units of people, rather than 1000s. 

Note 2: Population Data for Marshall Islands and Tuvalu are not currently available in age-distributed form. They are only available in Total Population, Both-Gender.
Kosovo is not available at all - known issue.

### Steps for adding new datasets (to be confirmed)

* New spreadsheets should be put somewhere in `C:\xampp\htdocs\mrcdata\resources` on `fi--didex1.dide.ic.ac.uk` - these will be downloaded on demand
* Add the new sheets to `meta/files.csv` arrange downloading
* Edit `meta/demographic_statistic_type.csv`, `meta/demographic_source.csv`, `meta/demographic_variant.csv`, `meta/process.csv`
* Write the function to import the data. (see the various `process` functions)
