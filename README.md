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

### From UNWPP 2012:

| Data                         | Units             | Years [Step]  | Ages [Step]      | Gender | Variants |
|------------------------------|-------------------|---------------|------------------|--------|----------|
| Interpolated Population      | people            | 1950-2100 [1] | 0-80+/100+ [1]   | M/F/B  | Est+M    |
| Total Population             | people            | 1950-2100 [1] |         -        | M/F/B  | Est+M    |

### From UNWPP 2015:

| Data                         | Units             | Years [Step]  | Ages [Step]      | Gender | Variants |
|------------------------------|-------------------|---------------|------------------|--------|----------|
| Interpolated Population      | people            | 1950-2100 [1] | 0-80+/100+ [1]   | M/F/B  | Est+M    |
| Total Population             | people            | 1950-2100 [1] |         -        | M/F/B  | Est+M    |

### From UNWPP 2017:

| Data                         | Units             | Years [Step]  | Ages [Step]      | Gender | Variants |
|------------------------------|-------------------|---------------|------------------|--------|----------|
| Interpolated Population      | people            | 1950-2100 [1] | 0-80+/100+ [1]   | M/F/B  | Est+M    |
| Total Population             | people            | 1950-2100 [1] |         -        | M/F/B  | Est+M    |
| Quinquennial Population      | people            | 1950-2100 [5] | 0-80+/100+ [5]   | M/F/B  | Est+MHL  |
| Quinquennial Births          | births            | 1950-2095 [5] |         -        |   B    | Est+MHL  |
| Sex Ratio At Birth           | m. per f. births  | 1950-2095 [5] |         -        |   B    | Est+M    |
| Total births                 | births            | 1950-2099 [1] |         -        |   B    | Est+M    |
| Crude birth rate (CBR)       | births/population | 1950-2099 [1] |         -        |   B    | Est+M    |
| Age-specific fertility       | births            | 1950-2095 [5] | (moth) 15-49 [5] |   B    | Est+M    |
| Total fertility              | avg births/mother | 1950-2099 [1] |         -        |   B    | Est+M    |
| Mortality (n.deaths) by age  | deaths            | 1950-2095 [5] | 0-94 [5], 95+    | M/F/B  | Est+MHL  |
| Total Mortality              | deaths            | 1950-2099 [1] |         -        | M/F/B  | Est+M    |
| Crude death rate (CDR)       | deaths/population | 1950-2099 [1] |         -        |   B    | Est+M    |
| Life expectancy @ age 0      | years             | 1950-2099 [1] |         -        | M/F/B  | Est+M    |
| p(dying at age x)            | probability       | 1950-2095 [5] | 0,1-4,5-9..85-100| M/F/B  | Est+M    |
| Survivors of 100,000 at age x| people            | 1950-2095 [5] | 0,1-4,5-9..85-100| M/F/B  | Est+M    |
| Expected years left at age x | years             | 1950-2095 [5] | 0,1-4,5-9..85-100| M/F/B  | Est+M    |
| Net migration rate           | change per person | 1950-2095 [5] |         -        |   B    | Est+M    |
| IMR <1 mortality rate        | deaths/live birth | 1950-2100 [1] |         -        |   B    | Est+M    |
| U5MR <5 Mortality rate       | deaths/live birth | 1950-2100 [1] |         -        |   B    | Est+M    |

### From childmortality.org 2015:

| Data                         | Units             | Years [Step]  | Ages [Step]      | Gender | Variants |
|------------------------------|-------------------|---------------|------------------|--------|----------|
| IMR <1 mortality rate        | deaths/live birth |~1950-2015 [1] |         -        |   B    | CI H/M/L |
| NMR Neo (28d) mortality rate | deaths/live birth |~1950-2015 [1] |         -        |   B    | CI H/M/L |
| U5MR <5 Mortality rate       | deaths/live birth |~1950-2015 [1] |         -        |   B    | CI H/M/L |

### Notes:

1: Demographic statistics are given in units of people, rather than 1000s. 

2: Population Data for Marshall Islands and Tuvalu are not currently available in age-distributed form. They are only available in Total Population, Both-Gender.
Kosovo is not available at all - known issue.

### Steps for adding new datasets (to be confirmed)

* New spreadsheets should be put somewhere in `C:\xampp\htdocs\mrcdata\resources` on `fi--didex1.dide.ic.ac.uk` - these will be downloaded on demand
* Add the new sheets to `meta/files.csv` arrange downloading
* Edit `meta/demographic_statistic_type.csv`, `meta/demographic_source.csv`, `meta/demographic_variant.csv`, `meta/process.csv`
* Write the function to import the data. (see the various `process` functions)

## Database meta-tables

### demographic_source

| code        | name                             |
|-------------|----------------------------------|
| unwpp_2012  | UNWPP 2012                       |
| unwpp_2015  | UNWPP 2015                       |
| unwpp_2017  | UNWPP 2017                       |
| cm_2015     | ChildMortality.org (IGME), 2015  |

### demographic_variant

| code                   | name                           |
|------------------------|--------------------------------|
| unwpp_estimates        | UNWPP Estimates                |
| unwpp_high_variant     | UNWPP High Variant             |
| unwpp_low_variant      | UNWPP Low Variant              |
| unwpp_medium_variant   | UNWPP Medium Variant           |
| cm_lower               | Child Mortality (IGME) Lower   |
| cm_median              | Child Mortality (IGME) Median  |
| cm_upper               | Child Mortality (IGME) Upper   |

### gender

| code   | name    |
|--------|---------|
| both   | Both    |
| male   | Male    |
| female | Female  |

### demographic_statistic_type

| code         | age_interpretation    | name                                                   | y | rd         |
|--------------|-----------------------|--------------------------------------------------------|---|------------|
| as_fert      | Age of mother (years) | Age-specific fertility                                 | 5 | 2000-07-01 |
| birth_mf     | N/A                   | Sex Ratio at Birth                                     | 5 | 2000-07-01 |
| births       | N/A                   | Number of births                                       | 1 | 2000-01-01 |
| cbr          | N/A                   | Crude birth rate                                       | 1 | 2000-01-01 |
| cdr          | N/A                   | Crude death rate                                       | 1 | 2000-01-01 |
| cm_imr       | Under 1 years         | Under 1 mortality rate per 1000 live births            | 1 | 2000-07-01 |
| cm_nmr       | 1 years               | Neonatal (28 day) mortality rate per 1000 live births  | 1 | 2000-07-01 |
| cm_u5mr      | Under 5 years         | Under 5 mortality rate per 1000 live births            | 1 | 2000-07-01 |
| fert_tot     | N/A                   | Total births per woman                                 | 1 | 2000-01-01 |
| int_pop      | Age (years)           | Interpolated Population                                | 1 | 2000-07-01 |
| life_ex      | Age (years)           | Expected remaining years of life                       | 5 | 2000-07-01 |
| lx0          | N/A                   | Life expectancy at birth                               | 1 | 2000-01-01 |
| mort_age     | 5 years               | Number of deaths by age                                | 5 | 2000-07-01 |
| mort_tot     | N/A                   | Total deaths                                           | 1 | 2000-01-01 |
| n_survivors  | Age (years)           | Number of survivors from a birth-cohort of 100,000     | 5 | 2000-07-01 |
| net_mig_rate | N/A                   | Net Migration Rate                                     | 5 | 2000-07-01 |
| p_dying      | Age (years)           | Probability of dying in a given age range              | 5 | 2000-07-01 |
| qq_births    | N/A                   | Quinquennial number of births                          | 5 | 2000-07-01 |
| qq_pop       | Age (years)           | Quinquennial Population                                | 5 | 2000-07-01 |
| tot_pop      | Age (years)           | Total Population                                       | 1 | 2000-07-01 |
| unwpp_imr    | Under 1 years         | Under 1 mortality rate per 1000 live births            | 1 | 2000-01-01 |
| unwpp_u5mr   | Under 5 years         | Under 5 mortality rate per 1000 live births            | 1 | 2000-01-01 |

y = year_step_size
rd = reference_date

