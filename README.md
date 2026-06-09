# Nuclear Energy Critical Minerals Dashboard

## Project Overview

This project is an interactive Shiny dashboard that examines production, reserves, supply concentration, and trade patterns for minerals connected to nuclear energy and related strategic technologies. The dashboard focuses on boron/borates, rare earths, uranium, and zirconium/hafnium.

The central question addressed by the dashboard is:

**How concentrated are the production and trade networks for nuclear energy critical minerals, and what does that reveal about supply-chain vulnerability?**

The dashboard is designed to help users identify major producer countries, compare production and reserve patterns, evaluate supply concentration, and examine trade exposure through bilateral import and export flows.

## Live Dashboard

The dashboard is deployed here:

https://ceja-guzman.shinyapps.io/Nuclear_Energy_Critical_Minerals_Dashboard/

## Why These Minerals Matter

These minerals are relevant to nuclear energy, reactor materials, fuel supply, and related advanced technologies.

* **Boron / Borates:** Relevant for neutron absorption, reactor control, shielding, and boron carbide applications.
* **Rare Earths:** Important for advanced manufacturing, magnets, electronics, defense-related technologies, and broader strategic supply chains.
* **Uranium:** Directly tied to nuclear fuel after processing, conversion, enrichment, and fabrication.
* **Zirconium / Hafnium:** Zirconium alloys are used in nuclear fuel cladding, while hafnium is linked to zirconium supply chains and is used in nuclear control rods.

## Data Sources

The dashboard uses prepared data from three main sources:

1. **U.S. Geological Survey Mineral Commodity Summaries**

   * Used for boron/borates, rare earths, and zirconium/hafnium production and reserve data.

2. **British Geological Survey World Mineral Statistics**

   * Used to supplement uranium production data.

3. **CEPII BACI HS17 V202601**

   * Used for bilateral mineral-related trade flows by exporter, importer, year, and HS product category.

The dashboard uses cleaned and prepared CSV files:

```text
data/project_data.csv
data/import_data.csv
```

The raw BACI files are not included in the deployed app or GitHub repository because they are large and are only needed for data preparation.

## Dashboard Features

The dashboard includes the following sections:

* **Overview:** Summarizes the selected mineral, metric, year, leading country, and concentration indicators.
* **Country Profile:** Shows production, reserves, and trade patterns for a selected country.
* **Country Comparison:** Compares selected countries over time for the chosen mineral and metric.
* **Concentration:** Calculates top-country, top-three, top-five, and HHI concentration measures.
* **Rankings:** Displays the leading producer or reserve-holding countries for the selected mineral and year.
* **Trade Dependence:** Connects production patterns with bilateral trade exposure.
* **Global Map:** Maps reported production or reserves by country.
* **Trade Scale Map:** Shows importer or exporter scale for selected mineral-related trade categories.
* **Trade Flows Map:** Visualizes bilateral exporter-to-importer trade flows.
* **Methods & Coverage:** Explains data coverage, mineral categories, known limitations, and mineral uses.
* **Data & Sources:** Documents the dashboard-ready data, raw records, duplicate flags, and data availability.

## Methods

Production and reserve values are standardized to metric tons where possible. Country-level shares are calculated within each selected mineral, metric, and year. Concentration metrics are calculated using reported country-level values.

The dashboard calculates:

* Top producer or reserve holder share
* Top three country share
* Top five country share
* Herfindahl-Hirschman Index
* Country rankings
* Bilateral import and export flows
* Trade exposure to major producers

Trade data are based on HS product categories. These categories are useful for identifying broad trade exposure, but they should not be interpreted as exact physical traces of mine output or complete supply chains.

## Known Limitations

Several limitations should be considered when interpreting the dashboard:

* Data availability varies by mineral and year.
* Some early-year rare earth and zirconium results appear incomplete and likely reflect source coverage or cleaning limitations.
* Rare earths are grouped as an aggregate category rather than separated by individual element.
* Zirconium and hafnium are closely linked because hafnium is commonly associated with zirconium-bearing minerals.
* Uranium uses BGS data, while several other minerals use USGS data, so coverage structures are not perfectly identical.
* BACI HS trade flows measure trade exposure, not exact physical supply-chain dependence.
* Missing countries may reflect non-reporting, aggregation, withheld data, unavailable data, or source limitations rather than true zero production.

## Repository Structure

```text
Nuclear_Minerals_Dashboard/
├── app.R
├── README.md
├── .gitignore
├── .rscignore
└── data/
    ├── project_data.csv
    └── import_data.csv
```

Files excluded from GitHub and shinyapps.io deployment include:

```text
data/baci_raw/
.Rapp.history
data/.DS_Store
prep_data.R
prepare_import_data.R
```

## How to Run Locally

To run the dashboard locally in R:

```r
shiny::runApp()
```

Or, if running from outside the project folder:

```r
shiny::runApp("~/Desktop/Nuclear_Minerals_Dashboard")
```

Required R packages include:

```r
install.packages(c(
  "shiny",
  "bslib",
  "tidyverse",
  "plotly",
  "DT",
  "countrycode",
  "scales",
  "maps"
))
```

## Authors

Esteban Ceja
Jimmy Guzman
UC San Diego School of Global Policy and Strategy
Spring 2026
