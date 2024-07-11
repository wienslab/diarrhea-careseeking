## Source code and data for _Care seeking for diarrheal illness: a systematic review and meta-analysis_
#### Kirsten E. Wiens, Marissa H. Miller, Daniel J. Costello, Ashlynn P. Solomon, Skye M. Hilbert, Andrea G. Shipper, Elizabeth C. Lee, Andrew S. Azman

### Instructions

1. Find data extracted from studies in the systematic review in folder data/extracted_data.
2. Download world bank income classifications from the link below and save in folder data/extracted_data.

    + World bank classifications: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
    + Note that classifications were downloaded on May 6, 2024 for analyses in the manuscript, and classifications of HIC vs LMIC had not changed historically for the countries included

3. Download shapefile from the links below and save in folder data/shapefiles.

    + Global admin 0 shapefile with iso3 codes: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/

4. Use "code/data_quality_checks.Rmd" to run quality checks on the data extracted in the systematic review.
5. Use "code/main.Rmd" to run analyses that correspond to figures and tables in manuscript.
6. Old analyses using a stan program can be found in fold code/stan_analysis, and corresponding modeled results can be found in data/generated_data. These are FYI only and not included in the manuscript.

### Scripts in code folder

| File                       | Description                                                                                                  |
| :------------------------- |:-------------------------------------------------------------------------------------------------------------|
| data_quality_checks.Rmd    | Script to clean and run data quality checks on extracted data; any issues are reported as warnings           |
| main.Rmd                   | Notebook that runs data cleaning, all analyses, and creates tables/figures for the manuscript                |
| utils.R                    | Helper functions used in Rmd notebooks                                                                       |
