# Women's Work: Reality & Possibilities for Arizona - 2022 Update

Authors: Kara Haberstock Tanoue, Madeleine deBlois, Terrace Ewinghill, Michele Walsh

### Purpose

In 2019, the CRED Team, in partnership with the Women's Foundation, published a white paper titled [*Women's Work: Realities and Possibilities for Arizona*](https://womengiving.org/wp-content/uploads/2022/01/WFSA-2019-research_Womens-Work.pdf). This paper identified mid-skill high-wage fields as promising options for workers in Arizona looking for careers that enable self-sufficiency and provided evidence-based suggestions on how to support women pursuing mid-skill, high-wage jobs, often in non-traditional fields. This project seeks to update that work in light of the economic disruption and labor market changes precipitated by the COVID-19 pandemic.

### File Structure

-   `raw-data`: raw data files
    -   `bls`: Employment data from BLS and OEO
        -   `AZ_OEO_projections_202030.csv`: 2020-2030 occupation projections from AZ Office of Economic Opportunity retrieved from <https://www.azcommerce.com/oeo/labor-market/employment-projections/> on 7 Sept 2022
        -   `AZ_OEWS_May2021extract.csv`: May 2021 State Occupational Employment and Wage Statistics data from U.S. Bureau of Labor Statistics retrieved from <https://www.bls.gov/oes/tables.htm> on 7 Sept 2022
        -   `TypicalEdTrainingReq_EP_2021.csv`: 2021 Education & training assignments by detailed occupation from U.S. Bureau of Labor Statistics Employment Projections retreived from <https://www.bls.gov/emp/tables/education-and-training-by-occupation.htm> on 7 Sept 2022
    -   `acs`
        -   pums data will go here
-   `clean-data`
    -   `az_occdata_may21.csv`: cleaned and merged dataset with 2021 wage data, 2021 education & training requirements, and 2020-2030 job projections for all detailed occupations in Arizona
    -   `az_promising_occupations21.csv`: filtered dataset of promising occupations in Arizona based on previously delineated criteria for mid-skill, high-wage jobs
-   `scripts`
    -   `01-extract-clean-BLS-data.R`: Script to process and clean BLS data and create merged occupation dataset
    -   `02-filter-BLS-data.R`: Script to filter occupation dataset to promising occupations
