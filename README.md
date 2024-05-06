# Files description

-   `01_data_preparation.r`

    -   phenotype data preparation and processing steps for polygenic scores (PGS).

    -   PGS were generated in LDpred2 using code available [here](https://github.com/AndreAllegrini/LDpred2).

-   `02_base_models.r`

    -   second-order and bifactor models.

-   `03_*_mimic_models_*.r`

    -   second-order multiple indicators multiple causes (MIMIC) trio, parent an child models.

    -   to run as batch job: `mimic-models.sh`

-   `04_extract_results.r`

    -   extract parameter estimates, model fit, and perform nested model comparisons.

-   `05_format_results_tables.Rmd`

    -   format summary-level data for tables and figures.

-   `power-sims.r`

    -   power simulations for trio MIMIC models across a range of effect sizes of interest.

    -   run as batch job: `power-sims.sh`

    -   note: (range of) child/mother/father effect sizes in trio (conditional) model can be manually changed at line 50.

    -   usage: `Rscript --vanilla power-sims.r sample_size iterations cores_cl SEED`

        -   *sample_size* = analyses N, number of trios. `Default = 15000`

        -   *iterations* = number of iteratons for simulations. `Default = 10000`

        -   *cores_cl* = number of cpu cores. `Default = 32`

        -   *SEED* = seed for reproducibility. `Default = 48151623`

-   `figures.Rmd`

    -   code for main manuscirpt figures (analyses results)

    -   summary-level data to reproduce figures in `output/` directory
