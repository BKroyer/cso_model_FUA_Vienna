# cso-modell-upper-danube

Welcome to the CSO model implementation based on the hydrological model by Quaranta et al.!

This project is a replicable code version of the published model, validated against the supplementary files, typo-corrected and with options to apply it to any area of interest and spatial reference units. 

It is created using ProjectTemplate, which is an R package that helps organize statistical
analysis projects. For more details about ProjectTemplate, see http://projecttemplate.net.

Every analysis file contains the following two
lines of R code:

	library('ProjectTemplate')
	load.project()

Running these two lines automatically does the following:
* Reading in the global configuration file contained in `config`.
* Loading any R packages listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing the data using the files in the `munge` directory.
* Running all files in lib/, including the `setup` file and the main model function.

Use the `setup` file (in lib/) for all subsequent settings such as area of interest, model parameters, spatial reference units and time frame. If needed, for the preprocessing of the input data, from src/ use files `01_download_and_crop_GloH2O_MSWEP_precipitation`, `02_extract_precipitation_time_series_for_settlements` and `03_prepare_additional_geodata`. Once the preprocessing is complete, go back to the `setup` file and specify the paths to the newly processed data. Then, run `04_data_import_processing` (in src/) for loading the data and running the model in parallel processing over the spatial reference units. Model output will be saved as Excel files per settlement units and as summary statistics across years and settlement units. Code snippets for various complimentary visualisations are given in file `Additional_visualisations` (also in src/).

If the model output shall be validated against measurement data, there is the option to set "validation_region" to TRUE in the `setup` file. This, however, requires the presence of a "Validation_data.xlsx" file in the data/ path. This "Validation_data.xlsx" needs at a minimum columns "Code" and "Value" with "Code" matching the gridcode of the spatial reference units and "Value" indicating the measurement values, expected as m³ per year.

Here is an overview of the project structure including short descriptions of the purpose of each file:

- **cso-modell-upper-danube/**
	- **config/** _Runs every time_
		- **global.dcf** _Project settings and which libraries to load_
	- **data/** _Input data such as precipitation, imperviousness and CS share_
	- **data_NOTREAD/** _Stores/saves intermediate results and preprocessed data_
	- **lib/** _Runs every time_
		- **cache/** _Runs every time, can store data (paths) temporarily_
		- **cso_model_helpers.cpp** _The translations to C++_
		- **cso_run_on_single_grid.R** _Helper function to parallelize gridcodes in cso model_
		- **model_cso.R** _Contains the main model function cso_model_
		- **Prepare_GloH2O_MSWEP_data.R** _Functions to download precipitation data from MSWEP googledrive_
		- **setup.R** _Basic model run settings_
		- **validation_functions.R** _Functions to process results and validation data_
		- **Preprocess_data.R** _Function to preprocess data (the AoI and settlements)_
	- **munge/** _Could store preprocessing scripts, here only unused examples_
	- **output/** _Storage for final results_
	- **src/** _Processing scripts to run manually_
		- **01_download_and_crop_GloH2O_MSWEP_precipitation.R** _Downloads precipitation data and crops to AoI_
		- **02_extract_precipitation_time_series_for_settlements.R** _Extracts and saves (.rds) precipitation time series for each settlement._
		- **03_prepare_additional_geodata.R** _Preprocessing of imperviousness, population and share of CS data._
		- **04_cso_data_import_processing.R** _loads relevant input data, parallel processing over gridcodes, aggregates and saves results_
		- **Additional_visualisations.R** _Some plots (precipitation, results,...)_
		- **Comparing_to_Excel.R** _Saved model run settings from Replication phase_
		- **Excel_formula.txt** _Copies from Excel commands_
		- **Investigate_lnB_A.R** _Visualising influence of ln typo_


