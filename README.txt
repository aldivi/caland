## The CALAND model and diagnostics ##
# Version 2

## written by:
# Alan Di Vittorio
# Maegen Simmonds
# Lawrence Berkeley National Laboratory
## for:
# California Natural Resources Agency
# copyright 2016-2017

# This is the carbon and ghg accounting model for CA natural and working lands

# CAlifornia natural and working LANDs carbon and greenhouse gas model

# There are four scripts in the caland/ directory, and each define a function
#	1) CALAND.r: This is the carbon accounting model
#	2) plot_caland.r: This generates many diagnostic output plots from two or more CALAND() output files
#	3) plot_scen_types.r: This plots land types on a single graph for a single scenario, using outputs from plot_caland()
#	4) write_caland_inputs.r: This writes the detailed input files (carbon_input.xls and multiple scenario files) from the specific area and carbon density files and from more generic C parameters and management scenarios

# These functions are designed to be run in the order above, with all desired scenarios completed with CALAND before plotting figures

# inputs/ca_carbon_input.xlsx contains the initial carbon state and all the carbon flow parameters

# There are five scenario input files in the inputs/ directory
# These are excel files
#	These scenarios are for the 2017 Version 2, and based on the November 2016 version 1 of CALAND for comparison
#	The historical Baseline scenario is Baseline_frst2xmort_fire_input.xlsx
#	Low land conversion protection with Baseline management is LowProtect_BaseManage_frst2xmort_fire.xlsx
#	High land conversion protection with Baseline management is HighProtect_BaseManage_frst2xmort_fire.xlsx
#	Baseline land conversion protection with Low management is BaseProtect_LowManage_frst2xmort_fire.xlsx
#	Baseline land conversion protection with High management is BaseProtect_HighManage_frst2xmort_fire.xlsx
# These scenarios include 2 times the baseline forest mortality rate from 2015 to 2025 to simulate the recent and expected tree die-off due to beetles and drought

############################
# downloading CALAND from github #

#	1) Sign up for a free github account on github.com
#	2) Navigate to the main page of the caland repository (put URL here)
#	3) Under repository name, click Clone or download
#	4) In the Clone with HTTPs section, click the clipboard icon to copy the clone URL for the repository
#	5) Open Terminal (MAC and Linux), or Git Bash (Windows)
#	6) Change the current working directory to the location where you want the cloned directory to be made
#	7) Type “git clone” (no quotes) and then paste the URL you copied in Step 3
#	8) Press Enter. Your local clone will be created

# Once you have a local copy on your machine, open and source the R scripts in R (to load the functions), make sure that the working directory in R is caland/, run CALAND for at least two scenarios using CALAND(), and make some plots using plot_caland() and plot_scen_types()
# Read the details and use the examples below to run the functions successfully

############################
# CALAND() #

# Inputs
# carbon_input.xlsx
#	The initial carbon density, carbon fluxes, and management/fire/conversion carbon adjustments
# <scenario_name>.xlsx
#	The initial land area, managed area, fire area, and annual changes to these areas; also the annual mortality rates
#	Name the file something appropriate
# These files have matching formats, including the number of preceding rows, the land types, the ownership, the management, the fire

# Outputs
# <scenario_name>_output_###.xlsx
# "_output_" is appended to the input scenario name, then a tag to denote which input values were used (e.g., the default ### = "mean")
# output precision is to the integer (for ha and Mg C and their ratios)

# This model follows basic density (stock) and flow guidelines similar to IPCC Tier 3 protocols
# The initial year area data are used for the first year for carbon operations
# The area data for each year are operated on by the carbon adjustments for management, flux, and fire within that year
#	first eco fluxes are applied, then management, then fire
# Land conversion area changes are applied after the carbon flux operations, and conversion carbon fluxes assigned to that same year
# Each subsequent step uses the updated carbon values from the previous step
# The new carbon density and area are assigned to the beginning of next year

# Output positive flux values are land uptake; negative values are losses to the atmosphere

# all wood products are lumped together and labeled as "wood"
# wood product emissions are based on landfill decay of discarded products

# This R script is in the caland directory, which should be the working directory
#	Open R by opening this R script, or set the working the directory to caland/
# setwd("<your_path>/caland/")

# The input excel files are in caland/inputs/ (unless a sub-directory <indir> is specified as different from the default "")
# Output excel file is written to caland/outputs/ (unless a sub-directory <outdir> is specifed as different from the default "")

# CALAND() has 11 arguments:
#	scen_file		name of the scenario file; assumed to be in caland/inptus/<indir>
#	c_file			name of the carbon parameter input file; assumed to be in caland/inputs/<outdir>
#	indir			name only of directory in caland/inputs/ that contains scen_file and c_file; do not include "/" character at the end
#	outdir			name only of directory in caland/outputs/ that contains scen_file and c_file; do not include "/" character at the end
#	start_year		simulation begins at the beginning of this year
#	end_year		simulation ends at the beginning of this year (so the simulation goes through the end of end_year - 1)
#	value_col_dens		select which carbon density values to use; 5 = min, 6 = max, 7 = mean, 8 = std dev
# 	value_col_accum 	select which carbon accumulation values to use; 5 = min, 6 = max, 7 = mean, 8 = std dev
#	ADD_dens			for use with value_col_dens ==8: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
#	ADD_accum			for use with value_col_accum ==8: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
#	WRITE_OUT_FILE		TRUE= write the output file; FALSE= do not write the output file

# notes:
# carbon calcs occur in start_year up to end_year-1
# end_year denotes the final area after the changes in end_year-1
# density values are the stats of the total pixel population within each land type id
# accumulation values are stats of literature values

# example:
# historical baseline:
CALAND(scen_file=“Baseline_frst2xmort_fire.xlsx”, c_file = "carbon_input.xlsx", indir = "", outdir = "", start_year = 2010, end_year = 2051, value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, WRITE_OUT_FILE = TRUE)
# high land conversion protection and baseline management:
CALAND(scen_file=“HighProtect_BaseManage_frst2xmort_fire.xlsx”, c_file = "carbon_input.xlsx", indir = "", outdir = "", start_year = 2010, end_year = 2051, value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, WRITE_OUT_FILE = TRUE)

#############################
# plot_caland() #

# make diagnostic plots (.pdf files) and associated text files (.csv files) of two or more CALAND scenario outputs

# this function ca loop over all the land types, all the regions, and all the ownerships
#  the default is to aggregate the ownerships

# get only the year columns (not the change column)

# make sure that the working directory is caland/
# This R script is in the caland directory, which should be the working directory
#	Open R by opening this R script, or set the working the directory to caland/
# setwd("<your_path>/caland/")

# the model output files for plotting are in caland/<data_dir>
# the plots are put into caland/<data_dir>/<figdir>/ within each region, land type, and ownership directory
#	where <data_dir> and <figdir> is an argument to this function

# plot_caland() has 8 arguments:
#	scen_fnames		array of scenario output file names; assumed to be in data_dir
#	scen_lnames		array of scenario labels associated with scen_fnames
#	scen_snames		array of scenario short lables associated with scen_fnames (up to 8 character labels for bar graphs)
#	data_dir		the path to the directory containing the caland output files; do not include the "/" character at the end; default is "./outputs"
#	reg				array of regions to plot; can be any number of available regions (all are below as default)
#	lt				array of land types to plot; can be any number of available types (all are below as default)
#	own				array of ownerships to plot; can be any number of available types (only "All_own" is the default, which is the aggregation of ownerships)
#	figdir			the directory within data_dir to write the figures; do not include the "/" character at the end
#	INDIVIDUAL		flag: set to TRUE when comparing a single practice scenario with a static control scenario to get per managed area diagnostics

# notes:
# need at least two scenarios for this to work
# the first scenario in the array is assumed to be the baseline
# scen_fnames, scen_lnames, and scen_snames all need to be equal length vectors
# this takes ~40 minutes for a single region with 5 scenarios, all land types, and aggregated ownership

# recommended:
#  use 4 multiple command land instances (in terminal windows) of R to run this function
#  run 3 regions in each of three instances, and the "Ocean" and "All_Region" regions in the fourth instance
#  all should be done in about 2 hours
#  if you want to plot individual ownerships it may be better to restrict the plots to a single region and/or land type

# available regions:
#	"Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "Ocean", "All_region"

# available land types:
#	"Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass", "All_land"

# available ownerhsips:
#	"All_own", "BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild"

# example:
# plot the high protection/baseline management in reference to the historical baseline
plot_caland(scen_fnames = c(“Baseline_frst2xmort_fire_output_mean.xlsx”, “BaseProtect_HighManage_frst2xmort_fire_output_mean.xlsx”), scen_lnames = c(“Baseline”, “HighManage”), scen_snames = c(“BASE”, “BOHM”), reg = c("All_region"), lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass", "All_land"), own = c("All_own"), figdir = "figures”, INDIVIDUAL = FALSE)

#############################
# plot_scen_types() #

# put a variable for specified land types within a region and ownership on the same plot, for a specified scenario

# this function reads the csv files produced by plot_caland()

# make sure that the working directory is caland/
# the csv files are assumed to be in <data_dir>/<figdir>, in the appropriate land type and ownership directories
# resulting output will go directly into <data_dir>/<figdir>/<region>/<own>, which should be the same as used in plot_caland()
# setwd("<your_path>/caland/")

# plot_scen_types() has 6 arguments:
#	scen_lname		name of scenario as given in plot_caland(scen_lnames)
#	varname			name of variable to plot (see the outputs from plot_caland)
#						this name is between the land type and "_output" in these file names; do not include the surrounding "_" characters
#	ylabel			y label for the plot; this indicates the units and whether it is a difference from baseline
#	data_dir		the path to the directory containing the caland output files; do not include the "/" character at the end; default is "./outputs"
#	file_tag		additional tag to file name to note what regions/lts/ownerships are included; default is "" (nothing added)
#	reg				array of region names to plot (see below)
#	lt				array of land types to plot; can be any number of available types (all but Seagrass are below as default; All_land is excluded)
#	own				array of ownerships to plot; can be any number of available types (only "All_own" is the default)
#	figdir			the directory within data_dir to write the figures; do not include the "/" character at the end

# available regions:
#	"Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "All_region"

# available land types:
#	"Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass", "All_land"

# available ownerhsips:
#	"All_own", "BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild"

# Note: plotting the ocean region doesn't provide any comparison with land types because only seagrass exists in the ocean

# Note: Seagrass has only a subset of the output files, so if including for All_region make sure that the desired varname is available
#	Seagrass is not in the default land type list

# example:
# plot the total organic c stock for the individual land types in one figure, for the high protection/baseline manage scenario, for all regions, and "All_own"
plot_scen_types(scen_lname = “HighProtect_BaseManage”, "All_orgC_stock", "MMTC", data_dir = "./outputs", file_tag = “”, reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "All_region"), lt = c(Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all"), own = c("All_own"), figdir = "figures")


#############################
# write_caland_inputs() #

# this function generates the detailed input files from specific area and carbon density data, and more general C parameters and management scenarios
# this is necessary because there are 940 land categories, all of which have at least one record in the suite of input tables
#  and there are not input data (except for the area and carbon density data), nor management activities, for each of these categories

# using this function to create new input files is a complicated task
# all of the input data to this function have to be carefully assembled
# this is not recommended unless you have a good understanding of the required data and how the model works
# see the actual function definition file for more detail

# write_caland_inputs as 8 arguments
#	scen_tag:			(optional) a scenario file name tag; scen files are already named based on the tables in scenarios_file
#	c_file:				the carbon density and parameter file created for input to caland (needs to be .xls)
#	start_year:			this is the initial year of the simulation, one of the area files needs to be for this year
#	end_year:			this is the final year output from the caland simulation (matches the caland end_year argument)
#	parameter_file:		carbon accumulation and transfer parameters for the original 45 land categories and seagrass (xls file)
#	scenarios_file:		generic scenarios to be expanded to the actual scenario files (xls file with one scenario per table)
#	area_gis_files:		vector of two csv file names of gis stats area by land category (sq m)
#	carbon_gis_files:	vector of 13 csv file names of gis stats carbon density by land category (t C per ha)