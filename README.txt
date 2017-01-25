## The CALAND model and diagnostics ##

# This is the carbon accounting model for CA natural and working lands

# CAlifornia natural and working LANDs carbon and greenhouse gas model

# There are three scripts in the caland/ directory, and each define a function
#	1) CALAND.r: This is the carbon accounting model
#	2) plot_caland.r: This generates many diagnostic output plots from two or more CALAND() output files
#	3) plot_scen_types.r: This plots land types on a single graph for a single scenario, using outputs from plot_caland()

# These functions are designed to be run in the order above, with all desired scenarios completed with CALAND before plotting figures

# inputs/ca_carbon_input.xlsx contains the initial carbon state and all the carbon flow parameters

# There are five scenario input files in the inputs/ directory
# These are excel files
#	These scenarios were used with the November 2016 version 1 of CALAND
#	The historical Baseline scenario is ca_carbon_scen_hist_all_manage_frst2xmort_fire_input.xlsx
#	Low land conversion protection with Baseline management is ca_carbon_scen_growth1_histmanage_frst2xmort_fire.xlsx
#	High land conversion protection with Baseline management is ca_carbon_scen_growth2_histmanage_frst2xmort_fire.xlsx
#	Baseline land conversion protection with Low management is ca_carbon_scen_histgrowth_lowmanage_frst2xmort_fire.xlsx
#	Baseline land conversion protection with High management is ca_carbon_scen_histgrowth_highmanage_frst2xmort_fire.xlsx
# These scenarios include 2 times the baseline forest mortality rate from 2015 to 2025 to simulate the recent and expected tree dieoff

############################
# downloading CALAND from github #

#	1) Navigate to the main page of the repository
#	2) Under repository name, click Clone or download
#	3) In the Clone with HTTPs section, click the clipboard icon to copy the clone URL for the repository
#	4) Open Terminal (MAC and Linux), or Git Bash (Windows)
#	5) Change the current working directory to the location where you want the cloned directory to be made
#	6) Type “git clone” (no quotes) and then paste the URL you copied in Step 3
#	7) Press Enter. Your local clone will be created

# Once you have a local copy on your machine, open and source the R scripts in R (to load the functions), make sure that the working directory in R is caland/, run CALAND for at least two scenarios using CALAND(), and make some plots using plot_caland() and plot_scen_types()
# Read the details and use the examples below to run the functions successfully

############################
# CALAND #

# Inputs
# ca_carbon_input.xlsx
#	The initial carbon density, carbon fluxes, and management/fire/conversion carbon adjustments
# <scenario_name>.xlsx
#	The initial land area, managed area, fire area, and annual changes to these areas; also the annual mortality rates
#	Name the file something appropriate
# These files have matching formats, including the number of preceding rows, the land types, the ownership, the management, the fire

# Outputs
# <scenario_name>_output_###.xlsx
# "_output_" is appended to the input scenario name, then a tag to denote which input values were used (e.g., the default ### = "mean")
# output precision is to the integer (for ha and Mg C and their ratios)

# This model follows basic density (stock) and flow guidelines similar to IPCC protocols
# The initial year area data are used for the first year for carbon operations
# The area data for each year are operated on by the carbon adjustments for management, flux, and fire within that year
#	first eco fluxes are applied, then management, then fire
# Land conversion area changes are applied after the carbon operations, and conversion carbon fluxes assigned to that same year
# Each subsequent step uses the updated carbon values from the previous step
# The new carbon density and area are assigned to the beginning of next year

# Output positive flux values are land uptake; negative values are losses to the atmosphere

# all wood products are lumped together and labeled as "wood"

# This R script is in the caland directory, which should be the working directory
#	Open R by opening this R script, or set the working the directory to caland/
# setwd("<your_path>/caland/")

# The input excel files are in caland/inputs/
# Output excel file is written to caland/outputs/

# CALAND is now a function!
# 7 arguments:
#	scen_file		name of the scenario file; assumed to be in caland/inptus/
#	c_file			name of the carbon parameter input file; assumed to be in caland/inputs/
#	start_year		simulation begins at the beginning of this year
#	end_year		simulation ends at the beginning of this year (so the simulation goes through the end of end_year - 1)
#	value_col		select which carbon density and accumulation values to use; 4 = min, 5 = max, 6 = mean, 7 = std dev
#	ADD			for use with value_col==7: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
#	WRITE_OUT_FILE		TRUE= write the output file; FALSE= do not write the output file

# notes:
# carbon calcs occur in start_year up to end_year-1
# end_year denotes the final area after the changes in end_year-1
# density values are the stats of the total pixel population within each land type id
# accumulation values are stats of literature values

# example:
# historical baseline:
CALAND <- function(scen_file=“ca_carbon_scen_hist_all_manage_frst2xmort_fire.xlsx”, c_file = "ca_carbon_input.xlsx", start_year = 2010, end_year = 2051, value_col = 6, ADD = TRUE, WRITE_OUT_FILE = TRUE)
# high land conversion protection and baseline management:
CALAND <- function(scen_file=“ca_carbon_scen_growth2_histmanage_frst2xmort_fire.xlsx”, c_file = "ca_carbon_input.xlsx", start_year = 2010, end_year = 2051, value_col = 6, ADD = TRUE, WRITE_OUT_FILE = TRUE)

#############################
# plot_caland #

# make diagnostic plots (.pdf files) and associated text files (.csv files) of two or more CALAND scenario outputs

# this script loops over all the land types
#  the ownerships will be aggregated

# get only the year columns (not the change column)

# make sure that the working directory is caland/
# This R script is in the caland directory, which should be the working directory
#	Open R by opening this R script, or set the working the directory to caland/
# setwd("<your_path>/caland/")

# the model output files for plotting are in caland/outputs/
# the plots are put into caland/outputs/<figdir>/ with each land type having its own directory
#	where <figdir> is an argument to the function

# plot_caland() has 5 arguments:
#	scen_fnames		array of scenario output file names; assumed to be in caland/outputs/
#	scen_lnames		array of scenario labels associated with scen_fnames
#	scen_snames		array of scenario short lables associated with scen_fnames (up to 8 character labels for bar graphs)
#	lt				array of land types to plot; can be any number of available types (all are below as default)
#	figdir			the directory within caland/outputs/ to write the figures; do not include the "/" character at the end

# notes:
# need at least two scenarios for this to work
# the first scenario in the array is assumed to be the baseline
# scen_fnames, scen_lnames, and scen_snames all need to be equal length vectors
# this takes ~25 minutes for 2 scenarios

# example:
# plot the high protection/baseline management in reference to the historical baseline
# plot_caland <- function(scen_fnames = c(“ca_carbon_scen_hist_all_manage_frst2xmort_fire_output_mean.xlsx”, “ca_carbon_scen_growth2_histmanage_frst2xmort_fire_output_mean.xlsx”), scen_lnames = c(“Baseline”, “HighProtect_BaseManage”), scen_snames = c(“BASE”, “HPBM”), lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Agriculture", "Developed_all", "Seagrass”, “All_land”), figdir = "figures")

#############################
# plot_scen_types #

# put a variable for specified land types on the same plot, for a specified scenario

# this script reads the csv files produced by plot_caland()

# make sure that the working directory is caland/
# the csv files are assumed to be in <figdir>, in the appropriate land type directories
# resulting output will go directly into <figdir>, which should be the same as used in plot_caland()
# setwd("<your_path>/caland/")

# plot_scen_types() has 5 arguments:
#	scen_lname		name of scenario as given in plot_caland(scen_lnames)
#	varname			name of variable to plot (see the outputs from plot_caland)
#						this name is between the land type and "_output" in these file names; do not include the surrounding "_" characters
#	ylabel			y label for the plot; this indicates the units and whether it is a difference from baseline
#	lt				array of land types to plot; can be any number of available types (all are below as default; All_land is excluded)
#	figdir			the directory within caland/outputs/ to write the figures; do not include the "/" character at the end

# example:
# plot the total organic c stock for the individual land types in one figure, for the high protection/baseline manage scenario
# plot_scen_types <- function(scen_lname = “HighProtect_BaseManage”, "All_orgC_stock", "MMTC", lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Agriculture", "Developed_all", "Seagrass"), figdir = "figures")