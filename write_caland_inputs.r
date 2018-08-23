# write_caland_inputs.r

# generate the caland input files based on the gis stats and parameters from the literature
#	make .xls files

# this script defines function write_caland_inputs() for writing the intput files
# arguments
#	scen_tag:			(optional) a scenario file name tag; scen files are already named based on the tables in scenarios_file
#	c_file:				the carbon density and parameter file created for input to caland (needs to be .xls)
#	start_year:			this is the initial year of the simulation, one of the area files needs to be for this year
#	end_year:			this is the final year output from the caland simulation (matches the caland end_year argument)
#	CLIMATE:				projected climate or BAU climate; "HIST" or "PROJ"; affects wildfire and veg and soil carbon accum values;
#		the projected scenario is determined by the fire and climate input files
# 	land_change_method: "Landcover" will use original method of remote sensing landcover change from 2001 to 2010. "Landuse_Avg_Annual" will 
#    use avg annual area change from 2010 to 2050 based on projected land use change of cultivated and developed lands (USGS data). 

#	parameter_file:		carbon accumulation and transfer parameters for the original 45 land categories and seagrass (xls file)
#	scenarios_file:		generic scenarios to be expanded to the actual scenario files (xls file with one scenario per table)
# 	units_scenario: 	specify units for input areas in scenarios_file - must be ac or ha.
#	climate_c_file:		the climate scalars for vegetation and soil carbon accumulation; future scenario must match the fire input file
#	fire_area_file:		annual burned area by region-ownership; future scenario must match the climate c file
#	mortality_file:		mortality rate as annual fraction of above ground biomass; includes values for all woody land types that have c accumulation in vegetation
#	area_gis_files_orig:		vector of two csv file names of gis stats area by land category (sq m)
#	area_gis_files_new:		vector of one csv file name of gis stats area by land category (sq m)
#	carbon_gis_files:	vector of 13 csv file names of gis stats carbon density by land category (t C per ha)
#	forest_mort_fact:		the value by which to adjust the forest mortality during the period specified by forest_mort_adj_first/last
#	forest_mort_adj_first:	the first year to adjust forest mortality
#	forest_mort_adj_last:	the last year to adjust forest mortality
# 	control_wildfire_lulcc:	if TRUE, read control_wildfire_lulcc_file to shut off wildfire, or lulcc, or both
#	control_wildfire_lulcc_file: file with flags for each scenario in scenarios_file to control wildfire and lulcc

# parameter_file
#	8 tables
#	vegc_uptake, soilc_accum, conversion2ag_urban, forest_manage, dev_manage, grass_manage, ag_manage, wildfire
#	column headers are on row 12
#	only non-zero values are included; more land type, ownership, management, or severity rows can be added as appropriate (if the column exists)
#	this xls file is a subset of the original ca_carbon_input.xlsx file, as a starting point for filling in the new parameter tables

# scenarios_file
#	one scenario per table; can have many scenarios defined in this file
#	the table names determine the output scenario file names
#	defines the scenarios fairly concisely, as given
#	10 columns each: Region, Land_Type, Ownership, Management, start_year, end_year, start_area, end_area, start_frac, end_frac
#	either area or frac values are used; the other two columns are NA
# units can be in ha or ac, which must be scecified by the "units_scenario" argument 
#	Region and Ownership can be "All" or a specific name
#	Each record defines management for a specific time period; outside of the defined time periods the management values are zero

# climate_c_file
#	One file contains both vegetation and soil scalars
#	Three ID columns: Region, Ownership, Component; all possible Region-Ownership combos are included (81), even if they don't exist
#	Then one column for each year, starting with 2010
#	The component is either "Vegetation" or "Soil"
#	These values directly scale the vegetation and soil c accumulation values in CALAND

# fire_area_file
#	Two ID columns: Region, Ownership; all possible Region-Ownership combos are included (81), even if they don't exist
#	Then one column for each year, starting with 2010
#	The initial year fire area is an average from 2001-2015
#	The BAU fire is the initial fire area extrapolated through the sim (same value, no trend)
#	Fire severity is estimated as fractions of the annual totals, and high sev frac increases at a historical rate
#	Non-regen area is parameterized as a function of the high severity fraction and a threshold distance from burn edge
#	Projected climate fire is directly from the area file, and is currently area only, so other aspects are estimated as for BAU fire

# mortality_file
#	Three ID columns: Region, Land_Type, Ownership
#	One data column: Mortality_frac
#	this is the annual mortality rate as a fraction of above ground biomass
#	includes values for all woody land types that have c accumulation in vegetation
#	all valid land types must be explicitly specified: Shrubland, Savanna, Woodland, Forest, Developed_all
#	region and/or ownership can be "All"

# area_gis_files (sq m)
#	7 columns
#	region code, region name, land type code, land type name, ownership code, ownership name, area
#	these two csv files contain the text labels and integer codes for Region, Land_type, and Ownership
#	the Land_cat integer codes will be calculated and then matched with the carbon_gis_files integer codes
#	the year has to be in the name of the file; currently assumes that the years are 2001 and 2010

# carbon_gis_files (t C per ha)
#	14 columns
#	land category code, label (blank), valid cell count, null cell count, min, max, range, mean, mean of absolute values, sdddev, variance, coeff_var, sum, sum_abs
#	use the min, max, mean, and stddev
#	these are the density and standard error csv files (13) for the seven carbon pools, with values by land category
#	above ground, below ground, downed dead, standing dead, litter, understory are from ARB inventory
#	soil organic c is from gSSURGO, and we assume that any zero values in the original data set are non-valid when calculating the land category averages
#		there is 12236544 ha (mostly desert) with no data that is converted to zero when the raster is created, and only 17601 ha (mostly rivers) with zeros in the gssurgo data

# control_wildfire_lulcc_file
#	flags to dtermine whether wildfire and/or lulcc will happen
#	three columns, in order: Scenario, Wildfire, LULCC
#		Scenario: matches the worksheet names in the scenarios_file; these scenario names MUST MATCH between these two files
#		Wildfire: 1=wildfire; 0=no wildfire
#		LULCC: 1=lulcc; 0=no lulcc

# mortality
# the recent and expected forest mortality due to insects and drought is emulated by a doubled forest mortality rate for 10 years (2015-2024)
# mortality applies only to woody systems with veg c accumulation
# these types are: shrubland, savanna, woodland, forest, and developed_all
# developed_all mortality is processed differently from the others
#  the morality from the scenario is transferred to the above ground harvest of the dead_removal management
#  this is because the urban system is highly managed, and allows for more control of what happens to the dead biomass

# the only land categories available throughout the sim are those that are included in the input files

# restoration
# fresh marsh comes out of only private and state land in the delta, so make sure that these are included (for now, all existing cultivate delta land categories are included as potential fresh marsh)
#	      so far, this is the only land type for which land categories need to be added (except for seagrass)
# coastal marsh comes out of only private and state land in the coastal regions, so make sure these are available, from cultivated
# meadow restoration is only in the sierra cascades, and in private, state, and usfs nonwilderness, from shrub, grass, savanna, and woodland
# woodland comes from cultivated and grassland
# afforestation is forest area expansion and comes from shrubland and grassland
# reforestation is forest area expansion that comes from shrubland only to match non-regeneration of forest

# output files
# areas are in ha (scenario files)
# carbon file:
#  densities are in Mg C per ha (t C per ha)
#  factors are fractions

# this takes about an hour for 5he 5 original scenarios

#setwd("/Users/adivi/projects/cnra_carbon/caland")

# this enables java to use up to 4GB of memory for reading and writing excel files
options(java.parameters = "-Xmx4g" )

# Load all the required packages
libs <- c( "XLConnect" )
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, 
              and install it for local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

########### set these here so that I can work without running the function
scen_tag = "frst2Xmort_fire"
#scen_tag = "ind"
#scen_tag = "ind_RCP85"
c_file = "carbon_input_nwl.xls"
#c_file = "carbon_input_ind.xls"
#c_file = "carbon_input.xls"
start_year = 2010
end_year = 2101
#end_year = 2051
CLIMATE = "HIST"
#CLIMATE = "PROJ"
parameter_file = "lc_params.xls"
#scenarios_file = "orig_scenarios.xls"
scenarios_file = "nwl_scenarios_v3_ac.xls"
#scenarios_file = "individual_proposed_sims_41_year_application_all.xls"
units_scenario <- "ac"
#units_scenario = "ha"
climate_c_file = "climate_c_scalars_unitary.csv"
#climate_c_file = "climate_c_scalars_iesm_rcp85.csv"
fire_area_file = "fire_area_canESM2_85_bau_2001_2100.csv"
mortality_file = "mortality_annual_july_2018.csv"
area_gis_files_new = "CALAND_Area_Changes_2010_to_2051.csv"
land_change_method = "Landuse_Avg_Annual"
# land_change_method = "Landcover"
area_gis_files_orig = c("area_lab_sp9_own9_2001lt15_sqm_stats.csv", "area_lab_sp9_own9_2010lt15_sqm_stats.csv")
carbon_gis_files = c("gss_soc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_agc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_agc_tpha_sp9_own9_2010lt15_stats.csv", 
                     "lfc_bgc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_bgc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_ddc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                     "lfc_ddc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_dsc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_dsc_tpha_sp9_own9_2010lt15_stats.csv", 
                     "lfc_ltc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_ltc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_usc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                     "lfc_usc_tpha_sp9_own9_2010lt15_stats.csv")
forest_mort_fact = 2
forest_mort_adj_first = 2015
forest_mort_adj_last = 2024
control_wildfire_lulcc <- FALSE
#control_wildfire_lulcc_file = "orig_scenarios_control_no_lulcc.csv"
control_wildfire_lulcc_file = "individual_proposed_sims_control_lulcc_wildfire.csv"

write_caland_inputs <- function(scen_tag = "frst2Xmort_fire", c_file = "carbon_input_nwl.xls", start_year = 2010, end_year = 2101, 
                                CLIMATE = "HIST", parameter_file = "lc_params.xls", scenarios_file = "nwl_scenarios_v3_ac.xls",
                                units_scenario = "ac",
                                climate_c_file = "climate_c_scalars_unitary.csv",
                                fire_area_file = "fire_area_canESM2_85_bau_2001_2100.csv",
                                mortality_file = "mortality_annual_july_2018.csv",
                                area_gis_files_new = "CALAND_Area_Changes_2010_to_2051.csv", land_change_method = "Landuse_Avg_Annual",
                                area_gis_files_orig = c("area_lab_sp9_own9_2001lt15_sqm_stats.csv", "area_lab_sp9_own9_2010lt15_sqm_stats.csv"), 
                                carbon_gis_files = c("gss_soc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_agc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                                                     "lfc_agc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_bgc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                                                     "lfc_bgc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_ddc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                                                     "lfc_ddc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_dsc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                                                     "lfc_dsc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_ltc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                                                     "lfc_ltc_tpha_sp9_own9_2010lt15_stats.csv", "lfc_usc_se_tpha_sp9_own9_2010lt15_stats.csv", 
                                                     "lfc_usc_tpha_sp9_own9_2010lt15_stats.csv"),
								forest_mort_fact = 2,
								forest_mort_adj_first = 2015,
								forest_mort_adj_last = 2024,
								control_wildfire_lulcc = FALSE,
								control_wildfire_lulcc_file = "individual_proposed_sims_control_lulcc_wildfire.csv") {
	
cat("Start write_caland_inputs at", date(), "\n")

num_c_in_files = length(carbon_gis_files)

# give warning if the climate_c_file does not correspond correctly with the historic CLIMATE setting 
if (CLIMATE == "HIST" & climate_c_file != "climate_c_scalars_unitary.csv") {
  stop("Historic climate (CLIMATE = 'HIST') must be paired with climate_c_file = 'climate_c_scalars_unitary.csv'")
}

# give warning if the climate_c_file does not correspond correctly with the projected CLIMATE setting 
if (CLIMATE == "PROJ" & climate_c_file == "climate_c_scalars_unitary.csv") {
  stop("Projected climate (CLIMATE = 'PROJ') must be paired with the RCP4.5 or RCP8.8 climate_c_file (not climate_c_scalars_unitary.csv)")
}

# reference year for calculating area changes from start year
ref_year = 2001
diff_years = start_year - ref_year
scen_end_year = end_year - 1

in_dir = "raw_data/"
out_dir = "inputs/"

xltag = ".xls"

c_file_out = paste0(out_dir, c_file)
# c_map_file_out = "local_files/carbon_density_map_source.xlsx" 

if (land_change_method == "Landcover") {
  scen_head_file = paste0(in_dir, "scenario_headers.xlsx")
} else {
  scen_head_file = paste0(in_dir, "scenario_headers_usgs_area_change.xlsx")
}  

param_head_file = paste0(in_dir, "parameter_headers.xlsx")

# convert acres to hectares
ac2ha = 0.404685642

# the column headers are on line 12, for both input and output files
start_row = 12

# the header goes from row 1 to row 10, and is only the first column
last_head_row = 10

# convert sq m to ha
sqm2ha = 1.0/10000

# output dataframe lists
out_scen_sheets = c("area_2010", "annual_net_area_change", "annual_managed_area", "annual_wildfire_area", "annual_mortality", "veg_climate_scalars", "soil_climate_scalars")

out_c_sheets = c("sum_allorgc_2010", "sum_biomassc_2010", "agcmain_2010", "bgcmain_2010", "usc_2010", "dsc_2010", "ddc_2010", "ltc_2010", 
                 "soc_2010", "vegc_uptake", "soilc_accum", "conversion2ag_urban", "forest_manage", "dev_manage", "grass_manage", "ag_manage", "wildfire")
out_c_tags = c("allorgc", "biomassc", "agc", "bgc", "usc", "dsc", "ddc", "ltc", "soc", "vegc_uptake", "soilc_accum", "conversion2ag_urban", "forest_manage", 
               "dev_manage", "grass_manage", "ag_manage", "wildfire")
num_scen_sheets = length(out_scen_sheets)
num_c_sheets = length(out_c_sheets)
out_scen_df_list <- list()
out_c_df_list <- list()
in_c_df_list <- list()
accum_df_list <- list()
man_df_list <- list()

# regions
reg_names = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")
num_reg = length(reg_names)

# land types
lt_names = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow",
       "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass")
num_lt = length(lt_names)

# ownerships
own_names = c("BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")
num_own = length(own_names)

# useful out_c_df_list indices
allc_ind = 1
biomassc_ind = 2
cpool_start = 3
cpool_end = 9
params_start = 10
params_end = 17
vegcuptake_ind = 10
soilcaccum_ind = 11
conversion_ind = 12
forest_man_ind = 13
ag_manage_ind = 16
wildfire_ind = 17

# useful indices of the input parameter files
param_start_col = c(4, 4, 2, 5, 3, 3, 4, 2)

# some default parameters

# fresh marsh land type code
# this comes out of Delta Cultivated land only
fresh_marsh_name = "Fresh_marsh"
fresh_marsh_code = 120
delta_code = 3
cult_code = 130

# seagrass
# Seagrass estimated extent range is 4451-6070 ha (NOAA)
seagrass_reg_name = "Ocean"
seagrass_lt_name = "Seagrass"
seagrass_own_name = "Other_fed"
seagrass_reg_code = 10
seagrass_lt_code = 200
seagrass_own_code = 6
seagrass_start_area_ha = 5261.00

###### mortality

# default mortality for woody land types with vegc_uptake
#	valid land types: Shrubland, Savanna, Woodland, Forest, Developed_all
#	all others need to be 0, although CALAND does check this
# this default value is just used to initialize the table
mortality_default = 0.01

###### wildfire (ha)
# assume that the intensities are: High, Medium, Low
# these intensities must match those in the parameter file
# input fire area is per region-ownership, and distributed annually in caland to land types proportionally
# the initial year fire area is the annual average of 2001-2015
# BAU fire will be determined by the 2001-2015 trend of the input file, applied to the initial area
# climate driven fire area will be determined by the annual values in the provided data file, except for initial year burn area

# do not use these any more! but useful numbers for comparison
# average burned area of 2000-2015 from CALFIRE fire perimeters dataset
#wildfire_mean = 243931.10
#wildfire_stddev = 151439.00
#wildfire_ann_val = wildfire_mean

# the years to average for the initial year burn area
initial_fire_start = 2001
initial_fire_end = 2015
num_initial_fire_years = initial_fire_end - initial_fire_start + 1

# initial fractions of burn area assigned to each severity (these must sum to 1)
high_fire_frac = 0.26
low_fire_frac = 0.45
med_fire_frac = 0.29
# BAU trend in high severity fraction, change in fraction of hs burn area per year
# other two classes will decrease proportionally
hs_fire_trend = 0.0027

######## Developed_all above and below ground C density
# regional averages of cities, based on urban forest
# mcpherson et al 2017
# the reported values have been mapped to caland in the file urban_forest_data.xlsx
# the split between above and below is based on the paper assumption that 28% of total live biomass in in roots
#  only the total live biomass is reported in the paper, and we were originally told the root values were not included
# we recently learned that the root values are included in the reported values
# use the above and below
# use the std err as the uncertainty (in the stddev column)
# add and subtract the stderr to get max and min
# the values are in the order of reg_names above
dev_all_agc_mean = c(14.27, 11.23, 11.23, 1.60, 7.49, 7.49, 17.32, 7.49, 6.23)
dev_all_bgc_mean = c(5.55, 4.37, 4.37, 0.62, 2.91, 2.91, 6.74, 2.91, 2.42)
dev_all_agc_stddev = c(0.33, 0.07, 0.07, 0.08, 0.11, 0.11, 0.59, 0.11, 0.08)
dev_all_bgc_stddev = c(0.01, 0.001, 0.001, 0.003, 0.003, 0.003, 0.02, 0.003, 0.002)
dev_all_agc_min = dev_all_agc_mean - dev_all_agc_stddev
dev_all_agc_max = dev_all_agc_mean + dev_all_agc_stddev
dev_all_bgc_min = dev_all_bgc_mean - dev_all_bgc_stddev
dev_all_bgc_max = dev_all_bgc_mean + dev_all_bgc_stddev
if (FALSE) { # don't need this any more
# now sum them into above only and propagate the error
dev_all_agc_mean = dev_all_agc_mean + dev_all_bgc_mean
dev_all_agc_stddev = sqrt(dev_all_agc_stddev^2 + dev_all_bgc_stddev^2)
dev_all_agc_min = dev_all_agc_mean - dev_all_agc_stddev
dev_all_agc_max = dev_all_agc_mean + dev_all_agc_stddev
dev_all_bgc_mean[] = 0
dev_all_bgc_stddev[] = 0
dev_all_bgc_min[] = 0
dev_all_bgc_max[] = 0
}

# these are the original statewide values from bjorkman et al 2015
#dev_all_agc_min = 0.13
#dev_all_agc_max = 47.01
#dev_all_agc_mean = 10.7
#dev_all_agc_stddev = 10.19

# Grassland, Savanna, Woodland soil c values
# statewide average of estimated total col soil c density from reviewed lit
# silver et al 2010
# keep the original stddev
range_soc_min = 47.0
range_soc_max = 246.0
range_soc_mean = 116.0

### forest regional npp values, all biomass (hudiburg et al 2009, appendix Table A2-A)
# units converted to MgC/ha/yr
# these are used to get relative regional breakdown to apply to the statewide input values
# north coast is from coast range
# eastside is from east cascades
# klamath is from klamath mountains
# sierra cascades is from sierra nevada
# deserts is from central basin
# south coast is from oak-chapparal
# central coast is from oak-chapparal
# Delta and central valley use the oak-chapparal because it is very close to the unweighted average of 4.76
reg_names = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")
reg_vals = c(4.7, 4.7, 4.7, 2.2, 3.3, 6.0, 7.8, 4.6, 4.7)
forest_npp = data.frame(Region=reg_names, npp=reg_vals)
forest_npp$Land_Type = "Forest"

### Delta adjustments
# commenting out this section - all cultivated input values are being updated in lc_params

# Delta Cultivated peatland soil c accum values MgC/ha/yr (negative value is soil c loss) (knox 2015)
# average these? Or just use corn?
# these are consistent with Hatala 2012 and Teh 2011, but are more complete in that the harvest values are included
# corn (NEE = 2.78 MgC/ha/yr soil c loss and harvest = 2.93 MgC/ha/yr)
#soil_c_accum_peat_corn = -5.71
#soil_c_accum_peat_corn_ci = 0.34
# rice (NEE = -0.5 MgC/ha/yr (which is a soil c gain) and harvest = 1.62 MgC/ha/yr)
# methane C lost is 0.053 MgC/ha/yr
#soil_c_accum_peat_rice = -1.17
#soil_c_accum_peat_rice_ci = 1.03
# average values because not all Delta land is peatland or corn
#soil_c_accum_peat_mean = -3.44
#soil_c_accum_peat_min = -5.71
#soil_c_accum_peat_max = -1.17
#soil_c_accum_peat_stddev = 3.21
#soil_c_accum_peat_avg_ci = 0.69

# If the cultivated peatland values are used for the Delta, then the SoilCaccum_frac in ag_manage needs to be 
# updated also assume the same mean benefit of 0.5 MgC/ha/yr
#	for rice, this would give: 0.67/1.17=0.57
# 	for corn, this would give: 5.21/5.71=0.91
#	for average, this would give: 2.94/3.44=0.85
#soil_c_accum_frac_peat = 0.85

  #####################################################################################################################
  ################################# process the original lancover area files ##########################################
  #####################################################################################################################
# first determine which file is the start year file
# the columns are: region code, region name, land type code, land type name, ownership code, ownership name, area
styr_ind = grep(start_year, area_gis_files_orig)
refyr_ind = grep(ref_year, area_gis_files_orig)
start_area_in = read.csv(paste0(in_dir, area_gis_files_orig[styr_ind]), header=FALSE, stringsAsFactors=FALSE)
start_area_in[,c(1,3,5)] <- as.numeric(unlist(start_area_in[,c(1,3,5)]))
ref_area_in = read.csv(paste0(in_dir, area_gis_files_orig[refyr_ind]), header=FALSE, stringsAsFactors=FALSE)
ref_area_in[,c(1,3,5)] <- as.numeric(unlist(ref_area_in[,c(1,3,5)]))
colnames(start_area_in) <- c("reg_code", "reg_name", "lt_code", "lt_name", "own_code", "own_name", "area_sqm")
colnames(ref_area_in) <- c("reg_code", "reg_name", "lt_code", "lt_name", "own_code", "own_name", "area_sqm")

# generate the land category codes and add them to the area in tables
#	region*100000 + landtype*100 + ownership
#	spatial and ownership can range from 0-99 classes (but zero is not used here)
#	land type can range from 0 to 999
#		aggregate land type (evt) is currently enumerated as multiples of 10, with water==0
start_area_in$lcat_code = start_area_in$reg_code * 100000 + start_area_in$lt_code * 100 + start_area_in$own_code
ref_area_in$lcat_code = ref_area_in $reg_code * 100000 + ref_area_in $lt_code * 100 + ref_area_in $own_code
# strip off the mismatched categories and diagnose these area differences
start_na = start_area_in[is.na(start_area_in$reg_code) & is.na(start_area_in$lt_code) & is.na(start_area_in$own_code),]
ref_na = ref_area_in[is.na(ref_area_in$reg_code) & is.na(ref_area_in$lt_code) & is.na(ref_area_in$own_code),]
cat("\nstart NA area (ha) =", start_na$area_sqm * sqm2ha, "\n")
cat("ref NA area (ha) =", ref_na$area_sqm * sqm2ha, "\n")
cat("start-ref NA area (ha) =", start_na$area_sqm * sqm2ha - ref_na$area_sqm * sqm2ha, "\n")
start_lcat_mismatch = start_area_in[is.na(start_area_in$lcat_code) & !(is.na(start_area_in$reg_code) & is.na(start_area_in$lt_code) & is.na(start_area_in$own_code)),]
ref_lcat_mismatch = ref_area_in[is.na(ref_area_in$lcat_code) & !(is.na(ref_area_in$reg_code) & is.na(ref_area_in$lt_code) & is.na(ref_area_in$own_code)),]
start_lcat_mismatch_ha = sum(start_lcat_mismatch$area_sqm * sqm2ha)
ref_lcat_mismatch_ha = sum(ref_lcat_mismatch$area_sqm * sqm2ha)
cat("\nstart lcat mismatch area (ha) =", start_lcat_mismatch_ha, "\n")
cat("ref lcat mismatch area (ha) =", ref_lcat_mismatch_ha, "\n")
cat("start-ref lcat mismatch area (ha) =", start_lcat_mismatch_ha - ref_lcat_mismatch_ha, "\n")
start_area_in = start_area_in[!is.na(start_area_in$lcat_code),]
ref_area_in = ref_area_in[!is.na(ref_area_in$lcat_code),]

# add a column for the area in ha (the values are only precise to 100 sqm)
start_area_in$start_area_ha = start_area_in$area_sqm * sqm2ha
ref_area_in$ref_area_ha = ref_area_in$area_sqm * sqm2ha
# get some necessary sums
start_area_sum_ha = sum(start_area_in$start_area_ha)
ref_area_sum_ha = sum(ref_area_in$ref_area_ha)
cat("\nstart total land area (ha) =", start_area_sum_ha, "\n")
cat("ref total land area (ha) =", ref_area_sum_ha, "\n")
cat("start-ref total land area (ha) =", start_area_sum_ha - ref_area_sum_ha, "\n")
# these are the region-ownership areas, which do not change over time
# the reference and start are nearly identical, so use the start year values
#	because the normalization of area needs to be consistent with the start area
ref_area_reg_own = aggregate(ref_area_ha ~ reg_name + own_name, ref_area_in, FUN=sum)
names(ref_area_reg_own)[ncol(ref_area_reg_own)] <- "ref_reg_own_area_ha"
start_area_reg_own = aggregate(start_area_ha ~ reg_name + own_name, start_area_in, FUN=sum)
names(start_area_reg_own)[ncol(start_area_reg_own)] <- "start_reg_own_area_ha"

# calculate the annual net area change
# need to merge the data to deal with mismatches in existing categories between the years
# first subtract 2001 from 2010
# if the area is zero in 2010 and the annual loss is < 0, set the annual loss to zero, and proportionally redistribute the loss to the other land types
# 	this means that losses in other land types will be increased and gains in other land types will be decreased
# adjust the differences by adjusting (reducing in this case) the 2001 areas proportionally by land category within ownership class and region (so that the total areas match and total differences are zero)
#	this done by adjusting the non-zero differences directly
# divide the differences by start-ref year-difference to annualize them
diff_area_in = merge(start_area_in, ref_area_in, by=c("lcat_code"), all=TRUE)
diff_area_in$start_area_ha = replace(diff_area_in$start_area_ha, is.na(diff_area_in$start_area_ha), 0)
diff_area_in$ref_area_ha = replace(diff_area_in$ref_area_ha, is.na(diff_area_in$ref_area_ha), 0)
diff_area_in$diff_area_ha = diff_area_in$start_area_ha - diff_area_in$ref_area_ha
diff_area_sum_ha = sum(diff_area_in$diff_area_ha)
# fill and drop duplicate and unnecessary columns
diff_area_in$area_sqm.x = NULL
diff_area_in$area_sqm.y = NULL
diff_area_in$reg_name.x[is.na(diff_area_in$reg_name.x)] = diff_area_in$reg_name.y[is.na(diff_area_in$reg_name.x)]
diff_area_in$lt_name.x[is.na(diff_area_in$lt_name.x)] = diff_area_in$lt_name.y[is.na(diff_area_in$lt_name.x)]
diff_area_in$own_name.x[is.na(diff_area_in$own_name.x)] = diff_area_in$own_name.y[is.na(diff_area_in$own_name.x)]
diff_area_in$reg_code.x[is.na(diff_area_in$reg_code.x)] = diff_area_in$reg_code.y[is.na(diff_area_in$reg_code.x)]
diff_area_in$lt_code.x[is.na(diff_area_in$lt_code.x)] = diff_area_in$lt_code.y[is.na(diff_area_in$lt_code.x)]
diff_area_in$own_code.x[is.na(diff_area_in$own_code.x)] = diff_area_in$own_code.y[is.na(diff_area_in$own_code.x)]
diff_area_in$reg_code.y = NULL
diff_area_in$reg_name.y = NULL
diff_area_in$lt_code.y = NULL
diff_area_in$lt_name.y = NULL
diff_area_in$own_code.y = NULL
diff_area_in$own_name.y = NULL
colnames(diff_area_in) <- c("lcat_code", "reg_code", "reg_name", "lt_code", "lt_name", "own_code", "own_name", "start_area_ha", "ref_area_ha", "diff_area_ha")
# get the region-ownership areas for the start year
diff_area_calc = merge(diff_area_in, start_area_reg_own, by=c("reg_name", "own_name"), all.x=TRUE)
diff_area_calc = diff_area_calc[order(diff_area_calc$lcat_code),]
# redistribute the unavailable losses, based on the start area propoertions
unavail_loss = diff_area_calc[diff_area_calc$diff_area_ha<0 & diff_area_calc$start_area_ha==0,]
unavail_loss_agg = aggregate(diff_area_ha ~ reg_name + own_name, unavail_loss, FUN=sum)
names(unavail_loss_agg)[ncol(unavail_loss_agg)] <- "start_reg_own_diff_area_ha"
diff_area_calc = merge(diff_area_calc, unavail_loss_agg, by=c("reg_name", "own_name"), all.x=TRUE)
diff_area_calc = diff_area_calc[order(diff_area_calc$lcat_code),]
diff_area_calc$start_reg_own_diff_area_ha = replace(diff_area_calc$start_reg_own_diff_area_ha, is.na(diff_area_calc$start_reg_own_diff_area_ha), 0)
diff_area_calc$diff_area_clean_ha = diff_area_calc$diff_area_ha
diff_area_calc$diff_area_clean_ha[diff_area_calc$diff_area_ha<0 & diff_area_calc$start_area_ha==0] = 0
diff_area_calc$diff_area_clean_ha = diff_area_calc$diff_area_clean_ha + diff_area_calc$start_reg_own_diff_area_ha * diff_area_calc$start_area_ha / diff_area_calc$start_reg_own_area_ha
diff_area_clean_sum_ha = sum(diff_area_calc$diff_area_clean_ha)
# adjust for differences due to total area mismatch
# get the region-ownership change area sums
extra_change_agg = aggregate(diff_area_clean_ha ~ reg_name + own_name, diff_area_calc, FUN=sum)
names(extra_change_agg)[ncol(extra_change_agg)] <- "extra_change_area_ha"
diff_area_calc = merge(diff_area_calc, extra_change_agg, by=c("reg_name", "own_name"), all.x=TRUE)
diff_area_calc = diff_area_calc[order(diff_area_calc$lcat_code),]
diff_area_calc$extra_change_area_ha = replace(diff_area_calc$extra_change_area_ha, is.na(diff_area_calc$extra_change_area_ha), 0)
diff_area_calc$diff_area_adj_ha = diff_area_calc$diff_area_clean_ha - diff_area_calc$extra_change_area_ha * diff_area_calc$start_area_ha / diff_area_calc$start_reg_own_area_ha
diff_area_adj_sum_ha = sum(diff_area_calc$diff_area_adj_ha)
# annualize difference
diff_area_calc$ann_diff_area_ha = diff_area_calc$diff_area_adj_ha / diff_years
ann_diff_area_sum_ha = sum(diff_area_calc$ann_diff_area_ha)
# drop the zero area cats and add fresh marsh to the Delta region
# make sure that caland checks for land cat existence for each management
# fresh marsh comes out of cultivated only, so only add the initial cultivated ownerships
# fresh marsh has been assigned a land type code of 120
zero_area = diff_area_calc[diff_area_calc$start_area_ha == 0,]
diff_area_calc = diff_area_calc[diff_area_calc$start_area_ha > 0,]
delta_cult = diff_area_calc[diff_area_calc$lt_code == cult_code & diff_area_calc$reg_code == delta_code,]
delta_cult$lcat_code = delta_cult$reg_code * 100000 + fresh_marsh_code * 100 + delta_cult$own_code
delta_cult$lt_name = fresh_marsh_name
delta_cult$lt_code = fresh_marsh_code
delta_cult[,c(8:ncol(delta_cult))] = 0.00
diff_area_calc = rbind(diff_area_calc, delta_cult)
diff_area_calc = diff_area_calc[order(diff_area_calc$lcat_code),]

# total land area
land_area_sum_ha = sum(diff_area_calc$start_area_ha)

# add seagrass
seagrass_df = diff_area_calc[1,]
seagrass_df$reg_name = seagrass_reg_name
seagrass_df$lt_name = seagrass_lt_name
seagrass_df$own_name = seagrass_own_name
seagrass_df$reg_code = seagrass_reg_code
seagrass_df$lt_code = seagrass_lt_code
seagrass_df$own_code = seagrass_own_code
seagrass_df$lcat_code = seagrass_reg_code * 100000 + seagrass_lt_code * 100 + seagrass_own_code
seagrass_df$start_area_ha = seagrass_start_area_ha
seagrass_df[,c(9:ncol(seagrass_df))] = 0.00
diff_area_calc = rbind(diff_area_calc, seagrass_df)

##############################################################################################
###########################  assign area calcs to out_scen_df_list ###########################  
##############################################################################################
# scen area tables
# initial area
out_scen_df_list[[1]] = data.frame(Land_Cat_ID=diff_area_calc$lcat_code, Region=diff_area_calc$reg_name, Land_Type=diff_area_calc$lt_name, 
                                   Ownership=diff_area_calc$own_name, Area_ha=diff_area_calc$start_area_ha)

# annaul area change
out_scen_df_list[[2]] = data.frame(Land_Cat_ID=diff_area_calc$lcat_code, Region=diff_area_calc$reg_name, Land_Type=diff_area_calc$lt_name, 
                                   Ownership=diff_area_calc$own_name, Area_change_ha=diff_area_calc$ann_diff_area_ha)

##############################################################################################
# if using new landuse change methods replace diff_area_calc$ann_diff_area_ha with new values  
##############################################################################################
if (land_change_method == "Landuse_Avg_Annual") {
# read new area changes csv (units = m2)
new_area_changes <- read.csv(paste0(in_dir, area_gis_files_new), header=TRUE)
# add column for average annual area changes
new_area_changes$Area_change_ha <- rowMeans(new_area_changes[,2:ncol(new_area_changes)])
# convert all areas to ha
new_area_changes[,2:ncol(new_area_changes)]  <- new_area_changes[,2:ncol(new_area_changes)] / 10000
# replace "m2" from end of column names with "ha"
colnames(new_area_changes)[2:(ncol(new_area_changes)-1)] <- sub("_m2", "_ha", colnames(new_area_changes[,2:(ncol(new_area_changes)-1)]))
colnames(new_area_changes)[2:(ncol(new_area_changes)-1)] <- sub("Area_", "Area_change_", colnames(new_area_changes[,2:(ncol(new_area_changes)-1)]))
# replace area change with average of individual annual area changes
  # match avg area changes column with column in out_scen_df_list[[2]] by landcat
  out_scen_df_list[[2]][match(new_area_changes$Landcat,out_scen_df_list[[2]][,"Land_Cat_ID"]), "Area_change_ha"] <- new_area_changes$Area_change_ha
  # replace ice and water landtype area changes with 0
  out_scen_df_list[[2]][out_scen_df_list[[2]]["Land_Type"] =="Ice" | out_scen_df_list[[2]][,"Land_Type"] == "Water", "Area_change_ha"] <- 0.0
}
# checks if matching is done correctly. Should equal -0.4541131. Good!
out_scen_df_list[[2]][out_scen_df_list[[2]]["Land_Cat_ID"] == 107001, "Area_change_ha"]

###### scen wildfire area table
# the available land types are forest, woodland, savanna, shrubland, grassland; but these are determined annually in caland
# caland can take multiple year columns
# assume that the intensities are: High, Medium, Low
# do not use the trends for BAU!!! They are negative!!!

fire_area_in = read.csv(paste0(in_dir,fire_area_file), stringsAsFactors = FALSE)

# first set up the table
# use all region-ownerships and all three severities so that the table is consistent across years and complete
# remove ocean and aggregate to region-ownership
burn_avail_reg_own = aggregate(Area_ha ~ Region + Ownership, out_scen_df_list[[1]][out_scen_df_list[[1]]$Region != "Ocean",], FUN=sum)
names(burn_avail_reg_own)[ncol(burn_avail_reg_own)] <- "reg_own_area_ha"
burn_avail_reg_own$lcat_code = -1
burn_avail_reg_own$lt_name = "Unspecified"

high_burn = burn_avail_reg_own
high_burn$Severity = "High"
low_burn = burn_avail_reg_own
low_burn$Severity = "Low"
med_burn = burn_avail_reg_own
med_burn$Severity = "Medium"

burn_area_ann = rbind(high_burn, low_burn, med_burn)
burn_area_ann = merge(burn_area_ann, fire_area_in, by = c("Region", "Ownership"), all.x = TRUE)
burn_area_ann = burn_area_ann[order(burn_area_ann$lcat_code, burn_area_ann$Region, burn_area_ann$Ownership, burn_area_ann$Severity),]

# get the initial year burn area by averaging
burn_area_ann$initial_area_tot = 0.0
for (y in initial_fire_start:initial_fire_end) {
	# get the column index for this year
	col_ind = which(names(burn_area_ann) == paste0("X", y, "_ha"))
	burn_area_ann$initial_area_tot = burn_area_ann$initial_area_tot + burn_area_ann[,col_ind] / num_initial_fire_years
} # end for y loop to set initial burn area
burn_area_ann[is.na(burn_area_ann)] <- 0.0

# calc the temporal trend slopes
# fits[1,] are the intercepts by row
# fits[2,] are the slopes by row
#start_col = which(names(burn_area_ann) == paste0("X", initial_fire_start, "_ha"))
#end_col = which(names(burn_area_ann) == paste0("X", initial_fire_end, "_ha"))
#fits = apply(burn_area_ann[,start_col:end_col], 1, function(x) lm(x~c(initial_fire_start:initial_fire_end), na.action=na.exclude)$coefficients)

# set initial year burn severity proportions and column name
burn_area_ann$initial_area_sev = 0.0
burn_area_ann[burn_area_ann$Severity == "High", "initial_area_sev"] = high_fire_frac * burn_area_ann[burn_area_ann$Severity == "High", "initial_area_tot"]
burn_area_ann[burn_area_ann$Severity == "Low", "initial_area_sev"] = low_fire_frac * burn_area_ann[burn_area_ann$Severity == "Low", "initial_area_tot"]
burn_area_ann[burn_area_ann$Severity == "Medium", "initial_area_sev"] = med_fire_frac * burn_area_ann[burn_area_ann$Severity == "Medium", "initial_area_tot"]

# start building the output table
out_scen_df_list[[4]] = data.frame(Land_Cat_ID=burn_area_ann$lcat_code, Region=burn_area_ann$Region, Land_Type=burn_area_ann$lt_name, Ownership=burn_area_ann$Ownership, Severity=burn_area_ann$Severity, start_ha=burn_area_ann$initial_area_sev)
names(out_scen_df_list[[4]])[ncol(out_scen_df_list[[4]])] <- paste0(start_year,"_ha")

# loop through the remaining years
for (y in (start_year+1):(end_year-1)) {
	# get the column index for this year
	col_ind = which(names(burn_area_ann) == paste0("X", y, "_ha"))
	# adjust the fire severity proportions based on BAU; make sure they add to 1
	hff_cur = high_fire_frac + (hs_fire_trend * (y-start_year))
	lff_cur = low_fire_frac - low_fire_frac / (low_fire_frac + med_fire_frac) * (hs_fire_trend * (y-start_year))
	mff_cur = med_fire_frac - low_fire_frac / (low_fire_frac + med_fire_frac) * (hs_fire_trend * (y-start_year))
	a = (1 - hff_cur) / (lff_cur + mff_cur)
	lff_cur = a * lff_cur
	mff_cur = a * mff_cur
	
	# if projected climate, then assign the annual years
	# else if BAU/historical then apply initial year burn area
	if (CLIMATE != "HIST") {
		# projected: set burn severity proportions and column name
		burn_area_ann[burn_area_ann$Severity == "High", col_ind] = hff_cur * burn_area_ann[burn_area_ann$Severity == "High", col_ind]
		burn_area_ann[burn_area_ann$Severity == "Low", col_ind] = lff_cur * burn_area_ann[burn_area_ann$Severity == "Low", col_ind]
		burn_area_ann[burn_area_ann$Severity == "Medium", col_ind] = mff_cur * burn_area_ann[burn_area_ann$Severity == "Medium", col_ind]
	} else {
		# BAU/historical: first set the annual area, then burn severity
		burn_area_ann[,col_ind] = burn_area_ann$initial_area_tot
		burn_area_ann[burn_area_ann$Severity == "High", col_ind] = hff_cur * burn_area_ann[burn_area_ann$Severity == "High", col_ind]
		burn_area_ann[burn_area_ann$Severity == "Low", col_ind] = lff_cur * burn_area_ann[burn_area_ann$Severity == "Low", col_ind]
		burn_area_ann[burn_area_ann$Severity == "Medium", col_ind] = mff_cur * burn_area_ann[burn_area_ann$Severity == "Medium", col_ind]
	}
	
	# set negative values to zero
	burn_area_ann[,-c(1:5)][burn_area_ann[,-c(1:5)] < 0] == 0
	
	# store this year
	out_scen_df_list[[4]] = cbind(out_scen_df_list[[4]], burn_area_ann[,col_ind])
	names(out_scen_df_list[[4]])[ncol(out_scen_df_list[[4]])] <- paste0(y,"_ha")
} # end for y loop for remaining years


###### scen mortality table
# this is a multiple year table
# currently, the only land types with explicit mortality are:
# shrubland, savanna, woodland, forest, developed_all
# this is because other land types do not have above gound carbon accumulation (i.e. mortality is implicit)

# read in the mortality values
# all valid land types must be explicitly specified
mort_in = read.csv(paste0(in_dir,mortality_file), stringsAsFactors = FALSE)

# initial year
mortality_types = out_scen_df_list[[1]][out_scen_df_list[[1]]$Land_Type == "Forest" | out_scen_df_list[[1]]$Land_Type == "Woodland" | out_scen_df_list[[1]]$Land_Type == "Savanna" | out_scen_df_list[[1]]$Land_Type == "Shrubland" | out_scen_df_list[[1]]$Land_Type == "Developed_all",]
names(mortality_types)[ncol(mortality_types)] <- "start_frac"
year_col_start = ncol(mortality_types)
mortality_types$start_frac = mortality_default

# create table of the mortality values by location specified
mort_in_landcat = mort_in[mort_in$Region != "All" & mort_in$Ownership != "All",]
mort_in_allreg = mort_in[mort_in$Region == "All" & mort_in$Ownership != "All",]
mort_in_allown = mort_in[mort_in$Region != "All" & mort_in$Ownership == "All",]
mort_in_allregown = mort_in[mort_in$Region == "All" & mort_in$Ownership == "All",]

# land cat
if (nrow(mort_in_landcat) > 0) {
	mortality_types = merge(mortality_types, mort_in_landcat, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
	mortality_types$start_frac[!is.na(mortality_types$Mortality_frac)] = mortality_types$Mortality_frac[!is.na(mortality_types$Mortality_frac)]
	mortality_types$Mortality_frac = NULL
}

# all region
if (nrow(mort_in_allreg) > 0) {
	mortality_types = merge(mortality_types, mort_in_allreg[,c("Land_Type", "Ownership", "Mortality_frac")], by = c("Land_Type", "Ownership"), all.x = TRUE)
	mortality_types$start_frac[!is.na(mortality_types$Mortality_frac)] = mortality_types$Mortality_frac[!is.na(mortality_types$Mortality_frac)]
	mortality_types$Mortality_frac = NULL
}

# all ownership
if (nrow(mort_in_allown) > 0) {
	mortality_types = merge(mortality_types, mort_in_allown[,c("Region", "Land_Type", "Mortality_frac")], by = c("Region", "Land_Type"), all.x = TRUE)
	mortality_types$start_frac[!is.na(mortality_types$Mortality_frac)] = mortality_types$Mortality_frac[!is.na(mortality_types$Mortality_frac)]
	mortality_types$Mortality_frac = NULL
}

# all reigon and all ownership
if (nrow(mort_in_allregown) > 0) {
	mortality_types = merge(mortality_types, mort_in_allregown[,c("Land_Type", "Mortality_frac")], by = c("Land_Type"), all.x = TRUE)
	mortality_types$start_frac[!is.na(mortality_types$Mortality_frac)] = mortality_types$Mortality_frac[!is.na(mortality_types$Mortality_frac)]
	mortality_types$Mortality_frac = NULL
}

mortality_types = mortality_types[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "start_frac")]

if(forest_mort_fact != 1) {
	# increased forest mortality
	# need to add the years surround the jumps so that each period has a flat mortality rate
	mortality_types$year2_frac = mortality_types$start_frac
	mortality_types$year3_frac = mortality_types$start_frac
	mortality_types$year3_frac[mortality_types$Land_Type == "Forest"] = forest_mort_fact * mortality_types$year3_frac[mortality_types$Land_Type == "Forest"]
	mortality_types$year3_frac[mortality_types$Land_Type == "Forest" & mortality_types$year3_frac >= 1] = 1
	mortality_types$year4_frac = mortality_types$year3_frac	
	mortality_types$year5_frac = mortality_types$start_frac
	mortality_types = mortality_types[order(mortality_types$Land_Cat_ID),]
	names(mortality_types)[year_col_start+1] <- paste0(forest_mort_adj_first-1,"_frac")
	names(mortality_types)[year_col_start+2] <- paste0(forest_mort_adj_first,"_frac")
	names(mortality_types)[year_col_start+3] <- paste0(forest_mort_adj_last,"_frac")
	names(mortality_types)[year_col_start+4] <- paste0(forest_mort_adj_last+1,"_frac")
}

names(mortality_types)[year_col_start] <- paste0(start_year,"_frac")
out_scen_df_list[[5]] = mortality_types

###### scen climate scalars tables
# these values are the direct scalars
# vegetation first, then soil

climate_c_in = read.csv(paste0(in_dir,climate_c_file), stringsAsFactors = FALSE)
# if landtype is "All" then it's the unitary file where all scalars are 1
if(climate_c_in$Land_Type[1] == "All") {
	UNITARY = TRUE
	climate_c_in$Land_Type = NULL
} else {
  UNITARY = FALSE
}
clim_start_col = which(names(climate_c_in) == paste0("X", start_year))
clim_end_col = ncol(climate_c_in)

# veg
# start building the output table
out_scen_df_list[[6]] = out_scen_df_list[[1]][,c(1:4)]

climate_c_veg = out_scen_df_list[[1]][,c(1:4)]
if(UNITARY) {
	climate_c_veg = merge(climate_c_veg, climate_c_in[climate_c_in$Component == "Vegetation", c(1:2,clim_start_col:clim_end_col)], by = c("Region", "Ownership"), all.x = TRUE)
	climate_c_veg = climate_c_veg[order(climate_c_veg$Land_Cat_ID),]
} else {
	climate_c_veg = merge(climate_c_veg, climate_c_in[climate_c_in$Component == "Vegetation", c(1:3,clim_start_col:clim_end_col)], by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
	climate_c_veg = climate_c_veg[order(climate_c_veg$Land_Cat_ID),]
}
climate_c_veg$Component = NULL
climate_c_veg[is.na(climate_c_veg)] = 1
if(CLIMATE == "HIST") { climate_c_veg[,-c(1:4)] = 1 }
# add the year data to the table
out_scen_df_list[[6]] = cbind(out_scen_df_list[[6]], climate_c_veg[,c(5:ncol(climate_c_veg))])
# set the column names
for (i in c(5:ncol(out_scen_df_list[[6]]))) {
	names(out_scen_df_list[[6]])[i] <- paste0(start_year+i-5)
}

# soil
# start building the output table
out_scen_df_list[[7]] = out_scen_df_list[[1]][,c(1:4)]

climate_c_soil = out_scen_df_list[[1]][,c(1:4)]
if(UNITARY) {
	climate_c_soil = merge(climate_c_soil, climate_c_in[climate_c_in$Component == "Soil", c(1:2,clim_start_col:clim_end_col)], by = c("Region", "Ownership"), all.x = TRUE)
	climate_c_soil = climate_c_soil[order(climate_c_soil$Land_Cat_ID),]
} else {
	climate_c_soil = merge(climate_c_soil, climate_c_in[climate_c_in$Component == "Soil", c(1:3,clim_start_col:clim_end_col)], by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
	climate_c_soil = climate_c_soil[order(climate_c_soil$Land_Cat_ID),]
}
climate_c_soil$Component = NULL
climate_c_soil[is.na(climate_c_soil)] = 1
if(CLIMATE == "HIST") { climate_c_soil[,-c(1:4)] = 1 }
# add the year data to the table
out_scen_df_list[[7]] = cbind(out_scen_df_list[[7]], climate_c_soil[,c(5:ncol(climate_c_soil))])
# set the column names
for (i in c(5:ncol(out_scen_df_list[[7]]))) {
	names(out_scen_df_list[[7]])[i] <- paste0(start_year+i-5)
}

###### read the scenario definition file

scenin_wrkbk = loadWorkbook(paste0(in_dir,scenarios_file))
# worksheet/table names
scenin_sheets = getSheets(scenin_wrkbk)
num_scenin_sheets = length(scenin_sheets)
# NA values need to be converted to numeric
# the warnings thrown by readWorksheet below are ok because they just state that the NA string can't be converted a number so it is 
# converted to NA value
scenin_df_list <- list()
for (i in 1:num_scenin_sheets) {
  scenin_df_list[[i]] <- readWorksheet(scenin_wrkbk, i, startRow = start_row, colTypes = c(rep("character",4), rep("numeric",50)), forceConversion = TRUE)
  # convert management acres to hectares as needed
  if (units_scenario=="ac") {
    scenin_df_list[[i]][,c("start_area", "end_area")] = scenin_df_list[[i]][,c("start_area", "end_area")] * ac2ha
  }
} # end for i loop to read in scenarios

###### read the scenario headers file for the outputs
scenhead_wrkbk = loadWorkbook(scen_head_file)
# worksheet/table names
scenhead_sheets = getSheets(scenhead_wrkbk)
num_scenhead_sheets = length(scenhead_sheets)
scen_head_df_list <- list()
for (i in 1:num_scenhead_sheets) {
	scen_head_df_list[[i]] <- readWorksheet(scenhead_wrkbk, i, startRow = 1, header = FALSE)
}

###### read the parameter file

param_wrkbk = loadWorkbook(paste0(in_dir,parameter_file))
# worksheet/table names
param_sheets = getSheets(param_wrkbk)
num_param_sheets = length(param_sheets)
# NA values need to be converted to numeric
# the warnings thrown by readWorksheet below are ok because they just state that the NA string can't be converted a number so it is 
# converted to NA value
c_col_types1 = c("character", rep("numeric",50))
c_col_types2 = c("character", "character", rep("numeric",50))
c_col_types3 = c("character", "character", "character", rep("numeric",50))
c_col_types4 = c("character", "character", "character", "character", rep("numeric",50))

# Load the param worksheets into a list of data frames
param_df_list <- list()
param_head_list <- list()
for (i in 1:2) { # vegc_uptake and soilc_accum
	param_head_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = 1, endRow = last_head_row, header=FALSE)
	param_df_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = start_row, colTypes = c_col_types3, forceConversion = TRUE)
}
for (i in 3:3) { # conversion2ag_urban
	param_head_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = 1, endRow = last_head_row, header=FALSE)
	param_df_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = start_row, colTypes = c_col_types1, forceConversion = TRUE)
}
# forest_manage
i = 4
param_head_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = 1, endRow = last_head_row, header=FALSE)
param_df_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = start_row, colTypes = c_col_types4, forceConversion = TRUE)

for (i in 5:6) { # dev_manage to grass_manage 
	param_head_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = 1, endRow = last_head_row, header=FALSE)
	param_df_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = start_row, colTypes = c_col_types2, forceConversion = TRUE)
}

# ag_manage
i = 7
param_head_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = 1, endRow = last_head_row, header=FALSE)
param_df_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = start_row, colTypes = c_col_types3, forceConversion = TRUE)

# wildfire
i=8
param_head_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = 1, endRow = last_head_row, header=FALSE)
param_df_list[[i]] <- readWorksheet(param_wrkbk, i, startRow = start_row, colTypes = c_col_types2, forceConversion = TRUE)

###### read the parameter headers file for the outputs
paramhead_wrkbk = loadWorkbook(param_head_file)
# worksheet/table names
paramhead_sheets = getSheets(paramhead_wrkbk)
num_paramhead_sheets = length(paramhead_sheets)
param_head_df_list <- list()
for (i in 1:num_paramhead_sheets) {
	param_head_df_list[[i]] <- readWorksheet(paramhead_wrkbk, i, startRow = 1, header = FALSE)
}

if (control_wildfire_lulcc) {
	# read in the instruction file for input scenario
	control_wildfire_lulcc_df <-read.csv(paste0(in_dir,control_wildfire_lulcc_file), header = TRUE)
}

###### loop over the scenario definitions
# make a complete scenario file for each one

# save the LULCC and wildfire dataframesin case they are modfied in each loop and need to be reset
  # for creating the individual sensitivity/scenario tests (control_wildfire_lulcc==TRUE)
orig_LULCC<- out_scen_df_list[[2]]
orig_fire<- out_scen_df_list[[4]]

for (s in 1:num_scenin_sheets) {
  ###### scenario managed area table
	
	scenin = scenin_df_list[[s]]
  	scenin_name = scenin_sheets[s]
  
  # if turning off wildfire and/or lulcc, 
  # use the scenario name to find the selection of out_scen_df_list sheets to modify (i.e, LULCC to 0 and/or Wildfire to 0)
  
  if (control_wildfire_lulcc) {
    
    # reset LULCC and wildfire with original datasets
    out_scen_df_list[[2]] <- orig_LULCC
    out_scen_df_list[[4]] <- orig_fire
    
    # find the correct row for this scenario
    nomatch = TRUE
    for (i in 1:nrow(control_wildfire_lulcc_df)) {
    	if (scenin_name == control_wildfire_lulcc_df$Scenario[i]) {
    		nomatch = FALSE
    		# read the row for current sheet
    		LULCC_switch <- control_wildfire_lulcc_df[i,3]
    		wildfire_switch <- control_wildfire_lulcc_df[i,2]
    
    		# update LULCC
    		out_scen_df_list[[2]][5] <- out_scen_df_list[[2]][5] * LULCC_switch
    
    		# update wildfire
    		out_scen_df_list[[4]][6:ncol(out_scen_df_list[[4]])] <- out_scen_df_list[[4]][6:ncol(out_scen_df_list[[4]])] * wildfire_switch
    		break
    	} 
    } # end for i loop over control file rows
    
    if (nomatch == TRUE) {
    	cat("\nError: No lulcc/wildfire control record for scenario", scenin_name, "in file ", control_wildfire_lulcc_file, "\n")
    	stop()
    }
    
  } # end if control_wildfire_lulcc
	
	# check the scenario management against the parameter management (if it exists)
	# Restoration, Urban_forest, and Growth do not have any parameters associated with them
	if (nrow(scenin) != 0) {
	for (m in 1:length(scenin$Management)) {
	  if (scenin$Management[m] != "Restoration" & scenin$Management[m] != "Urban_forest" & scenin$Management[m] != "Growth") {
			EXIST = FALSE
			for (t in 4:7) {
				for (r in 1:length(param_df_list[[t]]$Management)) {
					if (scenin$Management[m] == param_df_list[[t]]$Management[r]) {
						EXIST = TRUE
						break
						}
				}
				if (EXIST) {break}
			}
			if (!EXIST) {
				cat("\n STOP! Scenario management", scenin$Management[m], "does not exist in the parameter definitions\n")
				stop()
			}
		} # end if not Restoration
	} # end for m over management practices

	# split the records based on full land category definition or "All" for Region or Ownership
	# then merge them all together with the full land category set and drop the records with no management
	
	# assign all region- and own-specific practices (i.e. restoration) to complete_recs
	complete_recs = scenin[scenin$Region != "All" & scenin$Ownership != "All",]
	# assign all ownership-specific practices to allregion_recs (e.g. central valley, cultivated, soil conservation)
	allregion_recs = scenin[scenin$Region == "All" & scenin$Ownership != "All",]
	# change "Region" column in allregion_recs to "allregion"
	names(allregion_recs)[grep("^Region$", colnames(allregion_recs))] = "allregion"
	# assign all region-specific practices to allown_recs (e.g. private prescribed burning)
	allown_recs = scenin[scenin$Ownership == "All" & scenin$Region != "All",]
	# change "Ownership" column in allown_recs to "allown"
	names(allown_recs)[grep("^Ownership$", colnames(allown_recs))] = "allown"
	# assign all practices that are only land-type specific to allregionown_recs (e.g. developed_all urban forest)
	allregionown_recs = scenin[scenin$Ownership == "All" & scenin$Region == "All",]
	# change "Region" & "Ownership" columns in allregionown_recs to "allregion" & "allown"
	names(allregionown_recs)[grep("^Region$", colnames(allregionown_recs))] = "allregion"
	names(allregionown_recs)[grep("^Ownership$", colnames(allregionown_recs))] = "allown"
	
	# merge these groups accordingly with the start area table, plus the area change values
	# get the first several identifying columns and the initial area column
	area_change = out_scen_df_list[[1]]
	# add area change column
	area_change$Area_change_ha = out_scen_df_list[[2]]$Area_change_ha

	# assign area_change merged with landcategory-specific management areas (i.e. restoration) to manage1 
	manage1 = merge(area_change, complete_recs, by = c("Region", "Land_Type", "Ownership"), all.y = TRUE)
	# assign area_change merged with all own-specific management areas to manage2
	manage2 = merge(area_change, allregion_recs, by = c("Land_Type", "Ownership"), all.y = TRUE)
	manage2$allregion = NULL
	# assign area_change merged with all region-specific management areas to manage3
	manage3 = merge(area_change, allown_recs, by = c("Region", "Land_Type"), all.y = TRUE)
	manage3$allown = NULL
	# assign area_change merged with only land-type specific management areas to manage4
	manage4 = merge(area_change, allregionown_recs, by = c("Land_Type"), all.y = TRUE)
	manage4$allregion = NULL
	manage4$allown = NULL
	
	# now calculate the appropriate normalizing area for each group so that the management area can be distributed
	# normalizing area here is the respective total land area within the unit that needs to be disaagregated
	col_order = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", "Area_ha", "Area_change_ha", 
	              "start_year", "end_year", "start_area", "end_area", "start_area_frac", "end_area_frac", "Area_norm_ha")
	
	# need to first merge identical records with different start/end areas
	#	this only applies to the area practices because they may overlap
	# then sum land type area over the input spatial category
	
	if (nrow(manage1) > 0) {
		area_rows = which(!is.na(manage1$start_area))
		if (length(area_rows) > 0) {
			dup_agg = aggregate(cbind(manage1$start_area[area_rows], manage1$end_area[area_rows]) ~ Land_Cat_ID + Region + Land_Type + Ownership + Management + start_year + end_year, manage1[area_rows,], FUN=sum)
			dup_agg = merge(manage1[area_rows,], dup_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", "start_year", "end_year"), all.y = TRUE)
			dup_agg$start_area = dup_agg$V1
			dup_agg$end_area = dup_agg$V2
			dup_agg$V1 = NULL
			dup_agg$V2 = NULL
			dup_agg = unique(dup_agg)
			if(nrow(manage1) > length(area_rows)) {
				manage1 = rbind(manage1[-area_rows,], dup_agg)
			} else {
				manage1 = dup_agg
			}
		}
	  # sum land type area to land cat for each practice entry; this should not do any aggregating at all
		manage1_agg = aggregate(Area_ha ~ Land_Cat_ID + Region + Land_Type + Ownership + Management + start_year + end_year, manage1, FUN=sum)
		names(manage1_agg)[ncol(manage1_agg)] = "Area_norm_ha"
		# merge with non-aggregated areas
		manage1 = merge(manage1, manage1_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", "start_year", "end_year"), all.x = TRUE)
		manage1 = manage1[,col_order]
	} 
	if (nrow(manage2) > 0) {
		area_rows = which(!is.na(manage2$start_area))
		if (length(area_rows) > 0) {
			dup_agg = aggregate(cbind(manage2$start_area[area_rows], manage2$end_area[area_rows]) ~ Land_Cat_ID + Region + Land_Type + Ownership + Management + start_year + end_year, manage2[area_rows,], FUN=sum)
			dup_agg = merge(manage2[area_rows,], dup_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", "start_year", "end_year"), all.y = TRUE)
			dup_agg$start_area = dup_agg$V1
			dup_agg$end_area = dup_agg$V2
			dup_agg$V1 = NULL
			dup_agg$V2 = NULL
			dup_agg = unique(dup_agg)
			if(nrow(manage2) > length(area_rows)) {
				manage2 = rbind(manage2[-area_rows,], dup_agg)
			} else {
				manage2 = dup_agg
			}
		}
	  # sum land type area to all region for each practice entry
		manage2_agg = aggregate(Area_ha ~ Land_Type + Ownership + Management + start_year + end_year, manage2, FUN=sum)
		names(manage2_agg)[ncol(manage2_agg)] = "Area_norm_ha"
		manage2 = merge(manage2, manage2_agg, by = c("Land_Type", "Ownership", "Management", "start_year", "end_year"), all.x = TRUE)
		manage2 = manage2[,col_order]
	}
	if (nrow(manage3) > 0) {
		area_rows = which(!is.na(manage3$start_area))
		if (length(area_rows) > 0) {
			dup_agg = aggregate(cbind(manage3$start_area[area_rows], manage3$end_area[area_rows]) ~ Land_Cat_ID + Region + Land_Type + Ownership + Management + start_year + end_year, manage3[area_rows,], FUN=sum)
			dup_agg = merge(manage3[area_rows,], dup_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", "start_year", "end_year"), all.y = TRUE)
			dup_agg$start_area = dup_agg$V1
			dup_agg$end_area = dup_agg$V2
			dup_agg$V1 = NULL
			dup_agg$V2 = NULL
			dup_agg = unique(dup_agg)
			if(nrow(manage3) > length(area_rows)) {
				manage3 = rbind(manage3[-area_rows,], dup_agg)
			} else {
				manage3 = dup_agg
			}
		}
	  # sum land type area to all ownership for each practice entry
		manage3_agg = aggregate(Area_ha ~ Region + Land_Type + Management + start_year + end_year, manage3, FUN=sum)
		names(manage3_agg)[ncol(manage3_agg)] = "Area_norm_ha"
		manage3 = merge(manage3, manage3_agg, by = c("Region", "Land_Type", "Management", "start_year", "end_year"), all.x = TRUE)
		manage3 = manage3[,col_order]
	}
	if (nrow(manage4) > 0) {
		area_rows = which(!is.na(manage4$start_area))
		if (length(area_rows) > 0) {
			dup_agg = aggregate(cbind(manage4$start_area[area_rows], manage4$end_area[area_rows]) ~ Land_Cat_ID + Region + Land_Type + Ownership + Management + start_year + end_year, manage4[area_rows,], FUN=sum)
			dup_agg = merge(manage4[area_rows,], dup_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", "start_year", "end_year"), all.y = TRUE)
			dup_agg$start_area = dup_agg$V1
			dup_agg$end_area = dup_agg$V2
			dup_agg$V1 = NULL
			dup_agg$V2 = NULL
			dup_agg = unique(dup_agg)
			if(nrow(manage4) > length(area_rows)) {
				manage4 = rbind(manage4[-area_rows,], dup_agg)
			} else {
				manage4 = dup_agg
			}
		}
	  # sum land type area to all region-ownership for each practice entry
		manage4_agg = aggregate(Area_ha ~ Land_Type + Management + start_year + end_year, manage4, FUN=sum)
		names(manage4_agg)[ncol(manage4_agg)] = "Area_norm_ha"
		manage4 = merge(manage4, manage4_agg, by = c("Land_Type", "Management", "start_year", "end_year"), all.x = TRUE)
		manage4 = manage4[,col_order]
	}
	
	# bind the groups together into one table
	manage = rbind(manage1, manage2, manage3, manage4)
	manage = manage[order(manage$Land_Cat_ID, manage$Management),]
	manage = manage[!is.na(manage$Management),] # this should not be necessary
	manage[,paste0(start_year,"_ha")] = 0.0

	# get the necessary years for columns (note that the start_year column already exists)
	# years immediately before start years and immediately after end years need to be added (unless they are beyond the year range)
	
	# first years: includes the prior year (within start_year to scen_end_year)
	man_first = sort(unique(manage$start_year))
	man_last = sort(unique(manage$end_year))
	add_years = NULL
	for (y in 1:length(man_first)) {
		if (man_first[y] > start_year) {add_years = c(add_years, man_first[y] - 1)}
	}
	man_first = c(man_first, add_years, start_year)
	man_first = sort(unique(man_first))
	man_first_labels = paste0(man_first, "_ha")
	num_man_first = length(man_first)
	
	# last years: includes the next year (within start_year to scen_end_year)
	add_years = NULL
	for (y in 1:length(man_last)) {
		if (man_last[y] < scen_end_year) {add_years = c(add_years, man_last[y] + 1)}
	}
	man_last = c(man_last, add_years, scen_end_year)
	man_last = sort(unique(man_last))
	man_last_labels = paste0(man_last, "_ha")
	num_man_last = length(man_last)
	
	# all years
	man_years = c(man_first, man_last)
	man_years = sort(unique(man_years))
	man_years_labels = paste0(man_years, "_ha")
	num_man_years = length(man_years)
	
	# split the manage table based on cumulative area, annual managed area, or area fraction input
	  # select annual type
	manage_annareain = manage[!is.na(manage$start_area) & manage$Management != "Restoration" & manage$Management != "Reforestation" & manage$Management != "Afforestation",]
	  # select cumulative type
	manage_cumareain = manage[!is.na(manage$start_area) & (manage$Management == "Restoration" | manage$Management == "Reforestation" | manage$Management == "Afforestation"),]
	  # select rows which have area fraction input
	manage_fracin = manage[!is.na(manage$start_area_frac),]
	
	####### loop over the manage years to create and fill the columns
	for (y in 1:num_man_years) {
		year = man_years[y]
		year_lab = man_years_labels[y]
		
		# each row has a distinct set of years for a given activity
		# merge the appropriate rows together at the end to get the trajectory for a single activity in one row
		
		## process (all?) years before first years
		
		# annual area input; the values set are annual area
		if (nrow(manage_annareain)>0) {
		manage_annareain[manage_annareain$start_year > year,year_lab] = 0.0
		}
		# cumulative area input; the values set are annual area
		if (nrow(manage_cumareain)>0) {
		  manage_cumareain[manage_cumareain$start_year > year,year_lab] = 0.0
		}
		if (nrow(manage_fracin)>0) {
		# fraction input; the values set are annual area
		manage_fracin[manage_fracin$start_year > year,year_lab] = 0.0
		}
		
		## process first year to last year
		
		# annual area input; the values set are annual area; need to distribute the prescribed area
		# prescribed area is distributed relative to the sum aggregated landtype areas according to whether they are region- and/or ownership- specific
		# if normalizing area is zero, then no existing category, so set norm area to 1 so that end value is zero
		area_norm_recs = manage_annareain$Area_norm_ha[manage_annareain$start_year <= year & manage_annareain$end_year >= year]
		zinds = which(area_norm_recs == 0)
		area_norm_recs[zinds] = 1.0
		# annual area input for current year = (start_area + (year - start_year)*(end_area - start_area)/(end_year-start_year)) * 
		#                                           area_ha/area_norm_recs
		# annual area input for current year = linear interpolation of mgmt area 
		manage_annareain[manage_annareain$start_year <= year & manage_annareain$end_year >= year,year_lab] = 
		  (manage_annareain$start_area[manage_annareain$start_year <= year & manage_annareain$end_year >= year] + 
		     (year - manage_annareain$start_year[manage_annareain$start_year <= year & manage_annareain$end_year >= year]) * 
		     (manage_annareain$end_area[manage_annareain$start_year <= year & manage_annareain$end_year >= year] - 
		        manage_annareain$start_area[manage_annareain$start_year <= year & manage_annareain$end_year >= year]) / 
		     (manage_annareain$end_year[manage_annareain$start_year <= year & manage_annareain$end_year >= year] - 
		        manage_annareain$start_year[manage_annareain$start_year <= year & manage_annareain$end_year >= year])) * 
		  manage_annareain$Area_ha[manage_annareain$start_year <= year & manage_annareain$end_year >= year] / area_norm_recs
		
		# cumulative area input; the values set are annual area; need to distribute the prescribed area
		# prescribed area is distributed relative to the sum aggregated land type areas according to whether they are region- and/or ownership- specific
		# if normalizing area is zero, then no existing category, so set norm area to 1 so that end value is zero
		#	unless the zeros are for fresh marsh, then assume that the management values are specified for each land 
		# category already
		area_norm_recs = manage_cumareain$Area_norm_ha[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year]
		area_initial_recs = manage_cumareain$Area_ha[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year]
		zinds = which(area_norm_recs == 0)
		fresh_marsh_inds = which(manage_cumareain$Land_Type[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year] == "Fresh_marsh")
		fmzinds = intersect(zinds, fresh_marsh_inds)
		area_norm_recs[zinds] = 1.0
		area_initial_recs[fmzinds] = 1.0
		manage_cumareain[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year,year_lab] = 
		  ((manage_cumareain$end_area[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year] - 
		      manage_cumareain$start_area[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year]) / 
		     (manage_cumareain$end_year[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year] - 
		        manage_cumareain$start_year[manage_cumareain$start_year <= year & manage_cumareain$end_year >= year] + 1)) * 
		  area_initial_recs / area_norm_recs
		
		# fraction input; the values set are annual area; these fractions are directly applied to the correct area
		# Dead_removal and Urban_forest input values are fractions of Developed_area in year
		# Growth input values are fractions of the initial growth rate
		# The values set for Dead_removal and Urban_Forest can be negative if Developed_all runs out of area
		#	this is ok because these values are used to calculate intermediate-year values, which may not be negative
		#	these negative values are dealt with in CALAND

		# growth
		# for rows with management start_year <= current management year (e.g. 2010, 2020, 2021, 2050) in loop 
		# (that's all rows because start_year == 2010), assign the following calculation to the current management area
		# column in loop (e.g. "2010_ha" "2020_ha" "2021_ha" "2050_ha")
		# calc based on land_change_method

		manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & manage_fracin$Management == 
		                "Growth",year_lab] = 
		  # for rows with management == "Growth":
		  # [start_area_frac + (management year - start year) * end_area_frac - (start_area_frac/(end year - start_year)] *
		    # Area_Change_ha ==
		  # [start_area_frac + (management year - start year) * end_area_frac - (start_area_frac/(end year - start_year)] *
		  # Area_Change_ha
		  # calcs a linear change in growth rate between the years (man_fracin = actual frac). policy prescription is based on the 
		  # initial rate of area change. 2010-2016 = 1, 2017-2050 is some fraction. start and end.
		  (manage_fracin$start_area_frac[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                                                      manage_fracin$Management == "Growth"] + 
		                                        (year - manage_fracin$start_year[manage_fracin$start_year <= year & 
		                                                                           manage_fracin$end_year >= year & 
		                                                                           manage_fracin$Management == "Growth"]) * 
		                                        (manage_fracin$end_area_frac[manage_fracin$start_year <= year & 
		                                                                       manage_fracin$end_year >= year & 
		                                                                       manage_fracin$Management == "Growth"] - 
		                                           manage_fracin$start_area_frac[manage_fracin$start_year <= year & 
		                                                                           manage_fracin$end_year >= year & 
		                                                                           manage_fracin$Management == "Growth"]) / 
		                                        (manage_fracin$end_year[manage_fracin$start_year <= year & 
		                                                                  manage_fracin$end_year >= year & 
		                                                                  manage_fracin$Management == "Growth"] - 
		                                           manage_fracin$start_year[manage_fracin$start_year <= year & 
		                                                                      manage_fracin$end_year >= year & 
		                                                                      manage_fracin$Management == "Growth"])) * 
		  manage_fracin$Area_change_ha[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                 manage_fracin$Management == "Growth"]

	 
		# dead_removal and urban forest
		# first calculate the developed_all area for this year
		manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest"),year_lab] = 
		  manage_fracin$Area_ha[manage_fracin$start_year <= year & manage_fracin$end_year >= year & (manage_fracin$Management == "Dead_removal" | 
		                                                                                               manage_fracin$Management == "Urban_forest")]
		if (year > start_year) {
			for (p in 2:y) {
			  # assign current management year to pyear
				pyear = man_years[p]
				# assign corresponding label, e.g. "2020_ha", to man_years_labels
				pyear_lab = man_years_labels[p]
				# assign the previous management year to prev_pyear
				prev_pyear = man_years[p-1]
				# assign previous management year's corresponding label to prev_pyear_lab
				prev_pyear_lab = man_years_labels[p-1]

				### get the appropriate growth area values and merge them with the dead and urban data
				
				# assign all Growth management records that have _expanding_ Urban area to growth_temp
				growth_temp = manage_fracin[manage_fracin$Management == "Growth" & manage_fracin$Area_change_ha >= 0,]
				# assign the previous year's Growth management area to growth_temp$growth_val
				growth_temp$growth_val = growth_temp[,prev_pyear_lab]
				# check if any expanding Urban areas exist before aggregating them
				if (nrow(growth_temp) != 0) {
				# extract the previous year's _max_ Growth management area for each landcat, region, landtype, ownership combination
				growth_pos_agg = aggregate(growth_val ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, growth_temp, FUN=max)
				}
				
				# assign all Growth management records that have _contracting_ Urban area to growth_temp
				growth_temp = manage_fracin[manage_fracin$Management == "Growth" & manage_fracin$Area_change_ha < 0,]
				# assign the previous year's Growth management area to growth_temp$growth_val
				growth_temp$growth_val = growth_temp[,prev_pyear_lab]
				# check if any contracting Urban areas exist before aggregating them
				if (nrow(growth_temp) != 0) {
				  # extract the previous year's _min_ Growth management area for each landcat, region, landtype, ownership combination
				  growth_neg_agg = aggregate(growth_val ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, growth_temp, FUN=min)
				}
			  
				# assign the aggregated Growth management areas accordingly to growth_temp
				if (exists("growth_pos_agg") & exists("growth_neg_agg")) {
				  growth_temp = rbind(growth_pos_agg, growth_neg_agg)
				} else {
				  if (exists("growth_pos_agg")) {
				    growth_temp = growth_pos_agg
				  } else {
				    growth_temp = growth_neg_agg
				  }
				}
				
				# assign all previous records with Dead_removal & Urban_forest management areas from manage_fracin and growth_temp to manage_fracin_temp
				manage_fracin_temp = merge(manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
				                                           (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest"),], 
				                           growth_temp, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
				# calc area_delta = previous year's Growth management area * # years
				area_delta = manage_fracin_temp$growth_val * (pyear - prev_pyear)
				# add the area_delta to previous Dead_removal & Urban_forest management areas and assign to the current year management area
				manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
				                (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest"),year_lab] = 
				  manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
				                  (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest"),year_lab] + area_delta
			} # end for p loop over previous year columns
		} # end if not start year
		# now multiply the area by the fraction
		manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest"),year_lab] = 
		  (manage_fracin$start_area_frac[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                   (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest")] + 
		     (year - manage_fracin$start_year[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                        (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest")]) * 
		     (manage_fracin$end_area_frac[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                    (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest")] - 
		        manage_fracin$start_area_frac[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                        (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest")]) / 
		     (manage_fracin$end_year[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                               (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest")] - 
		        manage_fracin$start_year[manage_fracin$start_year <= year & manage_fracin$end_year >= year & 
		                                   (manage_fracin$Management == "Dead_removal" | manage_fracin$Management == "Urban_forest")])) * 
		  manage_fracin[manage_fracin$start_year <= year & manage_fracin$end_year >= year & (manage_fracin$Management == "Dead_removal" | 
		                                                                                       manage_fracin$Management == "Urban_forest"),year_lab]
	
		## process years after last years
		
		# annual area input; the values set are annual area
		if (nrow(manage_annareain)>0) {
		  manage_annareain[manage_annareain$end_year < year,year_lab] = 0.0
		}
		# cumulative area input; the values set are annual area
		if (nrow(manage_cumareain)>0) {
		manage_cumareain[manage_cumareain$end_year < year,year_lab] = 0.0
		}
		if (nrow(manage_fracin)>0) {
		# fraction input; the values set are annual area
		manage_fracin[manage_fracin$end_year < year,year_lab] = 0.0
		}
	
	} # end y loop over the management year columns
	
	### merge the rows containing the same practice but for different start and end years
	
	# for ann and cum area tyeps, non-assigned fields are zero, so these can just be summed
	if (nrow(manage_annareain)!=0) {
		manage_annareain$Area_ha = NULL
		manage_annareain$Area_change_ha = NULL	
		manage_annareain$start_year = NULL
		manage_annareain$end_year = NULL
		manage_annareain$start_area = NULL
		manage_annareain$end_area = NULL
		manage_annareain$start_area_frac = NULL
		manage_annareain$end_area_frac = NULL
		manage_annareain$Area_norm_ha = NULL
		manage_annareain = aggregate(. ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, manage_annareain, FUN=sum)
	} else { manage_annareain = NULL }
	
	if (nrow(manage_cumareain)!=0) {
		manage_cumareain$Area_ha = NULL
		manage_cumareain$Area_change_ha = NULL	
		manage_cumareain$start_year = NULL
		manage_cumareain$end_year = NULL
		manage_cumareain$start_area = NULL
		manage_cumareain$end_area = NULL
		manage_cumareain$start_area_frac = NULL
		manage_cumareain$end_area_frac = NULL
		manage_cumareain$Area_norm_ha = NULL
		manage_cumareain = aggregate(. ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, manage_cumareain, FUN=sum)
	} else { manage_cumareain = NULL }
	
	# for fractional types the only overlap should be during a transition year, which should have equal values
	#	so get only only values and use max for positive and min for negative growth
	#	and use the same rule (with respect growth) for dead removal and urban forest so that they are all consistent
	
	if (nrow(manage_fracin)!=0) {
		manage_fracin$Area_ha = NULL
		manage_fracin$start_year = NULL
		manage_fracin$end_year = NULL
		manage_fracin$start_area = NULL
		manage_fracin$end_area = NULL
		manage_fracin$start_area_frac = NULL
		manage_fracin$end_area_frac = NULL
		manage_fracin$Area_norm_ha = NULL
		
		# need to do this separately for negative growth
	  	# assign values for _contracting_ Urban area land cats
	  	land_cats = unique(manage_fracin$Land_Cat_ID[manage_fracin$Management == "Growth" & manage_fracin$Area_change_ha < 0])	  	
		neg_temp = manage_fracin[manage_fracin$Land_Cat_ID %in% land_cats,]
	  	# set the area change to NULL
		neg_temp$Area_change_ha = NULL
		if (nrow(neg_temp)!=0) {
	  		# aggregate the _minimum_ values for the fractional management areas by landcat, region, landtype, ownership, and management
	  		neg_agg = aggregate(. ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, neg_temp, FUN=min)
		} else { neg_agg = NULL }
		
		# For positive growth
		# assign values for _expanding_ Urban area land cats
		land_cats = unique(manage_fracin$Land_Cat_ID[manage_fracin$Management == "Growth" & manage_fracin$Area_change_ha >= 0])
		pos_temp = manage_fracin[manage_fracin$Land_Cat_ID %in% land_cats,]
	 	# set the area change to NULL
		pos_temp$Area_change_ha = NULL
		if (nrow(pos_temp)!=0) {
			#aggregate the _maximum_ values for the other management areas by landcat, region, landtype, ownership, and management
			pos_agg = aggregate(. ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, pos_temp, FUN=max)
		} else { pos_agg = NULL }
	} # end combine fraction rows
	
	manage_out = rbind(manage_annareain, manage_cumareain, neg_agg, pos_agg)
		
	# change order of rows 
	manage_out = manage_out[order(manage_out$Land_Cat_ID, manage_out$Management),]
	# assign to output sheet
	out_scen_df_list[[3]] = manage_out
  } else { # end if there are management practices
    out_scen_df_list[[3]] <- data.frame(Land_Cat_ID=numeric(0), Region=character(0), Land_Type=character(0), 
                                                 Ownership=character(0), Management=character(0), stringsAsFactors=FALSE)
  }
	
  # write the scenario file
	# write the headers also
	
	out_file = paste0(out_dir, scenin_name, "_", scen_tag, xltag)
	# out_file = paste0(out_dir, scenin_name, "_", xltag)
	# put the output tables in a workbook
	out_wrkbk =  loadWorkbook(out_file, create = TRUE)
	createSheet(out_wrkbk, name = out_scen_sheets)
	clearSheet(out_wrkbk, sheet = out_scen_sheets)
	writeWorksheet(out_wrkbk, data = scen_head_df_list, sheet = out_scen_sheets, startRow = 1, header = FALSE)
	writeWorksheet(out_wrkbk, data = out_scen_df_list, sheet = out_scen_sheets, startRow = start_row, header = TRUE)	
	# shut off wrap text
	cs <- createCellStyle(out_wrkbk)
	setWrapText(cs, wrap = FALSE)
	for (i in 1:length(out_scen_sheets)) {
		rc = expand.grid(row = 1:(nrow(out_scen_df_list[[i]])+start_row), col = 1:ncol(out_scen_df_list[[i]]))
		setCellStyle(out_wrkbk, sheet = out_scen_sheets[i], row = rc$row, col = rc$col, cellstyle = cs)
	}

	# write the workbook
	saveWorkbook(out_wrkbk)

} # end for s loop over the scenario definitions

#####################
# read in the carbon files
# these can have NaN values
for (i in 1:num_c_in_files) {
	in_c_df_list[[i]] = read.csv(paste0(in_dir,carbon_gis_files[i]), stringsAsFactors = FALSE)
}

# loop over the 7 carbon pool tables
# the order is above ground, below ground, understory, standing dead, downded dead, litter, soil
for (p in cpool_start:cpool_end) {
	# first identify which carbon files are needed for this pool
	file_found = grepl(out_c_tags[p], carbon_gis_files)
	file_indices = which(file_found)
	# which one is the se file
	se_index = intersect(which(grepl("_se_", carbon_gis_files)), file_indices)
	val_index = setdiff(file_indices, se_index)
	
	out_table = merge(out_scen_df_list[[1]], in_c_df_list[[val_index]], by.x = c("Land_Cat_ID"), by.y = c("zone"), all.x = TRUE)
	out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "min", "max", "mean", "stddev")]
	names(out_table)[] = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")
	if (length(se_index) > 0) { # biomass c
		out_table = merge(out_table, in_c_df_list[[se_index]], by.x = c("Land_Cat_ID"), by.y = c("zone"), all.x = TRUE)
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha", "mean", "stddev")]
	} else { # soil c
		out_table$"Mean_SE_Mg_ha" = NA
		out_table$"Stddev_SE_Mg_ha" = NA
	}
	names(out_table)[] = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")
	
	# fill Developed-all above ground c
	if (out_c_tags[p] == "agc") {
		for	(r in 1:num_reg) {
			out_table$Min_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_agc_min[r]
			out_table$Max_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_agc_max[r]
			out_table$Mean_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_agc_mean[r]
			out_table$Stddev_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_agc_stddev[r]
		}
	}
	
	# fill Developed-all below ground c
	if (out_c_tags[p] == "bgc") {
		for	(r in 1:num_reg) {
			out_table$Min_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_bgc_min[r]
			out_table$Max_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_bgc_max[r]
			out_table$Mean_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_bgc_mean[r]
			out_table$Stddev_Mg_ha[out_table$Land_Type == "Developed_all" & out_table$Region == reg_names[r]] = dev_all_bgc_stddev[r]
		}
	}
	
	########### for biomass c
	# fill values for land categories with no data, except for fresh marsh, cultivated, developed_all, 
	# and seagrass
	# currently, with the landfire data, there are no missing data to fill
	# the only errors that should occur are when there are no available vals
	# there should not be any zero area categories in these calculations
	# don't bother with the SE columns because they currently are not used
	if (out_c_tags[p] != "soc") {
		# first check same land type within region, and use area weighted average over other ownerships
		avail_vals = out_table[!is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		                         out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		                         out_table$Land_Type != "Seagrass",]
		# get the area sum for weighting
		area_agg = aggregate(Area_ha ~ Region + Land_Type, avail_vals, FUN = sum, na.rm = TRUE)
		names(area_agg)[ncol(area_agg)] = "Area_ha_sum"
		avail_vals = merge(avail_vals, area_agg, by = c("Region", "Land_Type"), all.x =TRUE)
		avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] * avail_vals$Area_ha / 
		  avail_vals$Area_ha_sum
		avail_vals_agg = aggregate(cbind(Min_Mg_ha, Max_Mg_ha, Mean_Mg_ha) ~ Region + Land_Type, avail_vals, 
		                           FUN = sum, na.rm = TRUE)
		# error prop on the stddev
		stddev_vals_agg = aggregate(Stddev_Mg_ha ~ Region + Land_Type, avail_vals, FUN = function(x) {sqrt(sum(x^2))})
		avail_vals_agg$Stddev_Mg_ha = stddev_vals_agg$Stddev_Mg_ha
		# merge these new values, assign them, then delete the extra columns
		names(avail_vals_agg)[3:6] = c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")
		out_table = merge(out_table, avail_vals_agg, by = c("Region", "Land_Type"), all.x =TRUE)
		out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		            out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		            out_table$Land_Type != "Seagrass",c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		              out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		              out_table$Land_Type != "Seagrass",c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", 
		                                                  "Stddev_Mg_ha_agg")]
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", 
		                         "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")]
		
		# now check same land type and same ownership within all regions, and use area weighted average over other regions
		avail_vals = out_table[!is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		                         out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		                         out_table$Land_Type != "Seagrass",]
		# get the area sum for weighting
		area_agg = aggregate(Area_ha ~ Land_Type + Ownership, avail_vals, FUN = sum, na.rm = TRUE)
		names(area_agg)[ncol(area_agg)] = "Area_ha_sum"
		avail_vals = merge(avail_vals, area_agg, by = c("Land_Type", "Ownership"), all.x =TRUE)
		avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] * 
		  avail_vals$Area_ha / avail_vals$Area_ha_sum
		avail_vals_agg = aggregate(cbind(Min_Mg_ha, Max_Mg_ha, Mean_Mg_ha) ~ Land_Type + Ownership, avail_vals, 
		                           FUN = sum, na.rm = TRUE)
		# error prop on the stddev
		stddev_vals_agg = aggregate(Stddev_Mg_ha ~ Land_Type + Ownership, avail_vals, FUN = function(x) {sqrt(sum(x^2))})
		avail_vals_agg$Stddev_Mg_ha = stddev_vals_agg$Stddev_Mg_ha
		# merge these new values, assign them, then delete the extra columns
		names(avail_vals_agg)[3:6] = c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")
		out_table = merge(out_table, avail_vals_agg, by = c("Land_Type", "Ownership"), all.x =TRUE)
		out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & out_table$Land_Type != "Cultivated" & 
		            out_table$Land_Type != "Developed_all" & out_table$Land_Type != "Seagrass",
		          c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		              out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		              out_table$Land_Type != "Seagrass",
		            c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")]
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", "Max_Mg_ha", 
		                         "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")]
		
		# finally check same land type in all ownerships within all regions, and use area weighted average over other 
		# regions and ownerships
		avail_vals = out_table[!is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		                         out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		                         out_table$Land_Type != "Seagrass",]
		# get the area sum for weighting
		area_agg = aggregate(Area_ha ~ Land_Type, avail_vals, FUN = sum, na.rm = TRUE)
		names(area_agg)[ncol(area_agg)] = "Area_ha_sum"
		avail_vals = merge(avail_vals, area_agg, by = c("Land_Type"), all.x =TRUE)
		avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] * avail_vals$Area_ha / 
		  avail_vals$Area_ha_sum
		avail_vals_agg = aggregate(cbind(Min_Mg_ha, Max_Mg_ha, Mean_Mg_ha) ~ Land_Type, avail_vals, FUN = sum, 
		                           na.rm = TRUE)
		# error prop on the stddev
		stddev_vals_agg = aggregate(Stddev_Mg_ha ~ Land_Type, avail_vals, FUN = function(x) {sqrt(sum(x^2))})
		avail_vals_agg$Stddev_Mg_ha = stddev_vals_agg$Stddev_Mg_ha
		# merge these new values, assign them, then delete the extra columns
		names(avail_vals_agg)[2:5] = c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")
		out_table = merge(out_table, avail_vals_agg, by = c("Land_Type"), all.x =TRUE)
		out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		            out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		            out_table$Land_Type != "Seagrass",c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		              out_table$Land_Type != "Cultivated" & out_table$Land_Type != "Developed_all" & 
		              out_table$Land_Type != "Seagrass",c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", 
		                                                  "Stddev_Mg_ha_agg")]
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", "Max_Mg_ha", 
		                         "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")]
		
		# remove the area column, order table, and put table in the output df list
		out_table$Area_ha = NULL
		out_table = out_table[order(out_table$Land_Cat_ID, out_table$Region, out_table$Land_Type, 
		                            out_table$Ownership),]
		out_c_df_list[[p]] = out_table
		
	} # end biomass c null data filling
	
	########### for soil c
	# first update the rangeland values (except for the stddev values)
	# currently, with the 940 land categories, only 6 need filling:
	#	110001 Central_Coast Meadow BLM; 303002 Delta Sparse DoD; 409005 Deserts Forest NPS; 409008 Deserts 
	# Forest State_gov; 501007 Eastside Ice Private; 601009 Klamath Ice USFS_nonwild
	#	these are all filled with the average of other ownerships in the same region and same land type
	# then fill values for the land categories without values, except for fresh marsh and seagrass
	# the only errors that should occur are when there are no available vals
	# there should not be any zero area categories in these calculations
	# don't bother with the SE columns because these data are not available for the soil c
	if (out_c_tags[p] == "soc") {
	
		# first update the rangeland soil data
		out_table$Min_Mg_ha[out_table$Land_Type == "Grassland" | out_table$Land_Type == "Savanna" | 
		                      out_table$Land_Type == "Woodland"] = range_soc_min
		out_table$Max_Mg_ha[out_table$Land_Type == "Grassland" | out_table$Land_Type == "Savanna" | 
		                      out_table$Land_Type == "Woodland"] = range_soc_max
		out_table$Mean_Mg_ha[out_table$Land_Type == "Grassland" | out_table$Land_Type == "Savanna" | 
		                       out_table$Land_Type == "Woodland"] = range_soc_mean

		# now fill the missing data
		
		# first check same land type within region, and use area weighted average over other ownerships
		avail_vals = out_table[!is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		                         out_table$Land_Type != "Seagrass",]
		# get the area sum for weighting
		area_agg = aggregate(Area_ha ~ Region + Land_Type, avail_vals, FUN = sum, na.rm = TRUE)
		names(area_agg)[ncol(area_agg)] = "Area_ha_sum"
		avail_vals = merge(avail_vals, area_agg, by = c("Region", "Land_Type"), all.x =TRUE)
		avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] * 
		  avail_vals$Area_ha / avail_vals$Area_ha_sum
		avail_vals_agg = aggregate(cbind(Min_Mg_ha, Max_Mg_ha, Mean_Mg_ha) ~ 
		                             Region + Land_Type, avail_vals, FUN = sum, na.rm = TRUE)
		# error prop on the stddev
		stddev_vals_agg = aggregate(Stddev_Mg_ha ~ Region + Land_Type, avail_vals, FUN = function(x) {sqrt(sum(x^2))})
		avail_vals_agg$Stddev_Mg_ha = stddev_vals_agg$Stddev_Mg_ha
		# merge these new values, assign them, then delete the extra columns
		names(avail_vals_agg)[3:6] = c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")
		out_table = merge(out_table, avail_vals_agg, by = c("Region", "Land_Type"), all.x =TRUE)
		out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		            out_table$Land_Type != "Seagrass",c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		              out_table$Land_Type != "Seagrass",c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", 
		                                                  "Stddev_Mg_ha_agg")]
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", 
		                         "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")]
		
		# now check same land type and same ownership within all regions, and use area weighted average 
		# over other regions
		avail_vals = out_table[!is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		                         out_table$Land_Type != "Seagrass",]
		# get the area sum for weighting
		area_agg = aggregate(Area_ha ~ Land_Type + Ownership, avail_vals, FUN = sum, na.rm = TRUE)
		names(area_agg)[ncol(area_agg)] = "Area_ha_sum"
		avail_vals = merge(avail_vals, area_agg, by = c("Land_Type", "Ownership"), all.x =TRUE)
		avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] * 
		  avail_vals$Area_ha / avail_vals$Area_ha_sum
		avail_vals_agg = aggregate(cbind(Min_Mg_ha, Max_Mg_ha, Mean_Mg_ha) ~ Land_Type + Ownership, avail_vals, 
		                           FUN = sum, na.rm = TRUE)
		# error prop on the stddev
		stddev_vals_agg = aggregate(Stddev_Mg_ha ~ Land_Type + Ownership, avail_vals, FUN = function(x) {sqrt(sum(x^2))})
		avail_vals_agg$Stddev_Mg_ha = stddev_vals_agg$Stddev_Mg_ha
		# merge these new values, assign them, then delete the extra columns
		names(avail_vals_agg)[3:6] = c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")
		out_table = merge(out_table, avail_vals_agg, by = c("Land_Type", "Ownership"), all.x =TRUE)
		out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		            out_table$Land_Type != "Seagrass",c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		              out_table$Land_Type != "Seagrass",c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", 
		                                                  "Stddev_Mg_ha_agg")]
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha", 
		                         "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")]
		
		# finally check same land type in all ownerships within all regions, and use area weighted average over 
		# other regions and ownerships
		avail_vals = out_table[!is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		                         out_table$Land_Type != "Seagrass",]
		# get the area sum for weighting
		area_agg = aggregate(Area_ha ~ Land_Type, avail_vals, FUN = sum, na.rm = TRUE)
		names(area_agg)[ncol(area_agg)] = "Area_ha_sum"
		avail_vals = merge(avail_vals, area_agg, by = c("Land_Type"), all.x =TRUE)
		avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  avail_vals[,c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] * 
		  avail_vals$Area_ha / avail_vals$Area_ha_sum
		avail_vals_agg = aggregate(cbind(Min_Mg_ha, Max_Mg_ha, Mean_Mg_ha) ~ Land_Type, avail_vals, 
		                           FUN = sum, na.rm = TRUE)
		# error prop on the stddev
		stddev_vals_agg = aggregate(Stddev_Mg_ha ~ Land_Type, avail_vals, FUN = function(x) {sqrt(sum(x^2))})
		avail_vals_agg$Stddev_Mg_ha = stddev_vals_agg$Stddev_Mg_ha
		# merge these new values, assign them, then delete the extra columns
		names(avail_vals_agg)[2:5] = c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", "Stddev_Mg_ha_agg")
		out_table = merge(out_table, avail_vals_agg, by = c("Land_Type"), all.x =TRUE)
		out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		            out_table$Land_Type != "Seagrass",c("Min_Mg_ha", "Max_Mg_ha", "Mean_Mg_ha", "Stddev_Mg_ha")] = 
		  out_table[is.na(out_table$Mean_Mg_ha) & out_table$Land_Type != "Fresh_marsh" & 
		              out_table$Land_Type != "Seagrass",c("Min_Mg_ha_agg", "Max_Mg_ha_agg", "Mean_Mg_ha_agg", 
		                                                  "Stddev_Mg_ha_agg")]
		out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Min_Mg_ha", "Max_Mg_ha", 
		                         "Mean_Mg_ha", "Stddev_Mg_ha", "Mean_SE_Mg_ha", "Stddev_SE_Mg_ha")]
		
		# order table, and put table in the output df list
		out_table = out_table[order(out_table$Land_Cat_ID, out_table$Region, out_table$Land_Type, 
		                            out_table$Ownership),]
		out_c_df_list[[p]] = out_table

	} # end soil c null data filling
	
} # end p loop over the carbon pools

# make the 2 aggregate carbon tables
# but do it two different ways

# first way, for mapping: aggregate all categories, even if component pools are missing 
# (Cultivated and Developed_all and Fresh_marsh and Seagrass)
# second way, for input files: if component pools are missing, let the sums go to NA
# the same loops for both

############### make a separate file for mapping the carbon
# here the totals will be summed for all types, even if some component pools are missing 
# (Cultivated and Developed_all and Fresh_marsh and Seagrass)

out_c_map_df_list = out_c_df_list

############### the input files will reflect where there are missing components 
# (Cultivated and Developed_all and Fresh_marsh and Seagrass)
# here the totals for land categories with missing components will revert to NA

# all organic c

out_c_df_list[[allc_ind]] = out_c_df_list[[cpool_start]]
out_c_df_list[[allc_ind]][,5:10] = 0
# don't propagate the SE stddev to the sums
out_c_df_list[[allc_ind]]$Stddev_SE_Mg_ha = NA

out_c_map_df_list[[allc_ind]] = out_c_map_df_list[[cpool_start]]
out_c_map_df_list[[allc_ind]][,5:10] = 0
# don't propagate the SE stddev to the sums
out_c_map_df_list[[allc_ind]]$Stddev_SE_Mg_ha = NA

# loop through all c dens pools and add to new column
for (i in cpool_start:cpool_end) {
	out_c_df_list[[allc_ind]]$Min_Mg_ha = out_c_df_list[[allc_ind]]$Min_Mg_ha + out_c_df_list[[i]]$Min_Mg_ha
	out_c_df_list[[allc_ind]]$Max_Mg_ha = out_c_df_list[[allc_ind]]$Max_Mg_ha + out_c_df_list[[i]]$Max_Mg_ha
	out_c_df_list[[allc_ind]]$Mean_Mg_ha = out_c_df_list[[allc_ind]]$Mean_Mg_ha + out_c_df_list[[i]]$Mean_Mg_ha
	out_c_df_list[[allc_ind]]$Stddev_Mg_ha = out_c_df_list[[allc_ind]]$Stddev_Mg_ha + 
	  out_c_df_list[[i]]$Stddev_Mg_ha * out_c_df_list[[i]]$Stddev_Mg_ha
	out_c_df_list[[allc_ind]]$Mean_SE_Mg_ha = out_c_df_list[[allc_ind]]$Mean_SE_Mg_ha + 
	  out_c_df_list[[i]]$Mean_SE_Mg_ha * out_c_df_list[[i]]$Mean_SE_Mg_ha
	
	na_inds = which(is.na(out_c_map_df_list[[i]]$Min_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Min_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[allc_ind]]$Min_Mg_ha = out_c_map_df_list[[allc_ind]]$Min_Mg_ha + add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Max_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Max_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[allc_ind]]$Max_Mg_ha = out_c_map_df_list[[allc_ind]]$Max_Mg_ha + add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Mean_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Mean_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[allc_ind]]$Mean_Mg_ha = out_c_map_df_list[[allc_ind]]$Mean_Mg_ha + add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Stddev_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Stddev_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[allc_ind]]$Stddev_Mg_ha = out_c_map_df_list[[allc_ind]]$Stddev_Mg_ha + 
	  add_vals * add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Mean_SE_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Mean_SE_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[allc_ind]]$Mean_SE_Mg_ha = out_c_map_df_list[[allc_ind]]$Mean_SE_Mg_ha + 
	  add_vals * add_vals
}
out_c_df_list[[allc_ind]]$Stddev_Mg_ha = sqrt(out_c_df_list[[allc_ind]]$Stddev_Mg_ha)
out_c_df_list[[allc_ind]]$Mean_SE_Mg_ha = sqrt(out_c_df_list[[allc_ind]]$Mean_SE_Mg_ha)

out_c_map_df_list[[allc_ind]]$Stddev_Mg_ha = sqrt(out_c_map_df_list[[allc_ind]]$Stddev_Mg_ha)
out_c_map_df_list[[allc_ind]]$Mean_SE_Mg_ha = sqrt(out_c_map_df_list[[allc_ind]]$Mean_SE_Mg_ha)

# biomass c (non-soil c)

out_c_df_list[[biomassc_ind]] = out_c_df_list[[allc_ind]]
out_c_df_list[[biomassc_ind]][,5:10] = 0
# don't propagate the SE stddev to the sums
out_c_df_list[[biomassc_ind]]$Stddev_SE_Mg_ha = NA

out_c_map_df_list[[biomassc_ind]] = out_c_map_df_list[[allc_ind]]
out_c_map_df_list[[biomassc_ind]][,5:10] = 0
# don't propagate the SE stddev to the sums
out_c_map_df_list[[biomassc_ind]]$Stddev_SE_Mg_ha = NA

# loop through all c dens pools and add to new column
for (i in cpool_start:(cpool_end-1)) {
	out_c_df_list[[biomassc_ind]]$Min_Mg_ha = out_c_df_list[[biomassc_ind]]$Min_Mg_ha + 
	  out_c_df_list[[i]]$Min_Mg_ha
	out_c_df_list[[biomassc_ind]]$Max_Mg_ha = out_c_df_list[[biomassc_ind]]$Max_Mg_ha + 
	  out_c_df_list[[i]]$Max_Mg_ha
	out_c_df_list[[biomassc_ind]]$Mean_Mg_ha = out_c_df_list[[biomassc_ind]]$Mean_Mg_ha + 
	  out_c_df_list[[i]]$Mean_Mg_ha
	out_c_df_list[[biomassc_ind]]$Stddev_Mg_ha = out_c_df_list[[biomassc_ind]]$Stddev_Mg_ha + 
	  out_c_df_list[[i]]$Stddev_Mg_ha * out_c_df_list[[i]]$Stddev_Mg_ha
	out_c_df_list[[biomassc_ind]]$Mean_SE_Mg_ha = out_c_df_list[[biomassc_ind]]$Mean_SE_Mg_ha + 
	  out_c_df_list[[i]]$Mean_SE_Mg_ha * out_c_df_list[[i]]$Mean_SE_Mg_ha
	
	na_inds = which(is.na(out_c_map_df_list[[i]]$Min_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Min_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[biomassc_ind]]$Min_Mg_ha = out_c_map_df_list[[biomassc_ind]]$Min_Mg_ha + add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Max_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Max_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[biomassc_ind]]$Max_Mg_ha = out_c_map_df_list[[biomassc_ind]]$Max_Mg_ha + add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Mean_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Mean_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[biomassc_ind]]$Mean_Mg_ha = out_c_map_df_list[[biomassc_ind]]$Mean_Mg_ha + add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Stddev_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Stddev_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[biomassc_ind]]$Stddev_Mg_ha = out_c_map_df_list[[biomassc_ind]]$Stddev_Mg_ha + 
	  add_vals * add_vals
	na_inds = which(is.na(out_c_map_df_list[[i]]$Mean_SE_Mg_ha))
	add_vals = out_c_map_df_list[[i]]$Mean_SE_Mg_ha
	add_vals[na_inds] = 0
	out_c_map_df_list[[biomassc_ind]]$Mean_SE_Mg_ha = out_c_map_df_list[[biomassc_ind]]$Mean_SE_Mg_ha + 
	  add_vals * add_vals
}
out_c_df_list[[biomassc_ind]]$Stddev_Mg_ha = sqrt(out_c_df_list[[biomassc_ind]]$Stddev_Mg_ha)
out_c_df_list[[biomassc_ind]]$Mean_SE_Mg_ha = sqrt(out_c_df_list[[biomassc_ind]]$Mean_SE_Mg_ha)

out_c_map_df_list[[biomassc_ind]]$Stddev_Mg_ha = sqrt(out_c_map_df_list[[biomassc_ind]]$Stddev_Mg_ha)
out_c_map_df_list[[biomassc_ind]]$Mean_SE_Mg_ha = sqrt(out_c_map_df_list[[biomassc_ind]]$Mean_SE_Mg_ha)


############### make the conversion/management parameter tables
# regionalization of veg and soil uptake values is now explicit in the raw data 
# wildfire is passed through exactly as is
# the rest are merged with the full set of land cats, then the NA rows are removed

# loop over the management parameter tables (except wildfire)
 # recall:
#params_start = 10
#params_end = 17
#vegcuptake_ind = 10
#soilcaccum_ind = 11
#conversion_ind = 12
#forest_man_ind = 13
#ag_manage_ind = 16
#wildfire_ind = 17

 # for vegCuptake, soilCuptake, conversion, forest_man, dev_manage, grass_manage, ag_manage, wildfire 
for (m in params_start:params_end) {
  # m = 10 to 17
  # in_index = 1 to 8
	in_index = m - params_start + 1
	# assign how merging will be done depending on which table is being worked on in loop
	  # if vegcuptake, soilcaccum merging will be done by reg, landtype, and ownership 
	if (m == vegcuptake_ind | m == soilcaccum_ind) {
		mergeby = c("Region", "Land_Type", "Ownership")
		# if forest_man merging will be done by landtype and ownership 
	} else if (m == forest_man_ind) {
		mergeby = c("Land_Type", "Ownership")
	# if conversion, dev_manage, grass_manage, or wildfire merge by landtype
	} else if (m == ag_manage_ind) {
		mergeby = c("Region", "Land_Type")
	} else {
		mergeby = c("Land_Type")
	}
	
	# if on wildfire table (m=17, in_index=8),
	if (m == wildfire_ind) {
	  # assign the wildfire param table to the 17th df of out_c_df_list
		out_c_df_list[[wildfire_ind]] = param_df_list[[in_index]]
		# otherwise for veg and soil c accum
	} else if (m == vegcuptake_ind | m==soilcaccum_ind) {
		# split the records based on complete specification, all own, or all region, or all own all region
		# assign the veg or soil C param table to paramin
	  	paramin = param_df_list[[in_index]]
		# assign rows with  ownership- and region-specific land types to complete_recs
	  	complete_recs = paramin[paramin$Ownership != "All" & paramin$Region != "All",]
	    # assign region-specifc landtype to allown_recs
	  	allown_recs = paramin[paramin$Ownership == "All" & paramin$Region != "All",]
	    # assign ownership-specifc landtype to allreg_recs
		allreg_recs = paramin[paramin$Region == "All" & paramin$Ownership != "All",]
		  # assign statewide landtype to allownreg_recs
		allownreg_recs = paramin[paramin$Region == "All" & paramin$Ownership == "All",]
		
		# create column names 
		 # "Region", "Land_Type","allown","Min_Mg_ha_yr","Max_Mg_ha_yr","Mean_Mg_ha_yr","Stddev_Mg_ha_yr"
		names(allown_recs)[grep("^Ownership$", colnames(allown_recs))] = "allown"
		  # "allreg","Land_Type","Ownership","Min_Mg_ha_yr","Max_Mg_ha_yr","Mean_Mg_ha_yr","Stddev_Mg_ha_yr"
		names(allreg_recs)[grep("^Region$", colnames(allreg_recs))] = "allreg"
		  # "allreg","Land_Type","allown","Min_Mg_ha_yr","Max_Mg_ha_yr","Mean_Mg_ha_yr","Stddev_Mg_ha_yr"
		names(allownreg_recs)[grep("^Region$", colnames(allownreg_recs))] = "allreg"
		 # "allreg","Land_Type","allown","Min_Mg_ha_yr","Max_Mg_ha_yr","Mean_Mg_ha_yr","Stddev_Mg_ha_yr"
		names(allownreg_recs)[grep("^Ownership$", colnames(allownreg_recs))] = "allown"
	
		# merge these groups accordingly with the start area table into accum1, accum2, accum3, and accum4, depending on if
		 # there any records in each, and create list accum_df_list with each table (1 to 4) represented regardless if it has any rows
		area = out_scen_df_list[[1]]
		AE = NULL
		# first merge region- and ownership-specific landtypes with initial areas
		if (nrow(complete_recs) > 0) {
			accum1 = merge(area, complete_recs, by = c("Region", "Land_Type", "Ownership"), all.y = TRUE)
			accum1 = accum1[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha_yr", "Max_Mg_ha_yr", "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
			accum1 = accum1[accum1$Region%in%complete_recs$Region & accum1$Land_Type%in%complete_recs$Land_Type & accum1$Ownership%in%complete_recs$Ownership,]
			accum_df_list[[1]] = accum1
			AE = c(AE,1)
		}
		# then region-specifc landtypes with initial areas
		if (nrow(allown_recs) > 0) {
			accum2 = merge(area, allown_recs, by = c("Region", "Land_Type"), all.y = TRUE)
			accum2$allown = NULL
			accum2 = accum2[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha_yr", "Max_Mg_ha_yr", "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
			accum2 = accum2[accum2$Region%in%allown_recs$Region & accum2$Land_Type%in% allown_recs$Land_Type,]
			accum_df_list[[2]] = accum2
			AE = c(AE,2)
		}
		# then ownership-specifc landtypes with initial areas
		if (nrow(allreg_recs) > 0) {
			accum3 = merge(area, allreg_recs, by = c("Land_Type", "Ownership"), all.y = TRUE)
			accum3$allreg = NULL
			accum3 = accum3[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha_yr", "Max_Mg_ha_yr", "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
			accum3 = accum3[accum3$Land_Type%in%allreg_recs$Land_Type & accum3$Ownership%in% allreg_recs $Ownership,]
			accum_df_list[[3]] = accum3
			AE = c(AE,3)
		}
		# then statewide landtypes with initial areas
		if (nrow(allownreg_recs) > 0) {
			accum4 = merge(area, allownreg_recs, by = c("Land_Type"), all.y = TRUE)
			accum4$allreg = NULL
			accum4$allown = NULL
			accum4 = accum4[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha", "Min_Mg_ha_yr", "Max_Mg_ha_yr", "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
			accum4 = accum4[accum4$Land_Type%in%allownreg_recs$Land_Type,]
			accum_df_list[[4]] = accum4
			AE = c(AE,4)
		}
		
		# bind the groups together into one accum table
		# if there's only one group
		if (length(AE) == 1) {
		  # index the single group ID in accum_df_list using tracker ID in AE, and call it accum 
			accum = accum_df_list[[AE[1]]]
			# otherwise, if there's >1 group
		} else if (length(AE) > 1) {
		  # do the same
			accum = accum_df_list[[AE[1]]]
			# and rbind additional groups to accum, resulting in df accum with 1 column and length 2 to 4
			for (al in 2:length(AE)) {
				accum = rbind(accum, accum_df_list[[AE[al]]])
			}
			# otherwise there's 0 records and give error
		} else {
			cat("Error: no veg or soil uptake records!\n")
		}
		
    # merge the initial area table with the ID for type of record by all spatial identifiers and area
		accum = merge(area, accum, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Area_ha"), all.x = TRUE)

		# get regional land type areas (reg_lt_ha) and merge with the accum table (C input params and areas)
		accum_reg_agg = aggregate(Area_ha ~ Region + Land_Type, accum, FUN=sum, na.rm = TRUE)
		names(accum_reg_agg)[ncol(accum_reg_agg)] = "reg_lt_ha"
		accum = merge(accum, accum_reg_agg, by = c("Region", "Land_Type"), all.x = TRUE)
		# get ownership land type areas (own_lt_ha) and merge with the accum table (C input params and areas)
		accum_own_agg = aggregate(Area_ha ~ Land_Type + Ownership, accum, FUN=sum, na.rm = TRUE)
		names(accum_own_agg)[ncol(accum_own_agg)] = "own_lt_ha"
		accum = merge(accum, accum_own_agg, by = c("Land_Type", "Ownership"), all.x = TRUE)
		# get total land type areas (lt_ha) and merge with the accum table (C input params and areas)
		accum_lt_agg = aggregate(Area_ha ~ Land_Type, accum, FUN=sum, na.rm = TRUE)
		names(accum_lt_agg)[ncol(accum_lt_agg)] = "lt_ha"
		accum = merge(accum, accum_lt_agg, by = c("Land_Type"), all.x = TRUE)
		
		# no adjustments below should be necessary, so this should be the last task for veg and soil c uptake:
		# delete extra columns and order the table and put it in the output list
		accum = accum[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Min_Mg_ha_yr", "Max_Mg_ha_yr", 
		                 "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
		accum = accum[order(accum$Land_Cat_ID, accum$Region, accum$Land_Type, accum$Ownership),]
		out_c_df_list[[m]] = accum
		
		# veg c accum table adjustments
		if (FALSE) {
		#if (m == vegcuptake_ind) {
			# forest
			# adjusted regional veg c uptake = statewide input veg c uptake average * regional npp / 
		  #   statewide npp average
			# adjusted regional ownership veg c uptake = reg own input veg c uptake * 
		  #     adjusted regional veg c uptake / regional input veg c uptake average
			# add the regional forest npp data to the veg table
			accum = merge(accum, forest_npp, by = c("Region", "Land_Type"), all.x = TRUE)
			
			# calculate the statewide input averages
			accum$tvegc_min[accum$Land_Type == "Forest"] = accum[accum$Land_Type == "Forest","Min_Mg_ha_yr"] * 
			  accum$own_lt_ha[accum$Land_Type == "Forest"] / accum$lt_ha[accum$Land_Type == "Forest"]
			accum$tvegc_max[accum$Land_Type == "Forest"] = accum[accum$Land_Type == "Forest","Max_Mg_ha_yr"] * 
			  accum$own_lt_ha[accum$Land_Type == "Forest"] / accum$lt_ha[accum$Land_Type == "Forest"]
			accum$tvegc_mean[accum$Land_Type == "Forest"] = accum[accum$Land_Type == "Forest","Mean_Mg_ha_yr"] * 
			  accum$own_lt_ha[accum$Land_Type == "Forest"] / accum$lt_ha[accum$Land_Type == "Forest"]
			accum$tvegc_std[accum$Land_Type == "Forest"] = (accum[accum$Land_Type == "Forest",c("Stddev_Mg_ha_yr")] * 
			                                                  accum$own_lt_ha[accum$Land_Type == "Forest"] / 
			                                                  accum$lt_ha[accum$Land_Type == "Forest"])^2
			# use a region that has all ownerships for Forest, so that the sum is complete for the state
			vegc_agg = aggregate(cbind(tvegc_min, tvegc_max, tvegc_mean, tvegc_std) ~ 
			                       Land_Type, accum[accum$Region == "Sierra_Cascades",], FUN=sum, na.rm = TRUE)
			vegc_agg$tvegc_std = sqrt(vegc_agg$tvegc_std)
			names(vegc_agg)[(ncol(vegc_agg)-3):ncol(vegc_agg)] = c("svegc_min", "svegc_max", "svegc_mean", "svegc_std")
			accum = merge(accum, vegc_agg, by = c("Land_Type"), all.x = TRUE)
			
			# calculate the regional input averages
			accum$tvegc_min[accum$Land_Type == "Forest"] = 
			  accum[accum$Land_Type == "Forest","Min_Mg_ha_yr"] * accum$Area_ha[accum$Land_Type == "Forest"] / 
			  accum$reg_lt_ha[accum$Land_Type == "Forest"]
			accum$tvegc_max[accum$Land_Type == "Forest"] = 
			  accum[accum$Land_Type == "Forest","Max_Mg_ha_yr"] * accum$Area_ha[accum$Land_Type == "Forest"] / 
			  accum$reg_lt_ha[accum$Land_Type == "Forest"]
			accum$tvegc_mean[accum$Land_Type == "Forest"] = 
			  accum[accum$Land_Type == "Forest","Mean_Mg_ha_yr"] * accum$Area_ha[accum$Land_Type == "Forest"] / 
			  accum$reg_lt_ha[accum$Land_Type == "Forest"]
			accum$tvegc_std[accum$Land_Type == "Forest"] = 
			  (accum[accum$Land_Type == "Forest",c("Stddev_Mg_ha_yr")] * accum$Area_ha[accum$Land_Type == "Forest"] /
			     accum$reg_lt_ha[accum$Land_Type == "Forest"])^2
			vegc_agg = aggregate(cbind(tvegc_min, tvegc_max, tvegc_mean, tvegc_std) ~ 
			                       Region + Land_Type, accum, FUN=sum, na.rm = TRUE)
			vegc_agg$tvegc_std = sqrt(vegc_agg$tvegc_std)
			names(vegc_agg)[(ncol(vegc_agg)-3):ncol(vegc_agg)] = c("rvegc_min", "rvegc_max", "rvegc_mean", "rvegc_std")
			accum = merge(accum, vegc_agg, by = c("Region", "Land_Type"), all.x = TRUE)
			
			# calculate the statewide npp average
			accum$tnpp_avg[accum$Land_Type == "Forest"] = accum[accum$Land_Type == "Forest","npp"] * 
			  accum$reg_lt_ha[accum$Land_Type == "Forest"] / accum$lt_ha[accum$Land_Type == "Forest"]
			# use a Forest ownership that is in all regions, so the sum is complete
			npp_agg = aggregate(tnpp_avg ~ Land_Type, accum[accum$Ownership == "Private",], FUN=sum, na.rm = TRUE)
			names(npp_agg)[ncol(npp_agg)] = "npp_avg"
			accum = merge(accum, npp_agg, by = c("Land_Type"), all.x = TRUE)
			# now calculate the adjusted forest vegc uptake values
			accum[accum$Land_Type == "Forest", c("Min_Mg_ha_yr", "Max_Mg_ha_yr", "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")] = 
			  accum[accum$Land_Type == "Forest", c("svegc_min", "svegc_max", "svegc_mean", "svegc_std")] * 
			  accum[accum$Land_Type == "Forest", "npp"] / accum[accum$Land_Type == "Forest", "npp_avg"] * 
			  accum[accum$Land_Type == "Forest", c("Min_Mg_ha_yr", "Max_Mg_ha_yr", "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")] / 
			  accum[accum$Land_Type == "Forest", c("rvegc_min", "rvegc_max", "rvegc_mean", "rvegc_std")]
			
			# delete extra columns and order the table and put it in the output list
			accum = accum[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Min_Mg_ha_yr", "Max_Mg_ha_yr", 
			                 "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
			accum = accum[order(accum$Land_Cat_ID, accum$Region, accum$Land_Type, accum$Ownership),]
			out_c_df_list[[m]] = accum
		} # end if veg c uptake adjustment
		
		
		# soil c accum table adjustments
		if (FALSE) {
		#if (m == soilcaccum_ind) {
		  #comment out the following adjustments for delta region as new values are now in lc_params.xls
			# adjust delta cultivated
			#accum[accum$Region == "Delta" & accum$Land_Type == "Cultivated", "Min_Mg_ha_yr"] = soil_c_accum_peat_min
			#accum[accum$Region == "Delta" & accum$Land_Type == "Cultivated", "Max_Mg_ha_yr"] = soil_c_accum_peat_max
			#accum[accum$Region == "Delta" & accum$Land_Type == "Cultivated", "Mean_Mg_ha_yr"] = soil_c_accum_peat_mean
			#accum[accum$Region == "Delta" & accum$Land_Type == "Cultivated", "Stddev_Mg_ha_yr"] = soil_c_accum_peat_stddev
			
			# delete extra columns and order the table and put it in the output list
			accum = accum[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Min_Mg_ha_yr", "Max_Mg_ha_yr", 
			                 "Mean_Mg_ha_yr", "Stddev_Mg_ha_yr")]
			accum = accum[order(accum$Land_Cat_ID, accum$Region, accum$Land_Type, accum$Ownership),]
			out_c_df_list[[m]] = accum
		} # end if soil c accum adjustment
		
	} # end if vegcuptake or soilcaccum
	# forest manage needs a special merge now to account for potential "All" region and/or "All" ownerships entries
	else if (m == forest_man_ind) {
		# land type is already specified as Forest
		# merge the records by location specified
		recs_landcat = param_df_list[[in_index]][param_df_list[[in_index]]$Region != "All" & param_df_list[[in_index]]$Ownership != "All",]
		recs_allreg = param_df_list[[in_index]][param_df_list[[in_index]]$Region == "All" & param_df_list[[in_index]]$Ownership != "All",]
		recs_allown = param_df_list[[in_index]][param_df_list[[in_index]]$Region != "All" & param_df_list[[in_index]]$Ownership == "All",]
		recs_allregown = param_df_list[[in_index]][param_df_list[[in_index]]$Region == "All" & param_df_list[[in_index]]$Ownership == "All",]
		ME = NULL

		# land cat
		if (nrow(recs_landcat) > 0) {
			out_table_landcat = merge(out_scen_df_list[[1]], recs_landcat, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
			# remove all the NA records from out_table
			out_table_landcat = out_table_landcat[!is.na(out_table_landcat[,ncol(out_table_landcat)]),]
			# remove the area column
			out_table_landcat $Area_ha = NULL
			# order the columns
			out_table_landcat = out_table_landcat[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", 
			                         # column headers from current param table
			                         # param_start_col = c(4, 4, 2, 5, 3, 3, 4, 2)
			                         # m = 10 to 17
			                         # in_index = 1 to 8
			                         # in_index = m - params_start + 1
			                         names(param_df_list[[in_index]])[param_start_col[in_index]:ncol(param_df_list[[in_index]])])]
			man_df_list[[1]] = out_table_landcat
			ME = c(ME,1)
		}

		# all region
		if (nrow(recs_allreg) > 0) {
			out_table_allreg = merge(out_scen_df_list[[1]], recs_allreg[,!(names(recs_allreg) %in% c("Region"))], by = c("Land_Type", "Ownership"), all.x = TRUE)
			# remove all the NA records from out_table
			out_table_allreg = out_table_allreg[!is.na(out_table_allreg[,ncol(out_table_allreg)]),]
			# remove the area column
			out_table_allreg $Area_ha = NULL
			# order the columns
			out_table_allreg = out_table_allreg[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", 
			                         names(param_df_list[[in_index]])[param_start_col[in_index]:ncol(param_df_list[[in_index]])])]
			man_df_list[[2]] = out_table_allreg
			ME = c(ME,2)
		}

		# all ownership
		if (nrow(recs_allown) > 0) {
			out_table_allown = merge(out_scen_df_list[[1]], recs_allown[,!(names(recs_allown) %in% c("Ownership"))], by = c("Region", "Land_Type"), all.x = TRUE)
			# remove all the NA records from out_table
			out_table_allown = out_table_allown[!is.na(out_table_allown[,ncol(out_table_allown)]),]
			# remove the area column
			out_table_allown $Area_ha = NULL
			# order the columns
			out_table_allown = out_table_allown[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", 
			                         names(param_df_list[[in_index]])[param_start_col[in_index]:ncol(param_df_list[[in_index]])])]
			man_df_list[[3]] = out_table_allown
			ME = c(ME,3)
		}

		# all region and all ownership
		if (nrow(recs_allregown) > 0) {
			out_table_allregown = merge(out_scen_df_list[[1]], recs_allregown[,!(names(recs_allregown) %in% c("Region", "Ownership"))], by = c("Land_Type"), all.x = TRUE)
			# remove all the NA records from out_table
			out_table_allregown = out_table_allregown[!is.na(out_table_allregown[,ncol(out_table_allregown)]),]	
			# remove the area column
			out_table_allregown $Area_ha = NULL
			# order the columns
			out_table_allown = out_table_allown[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", 
			                         names(param_df_list[[in_index]])[param_start_col[in_index]:ncol(param_df_list[[in_index]])])]
			man_df_list[[4]] = out_table_allregown
			ME = c(ME,4)
		}

		# bind the rows together
		
		# bind the groups together into one out table
		# if there's only one group
		if (length(ME) == 1) {
		  # index the single group ID in accum_df_list using tracker ID in AE, and call it out_table 
			out_table = man_df_list[[ME[1]]]
			# otherwise, if there's >1 group
		} else if (length(ME) > 1) {
		  # do the same
			out_table = man_df_list[[ME[1]]]
			# and rbind additional groups to out_table, resulting in df out_table
			for (ml in 2:length(ME)) {
				out_table = rbind(out_table, man_df_list[[ME[ml]]])
			}
			# otherwise there's 0 records and give error
		} else {
			cat("Error: no forest management definition records!\n")
		}
		
		# order the rows
		out_table = out_table[order(out_table$Land_Cat_ID, out_table$Region, out_table$Land_Type, out_table$Ownership, out_table$Management),]
		
		# add it to out_c_df_list
		out_c_df_list[[m]] = out_table
		
	} # end else if forest manage
	# for the rest of the parameter tables (conversion, dev_manage, grass_manage, ag_manage, wildfire)
	else { 
		# merge the parameter table from param_df_list with the initial areas and assign to out_table
		out_table = merge(out_scen_df_list[[1]], param_df_list[[in_index]], by = mergeby, all.x = TRUE)
		# remove all the NA records from out_table
		out_table = out_table[!is.na(out_table[,ncol(out_table)]),]
		# remove the area column
		out_table$Area_ha = NULL
		
		# commenting out the following speical treatment of Delta, as the values are now in lc_params
		  # using peat values for Delta agriculture
		  # if (m == ag_manage_ind) { out_table$SoilCaccum_frac[out_table$Region == "Delta"] = soil_c_accum_frac_peat}
		
		# adjust the column order
		# for dev_manage (m=14,in_index=5,param_start_col=3), 
		  #grass_manage (m=15,in_index=6, param_start_col=3), ag_manage (m=16,in_index=7,param_start_col=4), 
		  # wildfire (m=17, in_index=8, param_start_col=2)
		if (m != conversion_ind) {
		  # select the columns "Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management" along with
		   # a subset of columns from the current param table that correpond to the param_start_col to the last column
			out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management", 
			                         # column headers from current param table
			                         # param_start_col = c(4, 4, 2, 5, 3, 3, 4, 2)
			                         # m = 10 to 17
			                         # in_index = 1 to 8
			                         # in_index = m - params_start + 1
			                         names(param_df_list[[in_index]])[param_start_col[in_index]:ncol(param_df_list[[in_index]])])]
			# for conversion table
			} else {
				out_table = out_table[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", 
				                         names(param_df_list[[in_index]])[param_start_col[in_index]:ncol(param_df_list[[in_index]])])]
			}
		# order current out_table rows
		out_table = out_table[order(out_table$Land_Cat_ID, out_table$Region, out_table$Land_Type, out_table$Ownership),]
		# add it to out_c_df_list
		out_c_df_list[[m]] = out_table
	} # end if not veg and soil c accum and not forest manage

} # end 'for m loop' over the management parameter tables

# write the carbon file
# modify some of the input headers and write the headers also
	
out_file = c_file_out
# put the output tables in a workbook
out_wrkbk =  loadWorkbook(out_file, create = TRUE)
createSheet(out_wrkbk, name = out_c_sheets)
clearSheet(out_wrkbk, sheet = out_c_sheets)
writeWorksheet(out_wrkbk, data = param_head_df_list, sheet = out_c_sheets, startRow = 1, header = FALSE)
writeWorksheet(out_wrkbk, data = out_c_df_list, sheet = out_c_sheets, startRow = start_row, header = TRUE)
# shut off wrap text
cs <- createCellStyle(out_wrkbk)
setWrapText(cs, wrap = FALSE)
for (i in 1:length(out_c_sheets)) {
	rc = expand.grid(row = 1:(nrow(out_c_df_list[[i]])+start_row), col = 1:ncol(out_c_df_list[[i]]))
	setCellStyle(out_wrkbk, sheet = out_c_sheets[i], row = rc$row, col = rc$col, cellstyle = cs)
}
# write the workbook
saveWorkbook(out_wrkbk)

# write the carbon mapping file
# out_file = c_map_file_out 
# put the output tables in a workbook
# out_wrkbk =  loadWorkbook(out_file, create = TRUE)
# createSheet(out_wrkbk, name = out_c_sheets[1: cpool_end])
# clearSheet(out_wrkbk, sheet = out_c_sheets[1: cpool_end])
# writeWorksheet(out_wrkbk, data = param_head_df_list[1: cpool_end], sheet = out_c_sheets[1: cpool_end], 
# startRow = 1, header = FALSE)
# writeWorksheet(out_wrkbk, data = out_c_map_df_list[1: cpool_end], sheet = out_c_sheets[1: cpool_end], 
# startRow = start_row, header = TRUE)
# write the workbook
# saveWorkbook(out_wrkbk)

cat("Finish write_caland_inputs at", date(), "\n")
}
