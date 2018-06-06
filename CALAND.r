# CALAND.r

# This is the carbon accounting model for CA

# CAlifornia natural and working LANDs carbon and greenhouse gas model

# Inputs
# carbon_input.xls
#	The initial carbon density, carbon fluxes, and management/fire/conversion carbon adjustments
# <scenario_name>.xls
#	The initial land area, managed area, fire area, and annual changes to these areas; also the annual mortality rates and
# climate effect parameters for vegetation and soil
#	Name the file something appropriate
# These files have matching formats, including the number of preceding rows, the land types, the ownership, the management, the fire

# Outputs
# <scenario_name>_output_###.xls
# "_output_" is appended to the input scenario name, then a tag to denote which input values were used (e.g., the default ### = "mean")
# output precision is to the integer (for ha and Mg C and their ratios)

# This model follows basic density (stock) and flow guidelines similar to IPCC protocols
# The initial year area data are used for the first year for carbon operations
# The area data for each year are operated on by the carbon (density & flux) adjustments for climate, management, fires, and land conversion 
# within that year
#	Order of operations (Each subsequent step uses the updated carbon values from the previous step): 
# 1) climate effects are applied to input soil and vegetation carbon fluxes 
# 2) management effects are applied to climate-adjusted carbon fluxes resulting in an area-weighted eco C flux 
# 3) fire effects are applied to area-weighted eco C flux 
# 4) land conversion area changes are applied after the carbon operations, and their effects on carbon fluxes are assigned to that same year
# 5) new carbon density and area are assigned to the beginning of next year

# Output positive flux values are land uptake; negative values are losses to the atmosphere

# all wood products are lumped together and labeled as "wood"

# This R script is in the caland directory, which should be the working directory
#	Open R by opening this R script, or set the working the directory to caland/
# setwd("<your_path>/caland/")

# The input excel files are in caland/inputs/ (unless <indir> is specified as different from the default of "")
# Output excel file is written to caland/outputs/<outdir> (unless <outdir> is specified as different from the default of "")

# CALAND is now a function!
# 14  arguments (see function definition for default values):
#	scen_file_arg		name of the scenario file; assumed to be in caland/inptus/<indir>
#	c_file_arg			name of the carbon parameter input file; assumed to be in caland/inputs/<outdir>
#	indir				name only of directory in caland/inputs/ that contains scen_file and c_file; do not include "/" character at the end
#	outdir				name only of directory in caland/outputs/ that contains scen_file and c_file; do not include "/" character at the end
#	start_year		  	simulation begins at the beginning of this year
#	end_year		    simulation ends at the beginning of this year (so the simulation goes through the end of end_year - 1)
#	value_col_dens		select which carbon density values to use; 5 = min, 6 = max, 7 = mean, 8 = std dev
# 	value_col_accum 	select which carbon accumulation values to use; 5 = min, 6 = max, 7 = mean, 8 = std dev
# value_col_soilcon select which carbon accumulation values to use for cultivated soil conservation; 6 = min, 7 = max, 8 = mean, 9 = std dev
#	ADD_dens			for use with value_col_dens ==8: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
#	ADD_accum			for use with value_col_accum ==8: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
# ADD_soilcon   for use with value_col_soilcon ==9: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
#	NR_Dist			for adjusting the amount of non-regenerating forest after high severity fire (-1 = full regeneration, 120m is the minimum)
#	WRITE_OUT_FILE	TRUE= write the output file; FALSE= do not write the output file

# notes:
# carbon calcs occur in start_year up to end_year-1
# end_year denotes the final area after the changes in end_year-1
# density values are the stats of the total pixel population within each land type id
# accumulation values are stats of literature values

# How to use CALAND for beginners in R:
# (1) Set working directory to the location of CALAND folder by selecting "Set Working Directory" and "Choose Directory"
# from the Session menu.
# Or type setwd("<your_path>/caland/") pn the R command line
# (2) Load the libraries and all 3 functions (CALC.GWP, GET.NAMES, CALAND) by selecting "Source Document" from the Edit menu
#	Or type source("CALAND.R") on the R command line
#	Or by highlighting everything below and selecting clicking "Run," if that button exists
# (3) In command line, enter CALAND([define arguments here]). At a minimum you will need to define
# the scen_file (e.g. CALAND(scen_file = "Baseline_frst2Xmort_fire.xls")).

# this enables java to use up to 4GB of memory for reading and writing excel files
options(java.parameters = "-Xmx4g" )

# Load all the required packages
 libs <- c( "XLConnect" )
 for( i in libs ) {
   if( !require( i, character.only=T ) ) {
     cat( "Couldn't load", i, "\n" )
     stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it 
           for local user if you do not have root access\n" )
   }
   library( i, character.only=T )
}

#####################################################################################################################

# CALC.GWP (1) finds out whether each table in df.list is CO2, CH4, or BCC 
# by reading the name of the table (i.e. element name in list)
# and (2) converts gas columns to GWP accordingly

CALC.GWP <- function(df, gwp_CO2, gwp_CH4, gwp_BC) {      
  # get name of list of data frames 
  name <- names(df)
  new.df <- df
  for (l in 1:length(name)) { #loop over each data frame
    #loop over each data frame and get the unique number of columns (different for annual and cumulative)
    # loop over the C emissions columns
    for (i in 5:ncol(df[[l]])) { 
      # if not BCC and not CH4C
      if ((substr(name[[l]], nchar(name[[l]])-2, nchar(name[[l]]))) != "BCC" & 
          (substr(name[[l]], nchar(name[[l]])-2, nchar(name[[l]]))) != "H4C") {
        # then it's CO2C, so covert C to CO2 according to molar mass ratio 
        new.df[[l]][,i] <- df[[l]][,i] * (44.01/12.0107) * gwp_CO2 
        # otherwise if it's CH4C
      } else { if (substr(name[[l]], nchar(name[[l]])-2, nchar(name[[l]])) == "H4C") {
        # convert CH4-C to [Mg CO2-eq/ha/y]
        new.df[[l]][,i] <- df[[l]][,i] * (16.04/12.0107) * gwp_CH4
        # otherwise it's BCC, so convert to [Mg CO2-eq/ha/y]
        # multiplying by 1/0.6 based on assumption that 60% black C is C.
      } else { new.df[[l]][,i] <- df[[l]][,i] * (1/0.6) * gwp_BC } 
      }
    }
  }
  return(new.df)
}

# define function GET.NAMES that produces a list of new names that drops the last 'C' in CO2C, CH4C, and BCC 
# and adds 'eq' for CH4 and BC  
GET.NAMES <- function(df, new.name) {
  name <- names(df)
  for (i in 1:length(name)) {
    if ((substr(name[[i]], nchar(name[[i]])-2, nchar(name[[i]]))) == "BCC" | (substr(name[[i]], nchar(name[[i]])-2, nchar(name[[i]]))) == "H4C") {
      new.name[[i]] <- substr(name[[i]], 1, nchar(name[[i]]) - 1)
      new.name[[i]] <- paste0(new.name[[i]], "eq")
    } else { new.name[[i]] <- substr(name[[i]], 1, nchar(name[[i]]) - 1) }
  }
  return(new.name)
}

# set the default arguments here for debugging purposes
#scen_file_arg = "Baseline_frst2Xmort_fire.xls"
#scen_file_arg = "BaseProtect_HighManage_frst2Xmort_fire.xls"
scen_file_arg = "BAU_EcoFlux_frst2Xmort_fire.xls"
c_file_arg = "carbon_input.xls"
indir = ""
outdir = ""
start_year = 2010
end_year = 2051
#mean
value_col_dens = 7
ADD_dens = TRUE
#mean
value_col_accum = 7
ADD_accum = TRUE
#mean
value_col_soilcon = 8
ADD_soilcon = TRUE
NR_Dist = -1
WRITE_OUT_FILE = TRUE


CALAND <- function(scen_file_arg, c_file_arg = "carbon_input.xls", indir = "", outdir = "", start_year = 2010, end_year = 2051, value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, value_col_soilcon=8, ADD_soilcon = TRUE, NR_Dist = -1, WRITE_OUT_FILE = TRUE) {
  cat("Start CALAND at", date(), "\n")
  
  # output label for: value_col and ADD select which carbon density and accumulation values to use; see notes above
  ftag = c("", "", "", "", "min", "max", "mean", "sd")
  # for soil cons flux values
  ftag_soilcon = c("", "", "", "", "", "min", "max", "mean", "sd")
  
  inputdir = paste0("inputs/", indir)
  if(substr(inputdir,nchar(inputdir), nchar(inputdir)) != "/") { inputdir = paste0(inputdir, "/") }
  outputdir = paste0("outputs/", outdir)
  if(substr(outputdir,nchar(outputdir), nchar(outputdir)) != "/") { outputdir = paste0(outputdir, "/") }
  dir.create(outputdir, recursive=TRUE)
  
  # get scenario name as file name without extension
  scen_name = substr(scen_file_arg, 1, nchar(scen_file_arg) - 4)
  # add the directory to the scen_file_arg name and c_file_arg name
  scen_file = paste0(inputdir, scen_file_arg)
  c_file = paste0(inputdir, c_file_arg)
  
  # the start row of the tables is the same for all sheets
  start_row = 12
  
  # Several assumptions are contained between this line down to the output table lines
  # They shouldn't need to be changed for differenct scenarios, but they would be useful for testing sensitivity of the model
  # below the output tables and before the library load lines are names specific to columns in the input xls file
  
  # total area change threshold for round off error
  tot_area_change_thresh = 1.0/1000000.0
  
  # this is used only for forest understory mortality
  # default mortality is 1%
  default_mort_frac = 0.01
  
  # this is the default fration of root carbon to above ground veg carbon
  default_below2above_frac = 0.20
  
  # default fraction of understory to above main c
  # this is for types not listed here: forest, savanna, woodland
  default_under_frac = 0.10
  
  # default fractions of dead material to assign to dead pools
  # this is for types not listed here: forest, savanna, woodland
  default_standdead_frac = 0.11
  default_downdead_frac = 0.23
  default_litter_frac = 0.66
  
  # forest component biomass fractions from jenkins et al. 2003
  leaffrac = 0.05
  barkfrac = 0.12
  branchfrac = 0.17
  stemfrac = 0.66
  
  # average half-life for all CA wood products (years) (stewart and nakamura 2012)
  wp_half_life = 52
  # weighted average CH4 correction factor for anerobic decomposition in landfills (IPCC waste model)
  MCF = 0.71
  # default CH4 gas fraction in landfills (ARB 2016 GHG inventoty technical support)
  landfill_gas_frac = 0.5
  # default CH4 collection efficiency in landfills (ARB 2016 GHG inventoty technical support)
  CE = 0.75
  # default CH4 destruction efficiency via C filter in landfills (ARB 2016 GHG inventoty technical support)
  DE_filter = 0.01
  # default CH4 collection efficiency in landfills via combustion/oxidation in landfills (ARB 2016 GHG inventoty technical support)
  DE_combust = 0.99
  # default CH4 oxidation factor in landfill cover (ARB 2016 GHG inventoty technical support)
  OX = 0.1
  
  #### set the number of years an ecosystem exchange benefit occurs due to management
  
  # rangeland depends on the repeat period for the defined compost amendments
  range_lowfreq_period = 30
  range_medfreq_period = 10
  
  # forest
  # vegetation studies indicate that carbon benefits of managment can continue for at least 35 years
  # use 20 years here, as suggested by the TAC; also note that the current input data are for a 10-year estimate
  # soil measurements indicate that benefits are not significant after 10 years, and are gone by 30 years
  #  thus the input data currenlty show no soil c benefits
  # this is implicit for the fire severity reduction due to forest management because it is based on the benefited cumulative manage area
  forest_benefit_period = 20

  # urban forest
  # use the same value as forest
  urban_forest_benefit_period = 20

  # fire decay
  # temporal decay of newly dead material due to fire
  # bole and branch are set to recommended branch decay rate value from harmon et al 1987 to ensure more realistic rapid loss
  #  for decay rate = 0.09, in 10 years 59% of the material has decayed
  # also, stand dead may include varied fallen material in this case; using these two pools because the transfer pathways already exist
  # allow decay for 50 years to vent 99% of material
  fire_bole_decay_rate = 0.09
  fire_branch_decay_rate = 0.09
  fire_decay_years = c(1:50)
  standdead_decay_frac = c(1,exp(-fire_bole_decay_rate * fire_decay_years)[fire_decay_years[-length(fire_decay_years)]]) - exp(-fire_bole_decay_rate * fire_decay_years)
  downdead_decay_frac = c(1,exp(-fire_branch_decay_rate * fire_decay_years)[fire_decay_years[-length(fire_decay_years)]]) - exp(-fire_branch_decay_rate * fire_decay_years)
  
  ######### Determine output file names based on arguments in CALAND() that specify which input statistics are used for c density ######### 
  ############################ and c accumulation (baseline and cultivated soil conservation) ############################################
  
  # first check to make sure that the val cols are valid (min, max, mean, std dev)
  #		so the numbers have to be 5 - 8 for dens or 6 - 9 for soil conservation
  if ( value_col_dens < 5 | value_col_dens > 8 | value_col_accum < 5 | value_col_accum > 8 | value_col_soilcon < 6 | value_col_soilcon > 9) {
  	cat( "Invalid value column for value_col_dens, value_col_accum, or value_col_soilcon\n" )
    stop( "Please make sure that all of these arguemnts are one of the following:\nmin (5), max (6), mean (7), std_dev (8)\n" )
  }
  
  # if c density and c accumulation use _same_ input statisitic: they're both either min (5), max (6), mean (7), std_dev (8)
  if (value_col_dens == value_col_accum) {
    # same but not std_dev
    if (value_col_dens != 8) {
    # out_file is labeled by the statisitic without specifying density or accumulation
      out_file = paste0(outputdir, scen_name, "_output_", ftag[value_col_dens], ".xls")
      # otherwise they are both std_dev so attach "add" or "sub"
      } else { 
          # same sign
          if(ADD_dens == ADD_accum) {
            # both std_dev add
            if (ADD_dens) { 
              out_file = paste0(outputdir, scen_name, "_output_" , ftag[value_col_dens], "_add", ".xls")
            } else { 
              # both std_dev subtract
                out_file = paste0(outputdir, scen_name, "_output_" , ftag[value_col_dens], "_sub", ".xls") }
            # otherwise both std_dev but different sign
          } else { 
          # dens add and accum subtract
              if (ADD_dens) {
                out_file = paste0(outputdir, scen_name, "_output_" , ftag[value_col_dens], "_add_" , "dens_", ftag[value_col_accum], 
                                  "_sub_", "accum", ".xls")
              } else { 
                  # dens sub and accum add
                  out_file = paste0(outputdir, scen_name, "_output_" , ftag[value_col_dens], "_sub_" , "dens_", ftag[value_col_accum], 
                                    "_add_", "accum", ".xls") 
              } # end else sub
          } # end else not same sign
      } # end else value_col_dens == 8 (std_dev)
  # otherwise they are _not_ same statisitic
  } else { 
    # first case: neither is std_dev
    if (!(value_col_dens != 8 & value_col_accum == 8) & !(value_col_dens == 8 & value_col_accum != 8)) {
      out_file = paste0(outputdir, scen_name, "_output_" , ftag[value_col_dens], "_dens_", ftag[value_col_accum], "_accum", ".xls")
      # second case, one is std_dev and accum is something else
    } else {
      # std_dev dens and accum something else
      if (value_col_dens == 8) {
        # std_dev add dens
        if (ADD_dens) { 
          out_file = paste0(outputdir, scen_name, "_output_", ftag[value_col_dens], "_add_" , "dens_", ftag[value_col_accum], 
                            "_accum", ".xls")
          # std_dev sub dens
        } else { 
          out_file = paste0(outputdir, scen_name, "_output_", ftag[value_col_dens], "_sub_" , "dens_", ftag[value_col_accum], 
                            "_accum", ".xls")
        }
          # third case, std_dev accum and dens something else  
      } else { 
          if (value_col_accum == 8) { 
            # label them each 
            # std_dev add accum
            if (ADD_accum) { 
              out_file = paste0(outputdir, scen_name, "_output_", ftag[value_col_dens], "_dens_", ftag[value_col_accum], "_add_" ,
                                "accum", ".xls")
            # std_dev sub accum
            } else { 
              out_file = paste0(outputdir, scen_name, "_output_", ftag[value_col_dens], "_dens_", ftag[value_col_accum], "_sub_", 
                                "accum", ".xls")
            } # end else sub accum
          } # end if accum is std dev
      } # end else accum is std dev
    } # end else one val col is 8 (std_dev)
  } # end else dens and accum have different stat
  
  # paste extended name to output file to indicate which statistic is used for soil conservation flux
  if (ftag[value_col_dens] == "mean" & ftag[value_col_accum] == "mean" & ftag_soilcon[value_col_soilcon] == "mean") {
    # do nothing, filename stays the same: outputs/[scen_name]_output_mean.xls
  } else {
    # otherwise subtract ".xls" and replace with [mean, add, or sub]_soilcon.xls
    delete.xls <- ".xls"
    if (value_col_soilcon==8) {
      # mean soil conservation
      out_file = paste0(substr(out_file,1,nchar(out_file)-4), "_mean_soilcon.xls")
    } else {
      if (ADD_soilcon) {
        # plus sd soil conservation
        out_file = paste0(substr(out_file,1,nchar(out_file)-4), "_add_soilcon.xls")
      } else {
        # sub sd soil conservation
        out_file = paste0(substr(out_file,1,nchar(out_file)-4), "_sub_soilcon.xls")
      }
    }
  }
  
  #########################################################################################################################################
  
  # assign 100 yr global warming potential of CO2, CH4, and black C (BC)
  gwp_CO2 <- 1
  gwp_CH4 <- 25
  gwp_BC <- 900
  
  # assign fractions of soil c accumulation that is CO2-C and CH4-C in fresh marsh 
  marsh_CO2_C_frac <- -1.14
  marsh_CH4_C_frac <- 0.14
  
  # assign emissions fractions of total C emissions for all fires (Jenkins et al 1996)
  CO2C_fire_frac <- 0.9952
  CH4C_fire_frac <- 0.0021
  BCC_fire_frac <- 0.0027
  
  # assign emissions fractions of total C emissions for all bioenergy (Dabdub et al 2015)
  CO2C_energy_frac <- 0.9994
  CH4C_energy_frac <- 0.0001
  BCC_energy_frac <- 0.0005
  
  ####### assign burned fraction of 2Atmos fluxes due to management activities #######
  
  # do lit search regarding slash burning in logging and thinning practices to get fractions below:
  # forest clearcut and above-main removed to atmos
  # clrcut_mainremoved_burn <- 0.25
  # forest partial_cut and above-main removed  to atmos
  # parcut_mainremoved_burn <- 0.25
  # forest fuel_reduction and above-main removed  to atmos
  # fuelred_mainremoved_burn <- 0.25
  # forest Prescribed_burn and above-main removed  to atmos (currently above2atmos is 0; only understory, litter and down dead go to atmos)
  # prescrburn_mainremoved_burn <- 1
  # forest Weed_treatment and above-main removed to atmos
  # weedtrt_mainremoved_burn <- 0.25
  # forest clearcut and understory to atmos
  # clrcut_under_burn <- 0.25
  # forest partial_cut and understory to atmos
  # parcut_under_burn <- 0.25
  # forest fuel_reduction and understory to atmos
  # fuelred_under_burn <- 0.25
  # forest Prescribed_burn and understory to atmos
  # prescrburn_under_burn <- 1
  # forest weed_treatment and understory removed to atmos
  # weedtrt_under_burn <- 0.25
  # forest clearcut and down dead to atmos
  # clrcut_down_burn <- 0.25
  # forest partial_cut and down dead to atmos
  # parcut_down_burn <- 0.25
  # forest fuel_reduction and down dead to atmos
  #fuelred_down_burn <- 0.25
  # forest Prescribed_burn and down dead to atmos
  # prescrburn_down_burn <- 1
  # forest Weed_treatment and down dead removed to atmos
  # weedtrt_down_burn <- 0.25
  # forest clearcut and litter to atmos
  # clrcut_litter_burn <- 0.25
  # forest partial_cut and litter to atmos
  # parcut_litter_burn <- 0.25
  # forest fuel_reduction and litter to atmos
  # fuelred_litter_burn <- 0.25
  # forest Prescribed_burn and litter to atmos
  # prescrburn_litter_burn <- 1
  # forest weed_treatment and litter removed to atmos
  # weedtrt_litter_burn <- 0.25 
  
  # output tables
  out_area_sheets = c("Area", "Managed_area", "Wildfire_area")
  num_out_area_sheets = length(out_area_sheets)
  out_density_sheets = c("All_orgC_den", "All_biomass_C_den", "Above_main_C_den", "Below_main_C_den", "Understory_C_den", "StandDead_C_den", 
                         "DownDead_C_den", "Litter_C_den", "Soil_orgC_den")
  num_out_density_sheets = length(out_density_sheets)
  out_stock_sheets = c("All_orgC_stock", "All_biomass_C_stock", "Above_main_C_stock", "Below_main_C_stock", "Understory_C_stock", 
                       "StandDead_C_stock", "DownDead_C_stock", "Litter_C_stock", "Soil_orgC_stock")
  num_out_stock_sheets = length(out_stock_sheets)
  out_atmos_sheets = c("Eco_CumGain_C_stock", "Total_Atmos_CumGain_C_stock", "Manage_Atmos_CumGain_C_stock", "Fire_Atmos_CumGain_C_stock", 
                       "LCC_Atmos_CumGain_C_stock", "Wood_Atmos_CumGain_C_stock", "Total_Energy2Atmos_C_stock", "Eco_AnnGain_C_stock", 
                       "Total_Atmos_AnnGain_C_stock", "Manage_Atmos_AnnGain_C_stock", "Fire_Atmos_AnnGain_C_stock", 
                       "LCC_Atmos_AnnGain_C_stock", "Wood_Atmos_AnnGain_C_stock", "Total_AnnEnergy2Atmos_C_stock", 
                       "Manage_Atmos_CumGain_FireC", "Manage_Atmos_CumGain_TotEnergyC", "Man_Atmos_CumGain_Harv2EnergyC",
                       "Man_Atmos_CumGain_Slash2EnergyC", "Manage_Atmos_CumGain_NonBurnedC","Fire_Atmos_CumGain_BurnedC", 
                       "Fire_Atmos_CumGain_NonBurnedC", "LCC_Atmos_CumGain_FireC", "LCC_Atmos_CumGain_TotEnergyC", 
                       "LCC_Atmos_CumGain_Harv2EnergyC", "LCC_Atmos_CumGain_Slash2EnergyC", "LCC_Atmos_CumGain_NonBurnedC", 
                       "Manage_Atmos_AnnGain_FireC", "Manage_Atmos_AnnGain_TotEnergyC", "Man_Atmos_AnnGain_Harv2EnergyC",
                       "Man_Atmos_AnnGain_Slash2EnergyC", "Manage_Atmos_AnnGain_NonBurnedC", "Fire_Atmos_AnnGain_BurnedC", 
                       "Fire_Atmos_AnnGain_NonBurnedC", "LCC_Atmos_AnnGain_FireC", "LCC_Atmos_AnnGain_TotEnergyC", 
                       "LCC_Atmos_AnnGain_Harv2EnergyC", "LCC_Atmos_AnnGain_Slash2EnergyC", "LCC_Atmos_AnnGain_NonBurnedC")
  num_out_atmos_sheets = length(out_atmos_sheets)
  out_wood_sheets = c("Total_Wood_C_stock", "Total_Wood_CumGain_C_stock", "Total_Wood_CumLoss_C_stock", "Total_Wood_AnnGain_C_stock", 
                      "Total_Wood_AnnLoss_C_stock", "Manage_Wood_C_stock", "Manage_Wood_CumGain_C_stock", "Man_Harv2Wood_CumGain_C_stock",
                      "Man_Slash2Wood_CumGain_C_stock", "Manage_Wood_CumLoss_C_stock", "Manage_Wood_AnnGain_C_stock", 
                      "Man_Harv2Wood_AnnGain_C_stock",  "Man_Slash2Wood_AnnGain_C_stock", "Manage_Wood_AnnLoss_C_stock", 
                      "LCC_Wood_C_stock", "LCC_Wood_CumGain_C_stock", "LCC_Harv2Wood_CumGain_C_stock", "LCC_Slash2Wood_CumGain_C_stock",
                      "LCC_Wood_CumLoss_C_stock", "LCC_Wood_AnnGain_C_stock", "LCC_Harv2Wood_AnnGain_C_stock", "LCC_Slash2Wood_AnnGain_C_stock",
                      "LCC_Wood_AnnLoss_C_stock")
  num_out_wood_sheets = length(out_wood_sheets)
  
  # column names from the management table to calculate non-accum manage carbon adjustments
  man_frac_names = c("Above_harvested_frac", "StandDead_harvested_frac", "Harvested2Wood_frac", "Harvested2Energy_frac", "Harvested2SawmillDecay_frac", 
                     "Harvested2Slash_frac", "Under2Slash_frac", "DownDead2Slash_frac", "Litter2Slash_frac", "Slash2Energy_frac", "Slash2Wood_frac",
                     "Slash2Burn_frac", "Slash2Decay_frac", "Under2DownDead_frac", "Soil2Atmos_frac", "Above2StandDead_frac", "Below2Atmos_frac", "Below2Soil_frac")
  #man_frac_names = c("Above_removed_frac", "StandDead_removed_frac", "Removed2Wood_frac", "Removed2Energy_frac", "Removed2Atmos_frac", 
  #                   "Understory2Atmos_frac", "DownDead2Atmos_frac", "Litter2Atmos_frac", "Soil2Atmos_frac", "Understory2DownDead_frac", 
  #                   "Above2StandDead_frac", "Below2Atmos_frac", "Below2Soil_frac")

  num_manfrac_cols = length(man_frac_names)
  # new c trans column names matching the non-accum manage frac names
  c_trans_names = c("Above_harvested_c", "StandDead_harvested_c", "Harvested2Wood_c", "Harvested2Energy_c", "Harvested2SawmillDecay_c", "Harvested2Slash_c",
                    "Under2Slash_c", "DownDead2Slash_c", "Litter2Slash_c", "Slash2Energy_c", "Slash2Wood_c", "Slash2Burn_c", "Slash2Decay_c", "Under2DownDead_c", 
                    "Soil2Atmos_c", "Above2StandDead_c", "Below2Atmos_c", "Below2Soil_c")
  #c_trans_names = c("Above_removed_c", "StandDead_removed_c", "Removed2Wood_c", "Removed2Energy_c", "Removed2Atmos_c", "Understory2Atmos_c", 
  #                  "DownDead2Atmos_c", "Litter2Atmos_c", "Soil2Atmos_c", "Understory2DownDead_c", "Above2StandDead_c", "Below2Atmos_c", 
  #                 "Below2Soil_c")
  # indices of the appropriate density source df for the non-accum manage frac to c calcs; corresponds with out_density_sheets above
  # value == -1 indicates that the source is the harvested c; take the sum of the first two c trans columns
  # value == -2 indicates that the source is all slash-contributing pools; take the sum of c trans columns 6, 7 and 8 ("Under2Slash_c", "DownDead2Slash_c", "Litter2Slash_c")
  manage_density_inds = c(3, 6, -1, -1, -1, -1, 5, 7, 8, -2, -2, -2, -2, 5, 9, 3, 4, 4)
  # manage_density_inds = c(3, 6, -1, -1, -1, 5, 7, 8, 9, 5, 3, 4, 4)
  #manage_density_inds = c(3 (above), 6 (standdead), -1, -1, -1, 5 (under), 7 (down), 8 (litter), -2 (slash2energy), -2 (slash2wood), -2 (slash2burn), -2 (slash2decay), 5 (under2down),
                              # 9 (soil), 3 (above2stand), 4 (below), 4 (below))
  # out_density_sheets = c("All_orgC_den" (1), "All_biomass_C_den" (2), "Above_main_C_den" (3), "Below_main_C_den" (4), "Understory_C_den" (5), "StandDead_C_den" (6), 
  #                       "DownDead_C_den" (7), "Litter_C_den" (8), "Soil_orgC_den" (9))
  
  # column names from the fire table to calculate fire carbon adjustments
  fire_frac_names = c("Above2Atmos_frac", "StandDead2Atmos_frac", "Understory2Atmos_frac", "DownDead2Atmos_frac", "Litter2Atmos_frac", 
                      "Above2StandDead_frac", "Understory2DownDead_frac", "Below2Atmos_frac", "Soil2Atmos_frac")
  num_firefrac_cols = length(fire_frac_names)
  # new c trans column names matching the fire frac names
  firec_trans_names = c("Above2Atmos_c", "StandDead2Atmos_c", "Understory2Atmos_c", "DownDead2Atmos_c", "Litter2Atmos_c", "Above2StandDead_c", 
                        "Understory2DownDead_c", "Below2Atmos_c", "Soil2Atmos_c")
  # indices of the appropriate density source df for the fire frac to c calcs; corresponds with out_density_sheets above
  fire_density_inds = c(3, 6, 5, 7, 8, 3, 5, 4, 9)
  
  # column names from the conversion to ag/urban table to calculate conversion to ag/urban carbon adjustments
  conv_frac_names = c("Above_harvested_conv_frac", "StandDead_harvested_conv_frac", "Harvested2Wood_conv_frac", "Harvested2Energy_conv_frac", 
                      "Harvested2SawmillDecay_conv_frac", "Harvested2Slash_conv_frac", "Under2Slash_conv_frac", "DownDead2Slash_conv_frac", 
                      "Litter2Slash_conv_frac", "Slash2Energy_conv_frac", "Slash2Wood_conv_frac", "Slash2Burn_conv_frac", "Slash2Decay_conv_frac", 
                      "Under2DownDead_conv_frac", "Soil2Atmos_conv_frac", "Below2Atmos_conv_frac", "Below2Soil_conv_frac")
  num_convfrac_cols = length(conv_frac_names)
  # new c trans column names matching the conversion frac names
  convc_trans_names = c("Above_harvested_conv_c", "StandDead_harvested_conv_c", "Harvested2Wood_conv_c", "Harvested2Energy_conv_c", 
                        "Harvested2SawmillDecay_conv_c", "Harvested2Slash_conv_c", "Under2Slash_conv_c", "DownDead2Slash_conv_c", 
                        "Litter2Slash_conv_c", "Slash2Energy_conv_c", "Slash2Wood_conv_c", "Slash2Burn_conv_c", "Slash2Decay_conv_c", 
                        "Under2DownDead_conv_c", "Soil2Atmos_conv_c", "Below2Atmos_conv_c", "Below2Soil_conv_c")
  # indices of the appropriate density source df for the conversion frac to c calcs; corresponds with out_density_sheets above
  # value == -1 indicates that the source is the removed c; take the sum of the first two c trans columns
  conv_density_inds = c(3, 6, -1, -1, -1, -1, 5, 7, 8, -2, -2, -2, -2, 5, 9, 4, 4)
  
  # Load the input files
  c_wrkbk = loadWorkbook(c_file)
  scen_wrkbk = loadWorkbook(scen_file)
  
  # worksheet/table names
  c_sheets = getSheets(c_wrkbk)
  num_c_sheets = length(c_sheets)
  scen_sheets = getSheets(scen_wrkbk)
  num_scen_sheets = length(scen_sheets)
  
  # NA values need to be converted to numeric
  # the warnings thrown by readWorksheet below are ok because they just state that the NA string can't be converted a number so it is 
  # converted to NA value
  c_col_types1 = c("numeric", "character", "character", "character", rep("numeric",100))
  c_col_types2 = c("numeric", "character", "character", "character", "character", rep("numeric",100))
  c_col_types3 = c("character", rep("numeric",100))
  
  # Load the worksheets into a list of data frames
  c_df_list <- list()
  scen_df_list <- list()
  for (i in 1:12) { # through conversion2ag_urban
    c_df_list[[i]] <- readWorksheet(c_wrkbk, i, startRow = start_row, colTypes = c_col_types1, forceConversion = TRUE)
  }
  for (i in 13:16) { # forest_manage to ag_manage
    c_df_list[[i]] <- readWorksheet(c_wrkbk, i, startRow = start_row, colTypes = c_col_types2, forceConversion = TRUE)
  }
  for (i in 17:17) { # wildfire
    c_df_list[[i]] <- readWorksheet(c_wrkbk, i, startRow = start_row, colTypes = c_col_types3, forceConversion = TRUE)
  }
  for (i in 1:2) { # annual change and initial area
    scen_df_list[[i]] <- readWorksheet(scen_wrkbk, i, startRow = start_row, colTypes = c_col_types1, forceConversion = TRUE)
  }
  for (i in 3:4) { # annual managed area and annual wildfire area
    scen_df_list[[i]] <- readWorksheet(scen_wrkbk, i, startRow = start_row, colTypes = c_col_types2, forceConversion = TRUE)
  }
  
  # Check that all management areas in scen_df_list[[3]] have corresponding initial 2010 areas scen_df_list[[1]] 
  # get all category ID's for the managed areas and initial areas
  cat_ID_man <- scen_df_list[[3]][,1]
  cat_ID_exist <- scen_df_list[[1]][,1]
  if (any(!((cat_ID_man) %in% cat_ID_exist))) {
    stop("Error: each land category in the management area table must exist in the initial area table")
  }
  
  # Check that all slash2..._frac add to 1
  columns_1_check <- c_df_list[[12]][,c("Slash2Energy_conv_frac", "Slash2Burn_conv_frac", "Slash2Wood_conv_frac", "Slash2Decay_conv_frac")]
  sums <- rowSums(columns_1_check)
  if (any(((sums)!= 1))) {
    stop("Error: sum of slash pathways (frac) for each land category in the conversion2ag_urban table must equal 1")
  }
  columns_2_check <- c_df_list[[13]][,c("Slash2Energy_frac", "Slash2Burn_frac", "Slash2Wood_frac", "Slash2Decay_frac")]
  sums <- rowSums(columns_2_check)
  if (any((sums!= 1) & (sums!= 0))) {
    stop("Error: sum of slash pathways (frac) for each land category in the forest_manage table must equal 1 or 0")
  }
  columns_3_check <- c_df_list[[14]][,c("Slash2Energy_frac", "Slash2Burn_frac", "Slash2Wood_frac", "Slash2Decay_frac")]
  sums <- rowSums(columns_3_check)
  if (any(((sums)!= 1) & ((sums)!= 0))) {
    stop("Error: sum of slash pathways (frac) for each land category in the dev_manage table must equal 1")
  }
  
  for (i in 5:5) { # annual mortality fraction
    scen_df_list[[i]] <- readWorksheet(scen_wrkbk, i, startRow = start_row, colTypes = c_col_types1, forceConversion = TRUE)
  }
  for (i in 6:7) { # annual climate effects on veg and soil C fluxes, respectively
    scen_df_list[[i]] <- readWorksheet(scen_wrkbk, i, startRow = start_row, colTypes = c_col_types1, forceConversion = TRUE)
  }
  names(c_df_list) <- c_sheets
  names(scen_df_list) <- scen_sheets
  
  # remove the Xs added to the front of the year columns, and get the years as numbers
    # first check if there are any prescribed management practices 
  if (nrow(scen_df_list[[3]])>0) {
  man_targetyear_labels = names(scen_df_list[[3]])[c(6:ncol(scen_df_list[[3]]))]
  man_targetyear_labels = substr(man_targetyear_labels,2,nchar(man_targetyear_labels[1]))
  names(scen_df_list[[3]])[c(6:ncol(scen_df_list[[3]]))] = man_targetyear_labels
  man_targetyears = as.integer(substr(man_targetyear_labels,1,4))
  }
  
  fire_targetyear_labels = names(scen_df_list[[4]])[c(6:ncol(scen_df_list[[4]]))]
  fire_targetyear_labels = substr(fire_targetyear_labels,2,nchar(fire_targetyear_labels[1]))
  names(scen_df_list[[4]])[c(6:ncol(scen_df_list[[4]]))] = fire_targetyear_labels
  fire_targetyears = as.integer(substr(fire_targetyear_labels,1,4))
  
  mortality_targetyear_labels = names(scen_df_list[[5]])[c(5:ncol(scen_df_list[[5]]))]
  mortality_targetyear_labels = substr(mortality_targetyear_labels,2,nchar(mortality_targetyear_labels[1]))
  names(scen_df_list[[5]])[c(5:ncol(scen_df_list[[5]]))] = mortality_targetyear_labels
  mortality_targetyears = as.integer(substr(mortality_targetyear_labels,1,4))
  
  # get some tables
  
  # these include all target years
  man_target_df <- scen_df_list[[3]]
  fire_target_df <- scen_df_list[[4]]
  mortality_target_df <- scen_df_list[[5]]
  climate_veg_df <- scen_df_list[[6]]
  climate_soil_df <- scen_df_list[[7]]
  
  # these are useful
  # assign the conversion area sheet from sceario file to conv_area_df
  conv_area_df = scen_df_list[[2]]
  names(conv_area_df)[ncol(conv_area_df)] = "base_area_change"
  vegc_uptake_df = c_df_list[[10]]
  vegc_uptake_df$vegc_uptake_val = vegc_uptake_df[,value_col_accum]
  deadc_frac_df = c_df_list[[3]][,c("Land_Cat_ID", "Region", "Land_Type", "Ownership")]
  soilc_accum_df = c_df_list[[11]]
  soilc_accum_df$soilc_accum_val = soilc_accum_df[,value_col_accum]
  conv_df = c_df_list[[12]]
  man_forest_df = c_df_list[[13]]
  forest_soilcaccumfrac_colind = which(names(man_forest_df) == "SoilCaccum_frac")
  man_dev_df = c_df_list[[14]]
  dev_soilcaccumfrac_colind = which(names(man_dev_df) == "SoilCaccum_frac")
  man_grass_df = c_df_list[[15]]
  man_ag_df = c_df_list[[16]]
  # assign the correct soil conservation column (mean=8, SD=9) to man_ag_df$soilc_accum_val_soilcon 
  man_ag_df$soilc_accum_val_soilcon = man_ag_df[,value_col_soilcon]
  fire_df = c_df_list[[17]]
  
  # merge deadc_frac_df and mortality_target_df because zero rows do not exist and allrows are needed 
  mortality_target_df = merge(deadc_frac_df, mortality_target_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
  mortality_target_df[,c(5:ncol(mortality_target_df))] <- apply(mortality_target_df[,c(5:ncol(mortality_target_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
  
  # get the correct values for the baseline c accum tables if value is std dev
  if(value_col_accum == 8) { # std dev as value
    if(ADD_accum) {
      vegc_uptake_df$vegc_uptake_val = vegc_uptake_df$vegc_uptake_val + vegc_uptake_df$Mean_Mg_ha_yr
      soilc_accum_df$soilc_accum_val = soilc_accum_df$soilc_accum_val + soilc_accum_df$Mean_Mg_ha_yr
    } else {
      vegc_uptake_df$vegc_uptake_val = vegc_uptake_df$Mean_Mg_ha_yr - vegc_uptake_df$vegc_uptake_val
      soilc_accum_df$soilc_accum_val = soilc_accum_df$Mean_Mg_ha_yr - soilc_accum_df$soilc_accum_val
    }
  }
  
  # get the correct values for the cutivated soil conservation soil c accum if value is std dev
  if(value_col_soilcon == 9) { # std dev as value
    if(ADD_soilcon) {
      man_ag_df$soilc_accum_val_soilcon = man_ag_df$soilc_accum_val_soilcon + man_ag_df$Mean_Mg_ha_yr
    } else {
      man_ag_df$soilc_accum_val_soilcon = man_ag_df$Mean_Mg_ha_yr - man_ag_df$soilc_accum_val_soilcon
    }
  }
  
  ### Create a dummy variable for the soil_c_flux_frac for cultivated lands so that:
  ### man_soilc_flux = man_frac * baseline_soilc_flux
  ### man_soilc_flux = (man_soilc_flux/baseline_soilc_flux) * baseline_soilc_flux
  ### Thus, for cultivated lands, man_frac = man_soilc_flux/baseline_soilc_flux
  
  # subset baseline c accum values and landcat ID's to be merged into man_ag_df
  df <- soilc_accum_df[,c("Land_Cat_ID","soilc_accum_val")]
  man_ag_df <- merge(man_ag_df, df, by="Land_Cat_ID")
  # calc the dummy cultivated soil c flux frac
  man_ag_df$SoilCaccum_frac <- man_ag_df$soilc_accum_val_soilcon/man_ag_df$soilc_accum_val
  
  # create lists of the output tables
  # change the NA value to zero for calculations
  out_area_df_list <- list()
  out_density_df_list <- list()
  out_atmos_df_list <- list()
  out_stock_df_list <- list()
  out_wood_df_list <- list()
  start_area_label = paste0(start_year, "_ha")
  end_area_label = paste0(end_year, "_ha")
  start_density_label = paste0(start_year, "_Mg_ha")
  end_density_label = paste0(end_year, "_Mg_ha")
  start_atmos_label = paste0(start_year, "_Mg")
  end_atmos_label = paste0(end_year, "_Mg")
  start_stock_label = paste0(start_year, "_Mg")
  end_stock_label = paste0(end_year, "_Mg")
  start_wood_label = paste0(start_year, "_Mg")
  end_wood_label = paste0(end_year, "_Mg")
  
  # area
  # Assign the initial 2010 area table to "Area" in out_area_df_list[[1]]
  out_area_df_list[[1]] <- scen_df_list[[1]]
  names(out_area_df_list[[1]])[ncol(out_area_df_list[[1]])] <- as.character(start_area_label)
  # Assign the target management areas table to "Managed_area" in out_area_df_list[[2]]
    # First check if there are any prescribed management practices
  if (nrow(scen_df_list[[3]])>0) {
    out_area_df_list[[2]] <- scen_df_list[[3]][,c(1:6)]
    names(out_area_df_list[[2]])[ncol(out_area_df_list[[2]])] <- as.character(start_area_label)
  } else {
    out_area_df_list[[2]] <- scen_df_list[[3]][,c(1:5)]
  }
  #the wildfire out area df is added at the end because it has the breakdown across land type ids
  for ( i in 1:(num_out_area_sheets-1)) {
    out_area_df_list[[i]][is.na(out_area_df_list[[i]])] <- 0.0
  }
  
  # c density
  # assign input c density values to out_density c_df_list(9 density sheets): 
  # totalC, totalbiomass, mainC, root, under, deadstand, deaddown, litter, SOC
  # note: missing values for totalC and totalbiomass is due to missing c pools for some of the land categories 
  for (i in 1:num_out_density_sheets) {
    # populate out_density_df_list with the first 4 columns from each of the 9 C density sheets and either mean or +/-stdev
    out_density_df_list[[i]] <- c_df_list[[i]][,c(1,2,3,4,value_col_dens)]
    # out_density_df_list now has 5 columns for each sheet/C pool (Land_Cat_ID, Region, Land_Type, Ownership, Mean or Stdev)
    names(out_density_df_list[[i]])[ncol(out_density_df_list[[i]])] <- as.character(start_density_label)
    if(value_col_dens == 8) { # if std dev as value
      # this will not be the same as the sum of the components, so update it later
      if(ADD_dens) {
        # if we want +stddev, then C_density = std_dev + mean
        out_density_df_list[[i]][,5] = out_density_df_list[[i]][,5] + c_df_list[[i]][,"Mean_Mg_ha"]
      } else {
        # if we want -stddev, then C_density = mean - stdev
        out_density_df_list[[i]][,5] = c_df_list[[i]][,"Mean_Mg_ha"] - out_density_df_list[[i]][,5]
      }
    }
    out_density_df_list[[i]][is.na(out_density_df_list[[i]])] <- 0.0
    out_density_df_list[[i]][,5] <- replace(out_density_df_list[[i]][,5], out_density_df_list[[i]][,5] < 0, 0.00)
  }
  names(out_density_df_list) <- out_density_sheets
  
  # sum the total c pool density
  out_density_df_list[[1]][, start_density_label] = 0  
  # sum the following for each corresponding row: 
  ## totalC = Above_main_C_den + Below_main_C_den + Understory_C_den + StandDead_C_den + DownDead_C_den + Litter_C_den + Soil_orgC_den
  for (i in 3:num_out_density_sheets) {
    out_density_df_list[[1]][, start_density_label] = out_density_df_list[[1]][, start_density_label] + out_density_df_list[[i]][, start_density_label] 
  }
  
  # sum the biomass c pool density (all non-decomposed veg material; i.e. all non-soil c)
  out_density_df_list[[2]][, start_density_label] = 0  
  # All_biomass_C = Above_main_C_den + Below_main_C_den + Understory_C_den + StandDead_C_den + DownDead_C_den + Litter_C_den 
  for (i in 3:(num_out_density_sheets-1)) {
    out_density_df_list[[2]][, start_density_label] = out_density_df_list[[2]][, start_density_label] + out_density_df_list[[i]][, start_density_label]
  }
  
  # c stock
  for (i in 1:num_out_stock_sheets) {
    out_stock_df_list[[i]] <- out_density_df_list[[1]]
    names(out_stock_df_list[[i]])[ncol(out_stock_df_list[[i]])] <- as.character(start_stock_label)
    out_stock_df_list[[i]][, start_stock_label] = out_density_df_list[[i]][, start_density_label] * out_area_df_list[[1]][,start_area_label]
  }
  names(out_stock_df_list) <- out_stock_sheets
  for ( i in 1:num_out_stock_sheets) {
    out_stock_df_list[[i]][is.na(out_stock_df_list[[i]])] <- 0.0
  }
  
  # c to atmosphere (and c from atmosphere to ecosystems)
  for (i in 1:num_out_atmos_sheets) {
    # fill all the (empty) dataframes in the out_atmos_df_list with the All_orgC_den dataframe. This arbitrary, as it's only needed 
    # to fill in the first 3 columns with Land_Cat_ID, Land_Type and Ownership.
    out_atmos_df_list[[i]] <- out_density_df_list[[1]]
    # assign the name of 'start_atmos_label' (i.e. "2010_Mg"),
    names(out_atmos_df_list[[i]])[ncol(out_atmos_df_list[[i]])] <- as.character(start_atmos_label)
    # and clear all values and replace with 0's 
    out_atmos_df_list[[i]][,ncol(out_atmos_df_list[[i]])] = 0.0
  }
  names(out_atmos_df_list) <- out_atmos_sheets
  for ( i in 1:num_out_atmos_sheets) {
    out_atmos_df_list[[i]][is.na(out_atmos_df_list[[i]])] <- 0.0
  }
  
  # wood c stock
  for (i in 1:num_out_wood_sheets) {
    out_wood_df_list[[i]] <- c_df_list[[1]][,c("Land_Cat_ID", "Region", "Land_Type", "Ownership")]
    out_wood_df_list[[i]][,start_wood_label] = 0.0
  }
  names(out_wood_df_list) <- out_wood_sheets
  
  # useful variables
  
  man_area_sum = out_area_df_list[[2]]
  # check if there are any prescribed management practices
  if (nrow(out_area_df_list[[2]])>0) {
    names(man_area_sum)[ncol(man_area_sum)] <- "man_area"
    man_area_sum$man_area_sum = 0.0
  } 
  
  # set sum neginds eco to 0
  out_cum_neginds_eco_tot <- 0
  
  #####################
  # loop over the years
  for (year in start_year:(end_year-1)) {
    
    cat("\nStarting year ", year, "...\n")
    
    cur_density_label = paste0(year, "_Mg_ha")
    next_density_label = paste0(year+1, "_Mg_ha")
    cur_wood_label = paste0(year, "_Mg")
    next_wood_label = paste0(year+1, "_Mg")
    cur_area_label = paste0(year, "_ha")
    next_area_label = paste0(year+1, "_ha")
    cur_stock_label = paste0(year, "_Mg")
    next_stock_label = paste0(year+1, "_Mg")
    cur_atmos_label = paste0(year, "_Mg")
    next_atmos_label = paste0(year+1, "_Mg")
    
    # Determine the area weighted average eco fluxes based on the amount of managed land
    # use the running sum of the managed area for forest and range amendment, up to the total available area
    #  this is because the management changes the flux for an extended period of time, especially if the management is repeated later
    #  rangeland amendment is repeated on 10, 30, or 100 year periods
    # but ag is annual, and developed has its own system of independent areas
    # restoration and Afforestation practices are not dependent on existing area, and are applied in land conversion
    #  Afforestation area should not be included in forest aggregate managed area and aggregate managed area sum
    # store the difference between the unmanaged and averaged with management eco fluxes, per ag, soil, and forest
    
    # this is the current year total area by category id
    # assign the first 4 columns (Land_Cat_ID, Region, Land_Type, Ownership) and the last column (2010_ha) of the out_area_df_list dataframe to 
    # 'tot_area_df'.
    tot_area_df = out_area_df_list[[1]][,c(1:4,ncol(out_area_df_list[[1]]))]
    names(tot_area_df)[names(tot_area_df) == cur_area_label] <- "tot_area"
    
    # check if there any prescibed management practices
    if (nrow(man_area_sum)>0) {
    # reset the man_area_sum df
    man_area_sum = man_area_sum[,1:7]
    
    # determine the managed areas for this year from target years
    # linear interpolation between target years
    # if the year is past the final target year than use the final target year
    
    # man_targetyears (baseline man_targetyears: 2010, 2020, 2021, 2050), otherwise: 2010, 2016, 2017, 2020, 2021, 2050
    # assign the index of the management years (1:4) that are on or before the current year loop to 'linds'
    linds = which(man_targetyears <= year)
    # assign the index of the management years that are on or later to current year in loop to 'hinds'.
    hinds = which(man_targetyears >= year)
    # assign the most recent target year to prev_targetyear
    prev_targetyear = max(man_targetyears[linds])
    # assign the next closest target year to next_targetyear
    next_targetyear = min(man_targetyears[hinds])
    # assign the index of the most previous target year to pind
    pind = which(man_targetyears == prev_targetyear)
    # assign the index of the next target year to nind
    nind = which(man_targetyears == next_targetyear)
    # assign the previous label ("2010_ha" "2020_ha" "2021_ha" "2050_ha")
    pcol = man_targetyear_labels[pind]
    # assign the next label ("2010_ha" "2020_ha" "2021_ha" "2050_ha")
    ncol = man_targetyear_labels[nind]
    
    # if the current year is on the last management target year or there are no more target years ahead,
    if (prev_targetyear == next_targetyear | length(hinds) == 0) {
      # assign the the previous/current label "2050_ha" to man_area column in man_area_sum df
      man_area_sum$man_area = man_target_df[,pcol]
    } else {
      # otherwise assign man_area = (previous/current target area) + (# years since last target) * 
                        # (difference in areas between target years)/(# years between target years)
      man_area_sum$man_area = man_target_df[,pcol] + (year - prev_targetyear) * (man_target_df[,ncol] - 
                                                                                   man_target_df[,pcol]) / (next_targetyear - prev_targetyear)
    }
    
    # check if any man_area <0 that are != Growth  ### use this later after checking all scenario files ###
    # man_area_sum$man_area[man_area_sum$man_area < 0 & man_area_sum$Management != Growth] <- 0
    
    # Assign 0 to any negative man_area with dead_removal or urban_forest
    man_area_sum$man_area[man_area_sum$man_area < 0 & (man_area_sum$Management == "Dead_removal" | man_area_sum$Management == "Urban_forest")] <- 0
    
    ######## New:  check for excess man_area before man_area_sum ######## 
    # Check that the sum of man_area is not > tot_area
    man_area_agg_pre = aggregate(man_area ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & 
                                                                           man_area_sum$Management != "Restoration",], FUN=sum)
    # update aggregated column name (man_area) to man_area_agg_pre
    names(man_area_agg_pre)[ncol(man_area_agg_pre)] <- "man_area_agg_pre"
    # merge df's man_area_sum & tot_area_df, and assign to man_area_sum dataframe (essentially, add additional 
    # tot_area column to man_area_sum)
    man_area_sum = merge(man_area_sum, tot_area_df, by = c("Land_Cat_ID", "Region", "Land_Type","Ownership"), all.x = TRUE)
    # merge man_area_sum & man_area_agg_pre dataframes by Land_Cat_ID
    man_area_sum = merge(man_area_sum, man_area_agg_pre, by = "Land_Cat_ID", all.x = TRUE)
    # replace NA's in the new man_area_agg_pre column with 0's 
    man_area_sum$man_area_agg_pre = replace(man_area_sum$man_area_agg_pre, is.na(man_area_sum$man_area_agg_pre), 0)
    # sort the man_area_sum df by land cat ID and management 
    man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    
    # (7) don't use (untrimmed) aggregated areas for Developed_all: replace man_area_agg_pre column with individual man_area 
    # (note: dead removal in Dev_all = tot_area; urban_forest = fraction of tot_area that's urban forest)
    man_area_sum$man_area_agg_pre[man_area_sum$Land_Type == "Developed_all"] = 
      man_area_sum$man_area[man_area_sum$Land_Type == "Developed_all"]
    # (8) assign 0's to aggregate (untrimmed) areas for afforestation and restoration 
    man_area_sum$man_area_agg_pre[man_area_sum$Management == "Afforestation" | man_area_sum$Management == "Restoration"] = 0
    
     # add new column "excess_area", which is equal to the aggregated areas minus total land type areas 
    man_area_sum$excess_area_pre = man_area_sum$man_area_agg_pre - man_area_sum$tot_area
    # if excess_area > 0, assign the index to excess_area_inds
    excess_area_pre_inds = which(man_area_sum$excess_area_pre > 0)
    # trim the manaagement areas: subtract out a proportional amount of any excess from each man_area
    man_area_sum$man_area[excess_area_pre_inds] = man_area_sum$man_area[excess_area_pre_inds] - 
      man_area_sum$excess_area[excess_area_pre_inds] * man_area_sum$man_area[excess_area_pre_inds] / 
      man_area_sum$man_area_agg_pre[excess_area_pre_inds]
    # replace NaN (not a number) values in man_area with 0's 
    man_area_sum$man_area = replace(man_area_sum$man_area, is.nan(man_area_sum$man_area), 0)
    # replace Inf values in man_area_sum with 0's 
    man_area_sum$man_area = replace(man_area_sum$man_area, man_area_sum$man_area == Inf, 0)
    # replace neg values in man_area_sum with 0's 
    man_area_sum$man_area[man_area_sum$Management != "Growth"] = replace(man_area_sum$man_area[man_area_sum$Management != "Growth"], man_area_sum$man_area[man_area_sum$Management != "Growth"] < 0, 0)   
    # (9) create a _trimmed_ aggregated (current year) areas dataframe (man_area_agg2): aggregate and sum man_area vector by Land_Cat_ID for all management activities 
    # _except_ afforestation and restoration areas
    man_area_agg2 = aggregate(man_area ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & man_area_sum$Management != "Restoration",], FUN=sum)
    # rename header of _trimmed_ aggregate (current year) areas to "man_area_agg" in man_area_agg2 dataframe
    names(man_area_agg2)[ncol(man_area_agg2)] <- "man_area_agg"
    # (10) add column of _trimmed_ aggregated current areas called "man_area_agg" (Developed_all = _untrimmed_ agg areas, & afforestation and restoration excluded)
    # merge man_area_sum & man_area_agg2 dataframes by Land_Cat_ID
    man_area_sum = merge(man_area_sum, man_area_agg2, by = "Land_Cat_ID", all.x =TRUE)
    # replace NA's in the man_area_agg column with 0's
    man_area_sum$man_area_agg = replace(man_area_sum$man_area_agg, is.na(man_area_sum$man_area_agg), 0)
    # sort man_area_sum by land cat ID, then management 
    man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    # (11) don't use trimmed aggregated current year areas for Developed_all: replace man_area_agg column with individual man_area 
    man_area_sum$man_area_agg[man_area_sum$Land_Type == "Developed_all"] = man_area_sum$man_area[man_area_sum$Land_Type == "Developed_all"]
    # (12) assign 0's to aggregate (untrimmed) areas for afforestation and restoration 
    man_area_sum$man_area_agg[man_area_sum$Management == "Afforestation" | man_area_sum$Management == "Restoration"] = 0
    
    ##### adjust afforestation and restoration man_area values to available area if necessary
    # these same prioritizations are used in the land conversion section
    
    # prioritize afforestation over meadow because meadow comes from more types
    # first get shrubland + grassland areas for each region-ownership, then take the min of man_area and avail area
    avail_ro_area = aggregate(tot_area ~ Region + Ownership, tot_area_df[tot_area_df$Land_Type == "Shrubland" | tot_area_df$Land_Type == "Grassland",], FUN=sum)
    names(avail_ro_area)[ncol(avail_ro_area)] <- "avail_ro_area"
    check_avail_df = merge(man_area_sum[man_area_sum$Management == "Afforestation",], avail_ro_area, by = c("Region", "Ownership"), all.x = TRUE)
    man_area_sum$man_area[man_area_sum$Management == "Afforestation"] = apply(check_avail_df[,c("man_area", "avail_ro_area")], 1, FUN=min, na.rm=TRUE)
    # also calc the amount of shrub and grass needed
    frst_ro_areas = merge(tot_area_df[tot_area_df$Land_Type == "Shrubland" | tot_area_df$Land_Type == "Grassland",], avail_ro_area, by = c("Region", "Ownership"), all.x = TRUE)
    frst_ro_areas = merge(frst_ro_areas, check_avail_df, by = c("Region", "Ownership"), all.x = TRUE)
    frst_ro_areas$lt_area_need = frst_ro_areas$man_area * frst_ro_areas$tot_area.x / frst_ro_areas$avail_ro_area.x
    frst_ro_areas$lt_area_need[is.na(frst_ro_areas$lt_area_need)] = 0.00
    frst_ro_areas$lt_area_need[is.nan(frst_ro_areas$lt_area_need)] = 0.00
    frst_ro_areas$lt_area_need[frst_ro_areas$lt_area_need == Inf] = 0.00
    frst_ro_areas_agg = aggregate(lt_area_need ~ Region + Ownership, frst_ro_areas, FUN=sum, na.rm = TRUE)
    
    # meadow comes out of shrub, grass, savanna, woodland
    avail_ro_area = aggregate(tot_area ~ Region + Ownership, tot_area_df[tot_area_df$Land_Type == "Shrubland" | tot_area_df$Land_Type == "Grassland" | tot_area_df$Land_Type == "Savanna" | tot_area_df$Land_Type == "Woodland",], FUN=sum)
    names(avail_ro_area)[ncol(avail_ro_area)] <- "avail_ro_area"
    avail_ro_area = merge(avail_ro_area, frst_ro_areas_agg, by = c("Region", "Ownership"), all.x = TRUE)
    avail_ro_area$avail_ro_area = avail_ro_area$avail_ro_area - avail_ro_area$lt_area_need
    check_avail_df = merge(man_area_sum[man_area_sum$Management == "Restoration" & man_area_sum$Land_Type == "Meadow",], avail_ro_area, by = c("Region", "Ownership"), all.x = TRUE)
    man_area_sum$man_area[man_area_sum$Management == "Restoration" & man_area_sum$Land_Type == "Meadow"] = apply(check_avail_df[,c("man_area", "avail_ro_area")], 1, FUN=min, na.rm=TRUE)
    # calc the meadow grassland need
    mdw_ro_areas = merge(tot_area_df[tot_area_df$Land_Type == "Grassland",], avail_ro_area, by = c("Region", "Ownership"), all.x = TRUE)
    mdw_ro_areas = merge(mdw_ro_areas, check_avail_df, by = c("Region", "Ownership"), all.x = TRUE)
    mdw_ro_areas$lt_area_need = mdw_ro_areas$man_area * mdw_ro_areas$tot_area.x / mdw_ro_areas$avail_ro_area.x
    mdw_ro_areas$lt_area_need[is.na(mdw_ro_areas$lt_area_need)] = 0.00
    mdw_ro_areas$lt_area_need[is.nan(mdw_ro_areas$lt_area_need)] = 0.00
    mdw_ro_areas$lt_area_need[mdw_ro_areas$lt_area_need == Inf] = 0.00
    
    # fresh marsh and coastal marsh restoration both come from cultivated
    # if there is prescribed restoration, sum the restoration then scale the man_area and take the min of man_area and scaled man_area
    if (nrow(man_area_sum[man_area_sum$Management == "Restoration" & (man_area_sum$Land_Type == "Fresh_marsh" | man_area_sum$Land_Type == "Coastal_marsh"),])>0) {
    tot_rest_area = aggregate(man_area ~ Region + Ownership, man_area_sum[man_area_sum$Management == "Restoration" & (man_area_sum$Land_Type == "Fresh_marsh" | man_area_sum$Land_Type == "Coastal_marsh"),], FUN=sum)
    names(tot_rest_area)[ncol(tot_rest_area)] <- "tot_rest_area"
    check_avail_df = merge(man_area_sum[man_area_sum$Management == "Restoration" & (man_area_sum$Land_Type == "Fresh_marsh" | man_area_sum$Land_Type == "Coastal_marsh"),], tot_rest_area, by = c("Region", "Ownership"), all.x = TRUE)
    check_avail_df = merge(check_avail_df, tot_area_df[tot_area_df$Land_Type == "Cultivated",], by = c("Region", "Ownership"), all.x = TRUE)
    check_avail_df$avail_rest_area = check_avail_df$tot_area.y / check_avail_df$tot_rest_area * check_avail_df$man_area
    check_avail_df$avail_rest_area[is.na(check_avail_df$avail_rest_area)] = 0.00
    check_avail_df$avail_rest_area[is.nan(check_avail_df$avail_rest_area)] = 0.00
    check_avail_df$avail_rest_area[check_avail_df$avail_rest_area == Inf] = 0.00
    # fresh marsh - do these separately due to ordering issues
    man_area_sum$man_area[man_area_sum$Management == "Restoration" & man_area_sum$Land_Type == "Fresh_marsh"] = apply(check_avail_df[check_avail_df$Land_Type.x == "Fresh_marsh", c("man_area", "avail_rest_area")], 1, FUN=min, na.rm=TRUE)
    # coastal marsh - do these separately due to ordering issues
    man_area_sum$man_area[man_area_sum$Management == "Restoration" & man_area_sum$Land_Type == "Coastal_marsh"] = apply(check_avail_df[check_avail_df$Land_Type.x == "Coastal_marsh", c("man_area", "avail_rest_area")], 1, FUN=min, na.rm=TRUE)
    # calc the wetland cultivated need
    if (length(man_area_sum$man_area[man_area_sum$Management == "Restoration" & (man_area_sum$Land_Type == "Fresh_marsh" | man_area_sum$Land_Type == "Coastal_marsh")]) > 0) {
    	wet_ro_areas = aggregate(man_area ~ Region + Ownership, man_area_sum[man_area_sum$Management == "Restoration" & (man_area_sum$Land_Type == "Fresh_marsh" | man_area_sum$Land_Type == "Coastal_marsh"),], FUN = sum)
    	names(wet_ro_areas)[ncol(wet_ro_areas)] <- "wet_area_need"
    }
    wet_ro_areas$wet_area_need[is.na(wet_ro_areas$wet_area_need)] = 0.00
    wet_ro_areas$wet_area_need[is.nan(wet_ro_areas$wet_area_need)] = 0.00
    wet_ro_areas$wet_area_need[wet_ro_areas$wet_area_need == Inf] = 0.00
    } # end if there is prescribed restoration
    
    # woodland gets the leftovers of grassland and cultivated
    avail_ro_area = aggregate(tot_area ~ Region + Ownership, tot_area_df[tot_area_df$Land_Type == "Grassland" | tot_area_df$Land_Type == "Cultivated",], FUN=sum)
    names(avail_ro_area)[ncol(avail_ro_area)] <- "avail_ro_area"
    avail_ro_area = merge(avail_ro_area, frst_ro_areas[frst_ro_areas$Land_Type.x == "Grassland", c("Region", "Ownership", "lt_area_need")], by = c("Region", "Ownership"), all.x = TRUE)
    avail_ro_area = merge(avail_ro_area, mdw_ro_areas[mdw_ro_areas$Land_Type.x == "Grassland", c("Region", "Ownership", "lt_area_need")], by = c("Region", "Ownership"), all.x = TRUE)
    avail_ro_area$avail_ro_area = avail_ro_area$avail_ro_area - avail_ro_area$lt_area_need.x - avail_ro_area$lt_area_need.y
    if (exists("wet_ro_areas")) {
		avail_ro_area = merge(avail_ro_area, wet_ro_areas, by = c("Region", "Ownership"), all.x = TRUE)
		avail_ro_area$wet_area_need[is.na(avail_ro_area$wet_area_need)] = 0.00
    	avail_ro_area$wet_area_need[is.nan(avail_ro_area$wet_area_need)] = 0.00
    	avail_ro_area$wet_area_need[avail_ro_area$wet_area_need == Inf] = 0.00
		avail_ro_area$avail_ro_area = avail_ro_area$avail_ro_area - avail_ro_area$wet_area_need
	}
    check_avail_df = merge(man_area_sum[man_area_sum$Management == "Restoration" & man_area_sum$Land_Type == "Woodland",], avail_ro_area, by = c("Region", "Ownership"), all.x = TRUE)
    man_area_sum$man_area[man_area_sum$Management == "Restoration" & man_area_sum$Land_Type == "Woodland"] = apply(check_avail_df[,c("man_area", "avail_ro_area")], 1, FUN=min, na.rm=TRUE)
  
    
     ############# end pre man_area check #############
    
    # the developed practices are independent of each other and so calc cum sum independently, even though they are not currently used
    #  dead_removal values are assigned to the "aggregate" dev_all values for man_area and man_area_sum; these are not used either
    # current ag management does not use sum area because they are annual practices to maintain the benefits
    # Afforestation and restoration are not dependent on existing area and are not included in aggregate managed area
    #  these use the sum to ensure protection of restored area
    # forest and rangeland compost use the cumulative area for adjusting c accumulation
    #  calculate these cumulative sums based on the current and previous years up to the benefit period limit
    # range_lowfreq_period = 30
  	# range_medfreq_period = 10
  	# forest_benefit_period = 20
  	# urban_forest_benefit_period = 20
  	
  	# first add the current year management as the general calculation of man_area_sum
	man_area_sum$man_area_sum = man_area_sum$man_area_sum + man_area_sum$man_area
	man_area_sum$man_area_sum[man_area_sum$man_area_sum > man_area_sum$tot_area] = man_area_sum$tot_area[man_area_sum$man_area_sum > man_area_sum$tot_area]
  	
    # now make some calculations for specific practices
    # the out area table only has the previous years (except the first year target area, but use the calculated man area for the current year)
    
	temp_df = out_area_df_list[[2]]
	temp_df$sum = 0
	sum_end_col = which(names(temp_df) == paste0(year-1, "_ha"))
	
	# the first year will not have previous year values, so the value will be the first year value as calculated above
	if(length(sum_end_col) > 0) {
		
 		# rangeland low frequency compost
		sum_start = year - range_lowfreq_period + 1
		if (sum_start < start_year) { sum_start = start_year }
		sum_start_col = which(names(temp_df) == paste0(sum_start, "_ha"))
		if (sum_start_col == sum_end_col) {
			# only one year in sum, so rowSums() won't work
			temp_df$sum[temp_df$Management == "Low_frequency"] = temp_df[temp_df$Management == "Low_frequency",sum_start_col]
		} else { temp_df$sum[temp_df$Management == "Low_frequency"] = rowSums(temp_df[temp_df$Management == "Low_frequency", c(sum_start_col:sum_end_col)]) }
    	# rangeland med frequency compost
		sum_start = year - range_medfreq_period + 1
		if (sum_start < start_year) { sum_start = start_year }
		sum_start_col = which(names(temp_df) == paste0(sum_start, "_ha"))
		if (sum_start_col == sum_end_col) {
			# only one year in sum, so rowSums() won't work
			temp_df$sum[temp_df$Management == "Med_frequency"] = temp_df[temp_df$Management == "Med_frequency",sum_start_col]
		} else { temp_df$sum[temp_df$Management == "Med_frequency"] = rowSums(temp_df[temp_df$Management == "Med_frequency", c(sum_start_col:sum_end_col)]) }
    	
    	# forest non-afforestation
    	sum_start = year - forest_benefit_period + 1
		if (sum_start < start_year) { sum_start = start_year }
		sum_start_col = which(names(temp_df) == paste0(sum_start, "_ha"))
		if (sum_start_col == sum_end_col) {
			# only one year in sum, so rowSums() won't work
			temp_df$sum[temp_df$Land_Type == "Forest" & temp_df$Management != "Afforestation"] =
				temp_df[temp_df$Land_Type == "Forest" & temp_df$Management != "Afforestation",sum_start_col]
		} else { 
			temp_df$sum[temp_df$Land_Type == "Forest" & temp_df$Management != "Afforestation"] =
				rowSums(temp_df[temp_df$Land_Type == "Forest" & temp_df$Management != "Afforestation", c(sum_start_col:sum_end_col)])
		}
		
		# developed_all dead_removal
		sum_start = year - urban_forest_benefit_period + 1
		if (sum_start < start_year) { sum_start = start_year }
		sum_start_col = which(names(temp_df) == paste0(sum_start, "_ha"))
		if (sum_start_col == sum_end_col) {
			# only one year in sum, so rowSums() won't work
			temp_df$sum[temp_df$Land_Type == "Developed_all" & temp_df$Management == "Dead_removal"] =
				temp_df[temp_df$Land_Type == "Developed_all" & temp_df$Management == "Dead_removal",sum_start_col]
		} else { 
			temp_df$sum[temp_df$Land_Type == "Developed_all" & temp_df$Management == "Dead_removal"] =
				rowSums(temp_df[temp_df$Land_Type == "Developed_all" & temp_df$Management == "Dead_removal", c(sum_start_col:sum_end_col)])
		}
		
		# update man_area_sum by adding the previous years sum and the current year man area
    	man_area_sum = merge(man_area_sum, temp_df[,c(1:5,ncol(temp_df))], by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"), all.x = TRUE)
    	man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    	man_area_sum$man_area_sum[man_area_sum$Management == "Low_frequency" | man_area_sum$Management == "Med_frequency" | (man_area_sum$Land_Type == "Forest" & man_area_sum$Management != "Afforestation") | (man_area_sum$Land_Type == "Developed_all" & man_area_sum$Management == "Dead_removal")] = 
    		man_area_sum$sum[man_area_sum$Management == "Low_frequency" | man_area_sum$Management == "Med_frequency" | (man_area_sum$Land_Type == "Forest" & man_area_sum$Management != "Afforestation") | (man_area_sum$Land_Type == "Developed_all" & man_area_sum$Management == "Dead_removal")] +
    		man_area_sum$man_area[man_area_sum$Management == "Low_frequency" | man_area_sum$Management == "Med_frequency" | (man_area_sum$Land_Type == "Forest" & man_area_sum$Management != "Afforestation") | (man_area_sum$Land_Type == "Developed_all" & man_area_sum$Management == "Dead_removal")]
    	man_area_sum$sum = NULL
	} #end if second year or more
	
    ### make sure the cumulative sum is not greater than the existing area
    # this is only an issue for forest and rangeland and cultivated, where the practices are assumed to be mutually exclusive in area,
    #  and the cum sum matters
    # developed_all cum man area is ultimately limited to dev_all total area, and dead_removal man area is the only one that matters
    # afforestation and restoration are already limited by man area above, so man_area_sum is already limited
    
    ### merge df's man_area_sum & tot_area_df, and assign to man_area_sum dataframe (essentially, add additional 
    ### tot_area column to man_area_sum), excludes any land types from tot_area that are not in man_area_sum
    ### man_area_sum = merge(man_area_sum, tot_area_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    ### sort man_area_sum dataframe by Land_Type_ID, then by Manage_ID 
    ###man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    # (1) [if there are any other man areas] aggregate cumulative areas by landcat for all management except afforestation & restoration $ developd_all...
    # create df of aggregated cumulative areas (man_area_sum_agg): aggregate by summing man_area_sum with the same Land_Cat_ID _except_ 
    # for areas with Afforestation and Restoration management
	  if (nrow(man_area_sum[man_area_sum$Management != "Afforestation" & 
	                        man_area_sum$Management != "Restoration" & 
	                        man_area_sum$Land_Type != "Developed_all",])>0) { 
    man_area_sum_agg = aggregate(man_area_sum ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & 
                                                                             man_area_sum$Management != "Restoration" & 
                                                                             man_area_sum$Land_Type != "Developed_all",], FUN=sum)
    # update aggregated sums column name (man_area_sum) to man_area_sum_agg_extra
    names(man_area_sum_agg)[ncol(man_area_sum_agg)] <- "man_area_sum_agg_extra"
    # end if other type of prescribed management	
	  } else {
	  # create empty man_area_sum_agg
	  man_area_sum_agg <- data.frame(Land_Cat_ID=numeric(0), man_area_sum_agg_extra=numeric(0))
	}
    # merge man_area_sum & man_area_sum_agg dataframes by Land_Type_ID
    man_area_sum = merge(man_area_sum, man_area_sum_agg, by = "Land_Cat_ID", all.x = TRUE)
    # replace 'NA' values in man_area_sum$man_area_sum_agg_extra with 0's (these are land types that didn't have any management)
    man_area_sum$man_area_sum_agg_extra = replace(man_area_sum$man_area_sum_agg_extra, is.na(man_area_sum$man_area_sum_agg_extra), 0)
    # sort man_area_sum dataframe by man_area_sum$Land_Type_ID and man_area_sum$Manage_ID
    man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    # (2) update man_area_sum for developed_all with individual cumulative area
    # For developed_all land type only, replace values in (extra) aggregated areas column with values in man_area_sum 
    # (i.e. don't aggregate for developed_all, use individual cumulative mangagement areas)
    man_area_sum$man_area_sum_agg_extra[man_area_sum$Land_Type == "Developed_all"] = man_area_sum$man_area_sum[man_area_sum$Land_Type == "Developed_all"]
    # (3) assign 0's to aggregate cumulative areas for afforestation and restoration 
    # For afforestation or restoration management activities only, replace values in the (extra) aggregated area column with 0 
    # (i.e. afforestation and restoration not included in aggregate sum)
    man_area_sum$man_area_sum_agg_extra[man_area_sum$Management == "Afforestation" | man_area_sum$Management == "Restoration"] = 0
    # add new column "excess_sum_area", which is equal to the (extra) aggregated areas minus total land type areas 
    man_area_sum$excess_sum_area = man_area_sum$man_area_sum_agg_extra - man_area_sum$tot_area
    # if excess_sum_area > 0, assign the index to excess_sum_area_inds
    excess_sum_area_inds = which(man_area_sum$excess_sum_area > 0)
    # trim the cumulative manaagement areas: subtract out a proportional amount of any excess from each man_area_sum 
    man_area_sum$man_area_sum[excess_sum_area_inds] = man_area_sum$man_area_sum[excess_sum_area_inds] - 
      man_area_sum$excess_sum_area[excess_sum_area_inds] * man_area_sum$man_area_sum[excess_sum_area_inds] / 
      man_area_sum$man_area_sum_agg_extra[excess_sum_area_inds]
    # replace NaN (not a number) values in man_area_sum with 0's 
    man_area_sum$man_area_sum = replace(man_area_sum$man_area_sum, is.nan(man_area_sum$man_area_sum), 0)
    # replace neg values in man_area_sum with 0's 
    man_area_sum$man_area_sum = replace(man_area_sum$man_area_sum, man_area_sum$man_area_sum < 0, 0)   
    # replace Infinite values in man_area_sum with 0's 
    man_area_sum$man_area_sum = replace(man_area_sum$man_area_sum, man_area_sum$man_area_sum == Inf, 0)
    
    # if there are any other man areas create a _trimmed_ aggregated man_area_sum df (man_area_sum_agg2)
    if (nrow(man_area_sum[man_area_sum$Management != "Afforestation" & 
                          man_area_sum$Management != "Restoration" & 
                          man_area_sum$Land_Type != "Developed_all",])>0) {
    # aggregate sum man_area_sum vector by Land_Cat_ID for all management activities _except_ afforestation and restoration areas 
    man_area_sum_agg2 = aggregate(man_area_sum ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & 
                                                                              man_area_sum$Management != "Restoration" & 
                                                                             man_area_sum$Land_Type != "Developed_all",], FUN=sum)
    # rename _trimmed_ aggregate cumulative areas to "man_area_sum_agg" in man_area_sum_agg2 df
    names(man_area_sum_agg2)[ncol(man_area_sum_agg2)] <- "man_area_sum_agg"
    # end if there are other prescribed management practices
    } else {
      # create empty man_area_sum_agg2
      man_area_sum_agg2 <- data.frame(Land_Cat_ID=numeric(0), man_area_sum_agg=numeric(0))
    }
    # (4) add column "man_area_sum_agg" (Developed_all = _untrimmed_ individual cummulative areas, & afforestation and restoration excluded)
    # by merging man_area_sum & man_area_sum_agg2 dataframes by Land_Type_ID
    man_area_sum = merge(man_area_sum, man_area_sum_agg2, by = "Land_Cat_ID", all.x =TRUE)
    # replace NA's in the man_area_sum_agg column with 0's
    man_area_sum$man_area_sum_agg = replace(man_area_sum$man_area_sum_agg, is.na(man_area_sum$man_area_sum_agg), 0)
    # sort man_area_sum by land type, then management ID 
    man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    # (5) don't use man_area_sum_agg for Developed_all: replace (trimmed & aggregated) man_area_sum_agg column with individual (trimmed) man_area_sum 
    man_area_sum$man_area_sum_agg[man_area_sum$Land_Type == "Developed_all"] = man_area_sum$man_area_sum[man_area_sum$Land_Type == "Developed_all"]
    # (6) assign 0's to aggregate cumulative areas for afforestation and restoration 
    # For afforestation or restoration management activities only, replace values in the (extra) aggregated area column with 0 
    # (i.e. afforestation and restoration not included in aggregate sum)
    man_area_sum$man_area_sum_agg_extra[man_area_sum$Management == "Afforestation" | man_area_sum$Management == "Restoration"] = 0
    
    } # end if there are any prescribed management practices
    
    # build some useful data frames
    all_c_flux = tot_area_df
    
    # check if there any prescibed management practices
      # if there are merge  man_are_agg (2 columns: Land_Cat_ID, man_area_agg)  with all_c_flux (5 columns: Land_Cat_ID, Region, Land_Type, Ownership, tot_area)
    if (nrow(man_area_sum)>0){
    all_c_flux = merge(all_c_flux, man_area_agg2, by = "Land_Cat_ID", all.x = TRUE)
    } 
    
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    
    if (nrow(man_area_sum)>0){
    all_c_flux$man_area_agg[all_c_flux$Land_Type == "Developed_all"] = man_area_sum$man_area[man_area_sum$Management == "Dead_removal"]
    na_inds = which(is.na(all_c_flux[,"man_area_agg"]))
    all_c_flux[na_inds,"man_area_agg"] = 0
    all_c_flux$unman_area = all_c_flux[,"tot_area"] - all_c_flux[,"man_area_agg"]
    all_c_flux = merge(all_c_flux, man_area_sum_agg2, by = "Land_Cat_ID", all.x = TRUE)
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    all_c_flux$man_area_sum_agg[all_c_flux$Land_Type == "Developed_all"] = 
      man_area_sum$man_area_sum[man_area_sum$Management == "Dead_removal"]
    na_inds = which(is.na(all_c_flux[,"man_area_sum_agg"]))
    all_c_flux[na_inds,"man_area_sum_agg"] = 0
    all_c_flux$unman_area_sum = all_c_flux[,"tot_area"] - all_c_flux[,"man_area_sum_agg"]
    # all_c_flux now has 9 columns: Land_Cat_ID, Region, Land_Type, Ownership, tot_area, man_area_agg, unman_area, man_area_sum_agg, unman_area_sum
    } else { # end if there any prescribed management practices
      # if no prescribed practices, assign 0's to managed area variables
      all_c_flux$man_area_agg <- 0
      all_c_flux$unman_area = all_c_flux[,"tot_area"] - all_c_flux[,"man_area_agg"]
      # all_c_flux = merge(all_c_flux, man_area_sum_agg2, by = "Land_Cat_ID", all.x = TRUE)
      all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
      all_c_flux$man_area_sum_agg <- 0
      all_c_flux$unman_area_sum = all_c_flux[,"tot_area"] - all_c_flux[,"man_area_sum_agg"]
    }
    
    # merge rangeland management (soil) effect and cultivated land df's. Then merge with developed and forest management 
      # assign the common column names between man_grass_df and man_ag_df to common_cols since man_ag_df has extra columns now
    common_cols <- intersect(colnames(man_ag_df), colnames(man_grass_df))
      # rbind the commmon columns into man_adjust_df
    man_adjust_df = rbind(subset(man_grass_df, select = common_cols),subset(man_ag_df, select = common_cols))
    man_adjust_df = rbind(man_adjust_df, man_forest_df[,c(1:5,forest_soilcaccumfrac_colind)])
    man_adjust_df = rbind(man_adjust_df, man_dev_df[,c(1:5,dev_soilcaccumfrac_colind)])
    man_adjust_df = merge(man_adjust_df, rbind(man_forest_df, man_dev_df), by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", 
                                                                                  "Management", "SoilCaccum_frac"), all.x = TRUE)
    
    # if there are no prescribed mgmt practices, add some empty columns to man_area_sum so that when it's merged with man_adjust_df it has same structure as
      # other cases. This will ensure that later calcs won't be affected by missing columns.
    if (nrow(man_area_sum)==0 & year == start_year) {
      columnnames <- data.frame(man_area=numeric(0), man_area_sum=numeric(0), tot_area=numeric(0), man_area_agg_pre=numeric(0), excess_area_pre=numeric(0), 
                                man_area_agg=numeric(0), man_area_sum_agg_extra=numeric(0), excess_sum_area=numeric(0), man_area_sum_agg=numeric(0))
      man_area_sum<-cbind(man_area_sum,columnnames) 
    }
    # merge compiled management effects df with area calcs
      # this adds man_area, man_area_sum, tot_area, man_area_agg_pre, excess_area_pre, man_area_agg, man_area_sum_agg_extra, excess_sum_area, man_area_sum_agg
    # merge creates man_adjust_df with 38 variables
    man_adjust_df = merge(man_area_sum, man_adjust_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"), 
                          all.x = TRUE)
    
    man_adjust_df = man_adjust_df[order(man_adjust_df$Land_Cat_ID, man_adjust_df$Management),]
    # if there are any prescribed management practice, replace the NA values with more appropriate ones 
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,c("SoilCaccum_frac", "VegCuptake_frac", "DeadCaccum_frac")] <- 
      apply(man_adjust_df[,c("SoilCaccum_frac", "VegCuptake_frac", "DeadCaccum_frac")], 2, function (x) {replace(x, is.na(x), 1.00)})
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
    }
    # the proportional increase in urban forest area is represented as a proportional increase in veg c uptake
    if (year == start_year) {
      start_urban_forest_fraction = man_adjust_df[man_adjust_df$Management == "Urban_forest", "man_area"] / 
        man_adjust_df[man_adjust_df$Management == "Urban_forest", "tot_area"]
    }
    man_adjust_df[man_adjust_df$Management == "Urban_forest", "VegCuptake_frac"] = 
      man_adjust_df[man_adjust_df$Management == "Urban_forest", "man_area"] / 
      man_adjust_df[man_adjust_df$Management == "Urban_forest", "tot_area"] / start_urban_forest_fraction
    
    # soil
    # apply climate effect to baseline soil c flux. use current year loop to determine which column to use in climate_soil_df (first clim factor col ind is 5)
      # note that the soil conservation flux will be modified too: man_soilc_flux = (man_soilc_flux/baseline_soilc_flux) * (baseline_soilc_flux * soil climate scalar)
    soilc_accum_df$soilc_accum_val <- soilc_accum_df$soilc_accum_val * climate_soil_df[,year-2005]
    # Cultivated uses the current year managed area
    man_soil_df = merge(man_adjust_df, soilc_accum_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    man_soil_df = man_soil_df[order(man_soil_df$Land_Cat_ID, man_soil_df$Management),]
    # if there are  prescribed management practice(s),
    if (nrow(man_adjust_df)>0) {
      # soil C flux * area = cumulative managed area * soil C mgmt frac * baseline soil c flux
      # for rows with NA management or NA soilc_accum_val, this equals NA
    man_soil_df$soilcfluxXarea[man_soil_df$Land_Type != "Cultivated"] = man_soil_df$man_area_sum[man_soil_df$Land_Type != "Cultivated"] * 
      man_soil_df$SoilCaccum_frac[man_soil_df$Land_Type != "Cultivated"] * 
      man_soil_df$soilc_accum_val[man_soil_df$Land_Type != "Cultivated"]
    # for cultivated lands: soilcfluxXarea = current year managed area * modified-SoilCaccum_frac * climate-affected baseline soil C flux
    man_soil_df$soilcfluxXarea[man_soil_df$Land_Type == "Cultivated"] = man_soil_df$man_area[man_soil_df$Land_Type == "Cultivated"] * 
      man_soil_df$SoilCaccum_frac[man_soil_df$Land_Type == "Cultivated"] * 
      man_soil_df$soilc_accum_val[man_soil_df$Land_Type == "Cultivated"]
      } else {
        # if there are no prescribed management practices assign 0 to "soil C flux * managed area"
        man_soil_df$soilcfluxXarea <- 0.00  
      }
    
    # replace all NA soilcfluxXarea values with 0, otherwise they will not aggregate properly below
    na_inds <- which(is.na(man_soil_df$soilcfluxXarea))
    man_soil_df$soilcfluxXarea[na_inds] <- 0.00
    
    # aggregate soilcfluxXarea by landcat
    man_soilflux_agg = aggregate(soilcfluxXarea ~ Land_Cat_ID + Region + Land_Type + Ownership, man_soil_df, FUN=sum)
    # merge aggregated soil flux df with all_c_flux, which has man_area_agg, unman_area, man_area_sum_agg & unman_area_sum
    man_soilflux_agg = merge(all_c_flux, man_soilflux_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    # merge the soilc_accum_val from man_soil_df with man_soilflux_agg
    man_soilflux_agg = merge(man_soilflux_agg, man_soil_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "soilc_accum_val")], 
                             by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x=TRUE)
    man_soilflux_agg = man_soilflux_agg[order(man_soilflux_agg$Land_Cat_ID),]
    # only save the individual landcats (omit extra rows)
    man_soilflux_agg = unique(man_soilflux_agg)
    # replace the landcats without a soil C flux value with 0
    na_inds = which(is.na(man_soilflux_agg$soilc_accum_val))
    man_soilflux_agg[na_inds, "soilc_accum_val"] = 0.00
    
    # Calculate the area-weighted soil C flux value 
    # final non-cultivated soil c flux = [(managed soil c flux * cumulative mgmt area) + (baseline soil c flux * cumulative unmanaged area)]/ total area 
       # if no mgmt, unman_area_sum==tot_area, and fin_soilc_accum = baseline soil C flux
    man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type != "Cultivated"] = 
      (man_soilflux_agg$soilcfluxXarea[man_soilflux_agg$Land_Type != "Cultivated"] + 
         man_soilflux_agg$unman_area_sum[man_soilflux_agg$Land_Type != "Cultivated"] * 
         man_soilflux_agg$soilc_accum_val[man_soilflux_agg$Land_Type != "Cultivated"]) / 
      tot_area_df$tot_area[tot_area_df$Land_Type != "Cultivated"]
    # final cultivated soil c flux = [(managed soil c flux * annual mgmt area) + (baseline soil c flux * annual unmanaged area)]/ total area 
    man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type == "Cultivated"] = 
      (man_soilflux_agg$soilcfluxXarea[man_soilflux_agg$Land_Type == "Cultivated"] + 
         man_soilflux_agg$unman_area[man_soilflux_agg$Land_Type == "Cultivated"] * 
         man_soilflux_agg$soilc_accum_val[man_soilflux_agg$Land_Type == "Cultivated"]) / 
      tot_area_df$tot_area[tot_area_df$Land_Type == "Cultivated"]
    nan_inds = which(is.nan(man_soilflux_agg$fin_soilc_accum) | man_soilflux_agg$fin_soilc_accum == Inf)
    man_soilflux_agg$fin_soilc_accum[nan_inds] = man_soilflux_agg[nan_inds, "soilc_accum_val"]
    man_soilflux_agg$man_change_soilc_accum = man_soilflux_agg$fin_soilc_accum - man_soilflux_agg$soilc_accum_val
    
    ############################################################################################################
    #########################################  All land types ##################################################
    ########### calc managed/unmanaged area-weighted VEG C FLUXES [MgC/ha/y] (fin_vegc_uptake) #################
    ############################################################################################################
    
    # all developed area veg c uptake is adjusted because urban forest increased
      # (recall: current year's "VegCuptake_frac" for Urban_forest management = proportional change in managed urban forest area to the initial 
      # urban forest area)
    #  so remove the other developed managements from this table and multiply by total area and use unman area = 0
    
    # apply this year's veg climate effect to baseline veg c flux (first year clim factor col ind is 5)
    vegc_uptake_df$vegc_uptake_val <- vegc_uptake_df$vegc_uptake_val * climate_veg_df[,year-2005]
    # merge man_adjust_df and vegc_uptake_df and assign to man_veg_df 
    man_veg_df = merge(man_adjust_df, vegc_uptake_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    man_veg_df = man_veg_df[order(man_veg_df$Land_Cat_ID, man_veg_df$Management),] 
    # omit all records for Dead_removal & Growth and keep all others
    man_veg_df = man_veg_df[(man_veg_df$Management != "Dead_removal" & man_veg_df$Management != "Growth") | is.na(man_veg_df$Management),]
    
    ################ First, calc MANAGED AREA VEG C UPTAKE [MgC/y]  (vegcfluxXarea) #############################
    
    # calc managed area's total veg C uptake for all landtypes using cumulative areas: 
   
      # first check if there are no prescribed management practices and assign 0 to man_area_sum
    if (nrow(man_adjust_df)==0) {
      man_veg_df$man_area_sum <- 0
      man_veg_df$vegcfluxXarea <- 0
    } else {
      # vegcfluxXarea = cumulative_management_area x VegCuptake_frac x vegc_uptake_val 
      man_veg_df$vegcfluxXarea = man_veg_df$man_area_sum * man_veg_df$VegCuptake_frac * man_veg_df$vegc_uptake_val
      # special calc for managed area's total veg C uptake in developed landtype using total area (managed + unmanaged)   
      man_veg_df$vegcfluxXarea[man_veg_df$Land_Type == "Developed_all"] = 
        man_veg_df$tot_area[man_veg_df$Land_Type == "Developed_all"] * 
        man_veg_df$VegCuptake_frac[man_veg_df$Land_Type == "Developed_all"] * 
        man_veg_df$vegc_uptake_val[man_veg_df$Land_Type == "Developed_all"]
    }
    # aggregate sum veg C uptake across Land_Cat_ID + Region + Land_Type + Ownership 
    man_vegflux_agg = aggregate(vegcfluxXarea ~ Land_Cat_ID + Region + Land_Type + Ownership, man_veg_df, FUN=sum)
    # merge aggregate sums with all_c_flux (management areas and total areas) 
    man_vegflux_agg = merge(all_c_flux, man_vegflux_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    
    # clean up vegcfluxXarea values
    na_inds = which(is.na(man_vegflux_agg$vegcfluxXarea))
    man_vegflux_agg$vegcfluxXarea[na_inds] = 0
    
    # merge "vegc_uptake_val" (baseline veg c flux) column to man_vegflux_agg dataframe 
    man_vegflux_agg = merge(man_vegflux_agg, man_veg_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "vegc_uptake_val")], 
                            by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    man_vegflux_agg = man_vegflux_agg[order(man_vegflux_agg$Land_Cat_ID),]
    man_vegflux_agg = unique(man_vegflux_agg) 
    # not all land types have veg C accumulation value - these cases have NA and represent 0 C accum
    na_inds = which(is.na(man_vegflux_agg$vegc_uptake_val)) 
    man_vegflux_agg[na_inds, "vegc_uptake_val"] = 0
    
    ################ Last, calc area-weighted VEG C FLUXES [MgC/ha/y] (fin_vegc_uptake) ##########################
    # calc veg C flux for all landtypes using cumulative areas:
    # add column "fin_vegc_uptake" = ((veg C uptake due to mangement) + (cumulative unmanaged area)(vegc_uptake_val)) /  (total area)
    man_vegflux_agg$fin_vegc_uptake = (man_vegflux_agg$vegcfluxXarea + man_vegflux_agg$unman_area_sum * 
                                         man_vegflux_agg$vegc_uptake_val) / tot_area_df$tot_area
    man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type == "Developed_all"] = 
      man_vegflux_agg$vegcfluxXarea[man_vegflux_agg$Land_Type == "Developed_all"] / 
      tot_area_df$tot_area[man_vegflux_agg$Land_Type == "Developed_all"]
    nan_inds = which(is.nan(man_vegflux_agg$fin_vegc_uptake) | man_vegflux_agg$fin_vegc_uptake == Inf)
    man_vegflux_agg$fin_vegc_uptake[nan_inds] = man_vegflux_agg[nan_inds, "vegc_uptake_val"]
    # for cases without any prescribed management, fin_vegc_uptake should equal vegc_uptake_val, but due to rounding error the difference is not exactly 0
      # however, all(abs(man_vegflux_agg$fin_vegc_uptake-man_vegflux_agg$vegc_uptake_val)<0.000000000000001) == TRUE
      # thus, assign man_vegflux_agg$fin_vegc_uptake <- man_vegflux_agg$vegc_uptake_val for cases without prescribed management
    if (nrow(man_adjust_df)==0) {
      man_vegflux_agg$fin_vegc_uptake <- man_vegflux_agg$vegc_uptake_val
    }
    man_vegflux_agg$man_change_vegc_uptake = man_vegflux_agg$fin_vegc_uptake - man_vegflux_agg$vegc_uptake_val
    
    # dead
    
    # determine the fractional mortality c rate of above ground main for this year from target years (from the current year mortality fraction)
    # this is then applied to the above ground main c pools and the below ground main c pools
    # the understory mortality is set to a default value at the beginning of this script
    # linear interpolation between target years
    # if the year is past the final target year than use the final target year
    # the developed_all mortality is transferred to Above_harvest_frac in man_adjust_df, and zeroed in the deadfrac df
    
    linds = which(mortality_targetyears <= year)
    hinds = which(mortality_targetyears >= year)
    prev_targetyear = max(mortality_targetyears[linds])
    next_targetyear = min(mortality_targetyears[hinds])
    pind = which(mortality_targetyears == prev_targetyear)
    nind = which(mortality_targetyears == next_targetyear)
    pcol = mortality_targetyear_labels[pind]
    ncol = mortality_targetyear_labels[nind]
    
    if (prev_targetyear == next_targetyear | length(hinds) == 0) {
      deadc_frac_df$deadc_frac_in = mortality_target_df[,pcol]
    } else {
      deadc_frac_df$deadc_frac_in = mortality_target_df[,pcol] + 
        (year - prev_targetyear) * (mortality_target_df[,ncol] - mortality_target_df[,pcol]) / (next_targetyear - prev_targetyear)
    }
    
    # merge the initial deadc_frac from dead_c_frac_df with man_adjust_df (tot areas, all the man areas, and effect params)
    man_dead_df = merge(man_adjust_df, deadc_frac_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    # remove all but Dead_removal (i.e. Growth and Urban_forest) for Developed_all, which is the total Developed_all landcat area. 
      # this will ensure the aggregated management areas are correct (not larger than total landcat area)
    man_dead_df <- man_dead_df[man_dead_df$Management!="Growth" & man_dead_df$Management!="Urban_forest" | is.na(man_dead_df$Management),]
    man_dead_df = man_dead_df[order(man_dead_df$Land_Cat_ID, man_dead_df$Management),]
    
    # deadCaccum_frac is from the forest_manage tab in c_input, which is the effect of forest management on mortality 
    # (i.e., mortality is reduced by either 44% or 33%) and deadc_frac_in is determined by linear interpolation above
    # man_dead_area = agg_man_area * man_mort_factor * interp_mort_frac 
    # first check if no prescribed management practices
    if (nrow(man_adjust_df)==0) {
      # assign 0 to man_dead_df$deadcfracXarea if no prescribed mgmt practices
      man_dead_df$deadcfracXarea <- 0
    } else {
      man_dead_df$deadcfracXarea = man_dead_df$man_area_sum * man_dead_df$DeadCaccum_frac * man_dead_df$deadc_frac_in
    }
    # aggregate all the man_dead_area
    man_deadfrac_agg = aggregate(deadcfracXarea ~ Land_Cat_ID + Region + Land_Type + Ownership, man_dead_df, FUN=sum)
    
    man_deadfrac_agg = merge(all_c_flux, man_deadfrac_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    na_inds = which(is.na(man_deadfrac_agg$deadcfracXarea))
    man_deadfrac_agg$deadcfracXarea[na_inds] = 0
    man_deadfrac_agg = merge(man_deadfrac_agg, deadc_frac_df, 
                             by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    man_deadfrac_agg = man_deadfrac_agg[order(man_deadfrac_agg$Land_Cat_ID),]
    na_inds = which(is.na(man_deadfrac_agg$deadc_frac_in))
    man_deadfrac_agg[na_inds, "deadc_frac_in"] = 0
    
    # check if no prescribed management practices
    if (nrow(man_adjust_df)==0) {
      # if no prescribed mgmt practices assign deadc_frac_in to fin_deadc_frac 
      man_deadfrac_agg$fin_deadc_frac <- man_deadfrac_agg$deadc_frac_in
    } else {
    # fin_dead_c_frac = (agg_man_dead_area + agg_unman_area * interp_mort_frac) / tot_area
    # which is the area-weighted mortality_fraction to later apply to total above- and below-ground C in Forest, Savanna/Woodland, and ??
    man_deadfrac_agg$fin_deadc_frac = (man_deadfrac_agg$deadcfracXarea + man_deadfrac_agg$unman_area_sum * 
                                         man_deadfrac_agg$deadc_frac_in) / tot_area_df$tot_area
    nan_inds = which(is.nan(man_deadfrac_agg$fin_deadc_frac) | man_deadfrac_agg$fin_deadc_frac == Inf)
    # if NA or Inf, assign the interp_mort_frac to fin_deadc_frac
    man_deadfrac_agg$fin_deadc_frac[nan_inds] = man_deadfrac_agg[nan_inds, "deadc_frac_in"]
    }
    
    # man_change_deadc_accum = diff between area-weighted man & unman mort_frac and interp_mort_frac 
    man_deadfrac_agg$man_change_deadc_accum = man_deadfrac_agg$fin_deadc_frac - man_deadfrac_agg$deadc_frac_in
    
    ## now transfer the developed_all mortality to "Above_harvested_frac" in man_adjust_df
    # 1. assign the developed_all records in man_deadfrac_agg to dev_deadfrac
    dev_deadfrac = man_deadfrac_agg[man_deadfrac_agg$Land_Type == "Developed_all",]
    # 2. assign 0's to fin_deadc_frac & man_change_deadc_accum for all developed_all records in man_deadfrac_agg
    man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Developed_all"] = 0
    man_deadfrac_agg$man_change_deadc_accum[man_deadfrac_agg$Land_Type == "Developed_all"] = 0
    
    # merge the dev_deadcfrac with the man_adjust_df
    man_adjust_df = merge(man_adjust_df, dev_deadfrac[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "fin_deadc_frac")],
    						by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    # assign the non-zeroed fin_deadc_frac for Dead-removal in Developed_all to Above_harvested_frac
    man_adjust_df$Above_harvested_frac[man_adjust_df$Land_Type == "Developed_all" & man_adjust_df$Management=="Dead_removal"] = 
      man_adjust_df$fin_deadc_frac[man_adjust_df$Land_Type == "Developed_all" & man_adjust_df$Management=="Dead_removal"]
    # now assign NULL to all fin_deadc_frac in man_adjust_df
    man_adjust_df$fin_deadc_frac = NULL
    man_adjust_df = man_adjust_df[order(man_adjust_df$Land_Cat_ID, man_adjust_df$Management),]
    
    cat("Starting eco c transfers\n")
    
    # apply the eco fluxes to the carbon pools (current year area and carbon)
    # (final flux * tot area + density * tot area) / tot area
    # above main, below main, understory, stand dead, down dead, litter, soil
    
    # general procedure
    # assume veg uptake is net live standing biomass accum (sans mortality)
    # calculate net below ground accum based on above to below ratio
    # assume dead accum is net mortality, subtract from above veg uptake - this goes to standing dead, downed dead, litter
    # calculate net below ground mortality from dead accum to above accum ratio - this is only subtracted from below pool because soil c values 
    # are assumed to be net density changes
    # calculate net understory uptake and mortality from above values - this goes to downed dead and litter pools
    # estimate litter accum values from mortality and litter fraction of dead pools
    
    # notes
    # developed and ag have only above ground and soil c pools
    #  developed mortality is transferred to above harvest to be able to control what happens to this biomass
    #  no mortality for ag because woody crops are not tracked
    # treat na values as zeros
    # mortality fractions are zero in the input table if no veg c accum is listed in the carbon inputs (this is also checked below)
    # these flux transfers are normalized to current tot_area, and gains are positive
    
    # forest above main accum needs net foliage and branches/bark accums added to it based on estimated component fractions
    # forest downed dead and litter accum are estimated from the added above c based on mort:vegc flux ratio - this goes from above to downed 
    #  dead and litter - and this value is also a net value
    # forest dead standing is subtracted from above main
    # forest below main accum and understory accum need to be calculated based on ratio of these existing densities to the above densities
    # forest understory mortality uses a 1% default value (so it is not directly affected by prescribed tree mortality) - this is added to 
    #  downed dead and litter - as the additional veg c uptake is a net value, this accumulation is also a net value
    # forest below mortality is estimated based upon standing dead accum to vegc uptake ratio - this is only subtracted from below as soil c is 
    #  a net value
    
    # savanna/woodland veg uptake is net above and below (sans mortality) - so split it based on existing ratios
    # savanna/woodland has net eco exchange flux based on the tree uptake and the soil accum so don't add any other flux unless it cancels out
    # savanna/woodland to include mortality: transfer mortality to standing and downed dead and litter proportionally
    # savanna/woodland to include mortality: transfer mortality of below density to soil c
    # savanna/woodland understory will stay the same over time
    
    # out density sheet names: c("All_orgC_den", "All_biomass_C_den", "Above_main_C_den", "Below_main_C_den", "Understory_C_den", 
    # "StandDead_C_den", "DownDead_C_den", "Litter_C_den", "Soil_orgC_den")
    # put the current year density values into the next year column to start with
    # then add the carbon transfers to the next year column
    # eco accum names
    egnames = NULL
    for (i in 1:num_out_density_sheets) {
      out_density_df_list[[i]][, next_density_label] = out_density_df_list[[i]][, cur_density_label]
      if(i >= 3) {
        egnames = c(egnames, paste0(out_density_sheets[i], "_gain_eco"))
        all_c_flux[,egnames[i-2]] = 0
      }
    }
    
    #########################  forest  ######################### 
    
    # above main
    # above main c density
    above_vals = out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type == "Forest", cur_density_label]
    # above main stem c flux = area-weighted VEG C FLUXES
    vegc_flux_vals = man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type == "Forest"]
    # above main leaf + bark + branch c flux 
    added_vegc_flux_vals = vegc_flux_vals * (leaffrac + barkfrac + branchfrac) / stemfrac 
    # dead C flux from stem = area-weighted mortality_fraction * above main c density * stemfrac = area-weighted mortality_fraction * stem c density
    deadc_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] * above_vals * stemfrac
    # dead C flux from leaf + bark + branch = area-weighted mortality_fraction * above main c density * (1-stemfrac) = area-weighted mortality_fraction * remaining main c density
    above2dldead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] * above_vals * (1.0 - stemfrac)
    
    #deadc2vegc_ratios = deadc_flux_vals / vegc_flux_vals
    #above2dldead_flux_vals = deadc2vegc_ratios * added_vegc_flux_vals
    
    # Above_main_C_den_gain_eco = (stem + leaf + bark + branch c flux) - (mortality) 
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[1]] = vegc_flux_vals + added_vegc_flux_vals - deadc_flux_vals - above2dldead_flux_vals
    
    # standing dead C flux = c loss from stem
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[4]] = deadc_flux_vals
    
    # understory
    # understory c density
    under_vals = out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type == "Forest", cur_density_label]
    # ratio of understory to above main c density
    underfrac = under_vals / above_vals
    # understory c flux = (understory c dens/above main c dens) * (above main stem c flux) * (above main c dens/stem c dens)
    underc_flux_vals = underfrac * vegc_flux_vals / stemfrac
    # dead understory C =  0.01 * understory c dens
    under2dldead_flux_vals = default_mort_frac * out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type == "Forest", cur_density_label]
    #under2dldead_flux_vals = deadc2vegc_ratios * underc_flux_vals
    # Understory_C_den_gain_eco = understory c flux - dead understory C
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[3]] = underc_flux_vals - under2dldead_flux_vals
    
    # downed dead and litter
    # down dead frac = DownDead_C_den / (DownDead_C_den + Litter_C_den)
    downfrac = out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type == "Forest", cur_density_label] / 
      (out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type == "Forest", cur_density_label] + 
         out_density_df_list[[8]][out_density_df_list[[8]]$Land_Type == "Forest", cur_density_label])
    # DownDead_C_den_gain_eco = (DownDead_C_den / (DownDead_C_den + Litter_C_den)) * (dead understory + leaf + bark + branch C)
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[5]] = downfrac * (above2dldead_flux_vals + under2dldead_flux_vals)
    # Litter_C_den_gain_eco = (Litter_C_den / (DownDead_C_den + Litter_C_den)) * (dead understory + leaf + bark + branch C)
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[6]] = (1.0 - downfrac) * (above2dldead_flux_vals + under2dldead_flux_vals)
    
    # below ground
    # recall that the input historical soil c fluxes are net, so the default historical mortality here implicitly goes to the soil
    #  but any change from the default mortality needs to be added to the soil
    #  so store the initial below ground mortality flux
    # assume that the other soil fluxes do not change (litter input rates and emissions) because I don't have enough info to change these
    #  basically, the litter input would change based on its density, and the emissions may increase with additional soil c
    
    # Root C density = Below_main_C_den
    below_vals = out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type == "Forest", cur_density_label]
    # Dead root C flux = area-weighted mortality_fraction * root C density
    below2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] * below_vals
    # save the initial area-weighted mortality_fraction
    if (year == start_year) { initial_deadc_frac_forest = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] }
    	# { below2dead_flux_initial_forest = below2dead_flux_vals }
    # first calculate the net root biomass increase
    # rootfrac = root c density/above main C density
    rootfrac = below_vals / above_vals
    # Below_main_C_den_gain_eco = (root c density/above main C density) * above main stem c flux/stem frac
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[2]] = rootfrac * (vegc_flux_vals / stemfrac) - below2dead_flux_vals
    
    # soil
    # need to add the difference due to chnages from default/initial mortality
    # Soil_orgC_den_gain_eco = area-weighted soil C flux + (Dead root C flux - initial area-weighted mortality_fraction * root C density)
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[7]] = man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type == "Forest"] + 
      (below2dead_flux_vals - initial_deadc_frac_forest * below_vals)
      #(below2dead_flux_vals - below2dead_flux_initial_forest)
    
    #########################  savanna/woodland  ######################### 
    
    # above and below main
    # root loss has to go to soil c because the veg gain is tree nee, and the soil flux is ground nee, together they are the net flux
    #  so here changing mortality is already accounted for with respect to additions to soil carbon
    # but these additions to the dead pools and soil c may be overestimated because the flux measurements do not include mortality
    # transfer above loss proportionally to standing, down, and litter pools
    # leave understory c static because the available data are for a grass understory, which has no long-term veg accumulation
    
    # Above main C dens = Above_main_C_den  
    above_vals = out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type == "Savanna" | out_density_df_list[[3]]$Land_Type == "Woodland", 
                                          cur_density_label]
    # Above main C flux = area-weighted VEG C FLUX
    vegc_flux_vals = man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type == "Savanna" | man_vegflux_agg$Land_Type == "Woodland"]
    # Root C dens = Below_main_C_den 
    below_vals = out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type == "Savanna" | out_density_df_list[[4]]$Land_Type == "Woodland", 
                                          cur_density_label]
    # Above main C flux = area-weighted VEG C FLUX * Above C dens/(Above + Root C dens)
    above_flux_vals = vegc_flux_vals * above_vals / (above_vals + below_vals)
    # Root C flux = area-weighted VEG C FLUX * Root C dens/(Above + Root C dens)
    below_flux_vals = vegc_flux_vals * below_vals / (above_vals + below_vals)
    # Dead Above main C flux = area-weighted mortality_fraction * Above main C dens
    above2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Savanna" | man_deadfrac_agg$Land_Type == "Woodland"] * 
      above_vals
    #zinds = which(above2dead_flux_vals == 0 & above_flux_vals > 0)
    #above2dead_flux_vals[zinds] = default_mort_frac * above_vals[zinds]
    #deadc2vegc_ratios = above2dead_flux_vals / above_flux_vals
    # Dead root C flux = area-weighted mortality_fraction * root C dens
    below2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Savanna" | man_deadfrac_agg$Land_Type == "Woodland"] * 
      below_vals
    #naninds = which(is.nan(below2dead_flux_vals) & below_flux_vals > 0)
    #below2dead_flux_vals[naninds] = default_mort_frac * below_vals[naninds]
    #naninds = which(is.nan(below2dead_flux_vals))
    #below2dead_flux_vals[naninds] = 0
    
    # above_main_C_den_gain_eco = Above main C flux - Dead Above C flux
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[1]] = above_flux_vals - above2dead_flux_vals
    # Below_main_C_den_gain_eco = Root C flux - Dead Root C flux
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[2]] = below_flux_vals - below2dead_flux_vals
    
    # standing, down, and litter
    # Standing dead C dens = StandDead_C_den
    standdead_vals = out_density_df_list[[6]][out_density_df_list[[6]]$Land_Type == "Savanna" | out_density_df_list[[6]]$Land_Type == "Woodland", 
                                              cur_density_label]
    # Down dead C dens = DownDead_C_den
    downdead_vals = out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type == "Savanna" | out_density_df_list[[7]]$Land_Type == "Woodland", 
                                             cur_density_label]
    # Litter C dens = Litter_C_den
    litter_vals = out_density_df_list[[8]][out_density_df_list[[8]]$Land_Type == "Savanna" | out_density_df_list[[8]]$Land_Type == "Woodland", 
                                           cur_density_label]
    # Standing dead dens fraction = Stand Dead C dens/ Stand Dead + Down Dead + Litter)
    standdead_frac_vals = standdead_vals / (standdead_vals + downdead_vals + litter_vals)
    # Down dead dens fraction = Down Dead C dens/ Stand Dead + Down Dead + Litter)
    downdead_frac_vals = downdead_vals / (standdead_vals + downdead_vals + litter_vals)
    # Litter dens fraction = Litter C dens/ Stand Dead + Down Dead + Litter)
    litter_frac_vals = litter_vals / (standdead_vals + downdead_vals + litter_vals)
    # StandDead_C_den_gain_eco = Stand dead frac * Dead above main C flux
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[4]] = standdead_frac_vals * above2dead_flux_vals
    # DownDead_C_den_gain_eco = Down dead frac * Dead above main C flux
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[5]] = downdead_frac_vals * above2dead_flux_vals
    # Litter_C_den_gain_eco = Litter frac * Dead above main C flux
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[6]] = litter_frac_vals * above2dead_flux_vals
    
    # soil - recall that this is nee flux measurement, not density change, so the root mortality has to go to soil c
    # Soil C flux = area-weighted soil C flux
    soilc_flux_vals = man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type == "Savanna" | man_soilflux_agg$Land_Type == "Woodland"]
    # Soil_orgC_den_gain_eco = area-weighted soil C flux + Dead root C flux
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[7]] = soilc_flux_vals + below2dead_flux_vals
    
    ######################### the rest ######################### 
    # assume vegc flux is all standing net density change, sans mortality
    # assume above and understory deadc flux is all mort density change - take from above and distribute among stand, down, and litter
    # use mortality only if there is veg c accum due to growth
    # assume soil c flux is net density change - so the below is simply a net root density change,
    #	and the calculated or implicit mortality implicitly goes to soil
    
    # Above main C dens
    above_vals = out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type != "Savanna" & out_density_df_list[[3]]$Land_Type != "Woodland" & 
                                            out_density_df_list[[3]]$Land_Type != "Forest", cur_density_label]
    # Root main C dens
    below_vals = out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type != "Savanna" & out_density_df_list[[4]]$Land_Type != "Woodland" & 
                                            out_density_df_list[[4]]$Land_Type != "Forest", cur_density_label]
    # Understory C dens
    under_vals = out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type != "Savanna" & out_density_df_list[[5]]$Land_Type != "Woodland" & 
                                            out_density_df_list[[5]]$Land_Type != "Forest", cur_density_label]
    # Standing dead C dens
    standdead_vals = out_density_df_list[[6]][out_density_df_list[[6]]$Land_Type != "Savanna" & out_density_df_list[[6]]$Land_Type != "Woodland" & 
                                                out_density_df_list[[6]]$Land_Type != "Forest", cur_density_label]
    # Down dead C dens
    downdead_vals = out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type != "Savanna" & out_density_df_list[[7]]$Land_Type != "Woodland" & 
                                               out_density_df_list[[7]]$Land_Type != "Forest", cur_density_label]
    # Litter C dens
    litter_vals = out_density_df_list[[8]][out_density_df_list[[8]]$Land_Type != "Savanna" & out_density_df_list[[8]]$Land_Type != "Woodland" & 
                                             out_density_df_list[[8]]$Land_Type != "Forest", cur_density_label]
    # Soil C dens
    soil_vals = out_density_df_list[[9]][out_density_df_list[[9]]$Land_Type != "Savanna" & out_density_df_list[[9]]$Land_Type != "Woodland" & 
                                           out_density_df_list[[9]]$Land_Type != "Forest", cur_density_label]
    # above and below C fluxes
    # Above main c flux = area-weighted above-main C flux
    above_flux_vals = man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type != "Savanna" & man_vegflux_agg$Land_Type != "Woodland" & 
                                                        man_vegflux_agg$Land_Type != "Forest"]
    # Root main C flux = area-weighted above-main C flux * root C dens/above main C dens
    below_flux_vals = above_flux_vals * below_vals / above_vals
    # index NA values
    naninds = which(is.nan(below_flux_vals))
    # For NA root C flux, assume default root to above frac (0.2) 
      # Root main C flux = area-weighted above-main C flux * 0.2
    below_flux_vals[naninds] = above_flux_vals[naninds] * default_below2above_frac
    # Soil C flux = area-weighted soil C flux
    soilc_flux_vals = man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type != "Savanna" & man_soilflux_agg$Land_Type != "Woodland" & 
                                                         man_soilflux_agg$Land_Type != "Forest"]
    # recall that the input historical soil c fluxes are net, so the default historical mortality here implicitly goes to the soil
    #  but any change from the default mortality needs to be added to the soil
    #  so store the initial below ground mortality flux
    # assume that the other soil fluxes do not change (litter input rates and emissions) because I don't have enough info to change these
    #  basically, the litter input would change based on its density, and the emissions may increase with additional soil c
    # also, if there is no veg c accum, then mortality is zero, even for understory, because only net soil c changes                                                    
    
    # dead c frac = area-weighted mortaility frac 
    deadc_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type != "Savanna" & man_deadfrac_agg$Land_Type != "Woodland" &
    		man_deadfrac_agg$Land_Type != "Forest"]
    # for records with NA or negative above main C flux, assign 0 to the dead c flux
    deadc_flux_vals[is.na(above_flux_vals) | above_flux_vals <= 0] = 0.0
    # Dead above main C flux = area-weighted mortaility frac * above main C dens 
    above2dead_flux_vals = deadc_flux_vals * above_vals
    # Dead root C flux = area-weighted mortaility frac * root C dens
    below2dead_flux_vals = deadc_flux_vals * below_vals
    
    # save intital area-weighted mortaility frac
    if (year == start_year) { initial_deadc_frac_rest = deadc_flux_vals }
    	#{ below2dead_flux_initial_rest = below2dead_flux_vals }
    
    # above
    # Above_main_C_den_gain_eco = above main C flux - dead above main C flux
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[1]] = 
      above_flux_vals - above2dead_flux_vals
    
    # root
    # Below_main_C_den_gain_eco = root C flux - dead root C flux
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[2]] = 
      below_flux_vals - below2dead_flux_vals
    
    # understory
    # understory frac = understory/above main
    underfrac = under_vals / above_vals
    # understory C flux = understory/above main * area-weighted above main C flux
    underc_flux_vals = underfrac * above_flux_vals
    # index NA understory C flux
    naninds = which(is.nan(underc_flux_vals))
    # for records with NA understory C flux, assign to it default understory frac (0.1) * area-weighted above main C flux
    underc_flux_vals[naninds] = default_under_frac * above_flux_vals[naninds]
    # assign the dead area-weighted mortaility frac to understory mortality frac
    under_mort_frac = deadc_flux_vals
    # assign the default mortality fraction (0.01) to records with positive area-weighted above main C flux
    under_mort_frac[!is.na(above_flux_vals) & above_flux_vals > 0] = default_mort_frac
    # dead understory c flux = understory dead frac * Understory_C_den
    under2dead_flux_vals = under_mort_frac * out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type != "Savanna" & 
                                                                          out_density_df_list[[5]]$Land_Type != "Woodland" & 
                                                                          out_density_df_list[[5]]$Land_Type != "Forest", cur_density_label]
    # Understory_C_den_gain_eco = understory C flux - dead understory c flux
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[3]] = 
      underc_flux_vals - under2dead_flux_vals
    
    # stand, down, litter
    
    # Standing dead C frac = Standing dead C dens / (Standing dead + down dead + litter C dens)
    standdead_frac_vals = standdead_vals / (standdead_vals + downdead_vals + litter_vals)
    # index NA Standing dead C frac
    naninds = which(is.nan(standdead_frac_vals))
    # assign the default standing dead frac (0.11) to records with NA standing dead fracs
    standdead_frac_vals[naninds] = default_standdead_frac
    # down dead C frac = down dead C dens / (Standing dead + down dead + litter C dens)
    downdead_frac_vals = downdead_vals / (standdead_vals + downdead_vals + litter_vals)
    # index NA down dead C frac
    naninds = which(is.nan(downdead_frac_vals))
    # assign the default down dead frac (0.23) to records with NA down dead fracs
    downdead_frac_vals[naninds] = default_downdead_frac
    # litter C frac = litter C dens / (Standing dead + down dead + litter C dens)
    litter_frac_vals = litter_vals / (standdead_vals + downdead_vals + litter_vals)
    # index NA litter C frac
    naninds = which(is.nan(litter_frac_vals))
    # assign the default litter frac (0.66) to records with NA litter fracs
    litter_frac_vals[naninds] = default_litter_frac
    # StandDead_C_den_gain_eco = standing dead C frac * (area-weighted dead above main C flux + dead understory c flux)
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[4]] = 
      standdead_frac_vals * (above2dead_flux_vals + under2dead_flux_vals)
    # DownDead_C_den_gain_eco = down dead C frac * (area-weighted dead above main C flux + dead understory c flux)
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[5]] = 
      downdead_frac_vals * (above2dead_flux_vals + under2dead_flux_vals)
    # Litter_C_den_gain_eco = down dead C frac * (area-weighted dead above main C flux + dead understory c flux)
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[6]] = 
      litter_frac_vals * (above2dead_flux_vals + under2dead_flux_vals)
    
    # soil
    # add any c due to changes from default/initial mortality
    
    # Soil_orgC_den_gain_eco = area-weighted soil C flux + (area-weighted mortaility frac - intital area-weighted mortaility frac)
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[7]] = 
      soilc_flux_vals + (below2dead_flux_vals - initial_deadc_frac_rest * below_vals)
      #(below2dead_flux_vals - below2dead_flux_initial_rest)
    
    # clean up numerical errors
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.na(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    # loop over the out density tables to update the carbon pools based on the eco fluxes
    # carbon cannot go below zero
    sum_change = 0
    sum_neg_eco = 0
    for (i in 3:num_out_density_sheets) {
      sum_change = sum_change + sum(all_c_flux[, egnames[i-2]] * all_c_flux$tot_area)
      out_density_df_list[[i]][, next_density_label] = out_density_df_list[[i]][, next_density_label] + all_c_flux[, egnames[i-2]]
      # first calc the carbon not subtracted because it sends density negative
      neginds = which(out_density_df_list[[i]][, next_density_label] < 0)
      # print out the indices that have negative c
      cat("neginds for out_density_df_list eco" , i, "are", neginds, "\n")
      # print out the areas for land categries that have negative c (if greater than 0 then land category us running out of c, which may 
      # be due to a particular case of land conversions, i.e. land category x has negative c accumulation and is gaining area from land category y 
      # with lower c density, which can dilute the c density lower than the annual c loss sending the c density negative. 
      # Otherwise, if the area == 0 and there is negative c density, it can solely be due to the land cateogry running out of area and having a 
      # negative c accumulation rate.)
      cat("total areas for neginds out_density_df_list eco" , i, "are", out_area_df_list[[1]][neginds, cur_area_label], "\n")
      # check if any of the negative c density land categories have area > 0 (note: this only works if soil c density (i=9) is the only pool with 
      # negative values, as this is re-saved for each i loop)
      if (any((out_area_df_list[[1]][neginds, cur_area_label])>0)) {
        # if so, subset the rows from the out_area_df_list associated with all neginds and assign to area_neginds_df
        area_neginds_df <- out_area_df_list[[1]][neginds,]
        # add column to area_neginds_df that says which c pool ran out of c
        area_neginds_df$neg_c_density_pool <- rep(out_density_sheets[i],nrow(area_neginds_df)) 
        # add column that says which year it is
        area_neginds_df$Year <- rep(year,nrow(area_neginds_df)) 
        # print the land category ID's that have neginds _and_ area >0
        cat("Land_Cat_ID with neginds for", out_density_sheets[i], "& non-zero area are", 
            unlist(area_neginds_df[out_area_df_list[[1]][neginds,cur_area_label]>0, c("Land_Cat_ID","Region","Ownership","Land_Type")]), "\n")
        # subset rows from from area_neginds_df with area >0, and assign to out_neginds_eco_df_pre
        out_neginds_eco_df_pre <- area_neginds_df[out_area_df_list[[1]][neginds,cur_area_label]>0, 
                                                  c("Land_Cat_ID","Region","Ownership","Land_Type","Year",cur_area_label,"neg_c_density_pool")]
        # rename the area column to generic name so rbind() for out_neginds_eco_df_pre & out_neginds_eco_df will work each year 
        colnames(out_neginds_eco_df_pre)[6] <- "Area_ha"
        # get the c density from next_density_label column in out_density_df_list[[i]] that are <0 (neginds) and have tot_area >0, and add column to area_neginds_df
        out_neginds_eco_df_pre$neg_density <- out_density_df_list[[i]][out_density_df_list[[i]]$Land_Cat_ID %in% out_neginds_eco_df_pre$Land_Cat_ID, next_density_label]
        # turn on check that this exists
        area_neginds_exist <- TRUE
      } else {area_neginds_exist <- FALSE}
      # sum of all negative c cleared = tot_area * c density
      sum_neg_eco = sum_neg_eco + sum(all_c_flux$tot_area[out_density_df_list[[i]][,next_density_label] < 0] * 
                                        out_density_df_list[[i]][out_density_df_list[[i]][,next_density_label] < 0, next_density_label])
      out_density_df_list[[i]][, next_density_label] <- replace(out_density_df_list[[i]][, next_density_label], 
                                                                out_density_df_list[[i]][, next_density_label] <= 0, 0.00)
    } # end loop over out densities for updating due to eco fluxes
    cat("eco carbon change is", sum_change, "\n")
    cat("eco negative carbon cleared is", sum_neg_eco, "\n")
    # add this year's neg sum to cumulative so we get a total negative c cleared at end of annual loop (out_neginds_eco is set to 0 before annual loop is started)
    out_cum_neginds_eco_tot <- sum_neg_eco + out_cum_neginds_eco_tot 
    
    # join the out_neginds_eco_df_pre below the last row of the previous year in out_neginds_eco_df
    if (area_neginds_exist & exists("out_neginds_eco_df")) {
        out_neginds_eco_df <- rbind(out_neginds_eco_df, out_neginds_eco_df_pre)
    } else { if (area_neginds_exist) {
        out_neginds_eco_df <- out_neginds_eco_df_pre
        }
    }
    ######
    ######
    # apply the transfer (non-eco, non-accum) flux management to the carbon pools (current year area and updated carbon)
    cat("Starting manage c transfers\n")
    
    # loop over the non-accum manage frac columns to calculate the transfer carbon density for each frac column
    # the transfer carbon density is based on tot_area so that it can be aggregated and subtracted directly from the current density
    
    ############################################################################################################
    #################### Do management C transfers [MgC/y] for forest & developed areas ########################
    ############################################################################################################

   # man_frac_names = c("Above_harvested_frac", "StandDead_harvested_frac", "Harvested2Wood_frac", "Harvested2Energy_frac", "Harvested2SawmillDecay_frac", 
   #                    "Harvested2Slash_frac", "Under2Slash_frac", "DownDead2Slash_frac", "Litter2Slash_frac", "Slash2Energy_frac", "Slash2Burn_frac", "Slash2Decay_frac", 
   #                    "Under2DownDead_frac", "Soil2Atmos_frac", "Above2StandDead_frac", "Below2Atmos_frac", "Below2Soil_frac")
    
   # c_trans_names = c("Above_harvested_c", "StandDead_harvested_c", "Harvested2Wood_c", "Harvested2Energy_c", "Harvested2SawmillDecay_c", "Harvested2Slash_c", "Under2Slash_c", 
   #                   "DownDead2Slash_c", "Litter2Slash_c", "Slash2Energy_c", "Slash2Burn_c", "Slash2Decay_c", "Under2DownDead_c", "Soil2Atmos_c", 
   #                   "Above2StandDead_c", "Below2Atmos_c", "Below2Soil_c")

    # indices of the appropriate density source df for the non-accum manage frac to c calcs; corresponds with out_density_sheets above
    # value == -1 indicates that the source is the harvested c; take the sum of the first two c trans columns
    # value == -2 indicates that the source is all slash-contributing pools; take the sum of c trans columns 6, 7 and 8 ("Under2Slash_c", "DownDead2Slash_c", "Litter2Slash_c")
    # manage_density_inds = c(3 [i=1], 6 [i=2], -1 [i=3], -1 [i=4], -1 [i=5], -1 [i=6], 5 [i=7], 7 [i=8], 8 [i=9], -2 [i=10], -2 [i=11], -2 [i=12], 5 [i=13], 9 [i=14], 3 [i=15], 4 [i=16], 4 [i=17])
    # manage_density_inds = c(3 (above), 6 (standdead), -1, -1, -1, -1 (Harvested2slash), 5 (under2slash), 7 (down2slash), 8 (litter2slash), -2 (slash2energy), -2 (slash2burn), -2 (slash2decay), 5 (under2down), 9 (soil), 3 (above2stand), 4 (below), 4 (below))
    # out_density_sheets = c("All_orgC_den" (1), "All_biomass_C_den" (2), "Above_main_C_den" (3), "Below_main_C_den" (4), "Understory_C_den" (5), "StandDead_C_den" (6), 
    #                       "DownDead_C_den" (7), "Litter_C_den" (8), "Soil_orgC_den" (9))
    # req'd order of operations: (1) Calc Harvested C (from above and stand dead), (2) Calc harvested 2slash, 2energy, 2decay (3) Calc other transfers 2slash (under, downdead, litter) (4) all others
    # for i in 1:17
    for (i in 1:num_manfrac_cols){
      # the removed values are calculated first, so this will work
      # if manage_density_inds[i] == -1, then the source is the removed pool; use the sum of the first two c trans columns 
      if (manage_density_inds[i] == -1 | manage_density_inds[i] == -2) {
        # when i = 3, 4, 5, or 6
        if (manage_density_inds[i] == -1) {
          # (Harvested2Wood_c, Harvested2Energy_c, Harvested2Decay_c, Harvested2Slash_c) [Mg/ha] = (Above_harvested_c + StandDead_harvested_c) * 
                                                                                   # (Harvested2Wood_frac, Harvested2Energy_frac, Harvested2Decay_frac, Harvested2Slash_frac)
          man_adjust_df[,c_trans_names[i]] = (man_adjust_df[,c_trans_names[1]] + man_adjust_df[,c_trans_names[2]]) * man_adjust_df[,man_frac_names[i]]
        } else {
          # else manage_density_inds[i] == -2 (when i = 10, 11, or 12)
          # if manage_density_inds[i] == -2 the source is the slash pool; take the sum of c trans columns 6, 7, 8 & 9 (Harvest2Slash_c + Under2Slash_c + DownDead2Slash_c + Litter2Slash_c)

          # (slash2energy_c, slash2wood_c, slash2burn_c, slash2decay_c) [Mg/ha] = (Harvested2Slash_c + Under2Slash_c + DownDead2Slash_c + Litter2Slash_c) * 
                                                                     # (Harvested2Slsash_frac, Under2Slash_frac, DownDead2Slash_frac, Litter2Slash_frac)
            man_adjust_df[,c_trans_names[i]] = (man_adjust_df[,c_trans_names[6]] + man_adjust_df[,c_trans_names[7]] + man_adjust_df[,c_trans_names[8]] +
                                                man_adjust_df[,c_trans_names[9]]) * man_adjust_df[,man_frac_names[i]]
        }
        # when i = 1(above2harvest), 2 (downdead2harvest), 7 (under2slash), 8 (downdead2slash), 9 (litter2slash), 13, 14, 15, 16, or 17, so use the C densities
      } else {
        # if any of the C density columns are not already one of the headers in man_adjust_df
        if (!out_density_sheets[manage_density_inds[i]] %in% colnames(man_adjust_df)) {
          # subset next year's C density from corresponding c pool's density dataframe and merge with the man_adjust_df
          man_adjust_df = merge(man_adjust_df, 
                                out_density_df_list[[manage_density_inds[i]]][,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", next_density_label)], 
                                by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
          # subset next year's corresponding C density header from the man_adjust_df, and assign to it the corresponding name of the c pool's density 
          names(man_adjust_df)[names(man_adjust_df) == next_density_label] = out_density_sheets[manage_density_inds[i]]
        }
        # calc C transfer = C density [mgC/ha] * (management transfer fraction) * current year managed area [ha] / total area [ha]
        man_adjust_df[,c_trans_names[i]] = man_adjust_df[,out_density_sheets[manage_density_inds[i]]] * man_adjust_df[,man_frac_names[i]] * 
          man_adjust_df$man_area / man_adjust_df$tot_area
      }
    } # end for i loop over the managed transfer fractions for calcuting the transfer carbon
    
    # check if there are any prescribed management practices (if not, don't do the following as it will result in error due to no rows)
    if (nrow(man_adjust_df)>0) { 
    man_adjust_df = man_adjust_df[order(man_adjust_df$Land_Cat_ID, man_adjust_df$Management),]
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, x == Inf, 0.00)})
    }
    
    # now consolidate the c density transfers to the pools
    # convert these to gains for consistency: all terrestrial gains are positive, losses are negative
    # store the names for aggregation below
    # out_density_sheets = c("All_orgC_den" (1), "All_biomass_C_den" (2), "Above_main_C_den" (3), "Below_main_C_den" (4), "Understory_C_den" (5), "StandDead_C_den" (6), 
    #                       "DownDead_C_den" (7), "Litter_C_den" (8), "Soil_orgC_den" (9))
    agg_names = NULL
    # above
    # add column called "Above_main_C_den_gain_man":  above main C density = -(above harvested C) -(above to standing dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[3], "_gain_man"))
    man_adjust_df[,agg_names[1]] = -man_adjust_df$Above_harvested_c - man_adjust_df$Above2StandDead_c
    # below
    # add column called "Below_main_C_den_gain_man": root C density = -(root to soil C) -(root to atmos C)
    agg_names = c(agg_names, paste0(out_density_sheets[4], "_gain_man"))
    man_adjust_df[,agg_names[2]] = -man_adjust_df$Below2Soil_c - man_adjust_df$Below2Atmos_c
    # understory
    # add column called "Understory_C_den_gain_man": understory C density = -(understory to slash C) -(understory to down dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[5], "_gain_man"))
    man_adjust_df[,agg_names[3]] = -man_adjust_df$Under2Slash_c - man_adjust_df$Under2DownDead_c
    # standing dead
    # add column called "StandDead_C_den_gain_man": standing dead C density = -(harvested standing dead C) + (above main to standing dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[6], "_gain_man"))
    man_adjust_df[,agg_names[4]] = -man_adjust_df$StandDead_harvested_c + man_adjust_df$Above2StandDead_c
    # down dead
    # add column called "DownDead_C_den_gain_man": down dead C density = -(down dead to slash C) + (understory to down dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[7], "_gain_man"))
    man_adjust_df[,agg_names[5]] = -man_adjust_df$DownDead2Slash_c + man_adjust_df$Under2DownDead_c
    # litter
    # add column called "Litter_C_den_gain_man": litter C density = -(litter to slash C)
    agg_names = c(agg_names, paste0(out_density_sheets[8], "_gain_man"))
    man_adjust_df[,agg_names[6]] = -man_adjust_df$Litter2Slash_c
    # soil
    # add column called "Soil_orgC_C_den_gain_man": soil C density = -(soil to atmos C) + (root to soil C)
    agg_names = c(agg_names, paste0(out_density_sheets[9], "_gain_man"))
    man_adjust_df[,agg_names[7]] = -man_adjust_df$Soil2Atmos_c + man_adjust_df$Below2Soil_c
    
    # to get the carbon must multiply these by the tot_area
    #### C to atmos via 4 pathways (wood product decay("Wood"), all other organic matter decay or root respiration ("Decay"), burn, energy) which determine 
    # proportial fates of gaseous C emissions (CO2-C, CH4-C, BC-C) ####
    #  "Land2Atmos_DecayC_stock_man" = -(total area [ha]) * (soil emissons [MgC/ha] + litter emissons [Mg/ha] + down dead emissons [Mg/ha] + 
    #   understory emissons [Mg/ha] + removed above-ground emissons [Mg/ha] + root emissions [Mg/ha])
    agg_names = c(agg_names, paste0("Land2Atmos_DecayC_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[8]] = -man_adjust_df$tot_area * (man_adjust_df$Soil2Atmos_c + man_adjust_df$Harvested2SawmillDecay_c + 
                                                                man_adjust_df$Slash2Decay_c + man_adjust_df$Below2Atmos_c)
    } else { 
      man_adjust_df[,agg_names[8]] <- numeric(0) 
      }
    #  "Land2Atmos_BurnC_stock_man" = -(total area [ha]) * (slash burn emissions [MgC/ha]) 
    agg_names = c(agg_names, paste0("Land2Atmos_BurnC_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[9]] = -man_adjust_df$tot_area * (man_adjust_df$Slash2Burn_c)
    } else {
      man_adjust_df[,agg_names[9]] <- numeric(0) 
    }
    
    # "Land2Atmos_TotEnergyC_stock_man") = -(total area [ha]) * (Harvested C removed for energy [MgC/ha] + slash removal for energy [MgC/ha])
    agg_names = c(agg_names, paste0("Land2Atmos_TotEnergyC_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[10]] = -man_adjust_df$tot_area * (man_adjust_df$Harvested2Energy_c + man_adjust_df$Slash2Energy_c)
    } else {
      man_adjust_df[,agg_names[10]] <- numeric(0)
    }
    # Get amount of total energy that is from harvest versus slash utilization
      # Harv2Energy
    agg_names = c(agg_names, paste0("Land2Atmos_Harv2EnerC_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[11]] = -man_adjust_df$tot_area * man_adjust_df$Harvested2Energy_c
    } else {
      man_adjust_df[,agg_names[11]] <- numeric(0)
    }
      # Slash2Energy
    agg_names = c(agg_names, paste0("Land2Atmos_Slash2EnerC_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[12]] = -man_adjust_df$tot_area * man_adjust_df$Slash2Energy_c
    # replace NaN with 0
    man_adjust_df[,agg_names[11]] <- replace( man_adjust_df[,agg_names[11]], is.nan( man_adjust_df[,agg_names[11]]), 0.0)
    man_adjust_df[,agg_names[12]] <- replace( man_adjust_df[,agg_names[12]], is.nan( man_adjust_df[,agg_names[12]]), 0.0)
    } else {
      man_adjust_df[,agg_names[12]] <- numeric(0)
    }
    #### C to wood ####
    # wood - this decays with a half-life
    agg_names = c(agg_names, paste0("Land2Wood_c_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[13]] = -man_adjust_df$tot_area * (man_adjust_df$Harvested2Wood_c + man_adjust_df$Slash2Wood_c)
    } else {
      man_adjust_df[,agg_names[13]] <- numeric(0)
    }
    # Get amount of total wood that is from harvest versus slash utilization
    # Harv2Wood
    agg_names = c(agg_names, paste0("Harv2Wood_c_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[14]] = -man_adjust_df$tot_area * man_adjust_df$Harvested2Wood_c
    } else {
      man_adjust_df[,agg_names[14]] <- numeric(0)
    }
    # Slash2Wood
    agg_names = c(agg_names, paste0("Slash2Wood_c_stock_man"))
    if (nrow(man_adjust_df)>0) {
    man_adjust_df[,agg_names[15]] = -man_adjust_df$tot_area * man_adjust_df$Slash2Wood_c
    # replace NaN with 0
    man_adjust_df[,agg_names[14]] <- replace( man_adjust_df[,agg_names[14]], is.nan( man_adjust_df[,agg_names[14]]), 0.0)
    man_adjust_df[,agg_names[15]] <- replace( man_adjust_df[,agg_names[15]], is.nan( man_adjust_df[,agg_names[15]]), 0.0)
    } else {
      man_adjust_df[,agg_names[15]] <- numeric(0)
    }
    
    # now aggregate to land type by summing the management options
    # these c density values are the direct changes to the overall c density
    # the c stock values are the total carbon form each land type going to atmos, energy (atmos), and wood
    
    # first, create table that has a row for each land cat ID, and a column for each of the management-caused C density changes [MgC/ha], 
    # and corresponding net cumulative C transfers [Mg C] to atmosphere (via decomp, burning, or energy (also burning)) or to wood 
    agg_cols = array(dim=c(length(man_adjust_df$Land_Cat_ID),length(agg_names)))
    # second, populate the table by applying loop to each row's land cat ID  
    for (i in 1:length(agg_names)) { # 1 to 15 (extra columns with with new slash pathway)
      # fill columns with corresponding management-caused C transfers from the man_adjust_df
        # agg_cols has 15 columns
      agg_cols[,i] = man_adjust_df[,agg_names[i]]
    }
    # if there are prescribed management practices 
    if (nrow(man_adjust_df)>0) {
    # third, aggregate the C transfers by summing within each land type and ownership combination and assign to man_adjust_agg df
        # creates man_adjust_agg with 19 columns: Land_Cat_ID, Region, Land_Type, Ownership, V1:V15
    man_adjust_agg = aggregate(agg_cols ~ Land_Cat_ID + Region + Land_Type + Ownership, data=man_adjust_df, FUN=sum)
    } else {
      man_adjust_agg <- cbind(man_adjust_df[,1:4],(data.frame(V1=numeric(0), V2=numeric(0), V3=numeric(0), V4=numeric(0), V5=numeric(0),  
                                   V6=numeric(0), V7=numeric(0), V8=numeric(0), V9=numeric(0), V10=numeric(0), V11=numeric(0),
                                   V12=numeric(0),V13=numeric(0),V14=numeric(0),V15=numeric(0))))   
    } 
    # fourth, label the columns of the aggregated table 
      # 15 names: "Above_main_C_den_gain_man_agg","Below_main_C_den_gain_man_agg","Understory_C_den_gain_man_agg","StandDead_C_den_gain_man_agg","DownDead_C_den_gain_man_agg",         
      # "Litter_C_den_gain_man_agg","Soil_orgC_den_gain_man_agg", "Land2Atmos_DecayC_stock_man_agg", "Land2Atmos_BurnC_stock_man_agg", "Land2Atmos_TotEnergyC_stock_man_agg", 
      # "Land2Atmos_Harv2EnerC_stock_man_agg","Land2Atmos_Slash2EnerC_stock_man_agg", "Land2Wood_c_stock_man_agg", "Harv2Wood_c_stock_man_agg", "Slash2Wood_c_stock_man_agg" 
    agg_names2 = paste0(agg_names,"_agg")
    # replaces the last 15 columns "V1":"V15" with agg_names2
    names(man_adjust_agg)[c(5:ncol(man_adjust_agg))] = agg_names2
    
    # merge these values to the unman area table to apply the adjustments to each land type
    all_c_flux = merge(all_c_flux, man_adjust_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.na(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    # loop over the out density tables to update the carbon pools based on the management fluxes
    # carbon cannot go below zero
    sum_change = 0
    sum_neg_man = 0
    # for above-main C density through soil organic C density dataframes, do:
    for (i in 3:num_out_density_sheets) {
      # subset each of the columns representing aggregated management-caused C density gains and the C stock transfers (i.e. C emissions and wood), 
      # multiply by total area, and
      # sum them all 
      # this gives a single value for state-wide cumulative management C changes [Mg C/y] -- used to make sure sure nothing is negative
      sum_change = sum_change + sum(all_c_flux[, agg_names2[i-2]] * all_c_flux$tot_area)
      
      #################################################### UPDATE NEXT YEAR'S C DENSITIES ####################################################
      # add corresponding management C density change to the value that is there (previous year's C density)
      out_density_df_list[[i]][, next_density_label] = out_density_df_list[[i]][, next_density_label] + all_c_flux[, agg_names2[i-2]]
      # calc the total state-wide cumulative C not subtracted because it sends density negative -- used as check to make sure this is minimal
      neginds = which(out_density_df_list[[i]][, next_density_label] < 0)
      cat("neginds for out_density_df_list manage" , i, "are", neginds, "\n")
      sum_neg_man = sum_neg_man + sum(all_c_flux$tot_area[out_density_df_list[[i]][,next_density_label] < 0] * 
                                        out_density_df_list[[i]][out_density_df_list[[i]][,next_density_label] < 0, next_density_label])
      
      # replace any negative updated C densities with 0
      out_density_df_list[[i]][, next_density_label] <- replace(out_density_df_list[[i]][, next_density_label], 
                                                                out_density_df_list[[i]][, next_density_label] <= 0, 0.00)
    } # end loop over out densities for updating due to veg management
    cat("manage carbon change is ", sum_change, "\n")
    cat("manage carbon to wood is ", sum(man_adjust_agg$Land2Wood_c_stock_man), "\n")
    cat("manage carbon to atmos is ", sum(man_adjust_agg$Land2Atmos_c_stock_man), "\n")
    cat("manage carbon to energy is ", sum(man_adjust_agg$Land2Energy_c_stock_man), "\n")
    cat("manage burned carbon to atmos is ", sum(man_adjust_agg$Land2Atmos_burnedC_stock_man), "\n")
    cat("manage non-burned carbon to atmos is ", sum(man_adjust_agg$Land2Atmos_nonburnedC_stock_man), "\n")
    cat("manage negative carbon cleared is ", sum_neg_man, "\n")
    
    # update the managed wood tables
    # recall that the transfers from land are negative values
    # use the IPCC half life equation for first order decay of wood products, and the CA average half life for all products
    #  this includes the current year loss on the current year production
    # running stock and cumulative change values are at the beginning of the labeled year - so the next year value is the stock or sum after 
    # current year production and loss
    # annual change values are in the year they occurred
    
    k = log(2) / wp_half_life
    # Next year's "Manage_Wood_C_stock" = Current year's "Manage_Wood_C_stock" * exp(-log(2)/half-life) + 
    #                                   wood_accumulated * ((1 - exp(-log(2)/half-life) / log(2)/half-life) 
    out_wood_df_list[[6]][,next_wood_label] = out_wood_df_list[[6]][,cur_wood_label] * exp(-k) + ((1 - exp(-k)) / k) * 
      -all_c_flux$Land2Wood_c_stock_man_agg
    # Next year's "Manage_Wood_CumGain_C_stock" = Current year's "Manage_Wood_CumGain_C_stock" + total wood_accumulated
    out_wood_df_list[[7]][,next_wood_label] = out_wood_df_list[[7]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_man_agg
      # Next year's "Man_Harv2Wood_CumGain_C_stock" = Current year's "Man_Harv2Wood_CumGain_C_stock" + wood_accumulated from harvest
      out_wood_df_list[[8]][,next_wood_label] = out_wood_df_list[[8]][,cur_wood_label] - all_c_flux$Harv2Wood_c_stock_man_agg
      # Next year's "Man_Slash2Wood_CumGain_C_stock" = Current year's "Man_Slash2Wood_CumGain_C_stock" + wood_accumulated from slash
      out_wood_df_list[[9]][,next_wood_label] = out_wood_df_list[[9]][,cur_wood_label] - all_c_flux$Slash2Wood_c_stock_man_agg
    # Current year's "Manage_Wood_AnnGain_C_stock" = wood_accumulated
    out_wood_df_list[[11]][,cur_wood_label] = -all_c_flux$Land2Wood_c_stock_man_agg
      out_wood_df_list[[12]][,cur_wood_label] = -all_c_flux$Harv2Wood_c_stock_man_agg
      out_wood_df_list[[13]][,cur_wood_label] = -all_c_flux$Slash2Wood_c_stock_man_agg
    # Current year's "Manage_Wood_AnnLoss_C_stock" = Current year's "Manage_Wood_C_stock" + wood_accumulated - Next year's "Manage_Wood_C_stock"  
    out_wood_df_list[[14]][,cur_wood_label] = out_wood_df_list[[6]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_man_agg - 
      out_wood_df_list[[6]][,next_wood_label]
    # Next year's "Manage_Wood_CumLoss_C_stock" = Current year's "Manage_Wood_CumLoss_C_stock" + Current year's "Manage_Wood_AnnLoss_C_stock"
    out_wood_df_list[[10]][,next_wood_label] = out_wood_df_list[[10]][,cur_wood_label] + out_wood_df_list[[14]][,cur_wood_label]
   
    ############################################################################################################
    ############################################################################################################
    #########################################  Apply FIRE to C pools  ##########################################
    ############################################################################################################
    ############################################################################################################
    
    # apply fire to the carbon pools (current year area and updated carbon)
    # distribute fire to forest, woodland, savanna, shrubland, and grassland, proportionally within the ownerships each year
    # assume that burn area onversion is not reflected in the baseline land type change numbers
    #  (which isn't necessarily the case if using the remote sensing landfire data for lulcc)
    cat("Starting fire c transfers\n")
    
    ############################################################################################################
    ########################## first, calculate this year's FIRE AREA based on the targets #####################
    ############################################################################################################
    
    # if the year is past the final target year than use the final target year
    
    # indices of prior or current target years
    linds = which(fire_targetyears <= year)
    # indices of upcoming or current target years
    hinds = which(fire_targetyears >= year)
    # set latest (or current) target year
    prev_targetyear = max(fire_targetyears[linds])
    # set next (or current) target year
    next_targetyear = min(fire_targetyears[hinds])
    # index of previous target year
    pind = which(fire_targetyears == prev_targetyear)
    # index of next target year
    nind = which(fire_targetyears == next_targetyear)
    # column header of previous target year
    pcol = fire_targetyear_labels[pind]
    # column header of next target year
    ncol = fire_targetyear_labels[nind]
    
    # assign the fire target areas to fire_area_df
    fire_area_df = fire_target_df[,c(1:5)]
    # if current year is a target year or past all target years, 
    if (prev_targetyear == next_targetyear | length(hinds) == 0) {
      # then create column for previous year target area and set to previous (or current) year's target area 
      fire_area_df[,pcol] = fire_target_df[,pcol]
      # else add a column with previous year target area
    } else {
      fire_area_df = fire_target_df[,c(1:5,pcol)]
      # update the column with the linear interpolation of the areas between target years
      fire_area_df[,pcol] = fire_target_df[,pcol] + (year - prev_targetyear) * (fire_target_df[,ncol] - fire_target_df[,pcol]) / 
        (next_targetyear - prev_targetyear)
    }
    fire_area_df[which(is.na(fire_area_df))] = 0.0
    
    ############################################################################################################
    ################## second, proportionally distribute ownership fire areas to each landtype #################
    ############################################################################################################
    
    # assign assigned FIRE TARGET AREA BY REGION/OWNERSHIP [ha] to "fire_own_area" 
    names(fire_area_df)[names(fire_area_df) == pcol] = "fire_own_area"
    # merge the fire effects dataframe with the fire target areas and assign to fire_adjust_df
    fire_adjust_df = merge(fire_area_df, fire_df, by = c("Severity"), all.x = TRUE)
    fire_adjust_df$Land_Cat_ID = NULL
    fire_adjust_df$Land_Type = NULL
    # merge with the tot_area_df region and by ownership class
    fire_adjust_df = merge(tot_area_df, fire_adjust_df, by = c("Region", "Ownership"), all.x = TRUE)
    # trim dataframe to only include forest, woodland, savanna, grassland, shrubland
    fire_adjust_df = fire_adjust_df[fire_adjust_df$Land_Type == "Forest" | fire_adjust_df$Land_Type == "Woodland" | 
                                      fire_adjust_df$Land_Type == "Savanna" | fire_adjust_df$Land_Type == "Grassland" | 
                                      fire_adjust_df$Land_Type == "Shrubland",]
    # create new dataframe for REGION/OWNERSHIP AREA [ha]: AGGREGATE total AREAS by REGION/OWNERSHIP
    avail_own_area = aggregate(tot_area ~ Region + Ownership, data = fire_adjust_df[!duplicated(fire_adjust_df$Land_Cat_ID),c(1:5)], sum)
    # rename OWNERSHIP AREA [ha]: "avail_own_area"
    names(avail_own_area)[3] = "avail_own_area"
    # AGGREGATE severities to get total burn AREAS by REGION/OWNERSHIP
    fire_own_area_agg = aggregate(fire_own_area ~ Land_Cat_ID, data = fire_adjust_df[,c(1:4,6:7)], sum)
    # rename area column to "fire_own_area_agg"
    names(fire_own_area_agg)[2] = "fire_own_area_agg"
    # merge FIRE C TRANSFER EFFECTS (fractions) dataframe with the aggregated fire area
    fire_adjust_df = merge(fire_own_area_agg, fire_adjust_df, by = c("Land_Cat_ID"), all.y = TRUE)
    # merge FIRE C TRANSFER EFFECTS (fractions) dataframe with the ownership areas dataframe
    fire_adjust_df = merge(avail_own_area, fire_adjust_df, by = c("Region", "Ownership"), all.y = TRUE)
    # if assigned FIRE TARGET AREA BY OWNERSHIP [ha] > TOTAL AREA OF OWNERSHIP [ha],
    #	set target aggregated burn area equal to the total ownership area by scaling the severity areas
    fire_adjust_df$fire_own_area <- replace(fire_adjust_df$fire_own_area, fire_adjust_df$fire_own_area_agg > fire_adjust_df$avail_own_area, 
                                            fire_adjust_df$fire_own_area * fire_adjust_df$avail_own_area / fire_adjust_df$fire_own_area_agg)
    # create column for BURNED AREA [ha] for each landtype-ownership combination and proportinally distribute burned areas 
    # BURNED AREA [ha] = (FIRE TARGET AREA BY OWNERSHIP [ha]) * (landtype area / ownership area)
    fire_adjust_df$fire_burn_area = fire_adjust_df$fire_own_area * fire_adjust_df$tot_area / fire_adjust_df$avail_own_area
    
    ############################################################################################################
    ################## third, adjust fire severity for managed forest #################
    ############################################################################################################
    
    # merge the managed area with the fire df
      # man_adjust_df: 78 variables, and fire_adjust_df: 19 variables (including fire_burn_area)
    fire_sevadj_df = merge(fire_adjust_df, man_adjust_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    
    # burned area for adjusted severity = burned_area/forest_area * man_area_sum/forest_area * forest_area
    # this is per management practice, per severity, based on managed area and total forest area in the land cat
    # need to calculate decreases/increases based on input fractions, then scale increases so that total man burn area doesn't change
      # check that there are prescribed mmgmt practices and prescribed wildfire
    if (nrow(fire_sevadj_df)>0 & all(fire_sevadj_df$fire_burn_area!=0)) {
    # if there are prescribed management practices, create man_burn_area for variable for fire_sevadj_df (79 variables total)
      fire_sevadj_df$man_burn_area = 0.0
    # man_burn_area = fire_burn_area * (man_area_sum/tot_area)
    fire_sevadj_df$man_burn_area[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management)] = 
    	fire_sevadj_df$fire_burn_area[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management)] *
    	fire_sevadj_df$man_area_sum[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management)] /
    	fire_sevadj_df$tot_area.x[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management)]
    	
    # sum the managed burn area across severities for normalization later
      # man_burn_agg (6 variables: Land_Cat_ID, Region, Land_Type, Ownership, Management, man_burn_area)
    man_burn_agg = aggregate(man_burn_area ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, fire_sevadj_df, FUN=sum, na.rm = TRUE)
    # change man_burn_area to man_burn_area_agg
    names(man_burn_agg)[names(man_burn_agg) == "man_burn_area"] = "man_burn_area_agg"
    # merge the aggregated burn areas with the fire_sevadj_df
      # fire_sevadj_df has 95 variables 
    fire_sevadj_df = merge(fire_sevadj_df, man_burn_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"), all.x = TRUE)	
    
    # new adjusted man burn area = managed burn area * adjusted fraction for severity
      # add new column for the new adjusted man burn area (fire_sevadj_df: 96 variables)
    fire_sevadj_df$man_burn_area_new = 0.0
      # calculate the new adjusted man burn area for high severity
    fire_sevadj_df$man_burn_area_new[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "High"] =
    	fire_sevadj_df$man_burn_area[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "High"] *
    	fire_sevadj_df$high_sev_frac[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "High"]
    # calculate the new adjusted man burn area for med severity
    fire_sevadj_df$man_burn_area_new[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "Medium"] =
    	fire_sevadj_df$man_burn_area[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "Medium"] *
    	fire_sevadj_df$med_sev_frac[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "Medium"]
    # calculate the new adjusted man burn area for low severity
    fire_sevadj_df$man_burn_area_new[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "Low"] =
    	fire_sevadj_df$man_burn_area[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "Low"] *
    	fire_sevadj_df$low_sev_frac[fire_sevadj_df$Land_Type == "Forest" & !is.na(fire_sevadj_df$Management) & fire_sevadj_df$Severity == "Low"]
    
    # 1. REDUCED & UNCHANGED SEVERITY AREAS
    # sum the new decreased (and unchanged) burn area severities to normalize the increases so man burn area doesn't change
      # store in mban_dec_agg (Land_Cat_ID, Region, Land_Type, Ownership, Management, man_burn_area_new)
    mban_dec_agg = aggregate(man_burn_area_new ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, fire_sevadj_df[fire_sevadj_df$man_burn_area_new <= fire_sevadj_df$man_burn_area,], FUN=sum, na.rm = TRUE)
    # rename the new aggreagated man_burn_area_new in mban_dec_agg: "man_burn_area_new_dec_agg"
    names(mban_dec_agg)[names(mban_dec_agg) == "man_burn_area_new"] = "man_burn_area_new_dec_agg"
    # merge mban_dec_agg with fire_sevadj_df (fire_sevadj_df: 97 variables)
    fire_sevadj_df = merge(fire_sevadj_df, mban_dec_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"), all.x = TRUE)
    
    # 2a. INCREASED SEVERITY AREA
    # sum the new increased severities to normalize the increases so man burn area doesn't change
    mban_inc_agg = aggregate(man_burn_area_new ~ Land_Cat_ID + Region + Land_Type + Ownership + Management, fire_sevadj_df[fire_sevadj_df$man_burn_area_new > fire_sevadj_df$man_burn_area,], FUN=sum, na.rm = TRUE)
    # call it "man_burn_area_new_inc_agg"
    names(mban_inc_agg)[names(mban_inc_agg) == "man_burn_area_new"] = "man_burn_area_new_inc_agg"
    # add it to fire_sevadj_df (fire_sevadj_df: 98 variables)
    fire_sevadj_df = merge(fire_sevadj_df, mban_inc_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"), all.x = TRUE)
    
    # 2b. ADJUST INCREAED SEVERITY AREAS
    # scale the increased severities so that total man burn area doesn't change
    # man burn area new = man_burn area new * (total man burn area - total man burn area new decreased) / total man burn area new increased
    fire_sevadj_df$man_burn_area_new[fire_sevadj_df$man_burn_area_new > fire_sevadj_df$man_burn_area] =
    	fire_sevadj_df$man_burn_area_new[fire_sevadj_df$man_burn_area_new > fire_sevadj_df$man_burn_area] *
    	(fire_sevadj_df$man_burn_area_agg[fire_sevadj_df$man_burn_area_new > fire_sevadj_df$man_burn_area] -
    	fire_sevadj_df$man_burn_area_new_dec_agg[fire_sevadj_df$man_burn_area_new > fire_sevadj_df$man_burn_area]) /
    	fire_sevadj_df$man_burn_area_new_inc_agg[fire_sevadj_df$man_burn_area_new > fire_sevadj_df$man_burn_area]
    
    # 3. now aggregate original managed and new managed burn area across management and get adjustments to fire_burn_area
    man_burn_area_agg = aggregate(man_burn_area ~ Land_Cat_ID + Region + Land_Type + Ownership + Severity, fire_sevadj_df, FUN=sum, na.rm = TRUE)
      # man_burn_area_new_agg includes Land_Cat_ID, Region, Land_Type, Ownership, Severity, man_burn_area_new
    man_burn_area_new_agg = aggregate(man_burn_area_new ~ Land_Cat_ID + Region + Land_Type + Ownership + Severity, fire_sevadj_df, FUN=sum, na.rm = TRUE)	
      # fire_burn_area_adj = difference between the new aggregated burn severity areas and the original burn severity areas. 
    man_burn_area_new_agg$fire_burn_area_adj = man_burn_area_new_agg$man_burn_area_new - man_burn_area_agg$man_burn_area
    	
    # put the severity adjustments ("fire_burn_area_adj") from man_burn_area_new_agg into fire_adjust_df (19 to 20 variables) 
    fire_adjust_df = merge(fire_adjust_df, man_burn_area_new_agg[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Severity", "fire_burn_area_adj")], 
                           by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Severity"), all.x = TRUE)
    # update fire_burn_area
    fire_adjust_df$fire_burn_area[!is.na(fire_adjust_df$fire_burn_area_adj)] =
    	fire_adjust_df$fire_burn_area[!is.na(fire_adjust_df$fire_burn_area_adj)] +
    	fire_adjust_df$fire_burn_area_adj[!is.na(fire_adjust_df$fire_burn_area_adj)]
    } else {
      # if there are not prescribed mmgmt practices and prescribed wildfire
      # assign 0's to fire_burn_area_adj and update fire_burn_area (may not even be necessary)
      # and fire_furn_area stays the same
      fire_adjust_df$fire_burn_area_adj <- 0
      # reorder columns to match fire_adjust_df for cases with prescribed management
      fire_adjust_df <- fire_adjust_df[,c("Land_Cat_ID","Region","Land_Type","Ownership","Severity",
                                          "avail_own_area", "fire_own_area_agg", "tot_area", "fire_own_area",           
                                          "Above2Atmos_frac", "StandDead2Atmos_frac", "Understory2Atmos_frac",   
                                          "DownDead2Atmos_frac", "Litter2Atmos_frac", "Above2StandDead_frac",    
                                          "Understory2DownDead_frac", "Below2Atmos_frac", "Soil2Atmos_frac",        
                                          "fire_burn_area", "fire_burn_area_adj")]
    }
    
    ############################################################################################################
    ################## fourth, estimate non-regenerated area #################
    ############################################################################################################
    
    # aggregate the burn area in each land cat (i.e., sum the severities)
    lc_burn_area = aggregate(fire_burn_area ~ Land_Cat_ID + Region + Land_Type + Ownership, fire_adjust_df, FUN=sum, na.rm = TRUE)
    names(lc_burn_area)[names(lc_burn_area) == "fire_burn_area"] = "lc_burn_area"
    
    # forest
    # first calculate stand decay coefficient (SDC) from ln(SDC) = -3.34*high sev fraction of burn area - 4
    fire_nonreg_df = merge(lc_burn_area, fire_adjust_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.y = TRUE)
    fire_nonreg_df$sdc = 0.0
    fire_nonreg_df$sdc[fire_nonreg_df$Land_Type == "Forest" & fire_nonreg_df$Severity == "High"] = 
    	exp(-3.34 * fire_nonreg_df$fire_burn_area[fire_nonreg_df$Land_Type == "Forest" & fire_nonreg_df$Severity == "High"] / 
    	fire_nonreg_df$lc_burn_area[fire_nonreg_df$Land_Type == "Forest" & fire_nonreg_df$Severity == "High"] - 4.0)
    # now estimate the high severity area that does not regenerate: hs non-reg = hs area / 10^(SDC*Dist)
    #	Dist is the distance from the forest/burn edge beyond which no regeneration occurs, in meters
    # do this only if NR_Dist >= 0
    fire_nonreg_df$non_regen_area = 0.0
    if (NR_Dist >= 0) {
    	fire_nonreg_df$non_regen_area[fire_nonreg_df$Land_Type == "Forest" & fire_nonreg_df$Severity == "High"] =
    		fire_nonreg_df$fire_burn_area[fire_nonreg_df$Land_Type == "Forest" & fire_nonreg_df$Severity == "High"] /
    		10^(fire_nonreg_df$sdc[fire_nonreg_df$Land_Type == "Forest" & fire_nonreg_df$Severity == "High"] * NR_Dist)
    }
    fire_nonreg_df[,c("sdc", "non_regen_area")] <- apply(fire_nonreg_df[,c("sdc", "non_regen_area")], 2, function (x) {replace(x, is.na(x), 0.00)})
    fire_nonreg_df[,c("sdc", "non_regen_area")] <- apply(fire_nonreg_df[,c("sdc", "non_regen_area")], 2, function (x) {replace(x, is.nan(x), 0.00)})
    fire_nonreg_df[,c("sdc", "non_regen_area")] <- apply(fire_nonreg_df[,c("sdc", "non_regen_area")], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    ############################################################################################################
    ################# fifth, calc changes in C densities for each of the fire effects within the ###############
    #########################  fire areas withn each landtypes-ownership combination ############################
    ############################################################################################################ 
    # loop over the fire frac columns to calculate the transfer carbon density for each frac column
    # the transfer carbon density is based on tot_area so that it can be aggregated and subtracted directly from the current density
    for (i in 1:num_firefrac_cols){
      # if the column names of C densities MgC/ha are not in the fire_adjust df (basically saying stop when done)
      if (!out_density_sheets[fire_density_inds[i]] %in% colnames(fire_adjust_df)) {
        # then merge the C density pool corresponding to the source of all the fire C transfer fractions 
        fire_adjust_df = merge(fire_adjust_df, 
                               out_density_df_list[[fire_density_inds[i]]][,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", next_density_label)], 
                               by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
        # and assign the name of the C density pool to the column
        names(fire_adjust_df)[names(fire_adjust_df) == next_density_label] = out_density_sheets[fire_density_inds[i]]
      }
      # lastly, fill in each of the fire C transfer = C density [Mg/ha] * fraction of effected C pool *  landtype-ownership fire area/ total area
      fire_adjust_df[,firec_trans_names[i]] = fire_adjust_df[,out_density_sheets[fire_density_inds[i]]] * fire_adjust_df[,fire_frac_names[i]] * 
        fire_adjust_df$fire_burn_area / fire_adjust_df$tot_area
    } # end for loop over the fire transfer fractions for calcuting the transfer carbon
    # clean up fire output
    fire_adjust_df = fire_adjust_df[order(fire_adjust_df$Land_Cat_ID, fire_adjust_df$Severity),]
    fire_adjust_df[,is.na(c(8:ncol(fire_adjust_df)))] <- 0
    fire_adjust_df[,is.nan(c(8:ncol(fire_adjust_df)))] <- 0
    fire_adjust_df[,(c(8:ncol(fire_adjust_df))) == Inf] <- 0
   
    ############################################################################################################
    ################## sixth, apply some decay over time for newly added dead material #################
    ############################################################################################################
    # above to stand dead and understory to down dead are these delayed decay pools
    # most of this will go to atmosphere over the decay period
    # can use existing stand dead and understory to atmos transfer variables
    # so the annual losses to atmosphere from these two pools are calculated here
    # and in the next step these losses are applied to these two pools by adding them to the "to atmosphere" transfers
    
    # calculate the decay for this year's new dead material and add it to the ongoing decay
    if (year == start_year) {
    	fire_decay_standdead = fire_adjust_df
    	fire_decay_downdead = fire_adjust_df
    	for (i in fire_decay_years) {
    		fcname = paste0("decay_year_",i)
    		fire_decay_standdead[,fcname] = 0.0
    		fire_decay_downdead[,fcname] = 0.0  
    	}
    } # done creating decay df if start_year
    
    for (i in fire_decay_years) {
    	fcname = paste0("decay_year_",i)
    	if (i == 1) {
    		# add the loss of new dead material
    		fire_decay_standdead[, fcname] = fire_decay_standdead[, fcname] + standdead_decay_frac[i] * fire_adjust_df$Above2StandDead_c
    		fire_decay_downdead[, fcname] = fire_decay_downdead[, fcname] + downdead_decay_frac[i] * fire_adjust_df$Understory2DownDead_c
    		# apply this year's loss to the transfer variables
    		fire_adjust_df$StandDead2Atmos_c = fire_adjust_df$StandDead2Atmos_c + fire_decay_standdead[,"decay_year_1"]
    		fire_adjust_df$DownDead2Atmos_c = fire_adjust_df$DownDead2Atmos_c + fire_decay_downdead[,"decay_year_1"]
    	} else {
    		# update the decay array
    		fcname_prev = paste0("decay_year_",i-1)
    		# add the loss of new dead material
    		fire_decay_standdead[, fcname_prev] = fire_decay_standdead[, fcname] + standdead_decay_frac[i] * fire_adjust_df$Above2StandDead_c
    		fire_decay_downdead[, fcname_prev] = fire_decay_downdead[, fcname] + downdead_decay_frac[i] * fire_adjust_df$Understory2DownDead_c
    		if (i == length(fire_decay_years)) {
    			# also zero out the last value
    			fire_decay_standdead[, fcname] = 0.0
    			fire_decay_downdead[, fcname] = 0.0
    		}
    	}
    } # end for i loop over the fire decay years
    
    ############################################################################################################
    ################ seventh, consolidate the changes in C densities within each C density pool #################
    ############################################################################################################
    # now consolidate the c density transfers to the pools
    # convert these to gains for consistency: all terrestrial gains are positive, losses are negative
    # store the names for aggregation below
    fire_agg_names = NULL
    # above
    # add a column called "Above_main_C_den_gain": above-main C density = -(above to atmos C) -(above to standing dead C)
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[3], "_gain"))
    fire_adjust_df[,fire_agg_names[1]] = -fire_adjust_df$Above2Atmos_c - fire_adjust_df$Above2StandDead_c
    # below
    # add a column called "Below_main_C_den_gain": root C density = -(root to atmos C)
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[4], "_gain"))
    fire_adjust_df[,fire_agg_names[2]] = -fire_adjust_df$Below2Atmos_c
    # understory
    # add a column called "Understory_C_den_gain": understory C density = -(understory to atmos C) -(understory to down dead C)
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[5], "_gain"))
    fire_adjust_df[,fire_agg_names[3]] = -fire_adjust_df$Understory2Atmos_c - fire_adjust_df$Understory2DownDead_c
    # standing dead
    # add a column called "StandDead_C_den_gain": standing dead C density = -(standing dead to atmos C) + (above-main to standing dead C)
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[6], "_gain"))
    fire_adjust_df[,fire_agg_names[4]] = -fire_adjust_df$StandDead2Atmos_c + fire_adjust_df$Above2StandDead_c
    # down dead
    # add a column called "DownDead_C_den_gain": down dead C density = -(down dead to atmos C) + (understory to down dead C)
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[7], "_gain"))
    fire_adjust_df[,fire_agg_names[5]] = -fire_adjust_df$DownDead2Atmos_c + fire_adjust_df$Understory2DownDead_c
    # litter
    # add a column called "Litter_C_den_gain": litter C density = -(litter to atmos C)
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[8], "_gain"))
    fire_adjust_df[,fire_agg_names[6]] = -fire_adjust_df$Litter2Atmos_c
    # soil
    # add a column called "Soil_orgC_den_gain": soil C density = -(soil to atmos C) 
    fire_agg_names = c(fire_agg_names, paste0(out_density_sheets[9], "_gain"))
    fire_adjust_df[,fire_agg_names[7]] = -fire_adjust_df$Soil2Atmos_c
    
    ############################################################################################################
    ################################## eighth, calc total C loss to atmosphere #################################  
    ############################################################################################################
    
    # to get the carbon must multiply these by the tot_area
    # atmos
    # calc fire C loss to atmosphere [Mg C] ("Land2Atmos_c_stock_fire") = -(total area [ha]) * (soil emissons [MgC/ha] + 
    # litter emissons [Mg/ha] + down dead emissons [Mg/ha] + standing dead emissions [Mg/ha] + understory emissons [Mg/ha] + 
    # root emissions [Mg/ha] + above-main emissions [Mg/ha])
    fire_agg_names = c(fire_agg_names, paste0("Land2Atmos_c_stock"))
    fire_adjust_df[,fire_agg_names[8]] = -fire_adjust_df$tot_area * (fire_adjust_df$Soil2Atmos_c + fire_adjust_df$Litter2Atmos_c + 
                                                                       fire_adjust_df$DownDead2Atmos_c + fire_adjust_df$StandDead2Atmos_c + 
                                                                       fire_adjust_df$Understory2Atmos_c + fire_adjust_df$Below2Atmos_c + 
                                                                       fire_adjust_df$Above2Atmos_c)
    # Partition the Land2Atmos_c_stock into total burned (CO2-C+CH4-C+BC-C) and total non-burned C emissions (CO2-C). Currently soil c and root c 
    # are assumed not to burn, and decay is captured in ongoing soil accum rates. (i.e. input values for wildfire effects fractions on 
    # root and soil c to atmosphere are currently 0. Update if contrary evidence is found)
    # first, calculate burned c emissions from wildfire
    fire_agg_names = c(fire_agg_names, paste0("Land2Atmos_BurnedC_stock"))
    fire_adjust_df[,fire_agg_names[9]] = -fire_adjust_df$tot_area * (fire_adjust_df$Litter2Atmos_c + fire_adjust_df$DownDead2Atmos_c + 
                                                                       fire_adjust_df$StandDead2Atmos_c + 
                                                                       fire_adjust_df$Understory2Atmos_c + fire_adjust_df$Above2Atmos_c)
    # second, calculate non-burned c emissions from wildfire
    # currently this is 0 as there's no lost root or soil c.
    fire_agg_names = c(fire_agg_names, paste0("Land2Atmos_NonBurnedC_stock"))
    fire_adjust_df[,fire_agg_names[10]] = fire_adjust_df[,fire_agg_names[8]] - fire_adjust_df[,fire_agg_names[9]]
    
    # check that fire Land2Atmos c flux is equal to the sum of burned and non-burned c stock in the fire_adjust_df
    identical(fire_adjust_df[,fire_agg_names[8]], fire_adjust_df[,fire_agg_names[9]] + fire_adjust_df[,fire_agg_names[10]])
    
    ############################################################################################################
    ######### ninth, aggregate changes in each C density pool within each landtype-ownership class ############# 
    ############################################################################################################
    # now aggregate to land type by summing the fire intensities
    # these c density values are the direct changes to the overall c density
    # the c stock values are the total carbon form each land type going to atmos

    # first, create table that has a row for each land type ID, and a column for each of the fire-caused C density change [MgC/ha], 
    # and corresponding C transfer to atmosphere [Mg C]  
    fire_agg_cols = array(dim=c(length(fire_adjust_df$Land_Cat_ID),length(fire_agg_names)))
    # second, populate the table by applying loop to each row's land type ID 
    for (i in 1:length(fire_agg_names)) {
       #fill columns with corresponding fire-caused C DENSITY CHANGES from the fire_adjust_df
      fire_agg_cols[,i] = fire_adjust_df[,fire_agg_names[i]]
    }
    
    # third, aggregate the C DENSITY CHANGES by summing within each land type-ownership combination and assign to fire_adjust_agg 
    fire_adjust_agg = aggregate(fire_agg_cols ~ Land_Cat_ID + Region + Land_Type + Ownership, data=fire_adjust_df, FUN=sum)
    #fourth, label the columns of the aggregated table 
    
    ####
    # change names and add to all_c_flux
    last_col = ncol(fire_adjust_agg)
    first_col = last_col - length(fire_agg_names) + 1
    fire_agg_names2 = paste0(fire_agg_names,"_fire_agg")
    names(fire_adjust_agg)[first_col:last_col] = fire_agg_names2
    # merge these values to the unman area table to apply the adjustments to each land type
    all_c_flux = merge(all_c_flux, fire_adjust_agg[c(1:4,first_col:last_col)], by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.na(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    # check that the fire Land2Atmos c flux is equal to the sum of burned and non-burned land2Atmos c flux in the all_c_flux dataframe
    # checks true
    identical(all_c_flux[,"Land2Atmos_BurnedC_stock_fire_agg"] + all_c_flux[,"Land2Atmos_NonBurnedC_stock_fire_agg"], all_c_flux[,"Land2Atmos_c_stock_fire_agg"])
    
    # loop over the relevant out density tables to update the carbon pools based on the fire fluxes
    # carbon cannot go below zero
    sum_change = 0
    sum_neg_fire = 0
    # for above-main C density through soil organic C density dataframes, do:
    for (i in 3:num_out_density_sheets) {
      # subset each of the columns representing aggregated fire-caused C density changes and the C emissions 
      # multiply by total area
      # sum them all 
      # this gives a single value for state-wide cumulative fire C changes [Mg C/y] -- used to make sure sure nothing is negative
      sum_change = sum_change + sum(all_c_flux[, fire_agg_names2[i-2]] * all_c_flux$tot_area)
      
      ############################################################################################################
      ################################### Lastly, UPDATE NEXT YEAR'S C DENSITIES #################################
      ############################################################################################################
      
      # add the corresponding fire-caused C density change to next year's C density
      out_density_df_list[[i]][, next_density_label] = out_density_df_list[[i]][, next_density_label] + all_c_flux[, fire_agg_names2[i-2]]
      # calc the total state-wide cumulative C not subtracted because it sends density negative (used as check to make sure it's minimal)
      neginds = which(out_density_df_list[[i]][, next_density_label] < 0)
      cat("neginds for out_density_df_list fire" , i, "are", neginds, "\n")
      sum_neg_fire = sum_neg_fire + sum(all_c_flux$tot_area[out_density_df_list[[i]][,next_density_label] < 0] * 
                                          out_density_df_list[[i]][out_density_df_list[[i]][,next_density_label] < 0, next_density_label])
      # replace any negative updated C densities with 0
      out_density_df_list[[i]][, next_density_label] <- replace(out_density_df_list[[i]][, next_density_label], 
                                                                out_density_df_list[[i]][, next_density_label] <= 0, 0.00)
    } # end loop over out densities for updating due to fire
    cat("fire carbon to atmosphere is ", sum_change, "\n")
    cat("fire negative carbon cleared is ", sum_neg_fire, "\n")
    
    ############################################################################################################
    ############################################################################################################
    ###################################  Apply LAND CONVERSIONS to C pools  ####################################
    ############################################################################################################
    ############################################################################################################
    
    # apply land conversion to the carbon pools (current year area and updated carbon)
    # as the changes are net, the land type area gains will be distributed proportionally among the land type area losses
    # the "to" land type columns are in land type id order and do not include seagrass because it is just an expansion
    #  do ocean/seagrass separately
    # operate within ownership categories and merge them back together at the end
    cat("Starting conversion c transfers\n")
    
    # need to adjust historical baseline by the management targets
    # managed adjustments are assumed to be independent of each other so the net adjustments are calculated
    # these initial annual rates are assumed to be included in the baseline annual change numbers:
    #  Growth (Afforestation was removed from this assumption in v2 and is now treated like a direct prescribed annual change)
    #  so the adjustment is based on a difference between the target annual change and the baseline annual change
    #  the baseline annual change is the year 2010 for these management types
    # urban forest is only tracked internally to determine the carbon accumulation rate,
    #  it is not used here in the context of land type converison
    # Restoration & Afforestation is an annual addition of a land type
    # assume that these entries do not need aggregation with other similar management activities within land type id
    #  in other words, there is only one area-changing management per land type id and each is dealt with uniquely
    
    # create dataframe for conversion adjustments: merge the annual net coversion area changes with the total area dataframe (All 10 Regions)
    conv_adjust_df = merge(tot_area_df, conv_area_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    # sort
    conv_adjust_df = conv_adjust_df[order(conv_adjust_df$Land_Cat_ID),]
    # duplicate column for annual net area conversions ("base_area_change") and call it "area_change"
    conv_adjust_df$area_change = conv_adjust_df$base_area_change
    # put the total area in the new area column for now
    # so that it can be adjusted as necessary by ownership below
    conv_adjust_df$new_area = conv_adjust_df$tot_area
    
    ############################################################################################################
    #########  FIRST, ADJUST BASELINE AREA CHANGE FOR RESTORATION, Afforestation, LIMITED GROWTH, & non-regen forest ######
    ############################################################################################################
    
    # These area change activities have a priority for available land type area that is set above when calculating man_area
    # Order:
    #	Afforestation (from shrubland, grassland)
    #	Meadow (from shrubland, grassland, woodland, savanna)
    #	Fresh_marsh and Coastal_marsh jointly (from cultivated)
    #	Woodland (from grassland and cultivated)
    # The resulting area needs are used to adjust the transition matrix according to these activities
    
    # merge the appropriate management data
    man_conv_df = man_adjust_df[man_adjust_df$Management == "Restoration" | man_adjust_df$Management == "Afforestation" | 
                                  man_adjust_df$Management == "Growth",1:7]
      # check if there any prescribed management practices
    if (nrow(man_adjust_df)>0) {
      man_conv_df = merge(man_conv_df, man_target_df[,1:6], by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"))
      names(man_conv_df)[names(man_conv_df) == start_area_label] = "initial_man_area"
    } # don't merge if there aren't any as there are only 5 columns in man_target_df (no column for start_area_label)
    
    conv_adjust_df = merge(conv_adjust_df, man_conv_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x=TRUE)
    conv_adjust_df = conv_adjust_df[order(conv_adjust_df$Land_Cat_ID),]
    
    # merge the fire non-regen area
    conv_adjust_df = merge(conv_adjust_df, 
    	fire_nonreg_df[fire_nonreg_df$Severity == "High", c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "non_regen_area")], 
    	by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x=TRUE)
    conv_adjust_df = conv_adjust_df[order(conv_adjust_df$Land_Cat_ID),]
    conv_adjust_df[,c(10:ncol(conv_adjust_df))] <- apply(conv_adjust_df[,c(10:ncol(conv_adjust_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
    
    # initialize the managed adjustments to base area change to zero
    conv_adjust_df$base_change_adjust = 0
    
    # merge the conversion fractions before splitting upon ownership
    conv_adjust_df = merge(conv_adjust_df, conv_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    conv_adjust_df = conv_adjust_df[order(conv_adjust_df$Land_Cat_ID),]
   # conv_col_names = unique(conv_adjust_df$Land_Type[conv_adjust_df$Land_Type != "Seagrass"])
    conv_col_names = unique(conv_adjust_df$Land_Type)
    num_conv_col_names = length(conv_col_names)
    own_names = unique(conv_adjust_df$Ownership)
    
    # initialize list for the following loop
   
    own_conv_df_list <- list()
    
    ############ START BIG LOOP that ultimately calc C TRANSFER for land conversions ############ 
    # outer loop over regions
    for (r in 1:length(unique(conv_adjust_df$Region))) {
    # get region-specific ownerships to send to inner ownership loop
      region.names <- unique(conv_adjust_df$Region) 
      # subset rows in conv_adjust_df that have the region ID equal to region.sames[[r]]
      # first get row indices 
      current_region_ID <- region.names[[r]]
      # second subset these specific region rows from conv_adjust_df
      region.specific.own <- conv_adjust_df[conv_adjust_df$Region == current_region_ID,]
      # third subset region-specific ownerships from own_names 
      own_names <- unique(region.specific.own$Ownership)
      own_conv_df_list_pre <- list()
      
    # loop over ownerships
    for (i in 1:length(own_names)) {
      # subset one ownership class at a time from the conversion adjustment table
      conv_own = region.specific.own[region.specific.own$Ownership == own_names[i],]  
      # get region-ownership-specific landtype names and number
      conv_col_names <- unique(conv_own$Land_Type)
      num_conv_col_names <- length(conv_col_names)
      # first need to adjust the baseline change rates and calculate the new area
      # the seagrass adjustment is separate
      if (current_region_ID == "Ocean") {
        conv_own$base_change_adjust[conv_own$Land_Type == "Seagrass" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$man_area[conv_own$Land_Type == "Seagrass" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
      } else {
        
        # calc growth adjustment before specific activities
        # change will be distributed to other land types proportionally within land type id, except for fresh marsh, water, and ice
        # because fresh marsh is only a restored type that is protected and water and ice do not change
        # note that developed land doesn't quite play out as prescribed when using the original landfire rs lullc data
        
        # calc adjustment to the baseline growth rate for limited growth management 
        # (temp_adjust) [ha] = current year limited growth area - initial limited growth area
        temp_adjust = conv_own$man_area[conv_own$Management == "Growth" & !is.na(conv_own$Management)] - 
          conv_own$initial_man_area[conv_own$Management == "Growth" & !is.na(conv_own$Management)]
        # paste value(s) in the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Management == "Growth" & !is.na(conv_own$Management)] = temp_adjust
        # proportionally distribute the reductions in urban growth area to other land types
        # for each landtype except developed and fresh marsh (not affected by these manipulations since all of it is protected): 
        # base_change_adjust [ha] = base_change_adjust - (sum of adjustments to baseline urban growth rate) *
        # (total area of landtype)/(total area of all the other landtypes)
        conv_own$base_change_adjust[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Water" & conv_own$Land_Type != "Ice"] = 
          conv_own$base_change_adjust[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Water" & conv_own$Land_Type != "Ice"] - 
          sum(temp_adjust) * conv_own$tot_area[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Water" & conv_own$Land_Type != "Ice"] / 
          sum(conv_own$tot_area[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Water" & conv_own$Land_Type != "Ice"])
          
##################################  AFFORESTATION  #####################################################
        # Afforestation activities will come proportionally out of _shrub_ and _grassland_ only
        # calc area adjustment for Afforestation (temp_adjust) [ha] = current year Afforestation area 
        temp_adjust = conv_own$man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] 
        # add the prescribed Afforestation area to the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] + temp_adjust
        # subset the base_change_adjust areas for shrub and grass, and subtract, proportionally, the sum of all the area adjustments 
        # for Afforestation (temp_adjust) in shrubland and grassland
        # store the needed land type area
        conv_own$frst_need = 0.00
        conv_own$frst_need[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] = sum(temp_adjust) * 
          conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] / 
          sum(conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"])
        conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] - 
          conv_own$frst_need[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"]
          
#################################  RESTORATION  ########################################################
     
######### coastal marsh restoration will come out of _agriculture_ land only
        # get area adjustment for COASTAL MARSH RESTORATION (temp_adjust) [ha] = current year management area 
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the COASTAL MARSH RESTORATION area to the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from Cultivated
        # store the needed land type area
        conv_own$cm_need = 0.00
        conv_own$cm_need[conv_own$Land_Type == "Cultivated"] =
        	sum(temp_adjust) * conv_own$tot_area[conv_own$Land_Type == "Cultivated"] / sum(conv_own$tot_area[conv_own$Land_Type == "Cultivated"])
        conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] = conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] - 
          conv_own$cm_need[conv_own$Land_Type == "Cultivated"]
        
######### fresh marsh restoration will come out of _agriculture_ land only
        # get area adjustment for FRESH MARSH RESTORATION (temp_adjust) [ha] = current year management area 
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the FRESH MARSH RESTORATION area to the column for base_change_adjust
        conv_own$base_change_adjust[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from Cultivated
        # store the needed land type area
        conv_own$fm_need = 0.00
        conv_own$fm_need[conv_own$Land_Type == "Cultivated"] =
        	sum(temp_adjust) * conv_own$tot_area[conv_own$Land_Type == "Cultivated"] / sum(conv_own$tot_area[conv_own$Land_Type == "Cultivated"])
        conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] = conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] - 
          conv_own$fm_need[conv_own$Land_Type == "Cultivated"]
        
######### meadow restoration will come proportionally out of _shrubland_, _grassland_, _savanna_, _woodland_ only
        # get area adjustment for MEADOW RESTORATION (temp_adjust) [ha] = current year management area
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the MEADOW RESTORATION area to the column for base_change_adjust  
        conv_own$base_change_adjust[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from SHRUBLAND, GRASSLAND, & SAVANNA and woodland
        # subtract the available area needed for afforestation
        # store the needed land type area
        conv_own$mdw_need = 0.00
        conv_own$mdw_need[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
        	conv_own$Land_Type == "Woodland"] = sum(temp_adjust) * 
          	(conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
          		conv_own$Land_Type == "Woodland"] - conv_own$frst_need[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" |
          		conv_own$Land_Type == "Savanna" | conv_own$Land_Type == "Woodland"]) / 
          	sum( (conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
          		conv_own$Land_Type == "Woodland"] - conv_own$frst_need[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" |
          		conv_own$Land_Type == "Savanna" | conv_own$Land_Type == "Woodland"]) )
        conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
        	conv_own$Land_Type == "Woodland"] = 
          	conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" |
          		conv_own$Land_Type == "Woodland"] - 
            conv_own$mdw_need[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
        		conv_own$Land_Type == "Woodland"]

######### woodland restoration will come proportionally out of _grassland_ and cultivated only
        # get area adjustment for WOODLAND RESTORATION (temp_adjust) [ha] = current year management area
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the WOODLAND RESTORATION area to the column for base_change_adjust  
        conv_own$base_change_adjust[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from GRASSLAND, & cultivated
        # subtract the available area needed for afforestation and meadow and wetland
        # store the needed land type area
        conv_own$wd_need = 0.00
        conv_own$wd_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] = sum(temp_adjust) * 
          	(conv_own$tot_area[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] - 
          		conv_own$frst_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] -
          		conv_own$mdw_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] -
          		conv_own$cm_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] -
          		conv_own$fm_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"]) / 
          	sum( (conv_own$tot_area[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] - 
          		conv_own$frst_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] -
          		conv_own$mdw_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] -
          		conv_own$cm_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] -
          		conv_own$fm_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"]) )
        conv_own$base_change_adjust[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] = 
          	conv_own$base_change_adjust[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"] - 
            conv_own$wd_need[conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Cultivated"]
        		
##################################  Non-regeneration of forest after fire  #####################################################
        # non-regenerated forest will become shrubland, or grassland if shrubland does not exist in this region-ownership
        # so make the adjustment negative
        # i don't think it is necessary, based on the input land categories, but use savanna as a fall-back
        temp_adjust = - conv_own$non_regen_area[conv_own$Land_Type == "Forest"] 
        # add the negative value for non-regen area to the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Land_Type == "Forest"] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Forest"] + temp_adjust
        # add the lost forest area to shrubland (or grassland if necessary)
        # store the added area
        conv_own$nonreg_add = 0.00
        if (nrow(conv_own[conv_own$Land_Type == "Shrubland",]) > 0) {
        	conv_own$nonreg_add[conv_own$Land_Type == "Shrubland"] = sum(temp_adjust)
        	conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland"] = 
        		conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland"] - conv_own$nonreg_add[conv_own$Land_Type == "Shrubland"]       
      	} else if (nrow(conv_own[conv_own$Land_Type == "Grassland",]) > 0) {
      		conv_own$nonreg_add[conv_own$Land_Type == "Grassland"] = sum(temp_adjust)
      		conv_own$base_change_adjust[conv_own$Land_Type == "Grassland"] = 
        		conv_own$base_change_adjust[conv_own$Land_Type == "Grassland"] - conv_own$nonreg_add[conv_own$Land_Type == "Grassland"]
        		cat("Warning: forest to grassland due to lack of potential converted land types\n")
      	} else if (nrow(conv_own[conv_own$Land_Type == "Savanna",]) > 0) {
      		conv_own$nonreg_add[conv_own$Land_Type == "Savanna"] = sum(temp_adjust)
      		conv_own$base_change_adjust[conv_own$Land_Type == "Savanna"] = 
        		conv_own$base_change_adjust[conv_own$Land_Type == "Savanna"] - conv_own$nonreg_add[conv_own$Land_Type == "Savanna"]
        		cat("Warning: forest to savanna due to lack of potential converted land types\n")
      	} else {
      		conv_own$base_change_adjust[conv_own$Land_Type == "Forest"] = 
          		conv_own$base_change_adjust[cconv_own$Land_Type == "Forest"] - temp_adjust
      		cat("Warning: regenerating all forest after fire due to lack of potential converted land types\n")
      	}        
      } # end else calc land adjusments to baseline area change
      
      # clean up division numerical errors
      conv_own$base_change_adjust[is.nan(conv_own$base_change_adjust)] = 0
      conv_own$base_change_adjust[conv_own$base_change_adjust == Inf] = 0
      
      # calc the area change and the new area
      # recall AREA CHANGE [ha] starts as annual net area conversions ("base_area_change")
      # so add to it the newly calculated base_change_adjust:
      conv_own$area_change = conv_own$base_area_change + conv_own$base_change_adjust
      # and recalc NEW AREA [ha] = LANDTYPE AREA + (adjusted) AREA CHANGE
      conv_own$new_area = conv_own$tot_area + conv_own$area_change
      
      ######### ENSURE PROTECTION OF AFFORESTED AREA & RESTORED MARSH & MEADOW & WOODLAND AREA VIA AREA CHANGE ADJUSTMENT ########################  
      # first adjust the new area and area change to account for the protection of afforested area & restored fresh marsh, meadow and coastal 
      # marsh
      # this also accounts for new area going negative
      # new area should always be greater than the cumulative afforested & restored management area so there is no loss of protected area over time.
      # thus, for forest afforestation, fresh or coastal marsh, or meadow, if new area < cumulative management area, do the following two steps: 
      # (1) CORRECT "AREA CHANGE" for PROTECTED AREA by adding the deficit area to the area change
      # get indices for land categories that don't have at least new area equal to the cumulative protected area
      
      # assume that non-regen areas are replanted to meet these overall restoration targets that are "protected"
      #		the fire c losses are still incurred this year, but the cumulative restored area is replanted as necessary
      
      protected_deficit_inds <- which((conv_own$new_area < conv_own$man_area_sum) & 
                                        ((conv_own$Management == "Afforestation" & !is.na(conv_own$Management)) | conv_own$Land_Type == "Fresh_marsh" | 
                                           conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh" |
                                           conv_own$Land_Type == "Woodland")) 
      # if these indices exist, then
      if (length(protected_deficit_inds)>0) { 
      # add the deficit to the area change (effectively will set new_area = man_area_sum)  
        conv_own$area_change[protected_deficit_inds] <- conv_own$area_change[protected_deficit_inds] + (conv_own$man_area_sum[protected_deficit_inds] - 
          conv_own$new_area[protected_deficit_inds])
      # (2) SUM PROTECTED AREA DEFICIT 
      # sum_restored_neg = -1 * ( (aggregate sum all the above cumulative management areas > new areas) - new area )
      sum_restored_neg = -sum(conv_own$man_area_sum[protected_deficit_inds] - conv_own$new_area[protected_deficit_inds])
      # (3) CORRECT "NEW AREA" for PROTECTED AREA 
      # NEW AREA = CUMULATIVE MANAGEMENT AREA 
      conv_own$new_area[protected_deficit_inds] = conv_own$man_area_sum[protected_deficit_inds]
      } else {sum_restored_neg <- 0}
      
      # (4) CORRECT "AREA CHANGE" FOR NEGATIVE NEW AREAS
      # if new area is negative, add the magnitude of the negative area to the area_change and subtract the difference proportionally from the 
      # positive area changes (except from protected forest, fresh marsh, coastal marsh, and meadow - make sure not negated by changes), then calc 
      # new area again
      # AREA CHANGE = AREA CHANGE - NEW AREA (this later sets new area to 0)
      conv_own$area_change[conv_own$new_area < 0] = conv_own$area_change[conv_own$new_area < 0] - conv_own$new_area[conv_own$new_area < 0]
      
      # (5) SUM NEGATIVE NEW AREAS + PROTECTED AREA DEFICIT (i.e. total restored area)
      # sum_neg_new = SUM NEGATIVE NEW AREAS + PROTECTED AREA DEFICIT
      sum_neg_new = sum(conv_own$new_area[conv_own$new_area < 0]) + sum_restored_neg
     
      # (6) From expanding forest (with afforestation), meadow and marsh, where new_area >= man_area_sum & man_area_sum >= tot_area, temporarily set aside whatever 
      # area is required for: (new_area - area_change >= man_area_sum), so that these cases can have the unprotected portion of area_change adjusted, and what is 
      # excluded from adjustments and changes
      area_change_protect_inds <- which((conv_own$new_area >= conv_own$man_area_sum) & (conv_own$man_area_sum >= conv_own$tot_area) & (conv_own$area_change > 0) & 
                                          ((conv_own$Management == "Afforestation" & !is.na(conv_own$Management)) | 
                                             conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | 
                                             conv_own$Land_Type == "Coastal_marsh" | conv_own$Land_Type == "Woodland"))
      		conv_own$area_change[area_change_protect_inds] <- conv_own$area_change[area_change_protect_inds] - 
      		(conv_own$man_area_sum[area_change_protect_inds] - conv_own$tot_area[area_change_protect_inds]) 
     
      # (7) SUM EXPANDING AREAS, INCLUDING UNPROTECTED AREA in FOREST, FRESH & COASTAL MARSH, and MEADOW (excludes protected land categories with no unprotected area)
      # sum_pos_change = SUM EXPANDING AREAS
      sum_pos_change = sum(conv_own$area_change[conv_own$area_change > 0 & !(conv_own$area_change %in% conv_own$area_change[protected_deficit_inds])])
      # (8) CORRECT "AREA CHANGE" of all EXPANDING UNPROTECTED areas (includes unprotected area in forest, marshes and meadow) by
      # subtracting the proportional sum of negative new areas from the positive area changes: 
      # AREA CHANGE = AREA CHANGE + (sum_neg_new * area change / sum_pos_change)
      conv_own$area_change[conv_own$area_change > 0 & !(conv_own$area_change %in% conv_own$area_change[protected_deficit_inds])] <- 
        conv_own$area_change[conv_own$area_change > 0 & !(conv_own$area_change %in% conv_own$area_change[protected_deficit_inds])] + 
        sum_neg_new * conv_own$area_change[conv_own$area_change > 0 & !(conv_own$area_change %in% conv_own$area_change[protected_deficit_inds])] / 
        sum_pos_change
      # (9) Add back the protected area that was temporarily subtracted from area_change to the adjusted area change 
      conv_own$area_change[area_change_protect_inds] <- conv_own$area_change[area_change_protect_inds] + 
        (conv_own$man_area_sum[area_change_protect_inds] - conv_own$tot_area[area_change_protect_inds]) 
      
      # (10) update new area by adding adjusted area changes to the total areas for each landtype-ownership-region combination: 
      # NEW AREA [ha] = TOTAL AREA + AREA CHANGE
      conv_own$new_area = conv_own$tot_area + conv_own$area_change
      
      # check if any of the area changes resulted in negative new area and correct them
      while (any(conv_own$new_area < 0)) {
        # subset any land categories that have negative new areas and add them up to get the area needed to be offset to avoid negative new area
        sum_neg_new_area <- sum(conv_own$new_area[conv_own$new_area < 0])
        # create temporary column for new_area: new_area_temp_df$new_area
        new_area_temp_df <- conv_own
        # get row indices for land categories with protected area and additional unprotected area
        unprotected_inds <- which((new_area_temp_df$new_area > new_area_temp_df$man_area_sum) & 
                                    ((new_area_temp_df$Management == "Afforestation" & !is.na(new_area_temp_df$Management)) |
                                    new_area_temp_df$Land_Type == "Fresh_marsh" | new_area_temp_df$Land_Type == "Meadow" | 
                                    new_area_temp_df$Land_Type == "Coastal_marsh" | new_area_temp_df$Land_Type == "Woodland")) 
        # get row indices to exclude from area correction because they are fully protected
        exclude_inds <- which((conv_own$new_area == conv_own$man_area_sum) & ((conv_own$Management == "Afforestation" & !is.na(conv_own$Management)) | 
                                                                                conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | 
                                                                                conv_own$Land_Type == "Coastal_marsh" | 
                                                                                conv_own$Land_Type == "Woodland"))
        # temporarily replace new_area with the excess unprotected area (man_area_sum - new_area) for forest/afforestation & fresh_marsh, meadow and coastal_marsh
        new_area_temp_df$new_area[unprotected_inds] <- new_area_temp_df$new_area[unprotected_inds] - new_area_temp_df$man_area_sum[unprotected_inds]
        # sum all the positive new areas, excluding protected area
        sum_pos_new_area <- sum(new_area_temp_df$new_area[new_area_temp_df$new_area > 0 & 
                                                            !(new_area_temp_df$new_area %in% new_area_temp_df$new_area[exclude_inds])]) 
        # ADJUST "AREA CHANGE" for all postive new_area land categories (for the negative new areas) by adding the negative area offset (sum_neg_new_area) 
        # proportionally (w.r.t. new_area) from the area_change (lose more from contracting lands, and gain less for expanding lands)
        new_area_temp_df$area_change[new_area_temp_df$new_area > 0 & !(new_area_temp_df$new_area %in% new_area_temp_df$new_area[exclude_inds])] <- 
          new_area_temp_df$area_change[new_area_temp_df$new_area > 0 & !(new_area_temp_df$new_area %in% new_area_temp_df$new_area[exclude_inds])] +
           sum_neg_new_area * new_area_temp_df$new_area[new_area_temp_df$new_area > 0 & 
                                                          !(new_area_temp_df$new_area %in% new_area_temp_df$new_area[exclude_inds])] / sum_pos_new_area
        # CORRECT "AREA CHANGE" for all negative new_area land categories by subtracting their negative new_area from it (effectively 0's it)
        new_area_temp_df$area_change[new_area_temp_df$new_area < 0] <- new_area_temp_df$area_change[new_area_temp_df$new_area < 0] - 
          new_area_temp_df$new_area[new_area_temp_df$new_area < 0] 
        # CORRECT "NEW AREA" for negative new area land categories by assigning 0 to them to avoid roundoff errors
        new_area_temp_df$new_area[new_area_temp_df$new_area < 0] <- 0
        # CORRECT "NEW AREA" for postive new area land categories by adding the adjusted area_change to the tot_area
        new_area_temp_df$new_area[new_area_temp_df$new_area > 0] = new_area_temp_df$tot_area[new_area_temp_df$new_area > 0] + 
          new_area_temp_df$area_change[new_area_temp_df$new_area > 0]
        # add back the protected portion of new areas to thte rows it was subtracted from (unprotected_inds)
        new_area_temp_df$new_area[unprotected_inds] <- new_area_temp_df$new_area[unprotected_inds] + new_area_temp_df$man_area_sum[unprotected_inds]
        # replace conv_own with new_area_temp_df which has the corrected new_area and area_change
        conv_own <- new_area_temp_df
      } # end while loop
      
      # check if sum(new_area) > sum(tot_area)  
      if (all((sum(conv_own$new_area) > sum(conv_own$tot_area)) & conv_own$Region != "Ocean")) {
        # get sum of area deficit
        new_area_deficit <- sum(conv_own$new_area) - sum(conv_own$tot_area)
        # get sum of postive area changes
        sum_pos_change <- sum(conv_own$area_change[conv_own$area_change > 0]) 
        # subtract this proprtionally from the posisitve area_changes with respect to area_change
        conv_own$area_change[conv_own$area_change > 0] <- conv_own$area_change[conv_own$area_change > 0] - 
          (new_area_deficit * ((conv_own$area_change[conv_own$area_change > 0])/sum_pos_change))
        # recalc the new_area for all
        conv_own$new_area <- conv_own$area_change + conv_own$tot_area
      }
      
      ##### to do: check for a better way to do this
      # this case could arise when changes should still occur (e.g non-restoration increases in coastal marsh with increases in dveloped all)
      # check if there is any land left to do land conversions in the first place
      if (sum(conv_own$tot_area[conv_own$Land_Type == "Coastal_marsh" | conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow"]) == 
          sum(conv_own$tot_area)) { 
        # if all the area is in potential restored areas, then set all area_change == 0 to avoid round-off error
        conv_own$area_change <- 0.00
      }
    
      ######################################## NOW AREA CHANGES & NEW AREAS ARE CORRECT ######################################## 
      
      ################################ CALC SPECIFIC FROM/TO and TO/FROM AREA CONVERSIONS ######################################
      # need the specific amount of area conversions (from/to and to/from) to calculate the C transfers and density changes
      # calculate the conversion area matrices by ownership
      # these store the area change from the Land_Type column to the individual land type, by ownership
      # a from(row)-to(col) value is positive, a to(row)-from(col) value is negative
      # carbon needs to be subracted for the area losses because the density change values are tracked as normalized carbon
      
      # do only land here because ocean/seagrass is different
      if(current_region_ID != "Ocean") {
        
        # didn't find this case yet, but it may happen somewhere at some point? in the delta?
        ## try distributing generic transitions before specific adjustments (use base_area_change instead of area_change)
        # this is so that the carbon transitions are correct# this also means that the specific transitions do not need to be redistributed
        #  because they are not included yet, and by adding them area_change should be met
        
        # add up all positive area changes in new column "own_gain_sum" 
        conv_own$own_gain_sum = sum(conv_own$base_area_change[conv_own$base_area_change > 0])
        # duplicate dataframe and call it conv_own2 
        conv_own2 = conv_own
        # loop over the land types to get the positive from-to area values (from-to: expanding landtypes. if constant then value is 0)
        for (l in 1:length(conv_own$Land_Type)) {
          # l is landtype index. add new columns that are the gains in area for each land type
          conv_own[,conv_own$Land_Type[l]] = 0.0
          # for each from-to land type column, go to rows of decreasing landtypes: 
          # from-to value = absolute(neg area change) * (area change of the gaining land type)/(sum all gaining areas) 
          conv_own[,conv_own$Land_Type[l]][conv_own$base_area_change < 0] = - conv_own$base_area_change[conv_own$base_area_change < 0] * 
            conv_own$base_area_change[l] / conv_own$own_gain_sum[l]
        } # end for l loop over land type
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, x < 0, 0.00)})
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, is.nan(x), 0.00)})
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, x == Inf, 0.00)})
        
        # do it again to get the negative to-from values
        for (l in 1:length(conv_own$Land_Type)) {
          conv_own2[,conv_own2$Land_Type[l]] = 0.0
          conv_own2[,conv_own2$Land_Type[l]][conv_own2$base_area_change > 0] = - conv_own2$base_area_change[conv_own2$base_area_change > 0] * 
            conv_own2$base_area_change[l] / conv_own2$own_gain_sum[l]
        } # end for l loop over land type
        conv_own2[,conv_own2$Land_Type] <- apply(conv_own2[,conv_own2$Land_Type], 2, function (x) {replace(x, x < 0, 0.00)})
        conv_own2[,conv_own2$Land_Type] <- apply(conv_own2[,conv_own2$Land_Type], 2, function (x) {replace(x, is.nan(x), 0.00)})
        conv_own2[,conv_own2$Land_Type] <- apply(conv_own2[,conv_own2$Land_Type], 2, function (x) {replace(x, x == Inf, 0.00)})
        
        ## end of sectoin where area_change was replaced with base_area_change
        
        
        # put the negative to-from values into conv_own
        # first find which columns are empty
        zinds = which(apply(conv_own[,conv_col_names],2,sum) == 0)
        conv_own[,conv_col_names][,zinds] = -conv_own2[,conv_col_names][,zinds]

        # now adjust these conversions based on the specific non-growth conversions above (restoration and non-regen), after area adjustment
        #  growth has already been distributed proportionally to the appriate land types
        # the actual gross managed value is the smaller of: area_change minus base_area_change, and man_area
        #  assuming man_area is constrained by the available area to expand into
        # and use the non-regen area directly
        # first get the initial sum of row gains for redistributing the transitions
        # don't operate on the diagnonal, which should always be 0
        conv_own$row_gain_sum = apply(conv_own[,conv_col_names],1,FUN= function(x) {sum(x[which(x > 0)], na.rm=TRUE)})
        conv_own$row_loss_sum = apply(conv_own[,conv_col_names],1,FUN= function(x) {sum(x[which(x < 0)], na.rm=TRUE)})
        conv_own$row_change_sum = conv_own$row_gain_sum - conv_own$row_loss_sum
        num_avail_land_types = length(conv_own$Land_Type[conv_own$Land_Type != "Water" & conv_own$Land_Type != "Ice"]) - 1
        if (num_avail_land_types < 0 ){num_avail_land_types = 0}
        for (l in 1:length(conv_own$Land_Type)) {
        	### afforestation
        	if (conv_col_names[l] != "Forest" & length(conv_own$man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)]) > 0) {
        	man_area_adj = min(conv_own$man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)], 
        		conv_own$area_change[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] -
        		conv_own$base_area_change[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)])
        	scalar = man_area_adj / conv_own$man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)]
        	if(FALSE) {
        	# first distribute this adjusted loss area as gains to the changing columns proprtionally to maintain area_change
        	# check for a net zero change for land type l to avoid an error
        	# if no avail land types then this doesn't matter and will be cleaned up later
        	if (conv_own$row_change_sum[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] == 0) {
        		if (conv_col_names[l] != "Water" & conv_col_names[l] != "Ice") {
        			conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        				conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        				abs(man_area_adj / num_avail_land_types)
        		}
        	} else {
        		conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        			conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        			abs(man_area_adj * conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] /
        			conv_own$row_change_sum[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)])	
        	}
        	} # end FALSE
        	# scale the needed transitions by the adjusted managed area and subtract transition for the row value
        	conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] = 
        		(conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] -
        		scalar * conv_own$frst_need[conv_own$Land_Type == conv_col_names[l]])
        	# fill in the column for this land type with the negative of the row value
        	conv_own[l,"Forest"] = -conv_own[conv_own$Management == "Afforestation" & !is.na(conv_own$Management),conv_own$Land_Type[l]] 
        	} # end afforestation
        	
        	### coastal marsh
        	if (conv_col_names[l] != "Coastal_marsh" & length(conv_own$man_area[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]) > 0) {
        	man_area_adj = 
        		min(conv_own$man_area[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)], 
        		conv_own$area_change[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] -
        		conv_own$base_area_change[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	scalar = man_area_adj / 
        		conv_own$man_area[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        	if(FALSE) {
        	# first distribute this adjusted loss area as gains to the changing columns proprtionally to maintain area_change
        	# check for a net zero change for land type l to avoid an error
        	# if no avail land types then this doesn't matter and will be cleaned up later
        	if (conv_own$row_change_sum[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] == 0) {
        		if (conv_col_names[l] != "Water" & conv_col_names[l] != "Ice") {
        			conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        				conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        				abs(man_area_adj / num_avail_land_types)
        		}
        	} else {
				conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        			conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        			abs(man_area_adj * 
        			conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] /
        			conv_own$row_change_sum[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	}
        	} # end FALSE
        	# scale the needed transitions by the adjusted managed area and subtract transition
        	conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] = 
        		(conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] -
        		scalar * conv_own$cm_need[conv_own$Land_Type == conv_col_names[l]])
        	# fill in the column for this land type with the negative of the row value
        	conv_own[l,"Coastal_marsh"] = 
        		-conv_own[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]]
        	} # end coastal marsh
        		
        	### fresh marsh
        	if (conv_col_names[l] != "Fresh_marsh" & length(conv_own$man_area[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]) > 0) {
        	man_area_adj = 
        		min(conv_own$man_area[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)], 
        		conv_own$area_change[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] -
        		conv_own$base_area_change[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	scalar = man_area_adj / 
        		conv_own$man_area[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        	if(FALSE) {
        	# first distribute this adjusted loss area to the gain columns proprtionally to maintain area_change
        	# check for a net zero change for land type l to avoid an error
        	# if no avail land types then this doesn't matter and will be cleaned up later
        	if (conv_own$row_change_sum[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] == 0) {
        		if (conv_col_names[l] != "Water" & conv_col_names[l] != "Ice") {
        			conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        				conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        				abs(man_area_adj / num_avail_land_types)
        		}
        	} else {
           		conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        			conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        			abs(man_area_adj * 
        			conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] /
        			conv_own$row_change_sum[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	}
        	} # end FALSE
        	# scale the needed transitions by the adjusted managed area and subtract transition
        	conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] = 
        		(conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] -
        		scalar * conv_own$fm_need[conv_own$Land_Type == conv_col_names[l]])
        	# fill in the column for this land type with the negative of the row value
        	conv_own[l,"Fresh_marsh"] = 
        		-conv_own[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]]
        	} # end fresh marsh
        		
        	### meadow
        	if (conv_col_names[l] != "Meadow" & length(conv_own$man_area[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]) > 0) {
        	man_area_adj = 
        		min(conv_own$man_area[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)], 
        		conv_own$area_change[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] -
        		conv_own$base_area_change[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	scalar = man_area_adj / 
        		conv_own$man_area[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        	if(FALSE) {
        	# first distribute this adjusted loss area to the gain columns proprtionally to maintain area_change
        	# check for a net zero change for land type l to avoid an error
        	# if no avail land types then this doesn't matter and will be cleaned up later
        	if (conv_own$row_change_sum[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] == 0) {
        		if (conv_col_names[l] != "Water" & conv_col_names[l] != "Ice") {
        			conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        				conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        				abs(man_area_adj / num_avail_land_types)
        		}
        	} else {
        		conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        			conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        			abs(man_area_adj * 
        			conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] /
        			conv_own$row_change_sum[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	}
        	} # end FALSE
        	# scale the needed transitions by the adjusted managed area and subtract transition
        	conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] = 
        		(conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] -
        		scalar * conv_own$mdw_need[conv_own$Land_Type == conv_col_names[l]])
        	# fill in the column for this land type with the negative of the row value
        	conv_own[l,"Meadow"] = 
        		-conv_own[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]]
        	} # end meadow
        		
        	### woodland
        	if (conv_col_names[l] != "Woodland" & length(conv_own$man_area[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]) > 0) {
        	man_area_adj = 
        		min(conv_own$man_area[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)], 
        		conv_own$area_change[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] -
        		conv_own$base_area_change[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	scalar = man_area_adj / 
        		conv_own$man_area[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        	if(FALSE) {
        	# first distribute this adjusted loss area to the gain columns proprtionally to maintain area_change
        	# check for a net zero change for land type l to avoid an error
        	# if no avail land types then this doesn't matter and will be cleaned up later
        	if (conv_own$row_change_sum[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] == 0) {
        		if (conv_col_names[l] != "Water" & conv_col_names[l] != "Ice") {
        			conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        				conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        				abs(man_area_adj / num_avail_land_types)
        		}
        	} else {
        		conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] =
        			conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] + 
        			abs(man_area_adj * 
        			conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] /
        			conv_own$row_change_sum[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)])
        	}
        	} # end FALSE
        	# scale the needed transitions by the adjusted managed area and subtract transition
        	conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] = 
        		(conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]] -
        		scalar * conv_own$wd_need[conv_own$Land_Type == conv_col_names[l]])	
        	# fill in the column for this land type with the negative of the row value
        	conv_own[l,"Woodland"] = 
        		-conv_own[conv_own$Land_Type == "Woodland" & conv_own$Management == "Restoration" & !is.na(conv_own$Management),conv_own$Land_Type[l]]
        	} # end woodland
        		
        	### nonregen
        	if (conv_col_names[l] != "Forest" & length(conv_own$Land_Type[conv_own$Land_Type == "Forest"]) > 0) {
        	if(FALSE) {
        	# first distribute this gain area to the loss columns proprtionally to maintain area_change
        	# check for a net zero change for land type l to avoid an error
        	# if no avail land types then this doesn't matter and will be cleaned up later
        	if (conv_own$row_change_sum[conv_own$Land_Type == "Forest"] == 0) {
        		if (conv_col_names[l] != "Water" & conv_col_names[l] != "Ice") {
        			conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] =
        				conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] - 
        				abs(conv_own$non_regen_area[conv_own$Land_Type == "Forest"] / num_avail_land_types)
        		}
        	} else {
        		conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] =
        			conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] - 
        			abs(conv_own$non_regen_area[conv_own$Land_Type == "Forest"] * conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] /
        			conv_own$row_change_sum[conv_own$Land_Type == "Forest"])
        	}
        	} # end FALSE
        	# add transition - recall that this nonreg_add value is negative
        	conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] = 
        		(conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]] -
        		conv_own$nonreg_add[conv_own$Land_Type == conv_col_names[l]])
        	# fill in the column for this land type with the negative of the row value
        	conv_own[l,"Forest"] =
        		-conv_own[conv_own$Land_Type == "Forest",conv_own$Land_Type[l]]
        	} # end non regen
        		
        } # end for l loop over land types to incorporate specific transitions
        
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, is.na(x), 0.00)})
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, is.nan(x), 0.00)})
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, x == Inf, 0.00)})
        
        
        ############################# calc 'FROM' land type losses due to conversion to ag and developed ############################# 
        # if ag or developed is shrinking, then there is no conversion flux 
        # (ag and urban losses don't use conversion fractions, but gains are dictated by the input conversion fractions)
        # calc from land type losses due to conversion to ag and developed
        # if there is an ag or urban loss, then there is no conversion flux
        # assume that these losses are immediate
        # a comprehensive study shows that most soil c loss in conversion to ag happens within the first 3-5 years
        # ag and developed only have above main c, so only need to adjust this as new area with zero carbon
        # loop over the ag/dev conversion frac columns to calculate the transfer carbon density for each frac column
        # this applies to the ag and developed columns only, and add these two areas to get one adjustment
        # the carbon density change is based on land type id tot_area so that it can be aggregated and subtracted directly from the current 
        # density values
        # probably should deal with the remaining c transfer here, rather than below, so that all transfers are included
        
        # calculate all the c losses from landtypes that convert to ag or urban
        for (f in 1:num_convfrac_cols) {
          # the removed values are calculated first, so this will work
          # if conv_density_inds[f] == -1 or -2, then the source is the harvested pool or slash pool, so use the sum of its components respectively
          if (conv_density_inds[f] == -1 | conv_density_inds[f] == -2) {
            # FROM HARVESTED POOL
            if (conv_density_inds[f] == -1) { 
             # Harvested2Wood_conv_c, Harvested2Energy_conv_c, Harvested2Decay_conv_c, Harvested2Slash_conv_c) [Mg/ha] = (Above_harvested_conv_c + 
              # StandDead_harvested_conv_c) * (Harvested2Wood_conv_frac, Harvested2Energy_conv_frac, Harvested2Decay_conv_frac, Harvested2Slash_conv_frac)
            conv_own[,convc_trans_names[f]] = (conv_own[,convc_trans_names[1]] + conv_own[,convc_trans_names[2]]) * conv_own[,conv_frac_names[f]]
            } else {
            # FROM SLASH POOL
            # else conv_density_inds[f] == -2 (when f = 10, 11, 12, 13)
            # sum c trans columns 6, 7, 8 & 9 (Harvest2Slash_conv_c + Under2Slash_conv_c + DownDead2Slash_conv_c + Litter2Slash_conv_c)
            # (slash2energy_conv_c, slash2wood_conv_c, Burn_conv_c, slash2decay_conv_c) [Mg/ha] = (Harvested2Slash_conv_c + Under2Slash_conv_c + 
            # DownDead2Slash_conv_c + Litter2Slash_conv_c) * (Harvested2Slash_conv_frac, Under2Slash_conv_frac, DownDead2Slash_conv_frac, Litter2Slash_conv_frac)
            conv_own[,convc_trans_names[f]] = (conv_own[,convc_trans_names[6]] + conv_own[,convc_trans_names[7]] + conv_own[,convc_trans_names[8]] +
                                                 conv_own[,convc_trans_names[9]]) * conv_own[,conv_frac_names[f]]
            }
          } else {
            # FROM C DENSITY POOL
            # get all the C pool densities for each C fraction 
            if (!out_density_sheets[conv_density_inds[f]] %in% names(conv_own)) {
              conv_own = merge(conv_own, out_density_df_list[[conv_density_inds[f]]][,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", 
                                                                                        next_density_label)], 
                               by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
              names(conv_own)[names(conv_own) == next_density_label] = out_density_sheets[conv_density_inds[f]]
            }
            # if ag or developed land is expanding then calc the C removed or transfered from each of the land types:
            # subset the land type rows that are losing area to ag (i.e. Cultivated is >0) and calc the amount of C transferred from shrinking land type to ag
            # C transfer [MgC/ha] = (C density of _shrinking_ land type) * (conversion frac) * (ag area gain from _shrinking_ land type)/(total ag area)
            
            conv_own[conv_own$Cultivated > 0,convc_trans_names[f]] = 
              conv_own[conv_own$Cultivated > 0, out_density_sheets[conv_density_inds[f]]] * 
              conv_own[conv_own$Cultivated > 0,conv_frac_names[f]] * conv_own$Cultivated[conv_own$Cultivated > 0] / 
              conv_own$tot_area[conv_own$Cultivated > 0]
            conv_own[is.na(conv_own[,convc_trans_names[f]]),convc_trans_names[f]] <- 0.00
            conv_own[is.nan(conv_own[,convc_trans_names[f]]),convc_trans_names[f]] <- 0.00
            conv_own[conv_own[,convc_trans_names[f]] == Inf,convc_trans_names[f]] <- 0.00 
            # repeat for developed
            temp_vals = conv_own[,convc_trans_names[f]]
            temp_vals[] = 0.00
            temp_vals[conv_own$Developed_all > 0] =
              conv_own[conv_own$Developed_all > 0,out_density_sheets[conv_density_inds[f]]] * 
              conv_own[conv_own$Developed_all > 0,conv_frac_names[f]] * conv_own$Developed_all[conv_own$Developed_all > 0] /
              conv_own$tot_area[conv_own$Developed_all > 0]
           	temp_vals[is.na(temp_vals)] <- 0.00
            temp_vals[is.nan(temp_vals)] <- 0.00
            temp_vals[temp_vals == Inf] <- 0.00            
            conv_own[conv_own$Developed_all > 0,convc_trans_names[f]] = 
              conv_own[conv_own$Developed_all > 0,convc_trans_names[f]] + temp_vals[conv_own$Developed_all > 0]
              
          } # end if removed source else density source
        } # end for f loop over the conversion transfer fractions for calculating the transfer carbon
        conv_own[,10:ncol(conv_own)] <- apply(conv_own[,10:ncol(conv_own)], 2, function (x) {replace(x, is.nan(x), 0.00)})
        conv_own[,10:ncol(conv_own)] <- apply(conv_own[,10:ncol(conv_own)], 2, function (x) {replace(x, is.na(x), 0.00)})
        conv_own[,10:ncol(conv_own)] <- apply(conv_own[,10:ncol(conv_own)], 2, function (x) {replace(x, x == Inf, 0.00)})
        conv_own = conv_own[order(conv_own$Land_Cat_ID),]
        # need a copy for below
        conv_own_static = conv_own
        
        # calculate the density changes due to contraction and expansion of land types
        # these are actually normlized carbon values, so the change in density values need to be calculated for area gains and losses
        #  the density changes are based on change area, from carbon, and the land type id total area
        # the from-to areas are positive, the to-from areas are negative
        #
        # from-to (positive columns)
        # ag and developed only have above main c (zero new carbon added) and soil c (frac of from carbon added) densities
        #  the others are zero, so they are taken care of automatically for contraction
        #  they need a special case for above and below addition
        #   and the from conversion loss from total clearing has been calculated above
        # assume that loss happens faster than gain
        # above ground carbon expansion:
        #  if new land type has less carbon, send the difference to the atmosphere, and calc the to carbon change based on area change and to 
        # land type carbon
        #  if new land type has more carbon, just calc the to carbon change, based on area change and from land type carbon
        # below ground and soil carbon expansion:
        #  regardless of difference, calc the to density change based on area change and from land type carbon
        #  assume that these transitions do not alter underground c immediately, and that the new c dynamics will eventually dominate
        #
        # to-from (negative columns)
        # this is the carbon transferred from one land type to another
        # for to ag and urban some of the carbon has been removed already
        #  in fact, all above ground pools and a fraction of the underground pools as well
        # just need to remove some carbon from the remaining from land type area
        # from ag and developed only have above main c and soil c densities for now
        #   the others are zero, so they are taken care of automatically
        # assume that loss happens faster than gain
        # for all cases:
        #  calc density change based on change area and from land type carbon, normalized to from total area
        #  but make sure not to remove carbon already removed when the from-to c den diff is positive!
        #
        # loop over the specific land type columns to generate land type specific conversion effect dfs
        # the final change values in these dfs will be aggregated put in columns in conv_own
        # these density changes are with respect to the current land type id total area for consistency with the other changes
        # at the end of the current year multiply the final densities by tot_area/new_area
        
        conv_df_list <- list()
        for (l in 1:num_conv_col_names) {
          # create a dataframe of conversion C transfers for each of the 16 land type
          lt_conv = conv_own_static
          # loop over the c pools
          for (c in 3:num_out_density_sheets) {
            # indices for matching density columns in land-type conversion C transfer dataframe and the output C density dataframe
            cind = which(names(lt_conv) == out_density_sheets[c])
            # paste "_change" to the end of the current C density pool in loop (e.g. "Soil_orgC_den_change") 
            chname = paste0(out_density_sheets[c],"_change")
            # create a column with this name in the current land type's conversion C transfer dataframe (fill with 0's)
            lt_conv[, chname] = 0
            # paste "_diff" to the end of the current C density pool in loop (e.g. "Soil_orgC_den_diff") 
            diffname = paste0(out_density_sheets[c],"_diff")
            # this is the land type row minus the l column difference
            # i.e., difference in C density of pool 'c' between all landtypes & landtype 'l' = 
            # (C densities of pool 'c' [MgC/ha] in each landtype) - (C density of pool 'c' in landtype 'l')  
            lt_conv[,diffname] = lt_conv[,cind] - lt_conv[l,cind]
            # do from-to first; don't need to do anything for a zero column
            # if the area change in landtype 'l' is positive (from-to), then do...
            #if(sum(lt_conv[,conv_col_names[l]]) > 0) {
            	if (TRUE) { # this should work fine because there are checks throughought to make sure operating rows are > 0
              # only operate where the "to" area is > 0
              # and on above-ground C density pools 
              if (c != 4 & c != 9) { # above
                # and if the current land type dataframe in loop (lt_conv) is for Cultivated or Developed areas
                if(conv_col_names[l] == "Cultivated" | conv_col_names[l] == "Developed_all") {
                  # then, for these growing landtypes only, set the C density changes to 0
                  # initialize the changes in C density (don't keep any above-ground C when converting to ag or developed)
                  # density change = change in "from-to" area * zero carbon / "to" total area           
                  lt_conv[lt_conv[,conv_col_names[l]] > 0, chname] = 0
                } else {
                  # the diff matters, but the c to atmos is tallied below in the to-from section
                  # positive diff here means that some c is lost to atmosphere
                  # calc density for diff positive
                  # density change = change in "from-to" area * "to" carbon / "to" total area  ("to" land type is new land type)
                  lt_conv[(lt_conv[,diffname] > 0 & lt_conv[,conv_col_names[l]] > 0), chname] = 
                    lt_conv[(lt_conv[,diffname] > 0 & lt_conv[,conv_col_names[l]] > 0), conv_col_names[l]] * lt_conv[l, cind] / 
                    lt_conv$tot_area[l]
                  # calc density for diff negative
                  # density change = change in "from-to" area * "from" carbon / "to" total area
                  lt_conv[(lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] > 0), chname] = 
                    lt_conv[(lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] > 0), conv_col_names[l]] * 
                    lt_conv[(lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] > 0), cind] / lt_conv$tot_area[l]
                } # end not to ag or developed
              } else {		# underground
                # if ag or developed and root c, then...
                if(conv_col_names[l] == "Cultivated" | conv_col_names[l] == "Developed_all") {
                  if (c==4) {
                    # this should be zero, and the frac should send it all to the atmos above
                    # but add it just in case the frac is changed
                    # density change = change in "from-to" area * "from" carbon * rembelowcfrac / "to" total area
                    # go to all the rows in which a landtype is converting area to the land type l column, and multiply this area by
                    # the correpsonding change in C density and 1 minus the fraction that goes to atmosphere (currently 100%)/tot area, so this is 0.
                    lt_conv[lt_conv[,conv_col_names[l]] > 0, chname] = lt_conv[lt_conv[,conv_col_names[l]] > 0, conv_col_names[l]] * 
                      lt_conv[lt_conv[,conv_col_names[l]] > 0, cind] * (1-lt_conv[lt_conv[,conv_col_names[l]] > 0, "Below2Atmos_conv_frac"]) / 
                      lt_conv$tot_area[l]
                  } else {
                    # a fraction of the from soil c has been removed to atmosphere
                    # density change = change in "from-to" area * "from" carbon * remsoilcfrac / "to" total area
                    lt_conv[lt_conv[,conv_col_names[l]] > 0, chname] = lt_conv[lt_conv[,conv_col_names[l]] > 0, conv_col_names[l]] * 
                      lt_conv[lt_conv[,conv_col_names[l]] > 0, cind] * (1-lt_conv[lt_conv[,conv_col_names[l]] > 0, "Soil2Atmos_conv_frac"]) / 
                      lt_conv$tot_area[l]
                  } # end else soil c for to ag and dev
                } else {	# end else underground ag and dev
                  # soil org C & root for all other land types:
                  # density change = change in "from-to" area * "from" carbon / "to" total area
                  lt_conv[lt_conv[,conv_col_names[l]] > 0, chname] = lt_conv[lt_conv[,conv_col_names[l]] > 0, conv_col_names[l]] * 
                    lt_conv[lt_conv[,conv_col_names[l]] > 0, cind] / lt_conv$tot_area[l]
                }
              } # end else underground
              # else for land types losing area or for a static scenatio where nothing changes....
            #} else if (sum(lt_conv[,conv_col_names[l]]) <= 0) {
            	}
            	if (TRUE) { # this should work fine because ther are checks throughought to make sure operating rows are < 0
              # to-from
              # only operate where the "from" area is < 0
              # to ag and dev already have removed carbon based on clearing above
              #  all above ground has been removed
              #  but some soil carbon still needs to be tallied as transferred
              #  and some below ground is the frac is changed in input file (change in root c frac is uncertain, 
              #  can change fraction from 1 to something else in input file for sensitivity check)
              #  so only operate on the non-ag, non-dev rows for all except underground
              # carbon has been sent to atmos when the to-from difference is negative
              
              # if root c or soil org c...
              if (c==4 | c==9) {
              	# no 2 atmos transfer here, so don't worry about nonregen area
              	
                # include ag and dev for the underground
                # it doesn't matter what the c den diff is
                # density change = change in "to-from" area * "from" carbon / "from" total area
                # do the "to" non-ag non-dev
                # this value should be negative
                lt_conv[lt_conv[,conv_col_names[l]] < 0 & lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", chname] = 
                  lt_conv[lt_conv[,conv_col_names[l]] < 0 & lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", 
                          conv_col_names[l]] * 
                  lt_conv[l, cind] / lt_conv$tot_area[l]
                # "to" ag and dev needs the remaining fraction of c for each c pool
                # this value should be negative
                # calc c taken out of 'from' landtype when it IS going to ag or developed (need to know C remaining after loss to atmosphere)
                # if it's root c, then remaining c fraction = 1 - root frac lost to atmosphere
                if(c==4) {remfrac = (1-lt_conv[l,"Below2Atmos_conv_frac"])} else
                  # else it's soil org c, and remaining c fraction = 1 - soil C frac lost to atmosphere
                {remfrac = (1-lt_conv[l,"Soil2Atmos_conv_frac"])}
                # area lost * "from" carbon * remaining frac after conversion to ag or dev/ "from" total area 
                lt_conv[lt_conv[,conv_col_names[l]] < 0 & (lt_conv$Land_Type == "Cultivated" | lt_conv$Land_Type == "Developed_all"), chname] = 
                  lt_conv[lt_conv[,conv_col_names[l]] < 0 & (lt_conv$Land_Type == "Cultivated" | lt_conv$Land_Type == "Developed_all"), 
                          conv_col_names[l]] * remfrac * lt_conv[l, cind] / lt_conv$tot_area[l]	
              } else {	# end if underground for to-from
                # above ground
                # the diff matters here - positive diff values mean all from carbon is transferred
                # density change = change in "to-from" area * "from" carbon / "from" total area
                # this value should be negative
                # calc c dens change (loss) for losing areas with higher C dens than 'to' area (not to ag or dev), by 
                # subsetting rows in c dens change column with positive C dens difference & not ag or dev = 
                # lost area * diff in C dens
                lt_conv[lt_conv[,diffname] > 0 & lt_conv[,conv_col_names[l]] < 0 & 
                          lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", chname] = 
                  lt_conv[lt_conv[,diffname] > 0 & lt_conv[,conv_col_names[l]] < 0 & 
                            lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", conv_col_names[l]] * 
                  lt_conv[l, cind] / lt_conv$tot_area[l]
                
                # the diff matters here - negative diff values mean some carbon is sent to atmosphere  
                # adjust area change to account for nonregen area
                # this is to avoid double counting emissions due to fire then conversion
                # basically, do not emit to atmosphere for the non-regen area
                # distribute to appropriate land type transitions
                # carbon transferred to new type (below) is based on the conv_own area and current new type carbon
                # the carbon added to the new type (above in from-to) is consistent with this
                # this means that if the non-regen area has less carbon than the target (after burn), then a little extra carbon is transferred,
                #  and if non-regen area has more carbon than the target (after burn), then a litte less carbon is transferred
                # recall that this area is negative
                # need to use the non_regen_area value to ensure the correct land type is adjusted for not regnerating
                lt_conv$area_adj = lt_conv[, conv_col_names[l]]
                if (length(lt_conv$area_adj[lt_conv[,conv_col_names[l]] < 0]) > 0) {
                	lt_conv$area_adj[lt_conv[,conv_col_names[l]] < 0] = lt_conv$area_adj[lt_conv[,conv_col_names[l]] < 0] -
                		lt_conv$non_regen_area[l] * lt_conv$nonreg_add[lt_conv[,conv_col_names[l]] < 0] / lt_conv$non_regen_area[l]
                	lt_conv$area_adj[is.na(lt_conv$area_adj)] = 0.00
                	lt_conv$area_adj[is.nan(lt_conv$area_adj)] = 0.00
                	lt_conv$area_adj[lt_conv$area_adj == Inf] = 0.00
                	# this shouldn't happen because non-reg is a subset of total loss, but check anyway
                	if (TRUE %in% (lt_conv$area_adj[lt_conv[,conv_col_names[l]] < 0] > 0)) {
                		cat("Warning: nonregen error in land conversion at r, i, l\n", r, i, l)
                	}
                	lt_conv$area_adj[lt_conv[,conv_col_names[l]] < 0 & lt_conv$area_adj > 0] = 0.00
                }
                # the diff matters here - negative diff values mean some carbon is sent to atmosphere
                # send above ground lost carbon to the atmosphere if necessary
                # operate only where to-from diff is negative, and use area adjusted for non-regen area
                # including the case where the values are 0
                # 2atmos = row(to) minus col(from) c diff * "to-from" area / "from" total area
                # this value ends up positive, consistent with the removed transfers above
                # sent to atmos
                atmosname = paste0(out_density_sheets[c],"2Atmos")
                lt_conv[,atmosname] = 0
                lt_conv[(lt_conv[,diffname] <= 0 & lt_conv[,conv_col_names[l]] <= 0 & 
                           lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all"), atmosname] = 
                  lt_conv[(lt_conv[,diffname] <= 0 & lt_conv[,conv_col_names[l]] <= 0 & 
                             lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all"),diffname] * 
                  lt_conv[(lt_conv[,diffname] <= 0 & lt_conv[,conv_col_names[l]] <= 0 & lt_conv$Land_Type != "Cultivated" & 
                             lt_conv$Land_Type != "Developed_all"), "area_adj"] / lt_conv$tot_area[l]
                
                # the diff matters here - negative diff values mean some carbon is sent to atmosphere
                # density change = change in "to-from" area * "to" carbon / "from" total area
                # this value should be negative
                lt_conv[lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] < 0 & 
                          lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", chname] = 
                  lt_conv[lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] < 0 & lt_conv$Land_Type != "Cultivated" & 
                            lt_conv$Land_Type != "Developed_all", conv_col_names[l]] * 
                  lt_conv[lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] < 0 & 
                            lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", cind] / lt_conv$tot_area[l]
                
                # sum all C come out of 'from' land type going to atmosphere
                conv_own[conv_own$Land_Cat_ID == lt_conv$Land_Cat_ID[l],atmosname] = sum(lt_conv[,atmosname])
                # these deal with numerical errors due to roundoff, divide by zero, and any added NA values
                conv_own[,atmosname] = replace(conv_own[,atmosname], is.na(conv_own[,atmosname]), 0.0)
                conv_own[,atmosname] = replace(conv_own[,atmosname], is.nan(conv_own[,atmosname]), 0.0)
                conv_own[,atmosname] = replace(conv_own[,atmosname], conv_own[,atmosname] == Inf, 0.0)
              } # end else above ground for to-from
            } # end else to-from
            
            # sum amount of C either lost or gained for each land type
            conv_own[conv_own$Land_Cat_ID == lt_conv$Land_Cat_ID[l],chname] = sum(lt_conv[,chname])
            # these deal with numerical errors due to roundoff, divide by zero, and any added NA values
            conv_own[,chname] = replace(conv_own[,chname], is.na(conv_own[,chname]), 0.0)
            conv_own[,chname] = replace(conv_own[,chname], is.nan(conv_own[,chname]), 0.0)
            conv_own[,chname] = replace(conv_own[,chname], conv_own[,chname] == Inf, 0.0)
          } # end for c loop over the c pools
          conv_df_list[[l]] = lt_conv
        } # end for l loop over the "to" conversion column names
        
      } else {
        # ocean/seagrass
        # add the columns and update them accordingly
        # there is only expansion and contraction
        #  on expansion, do not add carbon because the initial state is unknown
        #  so calc carbon density transfers to maintain correct average c density
        #  these are also normalized to current tot_area
        # no losses to atmosphere - it is assumed that it stays in the ocean
        skip = length(names(conv_own))
        # 'add' gets a vector of all the needed column names from completed conv_own table
        add = names(own_conv_df_list[[1]])[(skip+1):ncol(own_conv_df_list[[1]])]
        conv_own[,add] = 0
        # fill in all the conv_own columns and fill with 0
        conv_own$own_gain_sum = sum(conv_own$area_change[conv_own$area_change > 0])
        conv_own[conv_own$Land_Type == "Seagrass", "Above_main_C_den"] = 
          out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type == "Seagrass",next_density_label]
        conv_own[conv_own$Land_Type == "Seagrass", "Soil_orgC_den"] = 
          out_density_df_list[[9]][out_density_df_list[[9]]$Land_Type == "Seagrass",next_density_label]
        # contraction
        conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "Above_main_C_den_change"] = 
          conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "area_change"] * 
          conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "Above_main_C"] / 
          conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "tot_area"]
        conv_own[,"Above_main_C_den_change"] = replace(conv_own[,"Above_main_C_den_change"], is.nan(conv_own[,"Above_main_C_den_change"]), 0.0)
        conv_own[,"Above_main_C_den_change"] = replace(conv_own[,"Above_main_C_den_change"], conv_own[,"Above_main_C_den_change"] == Inf, 0.0)
        conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "Soil_orgC_den_change"] = 
          conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "area_change"] * 
          conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "Soil_orgC_den"] / 
          conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change < 0), "tot_area"]
        conv_own[,"Soil_orgC_den_change"] = replace(conv_own[,"Soil_orgC_den_change"], is.nan(conv_own[,"Soil_orgC_den_change"]), 0.0)
        conv_own[,"Soil_orgC_den_change"] = replace(conv_own[,"Soil_orgC_den_change"], conv_own[,"Soil_orgC_den_change"] == Inf, 0.0)
        # expansion
        conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change > 0), "Above_main_C_den_change"] = 0
        conv_own[(conv_own$Land_Type == "Seagrass" & conv_own$area_change > 0), "Soil_orgC_den_change"] = 0
      } # end if land own else ocean/seagrass
      # do not include the extra columns for calculating the transitions
      conv_own$row_gain_sum = NULL
      conv_own$row_loss_sum = NULL
      conv_own$row_change_sum = NULL
      own_conv_df_list_pre[[i]] = conv_own
      # get column number of last column before all the land types - this attempts to make the following routine more generic to allow changes to 
      # the c_input file structure without updating the for loop below.
      last_column <- which(names(own_conv_df_list_pre[[i]])=="own_gain_sum")
    } # end i loop over ownership for calculating land conversion c adjustments
      # after all the ownership loops within a given region rbind all the ownership tables into one 
      # first, get the complete list of land type names 
      conv_col_names_all = unique(conv_adjust_df$Land_Type)
      num_conv_col_names = length(conv_col_names_all)
      # Second, check each ownership df within the current region's list (own_conv_df_list_pre) to see if it's 
      # missing a land type column name, and replace if it is.
      for (g in 1:length(own_conv_df_list_pre)) { 
        if (any(!((conv_col_names_all) %in% (names(own_conv_df_list_pre[[g]]))))) { 
          # get indices of full name list that are missing
          missing_inds <- which(!((conv_col_names_all) %in% (names(own_conv_df_list_pre[[g]]))))
          # get names of missing land type columns
          missing_names <- conv_col_names_all[missing_inds]
          # add the missing land type columns and fill with 0's
          own_conv_df_list_pre[[g]][missing_names] <- 0
          # get length of new complete df (=73)
          full_length <- length(own_conv_df_list_pre[[g]])
          # get columns preceding the land type columns
          begin_set_cols <- own_conv_df_list_pre[[g]][,c(1:last_column)]
          # get full set of landtype columns in order
          all_16landtype_cols <- own_conv_df_list_pre[[g]][,c(conv_col_names_all)]
          # get number of _not_ missing column names 
          numb_not_missing <- length(conv_col_names_all) - length(missing_names)
          # get column index to start on for end set of columns
          end_start_ind <- last_column + numb_not_missing + 1
          # get column index to end on for end set of columns
          end_end_ind <- full_length - length(missing_names)
          # subset the columns following the original landtype columns and before the ones that were added to the end
          # order the sequence of columns in the last chunk
          end_set_cols <- own_conv_df_list_pre[[g]][,c("Above_main_C_den", "Above_harvested_conv_c", "StandDead_C_den", "StandDead_harvested_conv_c", 
                                          "Harvested2Wood_conv_c", "Harvested2Energy_conv_c", "Harvested2SawmillDecay_conv_c", "Harvested2Slash_conv_c",
                                          "Understory_C_den", "Under2Slash_conv_c", "DownDead_C_den", "DownDead2Slash_conv_c", "Litter_C_den", 
                                          "Litter2Slash_conv_c", "Slash2Energy_conv_c", "Slash2Wood_conv_c", "Slash2Burn_conv_c", 
                                          "Slash2Decay_conv_c", "Under2DownDead_conv_c", "Soil_orgC_den", "Soil2Atmos_conv_c", "Below_main_C_den", 
                                          "Below2Atmos_conv_c", "Below2Soil_conv_c", "Above_main_C_den_change", "Below_main_C_den_change", 
                                          "Understory_C_den_change", "StandDead_C_den_change", "DownDead_C_den_change", "Litter_C_den_change", 
                                          "Soil_orgC_den_change", "Above_main_C_den2Atmos", "Understory_C_den2Atmos", "StandDead_C_den2Atmos", 
                                          "DownDead_C_den2Atmos", "Litter_C_den2Atmos")]
          # merge the subsets of columns back together. They're in proper order now to rbind below
          own_conv_df_list_pre[[g]] <- cbind(begin_set_cols, all_16landtype_cols, end_set_cols) 
      } # end if
      } # end for g loop
      # combine dataframes for each ownership type within a region
      if (length(own_conv_df_list_pre) > 1) {
        conv_df_pre = rbind(own_conv_df_list_pre[[1]], own_conv_df_list_pre[[2]])
        if (length(own_conv_df_list_pre) > 2) {
          for (z in 3:length(own_names)) {
            conv_df_pre = rbind(conv_df_pre, own_conv_df_list_pre[[z]])
          }
        }  
        own_conv_df_list[[r]] = conv_df_pre
      } else own_conv_df_list[[r]] = own_conv_df_list_pre[[1]]
    } # end r loop over Region
    
    # now rebuild the conv_adjust_df by Region
    # reset own_names   
    region_names <- unique(conv_adjust_df$Region)
    # start with adding the 1st and 2nd ownership data frames to conv_adjust
    # conv_adjust is currently a single df with x obs. of  73 variables, and own_conv_df_list[[1]]
    # is a list with 1 df, 114 obs., 73 variables.
    ## Reset conv_adjust_df 
    conv_adjust_df = rbind(own_conv_df_list[[1]], own_conv_df_list[[2]])  
    # then just add the remaining ownership data frames to conv_adjust
    for (i in 3:length(region_names)) {
      conv_adjust_df = rbind(conv_adjust_df, own_conv_df_list[[i]])
    }
    # sort so it looks like input tables
    conv_adjust_df = conv_adjust_df[order(conv_adjust_df$Land_Cat_ID),]
    
    # aggregate the transfer densities to the density pools
    # recall that the transfer densities are normalized to tot_area
    #  so after the sums, multiply by tot_area/new_area, because these are the final adjustments
    # convert these to gains where necessary for consistency: all terrestrial gains are positive, losses are negative
    # store the transfers in all_c_flux
    all_c_flux = merge(conv_adjust_df[,c(1:4,8)], all_c_flux, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    
    cgnames = NULL
    # above
    # changes in c dens
    cgnames = c(cgnames, paste0(out_density_sheets[3],"_gain_conv"))
    # c lost from any landtype going to ag/urban - c loss (emissions) if changing land area from hi to lo c density + 
    # c density gain if going to low to hi c density 
    all_c_flux[,cgnames[1]] = - conv_adjust_df$Above_harvested_conv_c - conv_adjust_df$Above_main_C_den2Atmos + 
      conv_adjust_df$Above_main_C_den_change
    # below
    cgnames = c(cgnames, paste0(out_density_sheets[4],"_gain_conv"))
    all_c_flux[,cgnames[2]] = - conv_adjust_df$Below2Atmos_conv_c + conv_adjust_df$Below_main_C_den_change
    # understory
    cgnames = c(cgnames, paste0(out_density_sheets[5],"_gain_conv"))
    all_c_flux[,cgnames[3]] = - conv_adjust_df$Under2Slash_conv_c - conv_adjust_df$Under2DownDead_conv_c - 
      conv_adjust_df$Understory_C_den2Atmos + conv_adjust_df$Understory_C_den_change
    # standing dead
    cgnames = c(cgnames, paste0(out_density_sheets[6],"_gain_conv"))
    all_c_flux[,cgnames[4]] = - conv_adjust_df$StandDead_harvested_conv_c - conv_adjust_df$StandDead_C_den2Atmos + 
      conv_adjust_df$StandDead_C_den_change
    # down dead
    cgnames = c(cgnames, paste0(out_density_sheets[7],"_gain_conv"))
    all_c_flux[,cgnames[5]] = - conv_adjust_df$DownDead2Slash_conv_c + conv_adjust_df$Under2DownDead_conv_c - 
      conv_adjust_df$DownDead_C_den2Atmos + conv_adjust_df$DownDead_C_den_change
    # litter
    cgnames = c(cgnames, paste0(out_density_sheets[8],"_gain_conv"))
    all_c_flux[,cgnames[6]] = - conv_adjust_df$Litter2Slash_conv_c - conv_adjust_df$Litter_C_den2Atmos + conv_adjust_df$Litter_C_den_change
    # soil
    cgnames = c(cgnames, paste0(out_density_sheets[9],"_gain_conv"))
    all_c_flux[,cgnames[7]] = - conv_adjust_df$Soil2Atmos_conv_c + conv_adjust_df$Soil_orgC_den_change
    
    # loop over the relevant out density tables to update the C pools based on the conversion fluxes
    # carbon cannot go below zero
    sum_change = 0
    sum_change2 = 0
    sum_neg_conv = 0
    # starts on 3 because 1 and 2 are total C and total living and dead biomass
    for (i in 3:num_out_density_sheets) {
      # diagnostic for c stock change
      sum_change = sum_change + sum(all_c_flux[,cgnames[i-2]] * all_c_flux$tot_area)
      # diagnostic for c stock change
      sum_change2 = sum_change2 + sum(conv_adjust_df[,paste0(out_density_sheets[i],"_change")] * all_c_flux$tot_area)
      # adds next year total c density columns: "Above_main_C_den"  "Below_main_C_den"  "Understory_C_den"  "StandDead_C_den"  
      # "DownDead_C_den"    "Litter_C_den"      "Soil_orgC_den"   
      out_density_df_list[[i]][, next_density_label] = out_density_df_list[[i]][, next_density_label] + all_c_flux[,cgnames[i-2]]
      # first calc the carbon not subtracted because it sends density negative
      # correction and diagnostic
      neginds = which(out_density_df_list[[i]][, next_density_label] < 0)
      cat("neginds for out_density_df_list lcc" , i, "are", neginds, "\n")
      cat("total areas for neginds out_density_df_list lcc" , i, "are", all_c_flux[neginds, "new_area"], "\n")
      sum_neg_eco = sum_neg_eco + sum(all_c_flux$tot_area[out_density_df_list[[i]][,next_density_label] < 0] * 
                                        out_density_df_list[[i]][out_density_df_list[[i]][,next_density_label] < 0, next_density_label])
      # sum all negative
      sum_neg_conv = sum_neg_conv + sum(all_c_flux$tot_area[out_density_df_list[[i]][,next_density_label] < 0] * 
                                          out_density_df_list[[i]][out_density_df_list[[i]][,next_density_label] < 0, next_density_label])
      # set neg desities to 0
      out_density_df_list[[i]][, next_density_label] <- replace(out_density_df_list[[i]][, next_density_label], 
                                                                out_density_df_list[[i]][, next_density_label] <= 0, 0.00)
      # normalize it to the new area and check for zero new area (sets correct c densities)
      out_density_df_list[[i]][, next_density_label] = out_density_df_list[[i]][, next_density_label] * all_c_flux$tot_area / all_c_flux$new_area
      # set NAN or INF desities to 0
      out_density_df_list[[i]][, next_density_label] <- replace(out_density_df_list[[i]][, next_density_label], 
                                                                is.nan(out_density_df_list[[i]][, next_density_label]), 0.00)
      out_density_df_list[[i]][, next_density_label] <- replace(out_density_df_list[[i]][, next_density_label], 
                                                                out_density_df_list[[i]][, next_density_label] == Inf, 0.00)
    } # end loop over out densities for updating due to conversion
    
    # Get total C lost to atmosphere from LCC via 3 potential flux pathways by the tot_area
    # (1) DECAY
    all_c_flux[,"Land2Atmos_DecayC_stock_conv"] = -conv_adjust_df$tot_area * 
      (conv_adjust_df$Soil2Atmos_conv_c + conv_adjust_df$Slash2Decay_conv_c + conv_adjust_df$Harvested2SawmillDecay_conv_c + 
         conv_adjust_df$Below2Atmos_conv_c + 
         conv_adjust_df$Above_main_C_den2Atmos + conv_adjust_df$Understory_C_den2Atmos + conv_adjust_df$StandDead_C_den2Atmos + 
         conv_adjust_df$DownDead_C_den2Atmos + conv_adjust_df$Litter_C_den2Atmos)
    # (2) MANAGED BURNS - currently assumed this is 0
    all_c_flux[,"Land2Atmos_BurnC_stock_conv"] = -conv_adjust_df$tot_area * (conv_adjust_df$Slash2Burn_conv_c)
    # (3) TOTAL ENERGY - this is assumed to go to the atmosphere immediately
    all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"] = -conv_adjust_df$tot_area * (conv_adjust_df$Harvested2Energy_conv_c + 
                                                                                 conv_adjust_df$Slash2Energy_conv_c)
    # Get amount of total LCC energy that is from harvest versus slash utilization
        # Harv2Energy
    all_c_flux[,"Land2Atmos_Harv2EnerC_stock_conv"] = -conv_adjust_df$tot_area * conv_adjust_df$Harvested2Energy_conv_c
        # Slash2Energy
    all_c_flux[,"Land2Atmos_Slash2EnerC_stock_conv"] = -conv_adjust_df$tot_area * conv_adjust_df$Slash2Energy_conv_c
    # replace NaN with 0
    all_c_flux[,"Land2Atmos_Harv2EnerC_stock_conv"] <- replace(all_c_flux[,"Land2Atmos_Harv2EnerC_stock_conv"], 
                                                                  is.nan( all_c_flux[,"Land2Atmos_Harv2EnerC_stock_conv"]), 0.0)
    all_c_flux[,"Land2Atmos_Slash2EnerC_stock_conv"] <- replace(all_c_flux[,"Land2Atmos_Slash2EnerC_stock_conv"], 
                                                                 is.nan(all_c_flux[,"Land2Atmos_Slash2EnerC_stock_conv"]), 0.0)
    # Get C lost to wood 
    # WOOD - this decays with a half-life
    all_c_flux[,"Land2Wood_c_stock_conv"] = -conv_adjust_df$tot_area * (conv_adjust_df$Harvested2Wood_conv_c + 
                                                                          conv_adjust_df$Slash2Wood_conv_c)
    # Get amount of total LCC Wood that is from harvest versus slash utilization
    # Harv2Wood
    all_c_flux[,"Harv2Wood_c_stock_conv"] = -conv_adjust_df$tot_area * conv_adjust_df$Harvested2Wood_conv_c
    # Slash2Wood
    all_c_flux[,"Slash2Wood_c_stock_conv"] = -conv_adjust_df$tot_area * conv_adjust_df$Slash2Wood_conv_c
    # replace NaN with 0
    all_c_flux[,"Harv2Wood_c_stock_conv"] <- replace(all_c_flux[,"Harv2Wood_c_stock_conv"], 
                                                                 is.nan(all_c_flux[,"Harv2Wood_c_stock_conv"]), 0.0)
    all_c_flux[,"Slash2Wood_c_stock_conv"] <- replace(all_c_flux[,"Slash2Wood_c_stock_conv"], 
                                                                 is.nan(all_c_flux[,"Slash2Wood_c_stock_conv"]), 0.0)
    
    cat("lcc carbon change is ", sum_change, "\n")
    cat("lcc net carbon transfer to other land types is ", sum_change2, "\n")
    cat("lcc carbon to wood is ", sum(all_c_flux$Land2Wood_c_stock_conv), "\n")
    cat("lcc carbon to atmos is ", sum(all_c_flux$Land2Atmos_c_stock_conv), "\n")
    cat("lcc carbon to energy is ", sum(all_c_flux$Land2Energy_c_stock_conv), "\n")
    cat("lcc negative carbon cleared is ", sum_neg_conv, "\n")
    
    # update the conversion wood tables
    # recall that the transfers from land are negative values
    # use the IPCC half life equation for first order decay of wood products, and the CA average half life for all products
    #  this includes the current year loss on the current year production
    # running stock and cumulative change values are at the beginning of the labeled year - so the next year value is the stock or sum after 
    # current year production and loss
    # annual change values are in the year they occurred
    
    k = log(2) / wp_half_life
    # next year's "LCC_Wood_C_stock" = current year's "LCC_Wood_C_stock" * decay_term1 + decay_term2 * more_wood
    out_wood_df_list[[15]][,next_wood_label] = out_wood_df_list[[15]][,cur_wood_label] * exp(-k) + ((1 - exp(-k)) / k) * 
      (-all_c_flux$Land2Wood_c_stock_conv)
    # next year's "LCC_Wood_CumGain_C_stock" = current year's "LCC_Wood_CumGain_C_stock" + harvested_wood_conv
    out_wood_df_list[[16]][,next_wood_label] = out_wood_df_list[[16]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_conv
    # Next year's "LCC_Harv2Wood_CumGain_C_stock" = Current year's "LCC_Harv2Wood_CumGain_C_stock" + wood_accumulated from harvest
    out_wood_df_list[[17]][,next_wood_label] = out_wood_df_list[[17]][,cur_wood_label] - all_c_flux$Harv2Wood_c_stock_conv
    # Next year's "LCC_Slash2Wood_CumGain_C_stock" = Current year's "LCC_Slash2Wood_CumGain_C_stock" + wood_accumulated from slash
    out_wood_df_list[[18]][,next_wood_label] = out_wood_df_list[[18]][,cur_wood_label] - all_c_flux$Slash2Wood_c_stock_conv
    # current year's "LCC_Wood_AnnGain_C_stock" = total wood accumulated
    out_wood_df_list[[20]][,cur_wood_label] = -all_c_flux$Land2Wood_c_stock_conv
    # current year's "LCC_Harv2Wood_AnnGain_C_stock" = total wood accumulated from harvest
    out_wood_df_list[[21]][,cur_wood_label] = -all_c_flux$Harv2Wood_c_stock_conv
    # current year's "LCC_Slash2Wood_AnnGain_C_stock" = total wood accumulated from slash utilization
    out_wood_df_list[[22]][,cur_wood_label] = -all_c_flux$Slash2Wood_c_stock_conv
    # current year's "LCC_Wood_AnnLoss_C_stock" = current year's "LCC_Wood_C_stock" + total wood accumulated - next year's "LCC_Wood_C_stock"
    out_wood_df_list[[23]][,cur_wood_label] = out_wood_df_list[[15]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_conv - 
      out_wood_df_list[[15]][,next_wood_label]
    # next year's "LCC_Wood_CumLoss_C_stock" = current year's "LCC_Wood_CumLoss_C_stock" + current year's "LCC_Wood_AnnLoss_C_stock" 
    out_wood_df_list[[19]][,next_wood_label] = out_wood_df_list[[19]][,cur_wood_label] + out_wood_df_list[[23]][,cur_wood_label]
    
    # update the total wood tables
    # total wood C stock = total management wood stock + total lcc wood stock
    out_wood_df_list[[1]][,next_wood_label] = out_wood_df_list[[6]][,next_wood_label] + out_wood_df_list[[15]][,next_wood_label]
    # total cumulative wood C gain = total cumulative management wood gain + total cumulative lcc wood gain
    out_wood_df_list[[2]][,next_wood_label] = out_wood_df_list[[7]][,next_wood_label] + out_wood_df_list[[16]][,next_wood_label]
    # total cumulative wood C loss = total cumulative management wood loss + total cumulative lcc wood loss
    out_wood_df_list[[3]][,next_wood_label] = out_wood_df_list[[10]][,next_wood_label] + out_wood_df_list[[19]][,next_wood_label]
    # total annual wood C gain = total annual management wood gain + total annual lcc wood gain
    out_wood_df_list[[4]][,cur_wood_label] = out_wood_df_list[[11]][,cur_wood_label] + out_wood_df_list[[20]][,cur_wood_label]
    # total annual wood C loss = total annual management wood loss + total annual lcc wood loss
    out_wood_df_list[[5]][,cur_wood_label] = out_wood_df_list[[14]][,cur_wood_label] + out_wood_df_list[[23]][,cur_wood_label]
    
    # set the new tot area
    out_area_df_list[[1]][,next_area_label] = all_c_flux$new_area
    
    # set this years actual managed area (the area change activities are still just targets)
    # can have the case where there isn't enough area to do the full input scenario target mangagement area (correction was done earlier)
    out_area_df_list[[2]][,cur_area_label] = man_adjust_df$man_area
    
    # set this years actual fire area - output by the lt breakdown
    if(year == start_year){
      # add 3rd data frame with these introductory columns
      out_area_df_list[[3]] = fire_adjust_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Severity")]
    }
    # add column for current (initial) burn area determined earlier for how it's distributed
    out_area_df_list[[3]][,cur_area_label] = fire_adjust_df$fire_burn_area
    
    # add up the total org c pool density
    out_density_df_list[[1]][, next_density_label] = 0
    # loop through all c dens pools and add to new column
    for (i in 3:num_out_density_sheets) {
      out_density_df_list[[1]][, next_density_label] = out_density_df_list[[1]][, next_density_label] + 
        out_density_df_list[[i]][, next_density_label]
    }
    
    # add up the biomass c pool density (all non-decomposed veg material)
    out_density_df_list[[2]][, next_density_label] = 0
    for (i in 3:(num_out_density_sheets-1)) {
      out_density_df_list[[2]][, next_density_label] = out_density_df_list[[2]][, next_density_label] + 
        out_density_df_list[[i]][, next_density_label]
    }
    
    # fill the carbon stock out tables and the atmos tables
    #out_stock_sheets = c("All_orgC_stock", "All_biomass_C_stock", "Above_main_C_stock", "Below_main_C_stock", "Understory_C_stock", 
    #"StandDead_C_stock", "DownDead_C_stock", "Litter_C_stock", "Soil_orgC_stock")
    #out_atmos_sheets = c("Eco_CumGain_C_stock", "Total_Atmos_CumGain_C_stock", "Manage_Atmos_CumGain_C_stock", "Fire_Atmos_CumGain_C_stock", 
    #"LCC_Atmos_CumGain_C_stock", "Wood_Atmos_CumGain_C_stock", "Total_Energy2Atmos_C_stock", "Eco_AnnGain_C_stock", "Total_Atmos_AnnGain_C_stock", 
    #"Manage_Atmos_AnnGain_C_stock", "Fire_Atmos_AnnGain_C_stock", "LCC_Atmos_AnnGain_C_stock", "Wood_Atmos_AnnGain_C_stock", 
    # "Total_AnnEnergy2Atmos_C_stock") out_wood_sheets = c("Total_Wood_C_stock", "Total_Wood_CumGain_C_stock", "Total_Wood_CumLoss_C_stock", 
    # "Total_Wood_AnnGain_C_stock", "Total_Wood_AnnLoss_C_stock", "Manage_Wood_C_stock", "Manage_Wood_CumGain_C_stock", 
    # "Manage_Wood_CumLoss_C_stock", "Manage_Wood_AnnGain_C_stock", "Manage_Wood_AnnLoss_C_stock", "LCC_Wood_C_stock", "LCC_Wood_CumGain_C_stock", 
    # "LCC_Wood_CumLoss_C_stock", "LCC_Wood_AnnGain_C_stock", "LCC_Wood_AnnLoss_C_stock")
    
    # carbon stock
    for (i in 1:num_out_stock_sheets) {
      out_stock_df_list[[i]][, next_stock_label] = out_density_df_list[[i]][, next_density_label] * out_area_df_list[[1]][,next_area_label]
    }
    
    # cumulative c to atmosphere (and net cumulative c from atmos to ecosystems)
    # also store the annual values
    # gains are positive (both land and atmosphere)
    # so need to subtract the releases to atmosphere becuase they are negative in all_c_flux
    # as these are cumulative, they represent the values at the beginning of the labelled year
    
    # net atmos to ecosystems; this includes c accumulation adjustments based on management
    # "Above_main_C_den_gain_eco" to "Soil_orgC_den_gain_eco"
    sum_change = 0
    for(i in 1:7){
      # checks the eco accum value below
      sum_change = sum_change + sum(all_c_flux[, egnames[i]] * all_c_flux$tot_area)
    }
    
    ##### CUMULATIVE FLUXES ##### 

    ### Net Ecosystem Flux (i.e. baseline) ###
    # "Eco_CumGain_C_stock" = current year "Eco_CumGain_C_stock"  + total area * (sum of all changes in c density pools)
    out_atmos_df_list[[1]][, next_atmos_label] = out_atmos_df_list[[1]][, cur_atmos_label] + all_c_flux[,"tot_area"] * 
      (all_c_flux[,11] + all_c_flux[,12] + all_c_flux[,13] + all_c_flux[,14] + all_c_flux[,15] + all_c_flux[,16] + all_c_flux[,17])
    
    ### Management C Emissions ###
    # "Manage_Atmos_CumGain_C_stock" based on biomass removal, managed burns and energy (note: actually adding terms because they are negative)
    # "Manage_Atmos_CumGain_C_stock" = (current year "Manage_Atmos_CumGain_C_stock") - "Land2Decay_c_stock_man_agg"  -
                                      # "Land2Burn_c_stock_man_agg" - "Land2Energy_c_stock_man_agg"  
    out_atmos_df_list[[3]][, next_atmos_label] = out_atmos_df_list[[3]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_DecayC_stock_man_agg"] - 
      all_c_flux[,"Land2Atmos_BurnC_stock_man_agg"] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_man_agg"]
    
    ### Wildfire C Emissions ###
    # "Fire_Atmos_CumGain_C_stock"
    out_atmos_df_list[[4]][, next_atmos_label] = out_atmos_df_list[[4]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_c_stock_fire_agg"]
    
    ### Land Cover Change C Emissions ###
    # "LCC_Atmos_CumGain_C_stock" based on land cover change with associated biomass removal and energy
    out_atmos_df_list[[5]][, next_atmos_label] = out_atmos_df_list[[5]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_DecayC_stock_conv"] - 
      all_c_flux[,"Land2Atmos_BurnC_stock_conv"] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"]
    
    ### Wood C Emissions ###
    # "Wood_Atmos_CumGain_C_stock" from the wood tables: "Total_Wood_CumLoss_C_stock"
    out_atmos_df_list[[6]][, next_atmos_label] = out_wood_df_list[[3]][,next_wood_label]
    
    ### Total Energy C Emissions ### 
    # "Total_Energy2Atmos_C_stock" just to compare it with the total cum atmos c
    out_atmos_df_list[[7]][, next_atmos_label] = out_atmos_df_list[[7]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_man_agg"] - 
      all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"]
    
    ### Total C Emissions (all sources and pathways except Net Eco Flux) ###
    # "Total_Atmos_CumGain_C_stock" the total release of land and wood product and energy c to the atmosphere
    # the energy release is inluded in the manage and lcc releases
    # Total cum C emissions (less Eco) = Management + Wildfire + LCC + Wood
    out_atmos_df_list[[2]][, next_atmos_label] = out_atmos_df_list[[3]][,next_atmos_label] + out_atmos_df_list[[4]][,next_atmos_label] + 
      out_atmos_df_list[[5]][,next_atmos_label] + out_atmos_df_list[[6]][,next_atmos_label]
    
    ##### ANNUAL FLUXES	#####
    
    ### Net Ecosystem Flux ###
    # "Eco_AnnGain_C_stock" = total area * (Above_main_C_den_gain_eco + Below_main_C_den_gain_eco + Understory_C_den_gain_ec +
    #                                       StandDead_C_den_gain_eco + DownDead_C_den_gain_eco + Litter_C_den_gain_eco +
    #                                       Soil_orgC_den_gain_eco)
    out_atmos_df_list[[8]][, cur_atmos_label] = all_c_flux[,"tot_area"] * 
      (all_c_flux[,11] + all_c_flux[,12] + all_c_flux[,13] + all_c_flux[,14] + all_c_flux[,15] + all_c_flux[,16] + all_c_flux[,17])
    
    ### Management C Emissions ###
    # "Manage_Atmos_AnnGain_C_stock" = decay + burn + energy
    out_atmos_df_list[[10]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_DecayC_stock_man_agg"] - all_c_flux[,"Land2Atmos_BurnC_stock_man_agg"] -
       all_c_flux[,"Land2Atmos_TotEnergyC_stock_man_agg"]

    ### Wildfire C Emissions ###
    # "Fire_Atmos_AnnGain_C_stock" based on fire
    out_atmos_df_list[[11]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_c_stock_fire_agg"]
    
    ### Land Cover Change C Emissions ###
    # "LCC_Atmos_AnnGain_C_stock" based on land cover change with associated biomass removal, includes energy from biomass
    out_atmos_df_list[[12]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_DecayC_stock_conv"] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"] -
      all_c_flux[,"Land2Atmos_BurnC_stock_conv"]

    ### Wood C Emissions ###
    # "Wood_Atmos_AnnGain_C_stock" from the wood tables: "Total_Wood_CumLoss_C_stock"
    out_atmos_df_list[[13]][, cur_atmos_label] = out_wood_df_list[[5]][,cur_wood_label]
    
    ### Total Energy C Emissions ###
    # "Total_AnnEnergy2Atmos_C_stock" just to compare it with the total cum atmos c
    out_atmos_df_list[[14]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_TotEnergyC_stock_man_agg"] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"]
    
    ### Total C Emissions (all sources and pathways except Net Eco Flux) ###
    # "Total_Atmos_AnnGain_C_stock" the total release of land and wood product and energy c to the atmosphere
    # the energy release is inluded in the manage and lcc releases
    out_atmos_df_list[[9]][, cur_atmos_label] = out_atmos_df_list[[10]][,cur_atmos_label] + out_atmos_df_list[[11]][,cur_atmos_label] + 
      out_atmos_df_list[[12]][,cur_atmos_label] + out_atmos_df_list[[13]][,cur_atmos_label]
    
    ### cumulative (again) ### 
    
    # Get C emissions from individual pathways: CONTROLLED FIRE, ENERGY, and NON-BURNED C fluxes to atmosphere
  
    # Manage BURN: "Manage_Atmos_CumGain_FireC" = (current year "Manage_Atmos_CumGain_FireC") - "Land2Atmos_BurnC_stock_man_agg" 
    out_atmos_df_list[[15]][, next_atmos_label] = out_atmos_df_list[[15]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_BurnC_stock_man_agg"] 
    # Manage TOTAL ENERGY: "Manage_Atmos_CumGain_TotEnergyC" = (current year "Manage_Atmos_CumGain_TotEnergyC") - "Land2Atmos_TotEnergyC_stock_man_agg" 
    out_atmos_df_list[[16]][, next_atmos_label] = out_atmos_df_list[[16]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_man_agg"]
      # Manage ENERGY FROM HARVEST: 
    out_atmos_df_list[[17]][, next_atmos_label] = out_atmos_df_list[[17]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_Harv2EnerC_stock_man_agg"]
      # Manage ENERGY FROM SLASH: 
    out_atmos_df_list[[18]][, next_atmos_label] = out_atmos_df_list[[18]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_Slash2EnerC_stock_man_agg"]
    # Manage DECAY: "Manage_Atmos_CumGain_NonBurnedC" = (current year "Manage_Atmos_CumGain_NonBurnedC") - "Land2Atmos_DecayC_stock_man_agg"
    out_atmos_df_list[[19]][, next_atmos_label] = out_atmos_df_list[[19]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_DecayC_stock_man_agg"]  
    
    # check that true:  total management land to atmosphere C flux equal to energy + controlled burns + unburned (decay) 
    all(out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, next_atmos_label] == out_atmos_df_list[["Manage_Atmos_CumGain_FireC"]][, next_atmos_label] + 
          out_atmos_df_list[["Manage_Atmos_CumGain_TotEnergyC"]][, next_atmos_label] + out_atmos_df_list[["Manage_Atmos_CumGain_NonBurnedC"]][, next_atmos_label])
    # Due to rounding error this checks true that the difference is <0.5 and >-0.5 
    all(out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, next_atmos_label] - (out_atmos_df_list[["Manage_Atmos_CumGain_FireC"]][, next_atmos_label] + 
           out_atmos_df_list[["Manage_Atmos_CumGain_TotEnergyC"]][, next_atmos_label] + out_atmos_df_list[["Manage_Atmos_CumGain_NonBurnedC"]][, next_atmos_label]) < 0.5 & 
          (out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, next_atmos_label] - (out_atmos_df_list[["Manage_Atmos_CumGain_FireC"]][, next_atmos_label] + 
              out_atmos_df_list[["Manage_Atmos_CumGain_TotEnergyC"]][, next_atmos_label] + out_atmos_df_list[["Manage_Atmos_CumGain_NonBurnedC"]][, next_atmos_label])) > -0.5)
    
    # Partition the "Fire_Atmos_CumGain_C_stock" into burned and non-burned C sources (currently all burned because root and soil C are 0, but
    # including this here in case changes are later made to those input wildfire fractions)
    
    # FIRE burned: "Fire_Atmos_CumGain_BurnedC" = (current year "Fire_Atmos_CumGain_BurnedC") - "Land2Atmos_BurnedC_stock_fire_agg" 
    out_atmos_df_list[[20]][, next_atmos_label] = out_atmos_df_list[[20]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_BurnedC_stock_fire_agg"]
    # FIRE non-burned: "Fire_Atmos_CumGain_NonBurnedC" = (current year "Fire_Atmos_CumGain_NonBurnedC") - "Land2Atmos_NonBurnedC_stock_fire_agg" 
    out_atmos_df_list[[21]][, next_atmos_label] = out_atmos_df_list[[21]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_NonBurnedC_stock_fire_agg"]
    
    # Partition the "LCC_Atmos_CumGain_C_stock" into burned (energy only) and non-burned C sources 
    # With the exception of removed C to energy, we are currently assuming that all lost above- and below-ground c (except removed2wood) 
    # is released as CO2 (decomposition) and not burned
    
    # LCC BURN: "LCC_Atmos_CumGain_FireC" = (current year "LCC_Atmos_CumGain_FireC") - "Land2Atmos_BurnC_stock_conv"
    out_atmos_df_list[[22]][, next_atmos_label] = out_atmos_df_list[[22]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_BurnC_stock_conv"]
    # LCC TOTAL ENERGY: "LCC_Atmos_CumGain_EnergyC" = (current year "LCC_Atmos_CumGain_EnergyC") - "Land2Atmos_TotEnergyC_stock_conv"
    out_atmos_df_list[[23]][, next_atmos_label] = out_atmos_df_list[[23]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"]
      # LCC ENERGY FROM HARVEST: 
      out_atmos_df_list[[24]][, next_atmos_label] = out_atmos_df_list[[24]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_Harv2EnerC_stock_conv"]
      # LCC ENERGY FROM SLASH: 
      out_atmos_df_list[[25]][, next_atmos_label] = out_atmos_df_list[[25]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_Slash2EnerC_stock_conv"]
    # LCC DECAY
    out_atmos_df_list[[26]][, next_atmos_label] = out_atmos_df_list[[26]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_DecayC_stock_conv"]
    
    ### annual (again) ###
    
    # Partition the "Manage_Atmos_AnnGain_C_stock" into FIRE, ENERGY, and NON-BURNED C fluxes to atmosphere
    
    # Manage BURN: "Manage_Atmos_AnnGain_FireC" = - "Land2Atmos_BurnC_stock_man_agg" 
    out_atmos_df_list[[27]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_BurnC_stock_man_agg"] 
    # Manage TOTAL ENERGY: "Manage_Atmos_AnnGain_TotEnergyC" = - "Land2Atmos_TotEnergyC_stock_man_agg"
    out_atmos_df_list[[28]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_TotEnergyC_stock_man_agg"]
      # Manage ENERGY FROM HARVEST: 
      out_atmos_df_list[[29]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_Harv2EnerC_stock_man_agg"] 
      # Manage ENERGY FROM SLASH: 
      out_atmos_df_list[[30]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_Slash2EnerC_stock_man_agg"]
    # Manage DECAY: "Manage_Atmos_AnnGain_NonBurnedC" = - "Land2Atmos_DecayC_stock_man_agg"
    out_atmos_df_list[[31]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_DecayC_stock_man_agg"]  
    
    # check that true:  total management land to atmosphere C flux equal to energy + controlled burns + unburned (decay) 
    all(out_atmos_df_list[["Manage_Atmos_AnnGain_C_stock"]][, cur_atmos_label] == out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]][, cur_atmos_label] + 
          out_atmos_df_list[["Manage_Atmos_AnnGain_TotEnergyC"]][, cur_atmos_label] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][, cur_atmos_label])
    # Due to rounding error this checks true that the difference is <0.5 and >-0.5 
    all(out_atmos_df_list[["Manage_Atmos_AnnGain_C_stock"]][, cur_atmos_label] - (out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]][, cur_atmos_label] + 
             out_atmos_df_list[["Manage_Atmos_AnnGain_TotEnergyC"]][, cur_atmos_label] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][, cur_atmos_label]) < 0.5 & 
          (out_atmos_df_list[["Manage_Atmos_AnnGain_C_stock"]][, cur_atmos_label] - (out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]][, cur_atmos_label] + 
                  out_atmos_df_list[["Manage_Atmos_AnnGain_TotEnergyC"]][, cur_atmos_label] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][, cur_atmos_label])) > -0.5)  
    
    # check that true:  total management land to atmosphere C flux equal to energy + controlled burns + unburned (decay) 
    all(out_atmos_df_list[["Manage_Atmos_AnnGain_C_stock"]][, cur_atmos_label] == out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]][, cur_atmos_label] + 
          out_atmos_df_list[["Manage_Atmos_AnnGain_EnergyC"]][, cur_atmos_label] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][, cur_atmos_label])
    # Due to rounding error this checks true that the difference is <0.5 and >-0.5 
    all(out_atmos_df_list[["Manage_Atmos_AnnGain_C_stock"]][, cur_atmos_label] - (out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]][, cur_atmos_label] + 
             out_atmos_df_list[["Manage_Atmos_AnnGain_EnergyC"]][, cur_atmos_label] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][, cur_atmos_label]) < 0.5 & 
          (out_atmos_df_list[["Manage_Atmos_AnnGain_C_stock"]][, cur_atmos_label] - (out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]][, cur_atmos_label] + 
                  out_atmos_df_list[["Manage_Atmos_AnnGain_EnergyC"]][, cur_atmos_label] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][, cur_atmos_label])) > -0.5)  
    
    
    # Partition the "Fire_Atmos_AnnGain_C_stock" into burned and non-burned C sources (currently all burned because root and soil C are 0, but
    # including this here in case changes are later made to those input wildfire fractions)
    # FIRE burned: "Fire_Atmos_AnnGain_BurnedC" = - "Land2Atmos_BurnedC_stock_fire_agg" 
    out_atmos_df_list[[32]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_BurnedC_stock_fire_agg"]
    # FIRE non-burned: "Fire_Atmos_AnnGain_NonBurnedC" = - "Land2Atmos_NonBurnedC_stock_fire_agg" 
    out_atmos_df_list[[33]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_NonBurnedC_stock_fire_agg"]
    
    # Partition the "LCC_Atmos_AnnGain_C_stock" into burned (fire + energy) and non-burned (decay) C sources 
    # LCC FIRE: "LCC_Atmos_AnnGain_FireC" = - "Land2Atmos_BurnC_stock_conv"
    out_atmos_df_list[[34]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_BurnC_stock_conv"]
    # LCC TOTAL ENERGY: "LCC_Atmos_AnnGain_EnergyC" = - "Land2Atmos_TotEnergyC_stock_conv"
    out_atmos_df_list[[35]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_TotEnergyC_stock_conv"]
      # LCC ENERGY FROM HARVEST: 
      out_atmos_df_list[[36]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_Harv2EnerC_stock_conv"]
      # LCC ENERGY FROM SLASH: 
      out_atmos_df_list[[37]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_Slash2EnerC_stock_conv"]
    # LCC non-burned: "LCC_Atmos_AnnGain_NonBurnedC" = - "Land2Atmos_DecayC_stock_conv"
    out_atmos_df_list[[38]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_DecayC_stock_conv"]
    
  } # end loop over calculation years
  ###################################
  
  if (exists("out_neginds_eco_df")) { 
  # print sum of negative eco c cleared
  cat("sum of eco negative carbon cleared is", out_cum_neginds_eco_tot, "\n")
  # round to whole number for saving in file
  out_cum_neginds_eco_tot <- round(out_cum_neginds_eco_tot, digits=0)
  # assign file name for .csv file that tracks the land categories that ran out of soil C
  out_file_csv <- substr(out_file,1,nchar(out_file)-4)
  # write .csv file showing all land categories that ran out of soil C but still had area (i.e. neginds eco 9 and tot_area > 0)
  write.csv(out_neginds_eco_df, file = paste0(out_file_csv, "_land_cats_depleted_of_soil_c_&_sum_neg_cleared", out_cum_neginds_eco_tot, ".csv"))
  }
  
  # Calculate CO2-C & CH4-C emissions from fresh marshland based on output table (Eco_CumGain_C_stock & Eco_AnnGain_C_stock). Note that 
  # the CO2 portion of Eco C is actually C uptake (negative value), and it's CO2-eq will later be added to CO2-eq of CH4 to determine net GWP. 
  # Additionally, here we will account for any negative Eco C values (currently only in grassland) as these are net C fluxes to atmosphere and will 
  # be counted as CO2-C. 
  
  # get dataframes for the C values for fresh marsh to calculate CO2 & CH4 emissions, and any negative Eco C fluxes to calc CO2 emissions
  # for other land types (i.e grassland)
  Eco_CumGain_C_stock <- out_atmos_df_list[[1]]
  Eco_AnnGain_C_stock <- out_atmos_df_list[[8]]
  
  ## Cummulative ## 
  # go through each year column 
  Fresh_marsh_Cum_Eco_C <- out_atmos_df_list[[1]][out_atmos_df_list[[1]]$Land_Type == "Fresh_marsh", ]
  # get the other land types 
  Other_Cum_Eco_C <- out_atmos_df_list[[1]][out_atmos_df_list[[1]]$Land_Type != "Fresh_marsh", ]
  for (i in 5:ncol(Eco_CumGain_C_stock)) { # outer i loop by column 
    # calc fresh march CO2-C (negative frac because it's net C sequestration based on flux tower measurement by Knox et al (2015)
    Fresh_marsh_Cum_Eco_C[,i] <- Fresh_marsh_Cum_Eco_C[[i]] * marsh_CO2_C_frac 
    # for other land type Eco CO2-C,
    for (r in 1:nrow(Other_Cum_Eco_C)) { # inner row loop 
      if (Other_Cum_Eco_C[,i][r] < 0) {
        # change sign of negative Eco C values to positive CO2-C emissions 
        Other_Cum_Eco_C[,i][r] <- abs(Other_Cum_Eco_C[,i][r])
        # change sign of positive Eco C values to negative CO2-C emissions (i.e sequestration)
      } else Other_Cum_Eco_C[,i][r] <- -1 * Other_Cum_Eco_C[,i][r]
    }
  }
  Eco_CumCO2C <- list(Other_Cum_Eco_C, Fresh_marsh_Cum_Eco_C)
  Eco_CumCO2C <- do.call(rbind, Eco_CumCO2C)
  Eco_CumCO2C <- transform(Eco_CumCO2C, Land_Cat_ID = as.numeric(Land_Cat_ID))
  Eco_CumCO2C = Eco_CumCO2C[order(Eco_CumCO2C$Land_Cat_ID),]
  
  # repeat for CH4-C
  Fresh_marsh_Cum_Eco_C <- out_atmos_df_list[[1]][out_atmos_df_list[[1]]$Land_Type == "Fresh_marsh", ]
  Other_Cum_Eco_C <- out_atmos_df_list[[1]][out_atmos_df_list[[1]]$Land_Type != "Fresh_marsh", ]
  for (i in 5:ncol(Eco_CumGain_C_stock)) {
    # calc fresh march CH4-C
    Fresh_marsh_Cum_Eco_C[,i] <- Fresh_marsh_Cum_Eco_C[[i]] * marsh_CH4_C_frac 
    # set CH4-C to 0 (Assuming no CH4 flux from other land types)
    Other_Cum_Eco_C[,i] <- 0
  }
  Eco_CumCH4C <- list(Other_Cum_Eco_C, Fresh_marsh_Cum_Eco_C)
  Eco_CumCH4C <- do.call(rbind, Eco_CumCH4C)
  Eco_CumCH4C <- transform(Eco_CumCH4C, Land_Cat_ID = as.numeric(Land_Cat_ID))
  Eco_CumCH4C = Eco_CumCH4C[order(Eco_CumCH4C$Land_Cat_ID),]
  
  ## Annual ## 
  # go through each year column 
  Fresh_marsh_Ann_Eco_C <- out_atmos_df_list[[8]][out_atmos_df_list[[8]]$Land_Type == "Fresh_marsh", ]
  # get the other land types 
  Other_Ann_Eco_C <- out_atmos_df_list[[8]][out_atmos_df_list[[8]]$Land_Type != "Fresh_marsh", ]
  for (i in 5:ncol(Eco_AnnGain_C_stock)) {
    # calc fresh march CO2-C
    Fresh_marsh_Ann_Eco_C[,i] <- Fresh_marsh_Ann_Eco_C[[i]] * marsh_CO2_C_frac 
    for (r in 1:nrow(Other_Ann_Eco_C)) {
      # for other land type Eco CO2-C,
      if (Other_Ann_Eco_C[,i][r] < 0) {
        # change sign of negative Eco C values to positive CO2-C emissions 
        Other_Ann_Eco_C[,i][r] <- abs(Other_Ann_Eco_C[,i][r])
        # change sign of positive Eco C values to negative CO2-C emissions (i.e sequestration)
      } else Other_Ann_Eco_C[,i][r] <- -1 * Other_Ann_Eco_C[,i][r]
    }
  }
  Eco_AnnCO2C <- list(Other_Ann_Eco_C, Fresh_marsh_Ann_Eco_C)
  Eco_AnnCO2C <- do.call(rbind, Eco_AnnCO2C)
  Eco_AnnCO2C <- transform(Eco_AnnCO2C, Land_Cat_ID = as.numeric(Land_Cat_ID))
  Eco_AnnCO2C = Eco_AnnCO2C[order(Eco_AnnCO2C$Land_Cat_ID),]
  
  # repeat for CH4-C
  Fresh_marsh_Ann_Eco_C <- out_atmos_df_list[[8]][out_atmos_df_list[[8]]$Land_Type == "Fresh_marsh", ]
  Other_Ann_Eco_C <- out_atmos_df_list[[8]][out_atmos_df_list[[8]]$Land_Type != "Fresh_marsh", ]
  for (i in 5:ncol(Eco_AnnGain_C_stock)) {
    # calc fresh march CH4-C
    Fresh_marsh_Ann_Eco_C[,i] <- Fresh_marsh_Ann_Eco_C[[i]] * marsh_CH4_C_frac 
    # set CH4-C to 0 (No soil CH4 emissions from other land types)
    Other_Ann_Eco_C[,i] <- 0
  }
  Eco_AnnCH4C <- list(Other_Ann_Eco_C, Fresh_marsh_Ann_Eco_C)
  Eco_AnnCH4C <- do.call(rbind, Eco_AnnCH4C)
  Eco_AnnCH4C <- transform(Eco_AnnCH4C, Land_Cat_ID = as.numeric(Land_Cat_ID))
  Eco_AnnCH4C = Eco_AnnCH4C[order(Eco_AnnCH4C$Land_Cat_ID),]
  
  # Partition all the C emitted from controlled burn & bioenergy in out_atmos_df_list into CO2C, CH4C and BC-C.
  
  ########## Cumulative ########## 
  
  # MANAGE FIRE
  Manage_CumFireC <- out_atmos_df_list[["Manage_Atmos_CumGain_FireC"]]
  ManFire_CumCO2C <- Manage_CumFireC
  for (i in 5:ncol(Manage_CumFireC)) {
    ManFire_CumCO2C[,i] <- CO2C_fire_frac * Manage_CumFireC[,i]
  }
  ManFire_CumCH4C <- Manage_CumFireC
  for (i in 5:ncol(Manage_CumFireC)) {
    ManFire_CumCH4C[,i] <- CH4C_fire_frac * Manage_CumFireC[,i]
  }
  ManFire_CumBCC <- Manage_CumFireC
  for (i in 5:ncol(Manage_CumFireC)) {
    ManFire_CumBCC[,i] <- BCC_fire_frac * Manage_CumFireC[,i]
  }
  
  # MANAGE TOTAL ENERGY
  Manage_CumEnergyC <- out_atmos_df_list[["Manage_Atmos_CumGain_TotEnergyC"]]  
  ManTotEnergy_CumCO2C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManTotEnergy_CumCO2C[,i] <- CO2C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManTotEnergy_CumCH4C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManTotEnergy_CumCH4C[,i] <- CH4C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManTotEnergy_CumBCC <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManTotEnergy_CumBCC[,i] <- BCC_energy_frac * Manage_CumEnergyC[,i]
  }
  
  # MANAGE HARVEST 2 ENERGY
  Manage_CumEnergyC <- out_atmos_df_list[["Man_Atmos_CumGain_Harv2EnergyC"]]  
  ManHarv2Energy_CumCO2C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManHarv2Energy_CumCO2C[,i] <- CO2C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManHarv2Energy_CumCH4C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManHarv2Energy_CumCH4C[,i] <- CH4C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManHarv2Energy_CumBCC <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManHarv2Energy_CumBCC[,i] <- BCC_energy_frac * Manage_CumEnergyC[,i]
  }
  
  # MANAGE SLASH 2 ENERGY
  Manage_CumEnergyC <- out_atmos_df_list[["Man_Atmos_CumGain_Slash2EnergyC"]]  
  ManSlash2Energy_CumCO2C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManSlash2Energy_CumCO2C[,i] <- CO2C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManSlash2Energy_CumCH4C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManSlash2Energy_CumCH4C[,i] <- CH4C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManSlash2Energy_CumBCC <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManSlash2Energy_CumBCC[,i] <- BCC_energy_frac * Manage_CumEnergyC[,i]
  }
  
  # LCC FIRE
  LCC_CumFireC <- out_atmos_df_list[["LCC_Atmos_CumGain_FireC"]]
  LCCFire_CumCO2C <- LCC_CumFireC
  for (i in 5:ncol(LCC_CumFireC)) {
    LCCFire_CumCO2C[,i] <- CO2C_fire_frac * LCC_CumFireC[,i]
  }
  LCCFire_CumCH4C <- LCC_CumFireC
  for (i in 5:ncol(LCC_CumFireC)) {
    LCCFire_CumCH4C[,i] <- CH4C_fire_frac * LCC_CumFireC[,i]
  }
  LCCFire_CumBCC <- LCC_CumFireC
  for (i in 5:ncol(LCC_CumFireC)) {
    LCCFire_CumBCC[,i] <- BCC_fire_frac * LCC_CumFireC[,i]
  }
  
  # LCC TOTAL ENERGY
  LCC_CumEnergyC <- out_atmos_df_list[["LCC_Atmos_CumGain_TotEnergyC"]]
  LCCTotEnergy_CumCO2C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCTotEnergy_CumCO2C[,i] <- CO2C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCTotEnergy_CumCH4C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCTotEnergy_CumCH4C[,i] <- CH4C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCTotEnergy_CumBCC <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCTotEnergy_CumBCC[,i] <- BCC_energy_frac * LCC_CumEnergyC[,i]
  }
  
  # LCC HARVEST 2 ENERGY
  LCC_CumEnergyC <- out_atmos_df_list[["LCC_Atmos_CumGain_Harv2EnergyC"]]  
  LCCHarv2Energy_CumCO2C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCHarv2Energy_CumCO2C[,i] <- CO2C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCHarv2Energy_CumCH4C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCHarv2Energy_CumCH4C[,i] <- CH4C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCHarv2Energy_CumBCC <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCHarv2Energy_CumBCC[,i] <- BCC_energy_frac * LCC_CumEnergyC[,i]
  }
  
  # LCC SLASH 2 ENERGY
  LCC_CumEnergyC <- out_atmos_df_list[["LCC_Atmos_CumGain_Slash2EnergyC"]]  
  LCCSlash2Energy_CumCO2C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCSlash2Energy_CumCO2C[,i] <- CO2C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCSlash2Energy_CumCH4C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCSlash2Energy_CumCH4C[,i] <- CH4C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCSlash2Energy_CumBCC <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCSlash2Energy_CumBCC[,i] <- BCC_energy_frac * LCC_CumEnergyC[,i]
  }
  
  # WILDFIRE
  Wildfire_CumBurnedC <- out_atmos_df_list[["Fire_Atmos_CumGain_BurnedC"]]
  Wildfire_CumCO2C <- Wildfire_CumBurnedC
  for (i in 5:ncol(Wildfire_CumBurnedC)) {
    Wildfire_CumCO2C[,i] <- CO2C_fire_frac * Wildfire_CumBurnedC[,i]
  }
  Wildfire_CumCH4C <- Wildfire_CumBurnedC
  for (i in 5:ncol(Wildfire_CumBurnedC)) {
    Wildfire_CumCH4C[,i] <- CH4C_fire_frac * Wildfire_CumBurnedC[,i]
  }
  Wildfire_CumBCC <- Wildfire_CumBurnedC
  for (i in 5:ncol(Wildfire_CumBurnedC)) {
    Wildfire_CumBCC[,i] <- BCC_fire_frac * Wildfire_CumBurnedC[,i]
  }
  
  # TOTAL CUM ENERGY CO2-C, CH4-C, and BC-C 
  TotalEnergy_CumCO2C <- LCCTotEnergy_CumCO2C
  TotalEnergy_CumCO2C[,5:ncol(LCCTotEnergy_CumCO2C)] <- LCCTotEnergy_CumCO2C[,5:ncol(LCCTotEnergy_CumCO2C)] + ManTotEnergy_CumCO2C[,5:ncol(LCCTotEnergy_CumCO2C)]
  TotalEnergy_CumCH4C <- LCCTotEnergy_CumCH4C
  TotalEnergy_CumCH4C[,5:ncol(LCCTotEnergy_CumCH4C)] <- LCCTotEnergy_CumCH4C[,5:ncol(LCCTotEnergy_CumCH4C)] + ManTotEnergy_CumCH4C[,5:ncol(LCCTotEnergy_CumCH4C)]
  TotalEnergy_CumBCC <- LCCTotEnergy_CumBCC
  TotalEnergy_CumBCC[,5:ncol(LCCTotEnergy_CumBCC)] <- LCCTotEnergy_CumBCC[,5:ncol(LCCTotEnergy_CumBCC)] + ManTotEnergy_CumBCC[,5:ncol(LCCTotEnergy_CumBCC)]
  
  # TOTAL CUM CONTROLLED BURN CO2-C, CH4-C, and BC-C 
  TotalCntlFire_CumCO2C <- LCCFire_CumCO2C
  TotalCntlFire_CumCO2C[,5:ncol(LCCFire_CumCO2C)] <- LCCFire_CumCO2C[,5:ncol(LCCFire_CumCO2C)] + ManFire_CumCO2C[,5:ncol(LCCFire_CumCO2C)]
  TotalCntlFire_CumCH4C <- LCCFire_CumCH4C
  TotalCntlFire_CumCH4C[,5:ncol(LCCFire_CumCH4C)] <- LCCFire_CumCH4C[,5:ncol(LCCFire_CumCH4C)] + ManFire_CumCH4C[,5:ncol(LCCFire_CumCH4C)]
  TotalCntlFire_CumBCC <- LCCFire_CumBCC
  TotalCntlFire_CumBCC[,5:ncol(LCCFire_CumBCC)] <- LCCFire_CumBCC[,5:ncol(LCCFire_CumBCC)] + ManFire_CumBCC[,5:ncol(LCCFire_CumBCC)]
  
  ########## Annual ########## 
  
  # MANAGE FIRE
  Manage_AnnFireC <- out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]]
  ManFire_AnnCO2C <- Manage_AnnFireC
  for (i in 5:ncol(Manage_AnnFireC)) {
    ManFire_AnnCO2C[,i] <- CO2C_fire_frac * Manage_AnnFireC[,i]
  }
  ManFire_AnnCH4C <- Manage_AnnFireC
  for (i in 5:ncol(Manage_AnnFireC)) {
    ManFire_AnnCH4C[,i] <- CH4C_fire_frac * Manage_AnnFireC[,i]
  }
  ManFire_AnnBCC <- Manage_AnnFireC
  for (i in 5:ncol(Manage_AnnFireC)) {
    ManFire_AnnBCC[,i] <- BCC_fire_frac * Manage_AnnFireC[,i]
  }
  
  # MANAGE TOTAL ENERGY
  Manage_AnnEnergyC <- out_atmos_df_list[["Manage_Atmos_AnnGain_TotEnergyC"]]  
  ManTotEnergy_AnnCO2C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManTotEnergy_AnnCO2C[,i] <- CO2C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManTotEnergy_AnnCH4C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManTotEnergy_AnnCH4C[,i] <- CH4C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManTotEnergy_AnnBCC <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManTotEnergy_AnnBCC[,i] <- BCC_energy_frac * Manage_AnnEnergyC[,i]
  }
  
  # MANAGE HARVEST 2 ENERGY
  Manage_AnnEnergyC <- out_atmos_df_list[["Man_Atmos_AnnGain_Harv2EnergyC"]]  
  ManHarv2Energy_AnnCO2C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManHarv2Energy_AnnCO2C[,i] <- CO2C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManHarv2Energy_AnnCH4C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManHarv2Energy_AnnCH4C[,i] <- CH4C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManHarv2Energy_AnnBCC <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManHarv2Energy_AnnBCC[,i] <- BCC_energy_frac * Manage_AnnEnergyC[,i]
  }
  
  # MANAGE SLASH 2 ENERGY
  Manage_AnnEnergyC <- out_atmos_df_list[["Man_Atmos_AnnGain_Slash2EnergyC"]]  
  ManSlash2Energy_AnnCO2C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManSlash2Energy_AnnCO2C[,i] <- CO2C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManSlash2Energy_AnnCH4C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManSlash2Energy_AnnCH4C[,i] <- CH4C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManSlash2Energy_AnnBCC <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManSlash2Energy_AnnBCC[,i] <- BCC_energy_frac * Manage_AnnEnergyC[,i]
  }
  
  # LCC FIRE
  LCC_AnnFireC <- out_atmos_df_list[["LCC_Atmos_AnnGain_FireC"]]
  LCCFire_AnnCO2C <- LCC_AnnFireC
  for (i in 5:ncol(LCC_AnnFireC)) {
    LCCFire_AnnCO2C[,i] <- CO2C_fire_frac * LCC_AnnFireC[,i]
  }
  LCCFire_AnnCH4C <- LCC_AnnFireC
  for (i in 5:ncol(LCC_AnnFireC)) {
    LCCFire_AnnCH4C[,i] <- CH4C_fire_frac * LCC_AnnFireC[,i]
  }
  LCCFire_AnnBCC <- LCC_AnnFireC
  for (i in 5:ncol(LCC_AnnFireC)) {
    LCCFire_AnnBCC[,i] <- BCC_fire_frac * LCC_AnnFireC[,i]
  }
  
  # LCC TOTAL ENERGY
  LCC_AnnEnergyC <- out_atmos_df_list[["LCC_Atmos_AnnGain_TotEnergyC"]]
  LCCTotEnergy_AnnCO2C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCTotEnergy_AnnCO2C[,i] <- CO2C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCTotEnergy_AnnCH4C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCTotEnergy_AnnCH4C[,i] <- CH4C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCTotEnergy_AnnBCC <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCTotEnergy_AnnBCC[,i] <- BCC_energy_frac * LCC_AnnEnergyC[,i]
  }
  
  # LCC HARVEST 2 ENERGY
  LCC_AnnEnergyC <- out_atmos_df_list[["LCC_Atmos_AnnGain_Harv2EnergyC"]]  
  LCCHarv2Energy_AnnCO2C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCHarv2Energy_AnnCO2C[,i] <- CO2C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCHarv2Energy_AnnCH4C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCHarv2Energy_AnnCH4C[,i] <- CH4C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCHarv2Energy_AnnBCC <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCHarv2Energy_AnnBCC[,i] <- BCC_energy_frac * LCC_AnnEnergyC[,i]
  }
  
  # LCC SLASH 2 ENERGY
  LCC_AnnEnergyC <- out_atmos_df_list[["LCC_Atmos_AnnGain_Slash2EnergyC"]]  
  LCCSlash2Energy_AnnCO2C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCSlash2Energy_AnnCO2C[,i] <- CO2C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCSlash2Energy_AnnCH4C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCSlash2Energy_AnnCH4C[,i] <- CH4C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCSlash2Energy_AnnBCC <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCSlash2Energy_AnnBCC[,i] <- BCC_energy_frac * LCC_AnnEnergyC[,i]
  }
  
  # WILDFIRE
  Wildfire_AnnBurnedC <- out_atmos_df_list[["Fire_Atmos_AnnGain_BurnedC"]]
  Wildfire_AnnCO2C <- Wildfire_AnnBurnedC
  for (i in 5:ncol(Wildfire_AnnBurnedC)) {
    Wildfire_AnnCO2C[,i] <- CO2C_fire_frac * Wildfire_AnnBurnedC[,i]
  }
  Wildfire_AnnCH4C <- Wildfire_AnnBurnedC
  for (i in 5:ncol(Wildfire_AnnBurnedC)) {
    Wildfire_AnnCH4C[,i] <- CH4C_fire_frac * Wildfire_AnnBurnedC[,i]
  }
  Wildfire_AnnBCC <- Wildfire_AnnBurnedC
  for (i in 5:ncol(Wildfire_AnnBurnedC)) {
    Wildfire_AnnBCC[,i] <- BCC_fire_frac * Wildfire_AnnBurnedC[,i]
  }
  
  # TOTAL Ann Controlled Fires CO2-C, CH4-C, and BC-C 
  TotalCntlFire_AnnCO2C <- LCCFire_AnnCO2C
  TotalCntlFire_AnnCO2C[,5:ncol(LCCFire_AnnCO2C)] <- LCCFire_AnnCO2C[,5:ncol(LCCFire_AnnCO2C)] + ManFire_AnnCO2C[,5:ncol(LCCFire_AnnCO2C)]
  TotalCntlFire_AnnCH4C <- LCCFire_AnnCH4C
  TotalCntlFire_AnnCH4C[,5:ncol(LCCFire_AnnCH4C)] <- LCCFire_AnnCH4C[,5:ncol(LCCFire_AnnCH4C)] + ManFire_AnnCH4C[,5:ncol(LCCFire_AnnCH4C)]
  TotalCntlFire_AnnBCC <- LCCFire_AnnBCC
  TotalCntlFire_AnnBCC[,5:ncol(LCCFire_AnnBCC)] <- LCCFire_AnnBCC[,5:ncol(LCCFire_AnnBCC)] + ManFire_AnnBCC[,5:ncol(LCCFire_AnnBCC)]
  
  # TOTAL Ann ENERGY CO2-C, CH4-C, and BC-C 
  TotalEnergy_AnnCO2C <- LCCTotEnergy_AnnCO2C
  TotalEnergy_AnnCO2C[,5:ncol(LCCTotEnergy_AnnCO2C)] <- LCCTotEnergy_AnnCO2C[,5:ncol(LCCTotEnergy_AnnCO2C)] + ManTotEnergy_AnnCO2C[,5:ncol(LCCTotEnergy_AnnCO2C)]
  TotalEnergy_AnnCH4C <- LCCTotEnergy_AnnCH4C
  TotalEnergy_AnnCH4C[,5:ncol(LCCTotEnergy_AnnCH4C)] <- LCCTotEnergy_AnnCH4C[,5:ncol(LCCTotEnergy_AnnCH4C)] + ManTotEnergy_AnnCH4C[,5:ncol(LCCTotEnergy_AnnCH4C)]
  TotalEnergy_AnnBCC <- LCCTotEnergy_AnnBCC
  TotalEnergy_AnnBCC[,5:ncol(LCCTotEnergy_AnnBCC)] <- LCCTotEnergy_AnnBCC[,5:ncol(LCCTotEnergy_AnnBCC)] + ManTotEnergy_AnnBCC[,5:ncol(LCCTotEnergy_AnnBCC)]
  
  ############## Partition Cumulative C emissions from total wood product decay (management & lcc) into CO2-C and CH4-C ############## 
  # subset the total cumulative C emissions from wood decay from the out_atmos_df_list
  wood2atmos_CumC <- out_atmos_df_list[["Wood_Atmos_CumGain_C_stock"]]
  # copy dataframe to fill in the following loop  
  CumANDOC <- wood2atmos_CumC
  # calc anaerobically decomposed wood C based on ARB's weighted mean CH4 correction factor (MCF) of 0.71
  for (i in 5:ncol(wood2atmos_CumC)) {
    CumANDOC[,i] <- MCF * wood2atmos_CumC[,i]
  }
  # copy dataframe to fill in the following loop  
  wood_CumCH4C_prod <- wood2atmos_CumC
  # calc CH4-C production based on ARB and IPCC default value of 0.5 for fraction of CH4, by volume, in generated landfill gas 
  for (i in 5:ncol(wood2atmos_CumC)) {
    wood_CumCH4C_prod[,i] <- CumANDOC[,i] * landfill_gas_frac
  }
  # copy the following dataframes to fill in the following loops for calc CH4-C emissions
  # wood_CumCH4C_combust <- wood2atmos_CumC
  wood_CumCH4C_filter <- wood2atmos_CumC
  wood_CumCH4C <- wood2atmos_CumC
  # calc CH4-C emissions based on ARB equation 89 using landfill gas collection efficiency (CE) = 0.75, landfill gas destruction efficiency 
  # via combustion (DE_combust) = 0.99, and oxidation of uncollected CH4 in landfill cover (OX) = 0.1
  # for (i in 5:ncol(wood2atmos_CumC)) {
  #   wood_CumCH4C_combust[,i] <- wood_CumCH4C_prod[,i] * CE * (1 - DE_combust) + wood_CumCH4C_prod[,i] * (1 - CE) * (1 - OX)
  # }
  # Same equation except using landfill gas destruction efficiency via C filtration (DE_filter) = 0.01
  for (i in 5:ncol(wood2atmos_CumC)) {
    wood_CumCH4C_filter[,i] <- wood_CumCH4C_prod[,i] * CE * (1 - DE_filter) + wood_CumCH4C_prod[,i] * (1 - CE) * (1 - OX)
  }
  # Average CH4C emissions using the 2 methods of CH4 removal
  for (i in 5:ncol(wood2atmos_CumC)) {
    if (exists("wood_CumCH4C_combust")) {
      wood_CumCH4C[,i] <- (wood_CumCH4C_filter[,i] + wood_CumCH4C_combust[,i]) / 2
    } else wood_CumCH4C[,i] <- wood_CumCH4C_filter[,i] 
  }
  # Calc CO2-C emissions from wood
  wood_CumCO2C <- wood2atmos_CumC
  for (i in 5:ncol(wood2atmos_CumC)) {
    wood_CumCO2C[,i] <- wood2atmos_CumC[,i] - wood_CumCH4C[,i]
  }
  
  ############## Partition Annual C emissions from total wood decay (management & lcc) into CO2-C and CH4-C ############## 
  # subset the total annual C emissions from wood decay from the out_atmos_df_list
  wood2atmos_AnnC <- out_atmos_df_list[["Wood_Atmos_AnnGain_C_stock"]]
  # copy dataframe to fill in the following loop  
  AnnANDOC <- wood2atmos_AnnC
  # calc anaerobically decomposed wood C based on ARB's weighted mean CH4 correction factor (MCF) of 0.71
  for (i in 5:ncol(wood2atmos_AnnC)) {
    AnnANDOC[,i] <- MCF * wood2atmos_AnnC[,i]
  }
  # copy dataframe to fill in the following loop  
  wood_AnnCH4C_prod <- wood2atmos_AnnC
  # calc CH4-C production based on ARB and IPCC default value of 0.5 for fraction of CH4, by volume, in generated landfill gas (F)
  for (i in 5:ncol(wood2atmos_AnnC)) {
    wood_AnnCH4C_prod[,i] <- AnnANDOC[,i] * landfill_gas_frac
  }
  # copy the following dataframes to fill in the following loops for calc CH4-C emissions
  # wood_AnnCH4C_combust <- wood2atmos_AnnC
  wood_AnnCH4C_filter <- wood2atmos_AnnC
  wood_AnnCH4C <- wood2atmos_AnnC
  # calc CH4-C emissions based on ARB equation 89 using landfill gas collection efficiency (CE) = 0.75, landfill gas destruction efficiency 
  # via combustion (DE_combust) = 0.99, and oxidation of uncollected CH4 in landfill cover (OX) = 0.1
  # for (i in 5:ncol(wood2atmos_AnnC)) {
  #  wood_AnnCH4C_combust[,i] <- wood_AnnCH4C_prod[,i] * CE * (1 - DE_combust) + wood_AnnCH4C_prod[,i] * (1 - CE) * (1 - OX)
  # }
  # Same equation except using landfill gas destruction efficiency via C filtration (DE_filter) = 0.01
  for (i in 5:ncol(wood2atmos_AnnC)) {
    wood_AnnCH4C_filter[,i] <- wood_AnnCH4C_prod[,i] * CE * (1 - DE_filter) + wood_AnnCH4C_prod[,i] * (1 - CE) * (1 - OX)
  }
  # Average CH4C emissions using the 2 methods of CH4 removal (currently using only C filtration)
  for (i in 5:ncol(wood2atmos_AnnC)) {
    if (exists("wood_AnnCH4C_combust")) {
    wood_AnnCH4C[,i] <- (wood_AnnCH4C_filter[,i] + wood_AnnCH4C_combust[,i]) / 2
    } else wood_AnnCH4C[,i] <- wood_AnnCH4C_filter[,i] 
  }
  # Calc CO2-C emissions from wood
  wood_AnnCO2C <- wood2atmos_AnnC
  for (i in 5:ncol(wood2atmos_AnnC)) {
    wood_AnnCO2C[,i] <- wood2atmos_AnnC[,i] - wood_AnnCH4C[,i]
  }
  
  # SUM total CO2-C, CH4-C, and BC-C emissions from burned and non-burned sources. Total equals net C land-atmos exchange (source vs sink).  
  
  ####### Cumulative ####### 
  # first, do cumulative CO2-C. Choice of ncol(ManFire_CumCO2C) is arbitrary - just need the total number of columns.
  Total_CumCO2C <- ManFire_CumCO2C
  for (i in 5:ncol(Total_CumCO2C)) {
    Total_CumCO2C[,i] <- 0
  }
  for (i in 5:ncol(Total_CumCO2C)) { 
    Total_CumCO2C[,i] <- Eco_CumCO2C[,i] + wood_CumCO2C[,i] + out_atmos_df_list[["Manage_Atmos_CumGain_NonBurnedC"]][,i] + 
      out_atmos_df_list[["Fire_Atmos_CumGain_NonBurnedC"]][,i] + out_atmos_df_list[["LCC_Atmos_CumGain_NonBurnedC"]][,i] + 
      TotalCntlFire_CumCO2C[,i] + TotalEnergy_CumCO2C[,i] + Wildfire_CumCO2C[,i] 
  }
  # Second, do cumulative CH4-C. Choice of ncol(ManFire_CumCH4C) is arbitrary -  just need the total number of columns.
  Total_CumCH4C <- ManFire_CumCH4C
  for (i in 5:ncol(Total_CumCH4C)) {
    Total_CumCH4C[,i] <- 0
  }
  for (i in 5:ncol(Total_CumCH4C)) { 
    Total_CumCH4C[,i] <- Eco_CumCH4C[,i] + wood_CumCH4C[,i] + TotalCntlFire_CumCH4C[,i] + TotalEnergy_CumCH4C[,i] + 
      Wildfire_CumCH4C[,i]  
  }
  # Third, do cumulative BC-C. Choice of ncol(ManFire_CumBCC) is arbitrary -  just need the total number of columns.
  Total_CumBCC <- ManFire_CumBCC
  for (i in 5:ncol(Total_CumBCC)) {
    Total_CumBCC[,i] <- 0
  }
  for (i in 5:ncol(Total_CumBCC)) {
    Total_CumBCC[,i] <- TotalCntlFire_CumBCC[,i] + TotalEnergy_CumBCC[,i] + Wildfire_CumBCC[,i] 
  }
  
  ####### Annual ####### 
  # first, do annual CO2-C. Choice of ncol(ManFire_AnnCO2C) is arbitrary -  just need the total number of columns.
  Total_AnnCO2C <- ManFire_AnnCO2C
  for (i in 5:ncol(Total_AnnCO2C)) {
    Total_AnnCO2C[,i] <- 0
  }
  for (i in 5:ncol(Total_AnnCO2C)) {  
    Total_AnnCO2C[,i] <- Eco_AnnCO2C[,i] + wood_AnnCO2C[,i] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][,i] + 
      out_atmos_df_list[["Fire_Atmos_AnnGain_NonBurnedC"]][,i] + out_atmos_df_list[["LCC_Atmos_AnnGain_NonBurnedC"]][,i] + 
      TotalCntlFire_AnnCO2C[,i] + TotalEnergy_AnnCO2C[,i] + Wildfire_AnnCO2C[,i]  
  }
  # Second, do annual CH4-C. Choice of ncol(ManFire_AnnCH4C) is arbitrary -  just need the total number of columns.
  Total_AnnCH4C <- ManFire_AnnCH4C
  for (i in 5:ncol(Total_AnnCH4C)) {
    Total_AnnCH4C[,i] <- 0
  }
  for (i in 5:ncol(Total_AnnCH4C)) { 
    Total_AnnCH4C[,i] <- Eco_AnnCH4C[,i] + wood_AnnCH4C[,i] + TotalCntlFire_AnnCH4C[,i] + TotalEnergy_AnnCH4C[,i] + 
      Wildfire_AnnCH4C[,i]
  }
  # Third, do annual BC-C. Choice of ncol(ManFire_AnnBCC) is arbitrary -  just need the total number of columns.
  Total_AnnBCC <- ManFire_AnnBCC
  for (i in 5:ncol(Total_AnnBCC)) {
    Total_AnnBCC[,i] <- 0
  }
  for (i in 5:ncol(ManFire_AnnBCC)) {
    Total_AnnBCC[,i] <- TotalCntlFire_AnnBCC[,i] + TotalEnergy_AnnBCC[,i] + Wildfire_AnnBCC[,i]
  }
  
  # the following check is used to show that the differences between total annual CO2-C, CH4-C and BC-C, and
  # total atmosphere C gain, less Eco C to atmosphere fluxes (i.e. grassland and coastal marsh) are < 0.5 and > -0.5. Due to 
  # rounding error, 0.5 is used instead of 0.
  all((Total_AnnCO2C[,5:ncol(Total_AnnCO2C)] + Total_AnnCH4C[,5:ncol(Total_AnnCO2C)] + Total_AnnBCC[,5:ncol(Total_AnnCO2C)]) - 
        (out_atmos_df_list[["Total_Atmos_AnnGain_C_stock"]][1:nrow(Total_AnnCO2C),5:ncol(Total_AnnCO2C)] + 
           Eco_AnnCO2C[,5:ncol(Total_AnnCO2C)] + Eco_AnnCH4C[,5:ncol(Total_AnnCO2C)]) < 0.5 & 
        (Total_AnnCO2C[,5:ncol(Total_AnnCO2C)] + Total_AnnCH4C[,5:ncol(Total_AnnCO2C)] + Total_AnnBCC[,5:ncol(Total_AnnCO2C)]) - 
        (out_atmos_df_list[["Total_Atmos_AnnGain_C_stock"]][1:nrow(Total_AnnCO2C),5:ncol(Total_AnnCO2C)] + 
           Eco_AnnCO2C[,5:ncol(Total_AnnCO2C)] + Eco_AnnCH4C[,5:ncol(Total_AnnCO2C)]) > -0.5)
  
  # individually convert total CO2-C, CH4-C and BC-C to CO2-eq. That way we can analyze proportions contributing to total CO2-eq
  # if desired
  ### cumulative ###
  # first, convert total cumulative CO2-C [Mg C/ha/y] to [Mg CO2-eq/ha/y]
  Total_CumCO2 <- Total_CumCO2C
  for (i in 5:ncol(Total_CumCO2)) {
    Total_CumCO2[,i] <- Total_CumCO2C[,i] * (44.01/12.0107) * gwp_CO2
  }
  # second, convert total cumulative CH4-C [Mg C/ha/y] to [Mg CO2-eq/ha/y]
  Total_CumCH4eq <- Total_CumCH4C
  for (i in 5:ncol(Total_CumCH4eq)) {
    Total_CumCH4eq[,i] <- Total_CumCH4C[,i] * (16.04/12.0107) * gwp_CH4
  }
  # third, convert total cumulative BC-C [Mg C/ha/y] to [Mg CO2-eq/ha/y]
  # multiplying by 1/0.6 based on assumption that 60% black C is C.
  Total_CumBCeq <- Total_CumBCC
  for (i in 5:ncol(Total_CumBCC)) {
    Total_CumBCeq[,i] <- Total_CumBCC[,i] * (1/0.6) * gwp_BC
  }
  
  ### annual ###
  # first, convert total annual CO2-C [Mg C/ha/y] to [Mg CO2-eq/ha/y]
  Total_AnnCO2 <- Total_AnnCO2C
  for (i in 5:ncol(Total_AnnCO2)) {
    Total_AnnCO2[,i] <- Total_AnnCO2C[,i] * (44.01/12.0107) * gwp_CO2
  }
  # second, convert total annual CH4-C [Mg C/ha/y] to [Mg CO2-eq/ha/y]
  Total_AnnCH4eq <- Total_AnnCH4C
  for (i in 5:ncol(Total_AnnCH4eq)) {
    Total_AnnCH4eq[,i] <- Total_AnnCH4C[,i] * (16.04/12.0107) * gwp_CH4
  }
  # third, convert total annual BC-C [Mg C/ha/y] to [Mg CO2-eq/ha/y]
  Total_AnnBCeq <- Total_AnnBCC
  for (i in 5:ncol(Total_AnnBCC)) {
    Total_AnnBCeq[,i] <- Total_AnnBCC[,i] * (1/0.6) * gwp_BC
  }
  
  # sum all CO2-eq to get total GWP [Mg CO2-eq/ha/y]
  ### cumulative ###
  Total_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(Total_CumCO2)) {
    Total_CumCO2eq_all[,i] <- Total_CumCO2[,i] + Total_CumCH4eq[,i] + Total_CumBCeq[,i]
  }
  ### annual ###
  Total_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(Total_AnnCO2)) {
    Total_AnnCO2eq_all[,i] <- Total_AnnCO2[,i] + Total_AnnCH4eq[,i] + Total_AnnBCeq[,i]
  }
  
  ######### Calculate additonal breakdowns of total CO2, CH4, & BC (CO2-eq) from various sources ######### 
  
  # subset outputs from out_atmos to include in df.list below
  ManNonBurn_CumCO2C <- out_atmos_df_list[["Manage_Atmos_CumGain_NonBurnedC"]]
  ManNonBurn_AnnCO2C <- out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]]
  LCCNonBurn_CumCO2C <- out_atmos_df_list[["LCC_Atmos_CumGain_NonBurnedC"]]
  LCCNonBurn_AnnCO2C <- out_atmos_df_list[["LCC_Atmos_AnnGain_NonBurnedC"]]
  
  # create list of all the additonal tables from which we want to calculate GWP 
  df.list <- list(Eco_CumCO2C = Eco_CumCO2C,
                  Eco_CumCH4C = Eco_CumCH4C,
                  
                  ManTotEnergy_CumCO2C = ManTotEnergy_CumCO2C,
                  ManTotEnergy_CumCH4C = ManTotEnergy_CumCH4C,
                  ManTotEnergy_CumBCC = ManTotEnergy_CumBCC,
                  ManHarv2Energy_CumCO2C = ManHarv2Energy_CumCO2C,
                  ManHarv2Energy_CumCH4C = ManHarv2Energy_CumCH4C,
                  ManHarv2Energy_CumBCC = ManHarv2Energy_CumBCC,
                  ManSlash2Energy_CumCO2C = ManSlash2Energy_CumCO2C,
                  ManSlash2Energy_CumCH4C = ManSlash2Energy_CumCH4C,
                  ManSlash2Energy_CumBCC = ManSlash2Energy_CumBCC,
                  ManFire_CumCO2C = ManFire_CumCO2C,
                  ManFire_CumCH4C = ManFire_CumCH4C,
                  ManFire_CumBCC = ManFire_CumBCC,
                  ManNonBurn_CumCO2C = ManNonBurn_CumCO2C,
                  
                  LCCTotEnergy_CumCO2C = LCCTotEnergy_CumCO2C,
                  LCCTotEnergy_CumCH4C = LCCTotEnergy_CumCH4C,
                  LCCTotEnergy_CumBCC  = LCCTotEnergy_CumBCC,
                  LCCHarv2Energy_CumCO2C = LCCHarv2Energy_CumCO2C,
                  LCCHarv2Energy_CumCH4C = LCCHarv2Energy_CumCH4C,
                  LCCHarv2Energy_CumBCC = LCCHarv2Energy_CumBCC,
                  LCCSlash2Energy_CumCO2C = LCCSlash2Energy_CumCO2C,
                  LCCSlash2Energy_CumCH4C = LCCSlash2Energy_CumCH4C,
                  LCCSlash2Energy_CumBCC = LCCSlash2Energy_CumBCC,
                  LCCFire_CumCO2C = LCCFire_CumCO2C,
                  LCCFire_CumCH4C = LCCFire_CumCH4C,
                  LCCFire_CumBCC  = LCCFire_CumBCC,
                  LCCNonBurn_CumCO2C = LCCNonBurn_CumCO2C,
                  
                  TotalEnergy_CumCO2C = TotalEnergy_CumCO2C,
                  TotalEnergy_CumCH4C = TotalEnergy_CumCH4C,
                  TotalEnergy_CumBCC  = TotalEnergy_CumBCC,
                  
                  TotalCntlFire_CumCO2C = TotalCntlFire_CumCO2C,
                  TotalCntlFire_CumCH4C = TotalCntlFire_CumCH4C,
                  TotalCntlFire_CumBCC  = TotalCntlFire_CumBCC,
                  
                  Wildfire_CumCO2C = Wildfire_CumCO2C,
                  Wildfire_CumCH4C = Wildfire_CumCH4C,
                  Wildfire_CumBCC  = Wildfire_CumBCC,
                  
                  Wood_CumCO2C = wood_CumCO2C,
                  Wood_CumCH4C = wood_CumCH4C,
                  
                  Eco_AnnCO2C = Eco_AnnCO2C,
                  Eco_AnnCH4C = Eco_AnnCH4C,
                  
                  ManTotEnergy_AnnCO2C = ManTotEnergy_AnnCO2C,
                  ManTotEnergy_AnnCH4C = ManTotEnergy_AnnCH4C,
                  ManTotEnergy_AnnBCC  = ManTotEnergy_AnnBCC,
                  ManHarv2Energy_AnnCO2C = ManHarv2Energy_AnnCO2C,
                  ManHarv2Energy_AnnCH4C = ManHarv2Energy_AnnCH4C,
                  ManHarv2Energy_AnnBCC = ManHarv2Energy_AnnBCC,
                  ManSlash2Energy_AnnCO2C = ManSlash2Energy_AnnCO2C,
                  ManSlash2Energy_AnnCH4C = ManSlash2Energy_AnnCH4C,
                  ManSlash2Energy_AnnBCC = ManSlash2Energy_AnnBCC,
                  ManFire_AnnCO2C = ManFire_AnnCO2C, 
                  ManFire_AnnCH4C = ManFire_AnnCH4C,
                  ManFire_AnnBCC  = ManFire_AnnBCC,
                  ManNonBurn_AnnCO2C = ManNonBurn_AnnCO2C,
                  
                  LCCTotEnergy_AnnCO2C = LCCTotEnergy_AnnCO2C,
                  LCCTotEnergy_AnnCH4C = LCCTotEnergy_AnnCH4C,
                  LCCTotEnergy_AnnBCC  = LCCTotEnergy_AnnBCC,
                  LCCHarv2Energy_AnnCO2C = LCCHarv2Energy_AnnCO2C,
                  LCCHarv2Energy_AnnCH4C = LCCHarv2Energy_AnnCH4C,
                  LCCHarv2Energy_AnnBCC = LCCHarv2Energy_AnnBCC,
                  LCCSlash2Energy_AnnCO2C = LCCSlash2Energy_AnnCO2C,
                  LCCSlash2Energy_AnnCH4C = LCCSlash2Energy_AnnCH4C,
                  LCCSlash2Energy_AnnBCC = LCCSlash2Energy_AnnBCC,
                  LCCFire_AnnCO2C = LCCFire_AnnCO2C,
                  LCCFire_AnnCH4C = LCCFire_AnnCH4C,
                  LCCFire_AnnBCC  = LCCFire_AnnBCC,
                  LCCNonBurn_AnnCO2C = LCCNonBurn_AnnCO2C,
                  
                  TotalEnergy_AnnCO2C = TotalEnergy_AnnCO2C,
                  TotalEnergy_AnnCH4C = TotalEnergy_AnnCH4C,
                  TotalEnergy_AnnBCC  = TotalEnergy_AnnBCC,
                  
                  TotalCntlFire_AnnCO2C = TotalCntlFire_AnnCO2C,
                  TotalCntlFire_AnnCH4C = TotalCntlFire_AnnCH4C,
                  TotalCntlFire_AnnBCC  = TotalCntlFire_AnnBCC,
                  
                  Wildfire_AnnCO2C = Wildfire_AnnCO2C,
                  Wildfire_AnnCH4C = Wildfire_AnnCH4C,
                  Wildfire_AnnBCC  = Wildfire_AnnBCC,
                  
                  Wood_AnnCO2C = wood_AnnCO2C,
                  Wood_AnnCH4C = wood_AnnCH4C
                  )
  
  # calc GWP [CO2-eq] for each table in df.list
  new.df <- c()
  for (i in length(df.list)) { 
    new.df <- c(new.df, CALC.GWP(df.list, gwp_CO2=gwp_CO2, gwp_CH4=gwp_CH4, gwp_BC=gwp_BC))
  }
  
  # get list of new names for df.list that drops the last 'C' in CO2C, CH4C, and BCC 
  # and adds 'eq' for CH4 and BC  
  new.name <- c()
  for (i in length(df.list)) { 
    new.name <- c(new.name, GET.NAMES(df.list, new.name=new.name))
  }
  
  # replace names of the elements in new.df with the CO2-eq names
  names(new.df) <- paste0(new.name)
  
  # sum total wood CO2eq 
  ### cumulative ###
  TotalWood_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalWood_CumCO2eq_all)) {
    TotalWood_CumCO2eq_all[,i] <- new.df[["Wood_CumCO2"]][,i] + new.df[["Wood_CumCH4eq"]][,i]
  }
  ### annual ###
  TotalWood_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalWood_AnnCO2eq_all)) {
    TotalWood_AnnCO2eq_all[,i] <- new.df[["Wood_AnnCO2"]][,i] + new.df[["Wood_AnnCH4eq"]][,i]
  }
  
  # sum total burned CO2eq (manage energy + manage fire + lcc fire + lcc energy + wildfire)
  ### cumulative ###
  TotalBurn_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalBurn_CumCO2eq_all)) {
    TotalBurn_CumCO2eq_all[,i] <- new.df[["ManTotEnergy_CumCO2"]][,i] + new.df[["ManTotEnergy_CumCH4eq"]][,i] + new.df[["ManTotEnergy_CumBCeq"]][,i] +
    new.df[["TotalCntlFire_CumCO2"]][,i] + new.df[["TotalCntlFire_CumCH4eq"]][,i] + new.df[["TotalCntlFire_CumBCeq"]][,i] + new.df[["LCCTotEnergy_CumCO2"]][,i] +
    new.df[["LCCTotEnergy_CumCH4eq"]][,i] + new.df[["LCCTotEnergy_CumBCeq"]][,i] + new.df[["Wildfire_CumCO2"]][,i] + new.df[["Wildfire_CumCH4eq"]][,i] +
    new.df[["Wildfire_CumBCeq"]][,i] 
  }
  # sum total non-burned CO2eq (eco + manage + lcc)  *Wood decay not included
  TotalNonBurn_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalNonBurn_CumCO2eq_all)) {
    TotalNonBurn_CumCO2eq_all[,i] <- new.df[["Eco_CumCO2"]][,i] + new.df[["Eco_CumCH4eq"]][,i] + new.df[["ManNonBurn_CumCO2"]][,i] +
      new.df[["LCCNonBurn_CumCO2"]][,i] 
  }
  ### annual ###
  # sum total burned CO2eq (manage energy + manage fire + lcc fire + lcc energy + wildfire)
  TotalBurn_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalBurn_AnnCO2eq_all)) {
    TotalBurn_AnnCO2eq_all[,i] <- new.df[["ManTotEnergy_AnnCO2"]][,i] + new.df[["ManTotEnergy_AnnCH4eq"]][,i] + new.df[["ManTotEnergy_AnnBCeq"]][,i] +
      new.df[["TotalCntlFire_AnnCO2"]][,i] + new.df[["TotalCntlFire_AnnCH4eq"]][,i] + new.df[["TotalCntlFire_AnnBCeq"]][,i] + new.df[["LCCTotEnergy_AnnCO2"]][,i] +
      new.df[["LCCTotEnergy_AnnCH4eq"]][,i] + new.df[["LCCTotEnergy_AnnBCeq"]][,i] + new.df[["Wildfire_AnnCO2"]][,i] + new.df[["Wildfire_AnnCH4eq"]][,i] +
      new.df[["Wildfire_AnnBCeq"]][,i]
  }
  # sum total non-burned CO2eq (eco + manage + lcc) *Wood decay not included
  TotalNonBurn_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalNonBurn_AnnCO2eq_all)) {
    TotalNonBurn_AnnCO2eq_all[,i] <- new.df[["Eco_AnnCO2"]][,i] + new.df[["Eco_AnnCH4eq"]][,i] + new.df[["ManNonBurn_AnnCO2"]][,i] +
      new.df[["LCCNonBurn_AnnCO2"]][,i] 
  }
  
  # sum total energy CO2eq (manage energy + lcc energy) and total fire CO2eq (manage fire + lcc fire + wildfire) 
  ### cumulative energy ###
  TotalEnergy_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalEnergy_CumCO2eq_all)) {
    TotalEnergy_CumCO2eq_all[,i] <- new.df[["ManTotEnergy_CumCO2"]][,i] + new.df[["ManTotEnergy_CumCH4eq"]][,i] + new.df[["ManTotEnergy_CumBCeq"]][,i] +
      new.df[["LCCTotEnergy_CumCO2"]][,i] + new.df[["LCCTotEnergy_CumCH4eq"]][,i] + new.df[["LCCTotEnergy_CumBCeq"]][,i] 
  }
  ### cumulative fire ###
  TotalFire_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalFire_CumCO2eq_all)) { 
    TotalFire_CumCO2eq_all[,i] <- new.df[["TotalCntlFire_CumCO2"]][,i] + new.df[["TotalCntlFire_CumCH4eq"]][,i] + new.df[["TotalCntlFire_CumBCeq"]][,i] + 
      new.df[["Wildfire_CumCO2"]][,i] + new.df[["Wildfire_CumCH4eq"]][,i] + new.df[["Wildfire_CumBCeq"]][,i]
  }
  ### annual energy ###
  TotalEnergy_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalEnergy_AnnCO2eq_all)) {
    TotalEnergy_AnnCO2eq_all[,i] <- new.df[["ManTotEnergy_AnnCO2"]][,i] + new.df[["ManTotEnergy_AnnCH4eq"]][,i] + new.df[["ManTotEnergy_AnnBCeq"]][,i] +
      new.df[["LCCTotEnergy_AnnCO2"]][,i] + new.df[["LCCTotEnergy_AnnCH4eq"]][,i] + new.df[["LCCTotEnergy_AnnBCeq"]][,i] 
  }
  ### annual fire ###
  TotalFire_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalFire_AnnCO2eq_all)) {
    TotalFire_AnnCO2eq_all[,i] <- new.df[["TotalCntlFire_AnnCO2"]][,i] + new.df[["TotalCntlFire_AnnCH4eq"]][,i] + new.df[["TotalCntlFire_AnnBCeq"]][,i] + 
      new.df[["Wildfire_AnnCO2"]][,i] + new.df[["Wildfire_AnnCH4eq"]][,i] + new.df[["Wildfire_AnnBCeq"]][,i]
  }
  
  # combine new.df with the additional CO2eq breakdowns with the out_atmos_df_list
  out_atmos_df_list <- c(out_atmos_df_list, new.df)
  
  # add GHG dataframes to out_atmos_df_list
  out_atmos_df_list[["Total_CumCO2"]] <- Total_CumCO2
  out_atmos_df_list[["Total_CumCH4eq"]] <- Total_CumCH4eq
  out_atmos_df_list[["Total_CumBCeq"]] <- Total_CumBCeq
  out_atmos_df_list[["Total_AnnCO2"]] <- Total_AnnCO2
  out_atmos_df_list[["Total_AnnCH4eq"]] <- Total_AnnCH4eq
  out_atmos_df_list[["Total_AnnBCeq"]] <- Total_AnnBCeq
  out_atmos_df_list[["TotalWood_CumCO2eq_all"]] <- TotalWood_CumCO2eq_all
  out_atmos_df_list[["TotalWood_AnnCO2eq_all"]] <- TotalWood_AnnCO2eq_all
  out_atmos_df_list[["TotalNonBurn_CumCO2eq_all"]] <- TotalNonBurn_CumCO2eq_all
  out_atmos_df_list[["TotalFire_CumCO2eq_all"]] <- TotalFire_CumCO2eq_all
  out_atmos_df_list[["TotalEnergy_CumCO2eq_all"]] <- TotalEnergy_CumCO2eq_all
  out_atmos_df_list[["TotalNonBurn_AnnCO2eq_all"]] <- TotalNonBurn_AnnCO2eq_all
  out_atmos_df_list[["TotalFire_AnnCO2eq_all"]] <- TotalFire_CumCO2eq_all
  out_atmos_df_list[["TotalEnergy_AnnCO2eq_all"]] <- TotalEnergy_CumCO2eq_all
  out_atmos_df_list[["TotalBurn_CumCO2eq_all"]] <- TotalBurn_CumCO2eq_all
  out_atmos_df_list[["TotalBurn_AnnCO2eq_all"]] <- TotalBurn_AnnCO2eq_all
  out_atmos_df_list[["Total_CumCO2eq_all"]] <- Total_CumCO2eq_all
  out_atmos_df_list[["Total_AnnCO2eq_all"]] <- Total_AnnCO2eq_all
  
  # replaces any -0 with 0
  for (i in 1:length(out_atmos_df_list)) {
    for (c in 5:ncol(out_atmos_df_list[[i]])) {
      out_atmos_df_list[[i]][,c] <- replace(out_atmos_df_list[[i]][,c], out_atmos_df_list[[i]][,c] == 0, 0.00000000) 
    }
    
  }
  # check that total atmos C gain is equal to the sum of the partitions, less the Eco C emissions (not included in the Total_Atmos gain C)
  zero_test <- Total_AnnCO2
  for (i in 5:ncol(zero_test)) {
    zero_test[,i] <- 0
  }
  for (i in 5:ncol(Total_AnnCO2)) {
    zero_test[,i] <- out_atmos_df_list[["Total_Atmos_AnnGain_C_stock"]][,i] - 
      (Total_AnnCO2[,i] * (12.0107/44.01) + Total_AnnCH4eq[,i] * (12.0107/(gwp_CH4*16.04)) + Total_AnnBCeq[,i] * (0.6/gwp_BC) - 
         Eco_AnnCO2C[,i] - Eco_AnnCH4C[,i])
  } 
  
  # rounding error so this test is false
  all(zero_test[5:ncol(zero_test)] == 0) 
  # but this checks to be true
  all(zero_test[5:ncol(zero_test)] < 1 & zero_test[5:ncol(zero_test)] > -1) 
  
  ###############################  Calculate some changes and totals  ###############################  
  # also round everything to 2 decimal places: ha, MgC and MgC/ha
  # the realistic precision is integer
  # but going to integer here cuts out some area that has some carbon
  cat("Starting change/total calcs...\n")
  # create columns in each section below for changes between initial and final year (final - initial) for all rows
  
  ######### (1) TOTAL AREA  ######### 
  # add column of total land/ocean area change for each land-type - ownership combination###### area ######
  out_area_df_list[[1]][, "Change_ha"] = out_area_df_list[[1]][,end_area_label] - out_area_df_list[[1]][,start_area_label]
  
  # (1a) Do each landtype
  # add rows for aggregate sums by landtype:
  # get names of landtypes
  landtype_names <- unique(out_area_df_list[[1]][,"Land_Type"])
  # create df to store all the landtype sums in the following loop
  all_landtype_sums <- out_area_df_list[[1]][1:length(landtype_names),]
  for (l in 1:length(landtype_names)) {
    # get current landtype name
    landtype_name <- landtype_names[l]
    # name the landtype cell accordingly
    all_landtype_sums[l,c(1:4)] <- c(-1, "All_region", landtype_names[l], "All_own")
    # subset landtype-specific df
    landtype_df_temp <- out_area_df_list[[1]][out_area_df_list[[1]][,"Land_Type"] == landtype_name,] 
    # aggregate sum areas of landtype-specific areas for each column year
    all_landtype_sums[l,c(5:ncol(all_landtype_sums))] <- apply(landtype_df_temp[,c(5:ncol(landtype_df_temp))], 2, sum)
  }
  # add the aggregated landtype rows to out_area_df_list[[1]]
  out_area_df_list[[1]] <- rbind(out_area_df_list[[1]], all_landtype_sums)
  
  # (1b) Do each region (Excludes Ocean)
  # add rows for aggregate sums by region:
  # get names of regions
  region_names <- unique(out_area_df_list[[1]][,"Region"])
  # remove "All_region" from region_names
  region_names <- region_names[!region_names %in% c("All_region","Ocean")]
  # create df to store all the region sums in the following loop
  all_region_sums <- out_area_df_list[[1]][1:length(region_names),]
  for (r in 1:length(region_names)) {
    # get current region name
    region_name <- region_names[r]
    # name the region cell accordingly
    all_region_sums[r,c(1:4)] <- c(-1, region_names[r], "All_land", "All_own")
    # subset region-specific df
    region_df_temp <- out_area_df_list[[1]][out_area_df_list[[1]][,"Region"] == region_name,] 
    # aggregate sum areas of region-specific areas for each column year
    all_region_sums[r,c(5:ncol(all_region_sums))] <- apply(region_df_temp[,c(5:ncol(region_df_temp))], 2, sum)
  }
  # add the aggregated region rows to out_area_df_list[[1]]
  out_area_df_list[[1]] = rbind(out_area_df_list[[1]], all_region_sums)
  
  # (1c) Do all California land (Excludes Ocean)
  # create single row for it
  sum_row = out_area_df_list[[1]][1,]
  sum_row[,c(1:4)] = c(-1, "All_region", "All_land", "All_own")
  sum_row[,c(5:ncol(sum_row))] = 
    apply(out_area_df_list[[1]][out_area_df_list[[1]][, "Land_Type"] == "All_land", c(5:ncol(out_area_df_list[[1]]))], 2 , sum)
  out_area_df_list[[1]] = rbind(out_area_df_list[[1]], sum_row)
  # do a another subtraction for change column
  out_area_df_list[[1]][,ncol(out_area_df_list[[1]])]<-out_area_df_list[[1]][,end_area_label] - out_area_df_list[[1]][,start_area_label]
  # round
  out_area_df_list[[1]][,c(5:ncol(out_area_df_list[[1]]))] = round(out_area_df_list[[1]][,c(5:ncol(out_area_df_list[[1]]))], 2)
 
  ######### (2) MANAGED & WILFIRE AREA  ######### 
  for (i in 2:num_out_area_sheets) {
    end_label = ncol(out_area_df_list[[i]])
    out_area_df_list[[i]][, "Change_ha"] = out_area_df_list[[i]][,end_label] - out_area_df_list[[i]][,start_area_label]
    # if there are no prescribed management practices, skip this section for the Managed_area output (out_area_df_list[[2]])
    if (nrow(man_adjust_df)>0 | i == 3) {
    # (2a) do each landtype within the current df in out_area_df_list
    landtype_names <- unique(out_area_df_list[[i]][,"Land_Type"])
    # create df to store all the landtype sums in the following loop for current df in out_area_df_list
    all_landtype_sums <- out_area_df_list[[i]][1:length(landtype_names),]
    for (l in 1:length(landtype_names)) {
      # get current landtype name
      landtype_name <- landtype_names[l]
      # name the landtype cell accordingly
      all_landtype_sums[l,c(1:5)] <- c(-1, "All_region", landtype_names[l], "All_own", "All")
      # subset landtype-specific df
      landtype_df_temp <- out_area_df_list[[i]][out_area_df_list[[i]][,"Land_Type"] == landtype_name,] 
      # aggregate sum areas of landtype-specific areas for each column year
      all_landtype_sums[l,c(6:ncol(all_landtype_sums))] <- apply(landtype_df_temp[,c(6:ncol(landtype_df_temp))], 2, sum)
    }
    # add the aggregated landtype rows to out_area_df_list[[i]]
    out_area_df_list[[i]] <- rbind(out_area_df_list[[i]], all_landtype_sums)
    
    # (2b) do each region within the current df in out_area_df_list (Excludes Ocean)
    region_names <- unique(out_area_df_list[[i]][,"Region"])
    # remove "All_region" from region_names
    region_names <- region_names[!region_names %in% c("All_region","Ocean")]
    # create df to store all the region sums in the following loop for current df in out_area_df_list
    all_region_sums <- out_area_df_list[[i]][1:length(region_names),]
    for (r in 1:length(region_names)) {
      # get current region name
      region_name <- region_names[r]
      # name the region cell accordingly
      all_region_sums[r,c(1:5)] <- c(-1, region_names[r], "All_land", "All_own", "All")
      # subset region-specific df
      region_df_temp <- out_area_df_list[[i]][out_area_df_list[[i]][,"Region"] == region_name,] 
      # aggregate sum areas of region-specific areas for each column year
      all_region_sums[r,c(6:ncol(all_region_sums))] <- apply(region_df_temp[,c(6:ncol(region_df_temp))], 2, sum)
    }
    # add the aggregated region rows to out_area_df_list[[i]]
    out_area_df_list[[i]] = rbind(out_area_df_list[[i]], all_region_sums)
    
    # (2c) do all California land for the current df in out_area_df_list (Excludes Ocean)
    sum_row = out_area_df_list[[i]][1,]
    sum_row[,c(1:5)] = c(-1, "All_region", "All_land", "All_own", "All")
    sum_row[,c(6:ncol(sum_row))] = apply(out_area_df_list[[i]][out_area_df_list[[i]][, "Land_Type"] == "All_land", 
                                    c(6:ncol(out_area_df_list[[i]]))], 2 , sum)
    out_area_df_list[[i]] = rbind(out_area_df_list[[i]], sum_row)
    # do a another subtraction for change column
    out_area_df_list[[i]][,ncol(out_area_df_list[[i]])]<-out_area_df_list[[i]][,end_label] - out_area_df_list[[i]][,start_area_label]
    # round
    out_area_df_list[[i]][,c(6:ncol(out_area_df_list[[i]]))] = round(out_area_df_list[[i]][,c(6:ncol(out_area_df_list[[i]]))], 2)
    } # end if there are prescribed management practices
  }
  
  ######### (3) DENSITY ######### 
  for (i in 1:num_out_density_sheets) {
    out_density_df_list[[i]][, "Change_Mg_ha"] <- out_density_df_list[[i]][,end_density_label] - out_density_df_list[[i]][,start_density_label]
    
    # (3a) do each landtype within the current df in out_density_df_list
    landtype_names <- unique(out_density_df_list[[i]][,"Land_Type"]) 
    # create df to store all the landtype averages in the following loop for current df in out_density_df_list
    all_landtype_avg <- out_density_df_list[[i]][1:length(landtype_names),]
    for (l in 1:length(landtype_names)) { 
      # get current landtype name
      landtype_name <- landtype_names[l]
      # name the landtype cell accordingly
      all_landtype_avg[l,c(1:4)] <- c(-1, "All_region", landtype_names[l], "All_own")
      # subset landtype-specific density df
      landtype_df_dens_temp <- out_density_df_list[[i]][out_density_df_list[[i]][,"Land_Type"] == landtype_name,] 
      # subset landtype-specific area df (landtype_names already excludes Seagrass)
      landtype_df_area_temp <- out_area_df_list[[1]][out_area_df_list[[1]][,"Land_Type"] == landtype_name,] 
      # delete last row to exclude the All_region row
      landtype_df_area_temp <- landtype_df_area_temp[landtype_df_area_temp$Region != "All_region",]
      # get total C for current landtype: aggregate landtype-specific densities*areas by each column year (excluding Seagrass)
      all_landtype_avg[l,c(5:ncol(all_landtype_avg))] <- 
        apply(landtype_df_dens_temp[,c(5:ncol(landtype_df_dens_temp))] * landtype_df_area_temp[,c(5:ncol(landtype_df_area_temp))],  
              2, sum)
      # get total area for current landtype: aggregate landtype-specific
      landtype_tot_area <- out_area_df_list[[1]][(out_area_df_list[[1]][, "Land_Type"] == landtype_name) &
                              (out_area_df_list[[1]][, "Region"] == "All_region"),]
      # get avg C density for current landtype (excluding last column for Change), divide total Mg C by total area (get original density?)
      all_landtype_avg[l,c(5:ncol(all_landtype_avg))] <- all_landtype_avg[l, c(5:ncol(all_landtype_avg))] / 
        landtype_tot_area[, c(5:ncol(landtype_tot_area))]
    }
    # add the aggregated landtype rows to out_density_df_list[[i]]
    out_density_df_list[[i]] = rbind(out_density_df_list[[i]], all_landtype_avg)
    
    # (3b) do each region within the current df in out_density_df_list (Excluded Ocean)
    region_names <- unique(out_density_df_list[[i]][,"Region"])
    # remove Ocean & All_region 
    region_names <- region_names[!region_names %in% c("Ocean","All_region")]
    # create df to store all the region averages in the following loop for current df in out_density_df_list
    all_region_avg <- out_density_df_list[[i]][1:length(region_names),]
    for (r in 1:length(region_names)) { 
      # get current region name
      region_name <- region_names[r]
      # name the region cell accordingly
      all_region_avg[r,c(1:4)] <- c(-1, region_names[r], "All_land", "All_own")
      # subset region-specific density df
      region_df_dens_temp <- out_density_df_list[[i]][out_density_df_list[[i]][,"Region"] == region_name,] 
      # delete last row to exclude the All_landtype row
      region_df_dens_temp <- region_df_dens_temp[region_df_dens_temp$Land_Type != "All_land",]
      # subset region-specific area df 
      region_df_area_temp <- out_area_df_list[[1]][out_area_df_list[[1]][,"Region"] == region_name,] 
      # delete last row to exclude the All_landtype row
      region_df_area_temp <- region_df_area_temp[region_df_area_temp$Land_Type != "All_land",]
      # get total C for current region: aggregate region-specific densities*areas by each column year (excluding Seagrass)
      all_region_avg[r,c(5:ncol(all_region_avg))] <- 
        apply(region_df_dens_temp[,c(5:ncol(region_df_dens_temp))] * region_df_area_temp[,c(5:ncol(region_df_area_temp))],  
              2, sum)
      # get total area for current region: aggregate region-specific
      region_tot_area <- out_area_df_list[[1]][(out_area_df_list[[1]][, "Region"] == region_name) &
                                                   (out_area_df_list[[1]][, "Land_Type"] == "All_land"),]
      # get avg C density for current region (excluding last column for Change), divide total Mg C by total area (get original density?)
      all_region_avg[r,c(5:ncol(all_region_avg))] <- all_region_avg[r, c(5:ncol(all_region_avg))] / 
        region_tot_area[, c(5:ncol(region_tot_area))]
    }
    # add the aggregated region rows to out_density_df_list[[i]]
    out_density_df_list[[i]] = rbind(out_density_df_list[[i]], all_region_avg)
    
    # (3c) Do all California land for the current df in out_density_df_list (Excludes Ocean)
    avg_row = out_density_df_list[[i]][1,]
    avg_row[,c(1:4)] = c(-1, "All_region", "All_land", "All_own")
    # calc all California land total C for each year
    avg_row[,c(5:ncol(avg_row))] = 
      apply(out_density_df_list[[i]][out_density_df_list[[i]][, "Land_Type"] == "All_land", c(5:ncol(out_density_df_list[[i]]))] *   
              out_area_df_list[[1]][out_area_df_list[[1]][, "Land_Type"] == "All_land" & out_area_df_list[[1]][, "Region"] != "All_region", 
                                    c(5:ncol(out_area_df_list[[1]]))], 2, sum)
    # calc all California land area-weighted avg C density for each year
    avg_row[1,c(5:(ncol(avg_row)-1))] = avg_row[1,c(5:(ncol(avg_row)-1))] / 
      out_area_df_list[[1]][out_area_df_list[[1]][, "Region"] == "All_region" & out_area_df_list[[1]][, "Land_Type"] == "All_land", 
                            c(5:(ncol(out_area_df_list[[1]])-1))]
    out_density_df_list[[i]] = rbind(out_density_df_list[[i]], avg_row)
    # final - initial 
    out_density_df_list[[i]][,ncol(out_density_df_list[[i]])] <- out_density_df_list[[i]][,end_density_label] - out_density_df_list[[i]][,start_density_label]
    # round
    out_density_df_list[[i]][,c(5:ncol(out_density_df_list[[i]]))] = round(out_density_df_list[[i]][,c(5:ncol(out_density_df_list[[i]]))], 2)
  }
  
  ######### (4) C STOCK ######### 
  for (i in 1:num_out_stock_sheets) {
    out_stock_df_list[[i]][, "Change_Mg"] = out_stock_df_list[[i]][,end_stock_label] - out_stock_df_list[[i]][,start_stock_label]
    
    # (4a) do each landtype within the current df in out_stock_df_list
    landtype_names <- unique(out_stock_df_list[[i]][,"Land_Type"]) 
    # create df to store all the landtype averages in the following loop for current df in out_stock_df_list
    all_landtype_stock <- out_stock_df_list[[i]][1:length(landtype_names),]
    for (l in 1:length(landtype_names)) { 
      # get current landtype name
      landtype_name <- landtype_names[l]
      # name the landtype cell accordingly
      all_landtype_stock[l,c(1:4)] <- c(-1, "All_region", landtype_names[l], "All_own")
      # subset landtype-specific stock df
      landtype_df_stock_temp <- out_stock_df_list[[i]][out_stock_df_list[[i]][,"Land_Type"] == landtype_name,] 
      # aggregate sum stock of landtype-specific areas for each column year
      all_landtype_stock[l,c(5:ncol(all_landtype_stock))] <- apply(landtype_df_stock_temp[,c(5:ncol(landtype_df_stock_temp))], 2, sum)
    }
    # add the aggregated landtype rows to out_stock_df_list[[i]]
    out_stock_df_list[[i]] = rbind(out_stock_df_list[[i]], all_landtype_stock)
    
    # (4b) do each region (Excludes Ocean)
    region_names <- unique(out_stock_df_list[[i]][,"Region"])
    # remove All_region 
    region_names <- region_names[!region_names %in% "All_region"]
    # remove "All_region" from region_names
    region_names <- region_names[!region_names %in% c("All_region","Ocean")]
    # create df to store all the region averages in the following loop for current df in out_stock_df_list
    all_region_stock <- out_stock_df_list[[i]][1:length(region_names),]
    for (r in 1:length(region_names)) { 
      # get current region name
      region_name <- region_names[r]
      # name the region cell accordingly
      all_region_stock[r,c(1:4)] <- c(-1, region_names[r], "All_land", "All_own")
      # subset region-specific stock df
      region_df_stock_temp <- out_stock_df_list[[i]][out_stock_df_list[[i]][,"Region"] == region_name,] 
      # delete the All_land row
      region_df_stock_temp <- region_df_stock_temp[region_df_stock_temp$Land_Type != "All_land",]
      # aggregate sum stock of region-specific areas for each column year
      all_region_stock[r,c(5:ncol(all_region_stock))] <- apply(region_df_stock_temp[,c(5:ncol(region_df_stock_temp))], 2, sum)
    }
      # add the aggregated landtype rows to out_stock_df_list[[i]]
      out_stock_df_list[[i]] = rbind(out_stock_df_list[[i]], all_region_stock)
      
    # (4c) do all Calif land (Excludes Ocean)
    sum_row = out_stock_df_list[[i]][1,]
    sum_row[,c(1:4)] = c(-1, "All_region", "All_land", "All_own")
    sum_row[,c(5:ncol(sum_row))] = apply(out_stock_df_list[[i]][out_stock_df_list[[i]][, "Land_Type"] == "All_land", 
                                                                c(5:ncol(out_stock_df_list[[i]]))], 2, sum)
    out_stock_df_list[[i]] = rbind(out_stock_df_list[[i]], sum_row)
    # subtract final-intial again now that all the sections are done
    out_stock_df_list[[i]][, "Change_Mg"] = out_stock_df_list[[i]][,end_stock_label] - out_stock_df_list[[i]][,start_stock_label]
    # round 
    out_stock_df_list[[i]][,c(5:ncol(out_stock_df_list[[i]]))] = round(out_stock_df_list[[i]][,c(5:ncol(out_stock_df_list[[i]]))], 2)
  }
  
  ######### (5) WOOD ######### 
  for (i in 1:num_out_wood_sheets) {
    end_label = ncol(out_wood_df_list[[i]])
    out_wood_df_list[[i]][, "Change_Mg"] = out_wood_df_list[[i]][,end_label] - out_wood_df_list[[i]][,start_wood_label]
    # (5a) do all land types 
    landtype_names <- unique(out_wood_df_list[[i]][,"Land_Type"]) 
    # create df to store all the landtype averages in the following loop for current df in out_wood_df_list
    all_landtype_wood <- out_wood_df_list[[i]][1:length(landtype_names),]
    for (l in 1:length(landtype_names)) { 
      # get current landtype name
      landtype_name <- landtype_names[l]
      # name the landtype cell accordingly
      all_landtype_wood[l,c(1:4)] <- c(-1, "All_region", landtype_names[l], "All_own")
      # subset landtype-specific wood df
      landtype_df_wood_temp <- out_wood_df_list[[i]][out_wood_df_list[[i]][,"Land_Type"] == landtype_name,] 
      # aggregate sum wood of landtype-specific areas for each column year
      all_landtype_wood[l,c(5:ncol(all_landtype_wood))] <- apply(landtype_df_wood_temp[,c(5:ncol(landtype_df_wood_temp))], 2, sum)
    }
    # add the aggregated landtype rows to out_wood_df_list[[i]]
    out_wood_df_list[[i]] = rbind(out_wood_df_list[[i]], all_landtype_wood)
    
    # (5b) do all Regions (Excludes Ocean)
    region_names <- unique(out_wood_df_list[[i]][,"Region"])
    # remove All_region 
    region_names <- region_names[!region_names %in% "All_region"]
    # remove "All_region" from region_names
    region_names <- region_names[!region_names %in% c("All_region","Ocean")]
    # create df to store all the region averages in the following loop for current df in out_wood_df_list
    all_region_wood <- out_wood_df_list[[i]][1:length(region_names),]
    for (r in 1:length(region_names)) { 
      # get current region name
      region_name <- region_names[r]
      # name the region cell accordingly
      all_region_wood[r,c(1:4)] <- c(-1, region_names[r], "All_land", "All_own")
      # subset region-specific wood df
      region_df_wood_temp <- out_wood_df_list[[i]][out_wood_df_list[[i]][,"Region"] == region_name,] 
      # delete the All_land row
      region_df_wood_temp <- region_df_wood_temp[region_df_wood_temp$Land_Type != "All_land",]
      # aggregate sum wood of region-specific areas for each column year
      all_region_wood[r,c(5:ncol(all_region_wood))] <- apply(region_df_wood_temp[,c(5:ncol(region_df_wood_temp))], 2, sum)
    }
    # add the aggregated landtype rows to out_wood_df_list[[i]]
    out_wood_df_list[[i]] = rbind(out_wood_df_list[[i]], all_region_wood)
    
    # (5c) do all California land (excludes Ocean)
    sum_row = out_wood_df_list[[i]][1,]
    sum_row[,c(1:4)] = c(-1, "All_region", "All_land", "All_own")
    sum_row[,c(5:ncol(sum_row))] <- apply(out_wood_df_list[[i]][out_wood_df_list[[i]][, "Land_Type"] == "All_land", 
                                                                c(5:ncol(out_wood_df_list[[i]]))], 2, sum)
    out_wood_df_list[[i]] = rbind(out_wood_df_list[[i]], sum_row)
    # subtract final-intial again now that all the sections are done
    out_wood_df_list[[i]][, "Change_Mg"] = out_wood_df_list[[i]][,end_label] - out_wood_df_list[[i]][,start_wood_label]
    # round 
    out_wood_df_list[[i]][,c(5:ncol(out_wood_df_list[[i]]))] = round(out_wood_df_list[[i]][,c(5:ncol(out_wood_df_list[[i]]))], 2)
  }
  
  # remove the Xs added to the front of the year columns so that the following atmosphere section can work without error
  colnames_Ann = names(out_atmos_df_list[[8]])
  colnames_Cum = names(out_atmos_df_list[[1]])
  names(out_atmos_df_list[["Eco_AnnCO2"]]) = colnames_Ann
  names(out_atmos_df_list[["Eco_AnnCH4eq"]]) = colnames_Ann
  names(out_atmos_df_list[["Eco_CumCO2"]]) = colnames_Cum
  names(out_atmos_df_list[["Eco_CumCH4eq"]]) = colnames_Cum
  
  
  ######### (6) ATMOSHPHERE #########
  for (i in 1:length(out_atmos_df_list)) {
    end_label = ncol(out_atmos_df_list[[i]])
    out_atmos_df_list[[i]][, "Change_Mg"] = out_atmos_df_list[[i]][,end_label] - out_atmos_df_list[[i]][,start_atmos_label]
    # (6a) do each landtype
    landtype_names <- unique(out_atmos_df_list[[i]][,"Land_Type"]) 
    # create df to store all the landtype averages in the following loop for current df in out_atmos_df_list
    all_landtype_atmos <- out_atmos_df_list[[i]][1:length(landtype_names),]
    for (l in 1:length(landtype_names)) { 
      # get current landtype name
      landtype_name <- landtype_names[l]
      # name the landtype cell accordingly
      all_landtype_atmos[l,c(1:4)] <- c(-1, "All_region", landtype_names[l], "All_own")
      # subset landtype-specific atmos df
      landtype_df_atmos_temp <- out_atmos_df_list[[i]][out_atmos_df_list[[i]][,"Land_Type"] == landtype_name,] 
      # aggregate sum atmos of landtype-specific areas for each column year
      all_landtype_atmos[l,c(5:ncol(all_landtype_atmos))] <- apply(landtype_df_atmos_temp[,c(5:ncol(landtype_df_atmos_temp))], 2, sum)
    }
    # add the aggregated landtype rows to out_atmos_df_list[[i]]
    out_atmos_df_list[[i]] = rbind(out_atmos_df_list[[i]], all_landtype_atmos)
    
    # (6b) do each region (excludes Ocean)
    region_names <- unique(out_atmos_df_list[[i]][,"Region"])
    # remove All_region 
    region_names <- region_names[!region_names %in% "All_region"]
    # remove "All_region" from region_names
    region_names <- region_names[!region_names %in% c("All_region","Ocean")]
    # create df to store all the region averages in the following loop for current df in out_atmos_df_list
    all_region_atmos <- out_atmos_df_list[[i]][1:length(region_names),]
    for (r in 1:length(region_names)) { 
      # get current region name
      region_name <- region_names[r]
      # name the region cell accordingly
      all_region_atmos[r,c(1:4)] <- c(-1, region_names[r], "All_land", "All_own")
      # subset region-specific atmos df
      region_df_atmos_temp <- out_atmos_df_list[[i]][out_atmos_df_list[[i]][,"Region"] == region_name,] 
      # delete the All_land row
      region_df_atmos_temp <- region_df_atmos_temp[region_df_atmos_temp$Land_Type != "All_land",]
      # aggregate sum atmos of region-specific areas for each column year
      all_region_atmos[r,c(5:ncol(all_region_atmos))] <- apply(region_df_atmos_temp[,c(5:ncol(region_df_atmos_temp))], 2, sum)
    }
    # add the aggregated landtype rows to out_atmos_df_list[[i]]
    out_atmos_df_list[[i]] = rbind(out_atmos_df_list[[i]], all_region_atmos)
    
    # (6c) do all California (excludes Ocean)
    sum_row = out_atmos_df_list[[i]][1,]
    sum_row[,c(1:4)] = c(-1, "All_region", "All_land", "All_own")
    sum_row[,c(5:ncol(sum_row))] <- apply(out_atmos_df_list[[i]][out_atmos_df_list[[i]][, "Land_Type"] == "All_land", 
                                  c(5:ncol(out_atmos_df_list[[i]]))], 2, sum)
    out_atmos_df_list[[i]] = rbind(out_atmos_df_list[[i]], sum_row)
    # subtract final-intial again now that all the sections are done
    out_atmos_df_list[[i]][, "Change_Mg"] = out_atmos_df_list[[i]][,end_label] - out_atmos_df_list[[i]][,start_atmos_label]
    # round
    out_atmos_df_list[[i]][,c(5:ncol(out_atmos_df_list[[i]]))] = round(out_atmos_df_list[[i]][,c(5:ncol(out_atmos_df_list[[i]]))], 2)
  }
  
  # write to excel file
  if(WRITE_OUT_FILE) {
    
    cat("Starting writing output at", date(), "\n")
    
    # put the output tables in a workbook
    out_wrkbk =  loadWorkbook(out_file, create = TRUE)
    
    # area
    createSheet(out_wrkbk, name = out_area_sheets)
    clearSheet(out_wrkbk, sheet = out_area_sheets)
    writeWorksheet(out_wrkbk, data = out_area_df_list, sheet = out_area_sheets, header = TRUE)
    
    # c density
    createSheet(out_wrkbk, name = out_density_sheets)
    clearSheet(out_wrkbk, sheet = out_density_sheets)
    writeWorksheet(out_wrkbk, data = out_density_df_list, sheet = out_density_sheets, header = TRUE)
    
    # c stock
    createSheet(out_wrkbk, name = out_stock_sheets)
    clearSheet(out_wrkbk, sheet = out_stock_sheets)
    writeWorksheet(out_wrkbk, data = out_stock_df_list, sheet = out_stock_sheets, header = TRUE)
    
    # wood
    createSheet(out_wrkbk, name = out_wood_sheets)
    clearSheet(out_wrkbk, sheet = out_wood_sheets)
    writeWorksheet(out_wrkbk, data = out_wood_df_list, sheet = out_wood_sheets, header = TRUE)
    
    # atmosphere
    createSheet(out_wrkbk, name = names(out_atmos_df_list))
    clearSheet(out_wrkbk, sheet = names(out_atmos_df_list))
    writeWorksheet(out_wrkbk, data = out_atmos_df_list, sheet = names(out_atmos_df_list), header = TRUE)
    
    # write the workbook
    saveWorkbook(out_wrkbk)
    
    cat("Finished writing output at", date(), "\n")
  }
  
  cat("Finished CALAND at", date(), "\n")
  
} # end function CALAND()
