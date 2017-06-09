# CALAND.r

# This is the carbon accounting model for CA

# CAlifornia natural and working LANDs carbon and greenhouse gas model

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
#	scen_file		    name of the scenario file; assumed to be in caland/inptus/
#	c_file			    name of the carbon parameter input file; assumed to be in caland/inputs/
#	start_year		  simulation begins at the beginning of this year
#	end_year		    simulation ends at the beginning of this year (so the simulation goes through the end of end_year - 1)
#	value_col_dens	select which carbon density values to use; 5 = min, 6 = max, 7 = mean, 8 = std dev, 9 = std error
# value_col_accum select which carbon accumulation values to use; 5 = min, 6 = max, 7 = mean, 8 = std dev, 9 = std error
#	ADD_dens				for use with value_col_dens ==8: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean
#	ADD_accum				for use with value_col_accum ==8: TRUE= add the std dev to the mean; FALSE= subtract the std dev from the mean

#	WRITE_OUT_FILE	TRUE= write the output file; FALSE= do not write the output file

# notes:
# carbon calcs occur in start_year up to end_year-1
# end_year denotes the final area after the changes in end_year-1
# density values are the stats of the total pixel population within each land type id
# accumulation values are stats of literature values

# How to use CALAND for beginners in R:
# (1) Run all 3 functions (CALC.GWP, GET.NAMES, CALAND) by highlighting everything below and clicking "Run".
# (2) In command line, enter CALAND([define arguments here]). At a minimum you will need to define
# the scen_file (e.g. CALAND(scen_file = "Baseline_frst2Xmort_fire.xls")).

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


CALAND <- function(scen_file, c_file = "carbon_input.xlsx", start_year = 2010, end_year = 2051, value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, WRITE_OUT_FILE = TRUE) {
  cat("Start CALAND at", date(), "\n")
  # this enables java to use up to 4GB of memory for reading and writing excel files
  options(java.parameters = "-Xmx4g" )
  
  # to do: separate the selection of c density and accumulation non-mean values values
  
  # output label for: value_col and ADD select which carbon density and accumulation values to use; see notes above
  ftag = c("", "", "", "", "min", "max", "mean", "sd", "mean_se", "sd_se")
  
  inputdir = "inputs/"
  outputdir = "outputs/"
  dir.create(outputdir, recursive=TRUE)
  
  # get scenario name as file name without extension
  scen_name = substr(scen_file, 1, nchar(scen_file) - 5)
  # add the directory to the scen_file name and c_file name
  scen_file = paste0(inputdir, scen_file)
  c_file = paste0(inputdir, c_file)
  
  # the start row of the tables is the same for all sheets
  start_row = 12
  
  # Several assumptions are contained between this line down to the output table lines
  # They shouldn't need to be changed for differenct scenarios, but they would be useful for testing sensitivity of the model
  # below the output tables and before the library load lines are names specific to columns in the input xlsx file
  
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
  
  ######### Determine output file names based on arguments in CALAND() that specify which input statistics are used for c density ######### 
  ##################################################### and c accumulation ################################################################
  # if c density and c accumulation use _same_ input statisitic: they're both either min (5), max (6), mean (7), std_dev (8), or se (9)
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
              }
          }
      }
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
            }
          }
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
  clrcut_mainremoved_burn <- 0.25
  # forest partial_cut and above-main removed  to atmos
  parcut_mainremoved_burn <- 0.25
  # forest fuel_reduction and above-main removed  to atmos
  fuelred_mainremoved_burn <- 0.25
  # forest Prescribed_burn and above-main removed  to atmos (currently above2atmos is 0; only understory, litter and down dead go to atmos)
  prescrburn_mainremoved_burn <- 1
  # forest Weed_treatment and above-main removed to atmos
  weedtrt_mainremoved_burn <- 0.25
  # forest clearcut and understory to atmos
  clrcut_under_burn <- 0.25
  # forest partial_cut and understory to atmos
  parcut_under_burn <- 0.25
  # forest fuel_reduction and understory to atmos
  fuelred_under_burn <- 0.25
  # forest Prescribed_burn and understory to atmos
  prescrburn_under_burn <- 1
  # forest weed_treatment and understory removed to atmos
  weedtrt_under_burn <- 0.25
  # forest clearcut and down dead to atmos
  clrcut_down_burn <- 0.25
  # forest partial_cut and down dead to atmos
  parcut_down_burn <- 0.25
  # forest fuel_reduction and down dead to atmos
  fuelred_down_burn <- 0.25
  # forest Prescribed_burn and down dead to atmos
  prescrburn_down_burn <- 1
  # forest Weed_treatment and down dead removed to atmos
  weedtrt_down_burn <- 0.25
  # forest clearcut and litter to atmos
  clrcut_litter_burn <- 0.25
  # forest partial_cut and litter to atmos
  parcut_litter_burn <- 0.25
  # forest fuel_reduction and litter to atmos
  fuelred_litter_burn <- 0.25
  # forest Prescribed_burn and litter to atmos
  prescrburn_litter_burn <- 1
  # forest weed_treatment and litter removed to atmos
  weedtrt_litter_burn <- 0.25 
  
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
                       "Manage_Atmos_CumGain_FireC", "Manage_Atmos_CumGain_EnergyC", "Manage_Atmos_CumGain_NonBurnedC", 
                       "Fire_Atmos_CumGain_BurnedC", "Fire_Atmos_CumGain_NonBurnedC", "LCC_Atmos_CumGain_EnergyC", 
                       "LCC_Atmos_CumGain_NonBurnEnerC","Manage_Atmos_AnnGain_FireC", "Manage_Atmos_AnnGain_EnergyC", 
                       "Manage_Atmos_AnnGain_NonBurnedC", "Fire_Atmos_AnnGain_BurnedC", "Fire_Atmos_AnnGain_NonBurnedC", 
                       "LCC_Atmos_AnnGain_EnergyC", "LCC_Atmos_AnnGain_NonBurnEnerC")
  num_out_atmos_sheets = length(out_atmos_sheets)
  out_wood_sheets = c("Total_Wood_C_stock", "Total_Wood_CumGain_C_stock", "Total_Wood_CumLoss_C_stock", "Total_Wood_AnnGain_C_stock", 
                      "Total_Wood_AnnLoss_C_stock", "Manage_Wood_C_stock", "Manage_Wood_CumGain_C_stock", "Manage_Wood_CumLoss_C_stock", 
                      "Manage_Wood_AnnGain_C_stock", "Manage_Wood_AnnLoss_C_stock", "LCC_Wood_C_stock", "LCC_Wood_CumGain_C_stock", 
                      "LCC_Wood_CumLoss_C_stock", "LCC_Wood_AnnGain_C_stock", "LCC_Wood_AnnLoss_C_stock")
  num_out_wood_sheets = length(out_wood_sheets)
  
  # column names from the management table to calculate non-accum manage carbon adjustments
  man_frac_names = c("Above_removed_frac", "StandDead_removed_frac", "Removed2Wood_frac", "Removed2Energy_frac", "Removed2Atmos_frac", 
                     "Understory2Atmos_frac", "DownDead2Atmos_frac", "Litter2Atmos_frac", "Soil2Atmos_frac", "Understory2DownDead_frac", 
                     "Above2StandDead_frac", "Below2Atmos_frac", "Below2Soil_frac")
  num_manfrac_cols = length(man_frac_names)
  # new c trans column names matching the non-accum manage frac names
  c_trans_names = c("Above_removed_c", "StandDead_removed_c", "Removed2Wood_c", "Removed2Energy_c", "Removed2Atmos_c", "Understory2Atmos_c", 
                    "DownDead2Atmos_c", "Litter2Atmos_c", "Soil2Atmos_c", "Understory2DownDead_c", "Above2StandDead_c", "Below2Atmos_c", 
                    "Below2Soil_c")
  # indices of the appropriate density source df for the non-accum manage frac to c calcs; corresponds with out_density_sheets above
  # value == -1 indicates that the source is the removed c; take the sum of the first two c trans columns
  manage_density_inds = c(3, 6, -1, -1, -1, 5, 7, 8, 9, 5, 3, 4, 4)
  
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
  conv_frac_names = c("Above_removed_conv_frac", "StandDead_removed_conv_frac", "Removed2Wood_conv_frac", "Removed2Energy_conv_frac", 
                      "Removed2Atmos_conv_frac", "Understory2Atmos_conv_frac", "DownDead2Atmos_conv_frac", "Litter2Atmos_conv_frac", 
                      "Soil2Atmos_conv_frac", "Understory2DownDead_conv_frac", "Below2Atmos_conv_frac", "Below2Soil_conv_frac")
  num_convfrac_cols = length(conv_frac_names)
  # new c trans column names matching the conversion frac names
  convc_trans_names = c("Above_removed_conv_c", "StandDead_removed_conv_c", "Removed2Wood_conv_c", "Removed2Energy_conv_c", 
                        "Removed2Atmos_conv_c", "Understory2Atmos_conv_c", "DownDead2Atmos_conv_c", "Litter2Atmos_conv_c", 
                        "Soil2Atmos_conv_c", "Understory2DownDead_conv_c", "Below2Atmos_conv_c", "Below2Soil_conv_c")
  # indices of the appropriate density source df for the conversion frac to c calcs; corresponds with out_density_sheets above
  # value == -1 indicates that the source is the removed c; take the sum of the first two c trans columns
  conv_density_inds = c(3, 6, -1, -1, -1, 5, 7, 8, 9, 5, 4, 4)
  
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
  c_col_types1 = c("numeric", "character", "character", "character", rep("numeric",50))
  c_col_types2 = c("numeric", "character", "character", "character", "character", rep("numeric",50))
  c_col_types3 = c("character", rep("numeric",50))
  
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
  
  for (i in 5:5) { # annual mortality fraction
    scen_df_list[[i]] <- readWorksheet(scen_wrkbk, i, startRow = start_row, colTypes = c_col_types1, forceConversion = TRUE)
  }
  names(c_df_list) <- c_sheets
  names(scen_df_list) <- scen_sheets
  
  # remove the Xs added to the front of the year columns, and get the years as numbers
  man_targetyear_labels = names(scen_df_list[[3]])[c(6:ncol(scen_df_list[[3]]))]
  man_targetyear_labels = substr(man_targetyear_labels,2,nchar(man_targetyear_labels[1]))
  names(scen_df_list[[3]])[c(6:ncol(scen_df_list[[3]]))] = man_targetyear_labels
  man_targetyears = as.integer(substr(man_targetyear_labels,1,4))
  
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
  fire_df = c_df_list[[17]]
  
  # merge deadc_frac_df and mortality_target_df because zero rows do not exist and allrows are needed 
  mortality_target_df = merge(deadc_frac_df, mortality_target_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
  mortality_target_df[,c(5:ncol(mortality_target_df))] <- apply(mortality_target_df[,c(5:ncol(mortality_target_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
  
  # get the correct values for the accum tables if value is std dev
  if(value_col_accum == 8) { # std dev as value
    if(ADD_accum) {
      vegc_uptake_df$vegc_uptake_val = vegc_uptake_df$vegc_uptake_val + vegc_uptake_df$Mean_Mg_ha_yr
      soilc_accum_df$soilc_accum_val = soilc_accum_df$soilc_accum_val + soilc_accum_df$Mean_Mg_ha_yr
    } else {
      vegc_uptake_df$vegc_uptake_val = vegc_uptake_df$Mean_Mg_ha_yr - vegc_uptake_df$vegc_uptake_val
      soilc_accum_df$soilc_accum_val = soilc_accum_df$Mean_Mg_ha_yr - soilc_accum_df$soilc_accum_val
    }
  }
  
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
  # Assign the target management areas table to "Managed_area" in out_area_df_list[[3]]
  out_area_df_list[[2]] <- scen_df_list[[3]][,c(1:6)]
  names(out_area_df_list[[2]])[ncol(out_area_df_list[[2]])] <- as.character(start_area_label)
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
  names(man_area_sum)[ncol(man_area_sum)] <- "man_area"
  man_area_sum$man_area_sum = 0.0
  
  # set sum neginds eco to 0
  out_cum_neginds_eco_tot <- 0
  
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
    
    # reset the man_area_sum df
    man_area_sum = man_area_sum[,1:7]
    
    # determine the managed areas for this year from target years
    # linear interpolation between target years
    # if the year is past the final target year than use the final target year
    
    # man_targetyears (man_targetyears: 2010, 2020, 2021, 2050), 
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
    
    # check if any man_area <0 that are != Growth  ### use this later after checking all scenatio files ###
    # man_area_sum$man_area[man_area_sum$man_area < 0 & man_area_sum$Management != Growth] <- 0
    man_area_sum$man_area[man_area_sum$man_area < 0 & (man_area_sum$Management == "Dead_removal" | man_area_sum$Management == "Urban_forest")] <- 0
    
    ######## New:  check for excess man_area before man_area_sum ######## 
    # Check that the sum of man_area is not > tot_area
    man_area_agg_pre = aggregate(man_area ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & 
                                                                           man_area_sum$Management != "Restoration",], FUN=sum)
    # update aggregated column name (man_area) to man_area_agg_extra_pre
    names(man_area_agg_pre)[ncol(man_area_agg_pre)] <- "man_area_agg_pre"
    # merge df's man_area_sum & tot_area_df, and assign to man_area_sum dataframe (essentially, add additional 
    # tot_area column to man_area_sum)
    man_area_sum = merge(man_area_sum, tot_area_df, by = c("Land_Cat_ID", "Region", "Land_Type","Ownership"), all.x = TRUE)
    # merge man_area_sum & man_area_agg_pre dataframes by Land_Type_ID
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
    
     ############# end pre man_area check #############
    
    # the developed practices are independent of each other and so they don't use the aggregate sums
    #  they use their individual practice sums
    # ag management does not use sum area because they are annual practices to maintain the benefits
    # Afforestation and restoration are not dependent on existing area and are not included in aggregate managed area
    # Calc cumulative management areas: man_area_sum$man_area_sum = 0 + man_area for current year 
    man_area_sum$man_area_sum = man_area_sum$man_area_sum + man_area_sum$man_area
    # merge df's man_area_sum & tot_area_df, and assign to man_area_sum dataframe (essentially, add additional 
    # tot_area column to man_area_sum), excludes any land types from tot_area that are not in man_area_sum
    # man_area_sum = merge(man_area_sum, tot_area_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    # sort man_area_sum dataframe by Land_Type_ID, then by Manage_ID 
    man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    # (1) aggregate cumulative areas (man_area_sum) for all management except afforestation & restoration...
    # create df of aggregated cumulative areas (man_area_sum_agg): aggregate by summing man_area_sum with the same Land_Type_ID _except_ 
    # for areas with Afforestation and Restoration management
    man_area_sum_agg = aggregate(man_area_sum ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & 
                                                                             man_area_sum$Management != "Restoration",], FUN=sum)
    # update aggregated sums column name (man_area_sum) to man_area_sum_agg_extra
    names(man_area_sum_agg)[ncol(man_area_sum_agg)] <- "man_area_sum_agg_extra"
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
    # create a _trimmed_ aggregated man_area_sum df (man_area_sum_agg2): aggregate sum man_area_sum vector by Land_Cat_ID 
    # for all management activities _except_ afforestation and restoration areas
    man_area_sum_agg2 = aggregate(man_area_sum ~ Land_Cat_ID, man_area_sum[man_area_sum$Management != "Afforestation" & 
                                                                              man_area_sum$Management != "Restoration",], FUN=sum)
    # rename _trimmed_ aggregate cumulative areas to "man_area_sum_agg" in man_area_sum_agg2 df
    names(man_area_sum_agg2)[ncol(man_area_sum_agg2)] <- "man_area_sum_agg"
    # (4) add column "man_area_sum_agg" (Developed_all = _untrimmed_ agg cummulative areas, & afforestation and restoration excluded)
    # by merging man_area_sum & man_area_sum_agg2 dataframes by Land_Type_ID
    man_area_sum = merge(man_area_sum, man_area_sum_agg2, by = "Land_Cat_ID", all.x =TRUE)
    # replace NA's in the man_area_sum_agg column with 0's
    man_area_sum$man_area_sum_agg = replace(man_area_sum$man_area_sum_agg, is.na(man_area_sum$man_area_sum_agg), 0)
    # sort man_area_sum by land type, then management ID 
    man_area_sum = man_area_sum[order(man_area_sum$Land_Cat_ID, man_area_sum$Management),]
    # (5) don't use man_area_sum_agg for Developed_all: replace (trimmed & aggregated) man_area_sum_agg column with individual (trimmed) man_area_sum 
    man_area_sum$man_area_sum_agg[man_area_sum$Land_Type == "Developed_all"] = man_area_sum$man_area_sum[man_area_sum$Land_Type == "Developed_all"]
    
    # build some useful data frames
    all_c_flux = tot_area_df
    all_c_flux = merge(all_c_flux, man_area_agg2, by = "Land_Cat_ID", all.x = TRUE)
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
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
    
    man_adjust_df = rbind(man_grass_df, man_ag_df)
      man_adjust_df = rbind(man_adjust_df, man_forest_df[,c(1:5,forest_soilcaccumfrac_colind)])
    man_adjust_df = rbind(man_adjust_df, man_dev_df[,c(1:5,dev_soilcaccumfrac_colind)])
    man_adjust_df = merge(man_adjust_df, rbind(man_forest_df, man_dev_df), by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", 
                                                                                  "Management", "SoilCaccum_frac"), all.x = TRUE)
    man_adjust_df = merge(man_area_sum, man_adjust_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"), 
                          all.x = TRUE)
    man_adjust_df = man_adjust_df[order(man_adjust_df$Land_Cat_ID, man_adjust_df$Management),]
    # replace the NA values with more appropriate ones
    man_adjust_df[,c("SoilCaccum_frac", "VegCuptake_frac", "DeadCaccum_frac")] <- 
      apply(man_adjust_df[,c("SoilCaccum_frac", "VegCuptake_frac", "DeadCaccum_frac")], 2, function (x) {replace(x, is.na(x), 1.00)})
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
    # the proportional increase in urban forest area is represented as a proportional increase in veg c uptake
    if (year == start_year) {
      start_urban_forest_fraction = man_adjust_df[man_adjust_df$Management == "Urban_forest", "man_area"] / 
        man_adjust_df[man_adjust_df$Management == "Urban_forest", "tot_area"]
    }
    man_adjust_df[man_adjust_df$Management == "Urban_forest", "VegCuptake_frac"] = 
      man_adjust_df[man_adjust_df$Management == "Urban_forest", "man_area"] / 
      man_adjust_df[man_adjust_df$Management == "Urban_forest", "tot_area"] / start_urban_forest_fraction
    
    # soil
    # Cultivated uses the current year managed area
    man_soil_df = merge(man_adjust_df, soilc_accum_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    man_soil_df = man_soil_df[order(man_soil_df$Land_Cat_ID, man_soil_df$Management),]
    man_soil_df$soilcfluxXarea[man_soil_df$Land_Type != "Cultivated"] = man_soil_df$man_area_sum[man_soil_df$Land_Type != "Cultivated"] * 
      man_soil_df$SoilCaccum_frac[man_soil_df$Land_Type != "Cultivated"] * 
      man_soil_df$soilc_accum_val[man_soil_df$Land_Type != "Cultivated"]
    man_soil_df$soilcfluxXarea[man_soil_df$Land_Type == "Cultivated"] = man_soil_df$man_area[man_soil_df$Land_Type == "Cultivated"] * 
      man_soil_df$SoilCaccum_frac[man_soil_df$Land_Type == "Cultivated"] * 
      man_soil_df$soilc_accum_val[man_soil_df$Land_Type == "Cultivated"]
    man_soilflux_agg = aggregate(soilcfluxXarea ~ Land_Cat_ID + Region + Land_Type + Ownership, man_soil_df, FUN=sum)
    man_soilflux_agg = merge(all_c_flux, man_soilflux_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    na_inds = which(is.na(man_soilflux_agg$soilcfluxXarea))
    man_soilflux_agg$soilcfluxXarea[na_inds] = 0
    man_soilflux_agg = merge(man_soilflux_agg, man_soil_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "soilc_accum_val")], 
                             by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    man_soilflux_agg = man_soilflux_agg[order(man_soilflux_agg$Land_Cat_ID),]
    man_soilflux_agg = unique(man_soilflux_agg)
    na_inds = which(is.na(man_soilflux_agg$soilc_accum_val))
    man_soilflux_agg[na_inds, "soilc_accum_val"] = 0
    man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type != "Cultivated"] = 
      (man_soilflux_agg$soilcfluxXarea[man_soilflux_agg$Land_Type != "Cultivated"] + 
         man_soilflux_agg$unman_area_sum[man_soilflux_agg$Land_Type != "Cultivated"] * 
         man_soilflux_agg$soilc_accum_val[man_soilflux_agg$Land_Type != "Cultivated"]) / 
      tot_area_df$tot_area[tot_area_df$Land_Type != "Cultivated"]
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
    # all developed area veg c uptake is adjusted because urban forest increased
    #  so remove the other developed managements from this table and multiply by total area and use unman area = 0
    # merge man_adjust_df and vegc_uptake_df and assign to man_veg_df (ROWS = 85)
    man_veg_df = merge(man_adjust_df, vegc_uptake_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    man_veg_df = man_veg_df[order(man_veg_df$Land_Cat_ID, man_veg_df$Management),] 
    # omit all records for Dead_removal and limited_Growth managements or with management activity = NA 
    man_veg_df = man_veg_df[(man_veg_df$Management != "Dead_removal" & man_veg_df$Management != "Growth") | is.na(man_veg_df$Management),]
    
    ################ First, calc MANAGED AREA VEG C UPTAKE [MgC/y]  (vegcfluxXarea) #############################
    
    # calc managed area's total veg C uptake for all landtypes using cumulative areas: 
    # vegcfluxXarea = cumulative_management_area x VegCuptake_frac x vegc_uptake_val (LENGTH = 79)
    man_veg_df$vegcfluxXarea = man_veg_df$man_area_sum * man_veg_df$VegCuptake_frac * man_veg_df$vegc_uptake_val
    # special calc for managed area's total veg C uptake in developed landtype using total area (managed + unmanaged)   
    man_veg_df$vegcfluxXarea[man_veg_df$Land_Type == "Developed_all"] = 
      man_veg_df$tot_area[man_veg_df$Land_Type == "Developed_all"] * 
      man_veg_df$VegCuptake_frac[man_veg_df$Land_Type == "Developed_all"] * 
      man_veg_df$vegc_uptake_val[man_veg_df$Land_Type == "Developed_all"]
    # aggregate sum veg C uptake across Land_Cat_ID + Region + Land_Type + Ownership 
    man_vegflux_agg = aggregate(vegcfluxXarea ~ Land_Cat_ID + Region + Land_Type + Ownership, man_veg_df, FUN=sum)
    # merge aggregate sums with all_c_flux (management areas and total areas) 
    man_vegflux_agg = merge(all_c_flux, man_vegflux_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    
    # clean up vegcfluxXarea values
    na_inds = which(is.na(man_vegflux_agg$vegcfluxXarea))
    man_vegflux_agg$vegcfluxXarea[na_inds] = 0
    
    # merge "vegc_uptake_val" (baseline veg c flux) column to man_vegflux_agg dataframe 
    man_vegflux_agg = merge(man_vegflux_agg, man_veg_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "vegc_uptake_val")], 
                            by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    man_vegflux_agg = man_vegflux_agg[order(man_vegflux_agg$Land_Cat_ID),]
    man_vegflux_agg = unique(man_vegflux_agg) 
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
    man_vegflux_agg$man_change_vegc_uptake = man_vegflux_agg$fin_vegc_uptake - man_vegflux_agg$vegc_uptake_val
    
    # dead
    
    # determine the fractional mortality c rate of above ground main for this year from target years (from the current year mortality fraction)
    # this is then applied to the above ground main c pools and the below ground main c pools
    # the understory mortality is set to a default value at the beginning of this script
    # linear interpolation between target years
    # if the year is past the final target year than use the final target year
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
    
    man_dead_df = merge(man_adjust_df, deadc_frac_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    man_dead_df = man_dead_df[order(man_dead_df$Land_Cat_ID, man_dead_df$Management),]
    man_dead_df$deadcfracXarea = man_dead_df$man_area_sum * man_dead_df$DeadCaccum_frac * man_dead_df$deadc_frac_in
    man_deadfrac_agg = aggregate(deadcfracXarea ~ Land_Cat_ID + Region + Land_Type + Ownership, man_dead_df, FUN=sum)
    man_deadfrac_agg = merge(all_c_flux, man_deadfrac_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all = TRUE)
    na_inds = which(is.na(man_deadfrac_agg$deadcfracXarea))
    man_deadfrac_agg$deadcfracXarea[na_inds] = 0
    man_deadfrac_agg = merge(man_deadfrac_agg, man_dead_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "deadc_frac_in")], 
                             by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"))
    man_deadfrac_agg = man_deadfrac_agg[order(man_deadfrac_agg$Land_Cat_ID),]
    man_deadfrac_agg = unique(man_deadfrac_agg)
    na_inds = which(is.na(man_deadfrac_agg$deadc_frac_in))
    man_deadfrac_agg[na_inds, "deadc_frac_in"] = 0
    man_deadfrac_agg$fin_deadc_frac = (man_deadfrac_agg$deadcfracXarea + man_deadfrac_agg$unman_area_sum * 
                                         man_deadfrac_agg$deadc_frac_in) / tot_area_df$tot_area
    nan_inds = which(is.nan(man_deadfrac_agg$fin_deadc_frac) | man_deadfrac_agg$fin_deadc_frac == Inf)
    man_deadfrac_agg$fin_deadc_frac[nan_inds] = man_deadfrac_agg[nan_inds, "deadc_frac_in"]
    man_deadfrac_agg$man_change_deadc_accum = man_deadfrac_agg$fin_deadc_frac - man_deadfrac_agg$deadc_frac_in
    
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
    #  estimate above ground mortality like the rest, based on above ground flux, but send it to atmosphere
    # treat na values as zeros
    # mortality fractions are zero in the input table if no veg c accum is listed in the carbon inputs
    # these flux transfers are normalized to current tot_area, and gains are positive
    
    # forest above main accum needs net foliage and branches/bark accums added to it based on estimated component fractions
    # forest downed dead and litter accum are estimated from the added above c based on mort:vegc flux ratio - this goes from above to downed 
    # dead and litter - and this value is also a net value
    # forest dead standing is subtracted from above main
    # forest below main accum and understory accum need to calculated based on ratio of these existing densities to the above densities
    # forest understory mortality uses a 1% default value (so it is not directly affected by prescribed tree mortality) - this is added to 
    # downed dead and litter - as the additional veg c uptake is a net values, this accumulation is also a net value
    # forest below mortality is estimated based upon standing dead accum to vegc uptake ratio - this is only subtracted from below as soil c is 
    # a net value
    
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
    
    # forest
    
    # above main
    above_vals = out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type == "Forest", cur_density_label]
    vegc_flux_vals = man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type == "Forest"]
    added_vegc_flux_vals = vegc_flux_vals * (leaffrac + barkfrac + branchfrac) / stemfrac 
    deadc_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] * above_vals * stemfrac
    above2dldead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] * above_vals * (1.0 - stemfrac)
    #deadc2vegc_ratios = deadc_flux_vals / vegc_flux_vals
    #above2dldead_flux_vals = deadc2vegc_ratios * added_vegc_flux_vals
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[1]] = vegc_flux_vals + added_vegc_flux_vals - deadc_flux_vals - above2dldead_flux_vals
    
    # standing dead
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[4]] = deadc_flux_vals
    
    # understory
    under_vals = out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type == "Forest", cur_density_label]
    underfrac = under_vals / above_vals
    underc_flux_vals = underfrac * vegc_flux_vals / stemfrac
    under2dldead_flux_vals = default_mort_frac * out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type == "Forest", cur_density_label]
    #under2dldead_flux_vals = deadc2vegc_ratios * underc_flux_vals
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[3]] = underc_flux_vals - under2dldead_flux_vals
    
    # downed dead and litter
    downfrac = out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type == "Forest", cur_density_label] / 
      (out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type == "Forest", cur_density_label] + 
         out_density_df_list[[8]][out_density_df_list[[8]]$Land_Type == "Forest", cur_density_label])
    # downded dead
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[5]] = downfrac * (above2dldead_flux_vals + under2dldead_flux_vals)
    # litter
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[6]] = (1.0 - downfrac) * (above2dldead_flux_vals + under2dldead_flux_vals)
    
    # below ground
    # recall that the input historical soil c fluxes are net, so the default historical mortality here implicitly goes to the soil
    #  but any change from the default mortality needs to be added to the soil
    #  so store the initial below ground mortality flux
    # assume that the other soil fluxes do not change (litter input rates and emissions) because I don't have enough info to change these
    #  basically, the litter input would change based on its density, and the emissions may increase with additional soil c
    below2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Forest"] * 
      out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type == "Forest", cur_density_label]
    if (year == start_year) { below2dead_flux_initial_forest = below2dead_flux_vals }
    # first calculate the net root biomass increase
    below_vals = out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type == "Forest", cur_density_label]
    rootfrac = below_vals / above_vals
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[2]] = rootfrac * vegc_flux_vals / stemfrac - below2dead_flux_vals
    
    # soil
    # need to add the difference due to chnages from default/initial mortality
    all_c_flux[all_c_flux$Land_Type == "Forest",egnames[7]] = man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type == "Forest"] + 
      (below2dead_flux_vals - below2dead_flux_initial_forest)
    
    # savanna/woodland
    
    # above and below main
    # root loss has to go to soil c because the veg gain is tree nee, and the soil flux is ground nee, together they are the net flux
    #  so here changing mortality is already accounted for with respect to additions to soil carbon
    # transfer above loss proportionally to standing, down, and litter pools
    # leave understory c static because the available data are for a grass understory, which has no long-term veg accumulation
    above_vals = out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type == "Savanna" | out_density_df_list[[3]]$Land_Type == "Woodland", 
                                          cur_density_label]
    vegc_flux_vals = man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type == "Savanna" | man_vegflux_agg$Land_Type == "Woodland"]
    below_vals = out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type == "Savanna" | out_density_df_list[[4]]$Land_Type == "Woodland", 
                                          cur_density_label]
    above_flux_vals = vegc_flux_vals * above_vals / (above_vals + below_vals)
    below_flux_vals = vegc_flux_vals * below_vals / (above_vals + below_vals)
    above2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Savanna" | man_deadfrac_agg$Land_Type == "Woodland"] * 
      above_vals
    #zinds = which(above2dead_flux_vals == 0 & above_flux_vals > 0)
    #above2dead_flux_vals[zinds] = default_mort_frac * above_vals[zinds]
    #deadc2vegc_ratios = above2dead_flux_vals / above_flux_vals
    below2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type == "Savanna" | man_deadfrac_agg$Land_Type == "Woodland"] * 
      below_vals
    #naninds = which(is.nan(below2dead_flux_vals) & below_flux_vals > 0)
    #below2dead_flux_vals[naninds] = default_mort_frac * below_vals[naninds]
    #naninds = which(is.nan(below2dead_flux_vals))
    #below2dead_flux_vals[naninds] = 0
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[1]] = above_flux_vals - above2dead_flux_vals
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[2]] = below_flux_vals - below2dead_flux_vals
    
    # standing, down, and litter
    standdead_vals = out_density_df_list[[6]][out_density_df_list[[6]]$Land_Type == "Savanna" | out_density_df_list[[6]]$Land_Type == "Woodland", 
                                              cur_density_label]
    downdead_vals = out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type == "Savanna" | out_density_df_list[[7]]$Land_Type == "Woodland", 
                                             cur_density_label]
    litter_vals = out_density_df_list[[8]][out_density_df_list[[8]]$Land_Type == "Savanna" | out_density_df_list[[8]]$Land_Type == "Woodland", 
                                           cur_density_label]
    standdead_frac_vals = standdead_vals / (standdead_vals + downdead_vals + litter_vals)
    downdead_frac_vals = downdead_vals / (standdead_vals + downdead_vals + litter_vals)
    litter_frac_vals = litter_vals / (standdead_vals + downdead_vals + litter_vals)
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[4]] = standdead_frac_vals * above2dead_flux_vals
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[5]] = downdead_frac_vals * above2dead_flux_vals
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[6]] = litter_frac_vals * above2dead_flux_vals
    
    # soil - recall that this is nee flux measurement, not density change, so the root mortality has to go to soil c
    soilc_flux_vals = man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type == "Savanna" | man_soilflux_agg$Land_Type == "Woodland"]
    all_c_flux[all_c_flux$Land_Type == "Savanna" | all_c_flux $Land_Type == "Woodland",egnames[7]] = soilc_flux_vals + below2dead_flux_vals
    
    # the rest
    # assume vegc flux is all standing net density change, sans mortality
    # assume above an understory deadc flux is all mort density change - take from above and distribute among stand, down, and litter
    # use mortality only if there is veg c accum due to growth
    # assume soilc flux is net density change - so the below is simply a net root density change, and the calculated mortality implicitly goes 
    # to soil
    above_vals = out_density_df_list[[3]][out_density_df_list[[3]]$Land_Type != "Savanna" & out_density_df_list[[3]]$Land_Type != "Woodland" & 
                                            out_density_df_list[[3]]$Land_Type != "Forest", cur_density_label]
    below_vals = out_density_df_list[[4]][out_density_df_list[[4]]$Land_Type != "Savanna" & out_density_df_list[[4]]$Land_Type != "Woodland" & 
                                            out_density_df_list[[4]]$Land_Type != "Forest", cur_density_label]
    under_vals = out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type != "Savanna" & out_density_df_list[[5]]$Land_Type != "Woodland" & 
                                            out_density_df_list[[5]]$Land_Type != "Forest", cur_density_label]
    standdead_vals = out_density_df_list[[6]][out_density_df_list[[6]]$Land_Type != "Savanna" & out_density_df_list[[6]]$Land_Type != "Woodland" & 
                                                out_density_df_list[[6]]$Land_Type != "Forest", cur_density_label]
    downdead_vals = out_density_df_list[[7]][out_density_df_list[[7]]$Land_Type != "Savanna" & out_density_df_list[[7]]$Land_Type != "Woodland" & 
                                               out_density_df_list[[7]]$Land_Type != "Forest", cur_density_label]
    litter_vals = out_density_df_list[[8]][out_density_df_list[[8]]$Land_Type != "Savanna" & out_density_df_list[[8]]$Land_Type != "Woodland" & 
                                             out_density_df_list[[8]]$Land_Type != "Forest", cur_density_label]
    soil_vals = out_density_df_list[[9]][out_density_df_list[[9]]$Land_Type != "Savanna" & out_density_df_list[[9]]$Land_Type != "Woodland" & 
                                           out_density_df_list[[9]]$Land_Type != "Forest", cur_density_label]
    # above and below
    above_flux_vals = man_vegflux_agg$fin_vegc_uptake[man_vegflux_agg$Land_Type != "Savanna" & man_vegflux_agg$Land_Type != "Woodland" & 
                                                        man_vegflux_agg$Land_Type != "Forest"]
    below_flux_vals = above_flux_vals * below_vals / above_vals
    naninds = which(is.nan(below_flux_vals))
    below_flux_vals[naninds] = above_flux_vals[naninds] * default_below2above_frac
    #deadc_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type != "Savanna" & man_deadfrac_agg$Land_Type != "Woodland" & 
    # man_deadfrac_agg$Land_Type != "Forest"]
    soilc_flux_vals = man_soilflux_agg$fin_soilc_accum[man_soilflux_agg$Land_Type != "Savanna" & man_soilflux_agg$Land_Type != "Woodland" & 
                                                         man_soilflux_agg$Land_Type != "Forest"]
    above2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type != "Savanna" & man_deadfrac_agg$Land_Type != "Woodland" & 
                                                             man_deadfrac_agg$Land_Type != "Forest"] * above_vals
    #zinds = which(above2dead_flux_vals == 0 & above_flux_vals > 0)
    #above2dead_flux_vals[zinds] = default_mort_frac * above_vals[zinds]
    #deadc2vegc_ratios = above2dead_flux_vals / above_flux_vals
    # recall that the input historical soil c fluxes are net, so the default historical mortality here implicitly goes to the soil
    #  but any change from the default mortality needs to be added to the soil
    #  so store the initial below ground mortality flux
    # assume that the other soil fluxes do not change (litter input rates and emissions) because I don't have enough info to change these
    #  basically, the litter input would change based on its density, and the emissions may increase with additional soil c
    below2dead_flux_vals = man_deadfrac_agg$fin_deadc_frac[man_deadfrac_agg$Land_Type != "Savanna" & man_deadfrac_agg$Land_Type != "Woodland" & 
                                                             man_deadfrac_agg$Land_Type != "Forest"] * below_vals
    if (year == start_year) { below2dead_flux_initial_rest = below2dead_flux_vals }
    #naninds = which(is.nan(below2dead_flux_vals) & below_flux_vals > 0)
    #below2dead_flux_vals[naninds] = default_mort_frac * below_vals[naninds]
    #naninds = which(is.nan(below2dead_flux_vals))
    #below2dead_flux_vals[naninds] = 0
    
    # above
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[1]] = 
      above_flux_vals - above2dead_flux_vals
    
    # below
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[2]] = 
      below_flux_vals - below2dead_flux_vals
    
    # understory
    underfrac = under_vals / above_vals
    underc_flux_vals = underfrac * above_flux_vals
    naninds = which(is.nan(underc_flux_vals))
    underc_flux_vals[naninds] = default_under_frac * above_flux_vals[naninds]
    under2dead_flux_vals = default_mort_frac * out_density_df_list[[5]][out_density_df_list[[5]]$Land_Type != "Savanna" & 
                                                                          out_density_df_list[[5]]$Land_Type != "Woodland" & 
                                                                          out_density_df_list[[5]]$Land_Type != "Forest", cur_density_label]
    #under2dead_flux_vals = deadc2vegc_ratios * underc_flux_vals
    #naninds = which(is.nan(under2dead_flux_vals) & underc_flux_vals > 0)
    #under2dead_flux_vals[naninds] = default_mort_frac * under_vals[naninds]
    #naninds = which(is.nan(under2dead_flux_vals))
    #under2dead_flux_vals[naninds] = 0
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[3]] = 
      underc_flux_vals - under2dead_flux_vals
    
    # stand, down, litter
    standdead_frac_vals = standdead_vals / (standdead_vals + downdead_vals + litter_vals)
    naninds = which(is.nan(standdead_frac_vals))
    standdead_frac_vals[naninds] = default_standdead_frac
    downdead_frac_vals = downdead_vals / (standdead_vals + downdead_vals + litter_vals)
    naninds = which(is.nan(downdead_frac_vals))
    downdead_frac_vals[naninds] = default_downdead_frac
    litter_frac_vals = litter_vals / (standdead_vals + downdead_vals + litter_vals)
    naninds = which(is.nan(litter_frac_vals))
    litter_frac_vals[naninds] = default_litter_frac
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[4]] = 
      standdead_frac_vals * (above2dead_flux_vals + under2dead_flux_vals)
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[5]] = 
      downdead_frac_vals * (above2dead_flux_vals + under2dead_flux_vals)
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[6]] = 
      litter_frac_vals * (above2dead_flux_vals + under2dead_flux_vals)
    
    # soil
    # add any c due to changes from default/initial mortality
    all_c_flux[all_c_flux$Land_Type != "Savanna" & all_c_flux$Land_Type != "Woodland" & all_c_flux$Land_Type != "Forest",egnames[7]] = 
      soilc_flux_vals + (below2dead_flux_vals - below2dead_flux_initial_rest)
    
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
      # print out the areas for land categries that have negative c (if greater than 0 then land category us running out of c, which may be due to a particular case of land conversions, i.e. land 
      # category x has negative c accumulation and is gaining area from land category y with lower c density, which can dilute the c density lower than the annual c loss sending the c density negative. 
      # Otherwise, if the area == 0 and there is negative c density, it can solely be due to the land cateogry running out of area and having a negative c accumulation rate.)
      cat("total areas for neginds out_density_df_list eco" , i, "are", out_area_df_list[[1]][neginds, cur_area_label], "\n")
      # check if any of the negative c density land categories have area > 0 (note: this only works if soil c density (i=9) is the only pool with negative values, as this is re-saved for each i loop)
      if (any((out_area_df_list[[1]][neginds, cur_area_label])>0)) {
        # if so, subset the rows from the out_area_df_list associated with all neginds and assign to area_neginds_df
        area_neginds_df <- out_area_df_list[[1]][neginds,]
        # add column to area_neginds_df that says which c pool ran out of c
        area_neginds_df$neg_c_density_pool <- rep(out_density_sheets[i],nrow(area_neginds_df)) 
        # add column that says which year it is
        area_neginds_df$Year <- rep(year,nrow(area_neginds_df)) 
        # print the land category ID's that have neginds _and_ area >0
        cat("Land_Cat_ID with neginds for", out_density_sheets[i], "& non-zero area are", unlist(area_neginds_df[out_area_df_list[[1]][neginds,cur_area_label]>0, c("Land_Cat_ID","Region","Ownership","Land_Type")]), "\n")
        # subset rows from from area_neginds_df with area >0, and assign to out_neginds_eco_df_pre
        out_neginds_eco_df_pre <- area_neginds_df[out_area_df_list[[1]][neginds,cur_area_label]>0, c("Land_Cat_ID","Region","Ownership","Land_Type","Year",cur_area_label,"neg_c_density_pool")]
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
    
    for (i in 1:num_manfrac_cols){
      # the removed values are calculated first, so this will work
      # if manage_density_inds[i] == -1, then the source is the removed pool; use the sum of the first two c trans columns
      if (manage_density_inds[i] == -1) {
        man_adjust_df[,c_trans_names[i]] = (man_adjust_df[,c_trans_names[1]] + man_adjust_df[,c_trans_names[2]]) * 
          man_adjust_df[,man_frac_names[i]]
      } else {
        if (!out_density_sheets[manage_density_inds[i]] %in% colnames(man_adjust_df)) {
          man_adjust_df = merge(man_adjust_df, 
                                out_density_df_list[[manage_density_inds[i]]][,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", next_density_label)], 
                                by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
          names(man_adjust_df)[names(man_adjust_df) == next_density_label] = out_density_sheets[manage_density_inds[i]]
        }
        man_adjust_df[,c_trans_names[i]] = man_adjust_df[,out_density_sheets[manage_density_inds[i]]] * man_adjust_df[,man_frac_names[i]] * 
          man_adjust_df$man_area / man_adjust_df$tot_area
      }
    } # end for i loop over the managed transfer fractions for calcuting the transfer carbon
    man_adjust_df = man_adjust_df[order(man_adjust_df$Land_Cat_ID),]
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    man_adjust_df[,c(6:ncol(man_adjust_df))] <- apply(man_adjust_df[,c(6:ncol(man_adjust_df))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    # now consolidate the c density transfers to the pools
    # convert these to gains for consistency: all terrestrial gains are positive, losses are negative
    # store the names for aggregation below
    agg_names = NULL
    # above
    # add column called "Above_main_C_den_gain_man":  above main C density = -(above removed C) -(above to standing dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[3], "_gain_man"))
    man_adjust_df[,agg_names[1]] = -man_adjust_df$Above_removed_c - man_adjust_df$Above2StandDead_c
    # below
    # add column called "Below_main_C_den_gain_man": root C density = -(root to soil C) -(root to atmos C)
    agg_names = c(agg_names, paste0(out_density_sheets[4], "_gain_man"))
    man_adjust_df[,agg_names[2]] = -man_adjust_df$Below2Soil_c - man_adjust_df$Below2Atmos_c
    # understory
    # add column called "Understory_C_den_gain_man": understory C density = -(understory to atmos C) -(understory to down dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[5], "_gain_man"))
    man_adjust_df[,agg_names[3]] = -man_adjust_df$Understory2Atmos_c - man_adjust_df$Understory2DownDead_c
    # standing dead
    # add column called "StandDead_C_den_gain_man": standing dead C density = -(removed standing dead C) + (above main to standing dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[6], "_gain_man"))
    man_adjust_df[,agg_names[4]] = -man_adjust_df$StandDead_removed_c + man_adjust_df$Above2StandDead_c
    # down dead
    # add column called "DownDead_C_den_gain_man": down dead C density = -(down dead to atmos C) + (understory to down dead C)
    agg_names = c(agg_names, paste0(out_density_sheets[7], "_gain_man"))
    man_adjust_df[,agg_names[5]] = -man_adjust_df$DownDead2Atmos_c + man_adjust_df$Understory2DownDead_c
    # litter
    # add column called "Litter_C_den_gain_man": litter C density = -(litter to atmos C)
    agg_names = c(agg_names, paste0(out_density_sheets[8], "_gain_man"))
    man_adjust_df[,agg_names[6]] = -man_adjust_df$Litter2Atmos_c
    # soil
    # add column called "Soil_orgC_C_den_gain_man": soil C density = -(soil to atmos C) + (root to soil C)
    agg_names = c(agg_names, paste0(out_density_sheets[9], "_gain_man"))
    man_adjust_df[,agg_names[7]] = -man_adjust_df$Soil2Atmos_c + man_adjust_df$Below2Soil_c
    
    # to get the carbon must multiply these by the tot_area
    # atmos
    # calc C loss to atmosphere [Mg C] ("Land2Atmos_c_stock_man") 
    #  "Land2Atmos_c_stock_man" = -(total area [ha]) * (soil emissons [MgC/ha] + litter emissons [Mg/ha] + down dead emissons [Mg/ha] + 
    #   understory emissons [Mg/ha] + removed above-ground emissons [Mg/ha] + root emissions [Mg/ha])
    agg_names = c(agg_names, paste0("Land2Atmos_c_stock_man"))
    man_adjust_df[,agg_names[8]] = -man_adjust_df$tot_area * (man_adjust_df$Soil2Atmos_c + man_adjust_df$Litter2Atmos_c + 
                                                                man_adjust_df$DownDead2Atmos_c + man_adjust_df$Understory2Atmos_c + 
                                                                man_adjust_df$Removed2Atmos_c + man_adjust_df$Below2Atmos_c)
    
    # energy - this is assume to go to the atmosphere immediately
    # calc C loss to atmosphere [Mg C] ("Land2Energy_c_stock_man") via energy generation (CO2 + CH4 + BC) (assume to go to the atmosphere 
    # immediately) = -(total area [ha]) * (above-ground C removed for energy [MgC/ha])
    agg_names = c(agg_names, paste0("Land2Energy_c_stock_man"))
    man_adjust_df[,agg_names[9]] = -man_adjust_df$tot_area * man_adjust_df$Removed2Energy_c
    
    # wood - this decays with a half-life
    agg_names = c(agg_names, paste0("Land2Wood_c_stock_man"))
    man_adjust_df[,agg_names[10]] = -man_adjust_df$tot_area * man_adjust_df$Removed2Wood_c
    
    # Before partioning the Land2Atmos_c_stock_man into total burned and total non-burned C emissions, partition the 
    # individual above-ground pools within it.
    # Soil c and root c are assumed to not burn, so 100% of 2Atmos c from these pools will be CO2.
    # First, calculate burned litter c using default fractions  of 2Atmos pools set in beginning
    man_adjust_df[,"Burned_litter_c"] = 0
    man_adjust_df[,"Burned_litter_c"][man_adjust_df$Management == "Clearcut"] <- clrcut_litter_burn * 
      man_adjust_df[,"Litter2Atmos_c"][man_adjust_df$Management == "Clearcut"]
    man_adjust_df[,"Burned_litter_c"][man_adjust_df$Management == "Partial_cut"] <- parcut_litter_burn * 
      man_adjust_df[,"Litter2Atmos_c"][man_adjust_df$Management == "Partial_cut"]
    man_adjust_df[,"Burned_litter_c"][man_adjust_df$Management == "Fuel_reduction"] <- fuelred_litter_burn * 
      man_adjust_df[,"Litter2Atmos_c"][man_adjust_df$Management == "Fuel_reduction"]
    man_adjust_df[,"Burned_litter_c"][man_adjust_df$Management == "Prescribed_burn"] <- prescrburn_litter_burn * 
      man_adjust_df[,"Litter2Atmos_c"][man_adjust_df$Management == "Prescribed_burn"]
    man_adjust_df[,"Burned_litter_c"][man_adjust_df$Management == "Weed_treatment"] <- weedtrt_litter_burn * 
      man_adjust_df[,"Litter2Atmos_c"][man_adjust_df$Management == "Weed_treatment"]
    # Second, calculate burned down dead c using default fractions of 2Atmos pools set in beginning
    man_adjust_df[,"Burned_downdead_c"] = 0
    man_adjust_df[,"Burned_downdead_c"][man_adjust_df$Management == "Clearcut"] <- clrcut_down_burn * 
      man_adjust_df[,"DownDead2Atmos_c"][man_adjust_df$Management == "Clearcut"]
    man_adjust_df[,"Burned_downdead_c"][man_adjust_df$Management == "Partial_cut"] <- parcut_down_burn * 
      man_adjust_df[,"DownDead2Atmos_c"][man_adjust_df$Management == "Partial_cut"]
    man_adjust_df[,"Burned_downdead_c"][man_adjust_df$Management == "Fuel_reduction"] <- fuelred_down_burn * 
      man_adjust_df[,"DownDead2Atmos_c"][man_adjust_df$Management == "Fuel_reduction"]
    man_adjust_df[,"Burned_downdead_c"][man_adjust_df$Management == "Prescribed_burn"] <- prescrburn_down_burn * 
      man_adjust_df[,"DownDead2Atmos_c"][man_adjust_df$Management == "Prescribed_burn"]
    man_adjust_df[,"Burned_downdead_c"][man_adjust_df$Management == "Weed_treatment"] <- weedtrt_down_burn * 
      man_adjust_df[,"DownDead2Atmos_c"][man_adjust_df$Management == "Weed_treatment"]
    # Third, calculate burned understory c using default fractions of 2Atmos pools set in beginning
    man_adjust_df[,"Burned_under_c"] = 0
    man_adjust_df[,"Burned_under_c"][man_adjust_df$Management == "Clearcut"] <- clrcut_under_burn * 
      man_adjust_df[,"Understory2Atmos_c"][man_adjust_df$Management == "Clearcut"]
    man_adjust_df[,"Burned_under_c"][man_adjust_df$Management == "Partial_cut"] <- parcut_under_burn * 
      man_adjust_df[,"Understory2Atmos_c"][man_adjust_df$Management == "Partial_cut"]
    man_adjust_df[,"Burned_under_c"][man_adjust_df$Management == "Fuel_reduction"] <- fuelred_under_burn * 
      man_adjust_df[,"Understory2Atmos_c"][man_adjust_df$Management == "Fuel_reduction"]
    man_adjust_df[,"Burned_under_c"][man_adjust_df$Management == "Prescribed_burn"] <- prescrburn_under_burn * 
      man_adjust_df[,"Understory2Atmos_c"][man_adjust_df$Management == "Prescribed_burn"]
    man_adjust_df[,"Burned_under_c"][man_adjust_df$Management == "Weed_treatment"] <- weedtrt_under_burn * 
      man_adjust_df[,"Understory2Atmos_c"][man_adjust_df$Management == "Weed_treatment"]
    # Fourth, calculate burned understory c using default fractions of 2Atmos pools set in beginning
    # doesn't include removed 2 energy
    man_adjust_df[,"Burned_mainremoved_c"] = 0
    man_adjust_df[,"Burned_mainremoved_c"][man_adjust_df$Management == "Clearcut"] <- clrcut_mainremoved_burn * 
      man_adjust_df[,"Removed2Atmos_c"][man_adjust_df$Management == "Clearcut"]
    man_adjust_df[,"Burned_mainremoved_c"][man_adjust_df$Management == "Partial_cut"] <- parcut_mainremoved_burn * 
      man_adjust_df[,"Removed2Atmos_c"][man_adjust_df$Management == "Partial_cut"]
    man_adjust_df[,"Burned_mainremoved_c"][man_adjust_df$Management == "Fuel_reduction"] <- fuelred_mainremoved_burn * 
      man_adjust_df[,"Removed2Atmos_c"][man_adjust_df$Management == "Fuel_reduction"]
    man_adjust_df[,"Burned_mainremoved_c"][man_adjust_df$Management == "Prescribed_burn"] <- prescrburn_mainremoved_burn * 
      man_adjust_df[,"Removed2Atmos_c"][man_adjust_df$Management == "Prescribed_burn"]
    man_adjust_df[,"Burned_mainremoved_c"][man_adjust_df$Management == "Weed_treatment"] <- weedtrt_mainremoved_burn * 
      man_adjust_df[,"Removed2Atmos_c"][man_adjust_df$Management == "Weed_treatment"]
    
    agg_names = c(agg_names, paste0("Land2Atmos_burnedC_stock_man"))
    # create man_adjust_df$Land2Atmos_burnedC_stock_man (note: does not include bioenergy)
    man_adjust_df[,agg_names[11]] = -man_adjust_df$tot_area * (man_adjust_df$Burned_litter_c + man_adjust_df$Burned_downdead_c +
                                                                 man_adjust_df$Burned_under_c + man_adjust_df$Burned_mainremoved_c)
    agg_names = c(agg_names, paste0("Land2Atmos_nonburnedC_stock_man"))
    # create man_adjust_df$Land2Atmos_nonburnedC_stock_man  = "Land2Atmos_c_stock_man" - "Land2Atmos_burnedC_stock_man"
    man_adjust_df[,agg_names[12]] = man_adjust_df[,agg_names[8]] - man_adjust_df[,agg_names[11]]
    
    # checks true that management Land2Atmos c flux equals the sum of burned and non-burned c in man_adjust_df 
    identical(man_adjust_df[,agg_names[8]], man_adjust_df[,agg_names[11]] + man_adjust_df[,agg_names[12]])
    
    # now aggregate to land type by summing the management options
    # these c density values are the direct changes to the overall c density
    # the c stock values are the total carbon form each land type going to atmos, energy (atmos), and wood
    
    # first, create table that has a row for each land type ID, and a column for each of the management-caused C density changes [MgC/ha], 
    # and corresponding net cumulative C transfers [Mg C] to atmosphere (via decomp, burning, or energy (also burning)) or to wood 
    agg_cols = array(dim=c(length(man_adjust_df$Land_Cat_ID),length(agg_names)))
    # second, populate the table by applying loop to each row's land type ID  
    for (i in 1:length(agg_names)) {
      # fill columns with corresponding management-caused C transfers from the man_adjust_df
      agg_cols[,i] = man_adjust_df[,agg_names[i]]
    }
    # third, aggregate the C transfers by summing within each land type and ownership combination and assign to man_adjust_agg df
    man_adjust_agg = aggregate(agg_cols ~ Land_Cat_ID + Region + Land_Type + Ownership, data=man_adjust_df, FUN=sum)
    # fourth, label the columns of the aggregated table 
    agg_names2 = paste0(agg_names,"_agg")
    names(man_adjust_agg)[c(5:ncol(man_adjust_agg))] = agg_names2
    # merge these values to the unman area table to apply the adjustments to each land type
    all_c_flux = merge(all_c_flux, man_adjust_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.na(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    # check that management Land2Atmos c flux is equal to sum of burned + non-burned land2atmos c flux in all_c_flux
    ## this checks true
    identical(all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]], all_c_flux[["Land2Atmos_c_stock_man_agg"]])
    # this checks true that the difference between total management Land2Atmos c flux and the sum of burned + non-burned land2atmos c flux is minimal (<1 & >-1)
    all(all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]] - all_c_flux[["Land2Atmos_c_stock_man_agg"]] < 1 &
          all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]] - all_c_flux[["Land2Atmos_c_stock_man_agg"]] > -1)
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
    # Next year's "Manage_Wood_CumGain_C_stock" = Current year's "Manage_Wood_CumGain_C_stock" + wood_accumulated
    out_wood_df_list[[7]][,next_wood_label] = out_wood_df_list[[7]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_man_agg
    # Current year's "Manage_Wood_AnnGain_C_stock" = wood_accumulated
    out_wood_df_list[[9]][,cur_wood_label] = -all_c_flux$Land2Wood_c_stock_man_agg
    # Current year's "Manage_Wood_AnnLoss_C_stock" = Current year's "Manage_Wood_C_stock" + wood_accumulated - Next year's "Manage_Wood_C_stock"  
    out_wood_df_list[[10]][,cur_wood_label] = out_wood_df_list[[6]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_man_agg - 
      out_wood_df_list[[6]][,next_wood_label]
    # Next year's "Manage_Wood_CumLoss_C_stock" = Current year's "Manage_Wood_CumLoss_C_stock" + Current year's "Manage_Wood_AnnLoss_C_stock"
    out_wood_df_list[[8]][,next_wood_label] = out_wood_df_list[[8]][,cur_wood_label] + out_wood_df_list[[10]][,cur_wood_label]
    
  
    ############################################################################################################
    ############################################################################################################
    #########################################  Apply FIRE to C pools  ##########################################
    ############################################################################################################
    ############################################################################################################
    
    # apply fire to the carbon pools (current year area and updated carbon)
    # distribute fire to forest, woodland, savanna, shrubland, and grassland, proportionally within the ownerships
    # assume that burn area is not reflected in the baseline land type change numbers
    #  (which isn't necessarily the case)
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
    ############################################################################################################
    ################## second, proportionally distribute ownership fire areas to each landtype #################
    ############################################################################################################
    
    # assign assigned FIRE TARGET AREA BY OWNERSHIP [ha] to "fire_own_area" 
    names(fire_area_df)[names(fire_area_df) == pcol] = "fire_own_area"
    # merge the fire effects dataframe with the fire target areas and assign to fire_adjust_df
    fire_adjust_df = merge(fire_area_df, fire_df, by = c("Intensity"), all.x = TRUE)
    fire_adjust_df$Land_Cat_ID = NULL
    fire_adjust_df$Region = NULL
    fire_adjust_df$Land_Type = NULL
    # merge with the tot_area_df by ownership class
    fire_adjust_df = merge(tot_area_df, fire_adjust_df, by = c("Ownership"), all.x = TRUE)
    # trim dataframe to only include forest, woodland, savanna, grassland, shrubland
    fire_adjust_df = fire_adjust_df[fire_adjust_df$Land_Type == "Forest" | fire_adjust_df$Land_Type == "Woodland" | 
                                      fire_adjust_df$Land_Type == "Savanna" | fire_adjust_df$Land_Type == "Grassland" | 
                                      fire_adjust_df$Land_Type == "Shrubland",]
    # create new dataframe for OWNERSHIP AREA [ha]: AGGREGATE total AREAS by OWNERSHIP, omitting duplicates
    avail_own_area = aggregate(tot_area ~ Ownership, data = unique(fire_adjust_df[,c(1:5)]), sum)
    # rename OWNERSHIP AREA [ha]: "avail_own_area"
    names(avail_own_area)[2] = "avail_own_area"
    # merge FIRE C TRANSFER EFFECTS (fractions) dataframe with the ownership areas dataframe
    fire_adjust_df = merge(avail_own_area, fire_adjust_df, by = c("Ownership"), all.y = TRUE)
    # if assigned FIRE TARGET AREA BY OWNERSHIP [ha] > TOTAL AREA OF OWNERSHIP [ha], set target area equal to the total ownership area
    fire_adjust_df$fire_own_area <- replace(fire_adjust_df$fire_own_area, fire_adjust_df$fire_own_area > fire_adjust_df$avail_own_area, 
                                            fire_adjust_df$avail_own_area)
    # create column for BURNED AREA [ha] for each landtype-ownership combination and proportinally distribute burned areas 
    # BURNED AREA [ha] = (FIRE TARGET AREA BY OWNERSHIP [ha]) * (landtype area / ownership area)
    fire_adjust_df$fire_burn_area = fire_adjust_df$fire_own_area * fire_adjust_df$tot_area / fire_adjust_df$avail_own_area
    
    ############################################################################################################
    ################# third, calc changes in C densities for each of the fire effects within the ###############
    #########################  fire areas withn each landtypes-ownership cmbination ############################
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
    fire_adjust_df = fire_adjust_df[order(fire_adjust_df$Land_Cat_ID),]
    fire_adjust_df[,c(8:ncol(fire_adjust_df))] <- apply(fire_adjust_df[,c(8:ncol(fire_adjust_df))], 2, function (x) {replace(x, is.na(x), 0.00)})
    fire_adjust_df[,c(8:ncol(fire_adjust_df))] <- apply(fire_adjust_df[,c(8:ncol(fire_adjust_df))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    fire_adjust_df[,c(8:ncol(fire_adjust_df))] <- apply(fire_adjust_df[,c(8:ncol(fire_adjust_df))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    ############################################################################################################
    ################ fourth, consolidate the changes in C densities within each C density pool #################
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
    ################################## fifth, calc total C loss to atmosphere #################################  
    ############################################################################################################
    
    # to get the carbon must multiply these by the tot_area
    # atmos
    # calc fire C loss to atmosphere [Mg C] ("Land2Atmos_c_stock_man") = -(total area [ha]) * (soil emissons [MgC/ha] + 
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
    ######### sixth, aggregate changes in each C density pool within each landtype-ownership class ############# 
    ############################################################################################################
    # now aggregate to land type by summing the fire intensities
    # these c density values are the direct changes to the overall c density
    # the c stock values are the total carbon form each land type going to atmos
    
    # first, create table that has a row for each land type ID, and a column for each of the fire-caused C density change [MgC/ha], 
    # and corresponding C transfer to atmosphere [Mg C]  
    fire_agg_cols = array(dim=c(length(fire_adjust_df$Land_Cat_ID),length(fire_agg_names)))
    # second, populate the table by applying loop to each row's land type ID 
    for (i in 1:length(fire_agg_names)) {
      # fill columns with corresponding fire-caused C DENSITY CHANNGES from the fire_adjust_df
      fire_agg_cols[,i] = fire_adjust_df[,fire_agg_names[i]]
    }
    # third, aggregate the C DENSITY CHANGES by summing within each land type-ownership combination and assign to fire_adjust_agg 
    fire_adjust_agg = aggregate(fire_agg_cols ~ Land_Cat_ID + Region + Land_Type + Ownership, data=fire_adjust_df, FUN=sum)
    # fourth, label the columns of the aggregated table 
    fire_agg_names2 = paste0(fire_agg_names,"_fire_agg")
    names(fire_adjust_agg)[c(5:ncol(fire_adjust_agg))] = fire_agg_names2
    # merge these values to the unman area table to apply the adjustments to each land type
    all_c_flux = merge(all_c_flux, fire_adjust_agg, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x = TRUE)
    all_c_flux = all_c_flux[order(all_c_flux$Land_Cat_ID),]
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.na(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, is.nan(x), 0.00)})
    all_c_flux[,c(8:ncol(all_c_flux))] <- apply(all_c_flux[,c(8:ncol(all_c_flux))], 2, function (x) {replace(x, x == Inf, 0.00)})
    
    # check that the fire Land2Atmos c flux is equal to the sum of burned and non-burned land2Atmos c flux in the all_c_flux dataframe
    # checks true
    identical(all_c_flux[["Land2Atmos_BurnedC_stock_fire_agg"]] + all_c_flux[["Land2Atmos_NonBurnedC_stock_fire_agg"]], all_c_flux[["Land2Atmos_c_stock_fire_agg"]])
    
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
    #########  FIRST, ADJUST BASELINE AREA CHANGE FOR RESTORATION, Afforestation & LIMITED GROWTH ##############
    ############################################################################################################
    man_conv_df = man_adjust_df[man_adjust_df$Management == "Restoration" | man_adjust_df$Management == "Afforestation" | 
                                  man_adjust_df$Management == "Growth",1:7]
    man_conv_df = merge(man_conv_df, man_target_df[,1:6], by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Management"))
    names(man_conv_df)[names(man_conv_df) == start_area_label] = "initial_man_area"
    
    conv_adjust_df = merge(conv_adjust_df, man_conv_df, by = c("Land_Cat_ID", "Region", "Land_Type", "Ownership"), all.x=TRUE)
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
        # change will be distributed to other land types proportionally within land type id, except for fresh marsh
        # note that developed land doesn't quite play out as prescribed, does this have to do with fresh marsh?
        
        # calc adjustment to the baseline growth rate for limited growth management 
        # (temp_adjust) [ha] = current year limited growth area - initial limited growth area
        temp_adjust = conv_own$man_area[conv_own$Management == "Growth" & !is.na(conv_own$Management)] - 
          conv_own$initial_man_area[conv_own$Management == "Growth" & !is.na(conv_own$Management)]
        # paste value(s) in the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Management == "Growth" & !is.na(conv_own$Management)] = temp_adjust
        # proportionally distribute the reductions in urban growth area to other land types
        # for each landtype except developed and fresh marsh (not affected by these manipulations since protected): 
        # base_change_adjust [ha] = base_change_adjust - (sum of adjustments to baseline urban growth rate) *
        # (total area of landtype)/(total area of all the other landtypes)
        conv_own$base_change_adjust[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh"] = 
          conv_own$base_change_adjust[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh"] - 
          sum(temp_adjust) * conv_own$tot_area[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh"] / 
          sum(conv_own$tot_area[conv_own$Land_Type != "Developed_all" & conv_own$Land_Type != "Fresh_marsh"])
##############################################################################################################################
##############################################################################################################################       
############################################################################################################################## 
############ DELETE THIS SECTION & TREAT AFFORESTATION LIKE RESTORATION FOR MEADOW AND MARSHES ###############################      
        # Afforestation activities will come proportionally out of _shrub_ and _grassland_ only
        # calc area adjustment for Afforestation (temp_adjust) [ha] = current year Afforestation area - initial Afforestation area
     #   temp_adjust = conv_own$man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] - 
     #     conv_own$initial_man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)]
        # add the difference in Afforestation area since 2010 to the column for base_change_adjust because it's assumed Afforestation is 
        # included in the initial baseline area changes
     #   conv_own$base_change_adjust[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] = 
     #     conv_own$base_change_adjust[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] + temp_adjust
        # subset the base_change_adjust areas for shrub and grass, and subtract, proportionally, the sum of all the area adjustments 
        # for Afforestation (temp_adjust) in shrubland and grassland
      #  conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] = 
      #    conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] - sum(temp_adjust) * 
      #    conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] / 
      #    sum(conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"])
##############################################################################################################################
        # Afforestation activities will come proportionally out of _shrub_ and _grassland_ only
        # calc area adjustment for Afforestation (temp_adjust) [ha] = current year Afforestation area 
        temp_adjust = conv_own$man_area[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] 
        # add the prescribed Afforestation area to the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Management == "Afforestation" & !is.na(conv_own$Management)] + temp_adjust
        # subset the base_change_adjust areas for shrub and grass, and subtract, proportionally, the sum of all the area adjustments 
        # for Afforestation (temp_adjust) in shrubland and grassland
        conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] - sum(temp_adjust) * 
          conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"] / 
          sum(conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland"])
##############################################################################################################################       
##############################################################################################################################       
        # coastal marsh restoration will come out of _agriculture_ land only
        # get area adjustment for COASTAL MARSH RESTORATION (temp_adjust) [ha] = current year management area 
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the COASTAL MARSH RESTORATION area to the column for base_change_adjust 
        conv_own$base_change_adjust[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Coastal_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from Cultivated
        conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] = conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] - 
          sum(temp_adjust) * conv_own$tot_area[conv_own$Land_Type == "Cultivated"] / sum(conv_own$tot_area[conv_own$Land_Type == "Cultivated"])
        
        # fresh marsh restoration will come out of _agriculture_ land only
        # get area adjustment for FRESH MARSH RESTORATION (temp_adjust) [ha] = current year management area 
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the FRESH MARSH RESTORATION area to the column for base_change_adjust
        conv_own$base_change_adjust[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Fresh_marsh" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from Cultivated
        conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] = conv_own$base_change_adjust[conv_own$Land_Type == "Cultivated"] - 
          sum(temp_adjust) * conv_own$tot_area[conv_own$Land_Type == "Cultivated"] / sum(conv_own$tot_area[conv_own$Land_Type == "Cultivated"])
        
        # meadow restoration will come proportionally out of _shrubland_, _grassland_, _savanna_, _woodland_ only
        # get area adjustment for MEADOW RESTORATION (temp_adjust) [ha] = current year management area 
        temp_adjust = conv_own$man_area[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)]
        # add the MEADOW RESTORATION area to the column for base_change_adjust  
        conv_own$base_change_adjust[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Meadow" & conv_own$Management == "Restoration" & !is.na(conv_own$Management)] + 
          temp_adjust
        # subtract this area propotionally from SHRUBLAND, GRASSLAND, & SAVANNA
        conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
                                      conv_own$Land_Type == "Woodland"] = 
          conv_own$base_change_adjust[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
                                        conv_own$Land_Type == "Woodland"] - sum(temp_adjust) * 
          conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
                              conv_own$Land_Type == "Woodland"] / 
          sum(conv_own$tot_area[conv_own$Land_Type == "Shrubland" | conv_own$Land_Type == "Grassland" | conv_own$Land_Type == "Savanna" | 
                                  conv_own$Land_Type == "Woodland"])
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
      ################################## PROTECT RESTORED MARSH & MEADOW AREA VIA AREA CHANGE ADJUSTMENT ################################  
      # first adjust the new area and area change to account for the protection of restored fresh marsh and restored meadow and restored coastal 
      # marsh
      # this also accounts for new area going negative
      # new area should always be greater than the cumulative restored management area so there is no loss of protected area over time.
      # thus, for fresh or coastal marsh, or meadow, if new area < cumulative management area, do the following two steps: 
      # (1) recalculate AREA CHANGE by adding the deficit area to the area change
      # (2) AREA CHANGE [ha] = AREA CHANGE + CUMULATIVE MANAGEMENT AREA - NEW AREA
      conv_own$area_change[conv_own$new_area < conv_own$man_area_sum & 
                             (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh")] = 
        conv_own$area_change[conv_own$new_area < conv_own$man_area_sum & 
                               (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh")] + 
        (conv_own$man_area_sum[conv_own$new_area < conv_own$man_area_sum & 
                                 (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh")] - 
           conv_own$new_area[conv_own$new_area < conv_own$man_area_sum & 
                               (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh")])
      # sum_restored_neg = -1 * ( (aggregate sum all the above cumulative management areas > new areas) + new area )
      sum_restored_neg = -sum(conv_own$man_area_sum[conv_own$new_area < conv_own$man_area_sum & 
                                                      (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | 
                                                         conv_own$Land_Type == "Coastal_marsh")] - 
                                conv_own$new_area[conv_own$new_area < conv_own$man_area_sum & 
                                                    (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | 
                                                       conv_own$Land_Type == "Coastal_marsh")])
      # second, for these cases, set the NEW AREA equal to CUMULATIVE MANAGEMENT AREA 
      conv_own$new_area[conv_own$new_area < conv_own$man_area_sum & 
                          (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh")] = 
        conv_own$man_area_sum[conv_own$new_area < conv_own$man_area_sum & 
                                (conv_own$Land_Type == "Fresh_marsh" | conv_own$Land_Type == "Meadow" | conv_own$Land_Type == "Coastal_marsh")]
      
      # if new area is negative, add the magnitude of the negative area to the area_change and subtract the difference proportionally from the 
      # positive area changes (except for fresh marsh and meadow and coastal marsh), then calc new area again
      # restored fresh marsh and meadow and coastal marsh are protected, so make sure that these restored areas are not negated by this adjustment
      # correct NEGATIVE new areas:
      # AREA CHANGE = AREA CHANGE - NEW AREA (this later sets new area to 0)
      conv_own$area_change[conv_own$new_area < 0] = conv_own$area_change[conv_own$new_area < 0] - conv_own$new_area[conv_own$new_area < 0]
      # sum_neg_new = SUM NEGATIVE NEW AREAS + sum_restored_neg
      sum_neg_new = sum(conv_own$new_area[conv_own$new_area < 0]) + sum_restored_neg
      #  For all expanding landtypes (_except_ FRESH & COASTAL marsh, and MEADOW), set area changes:
      # first, sum the expanding land type area 
      # sum_pos_change = SUM POSITIVE AREA CHANGES
      sum_pos_change = sum(conv_own$area_change[conv_own$area_change > 0 & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Meadow" & 
                                                  conv_own$Land_Type != "Coastal_marsh"])
      # second, adjust AREA CHANGE by subtracting the proportional sum of negative new areas from the positive area changes: 
      # AREA CHANGE = AREA CHANGE + (sum_neg_new * area change / sum_pos_change)
      conv_own$area_change[conv_own$area_change > 0 & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Meadow" & 
                             conv_own$Land_Type != "Coastal_marsh"] = 
        conv_own$area_change[conv_own$area_change > 0 & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Meadow" & conv_own$Land_Type != "Coastal_marsh"] + 
        sum_neg_new * conv_own$area_change[conv_own$area_change > 0 & conv_own$Land_Type != "Fresh_marsh" & conv_own$Land_Type != "Meadow" & 
                                             conv_own$Land_Type != "Coastal_marsh"] / sum_pos_change
      # For all landtypes:
      ## update new area by adding adjusted area changes to the total areas for each landtype-ownership-region combination: 
      # NEW AREA [ha] = TOTAL AREA + AREA CHANGE
      conv_own$new_area = conv_own$tot_area + conv_own$area_change
      
      # check if any of the area changes resulted in negative new area and correct them
      while (any(conv_own$new_area < 0)) {
        # subset any land categories that have negative new areas and add them up to get the area needed to be offset to avoid negative new area
        sum_neg_new_area <- sum(conv_own$new_area[conv_own$new_area < 0])
        # create temporary column for new_area: new_area_temp_df$new_area
        new_area_temp_df <- conv_own
        # replace new_area_temp with any excess new_area > man_area_sum for fresh_marsh, meadow and coastal_marsh
        new_area_temp_df$new_area[new_area_temp_df$new_area > new_area_temp_df$man_area_sum & new_area_temp_df$Land_Type == "Fresh_marsh" | 
                                    new_area_temp_df$Land_Type == "Meadow" | new_area_temp_df$Land_Type == "Coastal_marsh"] <-
          new_area_temp_df$new_area[new_area_temp_df$new_area > new_area_temp_df$man_area_sum & new_area_temp_df$Land_Type == "Fresh_marsh" | 
                                      new_area_temp_df$Land_Type == "Meadow" | new_area_temp_df$Land_Type == "Coastal_marsh"] -
          new_area_temp_df$man_area_sum[new_area_temp_df$new_area > new_area_temp_df$man_area_sum & new_area_temp_df$Land_Type == "Fresh_marsh" | 
                                      new_area_temp_df$Land_Type == "Meadow" | new_area_temp_df$Land_Type == "Coastal_marsh"]
        # sum all the positive new areas, which may include partial amount of new area for the protected land types
        sum_pos_new_area <- sum(new_area_temp_df$new_area[new_area_temp_df$new_area > 0]) 
        # update area_change for land categories with positive new_area by subtracting the area offset (sum_neg_new_area) proportionally (w.r.t. new_area) 
        # from the area_change 
        new_area_temp_df$area_change[new_area_temp_df$new_area > 0] <- new_area_temp_df$area_change[new_area_temp_df$new_area > 0] +
           sum_neg_new_area * new_area_temp_df$new_area[new_area_temp_df$new_area > 0] / sum_pos_new_area
        # update area_change for land categories with negative new_area by subtracting their negative new_area from it (effectively zeroing new area)
        new_area_temp_df$area_change[new_area_temp_df$new_area < 0] <- new_area_temp_df$area_change[new_area_temp_df$new_area < 0] - 
          new_area_temp_df$new_area[new_area_temp_df$new_area < 0]
        # update new_area for negative new_areas by assigning 0 to them to avoid roundoff errors
        new_area_temp_df$new_area[new_area_temp_df$new_area < 0] <- 0
        # update new_area for positive new area land categories by adding the adjusted area_change to the tot_area
        new_area_temp_df$new_area[new_area_temp_df$new_area > 0] = new_area_temp_df$tot_area[new_area_temp_df$new_area > 0] + 
          new_area_temp_df$area_change[new_area_temp_df$new_area > 0]
        # replace conv_own with new_area_temp_df which has the corrected new_area and area_change
        conv_own <- new_area_temp_df
      }
      
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
      # these store the area change from the Land_Type column to the individual land type columns, by ownership
      # a from-to value is positive, a to-from value is negative
      # carbon needs to be subracted for the area losses because the density change values are tracked as normalized carbon
      
      # do only land here because ocean/seagrass is different
      if(current_region_ID != "Ocean") {
        # add up all positive area changes in new column "own_gain_sum" 
        conv_own$own_gain_sum = sum(conv_own$area_change[conv_own$area_change > 0])
        # duplicate dataframe and call it conv_own2 
        conv_own2 = conv_own
        # loop over the land types to get the positive from-to area values (from-to: expanding landtypes. if constant then value is 0)
        for (l in 1:length(conv_own$Land_Type)) {
          # l is landtype index. add new columns that are the gains in area for each land type
          conv_own[,conv_own$Land_Type[l]] = 0.0
          # for each from-to land type column, go to rows of decreasing landtypes: 
          # from-to value = absolute(neg area change) * (area change of the gaining land type)/(sum all gaining areas) 
          conv_own[,conv_own$Land_Type[l]][conv_own$area_change < 0] = - conv_own$area_change[conv_own$area_change < 0] * 
            conv_own$area_change[l] / conv_own$own_gain_sum[l]
        } # end for l loop over land type
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, x < 0, 0.00)})
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, is.nan(x), 0.00)})
        conv_own[,conv_own$Land_Type] <- apply(conv_own[,conv_own$Land_Type], 2, function (x) {replace(x, x == Inf, 0.00)})
        
        # do it again to get the negative to-from values
        for (l in 1:length(conv_own$Land_Type)) {
          conv_own2[,conv_own2$Land_Type[l]] = 0.0
          conv_own2[,conv_own2$Land_Type[l]][conv_own2$area_change > 0] = - conv_own2$area_change[conv_own2$area_change > 0] * 
            conv_own2$area_change[l] / conv_own2$own_gain_sum[l]
        } # end for l loop over land type
        conv_own2[,conv_own2$Land_Type] <- apply(conv_own2[,conv_own2$Land_Type], 2, function (x) {replace(x, x < 0, 0.00)})
        conv_own2[,conv_own2$Land_Type] <- apply(conv_own2[,conv_own2$Land_Type], 2, function (x) {replace(x, is.nan(x), 0.00)})
        conv_own2[,conv_own2$Land_Type] <- apply(conv_own2[,conv_own2$Land_Type], 2, function (x) {replace(x, x == Inf, 0.00)})
        
        # put the negative to-from values into conv_own
        # first find which columns are empty
        zinds = which(apply(conv_own[,conv_col_names],2,sum) == 0)
        conv_own[,conv_col_names][,zinds] = -conv_own2[,conv_col_names][,zinds]
        
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
        for (f in 1:num_convfrac_cols){
          # the removed values are calculated first, so this will work
          # if conv_density_inds[f] == -1, then the source is the removed pool; use the sum of the first two c trans columns
          if (conv_density_inds[f] == -1) {
            # C removed [MgC/ha] =  (removed above C + removed standing dead C) * removed frac.... ("Removed2Wood_conv_frac", "Removed2Energy_conv_frac", 
            # or "Removed2Atmos_conv_frac"
            conv_own[,convc_trans_names[f]] = (conv_own[,convc_trans_names[1]] + conv_own[,convc_trans_names[2]]) * conv_own[,conv_frac_names[f]]
          } else {
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
            # repeat for developed 
            conv_own[conv_own$Developed_all > 0,convc_trans_names[f]] = 
              conv_own[conv_own$Developed_all > 0,convc_trans_names[f]] + 
              conv_own[conv_own$Developed_all > 0,out_density_sheets[conv_density_inds[f]]] * 
              conv_own[conv_own$Developed_all > 0,conv_frac_names[f]] * conv_own$Developed_all[conv_own$Developed_all > 0] / 
              conv_own$tot_area[conv_own$Developed_all > 0]
          } # end if removed source else density source
        } # end for f loop over the managed transfer fractions for calcuting the transfer carbon
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
            # this is the land type column minus the l column difference
            # i.e., difference in C density of pool 'c' between all landtypes & landtype 'l' = 
            # (C densities of pool 'c' [MgC/ha] in each landtype) - (C density of pool 'c' in landtype 'l')  
            lt_conv[,diffname] = lt_conv[,cind] - lt_conv[l,cind]
            # do from-to first; don't need to do anything for a zero column
            # if the area change in landtype 'l' is positive (from-to), then do...
            if(sum(lt_conv[,conv_col_names[l]]) > 0) {
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
                    # go to all the rows in which a landtype is converting area to the landtyle l column, and multiply this area by
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
            } else if (sum(lt_conv[,conv_col_names[l]]) <= 0) {
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
                # calc c dens change (loss) for losing areas with higher C dens that 'to' area (not to ag or dev), by 
                # subsetting rows in c dens change column with positive C dens difference & not ag or dev = 
                # lost area * diff in C dens
                lt_conv[lt_conv[,diffname] > 0 & lt_conv[,conv_col_names[l]] < 0 & 
                          lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", chname] = 
                  lt_conv[lt_conv[,diffname] > 0 & lt_conv[,conv_col_names[l]] < 0 & 
                            lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", conv_col_names[l]] * 
                  lt_conv[l, cind] / lt_conv$tot_area[l]
                # the diff matters here - negative diff values mean some carbon is sent to atmosphere
                # density change = change in "to-from" area * "to" carbon / "from" total area
                # this value should be negative
                lt_conv[lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] < 0 & 
                          lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", chname] = 
                  lt_conv[lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] < 0 & lt_conv$Land_Type != "Cultivated" & 
                            lt_conv$Land_Type != "Developed_all", conv_col_names[l]] * 
                  lt_conv[lt_conv[,diffname] < 0 & lt_conv[,conv_col_names[l]] < 0 & 
                            lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all", cind] / lt_conv$tot_area[l]
                # send above ground lost carbon to the atmosphere if necessary
                # operate only where to-from diff is negative
                # including the case where the values are 0
                # 2atmos = "to" minus "from" diff * "from-to" area / "from" total area
                # this value ends up positive, consistent with the removed transfers above
                atmosname = paste0(out_density_sheets[c],"2Atmos")
                lt_conv[,atmosname] = 0
                lt_conv[(lt_conv[,diffname] <= 0 & lt_conv[,conv_col_names[l]] <= 0 & 
                           lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all"), atmosname] = 
                  lt_conv[(lt_conv[,diffname] <= 0 & lt_conv[,conv_col_names[l]] <= 0 & 
                             lt_conv$Land_Type != "Cultivated" & lt_conv$Land_Type != "Developed_all"),diffname] * 
                  lt_conv[(lt_conv[,diffname] <= 0 & lt_conv[,conv_col_names[l]] <= 0 & lt_conv$Land_Type != "Cultivated" & 
                             lt_conv$Land_Type != "Developed_all"), conv_col_names[l]] / lt_conv$tot_area[l]
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
        conv_own$own_gain_sum = sum(conv_own$area_change[conv_own$area_change > 0])
        skip = length(names(conv_own))
        # 'add' gets a vector of all the column names from cov_own table
        #add = names(own_conv_df_list_pre[[1]])[(skip+1):ncol(own_conv_df_list_pre[[1]])]
        add = names(own_conv_df_list[[1]])[(skip+1):ncol(own_conv_df_list[[1]])]
        
        # fill in all the conv_own columns and fill with 0
        conv_own[,add] = 0
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
      own_conv_df_list_pre[[i]] = conv_own 
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
          begin_set_cols <- own_conv_df_list_pre[[g]][,c(1:26)]
          # get full set of landtype columns in order
          all_16landtype_cols <- own_conv_df_list_pre[[g]][,c(conv_col_names_all)]
          # get number of _not_ missing column names 
          numb_not_missing <- length(conv_col_names_all) - length(missing_names)
          # get column index to start on for end set of columns
          end_start_ind <- 26 + numb_not_missing + 1
          # get column index to end on for end set of columns
          end_end_ind <- full_length - length(missing_names)
          # subset the columns following the original landtype columns and before the ones that were added to the end
          # order the sequence of columns in the last chunk
          end_set_cols <- own_conv_df_list_pre[[g]][,c("Above_main_C_den", "Above_removed_conv_c", "StandDead_C_den", "StandDead_removed_conv_c", 
                                          "Removed2Wood_conv_c", "Removed2Energy_conv_c", "Removed2Atmos_conv_c", "Understory_C_den",           
                                          "Understory2Atmos_conv_c", "DownDead_C_den", "DownDead2Atmos_conv_c", "Litter_C_den", "Litter2Atmos_conv_c",
                                          "Soil_orgC_den", "Soil2Atmos_conv_c", "Understory2DownDead_conv_c", "Below_main_C_den","Below2Atmos_conv_c",        
                                          "Below2Soil_conv_c", "Above_main_C_den_change", "Below_main_C_den_change", "Understory_C_den_change",
                                          "StandDead_C_den_change", "DownDead_C_den_change", "Litter_C_den_change", "Soil_orgC_den_change", 
                                          "Above_main_C_den2Atmos", "Understory_C_den2Atmos", "StandDead_C_den2Atmos", "DownDead_C_den2Atmos",      
                                          "Litter_C_den2Atmos")]
          # merge the subsets of columns back together. They're in proper order now to rbind below
          own_conv_df_list_pre[[g]] <- cbind(begin_set_cols, all_16landtype_cols, end_set_cols) }
      }
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
    all_c_flux[,cgnames[1]] = - conv_adjust_df$Above_removed_conv_c - conv_adjust_df$Above_main_C_den2Atmos + 
      conv_adjust_df$Above_main_C_den_change
    # below
    cgnames = c(cgnames, paste0(out_density_sheets[4],"_gain_conv"))
    all_c_flux[,cgnames[2]] = - conv_adjust_df$Below2Atmos_conv_c + conv_adjust_df$Below_main_C_den_change
    # understory
    cgnames = c(cgnames, paste0(out_density_sheets[5],"_gain_conv"))
    all_c_flux[,cgnames[3]] = - conv_adjust_df$Understory2Atmos_conv_c - conv_adjust_df$Understory2DownDead_conv_c - 
      conv_adjust_df$Understory_C_den2Atmos + conv_adjust_df$Understory_C_den_change
    # standing dead
    cgnames = c(cgnames, paste0(out_density_sheets[6],"_gain_conv"))
    all_c_flux[,cgnames[4]] = - conv_adjust_df$StandDead_removed_conv_c - conv_adjust_df$StandDead_C_den2Atmos + 
      conv_adjust_df$StandDead_C_den_change
    # down dead
    cgnames = c(cgnames, paste0(out_density_sheets[7],"_gain_conv"))
    all_c_flux[,cgnames[5]] = - conv_adjust_df$DownDead2Atmos_conv_c + conv_adjust_df$Understory2DownDead_conv_c - 
      conv_adjust_df$DownDead_C_den2Atmos + conv_adjust_df$DownDead_C_den_change
    # litter
    cgnames = c(cgnames, paste0(out_density_sheets[8],"_gain_conv"))
    all_c_flux[,cgnames[6]] = - conv_adjust_df$Litter2Atmos_conv_c - conv_adjust_df$Litter_C_den2Atmos + conv_adjust_df$Litter_C_den_change
    # soil
    cgnames = c(cgnames, paste0(out_density_sheets[9],"_gain_conv"))
    all_c_flux[,cgnames[7]] = - conv_adjust_df$Soil2Atmos_conv_c + conv_adjust_df$Soil_orgC_den_change
    
    
    # loop over the relevant out density tables to update the carbon pools based on the conversion fluxes
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
    
    # to get the carbon must multiply these by the tot_area
    # atmos: assumed none of this is burned
    # getting stocks of total c emissions (assumed to be CO2) 
    all_c_flux[,"Land2Atmos_c_stock_conv"] = -conv_adjust_df$tot_area * 
      (conv_adjust_df$Soil2Atmos_conv_c + conv_adjust_df$Litter2Atmos_conv_c + conv_adjust_df$DownDead2Atmos_conv_c + 
         conv_adjust_df$Understory2Atmos_conv_c + conv_adjust_df$Removed2Atmos_conv_c + conv_adjust_df$Below2Atmos_conv_c + 
         conv_adjust_df$Above_main_C_den2Atmos + conv_adjust_df$Understory_C_den2Atmos + conv_adjust_df$StandDead_C_den2Atmos + 
         conv_adjust_df$DownDead_C_den2Atmos + conv_adjust_df$Litter_C_den2Atmos)
    # energy - this is assumed to go to the atmosphere immediately
    all_c_flux[,"Land2Energy_c_stock_conv"] = -conv_adjust_df$tot_area * (conv_adjust_df$Removed2Energy_conv_c)
    # wood - this decays with a half-life
    all_c_flux[,"Land2Wood_c_stock_conv"] = -conv_adjust_df$tot_area * (conv_adjust_df$Removed2Wood_conv_c)
    
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
    out_wood_df_list[[11]][,next_wood_label] = out_wood_df_list[[11]][,cur_wood_label] * exp(-k) + ((1 - exp(-k)) / k) * 
      (-all_c_flux$Land2Wood_c_stock_conv)
    # next year's "LCC_Wood_CumGain_C_stock" = current year's "LCC_Wood_CumGain_C_stock" + harvested_wood_conv
    out_wood_df_list[[12]][,next_wood_label] = out_wood_df_list[[12]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_conv
    # current year's "LCC_Wood_AnnGain_C_stock" = harvested_wood_conv
    out_wood_df_list[[14]][,cur_wood_label] = -all_c_flux$Land2Wood_c_stock_conv
    # current year's "LCC_Wood_AnnLoss_C_stock" = current year's "LCC_Wood_C_stock" + harvested_wood_conv - next year's "LCC_Wood_C_stock"
    out_wood_df_list[[15]][,cur_wood_label] = out_wood_df_list[[11]][,cur_wood_label] - all_c_flux$Land2Wood_c_stock_conv - 
      out_wood_df_list[[11]][,next_wood_label]
    # next year's "LCC_Wood_CumLoss_C_stock" = current year's "LCC_Wood_CumLoss_C_stock" + current year's "LCC_Wood_AnnLoss_C_stock" 
    out_wood_df_list[[13]][,next_wood_label] = out_wood_df_list[[13]][,cur_wood_label] + out_wood_df_list[[15]][,cur_wood_label]
    
    # update the total wood tables
    out_wood_df_list[[1]][,next_wood_label] = out_wood_df_list[[6]][,next_wood_label] + out_wood_df_list[[11]][,next_wood_label]
    out_wood_df_list[[2]][,next_wood_label] = out_wood_df_list[[7]][,next_wood_label] + out_wood_df_list[[12]][,next_wood_label]
    out_wood_df_list[[3]][,next_wood_label] = out_wood_df_list[[8]][,next_wood_label] + out_wood_df_list[[13]][,next_wood_label]
    out_wood_df_list[[4]][,cur_wood_label] = out_wood_df_list[[9]][,cur_wood_label] + out_wood_df_list[[14]][,cur_wood_label]
    out_wood_df_list[[5]][,cur_wood_label] = out_wood_df_list[[10]][,cur_wood_label] + out_wood_df_list[[15]][,cur_wood_label]
    
    # set the new tot area
    out_area_df_list[[1]][,next_area_label] = all_c_flux$new_area
    
    # set this years actual managed area (the area change activities are still just targets)
    # can have the case where there isn't enough area to do the full input scenario target mangagement area (correction was done earlier)
    out_area_df_list[[2]][,cur_area_label] = man_adjust_df$man_area
    
    # set this years actual fire area - output by the lt breakdown
    if(year == start_year){
      # add 3rd data frame with these introductory columns
      out_area_df_list[[3]] = fire_adjust_df[,c("Land_Cat_ID", "Region", "Land_Type", "Ownership", "Intensity")]
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
    
    # "Eco_CumGain_C_stock" = current year "Eco_CumGain_C_stock"  + total area * (sum of all changes in c density pools)
    out_atmos_df_list[[1]][, next_atmos_label] = out_atmos_df_list[[1]][, cur_atmos_label] + all_c_flux[,"tot_area"] * 
      (all_c_flux[,11] + all_c_flux[,12] + all_c_flux[,13] + all_c_flux[,14] + all_c_flux[,15] + all_c_flux[,16] + all_c_flux[,17])
    
    # "Manage_Atmos_CumGain_C_stock" based on biomass removal and energy (note: actually adding terms because they are negative)
    # "Manage_Atmos_CumGain_C_stock" = (current year "Manage_Atmos_CumGain_C_stock") - "Land2Atmos_c_stock_man_agg" -
    # "Land2Energy_c_stock_man_agg"  
    out_atmos_df_list[[3]][, next_atmos_label] = out_atmos_df_list[[3]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_c_stock_man_agg"] - 
      all_c_flux[,"Land2Energy_c_stock_man_agg"]
    
    # "Fire_Atmos_CumGain_C_stock"
    out_atmos_df_list[[4]][, next_atmos_label] = out_atmos_df_list[[4]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_c_stock_fire_agg"]
    
    # "LCC_Atmos_CumGain_C_stock" based on land cover change with associated biomass removal and energy
    out_atmos_df_list[[5]][, next_atmos_label] = out_atmos_df_list[[5]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_c_stock_conv"] - 
      all_c_flux[,"Land2Energy_c_stock_conv"]
    
    # "Wood_Atmos_CumGain_C_stock" from the wood tables: "Total_Wood_CumLoss_C_stock"
    out_atmos_df_list[[6]][, next_atmos_label] = out_wood_df_list[[3]][,next_wood_label]
    
    # "Total_Energy2Atmos_C_stock" just to compare it with the total cum atmos c
    out_atmos_df_list[[7]][, next_atmos_label] = out_atmos_df_list[[7]][, cur_atmos_label] - all_c_flux[,"Land2Energy_c_stock_man_agg"] - 
      all_c_flux[,"Land2Energy_c_stock_conv"]
    
    # "Total_Atmos_CumGain_C_stock" the total release of land and wood product and energy c to the atmosphere
    # the energy release is inluded in the manage and lcc releases
    out_atmos_df_list[[2]][, next_atmos_label] = out_atmos_df_list[[3]][,next_atmos_label] + out_atmos_df_list[[4]][,next_atmos_label] + 
      out_atmos_df_list[[5]][,next_atmos_label] + out_atmos_df_list[[6]][,next_atmos_label]
    
    ##### ANNUAL FLUXES	#####
    
    # "Eco_AnnGain_C_stock" = total area * (Above_main_C_den_gain_eco + Below_main_C_den_gain_eco + Understory_C_den_gain_ec +
    #                                       StandDead_C_den_gain_eco + DownDead_C_den_gain_eco + Litter_C_den_gain_eco +
    #                                       Soil_orgC_den_gain_eco)
    out_atmos_df_list[[8]][, cur_atmos_label] = all_c_flux[,"tot_area"] * 
      (all_c_flux[,11] + all_c_flux[,12] + all_c_flux[,13] + all_c_flux[,14] + all_c_flux[,15] + all_c_flux[,16] + all_c_flux[,17])
    # "Manage_Atmos_AnnGain_C_stock" based on biomass removal & energy from biomass
    out_atmos_df_list[[10]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_c_stock_man_agg"] - all_c_flux[,"Land2Energy_c_stock_man_agg"]
    # "Fire_Atmos_AnnGain_C_stock" based on fire
    out_atmos_df_list[[11]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_c_stock_fire_agg"]
    # "LCC_Atmos_AnnGain_C_stock" based on land cover change with associated biomass removal, includes energy from biomass
    out_atmos_df_list[[12]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_c_stock_conv"] - all_c_flux[,"Land2Energy_c_stock_conv"]
    # "Wood_Atmos_AnnGain_C_stock" from the wood tables: "Total_Wood_CumLoss_C_stock"
    out_atmos_df_list[[13]][, cur_atmos_label] = out_wood_df_list[[5]][,cur_wood_label]
    
    # "Total_AnnEnergy2Atmos_C_stock" just to compare it with the total cum atmos c
    out_atmos_df_list[[14]][, cur_atmos_label] = - all_c_flux[,"Land2Energy_c_stock_man_agg"] - all_c_flux[,"Land2Energy_c_stock_conv"]
    # "Total_Atmos_AnnGain_C_stock" the total release of land and wood product and energy c to the atmosphere
    # the energy release is inluded in the manage and lcc releases
    out_atmos_df_list[[9]][, cur_atmos_label] = out_atmos_df_list[[10]][,cur_atmos_label] + out_atmos_df_list[[11]][,cur_atmos_label] + 
      out_atmos_df_list[[12]][,cur_atmos_label] + out_atmos_df_list[[13]][,cur_atmos_label]
    
    ### cumulative (again) ### 
    
    # Partition the "Manage_Atmos_CumGain_C_stock" into FIRE, ENERGY, and NON-BURNED C fluxes to atmosphere
    
    # FIRE: "Manage_Atmos_CumGain_FireC" = (current year "Manage_Atmos_CumGain_FireC") - "Land2Atmos_burnedC_stock_man_agg" -
    # "Land2Energy_c_stock_man_agg" (note: Land2Atmos_burnedC_stock_man_agg does not include bioenergy)
    out_atmos_df_list[[15]][, next_atmos_label] = out_atmos_df_list[[15]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_burnedC_stock_man_agg"] 
    # ENERGY: "Manage_Atmos_CumGain_EnergyC" = (current year "Manage_Atmos_CumGain_EnergyC") - "Land2Energy_c_stock_man_agg" 
    out_atmos_df_list[[16]][, next_atmos_label] = out_atmos_df_list[[16]][, cur_atmos_label] - all_c_flux[,"Land2Energy_c_stock_man_agg"]
    # NON-BURNED: "Manage_Atmos_CumGain_NonBurnedC" = (current year "Manage_Atmos_CumGain_NonBurnedC") - "Land2Atmos_nonburnedC_stock_man_agg"
    out_atmos_df_list[[17]][, next_atmos_label] = out_atmos_df_list[[17]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_nonburnedC_stock_man_agg"]  
    
    # checks true:  management land to atmosphere C flux equal to the management burned plus unburned land to atmosphere C flux
    identical(all_c_flux[["Land2Atmos_c_stock_man_agg"]], all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]])
    # Also checks true that the difference is <0.5 and >-0.5 (in case rounding error)
    all((- all_c_flux[["Land2Atmos_c_stock_man_agg"]] + all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]]) < 0.5 & 
          (- all_c_flux[["Land2Atmos_c_stock_man_agg"]] + all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]]) > -0.5)
    
    # checks true: next year's management Land2Atmos cumulative C = current year's Land2Atmos cumulative C - current year Land2Atmos - current year's Land2Energy
    identical(out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, next_atmos_label], out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, cur_atmos_label] -
                all_c_flux[["Land2Atmos_c_stock_man_agg"]] - all_c_flux[["Land2Energy_c_stock_man_agg"]])
    
    # checks true: next year's management Land2Atmos cumulative C = current year's Land2Atmos cumulative C - current year Land2Atmos_burned - current year Land2Atmos_nonburned - 
    # current year land2energy
    identical(out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, next_atmos_label], out_atmos_df_list[["Manage_Atmos_CumGain_C_stock"]][, cur_atmos_label] -
                all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] - all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]] - all_c_flux[["Land2Energy_c_stock_man_agg"]])
    # recall that: all_c_flux[["Land2Atmos_c_stock_man_agg"]] == all_c_flux[["Land2Atmos_burnedC_stock_man_agg"]] + all_c_flux[["Land2Atmos_nonburnedC_stock_man_agg"]] 
    # all_c_flux[["Land2Energy_c_stock_man_agg"]] is separate, and added up in the out_atmos_df_list
    
    # Partition the "Fire_Atmos_CumGain_C_stock" into burned and non-burned C sources (currently all burned because root and soil C are 0, but
    # including this here in case changes are later made to those input wildfire fractions)
    
    # FIRE burned: "Fire_Atmos_CumGain_BurnedC" = (current year "Fire_Atmos_CumGain_BurnedC") - "Land2Atmos_BurnedC_stock_fire_agg" 
    out_atmos_df_list[[18]][, next_atmos_label] = out_atmos_df_list[[18]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_BurnedC_stock_fire_agg"]
    # FIRE non-burned: "Fire_Atmos_CumGain_NonBurnedC" = (current year "Fire_Atmos_CumGain_NonBurnedC") - "Land2Atmos_NonBurnedC_stock_fire_agg" 
    out_atmos_df_list[[19]][, next_atmos_label] = out_atmos_df_list[[19]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_NonBurnedC_stock_fire_agg"]
    
    # Partition the "LCC_Atmos_CumGain_C_stock" into burned (energy only) and non-burned C sources 
    # With the exception of removed C to energy, we are currently assuming that all lost above- and below-ground c (except removed2wood) 
    # is released as CO2 (decomposition) and not burned
    
    # LCC Energy: "LCC_Atmos_CumGain_EnergyC" = (current year "LCC_Atmos_CumGain_EnergyC") - "Land2Energy_c_stock_conv"
    out_atmos_df_list[[20]][, next_atmos_label] = out_atmos_df_list[[20]][, cur_atmos_label] - all_c_flux[,"Land2Energy_c_stock_conv"]
    # LCC non-burned: "LCC_Atmos_CumGain_NonBurnEnerC" = (current year "LCC_Atmos_CumGain_EnergyC") - "Land2Atmos_c_stock_conv"
    out_atmos_df_list[[21]][, next_atmos_label] = out_atmos_df_list[[21]][, cur_atmos_label] - all_c_flux[,"Land2Atmos_c_stock_conv"]
    
    ### annual (again) ###
    
    # Partition the "Manage_Atmos_AnnGain_C_stock" into FIRE, ENERGY, and NON-BURNED C fluxes to atmosphere
    
    # FIRE: "Manage_Atmos_AnnGain_FireC" = - "Land2Atmos_burnedC_stock_man_agg" 
    out_atmos_df_list[[22]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_burnedC_stock_man_agg"] 
    # ENERGY: "Manage_Atmos_AnnGain_EnergyC" = - "Land2Energy_c_stock_man_agg"
    out_atmos_df_list[[23]][, cur_atmos_label] = - all_c_flux[,"Land2Energy_c_stock_man_agg"]
    # non-burned: "Manage_Atmos_AnnGain_NonBurnedC" = - "Land2Atmos_nonburnedC_stock_man_agg"
    out_atmos_df_list[[24]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_nonburnedC_stock_man_agg"]  
    
    # Partition the "Fire_Atmos_AnnGain_C_stock" into burned and non-burned C sources (currently all burned because root and soil C are 0, but
    # including this here in case changes are later made to those input wildfire fractions)
    # burned: "Fire_Atmos_AnnGain_BurnedC" = - "Land2Atmos_BurnedC_stock_fire_agg" 
    out_atmos_df_list[[25]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_BurnedC_stock_fire_agg"]
    # non-burned: "Fire_Atmos_AnnGain_NonBurnedC" = - "Land2Atmos_NonBurnedC_stock_fire_agg" 
    out_atmos_df_list[[26]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_NonBurnedC_stock_fire_agg"]
    
    # Partition the "LCC_Atmos_AnnGain_C_stock" into burned (energy only) and non-burned C sources 
    # With the exception of removed C to energy, we are currently assuming that all lost above- and below-ground c (except removed2wood) 
    # is released as CO2 (decomposition) and not burned
    # burned: "LCC_Atmos_AnnGain_EnergyC" = - "Land2Energy_c_stock_conv"
    out_atmos_df_list[[27]][, cur_atmos_label] = - all_c_flux[,"Land2Energy_c_stock_conv"]
    # non-burned: "LCC_Atmos_AnnGain_NonBurnEnerC" = - "Land2Atmos_c_stock_conv"
    out_atmos_df_list[[28]][, cur_atmos_label] = - all_c_flux[,"Land2Atmos_c_stock_conv"]
    
  } # end loop over calculation years
  
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
  
  # Partition all the appropriate burned (fire + energy) dataframes in out_atmos_df_list into CO2C, CH4C and BC-C.
  
  ########## Cumulative ########## 
  
  # MANAGE FIRE
  Manage_CumFireC <- out_atmos_df_list[["Manage_Atmos_CumGain_FireC"]]
  Manage_Fire_CumCO2C <- Manage_CumFireC
  for (i in 5:ncol(Manage_CumFireC)) {
    Manage_Fire_CumCO2C[,i] <- CO2C_fire_frac * Manage_CumFireC[,i]
  }
  Manage_Fire_CumCH4C <- Manage_CumFireC
  for (i in 5:ncol(Manage_CumFireC)) {
    Manage_Fire_CumCH4C[,i] <- CH4C_fire_frac * Manage_CumFireC[,i]
  }
  Manage_Fire_CumBCC <- Manage_CumFireC
  for (i in 5:ncol(Manage_CumFireC)) {
    Manage_Fire_CumBCC[,i] <- BCC_fire_frac * Manage_CumFireC[,i]
  }
  
  # MANAGE ENERGY
  Manage_CumEnergyC <- out_atmos_df_list[["Manage_Atmos_CumGain_EnergyC"]]  
  ManEnergy_CumCO2C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManEnergy_CumCO2C[,i] <- CO2C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManEnergy_CumCH4C <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManEnergy_CumCH4C[,i] <- CH4C_energy_frac * Manage_CumEnergyC[,i]
  }
  ManEnergy_CumBCC <- Manage_CumEnergyC
  for (i in 5:ncol(Manage_CumEnergyC)) {
    ManEnergy_CumBCC[,i] <- BCC_energy_frac * Manage_CumEnergyC[,i]
  }
  
  # LCC ENERGY
  LCC_CumEnergyC <- out_atmos_df_list[["LCC_Atmos_CumGain_EnergyC"]]
  LCCEnergy_CumCO2C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCEnergy_CumCO2C[,i] <- CO2C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCEnergy_CumCH4C <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCEnergy_CumCH4C[,i] <- CH4C_energy_frac * LCC_CumEnergyC[,i]
  }
  LCCEnergy_CumBCC <- LCC_CumEnergyC
  for (i in 5:ncol(LCC_CumEnergyC)) {
    LCCEnergy_CumBCC[,i] <- BCC_energy_frac * LCC_CumEnergyC[,i]
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
  
  # TOTAL ENERGY CO2-C, CH4-C, and BC-C 
  TotalEnergy_CumCO2C <- LCCEnergy_CumCO2C
  TotalEnergy_CumCO2C[,5:ncol(LCCEnergy_CumCO2C)] <- LCCEnergy_CumCO2C[,5:ncol(LCCEnergy_CumCO2C)] + ManEnergy_CumCO2C[,5:ncol(LCCEnergy_CumCO2C)]
  TotalEnergy_CumCH4C <- LCCEnergy_CumCH4C
  TotalEnergy_CumCH4C[,5:ncol(LCCEnergy_CumCH4C)] <- LCCEnergy_CumCH4C[,5:ncol(LCCEnergy_CumCH4C)] + ManEnergy_CumCH4C[,5:ncol(LCCEnergy_CumCH4C)]
  TotalEnergy_CumBCC <- LCCEnergy_CumBCC
  TotalEnergy_CumBCC[,5:ncol(LCCEnergy_CumBCC)] <- LCCEnergy_CumBCC[,5:ncol(LCCEnergy_CumBCC)] + ManEnergy_CumBCC[,5:ncol(LCCEnergy_CumBCC)]
  
  ########## Annual ########## 
  
  # MANAGE FIRE
  Manage_AnnFireC <- out_atmos_df_list[["Manage_Atmos_AnnGain_FireC"]]
  Manage_Fire_AnnCO2C <- Manage_AnnFireC
  for (i in 5:ncol(Manage_AnnFireC)) {
    Manage_Fire_AnnCO2C[,i] <- CO2C_fire_frac * Manage_AnnFireC[,i]
  }
  Manage_Fire_AnnCH4C <- Manage_AnnFireC
  for (i in 5:ncol(Manage_AnnFireC)) {
    Manage_Fire_AnnCH4C[,i] <- CH4C_fire_frac * Manage_AnnFireC[,i]
  }
  Manage_Fire_AnnBCC <- Manage_AnnFireC
  for (i in 5:ncol(Manage_AnnFireC)) {
    Manage_Fire_AnnBCC[,i] <- BCC_fire_frac * Manage_AnnFireC[,i]
  }
  
  # MANAGE ENERGY
  Manage_AnnEnergyC <- out_atmos_df_list[["Manage_Atmos_AnnGain_EnergyC"]]  
  ManEnergy_AnnCO2C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManEnergy_AnnCO2C[,i] <- CO2C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManEnergy_AnnCH4C <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManEnergy_AnnCH4C[,i] <- CH4C_energy_frac * Manage_AnnEnergyC[,i]
  }
  ManEnergy_AnnBCC <- Manage_AnnEnergyC
  for (i in 5:ncol(Manage_AnnEnergyC)) {
    ManEnergy_AnnBCC[,i] <- BCC_energy_frac * Manage_AnnEnergyC[,i]
  }
  
  # LCC ENERGY
  LCC_AnnEnergyC <- out_atmos_df_list[["LCC_Atmos_AnnGain_EnergyC"]]
  LCCEnergy_AnnCO2C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCEnergy_AnnCO2C[,i] <- CO2C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCEnergy_AnnCH4C <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCEnergy_AnnCH4C[,i] <- CH4C_energy_frac * LCC_AnnEnergyC[,i]
  }
  LCCEnergy_AnnBCC <- LCC_AnnEnergyC
  for (i in 5:ncol(LCC_AnnEnergyC)) {
    LCCEnergy_AnnBCC[,i] <- BCC_energy_frac * LCC_AnnEnergyC[,i]
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
  
  # TOTAL ENERGY CO2-C, CH4-C, and BC-C 
  TotalEnergy_AnnCO2C <- LCCEnergy_AnnCO2C
  TotalEnergy_AnnCO2C[,5:ncol(LCCEnergy_AnnCO2C)] <- LCCEnergy_AnnCO2C[,5:ncol(LCCEnergy_AnnCO2C)] + ManEnergy_AnnCO2C[,5:ncol(LCCEnergy_AnnCO2C)]
  TotalEnergy_AnnCH4C <- LCCEnergy_AnnCH4C
  TotalEnergy_AnnCH4C[,5:ncol(LCCEnergy_AnnCH4C)] <- LCCEnergy_AnnCH4C[,5:ncol(LCCEnergy_AnnCH4C)] + ManEnergy_AnnCH4C[,5:ncol(LCCEnergy_AnnCH4C)]
  TotalEnergy_AnnBCC <- LCCEnergy_AnnBCC
  TotalEnergy_AnnBCC[,5:ncol(LCCEnergy_AnnBCC)] <- LCCEnergy_AnnBCC[,5:ncol(LCCEnergy_AnnBCC)] + ManEnergy_AnnBCC[,5:ncol(LCCEnergy_AnnBCC)]
  
  ############## Partition Cumulative C emissions from total wood decay (management & lcc) into CO2-C and CH4-C ############## 
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
  
  # sum all CO2-C, CH4-C, and BC-C emissions from burned and non-burned sources. Total should equal total atmosphere C gain 
  # (less eco C emissions).

  ####### Cumulative ####### 
  # first, do cumulative CO2-C. Choice of ncol(Manage_Fire_CumCO2C) is arbitrary -  just need the total number of columns.
  Total_CumCO2C <- Manage_Fire_CumCO2C
  for (i in 5:ncol(Total_CumCO2C)) {
    Total_CumCO2C[,i] <- 0
  }
  for (i in 5:ncol(Total_CumCO2C)) { 
    Total_CumCO2C[,i] <- Eco_CumCO2C[,i] + wood_CumCO2C[,i] + out_atmos_df_list[["Manage_Atmos_CumGain_NonBurnedC"]][,i] + 
      out_atmos_df_list[["Fire_Atmos_CumGain_NonBurnedC"]][,i] + out_atmos_df_list[["LCC_Atmos_CumGain_NonBurnEnerC"]][,i] + 
      Manage_Fire_CumCO2C[,i] + ManEnergy_CumCO2C[,i] + Wildfire_CumCO2C[,i] + LCCEnergy_CumCO2C[,i]
  }
  # Second, do cumulative CH4-C. Choice of ncol(Manage_Fire_CumCH4C) is arbitrary -  just need the total number of columns.
  Total_CumCH4C <- Manage_Fire_CumCH4C
  for (i in 5:ncol(Total_CumCH4C)) {
    Total_CumCH4C[,i] <- 0
  }
  for (i in 5:ncol(Total_CumCH4C)) { 
    Total_CumCH4C[,i] <- Eco_CumCH4C[,i] + Manage_Fire_CumCH4C[,i] + ManEnergy_CumCH4C[,i] + Wildfire_CumCH4C[,i] + 
      LCCEnergy_CumCH4C[,i] + wood_CumCH4C[,i]
  }
  # Third, do cumulative BC-C. Choice of ncol(Manage_Burn_CumBCC) is arbitrary -  just need the total number of columns.
  Total_CumBCC <- Manage_Fire_CumBCC
  for (i in 5:ncol(Total_CumBCC)) {
    Total_CumBCC[,i] <- 0
  }
  for (i in 5:ncol(Total_CumBCC)) {
    Total_CumBCC[,i] <- Manage_Fire_CumBCC[,i] + ManEnergy_CumBCC[,i] + Wildfire_CumBCC[,i] + LCCEnergy_CumBCC[,i]
  }
  
  ####### Annual ####### 
  # first, do annual CO2-C. Choice of ncol(Manage_Fire_AnnCO2C) is arbitrary -  just need the total number of columns.
  Total_AnnCO2C <- Manage_Fire_AnnCO2C
  for (i in 5:ncol(Total_AnnCO2C)) {
    Total_AnnCO2C[,i] <- 0
  }
  for (i in 5:ncol(Total_AnnCO2C)) {  
    Total_AnnCO2C[,i] <- Eco_AnnCO2C[,i] + wood_AnnCO2C[,i] + out_atmos_df_list[["Manage_Atmos_AnnGain_NonBurnedC"]][,i] + 
      out_atmos_df_list[["Fire_Atmos_AnnGain_NonBurnedC"]][,i] + out_atmos_df_list[["LCC_Atmos_AnnGain_NonBurnEnerC"]][,i] +
      Manage_Fire_AnnCO2C[,i] + ManEnergy_AnnCO2C[,i] + Wildfire_AnnCO2C[,i] + LCCEnergy_AnnCO2C[,i] 
  }
  # Second, do annual CH4-C. Choice of ncol(Manage_Fire_AnnCH4C) is arbitrary -  just need the total number of columns.
  Total_AnnCH4C <- Manage_Fire_AnnCH4C
  for (i in 5:ncol(Total_AnnCH4C)) {
    Total_AnnCH4C[,i] <- 0
  }
  for (i in 5:ncol(Total_AnnCH4C)) { 
    Total_AnnCH4C[,i] <- Eco_AnnCH4C[,i] + Manage_Fire_AnnCH4C[,i] + ManEnergy_AnnCH4C[,i] + Wildfire_AnnCH4C[,i] + 
      LCCEnergy_AnnCH4C[,i] + wood_AnnCH4C[,i]
  }
  # Third, do annual BC-C. Choice of ncol(Manage_Fire_AnnBCC) is arbitrary -  just need the total number of columns.
  Total_AnnBCC <- Manage_Fire_AnnBCC
  for (i in 5:ncol(Total_AnnBCC)) {
    Total_AnnBCC[,i] <- 0
  }
  for (i in 5:ncol(Manage_Fire_AnnBCC)) {
    Total_AnnBCC[,i] <- Manage_Fire_AnnBCC[,i] + ManEnergy_AnnBCC[,i] + Wildfire_AnnBCC[,i] + LCCEnergy_AnnBCC[,i]
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
  LCCNonBurn_CumCO2C <- out_atmos_df_list[["LCC_Atmos_CumGain_NonBurnEnerC"]]
  LCCNonBurn_AnnCO2C <- out_atmos_df_list[["LCC_Atmos_AnnGain_NonBurnEnerC"]]
  
  # create list of all the additonal tables from which we want to calculate GWP 
  df.list <- list(Eco_CumCO2C = Eco_CumCO2C,
                  Eco_CumCH4C = Eco_CumCH4C,
                  
                  ManEnergy_CumCO2C = ManEnergy_CumCO2C,
                  ManEnergy_CumCH4C = ManEnergy_CumCH4C,
                  ManEnergy_CumBCC = ManEnergy_CumBCC,
                  ManFire_CumCO2C = Manage_Fire_CumCO2C,
                  ManFire_CumCH4C = Manage_Fire_CumCH4C,
                  ManFire_CumBCC = Manage_Fire_CumBCC,
                  ManNonBurn_CumCO2C = ManNonBurn_CumCO2C,
                  
                  LCCEnergy_CumCO2C = LCCEnergy_CumCO2C,
                  LCCEnergy_CumCH4C = LCCEnergy_CumCH4C,
                  LCCEnergy_CumBCC  = LCCEnergy_CumBCC,
                  LCCNonBurn_CumCO2C = LCCNonBurn_CumCO2C,
                  
                  TotalEnergy_CumCO2C = TotalEnergy_CumCO2C,
                  TotalEnergy_CumCH4C = TotalEnergy_CumCH4C,
                  TotalEnergy_CumBCC  = TotalEnergy_CumBCC,
                  
                  Wildfire_CumCO2C = Wildfire_CumCO2C,
                  Wildfire_CumCH4C = Wildfire_CumCH4C,
                  Wildfire_CumBCC  = Wildfire_CumBCC,
                  
                  Wood_CumCO2C = wood_CumCO2C,
                  Wood_CumCH4C = wood_CumCH4C,
                  
                  Eco_AnnCO2C = Eco_AnnCO2C,
                  Eco_AnnCH4C = Eco_AnnCH4C,
                  
                  ManEnergy_AnnCO2C = ManEnergy_AnnCO2C,
                  ManEnergy_AnnCH4C = ManEnergy_AnnCH4C,
                  ManEnergy_AnnBCC  = ManEnergy_AnnBCC,
                  ManFire_AnnCO2C = Manage_Fire_AnnCO2C, 
                  ManFire_AnnCH4C = Manage_Fire_AnnCH4C,
                  ManFire_AnnBCC  = Manage_Fire_AnnBCC,
                  ManNonBurn_AnnCO2C = ManNonBurn_AnnCO2C,
                  
                  LCCEnergy_AnnCO2C = LCCEnergy_AnnCO2C,
                  LCCEnergy_AnnCH4C = LCCEnergy_AnnCH4C,
                  LCCEnergy_AnnBCC  = LCCEnergy_AnnBCC,
                  LCCNonBurn_AnnCO2C = LCCNonBurn_AnnCO2C,
                  
                  TotalEnergy_AnnCO2C = TotalEnergy_AnnCO2C,
                  TotalEnergy_AnnCH4C = TotalEnergy_AnnCH4C,
                  TotalEnergy_AnnBCC  = TotalEnergy_AnnBCC,
                  
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
  
  # sum total burned CO2eq (manage energy + manage fire + lcc energy + wildfire)
  ### cumulative ###
  TotalBurn_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalBurn_CumCO2eq_all)) {
    TotalBurn_CumCO2eq_all[,i] <- new.df[["ManEnergy_CumCO2"]][,i] + new.df[["ManEnergy_CumCH4eq"]][,i] + new.df[["ManEnergy_CumBCeq"]][,i] +
    new.df[["ManFire_CumCO2"]][,i] + new.df[["ManFire_CumCH4eq"]][,i] + new.df[["ManFire_CumBCeq"]][,i] + new.df[["LCCEnergy_CumCO2"]][,i] +
    new.df[["LCCEnergy_CumCH4eq"]][,i] + new.df[["LCCEnergy_CumBCeq"]][,i] + new.df[["Wildfire_CumCO2"]][,i] + new.df[["Wildfire_CumCH4eq"]][,i] +
    new.df[["Wildfire_CumBCeq"]][,i] 
  }
  # sum total non-burned CO2eq (eco + manage + lcc)  *Wood decay not included
  TotalNonBurn_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalNonBurn_CumCO2eq_all)) {
    TotalNonBurn_CumCO2eq_all[,i] <- new.df[["Eco_CumCO2"]][,i] + new.df[["Eco_CumCH4eq"]][,i] + new.df[["ManNonBurn_CumCO2"]][,i] +
      new.df[["LCCNonBurn_CumCO2"]][,i] 
  }
  ### annual ###
  # sum total burned CO2eq (manage energy + manage fire + lcc energy + wildfire)
  TotalBurn_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalBurn_AnnCO2eq_all)) {
    TotalBurn_AnnCO2eq_all[,i] <- new.df[["ManEnergy_AnnCO2"]][,i] + new.df[["ManEnergy_AnnCH4eq"]][,i] + new.df[["ManEnergy_AnnBCeq"]][,i] +
      new.df[["ManFire_AnnCO2"]][,i] + new.df[["ManFire_AnnCH4eq"]][,i] + new.df[["ManFire_AnnBCeq"]][,i] + new.df[["LCCEnergy_AnnCO2"]][,i] +
      new.df[["LCCEnergy_AnnCH4eq"]][,i] + new.df[["LCCEnergy_AnnBCeq"]][,i] + new.df[["Wildfire_AnnCO2"]][,i] + new.df[["Wildfire_AnnCH4eq"]][,i] +
      new.df[["Wildfire_AnnBCeq"]][,i]
  }
  # sum total non-burned CO2eq (eco + manage + lcc) *Wood decay not included
  TotalNonBurn_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalNonBurn_AnnCO2eq_all)) {
    TotalNonBurn_AnnCO2eq_all[,i] <- new.df[["Eco_AnnCO2"]][,i] + new.df[["Eco_AnnCH4eq"]][,i] + new.df[["ManNonBurn_AnnCO2"]][,i] +
      new.df[["LCCNonBurn_AnnCO2"]][,i] 
  }
  
  # sum total energy CO2eq (manage energy + lcc energy) and total fire CO2eq (manage fire + wildfire) 
  ### cumulative energy ###
  TotalEnergy_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalEnergy_CumCO2eq_all)) {
    TotalEnergy_CumCO2eq_all[,i] <- new.df[["ManEnergy_CumCO2"]][,i] + new.df[["ManEnergy_CumCH4eq"]][,i] + new.df[["ManEnergy_CumBCeq"]][,i] +
      new.df[["LCCEnergy_CumCO2"]][,i] + new.df[["LCCEnergy_CumCH4eq"]][,i] + new.df[["LCCEnergy_CumBCeq"]][,i] 
  }
  ### cumulative fire ###
  TotalFire_CumCO2eq_all <- Total_CumCO2
  for (i in 5:ncol(TotalFire_CumCO2eq_all)) {
    TotalFire_CumCO2eq_all[,i] <- new.df[["ManFire_CumCO2"]][,i] + new.df[["ManFire_CumCH4eq"]][,i] + new.df[["ManFire_CumBCeq"]][,i] + 
      new.df[["Wildfire_CumCO2"]][,i] + new.df[["Wildfire_CumCH4eq"]][,i] + new.df[["Wildfire_CumBCeq"]][,i]
  }
  ### annual energy ###
  TotalEnergy_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalEnergy_AnnCO2eq_all)) {
    TotalEnergy_AnnCO2eq_all[,i] <- new.df[["ManEnergy_AnnCO2"]][,i] + new.df[["ManEnergy_AnnCH4eq"]][,i] + new.df[["ManEnergy_AnnBCeq"]][,i] +
      new.df[["LCCEnergy_AnnCO2"]][,i] + new.df[["LCCEnergy_AnnCH4eq"]][,i] + new.df[["LCCEnergy_AnnBCeq"]][,i] 
  }
  ### annual fire ###
  TotalFire_AnnCO2eq_all <- Total_AnnCO2
  for (i in 5:ncol(TotalFire_AnnCO2eq_all)) {
    TotalFire_AnnCO2eq_all[,i] <- new.df[["ManFire_AnnCO2"]][,i] + new.df[["ManFire_AnnCH4eq"]][,i] + new.df[["ManFire_AnnBCeq"]][,i] + 
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
  zero_test <- Total_CumCO2
  for (i in 5:ncol(zero_test)) {
    zero_test[,i] <- 0
  }
  for (i in 5:ncol(Total_CumCO2)) {
    zero_test[,i] <- out_atmos_df_list[["Total_Atmos_CumGain_C_stock"]][,i] - 
      (Total_CumCO2[,i] * (12.0107/44.01) + Total_CumCH4eq[,i] * (12.0107/(gwp_CH4*16.04)) + Total_CumBCeq[,i] * (0.6/gwp_BC) - 
         Eco_CumCO2C[,i] - Eco_CumCH4C[,i]) 
  } 
  # rounding error so this test is false
  all(zero_test[5:ncol(zero_test)] == 0) 
  # but this checks to be true
  all(zero_test[5:ncol(zero_test)] < 0.001 & zero_test[5:ncol(zero_test)] > -0.001) 
  
  ###############################  Calculate some changes and totals  ###############################  
  # also round everything to integer ha, MgC and MgC/ha places for realistic precision
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
  out_area_df_list[[1]][,c(5:ncol(out_area_df_list[[1]]))] = round(out_area_df_list[[1]][,c(5:ncol(out_area_df_list[[1]]))], 0)
 
  ######### (2) MANAGED & WILFIRE AREA  ######### 
  for (i in 2:num_out_area_sheets) {
    end_label = ncol(out_area_df_list[[i]])
    out_area_df_list[[i]][, "Change_ha"] = out_area_df_list[[i]][,end_label] - out_area_df_list[[i]][,start_area_label]
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
    out_area_df_list[[i]][,c(6:ncol(out_area_df_list[[i]]))] = round(out_area_df_list[[i]][,c(6:ncol(out_area_df_list[[i]]))], 0)
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
    out_density_df_list[[i]][,c(5:ncol(out_density_df_list[[i]]))] = round(out_density_df_list[[i]][,c(5:ncol(out_density_df_list[[i]]))], 0)
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
    out_stock_df_list[[i]][,c(5:ncol(out_stock_df_list[[i]]))] = round(out_stock_df_list[[i]][,c(5:ncol(out_stock_df_list[[i]]))], 0)
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
    out_wood_df_list[[i]][,c(5:ncol(out_wood_df_list[[i]]))] = round(out_wood_df_list[[i]][,c(5:ncol(out_wood_df_list[[i]]))], 0)
  }
  
  # remove the Xs added to the front of the year columns so that the following atmosphere section can work without error
  colnames_Ann = names(out_atmos_df_list[[26]])
  colnames_Cum = names(out_atmos_df_list[[20]])
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
    out_atmos_df_list[[i]][,c(5:ncol(out_atmos_df_list[[i]]))] = round(out_atmos_df_list[[i]][,c(5:ncol(out_atmos_df_list[[i]]))], 0)
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
