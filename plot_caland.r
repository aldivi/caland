# plot_caland.r

# make diagnostic plots (.pdf files) and associated text files (.csv files) of two or more CALAND scenario outputs

# this script loops over all the regions and land types
#  the ownerships will be aggregated
#  note that Seagrass has non-zero values only for all c stock == soil c stock, and cum and ann eco c gain

# get only the year columns (not the change column)

# the model output files for plotting are in caland/<data_dir>/
# the plots are put into caland/<data_dir>/<figdir>/ within each region, land type, and ownership directory
#	where <data_dir> and <figdir> are arguments to the function

# plot_caland() has 12 arguments:
#	scen_fnames		array of scenario output file names; assumed to be in data_dir; scen_lnames is automatically determined from this by extracting text before "_output_..."
#	scen_snames		array of scenario short lables associated with scen_fnames (up to 8 character labels for bar graphs)
#	data_dir		the path to the directory containing the caland output files; do not include the "/" character at the end; default is "./outputs"
#	reg				array of regions to plot; can be any number of available regions (all are below as default)
#	lt				array of land types to plot; can be any number of available types (all are below as default)
#	own				array of ownerships to plot; can be any number of available types (only "All_own" is the default)
#	figdir			the directory within data_dir to write the figures; do not include the "/" character at the end
#	INDIVIDUAL		TRUE = output per area effects of individual practices based on model runs configured for this purpose
# units_ha         TRUE = output units in "ha", FALSE = "ac". Default is "ac".
# blackC        TRUE = black GWP equal to 900, FALSE = black GWP equal to 1 (default)
# blackC_plot   TRUE = plot BC, CO2, and CH4, FALSE = plot only CO2 and CH4 (BC added to CO2 which is only valid if black C is FALSE) (default)
# last_year			the last year to plot; this should be the final run year + 1 because cumulative and area outputs reflect previous years
#						e.g., 2050 is the final run year, but everything is defined and output to 2051, so last_year = 2051

# notes:
# need at least two scenarios for this to work
# the first scenario in the array is assumed to be the baseline
# scen_fnames, scen_lnames, and scen_snames all need to be equal length vectors
# this takes several hours for 5 scenarios
# it is faster to run four separate instances at once via the command line (assuming you have at least 4 cores)
#	this took about 2.5 hours to complete
#		three instancs each with three of the land regions
#		one instance with the Ocean and All_region regions
# When using All_region and All_land, there is only the All_own ownership avaialable in the output files
#	so it is best to not use these when plotting individual ownerships
#	otherwise you may get several outputs labelled with different ownerships, but that are identical and are the All_own outputs

# the indivudual per area outputs are only valid when comparing:
#	a static run (only ecosystem exchange enabled, no lulcc, no practices, no fire (except when evaluating effects on fire emissions))
#	with a run with a single practice enabled

#### the output files do not include the carbon transfered into and out of a land category due to lcc
#### so the component diagnostics include only land-atmosphere c exchange
#### which means that the carbon going between land categories is not represented in these diagnostics

# make sure that the working directory is caland/
# This R script is in the caland directory, which should be the working directory
#	Open R by opening this R script, or set the working the directory to caland/
# setwd("<your_path>/caland/")
setwd("./")

# this enables java to use up to 4GB of memory for reading and writing excel files
options(java.parameters = "-Xmx8g" )

# Load all the required packages
libs <- c( "XLConnect", "ggplot2", "grid", "RColorBrewer", "reshape2")
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it for
        local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

# set these here so the function does not have to be used
data_dir = "./outputs/sep11_2018_nwl_v4"
# scen_fnames = c("BAU_EcoFlux_frst2Xmort_fire_output_mean_BC1_new_outputs.xls","Woodland_restoration_frst2Xmort_fire_output_mean.xls") 
scen_fnames = c("BAU_EcoFlux_frst2Xmort_fire_output_mean_BC1_new_outputs.xls","BAU_All_frst2Xmort_fire_output_mean_BC1_new_outputs.xls")
scen_snames = c("BAUEco","BAUall")
#scen_snames = c("BAUEco","WoodRest")
scen_fnames = c("BAU_Fire_frst2Xmort_fire_output_mean_BC1.xls","USFS_partial_cut_frst2Xmort_fire_output_mean_BC1.xls") 
scen_snames = c("BAUFire","USFSPC")
lt=c("All_land")
own=c("All_own")
units_ha = FALSE
reg=c("All_region")

reg="All_region"
lt="All_land"
own = "All_own"

figdir = "figures"
INDIVIDUAL = TRUE
blackC = FALSE
blackC_plot = FALSE

last_year = 2051

#reg = c("All_region", "Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "Ocean")
#lt = c("All_land", "Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass")
#own = c("All_own", "BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")



scen_fnames = c("Baseline_frst2Xmort_fire_output_mean.xls", "LowProtect_BaseManage_frst2Xmort_fire_output_mean.xls",
                "HighProtect_BaseManage_frst2Xmort_fire_output_mean.xls", "BaseProtect_LowManage_frst2Xmort_fire_output_mean.xls",
                "BaseProtect_HighManage_frst2Xmort_fire_output_mean.xls")
scen_snames = c("BASE", "LPBM", "HPBM", "BPLM", "BPHM")

scen_fnames = c("Historical_frst2Xmort_fire_output_mean_BC1.xls", "BAU_NWL_frst2Xmort_fire_output_mean_BC1.xls", "Ambitious_frst2Xmort_fire_output_mean_BC1.xls")
scen_snames = c("HIST", "BAU", "AMB")

############# main function

plot_caland <- function(scen_fnames, scen_snames, data_dir = "./outputs", reg = c("All_region", "Central_Coast", "Central_Valley",
"Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "Ocean"),
lt = c("All_land", "Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest",
"Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated",  "Developed_all", "Seagrass"),
own = c("All_own"), figdir = "figures", INDIVIDUAL = FALSE, units_ha=FALSE, blackC = FALSE, blackC_plot = FALSE, last_year = 2051) {
    
    cat("Start plot_caland() at", date(), "\n")
    
    # CALAND starts in 2010 and its outputs are referenced to that so plots must start in this year
    start_year = 2010
    
  	# first check to make sure that the combination of blackC and backC_plot are valid 
  	if (blackC == TRUE & blackC_plot == FALSE) {
    	cat( "Invalid settings for black carbon\n" )
    	stop( "If black carbon GWP was computed as 900 (blackC==TRUE), it must be plotted as black carbon (blackC_plot==TRUE)\n" )
  	}
  
    outputdir = paste0(data_dir, "/")
    num_scen_names = length(scen_fnames)
    num_lt = length(lt)
    num_reg = length(reg)
    num_own = length(own)
    
    # create the long scenario names for the outfiles from the input file names
    # just drop the .xls because some output file distinctions are at the end
    scen_lnames = substr(scen_fnames, 1, regexpr(".xls", scen_fnames)-1)
    
    theme_set( theme_bw() )
    
    FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)
    
    # Mg C to Million Metric tons C
    Mg2MMT = 1 / 1000000
    
    # ha (or ac) to Thousand ha
    ha2kha = 1 / 1000
    
    # ha to ac
    ha2ac = 2.47105
    
    c_lab = "MMT C"
    ha_lab = "kha"
    ac_lab = "kac"
    dh_lab = "MgC/ha"
    da_lab = "MgC/ac"
    g_lab = "MMT CO2-eq"
    
    # these are the sheets for plotting C summary data
    stock_sheets = c("All_orgC_stock", "All_biomass_C_stock", "Above_main_C_stock", "Below_main_C_stock", "Understory_C_stock", "StandDead_C_stock",
    "DownDead_C_stock", "Litter_C_stock","Soil_orgC_stock", "Total_Wood_C_stock")
    num_stock_sheets = length(stock_sheets)
    
    ann_sheets = c("Total_Wood_AnnGain_C_stock", "Eco_AnnGain_C_stock", "Total_Atmos_AnnGain_C_stock", "Manage_Atmos_AnnGain_C_stock",
    "Fire_Atmos_AnnGain_C_stock", "LCC_Atmos_AnnGain_C_stock", "Wood_Atmos_AnnGain_C_stock", "Man_Atmos_AnnGain_Harv2EnergyC", "Man_Atmos_AnnGain_Slash2EnergyC")
    num_ann_sheets = length(ann_sheets)
    # use only the first 7 for bar graphs
    num_ann_comp_sheets = 7
    
    cum_sheets = c("Total_Wood_CumGain_C_stock", "Eco_CumGain_C_stock", "Total_Atmos_CumGain_C_stock", "Manage_Atmos_CumGain_C_stock",
    "Fire_Atmos_CumGain_C_stock", "LCC_Atmos_CumGain_C_stock", "Wood_Atmos_CumGain_C_stock", "Man_Atmos_CumGain_Harv2EnergyC", "Man_Atmos_CumGain_Slash2EnergyC")
    num_cum_sheets = length(cum_sheets)
    # use only the first 7 for bar graphs
    num_cum_comp_sheets = 7
    
    area_sheets = c("Area", "Managed_area", "Wildfire_area")
    num_area_sheets = length(area_sheets)
    
    den_sheets = c("All_orgC_den", "All_biomass_C_den", "Above_main_C_den", "Below_main_C_den", "Understory_C_den", "StandDead_C_den",
    "DownDead_C_den", "Litter_C_den", "Soil_orgC_den")
    num_den_sheets = length(den_sheets)
    
    bar_plot_labels = c("Wood_Gain_from_Eco", "Ecosystem_Gain_from_Atmos", "Total_Atmosphere_Gain", "Loss_to_Atmos_from_Manage",
    "Loss_to_Atmos_from_Fire", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Wood")
    
    # these are the sheets for reading/plotting GHG summary data
    # do line plots only for the totals
    # do stacked plots for the components by scenario
    ann_ghg_sheets = c("Total_AnnCO2eq_all", "TotalEnergy_AnnCO2eq_all", "TotalFire_AnnCO2eq_all", "TotalNonBurn_AnnCO2eq_all",
    "TotalWood_AnnCO2eq_all", "Total_AnnCO2", "Total_AnnCH4eq", "Total_AnnBCeq", "Wood_AnnCO2", "Wildfire_AnnCO2",
    "ManTotEnergy_AnnCO2", "LCCTotEnergy_AnnCO2", "Eco_AnnCO2", "ManFire_AnnCO2", "ManNonBurn_AnnCO2", "LCCFire_AnnCO2",
    "LCCNonBurn_AnnCO2", "Wood_AnnCH4eq", "Wildfire_AnnCH4eq", "ManTotEnergy_AnnCH4eq", "LCCTotEnergy_AnnCH4eq",
    "Eco_AnnCH4eq", "ManFire_AnnCH4eq", "LCCFire_AnnCH4eq","Wildfire_AnnBCeq", "ManTotEnergy_AnnBCeq", "LCCTotEnergy_AnnBCeq",
    "ManFire_AnnBCeq", "LCCFire_AnnBCeq")
    num_ann_ghg_sheets = length(ann_ghg_sheets)
    
    # The numbers below denote that the first 8 names in ann_ghg_sheets are totals for one type of plotting, and that 6-8 are the totals for
    # the three species.
    # So in this case the order of names in the sheets does matter.
    num_plot_ann_ghg_sheets = 8
    start_spec_ann = 6
    end_spec_ann = 8
    
    cum_ghg_sheets = c("Total_CumCO2eq_all", "TotalEnergy_CumCO2eq_all", "TotalFire_CumCO2eq_all", "TotalNonBurn_CumCO2eq_all",
    "TotalWood_CumCO2eq_all", "Total_CumCO2", "Total_CumCH4eq", "Total_CumBCeq", "Wood_CumCO2", "Wildfire_CumCO2",
    "ManTotEnergy_CumCO2", "LCCTotEnergy_CumCO2", "Eco_CumCO2", "ManFire_CumCO2", "ManNonBurn_CumCO2", "LCCFire_CumCO2",
    "LCCNonBurn_CumCO2", "Wood_CumCH4eq", "Wildfire_CumCH4eq", "ManTotEnergy_CumCH4eq", "LCCTotEnergy_CumCH4eq",
    "Eco_CumCH4eq", "ManFire_CumCH4eq", "LCCFire_CumCH4eq", "Wildfire_CumBCeq", "ManTotEnergy_CumBCeq", "LCCTotEnergy_CumBCeq",
    "ManFire_CumBCeq", "LCCFire_CumBCeq")
    num_cum_ghg_sheets = length(cum_ghg_sheets)
    num_plot_cum_ghg_sheets = 8
    start_spec_cum = 6
    end_spec_cum = 8
    
    # loop over the regions
    for (r in 1:num_reg) {
        
        reg_lab = reg[r]
        
        # loop over the land types
        for (l in 1:num_lt) {
            
            lt_lab = lt[l]
            
            # land regions do not have seagrass
            # All_region has all land types and seagrass
            # The Ocean region has only the Seagrass land type
            
            if ((reg_lab != "All_region" & reg_lab != "Ocean" & lt_lab != "Seagrass") | (reg_lab == "All_region")  |
            (reg_lab == "Ocean" & lt_lab == "Seagrass")) {
                
                for ( o in 1:num_own ) {
                    
                    own_lab = own[o]
                    
                    #### no longer need to exclude any cases #####
                    # All_own is the default that sums all the ownerships, and it exists for All_region plus each individual land type
                    #  and for All_land plus each individual region
                    # the individual ownerships are plotted for individual regions and land types and for All_land and All_region
                      # if All_own or a specific region and land type (not All_region and not All_land), which includes:
                        # All_own, All_land and All_region (i.e. statewide)
                        # All_own for individual Land_Type in individual region 
                        # All_own for individual Land_Type in All_region (i.e. statewide Land_Type)
                        # All_own for All_land in individual region 
                        # individual ownership, Land_Type, and region
                        # individual ownership for All_land in individual region
                        # individual ownership for individual Land_Type in All_region
                # previously excluded statewide individual ownerships
                #    if (own_lab == "All_own" | (reg_lab != "All_region" & lt_lab != "All_land")) {
                    
                        out_dir = paste0(outputdir, figdir, "/", reg_lab, "/", lt_lab, "/", own_lab, "/")
                        
                        dir.create(out_dir, recursive=TRUE)
                        
                        # data frames for plotting
                        out_stock_df_list <- list()
                        out_ann_df_list <- list()
                        out_cum_df_list <- list()
                        out_area_df_list <- list()
                        out_den_df_list <- list()
                        out_ann_ghg_df_list <- list()
                        out_cum_ghg_df_list <- list()
                        for (i in 1:num_stock_sheets) {
                            out_stock_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        for (i in 1:num_ann_sheets) {
                            out_ann_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        for (i in 1:num_cum_sheets) {
                            out_cum_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        for (i in 1:num_area_sheets) {
                            out_area_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        for (i in 1:num_den_sheets) {
                            out_den_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        for (i in 1: num_ann_ghg_sheets) {
                            out_ann_ghg_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        for (i in 1: num_cum_ghg_sheets) {
                            out_cum_ghg_df_list[[i]] <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL,
                            Value=NULL)
                        }
                        names(out_stock_df_list) = stock_sheets
                        names(out_ann_df_list) = ann_sheets
                        names(out_cum_df_list) = cum_sheets
                        names(out_area_df_list) = area_sheets
                        names(out_den_df_list) = den_sheets
                        names(out_ann_ghg_df_list) = ann_ghg_sheets
                        names(out_cum_ghg_df_list) = cum_ghg_sheets
                        cum_comp_df <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Component=NULL, Units=NULL, Year=NULL,
                        Value=NULL)
                        ann_comp_df <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Component=NULL, Units=NULL, Year=NULL,
                        Value=NULL)
                        #dev_man_data_df = NULL
                        man_df = NULL
                        al_area_df = NULL
                        
                        # loop over the scenario outputs to read them in and put the data into data frames for plotting
                        # aggregate the ownserhips to the land type
                        # only get the year columns (not the change column)
                        
                        for(s in 1:num_scen_names) {
                            ann_ghg_comp_df <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL, Value=NULL,
                            Diff=NULL, Component = NULL)
                            cum_ghg_comp_df <- data.frame(Scenario=NULL, Region=NULL, Land_Type=NULL, Ownership=NULL, Units=NULL, Year=NULL, Value=NULL,
                            Diff=NULL, Component = NULL)
                            
                            # Load the data file
                            data_file = paste0(outputdir, scen_fnames[s])
                            scen_wrkbk = loadWorkbook(data_file)
                            
                            # worksheet/table names
                            scen_sheets = getSheets(scen_wrkbk)
                            num_scen_sheets = length(scen_sheets)
                            
                            # there shouldn't be any NA values in these sheets, so the data types should convert properly
                            # Load the worksheets into a list of data frames
                            scen_df_list <- list()
                            for (i in 1:num_scen_sheets) {
                                scen_df_list[[i]] <- readWorksheet(scen_wrkbk, i, startRow = 1)
                                
                                # remove the Xs added to the front of the year columns, and get the years as numbers only
                                yinds = which(substr(names(scen_df_list[[i]]),1,1) == "X")
                                names(scen_df_list[[i]])[yinds] = substr(names(scen_df_list[[i]]),2,5)[yinds]
                                
                                # remove the years that are not to be plotted
                                #  need to keep the extra change column because it is removed throughout the code
                                #  i don't think it is used at all
                                remove_cols = which( as.integer(names(scen_df_list[[i]])) > last_year )
                                if (length(remove_cols) > 0) {
                                	scen_df_list[[i]] = scen_df_list[[i]][,-remove_cols]
                                }
                                
                                # convert the 3 area sheets to from ha to ac if units_ha == FALSE
                                if (scen_sheets[i] %in% area_sheets & units_ha == FALSE) {
                                  if (i==1) { 
                                  scen_df_list[[i]][,5:(ncol(scen_df_list[[i]]))] <- scen_df_list[[i]][,5:(ncol(scen_df_list[[i]]))] * ha2ac
                                  } else {
                                    scen_df_list[[i]][,6:(ncol(scen_df_list[[i]]))] <- scen_df_list[[i]][,6:(ncol(scen_df_list[[i]]))] * ha2ac
                                  }
                                }
                                
                                ######### get the stock data
                                if (scen_sheets[i] %in% stock_sheets) {
                                    oind = which(stock_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # extract and convert the values for this region (including All_region)
                                    
                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                      # 3) All_own & All_land, 4) All_own & All_region
                                    if (own_lab == "All_own") {
                                      ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab)
                                      if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                        startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                            # aggregate the ownerships across the specific region and landtype
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                       ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                        reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                            # get the correct single summary row from outputs (3 types possible)
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                            # Otherwise this land type does not exist in this region
                                            val_col = 0
                                          }   
                                    } else { # end All_own 
                                    ###### Extract individual ownership cases ######
                                    # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                      ### (1) get landcat if it exists ###
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                        lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                            # (This can include Seagrass)
                                          val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] ==
                                            own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else { 
                                      ### (2) get individual statewide ownership ###
                                          # aggregate individual ownership across regions and Land_Types
                                          if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                        scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                        startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                            # sum specific ownership data across all Land_Types and regions except Seagrass
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                              startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                          } else { 
                                      ### (3) get individual statewide ownership-Land_Type ###
                                            # aggregate current ownership & Land_Type across regions
                                            if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                        scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                                        startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                              # sum specific ownership data across all regions for specific Land_Type (This can extract Seagrass: All_region, Other_Fed, Seagrass)
                                              val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                  scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                           } else { # end statewide Land_Type-ownership level
                                      ### (4) get regional individual ownership ###
                                            # aggregate specific ownership within specific region across all landtypes
                                            if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                        scen_df_list[[i]][,"Region"] == reg_lab &
                                                                                                                        scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                        startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                              # sum data across Land_Types in current ownership and region excluding Ocean/Seagrass
                                              val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                  scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                          } else { 
                                            # ownership does not exist in this land type and region
                                            val_col = 0
                                            } # end individual ownership does not exist for this land type and region
                                             } # end regional individual ownership
                                            } # end individual statewide ownership-Land_Type
                                        } # end individual statewide ownership
                                      } # end landcat level
                                    
                                
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    unit_col = rep(c_lab, length(val_col))
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col,
                                    Year=year_col, Value=val_col)
                                    
                                    #### stock data stored here
                                    out_stock_df_list[[oind]] = rbind(out_stock_df_list[[oind]],temp_df)
                                    
                                } # end get stock data
                                
                                # get the annual flux data
                                if (scen_sheets[i] %in% ann_sheets) {
                                    oind = which(ann_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # extract and convert the values for this region
                                    if (reg_lab != "Ocean" & lt_lab != "Seagrass") {
                                        
                                        # land
                                      
                                      # For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                      # 3) All_own & All_land, 4) All_own & All_region
                                        if (own_lab == "All_own") {
                                          ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab)
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, 
                                                                       startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                              # aggregate the ownerships across the specific region and landtype
                                              val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                            ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                              # get the correct single summary row from outputs (3 types possible)
                                              val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            }
                                        } else { # end All_own 
                                          ###### Extract individual ownership cases ######
                                          # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                          # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                          ### (1) get landcat if it exists ###
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                                val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] ==
                                                own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                              ### (2) get individual statewide ownership ###
                                              # aggregate individual ownership across regions and Land_Types
                                              if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                          scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                          startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                # sum specific ownership data across all Land_Types and regions except Seagrass
                                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                    scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                  startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                              } else { 
                                                ### (3) get individual statewide ownership-Land_Type ###
                                                # aggregate current ownership & Land_Type across regions
                                                if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                       scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                                       startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                  # sum specific ownership data across all regions for specific Land_Type (This can extract Seagrass: All_region, Other_Fed, Seagrass)
                                                  val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                      scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                    startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                } else { # end statewide Land_Type-ownership level
                                                  ### (4) get regional individual ownership ###
                                                  # aggregate specific ownership within specific region across all landtypes
                                                  if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                              scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                                              scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                              startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                    # sum data across Land_Types in current ownership and region excluding Ocean/Seagrass
                                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                        scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                      scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                      startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                  } else { 
                                                    # ownership does not exist in this land type and region
                                                val_col = 0
                                                  } # end individual ownership does not exist for this land type and region
                                                } # end regional individual ownership
                                              } # end individual statewide ownership-Land_Type
                                            } # end individual statewide ownership
                                        } # end landcat level
                                        
                                        scen_col = rep(scen_lnames[s], length(val_col))
                                        reg_col = rep(reg_lab, length(val_col))
                                        lt_col = rep(lt_lab, length(val_col))
                                        own_col = rep(own_lab, length(val_col))
                                        unit_col = rep(c_lab, length(val_col))
                                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                        temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col,
                                        Year=year_col, Value=val_col)
                                        
                                        #### annual flux data stored here
                                        out_ann_df_list[[oind]] = rbind(out_ann_df_list[[oind]],temp_df)
                                        
                                        # bar graph of annual components
                                        # not include the total atmos
                                        # all are negative except eco, which can be positive or negative
                                        # get the net wood c stock rather than the gross gain
                                        #  convert this net wood gain to positive so it represents c retention with eco
                                        scen_col2 = rep(scen_snames[s], length(val_col))
                                        if (ann_sheets[oind] != "Total_Atmos_AnnGain_C_stock" & ann_sheets[oind] != "Man_Atmos_AnnGain_Harv2EnergyC" &
                                        ann_sheets[oind] != "Man_Atmos_AnnGain_Slash2EnergyC") { 
                                            ann_comp_col = rep(bar_plot_labels[oind], length(val_col))
                                            if (ann_sheets[oind] != "Eco_AnnGain_C_stock") {
                                                if (ann_sheets[oind] == "Total_Wood_AnnGain_C_stock") {
                                                    # subtract the loss to atmosphere
                                                    lind = which(scen_sheets == "Wood_Atmos_AnnGain_C_stock")
                                                    temp_df <- readWorksheet(scen_wrkbk, lind, startRow = 1)
                                                    # remove the Xs added to the front of the year columns, and get the years as numbers only
                                                    yinds = which(substr(names(temp_df),1,1) == "X")
                                                    names(temp_df)[yinds] = substr(names(temp_df),2,5)[yinds]
                                                    # remove the years that are not to be plotted
                                					#  need to keep the extra change column because it is removed throughout the code
                                					#  i don't think it is used at all
                                					remove_cols = which( as.integer(names(temp_df)) > last_year )
                                					if (length(remove_cols) > 0) {
                                						temp_df = temp_df[,-remove_cols]
                                					}
                                                    
                                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                                    # 3) All_own & All_land, 4) All_own & All_region
                                                    if (own_lab == "All_own") {
                                                      ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab) 
                                                        if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                        startcol:(ncol(temp_df)-1)]) > 1) {
                                                            # aggregate the ownerships across the specific region and landtype
                                                            val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == 
                                                                                                      lt_lab & temp_df[,"Region"] ==
                                                            reg_lab, startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                      ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                                        } else if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                        startcol:(ncol(temp_df)-1)]) == 1) {
                                                          # get the correct single summary row from outputs (3 types possible)
                                                          val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                            startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                          # Otherwise this land type does not exist in this region
                                                            val_col = 0
                                                        }
                                                    } else { # end All_own 
                                                      ###### Extract individual ownership cases ######
                                                      # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                                      ### (1) get landcat if it exists ###
                                                        if (nrow(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] ==
                                                        reg_lab, startcol:(ncol(temp_df)-1)]) == 1) {
                                                            val_col = Mg2MMT * unlist(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] ==
                                                            lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                          ### (2) get individual statewide ownership ###
                                                          # aggregate individual ownership across regions and Land_Types
                                                          if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(temp_df[temp_df[,"Ownership"] == own_lab &
                                                                                                                            temp_df[,"Region"] != "Ocean", 
                                                                                                                            startcol:(ncol(temp_df)-1)]) >=1) {
                                                            # sum specific ownership data across all Land_Types and regions
                                                            val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Ownership"] == own_lab &
                                                                                                      temp_df[,"Region"] != "Ocean", 
                                                                                                              startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                          } else { 
                                                            ### (3) get individual statewide ownership-Land_Type ###
                                                            # aggregate current ownership & Land_Type across regions
                                                            if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                                   temp_df[,"Land_Type"] == lt_lab, 
                                                                                                                                   startcol:(ncol(temp_df)-1)]) >=1) {
                                                              # sum specific ownership data across all regions for specific Land_Type
                                                              val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                  temp_df[,"Land_Type"] == lt_lab, 
                                                                                                                startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                            } else { # end statewide Land_Type-ownership level
                                                              ### (4) get regional individual ownership ###
                                                              # aggregate specific ownership within specific region across all landtypes
                                                              if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                                     temp_df[,"Region"] == reg_lab &
                                                                                                                                temp_df[,"Region"] != "Ocean", 
                                                                                                                                     startcol:(ncol(temp_df)-1)]) >=1) {
                                                                # sum data across Land_Types in current ownership and region
                                                                val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                    temp_df[,"Region"] == reg_lab &
                                                                                                          temp_df[,"Region"] != "Ocean",
                                                                                                                  startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                              } else { 
                                                                # ownership does not exist in this land type and region
                                                            val_col = 0
                                                              } # end individual ownership does not exist for this land type and region
                                                            } # end regional individual ownership
                                                          } # end individual statewide ownership-Land_Type
                                                        } # end individual statewide ownership
                                                    } # end landcat level
                                                    
                                                    ann_comp_col = rep("Net_Wood_Gain", length(val_col))
                                                } # end if calc net wood stock gain
                                                temp_df = data.frame(Scenario=scen_col2, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Component=ann_comp_col,
                                                Units=unit_col, Year=year_col, Value=-val_col)
                                            } else { # end if not eco gain
                                                temp_df = data.frame(Scenario=scen_col2, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Component=ann_comp_col,
                                                Units=unit_col, Year=year_col, Value=val_col)
                                            } # end else eco gain
                                            ann_comp_df = rbind(ann_comp_df, temp_df)
                                        } # end if not total atmos gain
                                        
                                    } else {
                                        # do seagrass only for ocean and all region
                                        if (lt_lab == "Seagrass") {
                                            
                                            # all own
                                            if (own_lab == "All_own") {
                                              # >1 row of Seagrass and All_own would never exist in outputs
                                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                    lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                    startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                    # 1 row of Seagrass, All_region and All_own exists 
                                                } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                                    val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                    lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                    startcol:(ncol(scen_df_list[[i]])-1)])
                                                } else {
                                                    # this land type does not exist in this region
                                                    val_col = 0
                                                }
                                            } else { # single ownership
                                                # only one or zero rows of this ownership in this land type and region
                                                # get landcat: Ocean, Seagrass, Other_fed
                                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                                    val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] ==
                                                    own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                    lt_lab & scen_df_list[[i]][,"Region"] ==
                                                    reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                                } else {
                                                    # this ownership does not exist in this land type and region
                                                    val_col = 0
                                                }
                                            } # end else extract a single ownership
                                            
                                            scen_col = rep(scen_lnames[s], length(val_col))
                                            reg_col = rep(reg_lab, length(val_col))
                                            lt_col = rep(lt_lab, length(val_col))
                                            own_col = rep(own_lab, length(val_col))
                                            unit_col = rep(c_lab, length(val_col))
                                            year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                            temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col,
                                            Year=year_col, Value=val_col)
                                            out_ann_df_list[[oind]] = rbind(out_ann_df_list[[oind]],temp_df)
                                        } # end if region ocean and seagrass lt
                                    } # end else ocean or seagrass
                                    
                                } # end get annual data
                                
                                # get the cumulative data
                                if (scen_sheets[i] %in% cum_sheets) {
                                    oind = which(cum_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # extract and convert the values for this region
                                    if (reg_lab != "Ocean" & lt_lab != "Seagrass") {
                                        
                                        # land
                                        
                                      #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                      # 3) All_own & All_land, 4) All_own & All_region
                                        if (own_lab == "All_own") {
                                          ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab)
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                              # aggregate the ownerships across the specific region and landtype  
                                              val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                           ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                              # get the correct single summary row from outputs (3 types possible)  
                                              val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            }
                                        } else { # end All_own 
                                          ###### Extract individual ownership cases ######
                                          # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                          # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                          ### (1) get landcat if it exists ###
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                                val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] ==
                                                own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                          ### (2) get individual statewide ownership ###
                                              # aggregate individual ownership across regions and Land_Types
                                              if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                          scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                          startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                # sum specific ownership data across all Land_Types and regions
                                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                                    scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                  startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                              } else { 
                                                ### (3) get individual statewide ownership-Land_Type ###
                                                # aggregate current ownership & Land_Type across regions
                                                if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                            scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                                            startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                  # sum specific ownership data across all regions for specific Land_Type
                                                  val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                      scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                    startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                } else { # end statewide Land_Type-ownership level
                                                  ### (4) get regional individual ownership ###
                                                  # aggregate specific ownership within specific region across all landtypes
                                                  if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                         scen_df_list[[i]][,"Region"] == reg_lab &
                                                                                                                         scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                         startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                    # sum specific ownership and region across Land_Types 
                                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                        scen_df_list[[i]][,"Region"] == reg_lab &
                                                                                                        scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                      startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                  } else { 
                                                    # ownership does not exist in this land type and region
                                                val_col = 0
                                                  } # end individual ownership does not exist for this land type and region
                                                } # end regional individual ownership
                                              } # end individual statewide ownership-Land_Type
                                            } # end individual statewide ownership
                                        } # end landcat level
                                        
                                        scen_col = rep(scen_lnames[s], length(val_col))
                                        reg_col = rep(reg_lab, length(val_col))
                                        lt_col = rep(lt_lab, length(val_col))
                                        own_col = rep(own_lab, length(val_col))
                                        unit_col = rep(c_lab, length(val_col))
                                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                        temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                        Value=val_col)
                                        out_cum_df_list[[oind]] = rbind(out_cum_df_list[[oind]],temp_df)
                                        
                                        # bar graph of cumulative components
                                        # not include the total atmos
                                        # all are negative except eco, which can be positive or negative
                                        # get the net wood c stock rather than the gross gain
                                        #  convert this net wood gain to positive so it represents c retention with eco
                                        scen_col2 = rep(scen_snames[s], length(val_col))
                                        if (cum_sheets[oind] != "Total_Atmos_CumGain_C_stock" & cum_sheets[oind] != "Man_Atmos_CumGain_Harv2EnergyC" &
                                        cum_sheets[oind] != "Man_Atmos_CumGain_Slash2EnergyC") {
                                            cum_comp_col = rep(bar_plot_labels[oind], length(val_col))
                                            if (cum_sheets[oind] != "Eco_CumGain_C_stock") {
                                                if (cum_sheets[oind] == "Total_Wood_CumGain_C_stock") {
                                                    # subtract the loss to atmosphere
                                                    lind = which(scen_sheets == "Wood_Atmos_CumGain_C_stock")
                                                    temp_df <- readWorksheet(scen_wrkbk, lind, startRow = 1)
                                                    # remove the Xs added to the front of the year columns, and get the years as numbers only
                                                    yinds = which(substr(names(temp_df),1,1) == "X")
                                                    names(temp_df)[yinds] = substr(names(temp_df),2,5)[yinds]
                                                    # remove the years that are not to be plotted
                                					#  need to keep the extra change column because it is removed throughout the code
                                					#  i don't think it is used at all
                                					remove_cols = which( as.integer(names(temp_df)) > last_year )
                                					if (length(remove_cols) > 0) {
                                						temp_df = temp_df[,-remove_cols]
                                					}
                                                    
                                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                                    # 3) All_own & All_land, 4) All_own & All_region 
                                                    if (own_lab == "All_own") {
                                                      ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab)
                                                        if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, 
                                                                         startcol:(ncol(temp_df)-1)]) > 1) {
                                                          # aggregate the ownerships across the specific region and landtype
                                                            val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                            startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                        } else if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, 
                                                                                startcol:(ncol(temp_df)-1)]) == 1) {
                                                          ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                                            val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                            startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                            # this land type does not exist in this region
                                                            val_col = 0
                                                        }
                                                    } else { # end All_own 
                                                      ###### Extract individual ownership cases ######
                                                      # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate), (3) single Land_Type & All_region 
                                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                                      ### (1) get landcat if it exists ###
                                                      if (nrow(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] ==
                                                                       reg_lab, startcol:(ncol(temp_df)-1)]) == 1) {
                                                        val_col = Mg2MMT * unlist(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] ==
                                                                                            lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) - val_col
                                                      } else {
                                                        ### (2) get individual statewide ownership ###
                                                        # aggregate individual ownership across regions and Land_Types
                                                        if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                          temp_df[,"Region"] != "Ocean", 
                                                                                                                          startcol:(ncol(temp_df)-1)]) >=1) {
                                                          # sum specific ownership data across all Land_Types and regions
                                                          val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Ownership"] == own_lab &
                                                                                                    temp_df[,"Region"] != "Ocean",
                                                                                                       startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                        } else { 
                                                          ### (3) get individual statewide ownership-Land_Type ###
                                                          # aggregate current ownership & Land_Type across regions
                                                          if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                                 temp_df[,"Land_Type"] == lt_lab, 
                                                                                                                                 startcol:(ncol(temp_df)-1)]) >=1) {
                                                            # sum specific ownership and landtype across all regions 
                                                            val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                           temp_df[,"Land_Type"] == lt_lab, 
                                                                                                         startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                          } else { # end statewide Land_Type-ownership level
                                                            ### (4) get regional individual ownership ###
                                                            # aggregate specific ownership within specific region across all landtypes
                                                            if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                                              temp_df[,"Region"] == reg_lab &
                                                                                                                              temp_df[,"Region"] != "Ocean", 
                                                                                                                              startcol:(ncol(temp_df)-1)]) >=1) {
                                                              # sum specific ownership and region across Land_Types 
                                                              val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Ownership"] == own_lab & 
                                                                                                             temp_df[,"Region"] == reg_lab &
                                                                                                        temp_df[,"Region"] != "Ocean", 
                                                                                                           startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                            } else { 
                                                              # ownership does not exist in this land type and region
                                                              val_col = 0
                                                            } # end individual ownership does not exist for this land type and region
                                                          } # end regional individual ownership
                                                        } # end individual statewide ownership-Land_Type
                                                      } # end individual statewide ownership
                                                    } # end landcat level
                                                    
                                                    cum_comp_col = rep("Net_Wood_Gain", length(val_col))
                                                } # end if calc net wood stock gain
                                                temp_df = data.frame(Scenario=scen_col2, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Component=cum_comp_col,
                                                Units=unit_col, Year=year_col, Value=-val_col)
                                            } else { # end if not eco gain
                                                temp_df = data.frame(Scenario=scen_col2, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Component=cum_comp_col,
                                                Units=unit_col, Year=year_col, Value=val_col)
                                            } # end if eco gain
                                            cum_comp_df = rbind(cum_comp_df, temp_df)
                                        } # end if not total atmos gain
                                    } else {
                                        # do seagrass only for ocean and all_region
                                        if (lt_lab == "Seagrass") {
                                            
                                            # all own
                                            if (own_lab == "All_own") {
                                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                    lt_lab & scen_df_list[[i]][,"Region"] ==
                                                    reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                                    val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                                    reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                                } else {
                                                    # this land type does not exist in this region
                                                    val_col = 0
                                                }
                                            } else { # single ownership
                                                # only one or zero rows of this ownership in this land type and region
                                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                                    val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                    lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                    startcol:(ncol(scen_df_list[[i]])-1)])
                                                } else {
                                                    # this ownership does not exist in this land type and region
                                                    val_col = 0
                                                }
                                            } # end else extract a single ownership
                                            
                                            scen_col = rep(scen_lnames[s], length(val_col))
                                            reg_col = rep(reg_lab, length(val_col))
                                            lt_col = rep(lt_lab, length(val_col))
                                            own_col = rep(own_lab, length(val_col))
                                            unit_col = rep(c_lab, length(val_col))
                                            year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                            temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                            Value=val_col)
                                            out_cum_df_list[[oind]] = rbind(out_cum_df_list[[oind]],temp_df)
                                        } # if ocean region and seagrass lt
                                    } # end else ocean or seagrass
                                    
                                } # end get cumulative data
                                
                                ############### get the area data
                                # store the total area data for All_own & individual ownerships; it is needed for the density data
                                # also store the individual management areas for this reg, lt, own
                                # also store all individual land type data for this ownership and region if lt = "All_land"
                                if (scen_sheets[i] %in% area_sheets) {
                                    oind = which(area_sheets == scen_sheets[i])
                                    if (scen_sheets[i] == "Area") {
                                        startcol = 5
                                    } else {
                                        startcol = 6
                                    }
                                    
                                    # need per management area in usable data frame for per area calcs
                                    if (scen_sheets[i] == "Managed_area") {
                                    	temp_man_df = scen_df_list[[i]][, 2:(ncol(scen_df_list[[i]])-1)]
                                    	temp_man_df = temp_man_df[temp_man_df$Region != "All_region" & temp_man_df$Land_Type != "All_land" & temp_man_df$Ownership != "All_own",]
                                    	if (lt_lab == "All_land") {
                                    		temp_man_df = temp_man_df[temp_man_df$Region != "Ocean",]
                                    		if (nrow(temp_man_df) > 0) {
                                    			temp_df = aggregate(. ~ Region + Ownership + Management, data=temp_man_df[,-which(names(temp_man_df) == "Land_Type")], FUN=sum, na.rm = TRUE)
                                    			temp_df$Land_Type = "All_land"
                                    			temp_man_df = temp_df[,c("Region", "Land_Type", "Ownership", "Management", names(temp_man_df)[(startcol-1):ncol(temp_man_df)])]
                                    		}
                                    	} else {
                                    		temp_man_df = temp_man_df[temp_man_df$Land_Type == lt_lab,]
                                    	}
                                    	if (reg_lab == "All_region") {
                                    		if (nrow(temp_man_df) > 0) {
                                    			temp_df = aggregate(. ~ Land_Type + Ownership + Management, data=temp_man_df[,-which(names(temp_man_df) == "Region")], FUN=sum, na.rm = TRUE)
                                    			temp_df$Region = "All_region"
                                    			temp_man_df = temp_df[,c("Region", "Land_Type", "Ownership", "Management", names(temp_man_df)[(startcol-1):ncol(temp_man_df)])]
                                    		}
                                    	} else {
                                    		temp_man_df = temp_man_df[temp_man_df$Region == reg_lab,]
                                    	}
                                    	if (own_lab == "All_own") {
                                    		if (nrow(temp_man_df) > 0) {
                                    			temp_df = aggregate(. ~ Region + Land_Type + Management, data=temp_man_df[,-which(names(temp_man_df) == "Ownership")], FUN=sum, na.rm = TRUE)
                                    			temp_df$Ownership = "All_own"
                                    			temp_man_df = temp_df[,c("Region", "Land_Type", "Ownership", "Management", names(temp_man_df)[(startcol-1):ncol(temp_man_df)])]
                                    		}
                                    	} else {
                                    		temp_man_df = temp_man_df[temp_man_df$Ownership == own_lab,]
                                    	}
                                    	
                                    	# do not add anything to man_df if there are no data
                                    	if (nrow(temp_man_df) > 0) {
                                    		# need to reshape temp_man_df so that there is only one value column, and order it
                                        	temp_man_df = melt(temp_man_df, variable.name = "Year", id.vars = names(temp_man_df)[1:4])
                                        	names(temp_man_df)[ncol(temp_man_df)] = "Value"
                                        	if (units_ha) { temp_man_df$Units = "ha"
                                        	} else { temp_man_df$Units = "ac" }
                                        	temp_man_df$Scenario = scen_lnames[s]
                                        	temp_man_df = temp_man_df[,c("Scenario", "Region", "Land_Type", "Ownership", "Management", "Year", "Units", "Value")]
                                        	temp_man_df = na.omit(temp_man_df[order(c(temp_man_df$Region, temp_man_df$Land_Type, temp_man_df$Ownership, temp_man_df$Management, temp_man_df$Year)),])
                                        	# store all the scenarios
                                        	man_df = rbind(man_df, temp_man_df)
                                        }
                                                
                                    } # end if store management area for per area calcs
                                    
                                    # store individual land type area for this ownership and region if lt = "All_land" - for individual practice per area calcs
                                    if (lt == "All_land" & scen_sheets[i] == "Area") {
                                    	temp_area_df = scen_df_list[[i]][, 2:(ncol(scen_df_list[[i]])-1)]
                                    	temp_area_df = temp_area_df[temp_area_df$Region != "All_region" & temp_area_df$Land_Type != "All_land" &
                                    						temp_area_df$Ownership != "All_own" & temp_area_df$Region != "Ocean",]
            								if (reg_lab == "All_region") {
                                    		if (nrow(temp_area_df) > 0) {
                                    			temp_df = aggregate(. ~ Land_Type + Ownership, data= temp_area_df[,-which(names(temp_area_df) == "Region")], FUN=sum, na.rm = TRUE)
                                    			temp_df$Region = "All_region"
                                    			temp_area_df = temp_df[,c("Region", "Land_Type", "Ownership", names(temp_area_df)[(startcol-1):ncol(temp_area_df)])]
                                    		}
                                    	} else {
                                    		temp_area_df = temp_area_df[temp_area_df$Region == reg_lab,]
                                    	}      
                                    	if (own_lab == "All_own") {
                                    		if (nrow(temp_area_df) > 0) {
                                    			temp_df = aggregate(. ~ Region + Land_Type, data= temp_area_df[,-which(names(temp_area_df) == "Ownership")], FUN=sum, na.rm = TRUE)
                                    			temp_df$Ownership = "All_own"
                                    			temp_area_df = temp_df[,c("Region", "Land_Type", "Ownership", names(temp_area_df)[(startcol-1):ncol(temp_area_df)])]
                                    		}
                                    	} else {
                                    		temp_area_df = temp_area_df[temp_area_df$Ownership == own_lab,]
                                    	}        
                                    	
                                    	# do not add anything to al_area_df if there are no data
                                    	if (nrow(temp_area_df) > 0) {
                                    		# need to reshape temp_area_df so that there is only one value column, and order it
                                        	temp_area_df = melt(temp_area_df, variable.name = "Year", id.vars = names(temp_area_df)[1:3])
                                        	names(temp_area_df)[ncol(temp_area_df)] = "Value"
                                        	if (units_ha) { temp_area_df$Units = "ha"
                                        	} else { temp_area_df$Units = "ac" }
                                        	temp_area_df$Scenario = scen_lnames[s]
                                        	temp_area_df = temp_area_df[,c("Scenario", "Region", "Land_Type", "Ownership", "Year", "Units", "Value")]
                                        	temp_area_df = na.omit(temp_area_df[order(c(temp_area_df$Region, temp_area_df$Land_Type, temp_area_df$Ownership, temp_area_df$Year)),])
                                        	# store all the scenarios
                                        	al_area_df = rbind(al_area_df, temp_area_df)
                                        }
                                    	          	
                                    } # end store land type areas for All_land
                                    
                                    # each land type, including seagrass
                                    
                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                    # 3) All_own & All_land, 4) All_own & All_region
                                    if (own_lab == "All_own") {
                                        # area and wildfire get summed, non-developed manage gets summed, developed manage is assigned dead_removal and its different 
                                         # management areas stored
                                        # wildfire severity is mutually exclusive, so it can be summed across the region
                                        # developed management generally has overlapping areas, so just use dead_removal if it exists
                                        # need to grab the individual ownerships and regions, even for the "All_region" selection!
                                        # drop the change column
                                        
                                      ######## Extract All_own Land_Type (All_land or specific Land_Type) from current area sheet (Area, Managed_area, Wildfire_area) ##########
                                        # area_df will consist of either 1 row for Seagrass (because the landcat level is excluded by All_own) or multiple rows with another specific Land_Type or All_land
                                      area_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                      
                                        # for All_own in area or wilfire area sheets, or any area sheets for an individual non-Developed_all Land_Type (only one record exists)
                                        if (scen_sheets[i] != "Managed_area" | (lt_lab != "Developed_all" & lt_lab != "All_land")) {
                                           # includes Areas, Managed Areas for non-Developed_all specific Land_Types
                                            
                                            # first sum up the ownerships for the direct area values
                                            # more than one row (which means more than one ownership, and/or multiple fire severities)
                                          
                                          # each area_df may include a row for All_region and each relevant land type, and a row for All_land and each relevant region;
                                          	# with all of these having All_own ownership (and All management)
                                          # possible nrows in area_df for given lt_lab: (1) specific Land_Type (1 to multiple rows) or (2) All_land (1 to multiple rows) 
                                          
                                          ######## For Area and Wildfire_area and non-Developed_all and non-All_land Management_area ########
                                           ####### All_own ####### 
                                             ####### (1) more than one region record (e.g., Forest) #########
                                            if (nrow(area_df[area_df[,"Region"] == reg_lab, ]) > 1) {
                                              # Sum across ownerships for given Land_Type & Region 
                                                val_col = ha2kha * unlist(apply(area_df[area_df[,"Region"] == reg_lab,
                                                startcol:ncol(area_df)], 2, sum))
                                             ####### (2) single region record (e.g., Ocean-Seagrass; or All_region-All_own) #########
                                            } else if (nrow(area_df[area_df[,"Region"] == reg_lab, ]) == 1) {
                                              # Extract row for All_region or specific region 
                                                val_col = ha2kha * unlist(area_df[area_df[,"Region"] == reg_lab,
                                                startcol:ncol(area_df)])
                                            } else {
                                             ####### this land type does not exist in this region (e.g. Coastal_marsh in Eastside) #########
                                                val_col = 0
                                            }
                                            
                                          ######## store the total Area by ownership to area-weight the density data if All_own ########
                                            # need all regions if the selection is All_region
                                            # need all land types if All_land
                                            # the same extraction is done below for density, and the output tables are structured identically, so can get away with just the data
                                          
                                          ######## Area ########  
                                          ####### All_own ####### 
                                            if (scen_sheets[i] == "Area") {
                                                
                                              if (lt_lab == "All_land") {
                                                  # Assign all _inidividual_ Land_Types for specific region or All_region if All_land to temp_df
                                                    temp_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] != "All_land" & scen_df_list[[i]][,"Region"] != "All_region" &
                                                    scen_df_list[[i]][,"Region"] != "Ocean", 1:(ncol(scen_df_list[[i]])-1)]
                                                  # Otherwise assign areas for current specific Land_Type (area_df) to temp_df 
                                                } else {
                                                  temp_df = area_df
                                                  } # end All_land or individual landtype
                                              
                                              if (reg_lab == "All_region") {
                                                  # Assign all _inidividual_ regions in individual Land_Type or in All_land if All_region to area_data
                                                    area_data = temp_df[temp_df[,"Region"] != reg_lab, startcol:ncol(temp_df)]
                                                } else {
                                                  # Otherwise for current specific region, assign thaose regions to area_data
                                                    area_data = temp_df[temp_df[,"Region"] == reg_lab, startcol:ncol(temp_df)]
                                                } # end All_region or individual region
                                            } # end if storing area data for density calcs
                                            
                                        } else { # end if Area and Wildfire_area and non-Dev landtype and non-All_land Management_area
                                          
                                      ######## Extract All_own Developed_all or All_own All_land or All_region All_own All_land from Management_area sheet ########
                                            ####### Developed_all #######
                                            if (lt_lab == "Developed_all") {
                                                # val col is just dead removal
                                                # keep the managements separate for per area calcs
                                                area_df = area_df[area_df$Region != "All_region",]
                                            } else {
                                              
                                            ####### All_land Managed Areas #######
                                              
                                              # Extract individual land types and regions (non-ocean because not land) from original Managed_area sheet and assign to temp_df
                                               temp_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] != "All_land" & scen_df_list[[i]][,"Region"] != "All_region" & 
                                                                              scen_df_list[[i]][,"Region"] != "Ocean", 1:(ncol(scen_df_list[[i]])-1)]
                                              
                                                temp_df = na.omit(temp_df[order(c(temp_df$Ownership)),])
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, area_df$Management)),])
                                                # first aggregate the land types, but keep the other variables separate
                                                # this will mainly just change the Land_Type to All_land for all rows, I think
                                                #  because each management is unique to a land type
                                                  # note: This is reassigning area_df, which is currently all rows from management area output with All_land
                                                area_df = aggregate(cbind(unlist(temp_df[startcol])) ~ Region + Ownership + Management, data=temp_df, FUN=sum, na.rm = TRUE)
                                                area_df$Land_Type = "All_land"
                                                area_df$Land_Cat_dummy = -1
                                                area_df = area_df[,c(6,1,5,2,3,4)]
                                                area_df[,colnames(temp_df[startcol])] = area_df$V1
                                                area_df$V1 = NULL
                                                for (tc in (startcol+1):(ncol(temp_df))) {
                                              # Aggregate each year's management areas for individual land types within regions (non-ocean) from original Managed_area sheet (temp_df) and assign to temp_agg
                                                    temp_agg = aggregate(cbind(unlist(temp_df[tc])) ~ Region + Ownership + Management, data=temp_df, FUN=sum, na.rm = TRUE)
                                              # Assign the All-land management-specific and ownership-specific aggregated management areas to area_df
                                                    area_df[,colnames(temp_df[tc])] = temp_agg$V1
                                                }
                                                area_df = na.omit(area_df[order(c(area_df$Ownership)),])
                                                area_df = na.omit(area_df[order(c(area_df$Region, area_df$Management)),])
                                            } # end else all_land managed area
                                          
                                          ######## Managed_area sheet: Developed_all and All_land ########  THIS HAPPENS TO ALL_LAND TOO
                                            if (nrow(area_df) > 0) {
                                               
                                                 
                                                # first aggregate the ownerships
                                                # these data area used later for per area calcs
                                                dev_man_data = aggregate(cbind(unlist(area_df[startcol])) ~ Region + Land_Type + Management, data=area_df, FUN=sum, na.rm = TRUE)
                                                dev_man_data[,colnames(area_df[startcol])] = dev_man_data$V1
                                                dev_man_data$V1 = NULL
                                                for (tc in (startcol+1):(ncol(area_df))) {
                                                    temp_agg = aggregate(cbind(unlist(area_df[tc])) ~ Region + Land_Type + Management, data=area_df, FUN=sum, na.rm = TRUE)
                                                    dev_man_data[,colnames(area_df[tc])] = temp_agg$V1
                                                }
                                                
                                                # for val col, need to reshape dev_man_data so that there is only one value column, and order it ultimately by year
                                                temp_df = melt(dev_man_data, variable.name = "Year", id.vars = colnames(dev_man_data)[1:3])
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, temp_df$Land_Type, temp_df$Management, temp_df$Year)),])
                                                
                                                
                                                # for val col, if All_region need to aggregate the regions, otherwise just extract the region of interest
                                                if (reg_lab == "All_region") {
                                                    valcol_df = aggregate(value ~ Land_Type + Management + Year, data= temp_df, FUN=sum, na.rm = TRUE)
                                                    valcol_df$Region ="All_region"
                                                } else {
                                                    valcol_df = temp_df[temp_df$Region == reg_lab,]
                                                }
                                                
                                                # order the val col management area table ultimately by year
                                                valcol_df = na.omit(valcol_df[order(c(valcol_df$Region, valcol_df$Land_Type, valcol_df$Management, valcol_df$Year)),])
                                                
                                                # use this region-specific melted version for the per area calcs
                                                # store all the scenarios
                                                valcol_df = data.frame(Scenario = rep(scen_lnames[s], length(valcol_df$Year)), valcol_df, stringsAsFactors = FALSE)
                                                #dev_man_data_df = rbind(dev_man_data_df, valcol_df)
                                                # store the baseline df for Urban_forest per area calcs
                                                #if (s == 1) { base_dev_man_data_df = dev_man_data_df }
                                                
                                                # Developed_all managed area is different from the rest as it represents total areas for distinctly different things that can overlap
                                                # so set this val_col to the dead removal area
                                                # and store the individual management areas for later use by the individual practice calculations
                                                
                                                # need to get just the value column, but make sure the order is by year by sorting above
                                                deadrem = valcol_df$value[valcol_df$Management == "Dead_removal"]
                                                  
                                                  if (lt_lab == "Developed_all") {
                                                    if (length(deadrem) > 0) {
                                                      val_col = ha2kha * deadrem
                                                    } else {
                                                      val_col = 0
                                                    }
                                                  } else { 
                                                      # All_land & All_region or Specific region 
                                                      # aggregate by scenario, region (All_region vs specific reg already taken care of), year, # management, & exclude and urban_forest and Growth
                                                      df <- aggregate(value ~ Scenario+Region+Land_Type+Year, data=valcol_df[valcol_df$Management != "Urban_forest" & valcol_df$Management != "Growth",],
                                                                      FUN=sum)
                                                      val_col = ha2kha * df$value
                                                      if (length(val_col) == 0) {
                                                        val_col = 0 
                                                      }
                                                    } # end if Developed_all or All_land
                                                
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            } # end records exist or not, dev and all_land management area
                                            
                                        } # end else dev and all_land managed for all_own
                                      
                                    } else { # end All_own
                                      ###### Extract individual ownership cases ######
                                      
                                      ######## Extract individual ownership from current area sheet (Area, Managed_area, Wildfire_area) ##########
                                      # area_df will include 1 row for Seagrass if own_lab = Other_fed
                                      
                                      area_df = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                      
                                      # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                        # individual ownership for All_land & All_region, All_region or All_land must be aggregated accordingly because these records do not exist in the output file
                                        # if area, only one or zero rows of this ownership in this land type and region
                                        # managed and wildfire area can have multiple ownership rows due to management type or severity
                                        # wildfire gets summed, non-developed manage gets summed, developed manage is assigned dead_removal and its different management areas stored
                                      ####### Extract Area for all landtypes, Managed_area without Developed_all, and Wilfire_area ####### 
                                      if (scen_sheets[i] != "Managed_area" | lt_lab != "Developed_all") {
                                        
                                        # area and wildfire and non-dev managemenent, more than one row (so can be wildfire or management)
                                        # this first case is also landcat but only for Management or Wildfire
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                                   lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                          val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                              scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                                                              scen_df_list[[i]][,"Region"] == reg_lab,
                                                                                            startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                        } else { 
                                          # one row, so area or wildfire or non-dev management
                                          ### (1) get landcat if it exists (this should work for single row in Area or multiple in Wildfire or Management) ###
                                          if (lt_lab != "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                                                      scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                                                                                      scen_df_list[[i]][,"Region"] == reg_lab,
                                                                                                                      startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                            val_col = ha2kha * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                                scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                                                                scen_df_list[[i]][,"Region"] == reg_lab,
                                                                                              startcol:(ncol(scen_df_list[[i]])-1)])
                                            
                                            # I think this should be here
                                            # store area data for density calcs
                                              if (scen_sheets[i] == "Area") {
                                              	# save all the area for this land cat
                                              	area_data = area_df[area_df$Region == reg_lab & area_df$Land_Type == lt_lab, startcol:ncol(area_df)]
                                              }
                                            
                                          } else { # end landcat 
                                          ### (2) get individual statewide ownership ###
                                            # aggregate individual ownership across regions and Land_Types, excluding Seagrass
                                            if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Region"] != "Ocean" &
                                                scen_df_list[[i]][,"Ownership"] == own_lab, startcol:(ncol(scen_df_list[[i]])-1)])>=1) {
                                              # sum specific ownership data across all Land_Types and regions
                                              # exclude Growth & Urban_forest for Developed_all if on Management sheet (i=2) due their overlapping areas with Dead_removal
                                              if (i==2) {
                                                val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                    scen_df_list[[i]][,"Region"] != "Ocean" &
                                                                                                  scen_df_list[[i]]["Management"] != "Urban_forest" &
                                                                                                    scen_df_list[[i]]["Management"] != "Growth",
                                                                                                  startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                              } else { 
                                                val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                  scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                              }
                                              # store area data for density calcs
                                              if (scen_sheets[i] == "Area") {
                                              # save all the region-landtype combinations for this ownership except Ocean-Seagrass
                                              area_data = area_df[area_df$Region != "Ocean",startcol:ncol(area_df)]
                                              }
                                              
                                            } else { # end individual statewide ownership
                                          ### (3) get individual statewide ownership-Land_Type ###
                                              # aggregate current ownership & Land_Type across regions
                                              if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                          scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                                          startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                # sum specific ownership and landtype across all regions
                                                val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                    scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                  startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                
                                                # store area data for density calcs
                                                if (scen_sheets[i] == "Area") {
                                                  # save all the individual regions for this ownership-landtype combination
                                                  area_data = area_df[area_df$Land_Type == lt_lab, startcol:ncol(area_df)]
                                                }
                                                
                                              } else { # end statewide Land_Type-ownership level
                                          ### (4) get regional individual ownership ###
                                                # aggregate specific ownership and region across landtypes
                                                if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                       scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                                       scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                       startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                  # exclude Growth & Urban_forest for Developed_all if on Management sheet (i=2) due their overlapping areas with Dead_removal
                                                  if (i==2) {
                                                    val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                        scen_df_list[[i]][,"Region"] == reg_lab &
                                                                                                        scen_df_list[[i]][,"Region"] != "Ocean" &
                                                                                                        scen_df_list[[i]]["Management"] != "Urban_forest" &
                                                                                                        scen_df_list[[i]]["Management"] != "Growth",
                                                                                                      startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                  } else {
                                                    val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                        scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                        scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                      startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                                  }
                                                  
                                                  # store area data for density calcs
                                                  if (scen_sheets[i] == "Area") {
                                                    # save all the individual landtypes for this ownership-region combination 
                                                    area_data = area_df[area_df$Region == reg_lab & area_df$Region != "Ocean", startcol:ncol(area_df)]
                                                  }
                                                  
                                                } else { 
                                                  # ownership does not exist in this land type and region
                                                  val_col = 0
                                                } # end individual ownership does not exist for this land type and region
                                              } # end regional individual ownership
                                            } # end individual statewide ownership
                                          } # end not a landcat 
                                        } # end not landcat from Managed area or Wildfire
                                        } else { # end Area, Managed_area without Developed_all, and Wilfire_area
                                          
                                      ####### Get Developed_all Managed_area in individual ownership #######
                                       # check if any Developed_all exists
                                          if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 0) {
                                            ### (1) get landcat if it exists ###  
                                            # if there's a row in manage area outputs ownership- and region-specific Developed_all (All_region doesn't exist in outputs with this combo)
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                                       lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 0) {
                                              # just use dead_removal for managed, as long as it exists
                                                deadrem = unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                scen_df_list[[i]][,"Region"] == reg_lab &
                                                scen_df_list[[i]][,"Management"] == "Dead_removal",
                                                startcol:(ncol(scen_df_list[[i]])-1)])
                                                if (length(deadrem) > 0) {
                                                    val_col = ha2kha * deadrem
                                                } else { 
                                                  val_col = 0 
                                                }
                                                
                                                # store the developed_all managed areas for later use by the individual practice calcs
                                                dev_man_data = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                   scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                                                 1:(ncol(scen_df_list[[i]])-1)]
                                                
                                                # need to reshape dev_man_data so that there is only one value column, and order it ultimately by year
                                                temp_df = melt(dev_man_data, variable.name = "Year", id.vars = colnames(dev_man_data)[1:(startcol-1)])
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, temp_df$Land_Type, temp_df$Management, temp_df$Year)),])
                                                
                                              
                                          } else { # end if landcat-level Developed_all
                                          ### (2) get individual statewide ownership-specific Developed_all ###
                                            # aggregate current ownership of Developed_all across regions
                                            if (reg_lab == "All_region") {
                                              if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                         startcol:(ncol(scen_df_list[[i]])-1)]) > 0) { # checks that ownership exists for Developed_all
                                                # sum Developed_all data across all regions for current ownership 
                                                deadrem = unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                     scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                                                     scen_df_list[[i]][,"Management"] == "Dead_removal",
                                                                                   startcol:(ncol(scen_df_list[[i]])-1)],2,sum))
                                                if (length(deadrem) > 0) {
                                                  val_col = ha2kha * deadrem
                                                } else { 
                                                  val_col = 0 
                                                }
                                                
                                                # store the developed_all managed areas for later use by the individual practice calcs 
                                                  # (we don't need this for this purpose but we do need dev_man_data to create the temp_df below)
                                                dev_man_data = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                                                   scen_df_list[[i]][,"Land_Type"] == lt_lab,
                                                                                 1:(ncol(scen_df_list[[i]])-1)]
                                                
                                                # need to reshape dev_man_data so that there is only one value column, and order it ultimately by year
                                                temp_df = melt(dev_man_data, variable.name = "Year", id.vars = colnames(dev_man_data)[1:(startcol-1)])
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, temp_df$Land_Type, temp_df$Management, temp_df$Year)),])
                                                
                                                temp_df = aggregate(value ~ Land_Type + Management + Year, data= temp_df, FUN=sum, na.rm = TRUE)
                                                temp_df$Region ="All_region"
                                                
                                              } # end if the ownership for Developed_all exists
                                              
                                              } else {
                                              # if not All_region, save specific <=== don't know what this case would ever be
                                              temp_df = temp_df[temp_df$Region == reg_lab,]
                                            }
                                          }  # end get All_region, ownership-specific Developed_all
                                            
                                            # store all the scenarios
                                            temp_df = data.frame(Scenario = rep(scen_lnames[s], length(temp_df$Year)), temp_df, stringsAsFactors = FALSE)
                                            #dev_man_data_df = rbind(dev_man_data_df, temp_df)
                                            
                                          } else { # end either type of Developed_all managed areas (landcat or All_region)     
                                            # this ownership does not exist in Developed_all and region
                                            val_col = 0
                                          }
                                        } # end else extract Developed_all Managed_area
                                    } # end extract single ownership
                                    
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    if (units_ha==TRUE){
                                      unit_col = rep(ha_lab, length(val_col))
                                    } else {
                                      unit_col = rep(ac_lab, length(val_col))
                                    }
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
                                    out_area_df_list[[oind]] = rbind(out_area_df_list[[oind]],temp_df)
                                } # end get area data
                                
                                ######################## get the density data
                                # need the area data also for All_own
                                if (scen_sheets[i] %in% den_sheets) {
                                    oind = which(den_sheets == scen_sheets[i])
                                    startcol = 5
                                    # convert units from Mg/ha to Mg/ac if units_ha==FALSE
                                    if (units_ha==FALSE) {
                                      scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]]))] <- scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]]))] / ha2ac
                                    }
                                    # all own
                                    # actually need the individual region/ownerships for area weighting;
                                    #  All_own == own_lab will return only a single record with the straight sum of values across ownerships
                                    # assume the output tables are structured exactly the same, and processed the same, so only the data columns are needed and they line up
                                    # need to deal with All_land and All_region
                                    
                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                    # 3) All_own & All_land, 4) All_own & All_region
                                    
                                    if (own_lab == "All_own") {
                                      ### 2) All_own, All_land & All_region or 3) All_own & All_land ###
                                        if (lt_lab == "All_land") {
                                          # Extract all the individual landtypes (except Ocean) and regions for area weighting 
                                            den_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] != "All_land" & scen_df_list[[i]][,"Region"] != "All_region" &
                                            scen_df_list[[i]][,"Region"] != "Ocean", 1:(ncol(scen_df_list[[i]])-1)]
                                        } else {
                                      ### 1) All_own or 4) All_own & All_region ###
                                          # Extract the row(s) for the current individual landtype 
                                            den_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                        }
                                      
                                        # Extract from den_df, the region of interest (all individual regions if All_region, otherwise get specific region)
                                        if (reg_lab == "All_region") {
                                        # Extract all individual regions 
                                            den_data = den_df[den_df[,"Region"] != reg_lab, startcol:ncol(den_df)]
                                        } else {
                                        # Extract the rows for specific region
                                            den_data = den_df[den_df[,"Region"] == reg_lab, startcol:ncol(den_df)]
                                        }
                                        
                                        # if there are multiple rows
                                        if (nrow(den_data) > 1) {
                                            # need to area weight the density data if more than one record
                                            # need to catch the divide by zero
                                            
                                            temp_stock = den_data * area_data
                                            area_sum = unlist(apply(area_data, 2, sum))
                                            # calculate the are-weighted density
                                            val_col = unlist(apply(temp_stock, 2, sum)) / area_sum
                                            nan_inds = which(is.nan(val_col))
                                            inf_inds = which(val_col == Inf)
                                            neginf_inds = which(val_col == -Inf)
                                            val_col[nan_inds] = 0
                                            val_col[inf_inds] = 0
                                            val_col[neginf_inds] = 0
                                            
                                        } else if (nrow(den_data) == 1){
                                            # this is the case only a single region has only one ownership (don't need to area-weight)
                                            val_col = unlist(den_data)
                                        } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                        }
                                    } else { # single ownership
                                      
                                      ###### Extract individual ownership cases ######
                                      # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate)
                                      
                                      ### (1) get landcat if it exists ###
                                      if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                                 lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                      
                                    	den_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab & 
                                    	                             scen_df_list[[i]][,"Ownership"] == own_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                    	# don't need to area-weight
                                    	val_col = unlist(den_df[, startcol:(ncol(den_df))])
                                      } else {
                                        ### (2) get individual statewide ownership ###
                                        # aggregate individual ownership across regions and Land_Types. Exclude Ocean because not land.
                                        if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                        													scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                            1:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                          den_df = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                     1:(ncol(scen_df_list[[i]])-1)]
                                          den_data = den_df[, startcol:ncol(den_df)]
                                          # if there are multiple rows
                                          if (nrow(den_data) > 1) {
                                            # need to area weight the density data if more than one record
                                            # need to catch the divide by zero
                                            
                                            temp_stock = den_data * area_data
                                            area_sum = unlist(apply(area_data, 2, sum))
                                            val_col = unlist(apply(temp_stock, 2, sum)) / area_sum
                                            nan_inds = which(is.nan(val_col))
                                            inf_inds = which(val_col == Inf)
                                            val_col[nan_inds] = 0
                                            val_col[inf_inds] = 0
                                            
                                          } else if (nrow(den_data) == 1){
                                            # this is the case only a single region has only one ownership
                                            val_col = unlist(den_data)
                                          } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                          }
                                        } else { # end if indiviual statewide ownership
                                      ### (3) get individual statewide ownership-Land_Type ###
                                          if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                          														scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                1:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                            den_df = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                       1:(ncol(scen_df_list[[i]])-1)]
                                            den_data = den_df[, startcol:ncol(den_df)]
                                            # if there are multiple rows
                                            if (nrow(den_data) > 1) {
                                              # need to area weight the density data if more than one record
                                              # need to catch the divide by zero
                                              
                                              temp_stock = den_data * area_data
                                              area_sum = unlist(apply(area_data, 2, sum))
                                              val_col = unlist(apply(temp_stock, 2, sum)) / area_sum
                                              nan_inds = which(is.nan(val_col))
                                              inf_inds = which(val_col == Inf)
                                              val_col[nan_inds] = 0
                                              val_col[inf_inds] = 0
                                              
                                            } else if (nrow(den_data) == 1){
                                              # this is the case only a single region has only one ownership
                                              val_col = unlist(den_data)
                                            } else {
                                              # this land type does not exist in this region
                                              val_col = 0
                                            }
                                          } else { # end individual statewide ownership-Land_Type
                                       ### (4) get regional individual ownership ###
                                            if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                                        scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                        1:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                              den_df = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                           scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                         1:(ncol(scen_df_list[[i]])-1)]
                                              den_data = den_df[, startcol:ncol(den_df)]
                                              # if there are multiple rows
                                              if (nrow(den_data) > 1) {
                                                # need to area weight the density data if more than one record
                                                # need to catch the divide by zero
                                                
                                                temp_stock = den_data * area_data
                                                area_sum = unlist(apply(area_data, 2, sum))
                                                val_col = unlist(apply(temp_stock, 2, sum)) / area_sum
                                                nan_inds = which(is.nan(val_col))
                                                inf_inds = which(val_col == Inf)
                                                val_col[nan_inds] = 0
                                                val_col[inf_inds] = 0
                                                
                                              } else if (nrow(den_data) == 1){
                                                # this is the case only a single region has only one ownership
                                                val_col = unlist(den_data)
                                              } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                              }
                                            } # end if regional individual ownership 
                                          } # end else not individual statewide ownership-Land_Type
                                      } # end else not indiviual statewide ownership
                                    } # end else landcat does not exist
                                    } # end else extract single ownership
                                      
                                    
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    if (units_ha == TRUE){
                                      unit_col = rep(dh_lab, length(val_col))
                                    } else {
                                      unit_col = rep(da_lab, length(val_col))
                                    }
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
                                    
                                    #### density data stored here
                                    out_den_df_list[[oind]] = rbind(out_den_df_list[[oind]],temp_df)
                                    
                                } # end get density data
                                
                                ####### get the annual ghg data ####### 
                                if (scen_sheets[i] %in% ann_ghg_sheets) {
                                    oind = which(ann_ghg_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # if blackC was output as GWP=1 and blackC_plot is FALSE, add BC to CO2 sheets
                                    if (blackC == FALSE & blackC_plot == FALSE & scen_sheets[i] %in% c("Total_AnnCO2","Wildfire_AnnCO2", "ManTotEnergy_AnnCO2", "LCCTotEnergy_AnnCO2", 
                                                                                                       "ManFire_AnnCO2", "LCCFire_AnnCO2")) {
                                      # read the BC sheet
                                      BC_df <- readWorksheet(scen_wrkbk, i+2, startRow = 1)
                                      # add the respective BC sheet to CO2
                                      scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]])-1)] <- scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]])-1)] + BC_df[,startcol:(ncol(scen_df_list[[i]])-1)]
                                    }
                                    
                                    # do the following if plotting BC is TRUE or if it's FALSE, not on one of the BC sheets
                                    
                                    if (blackC_plot == TRUE | blackC_plot == FALSE & !(scen_sheets[i] %in% c("Total_AnnBCeq","Wildfire_AnnBCeq", "ManTotEnergy_AnnBCeq", "LCCTotEnergy_AnnBCeq", 
                                                                                     "ManFire_AnnBCeq", "LCCFire_AnnBCeq"))) { 
                                    
                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                    # 3) All_own & All_land, 4) All_own & All_region
                                    if (own_lab == "All_own") {
                                      ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab)
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                        startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                          # aggregate the ownerships across the specific region and landtype
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                      ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                        reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                          # get the correct single summary row from outputs (3 types possible)
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                        }
                                    } else { # end All_own
                                      
                                      ###### Extract individual ownership cases ######
                                      # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                      
                                      ### (1) get landcat if it exists ###
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                        lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                          # (This can include Seagrass)
                                          val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                            startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                          ### (2) get individual statewide ownership ###
                                          # aggregate individual ownership across regions and Land_Types, excluding Ocean Seagrass
                                          if (lt_lab == "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                      scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                      startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                            # sum specific ownership data across all Land_Types and regions except Seagrass
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                              startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                          } else { 
                                            ### (3) get individual statewide ownership-Land_Type ###
                                            # aggregate current ownership & Land_Type across regions
                                            if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                        scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                                        startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                              # sum specific ownership and landtype across all regions (This can extract Seagrass: All_region, Other_Fed, Seagrass)
                                              val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                  scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                            } else { # end statewide Land_Type-ownership level
                                              ### (4) get regional individual ownership ###
                                              # aggregate specific ownership and region across landtypes, excluding ocean seagrass
                                              if (lt_lab == "All_land" & reg_lab != "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                          scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                                          scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                                          startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                # sum specific ownership and region across Land_Types excluding Ocean/Seagrass
                                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                    scen_df_list[[i]][,"Region"] == reg_lab & 
                                                                                                  scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                  startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                              } else { 
                                            # this ownership does not exist in this land type and region
                                            val_col = 0
                                              } # end individual ownership does not exist for this land type and region
                                            } # end regional individual ownership
                                          } # end individual statewide ownership-Land_Type
                                        } # end individual statewide ownership
                                    } # end landcat level
                                   
                                    
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    unit_col = rep(g_lab, length(val_col))
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
                                    
                                    # annual ghg data stored here
                                    out_ann_ghg_df_list[[oind]] = rbind(out_ann_ghg_df_list[[oind]],temp_df)
                                    
                                    # fill the stacked line df for this scenario and this region/land type/ownership and the component variables
                                    # only for individual GHG species and components of totals (not totals which are 1:8 of ann_ghg_sheets)
                                    if (oind > num_plot_ann_ghg_sheets & num_plot_ann_ghg_sheets <= num_ann_ghg_sheets) {
                                        # get this scenario data - remove duplicate rows
                                        temp_df = out_ann_ghg_df_list[[oind]][out_ann_ghg_df_list[[oind]]$Scenario == scen_lnames[s] & out_ann_ghg_df_list[[oind]]$Region == reg_lab &
                                        out_ann_ghg_df_list[[oind]]$Land_Type == lt_lab & out_ann_ghg_df_list[[oind]]$Ownership ==
                                        own_lab,]
                                        temp_df = unique(temp_df)
                                        # calculate the difference from the baseline scenario
                                        temp_df$Diff = temp_df$Value - out_ann_ghg_df_list[[oind]][out_ann_ghg_df_list[[oind]]$Scenario == scen_lnames[1] &
                                        out_ann_ghg_df_list[[oind]]$Region == reg_lab &
                                        out_ann_ghg_df_list[[oind]]$Land_Type == lt_lab &
                                        out_ann_ghg_df_list[[oind]]$Ownership == own_lab, "Value"]
                                        # add the variable label
                                        temp_df$Component = ann_ghg_sheets[oind]
                                        # add this variable to the plot df
                                        ann_ghg_comp_df = rbind(ann_ghg_comp_df, temp_df)
                                    }
                                    
                                    } # end calcs on condition that not on BC sheet and want to exclude BC from plots
                                } # end get annual ghg data
                                
                                # get the cumulative ghg data
                                if (scen_sheets[i] %in% cum_ghg_sheets) {
                                    oind = which(cum_ghg_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # if blackC was output as GWP=1 and blackC_plot is FALSE, add BC to CO2 sheets
                                    if (blackC == FALSE & blackC_plot == FALSE & scen_sheets[i] %in% c("Total_CumCO2","Wildfire_CumCO2", "ManTotEnergy_CumCO2", "LCCTotEnergy_CumCO2", 
                                                                                                       "ManFire_CumCO2", "LCCFire_CumCO2")) {
                                      # read the BC sheet
                                      BC_df <- readWorksheet(scen_wrkbk, i+2, startRow = 1)
                                      # add the respective BC sheet to CO2
                                      scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]])-1)] <- scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]])-1)] + BC_df[,startcol:(ncol(scen_df_list[[i]])-1)]
                                    }
                                    
                                    # do the following if plotting BC is TRUE or if it's FALSE, not on one of the BC sheets
                                    
                                    if (blackC_plot == TRUE | blackC_plot == FALSE & !(scen_sheets[i] %in% c("Total_CumBCeq","Wildfire_CumBCeq", "ManTotEnergy_CumBCeq", "LCCTotEnergy_CumBCeq", 
                                                                                                             "ManFire_CumBCeq", "LCCFire_CumBCeq"))) { 
                                    
                                    #  For all All_own cases: 1) All_own in specific region & landtype, 2) All_own, All_land & All_region, 
                                    # 3) All_own & All_land, 4) All_own & All_region
                                    
                                    if (own_lab == "All_own") {
                                      ### 1) Extract data for All_own in specific region & landtype (only option for >1 row of reg_lab and lt_lab) 
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                        startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                          # aggregate the ownerships across the specific region and landtype
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                            startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                      ### 2) Extract data for All_own, All_land & All_region, 2) All_own & All_land, or 3) All_own & All_region
                                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                        reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                          # get the correct single summary row from outputs (3 types possible)
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                            startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                        }
                                    } else { # end extract All_own cases
                                      ###### Extract individual ownership cases ######
                                      # (1) landcat, (2) All_region & All_land (not in outputs - must aggregate; exclude seagrass because not land), (3) single Land_Type & All_region 
                                      # (not in outputs - must aggregate), and (4) All_land & single region (not in outputs - must aggregate) 
                                      ### (1) get landcat if it exists ###
                                      if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                        lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                            scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                            startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else { 
                                          ### (2) get individual statewide ownership ###
                                          # aggregate individual ownership across regions and Land_Types, except Ocean seagrass
                                          if (lt_lab == "All_land" & reg_lab == "All_region" & 
                                              nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                              					scen_df_list[[i]][,"Region"] != "Ocean", startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                            # sum ownership data across all Land_Types and regions, except seagrass
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                            												  scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                              startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                          } else { 
                                            ### (3) get individual statewide ownership-Land_Type ###
                                            # aggregate current ownership & Land_Type across regions
                                            if (lt_lab != "All_land" & reg_lab == "All_region" & nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                                        scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                                        startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                              # sum data across all regions for current ownership & Land_Type
                                              val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                  scen_df_list[[i]][,"Land_Type"] == lt_lab, 
                                                                                                startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                            } else { # end statewide Land_Type-ownership level
                                              ### (4) get regional individual ownership ###
                                              # aggregate specific ownership and region across landtypes, excluding Seagrass
                                              if (lt_lab == "All_land" & reg_lab != "All_region" & 
                                                  nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Region"] == reg_lab
                                                  						 & scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                         startcol:(ncol(scen_df_list[[i]])-1)]) >=1) {
                                                # sum data across Land_Types in current ownership and region
                                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & 
                                                                                                	scen_df_list[[i]][,"Region"] == reg_lab &
                                                                                                   	scen_df_list[[i]][,"Region"] != "Ocean", 
                                                                                                  	startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                              } else { 
                                                # ownership does not exist in this land type and region
                                                val_col = 0
                                              } # end individual ownership does not exist for this land type and region
                                            } # end else not individual statewide ownership-Land_Type
                                          } # end else not a individual statewide ownership
                                        } # end else not a landcat
                                    } # end else single ownership 
                                        
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    unit_col = rep(g_lab, length(val_col))
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
                                    #### cumulative ghg data stored here
                                    out_cum_ghg_df_list[[oind]] = rbind(out_cum_ghg_df_list[[oind]],temp_df)
                                    
                                    # fill the stacked line df for this scenario and this region/land type/ownership and the component variables
                                    if (oind > num_plot_cum_ghg_sheets & num_plot_cum_ghg_sheets <= num_cum_ghg_sheets) {
                                        # get this scenario data - remove duplicate rows
                                        temp_df = out_cum_ghg_df_list[[oind]][out_cum_ghg_df_list[[oind]]$Scenario == scen_lnames[s] &
                                        out_cum_ghg_df_list[[oind]]$Region == reg_lab &
                                        out_cum_ghg_df_list[[oind]]$Land_Type == lt_lab &
                                        out_cum_ghg_df_list[[oind]]$Ownership == own_lab,]
                                        temp_df = unique(temp_df)
                                        # calculate the difference from the baseline scenario
                                        temp_df$Diff = temp_df$Value - out_cum_ghg_df_list[[oind]][out_cum_ghg_df_list[[oind]]$Scenario == scen_lnames[1] &
                                        out_cum_ghg_df_list[[oind]]$Region == reg_lab &
                                        out_cum_ghg_df_list[[oind]]$Land_Type == lt_lab &
                                        out_cum_ghg_df_list[[oind]]$Ownership == own_lab, "Value"]
                                        # add the variable label
                                        temp_df$Component = cum_ghg_sheets[oind]
                                        # add this variable to the plot df
                                        cum_ghg_comp_df = rbind(cum_ghg_comp_df, temp_df)
                                    }
                                    } # end calcs on condition that not on BC sheet and want to exclude BC from plots
                                } # end get cumulative ghg data
                                
                            } # end for i loop over the sheets
                            
                            ##################################
                            
                            names(scen_df_list) = scen_sheets
                            
                            # plot the ghg components as stacked line graphs, by scenario and by ann/cum
                            
                            # "Wood_AnnCO2", "Wildfire_AnnCO2", "ManTotEnergy_AnnCO2", "LCCTotEnergy_AnnCO2", "Eco_AnnCO2", "ManFire_AnnCO2", "ManNonBurn_AnnCO2",
                            # "LCCNonBurn_AnnCO2", "Wood_AnnCH4eq", "Wildfire_AnnCH4eq", "ManTotEnergy_AnnCH4eq", "LCCTotEnergy_AnnCH4eq", "Eco_AnnCH4eq",
                            # "ManFire_AnnCH4eq", "Wildfire_AnnBCeq", "ManTotEnergy_AnnBCeq", "LCCTotEnergy_AnnBCeq", "ManFire_AnnBCeq"
                            
                            # annual ghg components
                            
                            acl = unique(ann_ghg_comp_df$Component)
                            acl=acl[order(acl)]
                            breaks <- acl
                            brew_ch4 <- c(brewer.pal(9, "Blues"))
                            brew_co2 <- c(brewer.pal(9, "Greys"))
                            brew_bc <- c(brewer.pal(9, "Reds"))
                            if (blackC_plot == TRUE) {
                            ghg_colors = c(Eco_AnnCH4eq  = brew_ch4[2], Eco_AnnCO2  = brew_co2[2], LCCTotEnergy_AnnBCeq = brew_bc[2],
                            LCCTotEnergy_AnnCH4eq = brew_ch4[4], LCCTotEnergy_AnnCO2 = brew_co2[3], LCCNonBurn_AnnCO2 = brew_co2[4],
                            ManTotEnergy_AnnBCeq = brew_bc[4], ManTotEnergy_AnnCH4eq = brew_ch4[6], ManTotEnergy_AnnCO2 = brew_co2[5],
                            ManFire_AnnBCeq = brew_bc[6], ManFire_AnnCH4eq = brew_ch4[8], ManFire_AnnCO2 = brew_co2[6],
                            ManNonBurn_AnnCO2 = brew_co2[7], Wildfire_AnnBCeq = brew_bc[8], Wildfire_AnnCH4eq = brew_ch4[9],
                            Wildfire_AnnCO2 = brew_co2[8], Wood_AnnCH4eq = brew_ch4[9], Wood_AnnCO2 = brew_co2[9]) 
                            } else {
                            		brew_ch4 <- c(brewer.pal(9, "Blues"))
                            	brew_co2 <- c(brewer.pal(9, "PRGn"))
                              ghg_colors = c(Eco_AnnCH4eq  = brew_ch4[2], Eco_AnnCO2  = brew_co2[2], 
                                             LCCTotEnergy_AnnCH4eq = brew_ch4[4], LCCTotEnergy_AnnCO2 = brew_co2[3], LCCNonBurn_AnnCO2 = brew_co2[4],
                                             ManTotEnergy_AnnCH4eq = brew_ch4[6], ManTotEnergy_AnnCO2 = brew_co2[5],
                                             ManFire_AnnCH4eq = brew_ch4[8], ManFire_AnnCO2 = brew_co2[6],
                                             ManNonBurn_AnnCO2 = brew_co2[7], Wildfire_AnnCH4eq = brew_ch4[9],
                                             Wildfire_AnnCO2 = brew_co2[8], Wood_AnnCH4eq = brew_ch4[9], Wood_AnnCO2 = brew_co2[9]) 
                            }
                  
                            #ghg_colors = c(brew_ch4[2], brew_co2[2], brew_bc[2], brew_ch4[4], brew_co2[3], brew_co2[4], brew_bc[4], brew_ch4[6],
                            # brew_co2[5], brew_bc[6], brew_ch4[8], brew_co2[6], brew_co2[7], brew_bc[8], brew_ch4[9], brew_co2[8], brew_ch4[9],
                            # brew_co2[9])
                            ann_ghg_comp_df = na.omit(ann_ghg_comp_df[order(c(ann_ghg_comp_df$Component, ann_ghg_comp_df$Year)),])
                            
                            # absolute values
                            p <- ( ggplot(ann_ghg_comp_df,aes(x=Year))
                            + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Value > 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
                            + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Value < 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
                            + geom_hline(yintercept=0)
                            + ylab("Absolute (MMT CO2-eq per year)")
                            + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to net annual GWP"))
                            + scale_fill_manual(values = ghg_colors, breaks = breaks)
                            )
                            #print(p)
                            
                            p$save_args <- FIGURE_DIMS
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_ann_comp_output.pdf")
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_ann_comp_output.csv")
                            write.csv(ann_ghg_comp_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # difference from baseline
                            if (s > 1) {
                                
                                p <- ( ggplot(ann_ghg_comp_df, aes(x=Year))
                                + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Diff > 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
                                + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Diff < 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
                                + geom_hline(yintercept=0)
                                + ylab("Change from Baseline (MMT CO2-eq per year)")
                                + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to change in net annual GWP"))
                                + scale_fill_manual(values = ghg_colors, breaks = breaks)
                                )
                                #print(p)
                                
                                p$save_args <- FIGURE_DIMS
                                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_ann_comp_diff_output.pdf")
                                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                                
                                # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                                # only apply to individual land types ==> actually we need All_land for restoration per area benefits
                                #if (INDIVIDUAL & lt_lab != "All_land") {
                                  #if (INDIVIDUAL) {
                                  if (FALSE) {
                                	# non-dev management can use the regular management area output table
                                	#   because the management areas are mutually exclusive and they don't need special treatment
                                	
                                    # dev_man_data_df has scenario, region, management, land type, year, and value in ha
                                    # these data are specific to region, land type, and ownership (but ownership is not in the df)
                                    # extract only the current scenario here
                                    # get the managed area, and the cumulative managed area
                                    # ann_ghg_comp_df can have All_own and All_region; the managed area data have already been processed accordingly

                                    # for everything except cultivated and developed:
                                    # divide Diff by cumulative managed area because of cumulative effects on carbon accumulation
                                    # this applies for restoration activities and forest and rangeland practices
                                    if (lt_lab != "Cultivated" & lt_lab != "Developed_all") {
                                    	ann_ghg_comp_df = merge(ann_ghg_comp_df, out_area_df_list[[2]][out_area_df_list[[2]]$Scenario == scen_lnames[s],], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	ann_ghg_comp_df$CumArea = cumsum(ann_ghg_comp_df$Value.y)
                                        ann_ghg_comp_df$DiffPerArea[ann_ghg_comp_df$Land_Type != "Cultivated" & ann_ghg_comp_df$Land_Type != "Developed_all"] =
                                        ann_ghg_comp_df$Diff[ann_ghg_comp_df$Land_Type != "Cultivated" & ann_ghg_comp_df$Land_Type != "Developed_all"] /
                                        ann_ghg_comp_df$CumArea[ann_ghg_comp_df$Land_Type != "Cultivated" & ann_ghg_comp_df$Land_Type != "Developed_all"] / Mg2MMT * ha2kha
                                    } else if (lt_lab == "Cultivated") {
                                        # for cultivated, divide diff by annual managed area
                                        ann_ghg_comp_df = merge(ann_ghg_comp_df, out_area_df_list[[2]][out_area_df_list[[2]]$Scenario == scen_lnames[s],], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                        ann_ghg_comp_df$DiffPerArea[ann_ghg_comp_df$Land_Type == "Cultivated"] = 
                                        ann_ghg_comp_df$Diff[ann_ghg_comp_df$Land_Type == "Cultivated"] /
                                        ann_ghg_comp_df$Value.y[ann_ghg_comp_df$Land_Type == "Cultivated"] / Mg2MMT * ha2kha
                                    } else if (lt_lab == "Developed_all") {
                                        # for developed growth divide diff by cumulative difference in urban area between the scenario #2 and the baseline #1
                                        # for developed urban forest divide diff by cumulative difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by annual dead removal area (this does not have a long-term effect on growth rates)
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period doesn't cancel itself out

										# extract current scenario
										dev_man_scen_df = dev_man_data_df[dev_man_data_df$Scenario == scen_lnames[s],]
                                        if ( sum(dev_man_scen_df$value[dev_man_scen_df$Management == "Dead_removal"]) > 0 ) {
                                            # Dead_removal practice
                                    		ann_ghg_comp_df = merge(ann_ghg_comp_df, dev_man_scen_df[dev_man_scen_df$Management == "Dead_removal",], by = c("Region", "Land_Type", "Year"), all.x = TRUE)
                                            ann_ghg_comp_df$DiffPerArea[ann_ghg_comp_df$Land_Type == "Developed_all"] =
                                            ann_ghg_comp_df$Diff[ann_ghg_comp_df$Land_Type == "Developed_all"] /
                                            ann_ghg_comp_df$value[ann_ghg_comp_df$Land_Type == "Developed_all"] / Mg2MMT

                                        } else if ( sum(dev_man_scen_df$value[dev_man_scen_df$Management == "Growth"] ) != 0 ) {
                                            # Growth
                                            # need to cumulate before merge; these data are also specific to region, land type, ownership
                                            tot_area_scen_df = out_area_df_list[[1]][out_area_df_list[[1]]$Scenario == scen_lnames[s],]
                                            tot_area_scen_df$DiffArea = tot_area_scen_df$Value - out_area_df_list[[1]][out_area_df_list[[1]]$Scenario == scen_lnames[1],]$Value
                                            tot_area_scen_df$CumArea = cumsum(tot_area_scen_df$DiffArea)
                                            ann_ghg_comp_df = merge(ann_ghg_comp_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            ann_ghg_comp_df$DiffPerArea[ann_ghg_comp_df$Land_Type == "Developed_all"] =
                                            ann_ghg_comp_df$Diff[ann_ghg_comp_df$Land_Type == "Developed_all"] /
                                            ann_ghg_comp_df$CumArea[ann_ghg_comp_df$Land_Type == "Developed_all"] / Mg2MMT * ha2kha
                                        } else {
                                            # Urban_forest
                                            # need to cumulate before merge with variable
                                            dev_man_data_sub_df = merge(dev_man_scen_df[dev_man_scen_df$Management == "Urban_forest",], 
                                                                        dev_man_data_df[dev_man_data_df$Scenario == scen_lnames[1] & dev_man_data_df$Management == "Urban_forest",], 
                                                                        by = c("Region", "Land_Type", "Year"), all.x = TRUE)
                                            dev_man_data_sub_df$DiffArea = dev_man_data_sub_df$value.x - dev_man_data_sub_df$value.y
                                            dev_man_data_sub_df$CumArea = cumsum(dev_man_data_sub_df$DiffArea)
                                            ann_ghg_comp_df = merge(ann_ghg_comp_df, dev_man_data_sub_df, by = c("Region", "Land_Type", "Year"), all.x = TRUE)
                                            ann_ghg_comp_df$DiffPerArea[ann_ghg_comp_df$Land_Type == "Developed_all"] =
                                            ann_ghg_comp_df$Diff[ann_ghg_comp_df$Land_Type == "Developed_all"] /
                                            ann_ghg_comp_df$CumArea[ann_ghg_comp_df$Land_Type == "Developed_all"] / Mg2MMT
                                        }
                                    } # end else Developed_all
                                    
                                    # deal with NA, NaN and Inf
                                    na_inds = which(is.na(ann_ghg_comp_df$DiffPerArea))
                                    nan_inds = which(is.nan(ann_ghg_comp_df$DiffPerArea))
                                    inf_inds = which(ann_ghg_comp_df$DiffPerArea == Inf)
                                    ninf_inds = which(ann_ghg_comp_df$DiffPerArea == -Inf)
                                    ann_ghg_comp_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    
                                    if (units_ha == TRUE) {
                                      ann_ghg_comp_df$units_dpa = "MgCO2eq/ha/yr"
                                      p <- ( ggplot(ann_ghg_comp_df, aes(x=Year))
                                             + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$DiffPerArea > 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$DiffPerArea < 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_hline(yintercept=0)
                                             + ylab("Change from Baseline (Mg CO2-eq per ha per year)")
                                             + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to change in net annual GWP per man area"))
                                             + scale_fill_manual(values = ghg_colors, breaks = breaks)
                                      )
                                    #print(p)
                                    } else {
                                      ann_ghg_comp_df$units_dpa = "MgCO2eq/ac/yr"
                                      p <- ( ggplot(ann_ghg_comp_df, aes(x=Year))
                                             + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$DiffPerArea > 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$DiffPerArea < 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_hline(yintercept=0)
                                             + ylab("Change from Baseline (Mg CO2-eq per ac per year)")
                                             + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to change in net annual GWP per man area"))
                                             + scale_fill_manual(values = ghg_colors, breaks = breaks)
                                      )
                                    }
                                    
                                    p$save_args <- FIGURE_DIMS
                                    out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_ann_comp_diffperarea_output.pdf")
                                    do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                                    out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_ann_comp_diffperarea_output.csv")
                            		write.csv(ann_ghg_comp_df, out_file, quote=FALSE, row.names=FALSE)
                                } # end if annual per area effect of individual practice
                                
                            } # end if difference from baseline for annual ghg stacked components
                            
                            # cumulative ghg components
                            
                            ccl = unique(cum_ghg_comp_df$Component)
                            ccl=ccl[order(ccl)]
                            breaks <- ccl
                            brew_ch4 <- c(brewer.pal(9, "Blues"))
                            brew_co2 <- c(brewer.pal(9, "Greys"))
                            brew_bc <- c(brewer.pal(9, "Reds"))
                            
                            if (blackC_plot == TRUE) {
                            ghg_colors = c(Eco_CumCH4eq  = brew_ch4[2], Eco_CumCO2  = brew_co2[2], LCCTotEnergy_CumBCeq = brew_bc[2],
                            LCCTotEnergy_CumCH4eq = brew_ch4[4], LCCTotEnergy_CumCO2 = brew_co2[3], LCCNonBurn_CumCO2 = brew_co2[4],
                            ManTotEnergy_CumBCeq = brew_bc[4], ManTotEnergy_CumCH4eq = brew_ch4[6], ManTotEnergy_CumCO2 = brew_co2[5],
                            ManFire_CumBCeq = brew_bc[6], ManFire_CumCH4eq = brew_ch4[8], ManFire_CumCO2 = brew_co2[6],
                            ManNonBurn_CumCO2 = brew_co2[7], Wildfire_CumBCeq = brew_bc[8], Wildfire_CumCH4eq = brew_ch4[9],
                            Wildfire_CumCO2 = brew_co2[8], Wood_CumCH4eq = brew_ch4[9], Wood_CumCO2 = brew_co2[9])
                            cum_ghg_comp_df = na.omit(cum_ghg_comp_df[order(c(cum_ghg_comp_df$Component, cum_ghg_comp_df$Year)),])
                            } else {
                            		brew_ch4 <- c(brewer.pal(9, "Blues"))
                            	brew_co2 <- c(brewer.pal(9, "PRGn"))
                              ghg_colors = c(Eco_CumCH4eq  = brew_ch4[2], Eco_CumCO2  = brew_co2[2], 
                                             LCCTotEnergy_CumCH4eq = brew_ch4[4], LCCTotEnergy_CumCO2 = brew_co2[3], LCCNonBurn_CumCO2 = brew_co2[4],
                                             ManTotEnergy_CumCH4eq = brew_ch4[6], ManTotEnergy_CumCO2 = brew_co2[5],
                                             ManFire_CumCH4eq = brew_ch4[8], ManFire_CumCO2 = brew_co2[6],
                                             ManNonBurn_CumCO2 = brew_co2[7], Wildfire_CumCH4eq = brew_ch4[9],
                                             Wildfire_CumCO2 = brew_co2[8], Wood_CumCH4eq = brew_ch4[9], Wood_CumCO2 = brew_co2[9])
                              cum_ghg_comp_df = na.omit(cum_ghg_comp_df[order(c(cum_ghg_comp_df$Component, cum_ghg_comp_df$Year)),])
                            }
                            
                            # absolute values
                            p <- ( ggplot(cum_ghg_comp_df, aes(x=Year))
                            + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Value > 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
                            + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Value < 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
                            + geom_hline(yintercept=0)
                            + ylab("Absolute (MMT CO2-eq)")
                            + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to net cumulative GWP"))
                            + scale_fill_manual(values = ghg_colors, breaks = breaks)
                            )
                            #print(p)
                            
                            p$save_args <- FIGURE_DIMS
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_cum_comp_output.pdf")
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_cum_comp_output.csv")
                            write.csv(cum_ghg_comp_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # difference from baseline
                            if (s > 1) {
                                p <- ( ggplot(cum_ghg_comp_df, aes(x=Year))
                                + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Diff > 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
                                + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Diff < 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
                                + geom_hline(yintercept=0)
                                + ylab("Change from Baseline (MMT CO2-eq)")
                                + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to change in net cumulative GWP"))
                                + scale_fill_manual(values = ghg_colors, breaks = breaks)
                                )
                                #print(p)
                                
                                p$save_args <- FIGURE_DIMS
                                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_cum_comp_diff_output.pdf")
                                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                                
                                # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                                # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                                #if (INDIVIDUAL & lt_lab != "All_land") {
                                #if (INDIVIDUAL) {
                                if (FALSE) {
                                	# non-dev management can use the regular management area output table
                                	#   because the management areas are mutually exclusive and they don't need special treatment
                                	
                                    # dev_man_data_df has scenario, region, management, land type, year, and value in ha
                                    # these data are specific to current region, land type, and ownership (but ownership is not in the df)
                                    # extract the current scenario here
                                    # get the managed area, and the cumulative managed area
                                    # cum_ghg_comp_df can have All_own and All_region; the managed area data have already been processed accordingly

                                    # divide Diff by cumulative managed area
                                    # the first year of cumulative values is zero, and the last year is end_year+1
                                    
                                    first_year = cum_ghg_comp_df$Year[1]
                                    last_year = cum_ghg_comp_df$Year[nrow(cum_ghg_comp_df)]
                                    
                                    if (lt_lab != "Developed_all") {
                                    	cum_ghg_comp_df = merge(cum_ghg_comp_df, out_area_df_list[[2]][out_area_df_list[[2]]$Scenario == scen_lnames[s],], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	cum_ghg_comp_df$CumArea = cumsum(cum_ghg_comp_df$Value.y)
                                        cum_ghg_comp_df$DiffPerArea[cum_ghg_comp_df$Land_Type != "Developed_all" & cum_ghg_comp_df$Year > first_year] =
                                        cum_ghg_comp_df$Diff[cum_ghg_comp_df$Land_Type != "Developed_all" & cum_ghg_comp_df$Year > first_year] /
                                        cum_ghg_comp_df$CumArea[cum_ghg_comp_df$Land_Type != "Developed_all" & cum_ghg_comp_df$Year < last_year] / Mg2MMT * ha2kha
                                    } else {
                                        # for developed growth divide diff by cumulative difference in urban area between the scenario #2 and the baseline #1
                                        # for developed urban forest divide diff by cumulative difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by cumulative dead removal area
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period doesn't cancel itself out

										# extract current scenario
										dev_man_scen_df = dev_man_data_df[dev_man_data_df$Scenario == scen_lnames[s],]
                                        if ( sum(dev_man_scen_df$value[dev_man_scen_df$Management == "Dead_removal"]) > 0 ) {
                                            # Dead_removal practice
                                            dev_man_data_sub_df = dev_man_scen_df[dev_man_scen_df$Management == "Dead_removal",]
                                            dev_man_data_sub_df$CumArea = cumsum(dev_man_data_sub_df$value)
                                    		cum_ghg_comp_df = merge(cum_ghg_comp_df, dev_man_data_sub_df, by = c("Region", "Land_Type", "Year"), all.x = TRUE)
                                            cum_ghg_comp_df$DiffPerArea[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year > first_year] =
                                            cum_ghg_comp_df$Diff[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year > first_year] /
                                            cum_ghg_comp_df$CumArea[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year < last_year] / Mg2MMT

                                        } else if ( sum(dev_man_scen_df$value[dev_man_scen_df$Management == "Growth"] ) != 0 ) {
                                            # Growth
                                            # need to cumulate before merge; these data are also specific to region, land type, ownership
                                            tot_area_scen_df = out_area_df_list[[1]][out_area_df_list[[1]]$Scenario == scen_lnames[s],]
                                            tot_area_scen_df$DiffArea = tot_area_scen_df$Value - out_area_df_list[[1]][out_area_df_list[[1]]$Scenario == scen_lnames[1],]$Value
                                            tot_area_scen_df$CumArea = cumsum(tot_area_scen_df$DiffArea)
                                            cum_ghg_comp_df = merge(cum_ghg_comp_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            cum_ghg_comp_df$DiffPerArea[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year > first_year] =
                                            cum_ghg_comp_df$Diff[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year > first_year] /
                                            cum_ghg_comp_df$CumArea[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year < last_year] / Mg2MMT * ha2kha
                                        } else {
                                            # Urban_forest
                                            # need to cumulate before merge with variable
                                            dev_man_data_sub_df = merge(dev_man_scen_df[dev_man_scen_df$Management == "Urban_forest",], dev_man_data_df[dev_man_data_df$Management == "Urban_forest" & dev_man_data_df$Scenario == scen_lnames[1],], by = c("Region", "Land_Type", "Year"), all.x = TRUE)
                                            dev_man_data_sub_df$DiffArea = dev_man_data_sub_df$value.x - dev_man_data_sub_df$value.y
                                            dev_man_data_sub_df$CumArea = cumsum(dev_man_data_sub_df$DiffArea)
                                            cum_ghg_comp_df = merge(cum_ghg_comp_df, dev_man_data_sub_df, by = c("Region", "Land_Type", "Year"), all.x = TRUE)
                                            cum_ghg_comp_df$DiffPerArea[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year > first_year] =
                                            cum_ghg_comp_df$Diff[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year > first_year] /
                                            cum_ghg_comp_df$CumArea[cum_ghg_comp_df$Land_Type == "Developed_all" & cum_ghg_comp_df$Year < last_year] / Mg2MMT
                                        }
                                    } # end else Developed_all
                                    
                                    # deal with NA, NaN and Inf
                                    na_inds = which(is.na(cum_ghg_comp_df$DiffPerArea))
                                    nan_inds = which(is.nan(cum_ghg_comp_df$DiffPerArea))
                                    inf_inds = which(cum_ghg_comp_df$DiffPerArea == Inf)
                                    ninf_inds = which(cum_ghg_comp_df$DiffPerArea == -Inf)
                                    cum_ghg_comp_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    
                                    if (units_ha == TRUE) {
                                      cum_ghg_comp_df$units_dpa = "MgCO2eq/ha"
                                      
                                      p <- ( ggplot(cum_ghg_comp_df, aes(x=Year))
                                             + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$DiffPerArea > 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$DiffPerArea < 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_hline(yintercept=0)
                                             + ylab("Change from Baseline (Mg CO2-eq per ha)")
                                             + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to change in net cumulative GWP per man area"))
                                             + scale_fill_manual(values = ghg_colors, breaks = breaks)
                                      )
                                    #print(p)
                                    } else {
                                      cum_ghg_comp_df$units_dpa = "MgCO2eq/ac"
                                      p <- ( ggplot(cum_ghg_comp_df, aes(x=Year))
                                             + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$DiffPerArea > 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$DiffPerArea < 0,], aes(x=Year, y=DiffPerArea, fill = Component), position = 'stack')
                                             + geom_hline(yintercept=0)
                                             + ylab("Change from Baseline (Mg CO2-eq per ac)")
                                             + ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, own_lab, ": Contribution to change in net cumulative GWP per man area"))
                                             + scale_fill_manual(values = ghg_colors, breaks = breaks)
                                      )
                                    }
                                    
                                    
                                    
                                    p$save_args <- FIGURE_DIMS
                                    out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_cum_comp_diffperarea_output.pdf")
                                    do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                                    out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", scen_lnames[s], "_ghg_cum_comp_diffperarea_output.csv")
                            		write.csv(cum_ghg_comp_df, out_file, quote=FALSE, row.names=FALSE)
                                } # end if cumulative per area effect of individual practice
                            } # end if difference from baseline plots for cum ghg
                            
                        } # end s loop over scenario output files
                        
                        #########################################
                        
                        # plot the stock
                        for (i in 1:num_stock_sheets) {
                            
                            # land
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", stock_sheets[i], "_output.pdf")
                            plot_df = out_stock_df_list[[i]][out_stock_df_list[[i]][,"Land_Type"] == lt_lab,]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT C" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, stock_sheets[i]))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", stock_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # land diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", stock_sheets[i], "_diff_output.pdf")
                            temp_df = out_stock_df_list[[i]][out_stock_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT C)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, stock_sheets[i], "Change from Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", stock_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # plot the per area effects on carbon pools of individual practices
                            # use cumulative managed area
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                            # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                            #if (INDIVIDUAL & lt_lab != "All_land") {
                                #if (INDIVIDUAL) {
                                if (FALSE) {
                                	# non-dev management can use the regular management area output table
                                	#   because the management areas are mutually exclusive and they don't need special treatment
                                	
                                    # dev_man_data_df has scenario, region, management, land type, year, and value in ha
                                    # these data are specific to current region, land type, and ownership (but ownership is not in the df)
                                    # need to filter the scenarios to get cumulative areas
                                    # get the managed area, and the cumulative managed area
                                    # plot_df can have All_own and All_region; the managed area data have already been processed accordingly
                                    # assume that the years are in order and are the same length for each scenario

                                    # divide plot_df$Value by cumulative managed area
                                    # these values represent the beginning of the year
                                    # so the first year diff is zero, and the last year is end_year+1
                                    
                                    first_year = plot_df$Year[1]
                                    last_year = plot_df$Year[nrow(plot_df)]
                                    
                                    if (lt_lab != "Developed_all") {
                                    	plot_df = merge(plot_df, out_area_df_list[[2]], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	# need to get cumulative sums and calcs per scenario
                                    	for (s in 2:num_scen_names) {
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]])
                                    	}
                                        plot_df$DiffPerArea[plot_df$Land_Type != "Developed_all" & plot_df$Year > first_year] =
                                        plot_df$Value.x[plot_df$Land_Type != "Developed_all" & plot_df$Year > first_year] /
                                        plot_df$CumArea[plot_df$Land_Type != "Developed_all" & plot_df$Year < last_year] / Mg2MMT * ha2kha
                                    } else {
                                        # for developed growth divide diff by cumulative difference in urban area between the scenario #2 and the baseline #1
                                        # for developed urban forest divide diff by cumulative difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by cumulative dead removal area
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period or scenarios doesn't cancel itself out

                                        if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Dead_removal"]) > 0 ) {
                                            # Dead_removal practice
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Dead_removal",]
                                            # need to get cumulative sums and calcs per scenario
                                            for (s in 2:num_scen_names) {
                                    		dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                    			cumsum(dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]])
                                    		}
                                    		plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] / Mg2MMT

                                        } else if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Growth"] ) != 0 ) {
                                            # Growth
                                            tot_area_scen_df = out_area_df_list[[1]]
                                            # need to get cumulative sums and calcs per scenario; these data are also specific to region, land type, ownership
                                            for (s in 2:num_scen_names) {
                                            tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] -
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]]
                                            tot_area_scen_df$CumArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            	cumsum(tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]])
                                            }
                                            plot_df = merge(plot_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] / Mg2MMT * ha2kha
                                        } else {
                                            # Urban_forest
                                            # need to cumulate before merge with variable
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Urban_forest",]
                                            for (s in 2:num_scen_names) {
                                            	dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]] -
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[1]]
                                            	dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		cumsum(dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]])	
                                            }
                                            plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] / Mg2MMT
                                        }
                                    } # end else Developed_all
                                    
                                    # deal with NA, NaN and Inf
                                    na_inds = which(is.na(plot_df$DiffPerArea))
                                    nan_inds = which(is.nan(plot_df$DiffPerArea))
                                    inf_inds = which(plot_df$DiffPerArea == Inf)
                                    ninf_inds = which(plot_df$DiffPerArea == -Inf)
                                    plot_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    
                                    if (units_ha == TRUE) {
                                      plot_df$units_dpa = "MgC/ha"
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ha)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, stock_sheets[i], "Change from Baseline per man area"))
                                      )
                                    } else {
                                      plot_df$units_dpa = "MgC/ha"
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ac)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, stock_sheets[i], "Change from Baseline per man area"))
                                      )
                                    }
                                    
                            		p$save_args <- FIGURE_DIMS
                            		#print(p)
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", stock_sheets[i], "_diffperarea_output.pdf")
                            		do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", stock_sheets[i], "_diffperarea_output.csv")
                            		write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                                } # end if cumulative per area effect of individual practice
                            
                        } # end plot stock
                        
                        # plot the annual
                        for (i in 1:num_ann_sheets) {
                            
                            ########## there are a couple of extra variables thrown in to check
                            
                            # land
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_sheets[i], "_output.pdf")
                            plot_df = out_ann_df_list[[i]][out_ann_df_list[[i]][,"Land_Type"] == lt_lab,]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT C per year" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_sheets[i]))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # land diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_sheets[i], "_diff_output.pdf")
                            temp_df = out_ann_df_list[[i]][out_ann_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT C per year)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_sheets[i], "Change from Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # plot the per area effects on carbon pools of individual practices
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                            # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                            #if (INDIVIDUAL & lt_lab != "All_land") {
                                #if (INDIVIDUAL) {
                                if (FALSE) {
                                	# non-dev management can use the regular management area output table
                                	#   because the management areas are mutually exclusive and they don't need special treatment
                                	
                                    # dev_man_data_df has scenario, region, management, land type, year, and value in ha
                                    # these data are specific to current region, land type, and ownership (but ownership is not in the df)
                                    # need to filter the scenarios to get cumulative areas
                                    # get the managed area, and the cumulative managed area as needed
                                    # plot_df can have All_own and All_region; the managed area data have already been processed accordingly
                                    # assume that the years are in order and are the same length for each scenario

                                    # for everything except cultivated and developed:
                                    # divide Value (difference) by cumulative managed area because of cumulative effects on carbon accumulation
                                    # this applies for restoration activities and forest and rangeland practices
                                    if (lt_lab != "Cultivated" & lt_lab != "Developed_all") {
                                    		# need to get cumulative sums and calcs per scenario
                                    		plot_df = merge(plot_df, out_area_df_list[[2]], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    		for (s in 2:num_scen_names) {
                                    			plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]])
                                    		}
                                        	plot_df$DiffPerArea[plot_df$Land_Type != "Cultivated" & plot_df$Land_Type != "Developed_all"] =
                                        	plot_df$Value.x[plot_df$Land_Type != "Cultivated" & plot_df$Land_Type != "Developed_all"] /
                                        	plot_df$CumArea[plot_df$Land_Type != "Cultivated" & plot_df$Land_Type != "Developed_all"] / Mg2MMT * ha2kha
                                    } else if (lt_lab == "Cultivated") {
                                        # for cultivated, divide diff by annual managed area
                                        plot_df = merge(plot_df, out_area_df_list[[2]], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                        plot_df$DiffPerArea[plot_df$Land_Type == "Cultivated"] =
                                        plot_df$Value.x[plot_df$Land_Type == "Cultivated"] /
                                        plot_df$Value.y[plot_df$Land_Type == "Cultivated"] / Mg2MMT * ha2kha
                                    } else if (lt_lab == "Developed_all") {
                                        # for developed growth divide diff by cumulative difference in urban area between the scenario #2 and the baseline #1
                                        # for developed urban forest divide diff by cumulative difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by annual dead removal area (this does not have a long-term effect on growth rates)
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period or scenarios doesn't cancel itself out

                                        if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Dead_removal"]) > 0 ) {
                                            # Dead_removal practice
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Dead_removal",]
                                    			plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            	plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all"] =
                                            	plot_df$Value[plot_df$Land_Type == "Developed_all"] /
                                            	plot_df$value[plot_df$Land_Type == "Developed_all"] / Mg2MMT

                                        } else if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Growth"] ) != 0 ) {
                                            # Growth
                                            tot_area_scen_df = out_area_df_list[[1]]
                                            # need to get cumulative sums and calcs per scenario; these data are also specific to region, land type, ownership
                                            for (s in 2:num_scen_names) {
                                            		tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            			tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] -
                                            			tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]]
                                            		tot_area_scen_df$CumArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            			cumsum(tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]])
                                            }
                                            plot_df = merge(plot_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all"] =
                                            plot_df$Value.x[plot_df$Land_Type == "Developed_all"] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all"] / Mg2MMT * ha2kha
                                        } else {
                                            # Urban_forest
                                            # need to cumulate before merge with variable
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Urban_forest",]
                                            for (s in 2:num_scen_names) {
                                            	dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]] -
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[1]]
                                            	dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		cumsum(dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]])	
                                            }
                                            plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all"] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all"] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all"] / Mg2MMT
                                        }
                                    } # end else Developed_all
                                    
                                    # deal with NA, NaN and Inf
                                    na_inds = which(is.na(plot_df$DiffPerArea))
                                    nan_inds = which(is.nan(plot_df$DiffPerArea))
                                    inf_inds = which(plot_df$DiffPerArea == Inf)
                                    ninf_inds = which(plot_df$DiffPerArea == -Inf)
                                    plot_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    
                                    if (units_ha == TRUE) {
                                      plot_df$units_dpa = "MgC/ha/yr"
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ha per yr)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_sheets[i], "Change from Baseline per man area per yr"))
                                      )
                                    } else {
                                      plot_df$units_dpa = "MgC/ac/yr"
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ac per yr)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_sheets[i], "Change from Baseline per man area per yr"))
                                      )
                                    }
                                    
                                    
                            		p$save_args <- FIGURE_DIMS
                            		#print(p)
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_sheets[i], "_diffperarea_output.pdf")
                            		do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_sheets[i], "_diffperarea_output.csv")
                            		write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                                } # end if cumulative per area effect of individual practice
                            
                        } # end plot annual
                        
                        # plot the cumulative
                        for (i in 1:num_cum_sheets) {
                            
                            ############### there are a couple of extra variables thrown in to check
                            
                            # land line
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_sheets[i], "_output.pdf")
                            plot_df = out_cum_df_list[[i]][out_cum_df_list[[i]][,"Land_Type"] == lt_lab,]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT C" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, cum_sheets[i]))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # land diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_sheets[i], "_diff_output.pdf")
                            temp_df = out_cum_df_list[[i]][out_cum_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT C)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, cum_sheets[i], "Change from Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # plot the per area effects on carbon pools of individual practices
                            # use cumulative managed area
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                            # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                            #if (INDIVIDUAL & lt_lab != "All_land") {
                                #if (INDIVIDUAL) {
                                if (FALSE) {
                                	# non-dev management can use the regular management area output table
                                	#   because the management areas are mutually exclusive and they don't need special treatment
                                	
                                    # dev_man_data_df has scenario, region, management, land type, year, and value in ha
                                    # these data are specific to current region, land type, and ownership (but ownership is not in the df)
                                    # need to filter the scenarios to get cumulative areas
                                    # get the managed area, and the cumulative managed area
                                    # plot_df can have All_own and All_region; the managed area data have already been processed accordingly
                                    # assume that the years are in order and are the same length for each scenario

                                    # divide plot_df$Value by cumulative managed area
                                    # these values represent the beginning of the year
                                    # so the first year diff is zero, and the last year is end_year+1
                                    
                                    first_year = plot_df$Year[1]
                                    last_year = plot_df$Year[nrow(plot_df)]
                                    
                                    if (lt_lab != "Developed_all") {
                                    	plot_df = merge(plot_df, out_area_df_list[[2]], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	# need to get cumulative sums and calcs per scenario
                                    	for (s in 2:num_scen_names) {
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]])
                                    	}
                                        plot_df$DiffPerArea[plot_df$Land_Type != "Developed_all" & plot_df$Year > first_year] =
                                        plot_df$Value.x[plot_df$Land_Type != "Developed_all" & plot_df$Year > first_year] /
                                        plot_df$CumArea[plot_df$Land_Type != "Developed_all" & plot_df$Year < last_year] / Mg2MMT * ha2kha
                                    } else {
                                        # for developed growth divide diff by cumulative difference in urban area between the scenario #2 and the baseline #1
                                        # for developed urban forest divide diff by cumulative difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by cumulative dead removal area
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period or scenarios doesn't cancel itself out

                                        if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Dead_removal"]) > 0 ) {
                                            # Dead_removal practice
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Dead_removal",]
                                            # need to get cumulative sums and calcs per scenario
                                            for (s in 2:num_scen_names) {
                                    		dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                    			cumsum(dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]])
                                    		}
                                    		plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] / Mg2MMT

                                        } else if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Growth"] ) != 0 ) {
                                            # Growth
                                            tot_area_scen_df = out_area_df_list[[1]]
                                            # need to get cumulative sums and calcs per scenario; these data are also specific to region, land type, ownership
                                            for (s in 2:num_scen_names) {
                                            tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] -
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]]
                                            tot_area_scen_df$CumArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            	cumsum(tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]])
                                            }
                                            plot_df = merge(plot_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] / Mg2MMT * ha2kha
                                        } else {
                                            # Urban_forest
                                            # need to cumulate before merge with variable
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Urban_forest",]
                                            for (s in 2:num_scen_names) {
                                            	dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]] -
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[1]]
                                            	dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		cumsum(dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]])	
                                            }
                                            plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] / Mg2MMT
                                        }
                                    } # end else Developed_all
                                    
                                    # deal with NA, NaN and Inf
                                    na_inds = which(is.na(plot_df$DiffPerArea))
                                    nan_inds = which(is.nan(plot_df$DiffPerArea))
                                    inf_inds = which(plot_df$DiffPerArea == Inf)
                                    ninf_inds = which(plot_df$DiffPerArea == -Inf)
                                    plot_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    
                                    if (units_ha == TRUE) {
                                    plot_df$units_dpa = "MgC/ha"
                                    p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                           + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                           + geom_line(size = 0.3)
                                           + geom_point(aes(shape=Scenario), size = 1.5)
                                           + ylab( paste( "Change from Baseline (Mg C per ha)" ) )
                                           + theme(legend.key.size = unit(0.4,"cm"))
                                           + ggtitle(paste(reg_lab, lt_lab, own_lab, cum_sheets[i], "Change from Baseline per man area"))
                                    )
                                    } else {
                                      plot_df$units_dpa = "MgC/ac"
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ac)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, cum_sheets[i], "Change from Baseline per man area"))
                                      )
                                    }
                                    
                            		p$save_args <- FIGURE_DIMS
                            		#print(p)
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_sheets[i], "_diffperarea_output.pdf")
                            		do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_sheets[i], "_diffperarea_output.csv")
                            		write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                                } # end if cumulative per area effect of individual practice
                            
                        } # end plot cumulative
                        
                        # plot the area
                        for (i in 1:num_area_sheets) {
                            
                            # land
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", area_sheets[i], "_output.pdf")
                            # out_area_df_list (3 df's)
                              #1: Annual Area, 2010-2051 (Scenario, Reg, Land_Type, Ownership, Units, Year, Value)
                              #2: Annual Managed Area, 2010-2050 (Scenario, Reg, Land_Type, Ownership, Units, Year, Value)
                              #3: Annual Wildfire Area, 2010-2050 (Scenario, Reg, Land_Type, Ownership, Units, Year, Value)
                            plot_df = out_area_df_list[[i]][out_area_df_list[[i]][,"Land_Type"] == lt_lab,]
                            if (units_ha == TRUE) { 
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Thousand ha" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, area_sheets[i]))
                            )
                            } else {
                              p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                                     + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                     + geom_line(size = 0.3)
                                     + geom_point(aes(shape=Scenario), size = 1.5)
                                     + ylab( paste( "Thousand ac" ) )
                                     + theme(legend.key.size = unit(0.4,"cm"))
                                     + ggtitle(paste(reg_lab, lt_lab, own_lab, area_sheets[i]))
                              )
                            }
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", area_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # land diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", area_sheets[i], "_diff_output.pdf")
                            temp_df = out_area_df_list[[i]][out_area_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            # error if there are no prescribed management practices in alternative scenario
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            if (units_ha == TRUE) {
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (Thousand ha)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, area_sheets[i], "Change from Baseline"))
                            )   
                            } else {
                              p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                                     + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                     + geom_line(size = 0.3)
                                     + geom_point(aes(shape=Scenario), size = 1.5)
                                     + ylab( paste( "Change from Baseline (Thousand ac)" ) )
                                     + theme(legend.key.size = unit(0.4,"cm"))
                                     + ggtitle(paste(reg_lab, lt_lab, own_lab, area_sheets[i], "Change from Baseline"))
                              )  
                            }
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", area_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                        } # end plot area
                        
                        # plot the density
                        for (i in 1:num_den_sheets) {
                            
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", den_sheets[i], "_output.pdf")
                            plot_df = out_den_df_list[[i]][out_den_df_list[[i]][,"Land_Type"] == lt_lab,]
                            if (units_ha == TRUE) {
                              p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                                     + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                     + geom_line(size = 0.3)
                                     + geom_point(aes(shape=Scenario), size = 1.5)
                                     + ylab( paste( "MgC per ha" ) )
                                     + theme(legend.key.size = unit(0.4,"cm"))
                                     + ggtitle(paste(reg_lab, lt_lab, own_lab, den_sheets[i]))
                              )
                            } else {
                              p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                                     + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                     + geom_line(size = 0.3)
                                     + geom_point(aes(shape=Scenario), size = 1.5)
                                     + ylab( paste( "MgC per ac" ) )
                                     + theme(legend.key.size = unit(0.4,"cm"))
                                     + ggtitle(paste(reg_lab, lt_lab, own_lab, den_sheets[i]))
                              )
                            }
                            
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", den_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", den_sheets[i], "_diff_output.pdf")
                            temp_df = out_den_df_list[[i]][out_den_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            if (units_ha == TRUE) {
                              p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                                     + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                     + geom_line(size = 0.3)
                                     + geom_point(aes(shape=Scenario), size = 1.5)
                                     + ylab( paste( "Change from Baseline (MgC per ha)" ) )
                                     + theme(legend.key.size = unit(0.4,"cm"))
                                     + ggtitle(paste(reg_lab, lt_lab, own_lab, own_lab, den_sheets[i], "Change from Baseline"))
                              )
                            } else {
                              p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                                     + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                     + geom_line(size = 0.3)
                                     + geom_point(aes(shape=Scenario), size = 1.5)
                                     + ylab( paste( "Change from Baseline (MgC per ac)" ) )
                                     + theme(legend.key.size = unit(0.4,"cm"))
                                     + ggtitle(paste(reg_lab, lt_lab, own_lab, own_lab, den_sheets[i], "Change from Baseline"))
                              )
                            }
                            
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", den_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                        
                        	# plot the per area effects on carbon pools of individual practices
                            # use cumulative managed area
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                            # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                            #if (INDIVIDUAL & lt_lab != "All_land") {
                                #if (INDIVIDUAL) {
                                if (FALSE) {
                                	# non-dev management can use the regular management area output table
                                	#   because the management areas are mutually exclusive and they don't need special treatment
                                	
                                    # dev_man_data_df has scenario, region, management, land type, year, and value in ha
                                    # these data are specific to current region, land type, and ownership (but ownership is not in the df)
                                    # need to filter the scenarios to get cumulative areas
                                    # get the managed area, and the cumulative managed area
                                    # plot_df can have All_own and All_region; the managed area data have already been processed accordingly
                                    # assume that the years are in order and are the same length for each scenario

                                    # divide plot_df$Value by cumulative managed area
                                    # these values represent the beginning of the year
                                    # so the first year diff is zero, and the last year is end_year+1
                                    
                                    first_year = plot_df$Year[1]
                                    last_year = plot_df$Year[nrow(plot_df)]
                                    
                                    if (lt_lab != "Developed_all") {
                                    	plot_df = merge(plot_df, out_area_df_list[[2]], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	# need to get cumulative sums and calcs per scenario
                                    	for (s in 2:num_scen_names) {
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]])
                                    	}
                                        plot_df$DiffPerArea[plot_df$Land_Type != "Developed_all" & plot_df$Year > first_year] =
                                        plot_df$Value.x[plot_df$Land_Type != "Developed_all" & plot_df$Year > first_year] /
                                        plot_df$CumArea[plot_df$Land_Type != "Developed_all" & plot_df$Year < last_year] * ha2kha
                                    } else {
                                        # for developed growth divide diff by cumulative difference in urban area between the scenario #2 and the baseline #1
                                        # for developed urban forest divide diff by cumulative difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by cumulative dead removal area
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period or scenarios doesn't cancel itself out

                                        if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Dead_removal"]) > 0 ) {
                                            # Dead_removal practice
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Dead_removal",]
                                            # need to get cumulative sums and calcs per scenario
                                            for (s in 2:num_scen_names) {
                                    		dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                    			cumsum(dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]])
                                    		}
                                    		plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year]

                                        } else if ( sum(dev_man_data_df$value[dev_man_data_df$Management == "Growth"] ) != 0 ) {
                                            # Growth
                                            tot_area_scen_df = out_area_df_list[[1]]
                                            # need to get cumulative sums and calcs per scenario; these data are also specific to region, land type, ownership
                                            for (s in 2:num_scen_names) {
                                            tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] -
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]]
                                            tot_area_scen_df$CumArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            	cumsum(tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]])
                                            }
                                            plot_df = merge(plot_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year] * ha2kha
                                        } else {
                                            # Urban_forest
                                            # need to cumulate before merge with variable
                                            dev_man_data_sub_df = dev_man_data_df[dev_man_data_df$Management == "Urban_forest",]
                                            for (s in 2:num_scen_names) {
                                            	dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[s]] -
                                            		dev_man_data_sub_df$value[dev_man_data_sub_df$Scenario == scen_lnames[1]]
                                            	dev_man_data_sub_df$CumArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            		cumsum(dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]])	
                                            }
                                            plot_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] =
                                            plot_df$Value[plot_df$Land_Type == "Developed_all" & plot_df$Year > first_year] /
                                            plot_df$CumArea[plot_df$Land_Type == "Developed_all" & plot_df$Year < last_year]
                                        }
                                    } # end else Developed_all
                                    
                                    # deal with NA, NaN and Inf
                                    na_inds = which(is.na(plot_df$DiffPerArea))
                                    nan_inds = which(is.nan(plot_df$DiffPerArea))
                                    inf_inds = which(plot_df$DiffPerArea == Inf)
                                    ninf_inds = which(plot_df$DiffPerArea == -Inf)
                                    plot_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    
                                    if (units_ha == TRUE) {
                                      plot_df$units_dpa = "Mg/ha/ha"
                                    } else {
                                      plot_df$units_dpa = "Mg/ac/ac"
                                    }
                                    
                                    if (units_ha == TRUE) {
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ha per ha)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, den_sheets[i], "Change from Baseline per man area"))
                                      )
                                    } else {
                                      p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             + geom_line(size = 0.3)
                                             + geom_point(aes(shape=Scenario), size = 1.5)
                                             + ylab( paste( "Change from Baseline (Mg C per ac per ac)" ) )
                                             + theme(legend.key.size = unit(0.4,"cm"))
                                             + ggtitle(paste(reg_lab, lt_lab, own_lab, den_sheets[i], "Change from Baseline per man area"))
                                      )
                                    }
                            		p$save_args <- FIGURE_DIMS
                            		#print(p)
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", den_sheets[i], "_diffperarea_output.pdf")
                            		do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            		out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", den_sheets[i], "_diffperarea_output.csv")
                            		write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                                } # end if cumulative per area effect of individual practice
                        
                        } # end plot density
                        
                        # plot the annual ghg line plot comparisons
                        for (i in 1:num_plot_ann_ghg_sheets) {
                            if (blackC_plot==TRUE | blackC_plot==FALSE & !(ann_ghg_sheets[i] %in% c("Total_AnnBCeq", "Wildfire_AnnBCeq", "ManTotEnergy_AnnBCeq",
                                                                                                    "LCCTotEnergy_AnnBCeq", "ManFire_AnnBCeq", "LCCFire_AnnBCeq"))) {
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_ghg_sheets[i], "_output.pdf")
                            plot_df = out_ann_ghg_df_list[[i]][out_ann_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT CO2-eq per year" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_ghg_sheets[i]))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_ghg_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_ghg_sheets[i], "_diff_output.pdf")
                            temp_df = out_ann_ghg_df_list[[i]][out_ann_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT CO2-eq per year)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_ghg_sheets[i], "Change from Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_ghg_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                        
                        		# plot the per area effects on carbon pools of individual practices
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                            # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                            #if (INDIVIDUAL & lt_lab != "All_land") {
                                if (INDIVIDUAL) {
                                	
                                	# man_df is managed area with scenario, region, land type, ownership, management, year, units, and value in ha or ac
                                	# different management practices require different areas
                                	
                                	if (lt_lab == "Cultivated" & length(nrow(man_df)) > 0) {
                                		# cultivated soil conservation uses annual area because effects occur in current year only
                                		# for cultivated, divide diff by annual managed area
                                        plot_df = merge(plot_df, man_df[man_df$Management == "Soil_conservation",], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                        plot_df$DiffPerArea[plot_df$Land_Type == "Cultivated"] =
                                        plot_df$Value.x[plot_df$Land_Type == "Cultivated"] /
                                        plot_df$Value.y[plot_df$Land_Type == "Cultivated"] / Mg2MMT
                                	} else if (lt_lab == "Forest" & length(nrow(man_df)) > 0) {
                                		# don't do afforestation here because it needs lt_lab == All_land
                                		# first count how many practices there are
                                		# man_df has already been pared down to current reg, lt, own, but with multiple scenarios
                                		num_prac = NULL
                                		num_prac_ok = TRUE
                                		for (s in 2:num_scen_names) {
                                			sub_man_df = man_df[man_df$Scenario == scen_lnames[s] & man_df$Year == man_df$Year[1],]
                                			num_prac = c(num_prac, nrow(sub_man_df))
                                			if (s > 2) {
                                				if (num_prac[s-1] != num_prac[s-2]) {
                                					cat("\nError: non-baseline forest simes have to have the same number of forest practices!\n")
                                					stop()
                                				}
                                			}
                                		}
                                		
                                		# biomass practices: need cumulative area
                                		# only one type of land and management should be present
                                		if (num_prac[1] == 1) {
                                			plot_df = merge(plot_df, man_df[man_df$Land_Type == lt_lab,], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    		for (s in 2:num_scen_names) {
                                    			plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]])
                                    		}
                                        	plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] =
                                        		plot_df$Value.x[plot_df$Land_Type == lt_lab] /
                                        		plot_df$CumArea[plot_df$Land_Type == lt_lab] / Mg2MMT
                                        } else {
                                        	# less intensive management: need cumulative area of transferred managed area
                                        	# should only be clearcut and partial_cut
                                        	# this won't work for med or hi slash util
                                        	cc_man_df = man_df[man_df$Management == "Clearcut",]
                                        	pc_man_df = man_df[man_df$Management == "Partial_cut",]
                                        	
                                    		for (s in 2:num_scen_names) {
                                    			man_df$DiffArea[man_df$Scenario == scen_lnames[s]] = 
                                    				man_df$Value[man_df$Scenario == scen_lnames[s]] -
                                            		man_df$Value[man_df$Scenario == scen_lnames[1]]                                    			
                                    			cc_man_df$DiffArea[cc_man_df$Scenario == scen_lnames[s]] = 
                                    				cc_man_df$Value[cc_man_df$Scenario == scen_lnames[s]] -
                                            		cc_man_df$Value[cc_man_df$Scenario == scen_lnames[1]]
                                    			pc_man_df$DiffArea[pc_man_df$Scenario == scen_lnames[s]] = 
                                    				pc_man_df$Value[pc_man_df$Scenario == scen_lnames[s]] -
                                            		pc_man_df$Value[pc_man_df$Scenario == scen_lnames[1]]
                                            		
                                            	cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]] = cumsum(cc_man_df$DiffArea[cc_man_df$Scenario == scen_lnames[s]])
                                            	pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]] = cumsum(pc_man_df$DiffArea[pc_man_df$Scenario == scen_lnames[s]])
                                            	check_cc_val = sum(cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]],na.rm = TRUE)
                                            	check_pc_val = sum(pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]],na.rm = TRUE)
                                            	if (check_cc_val != 0 & check_pc_val != 0) {
                                            		# clearcut to partial
                                            		cc_man_df$TranArea[cc_man_df$Scenario == scen_lnames[s]] = -cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]]
                                            	} else if (check_cc_val != 0 & check_pc_val == 0) {
                                            		# clearcut to reserve
                                            		cc_man_df$TranArea[cc_man_df$Scenario == scen_lnames[s]] = -cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]]
                                            	} else if (check_cc_val == 0 & check_pc_val != 0) {
                                            		# partial to reserve
                                            		# put this in the cc data frame for output
                                            		cc_man_df$DiffArea[cc_man_df$Scenario == scen_lnames[s]] = pc_man_df$DiffArea[pc_man_df$Scenario == scen_lnames[s]]
                                            		cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]] = pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]]
                                            		cc_man_df$TranArea[pc_man_df$Scenario == scen_lnames[s]] = -pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]]
                                            	} else {
                                            		cat("\nError in less intensive management: prescription outside of available practices\n")
                                            		stop()
                                            	}
                                    		} # end for s loop over non-base scenarios
                                    		
                                    		plot_df = merge(plot_df, cc_man_df[cc_man_df$Land_Type == lt_lab,], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    		plot_df$Management = "Less_intensive"
                                    		
                                        	plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] =
                                        		plot_df$Value.x[plot_df$Land_Type == lt_lab] /
                                        		plot_df$TranArea[plot_df$Land_Type == lt_lab] / Mg2MMT
                                        } # end else less intensive management
                                		
                                	} else if (lt_lab == "Developed_all" & length(nrow(man_df)) > 0) {
                                		# do developed growth below with land cover change
                                		# for now check whether urban forest fraction is changing over time to determine practice
                                			# because if the urban forest fraction is changing then do it even if land use change is happening
                                        # for developed urban forest divide diff by difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by annual dead removal area (this does not have a long-term effect on growth rates)
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period or scenarios doesn't cancel itself out
										
										dev_man_data_sub_df = man_df[man_df$Management == "Urban_forest",]
										if (length(nrow(dev_man_data_sub_df)) > 0) {
												# need to check on a scenario basis
												plot_uf_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
												plot_dr_df = merge(plot_df, man_df[man_df$Management == "Dead_removal",], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            	for (s in 2:num_scen_names) {
                                            		dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] =
                                            			dev_man_data_sub_df$Value[dev_man_data_sub_df$Scenario == scen_lnames[s]] -
                                            			dev_man_data_sub_df$Value[dev_man_data_sub_df$Scenario == scen_lnames[1]]
                                            		
                                            		# if the difference in urban forest area is different, do it, otherwise do dead removal
                                            		if (sum(dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]], na.rm = TRUE) > 0) {
                                            			# Urban_forest
                                            			plot_df = plot_uf_df
                                            			plot_df$DiffArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] = 
                                            				dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Land_Type == "Developed_all" & dev_man_data_sub_df$Scenario == scen_lnames[s]]
                                            			plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] =
                                            				plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] /
                                            				plot_df$DiffArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] / Mg2MMT
                                            		} else {
                                            			# Dead_removal practice
                                    					plot_df = plot_dr_df
                                            			plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] =
                                            				plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] /
                                            				plot_df$Value.y[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] / Mg2MMT
                                            		}
                                            			
                                            			
                                            	} # end for loop over scenarios
                                       	} else { # end if there is urban forest
                                       		# Dead_removal practice
                                    		plot_df = merge(plot_df, man_df[man_df$Management == "Dead_removal",], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all"] =
                                            		plot_df$Value.x[plot_df$Land_Type == "Developed_all"] /
                                            plot_df$Value.y[plot_df$Land_Type == "Developed_all"] / Mg2MMT
                                       	} # end else just do dead removal cuz no urban forest
										  
                                	} else if (lt_lab == "All_land" & length(nrow(al_area_df)) > 0) {
                                		# restoration and afforestation and growth
                                		
                                		# need to associate the desired land type difference with the total benefit
                                       	# so first find which land type is driving the change
                                       		# this will be the one that is increasing or decreasing in isolation
                                       	# al_area_df has individual land type areas for this region and ownership, by scenario
                                     
                                		tot_area_scen_df = out_area_df_list[[1]]
                                		tot_area_scen_df$Units = as.character(tot_area_scen_df$Units)
                                		for (s in 2:num_scen_names) {
                                            al_area_df$DiffArea[al_area_df$Scenario == scen_lnames[s]] = 
                                            	al_area_df$Value[al_area_df$Scenario == scen_lnames[s]] -
                                            	al_area_df$Value[al_area_df$Scenario == scen_lnames[1]]
                                			pos_diffs = al_area_df[al_area_df$DiffArea > 0 & al_area_df$Scenario == scen_lnames[s],]
                                			neg_diffs = al_area_df[al_area_df$DiffArea < 0 & al_area_df$Scenario == scen_lnames[s],]
                                			if (nrow(pos_diffs) > nrow(neg_diffs)) {
                                				# the driving land type is decreasing
                                				lt_try = as.character( unique(neg_diffs$Land_Type) )
                                				if (length(lt_try) > 1) {
                                					# this could be difference in forest fire/nonregen due to woodland or meadow restoration
                                					# meadow actually reduces forest because meadow does not burn
                                					# in these two cases there are at least 3 conversion sources, so the pos/neg rows should still hold
                                					# so remove forest and try again
                                					lt_try = as.character( unique(neg_diffs$Land_Type[neg_diffs$Land_Type != "Forest"]) )
                                					if (length(lt_try) > 1) {
                                						cat("\nError: multiple lt drivers\n")
                                						stop()
                                					} else {
                                						lt_drive = lt_try
                                						lt_drive_sign = -1
                                					}
                                				} else {
                                					lt_drive = lt_try
                                					lt_drive_sign = -1
                                				}
                                			} else {
                                				# the driving land type is increasing
                                				lt_try = as.character( unique(pos_diffs$Land_Type) )
                                				if (length(lt_try) > 1) {
                                					# this could be difference in forest fire/nonregen due to woodland or meadow restoration
                                					# meadow actually reduces forest because meadow does not burn
                                					# in these two cases there are at least 3 conversion sources, so the pos/neg rows should still hold
                                					# so remove forest and try again
                                					lt_try = as.character( unique(pos_diffs$Land_Type[pos_diffs$Land_Type != "Forest"]) )
                                					if (length(lt_try) > 1) {
                                						cat("\nError: multiple lt drivers\n")
                                						stop()
                                					} else {
                                						lt_drive = lt_try
                                						lt_drive_sign = 1
                                					}
                                				} else {
                                					lt_drive = lt_try
                                					lt_drive_sign = 1
                                				}
                                			}
                                			
                                			# extract the driving land type and put these values into the all land placeholder df
                                			# assume that the row order stays the same
                                			temp_df = al_area_df[al_area_df$Land_Type == lt_drive,]
                                			
                                			# if nothing is changing in this case then set the values to 0
                                			# values from al_area_df are in ha or ac, so get the Units too
                                			if (nrow(temp_df) > 0) {
                                				tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] = temp_df$Value[temp_df$Scenario == scen_lnames[s]]
                                				tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]] = temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                				
                                				tot_area_scen_df$Units[tot_area_scen_df$Scenario == scen_lnames[s]] = temp_df$Units[temp_df$Scenario == scen_lnames[s]]
                                				tot_area_scen_df$Units[tot_area_scen_df$Scenario == scen_lnames[1]] = temp_df$Units[temp_df$Scenario == scen_lnames[1]]
                                		
                                        		tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] =
                                            		(tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] -
                                            		tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]] ) * lt_drive_sign
                                            } else {
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] = 0
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]] = 0
                                            	tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] = 0
                                            }
                                       	} # end for s over scenarios
                                       	
                                		plot_df = merge(plot_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                        plot_df$DiffPerArea[plot_df$Land_Type == "All_land"] =
                                        	plot_df$Value.x[plot_df$Land_Type == "All_land"] /
                                        	plot_df$DiffArea[plot_df$Land_Type == "All_land"] / Mg2MMT
                                	} else if ((lt_lab == "Grassland" | lt_lab == "Savanna" | lt_lab == "Woodland") & length(nrow(man_df)) > 0) {
                                		# rangeland compost management: need cumulative area
                                		# only one type of land and management should be present
                                		plot_df = merge(plot_df, man_df[man_df$Land_Type == lt_lab,], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	for (s in 2:num_scen_names) {
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]])
                                    	}
                                        plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] =
                                        	plot_df$Value.x[plot_df$Land_Type == lt_lab] /
                                        	plot_df$CumArea[plot_df$Land_Type == lt_lab] / Mg2MMT
                                	} # end if-else land types
                                	
                                	# make sure that DiffPerArea exists
                                	if (length(which(names(plot_df) == "DiffPerArea")) > 0) {
                                	                      
                                    	# deal with NA, NaN and Inf
                                    	na_inds = which(is.na(plot_df$DiffPerArea))
                                    	nan_inds = which(is.nan(plot_df$DiffPerArea))
                                    	inf_inds = which(plot_df$DiffPerArea == Inf)
                                    	ninf_inds = which(plot_df$DiffPerArea == -Inf)
                                    	plot_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    	
                                    	if (units_ha == TRUE) {
                                    	  plot_df$units_dpa = "MgCO2eq/ha/yr"
                                    	} else {
                                    	  plot_df$units_dpa = "MgCO2eq/ac/yr"
                                    	}
                                    	
                                    	if (units_ha == TRUE) {
                                    	  	p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                   	          	+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                    	     	+ geom_line(size = 0.3)
                                    	        + geom_point(aes(shape=Scenario), size = 1.5)
                                    	        + ylab( paste( "Change from Baseline (Mg CO2eq per ha per yr)" ) )
                                    	        + theme(legend.key.size = unit(0.4,"cm"))
                                    	        + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_ghg_sheets[i], "Change from Baseline per man area per yr"))
                                    	  	)
                                    	} else {
                                    	  	p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                    	       	+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                    	        + geom_line(size = 0.3)
                                    	        + geom_point(aes(shape=Scenario), size = 1.5)
                                    	        + ylab( paste( "Change from Baseline (Mg CO2eq per ac per yr)" ) )
                                    	        + theme(legend.key.size = unit(0.4,"cm"))
                                    	        + ggtitle(paste(reg_lab, lt_lab, own_lab, ann_ghg_sheets[i], "Change from Baseline per man area per yr"))
                                    	  	)
                                    	}
                            				p$save_args <- FIGURE_DIMS
                            				#print(p)
                            				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_ghg_sheets[i], "_diffperarea_output.pdf")
                            				do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", ann_ghg_sheets[i], "_diffperarea_output.csv")
                            				write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            			} # end if DiffPerArea has been calculted
                                } # end if per area effect of individual practice
                            } # end if plotting BC or plotting another GHG species
                        
                        } # end plot annual ghg line plot comparisons
                        
                        # plot the cumulative ghg line plot comparisons
                        for (i in 1:num_plot_cum_ghg_sheets) {
                          if (blackC_plot==TRUE | blackC_plot==FALSE & !(cum_ghg_sheets[i] %in% c("Total_CumBCeq", "Wildfire_CumBCeq", "ManTotEnergy_CumBCeq",
                                                                                                  "LCCTotEnergy_CumBCeq", "ManFire_CumBCeq", "LCCFire_CumBCeq"))) {
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_output.pdf")
                            plot_df = out_cum_ghg_df_list[[i]][out_cum_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT CO2-eq" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, cum_ghg_sheets[i]))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # diffs
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_diff_output.pdf")
                            temp_df = out_cum_ghg_df_list[[i]][out_cum_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_lnames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_lnames[s]] - temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT CO2-eq)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste(reg_lab, lt_lab, own_lab, cum_ghg_sheets[i], "Change from Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                        
                        # plot the per area effects on carbon pools of individual practices
                            # use cumulative managed area
                            		# the cumulative values are at the beginning of the labelled year
                            		# the total areas are at the beginning of the lablelled year
                            		# the managed areas are during the labelled year
                            		# so need to use an offset a year for cum area so that correct outputs are at beginning of labelled year
                            			# for restoration, and afforestation need to offset 2 years of cum years because there is no difference in year 1
                            			# for growth need to offset 3 years of cum years because there is no difference in years 1 and 2
                            	# also add the cumulative years and the final diff per area per year value
                            		# this is the per area per year benefit for the period of CumYears (the years prior to the output labelled year)
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                            # only apply to individual land types ==> actually we need All_land for restoration per acre benefits
                            #if (INDIVIDUAL & lt_lab != "All_land") {
                                if (INDIVIDUAL) {
                                	# man_df is managed area with scenario, region, land type, ownership, management, year, units, and value in ha or ac
                                	# different management practices require different areas
                                	
                                	if (lt_lab == "Cultivated" & length(nrow(man_df)) > 0) {
                                		# cultivated soil conservation uses annual area because effects occur in current year only
                                		# for cultivated, divide diff by cumulative managed area
                                        plot_df = merge(plot_df, man_df[man_df$Management == "Soil_conservation",], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                        for (s in 2:num_scen_names) {
                                        	cumyears = c(0,rep(1,(length(plot_df$Year[plot_df$Scenario == scen_lnames[s]])-1)))
                                        	cumyears = cumsum(cumyears)
                                        	plot_df$CumYears[plot_df$Scenario == scen_lnames[s]] = cumyears
                                        	plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = 0
                                        	cumarea = c(0,cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]]))
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                    	}
                                        plot_df$DiffPerArea[plot_df$Land_Type == "Cultivated"] =
                                        	plot_df$Value.x[plot_df$Land_Type == "Cultivated"] /
                                        	plot_df$CumArea[plot_df$Land_Type == "Cultivated"] / Mg2MMT
                                        # now add diff per area per year
                                        plot_df$DiffPerAreaPerYr[plot_df$Land_Type == "Cultivated"] =
                                        	plot_df$DiffPerArea[plot_df$Land_Type == "Cultivated"] /
                                        	plot_df$CumYears[plot_df$Land_Type == "Cultivated"]
                                	} else if (lt_lab == "Forest" & length(nrow(man_df)) > 0) {
                                		# don't do afforestation here because it needs lt_lab == All_land
                                		# first count how many practices there are
                                		# man_df has already been pared down to current reg, lt, own, but with multiple scenarios
                                		num_prac = NULL
                                		num_prac_ok = TRUE
                                		for (s in 2:num_scen_names) {
                                			sub_man_df = man_df[man_df$Scenario == scen_lnames[s] & man_df$Year == man_df$Year[1],]
                                			num_prac = c(num_prac, nrow(sub_man_df))
                                			if (s > 2) {
                                				if (num_prac[s-1] != num_prac[s-2]) {
                                					cat("\nError: non-baseline forest simes have to have the same number of forest practices!\n")
                                					stop()
                                				}
                                			}
                                		}
                                		
                                		# biomass practices: need cumulative area
                                		# only one type of land and management should be present
                                		if (num_prac[1] == 1) {
                                			plot_df = merge(plot_df, man_df[man_df$Land_Type == lt_lab,], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    		for (s in 2:num_scen_names) {
                                    			cumyears = c(0,rep(1,(length(plot_df$Year[plot_df$Scenario == scen_lnames[s]])-1)))
                                        		cumyears = cumsum(cumyears)
                                        		plot_df$CumYears[plot_df$Scenario == scen_lnames[s]] = cumyears
                                    			plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = 0
                                        		cumarea = c(0,cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]]))
                                    			plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                    		}
                                        	plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] =
                                        		plot_df$Value.x[plot_df$Land_Type == lt_lab] /
                                        		plot_df$CumArea[plot_df$Land_Type == lt_lab] / Mg2MMT
                                        	# now add diff per area per year
                                        	plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab] =
                                        		plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] /
                                        		plot_df$CumYears[plot_df$Land_Type == lt_lab]
                                        } else {
                                        	# less intensive management: need cumulative area of transferred managed area
                                        	# should only be clearcut and partial_cut
                                        	# this won't work for med or hi slash util
                                        	cc_man_df = man_df[man_df$Management == "Clearcut",]
                                        	pc_man_df = man_df[man_df$Management == "Partial_cut",]
                                        	
                                    		for (s in 2:num_scen_names) {
                                    			cumyears = c(0,rep(1,(length(cc_man_df$Year[cc_man_df$Scenario == scen_lnames[s]])-1)))
                                        		cumyears = cumsum(cumyears)
                                        		cc_man_df$CumYears[cc_man_df$Scenario == scen_lnames[s]] = cumyears
                                        		pc_man_df$CumYears[pc_man_df$Scenario == scen_lnames[s]] = cumyears
                                    			
                                    			man_df$DiffArea[man_df$Scenario == scen_lnames[s]] = 
                                    				man_df$Value[man_df$Scenario == scen_lnames[s]] -
                                            		man_df$Value[man_df$Scenario == scen_lnames[1]]                                    			
                                    			cc_man_df$DiffArea[cc_man_df$Scenario == scen_lnames[s]] = 
                                    				cc_man_df$Value[cc_man_df$Scenario == scen_lnames[s]] -
                                            		cc_man_df$Value[cc_man_df$Scenario == scen_lnames[1]]
                                    			pc_man_df$DiffArea[pc_man_df$Scenario == scen_lnames[s]] = 
                                    				pc_man_df$Value[pc_man_df$Scenario == scen_lnames[s]] -
                                            		pc_man_df$Value[pc_man_df$Scenario == scen_lnames[1]]
                                            	
                                            	cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]] = 0	
                                            	cumarea = c(0, cumsum(cc_man_df$DiffArea[cc_man_df$Scenario == scen_lnames[s]]))
                                            	cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                            	pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]] = 0
                                            	cumarea = c(0, cumsum(pc_man_df$DiffArea[pc_man_df$Scenario == scen_lnames[s]]))
                                            	pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                            	
                                            	check_cc_val = sum(cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]],na.rm = TRUE)
                                            	check_pc_val = sum(pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]],na.rm = TRUE)
                                            	if (check_cc_val != 0 & check_pc_val != 0) {
                                            		# clearcut to partial
                                            		cc_man_df$TranArea[cc_man_df$Scenario == scen_lnames[s]] = -cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]]
                                            	} else if (check_cc_val != 0 & check_pc_val == 0) {
                                            		# clearcut to reserve
                                            		cc_man_df$TranArea[cc_man_df$Scenario == scen_lnames[s]] = -cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]]
                                            	} else if (check_cc_val == 0 & check_pc_val != 0) {
                                            		# partial to reserve
                                            		# put this in the cc data frame for output
                                            		cc_man_df$DiffArea[cc_man_df$Scenario == scen_lnames[s]] = pc_man_df$DiffArea[pc_man_df$Scenario == scen_lnames[s]]
                                            		cc_man_df$CumArea[cc_man_df$Scenario == scen_lnames[s]] = pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]]
                                            		cc_man_df$TranArea[pc_man_df$Scenario == scen_lnames[s]] = -pc_man_df$CumArea[pc_man_df$Scenario == scen_lnames[s]]
                                            	} else {
                                            		cat("\nError in less intensive management: prescription outside of available practices\n")
                                            		stop()
                                            	}
                                    		} # end for s loop over non-base scenarios
                                    		
                                    		plot_df = merge(plot_df, cc_man_df[cc_man_df$Land_Type == lt_lab,], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    		plot_df$Management = "Less_intensive"
                                    		
                                        	plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] =
                                        		plot_df$Value.x[plot_df$Land_Type == lt_lab] /
                                        		plot_df$TranArea[plot_df$Land_Type == lt_lab] / Mg2MMT
                                        	# now add diff per area per year
                                        	plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab] =
                                        		plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] /
                                        		plot_df$CumYears[plot_df$Land_Type == lt_lab]
                                        } # end else less intensive management
                                		
                                	} else if (lt_lab == "Developed_all" & length(nrow(man_df)) > 0) {
                                		# do developed growth below with land cover change
                                		# for now check whether urban forest fraction is changing over time to determine practice
                                			# because if the urban forest fraction is changing then do it even if land use change is happening
                                       	# for developed urban forest divide diff by difference in urban forest area between the scenario #2 and the baseline #1
                                        # for developed dead removal divide diff by cumulative dead removal area (this does not have a long-term effect on growth rates)
                                        # urban forest will always have non-zero area because it is a fraction of developed area
                                        # assume that the sum of annual growth over the period or scenarios doesn't cancel itself out
										
										dev_man_data_sub_df = man_df[man_df$Management == "Urban_forest",]
										if (length(nrow(dev_man_data_sub_df)) > 0) {
												# need to check on a scenario basis
												plot_uf_df = merge(plot_df, dev_man_data_sub_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
												plot_dr_df = merge(plot_df, man_df[man_df$Management == "Dead_removal",], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            	for (s in 2:num_scen_names) {
                                            		cumyears = c(0, 0, rep(1,(length(dev_man_data_sub_df$Year[dev_man_data_sub_df$Scenario == scen_lnames[s]])-2)))
                                        			cumyears = cumsum(cumyears)
                                        			dev_man_data_sub_df$CumYears[dev_man_data_sub_df$Scenario == scen_lnames[s]] = cumyears
                                            		dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] = 0
                                            		diffarea = c(0,
                                            			dev_man_data_sub_df$Value[dev_man_data_sub_df$Scenario == scen_lnames[s]] -
                                            			dev_man_data_sub_df$Value[dev_man_data_sub_df$Scenario == scen_lnames[1]] )
                                            		dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]] = diffarea[-length(diffarea)]
                                            		
                                            		# if the difference in urban forest area is different, do it, otherwise do dead removal
                                            		if (sum(dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Scenario == scen_lnames[s]], na.rm = TRUE) > 0) {
                                            			# Urban_forest
                                            			plot_df = plot_uf_df
                                            			plot_df$DiffArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] = 
                                            				dev_man_data_sub_df$DiffArea[dev_man_data_sub_df$Land_Type == "Developed_all" & dev_man_data_sub_df$Scenario == scen_lnames[s]]
                                            			plot_df$CumYears[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] = 
                                            				dev_man_data_sub_df$CumYears[dev_man_data_sub_df$Land_Type == "Developed_all" & dev_man_data_sub_df$Scenario == scen_lnames[s]]
                                            			plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] =
                                            				plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] /
                                            				plot_df$DiffArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] / Mg2MMT
                                            			# now add diff per area per year
                                        				plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab & plot_df$Scenario == scen_lnames[s]] =
                                        					plot_df$DiffPerArea[plot_df$Land_Type == lt_lab & plot_df$Scenario == scen_lnames[s]] /
                                        					plot_df$CumYears[plot_df$Land_Type == lt_lab & plot_df$Scenario == scen_lnames[s]]
                                            		} else {
                                            			# Dead_removal practice
                                    					plot_df = plot_dr_df
                                    					cumyears = c(0,rep(1,(length(plot_df$Year[plot_df$Scenario == scen_lnames[s]])-1)))
                                        				cumyears = cumsum(cumyears)
                                        				plot_df$CumYears[plot_df$Scenario == scen_lnames[s]] = cumyears
                                        				plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = 0
                                        				cumarea = c(0,cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]]))
                                    					plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                    					
                                            			plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] =
                                            				plot_df$Value.x[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] /
                                            				plot_df$Value.y[plot_df$Land_Type == "Developed_all" & plot_df$Scenario == scen_lnames[s]] / Mg2MMT
                                            			# now add diff per area per year
                                        				plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab & plot_df$Scenario == scen_lnames[s]] =
                                        					plot_df$DiffPerArea[plot_df$Land_Type == lt_lab & plot_df$Scenario == scen_lnames[s]] /
                                        					plot_df$CumYears[plot_df$Land_Type == lt_lab & plot_df$Scenario == scen_lnames[s]]
                                            		} # end dead removal if urban forest exists (but isn't expanding in fraction)
                                            					
                                            	} # end for loop over scenarios
                                       	} else { # end if there is urban forest
                                       		 # Dead_removal practice
                                    		plot_df = merge(plot_df, man_df[man_df$Management == "Dead_removal",], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                            for (s in 2:num_scen_names) {
                                            	cumyears = c(0,rep(1,(length(plot_df$Year[plot_df$Scenario == scen_lnames[s]])-1)))
                                        		cumyears = cumsum(cumyears)
                                        		plot_df$CumYears[plot_df$Scenario == scen_lnames[s]] = cumyears
                                        		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = 0
                                        		cumarea = c(0,cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]]))
                                    			plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                    		}
                                            plot_df$DiffPerArea[plot_df$Land_Type == "Developed_all"] =
                                            	plot_df$Value.x[plot_df$Land_Type == "Developed_all"] /
                                            	plot_df$CumArea[plot_df$Land_Type == "Developed_all"] / Mg2MMT
                                            # now add diff per area per year
                                        	plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab] =
                                        		plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] /
                                        		plot_df$CumYears[plot_df$Land_Type == lt_lab]
                                       	} # end else just do dead removal cuz no urban forest
                                        
                                	} else if (lt_lab == "All_land" & length(nrow(al_area_df)) > 0) {
                                		# restoration and afforestation and growth
                                		
                                		# need to associate the desired land type difference with the total benefit
                                       	# so first find which land type is driving the change
                                       		# this will be the one that is increasing or decreasing in isolation
                                       	# al_area_df has individual land type areas for this region and ownership, by scenario
                                     
                                		tot_area_scen_df = out_area_df_list[[1]]
                                		tot_area_scen_df$Units = as.character(tot_area_scen_df$Units)
                                		for (s in 2:num_scen_names) {
                                            al_area_df$DiffArea[al_area_df$Scenario == scen_lnames[s]] = 
                                            	al_area_df$Value[al_area_df$Scenario == scen_lnames[s]] -
                                            	al_area_df$Value[al_area_df$Scenario == scen_lnames[1]]
                                			pos_diffs = al_area_df[al_area_df$DiffArea > 0 & al_area_df$Scenario == scen_lnames[s],]
                                			neg_diffs = al_area_df[al_area_df$DiffArea < 0 & al_area_df$Scenario == scen_lnames[s],]
                                			if (nrow(pos_diffs) > nrow(neg_diffs)) {
                                				# the driving land type is decreasing
                                				lt_try = as.character( unique(neg_diffs$Land_Type) )
                                				if (length(lt_try) > 1) {
                                					# this could be difference in forest fire/nonregen due to woodland or meadow restoration
                                					# meadow actually reduces forest because meadow does not burn
                                					# in these two cases there are at least 3 conversion sources, so the pos/neg rows should still hold
                                					# so remove forest and try again
                                					lt_try = as.character( unique(neg_diffs$Land_Type[neg_diffs$Land_Type != "Forest"]) )
                                					if (length(lt_try) > 1) {
                                						cat("\nError: multiple lt drivers\n")
                                						stop()
                                					} else {
                                						lt_drive = lt_try
                                						lt_drive_sign = -1
                                					}
                                				} else {
                                					lt_drive = lt_try
                                					lt_drive_sign = -1
                                				}
                                			} else {
                                				# the driving land type is increasing
                                				lt_try = as.character( unique(pos_diffs$Land_Type) )
                                				if (length(lt_try) > 1) {
                                					# this could be difference in forest fire/nonregen due to woodland or meadow restoration
                                					# meadow actually reduces forest because meadow does not burn
                                					# in these two cases there are at least 3 conversion sources, so the pos/neg rows should still hold
                                					# so remove forest and try again
                                					lt_try = as.character( unique(pos_diffs$Land_Type[pos_diffs$Land_Type != "Forest"]) )
                                					if (length(lt_try) > 1) {
                                						cat("\nError: multiple lt drivers\n")
                                						stop()
                                					} else {
                                						lt_drive = lt_try
                                						lt_drive_sign = 1
                                					}
                                				} else {
                                					lt_drive = lt_try
                                					lt_drive_sign = 1
                                				}
                                			}
                                			
                                			# extract the driving land type and put these values into the all land placeholder df
                                			# assume that the row order stays the same
                                			temp_df = al_area_df[al_area_df$Land_Type == lt_drive,]
                                			
                                			# if nothing is changing in this case then set the values to 0
                                			# units in al_area_df are ha or ac, so get the untis too
                                			if (nrow(temp_df) > 0) {

                                			tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] = temp_df$Value[temp_df$Scenario == scen_lnames[s]]
                                			tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]] = temp_df$Value[temp_df$Scenario == scen_lnames[1]]
                                			
                                			tot_area_scen_df$Units[tot_area_scen_df$Scenario == scen_lnames[s]] = temp_df$Units[temp_df$Scenario == scen_lnames[s]]
                                			tot_area_scen_df$Units[tot_area_scen_df$Scenario == scen_lnames[1]] = temp_df$Units[temp_df$Scenario == scen_lnames[1]]
                                			
                                			if (lt_drive == "Developed_all") {
                                				cumyears = c(0, 0, 0, rep(1,(length(tot_area_scen_df$Year[tot_area_scen_df$Scenario == scen_lnames[s]])-3)))
                                			} else {
                                				cumyears = c(0, 0, rep(1,(length(tot_area_scen_df$Year[tot_area_scen_df$Scenario == scen_lnames[s]])-2)))
                                			}
                                        	cumyears = cumsum(cumyears)
                                        	tot_area_scen_df$CumYears[tot_area_scen_df$Scenario == scen_lnames[s]] = cumyears
                                        	tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] = 0
                                            	diffarea = c(0,
                                            		tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] -
                                            		tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]] )
                                            tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] = lt_drive_sign * diffarea[-length(diffarea)]
                                            
                                            } else {
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[s]] = 0
                                            	tot_area_scen_df$Value[tot_area_scen_df$Scenario == scen_lnames[1]] = 0
                                            	tot_area_scen_df$CumYears[tot_area_scen_df$Scenario == scen_lnames[s]] = 0
                                            	tot_area_scen_df$DiffArea[tot_area_scen_df$Scenario == scen_lnames[s]] = 0
                                            }
                                            
                                       	} # end for s over scenarios
                                       	
                                		plot_df = merge(plot_df, tot_area_scen_df, by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                        plot_df$DiffPerArea[plot_df$Land_Type == "All_land"] =
                                        	plot_df$Value.x[plot_df$Land_Type == "All_land"] /
                                        	plot_df$DiffArea[plot_df$Land_Type == "All_land"] / Mg2MMT
                                        # now add diff per area per year
                                        plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab] =
                                        	plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] /
                                        	plot_df$CumYears[plot_df$Land_Type == lt_lab]
                                	} else if ((lt_lab == "Grassland" | lt_lab == "Savanna" | lt_lab == "Woodland") & length(nrow(man_df)) > 0) {
                                		# rangeland compost management: need cumulative area
                                		# only one type of land and management should be present
                                		plot_df = merge(plot_df, man_df[man_df$Land_Type == lt_lab,], by = c("Scenario", "Region", "Land_Type", "Ownership", "Year"), all.x = TRUE)
                                    	for (s in 2:num_scen_names) {
                                    		cumyears = c(0,rep(1,(length(plot_df$Year[plot_df$Scenario == scen_lnames[s]])-1)))
                                        	cumyears = cumsum(cumyears)
                                        	plot_df$CumYears[plot_df$Scenario == scen_lnames[s]] = cumyears
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = 0
                                        	cumarea = c(0,cumsum(plot_df$Value.y[plot_df$Scenario == scen_lnames[s]]))
                                    		plot_df$CumArea[plot_df$Scenario == scen_lnames[s]] = cumarea[-length(cumarea)]
                                    	}
                                        plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] =
                                        	plot_df$Value.x[plot_df$Land_Type == lt_lab] /
                                        	plot_df$CumArea[plot_df$Land_Type == lt_lab] / Mg2MMT
                                        # now add diff per area per year
                                        plot_df$DiffPerAreaPerYr[plot_df$Land_Type == lt_lab] =
                                        	plot_df$DiffPerArea[plot_df$Land_Type == lt_lab] /
                                        	plot_df$CumYears[plot_df$Land_Type == lt_lab]
                                	} # end if-else land types
                                	
                                	# if DiffPerArea exists so does DiffPerAreaPerYr
                                	if (length(which(names(plot_df) == "DiffPerArea")) > 0) {
                                			                                    
                                    	# deal with NA, NaN and Inf
                                    	na_inds = which(is.na(plot_df$DiffPerArea))
                                   		nan_inds = which(is.nan(plot_df$DiffPerArea))
                                    	inf_inds = which(plot_df$DiffPerArea == Inf)
                                    	ninf_inds = which(plot_df$DiffPerArea == -Inf)
                                    	plot_df$DiffPerArea[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    	# for per year
                                    	na_inds = which(is.na(plot_df$DiffPerAreaPerYr))
                                    	nan_inds = which(is.nan(plot_df$DiffPerAreaPerYr))
                                    	inf_inds = which(plot_df$DiffPerAreaPerYr == Inf)
                                    	ninf_inds = which(plot_df$DiffPerAreaPerYr == -Inf)
                                    	plot_df$DiffPerAreaPerYr[c(na_inds, nan_inds, inf_inds, ninf_inds)] = 0
                                    	
                                    	if (units_ha == TRUE) {
                                   	   		plot_df$units_dpa = "MgCO2eq/ha"
                                    	  	plot_df$units_dpapy = "MgCO2eq/ha/yr"
                                    	} else {
                                   	   		plot_df$units_dpa = "MgCO2eq/ac"
                                    	  	plot_df$units_dpapy = "MgCO2eq/ac/yr"
                                    	}
                                    
                                    	# plot the cumulative per area benefit
                                    	if (units_ha == TRUE) {
                                    	  	p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                    	       	+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                    	        + geom_line(size = 0.3)
                                    	        + geom_point(aes(shape=Scenario), size = 1.5)
                                       		    + ylab( paste( "Change from Baseline (Mg CO2eq per ha)" ) )
                                             	+ theme(legend.key.size = unit(0.4,"cm"))
                                             	+ ggtitle(paste(reg_lab, lt_lab, own_lab, cum_ghg_sheets[i], "Change from Baseline per man area"))
                                      	  	)
                                    	} else {
                                     	  	p <- ( ggplot(plot_df, aes(Year, DiffPerArea, color=Scenario))
                                             	+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             	+ geom_line(size = 0.3)
                                             	+ geom_point(aes(shape=Scenario), size = 1.5)
                                             	+ ylab( paste( "Change from Baseline (Mg CO2eq per ac)" ) )
                                            	 + theme(legend.key.size = unit(0.4,"cm"))
                                             	+ ggtitle(paste(reg_lab, lt_lab, own_lab, cum_ghg_sheets[i], "Change from Baseline per man area"))
                                      	  	)
                                    	}
                                    
                            				p$save_args <- FIGURE_DIMS
                            				#print(p)
                            				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_diffperarea_output.pdf")
                            				do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_diffperarea_output.csv")
                            				write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            			
                            				# plot the per area per year benefit
                                    	if (units_ha == TRUE) {
                                     		p <- ( ggplot(plot_df, aes(Year, DiffPerAreaPerYr, color=Scenario))
                                            	+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                             	+ geom_line(size = 0.3)
                                             	+ geom_point(aes(shape=Scenario), size = 1.5)
                                             	+ ylab( paste( "Change from Baseline (Mg CO2eq per ha per yr)" ) )
                                             	+ theme(legend.key.size = unit(0.4,"cm"))
                                             	+ ggtitle(paste(reg_lab, lt_lab, own_lab, cum_ghg_sheets[i], "Change from Baseline per man area per yr"))
                                      		)
                                    	} else {
                                      		p <- ( ggplot(plot_df, aes(Year, DiffPerAreaPerYr, color=Scenario))
                                             	+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                                            	 + geom_line(size = 0.3)
                                             	+ geom_point(aes(shape=Scenario), size = 1.5)
                                             	+ ylab( paste( "Change from Baseline (Mg CO2eq per ac per yr)" ) )
                                            	 + theme(legend.key.size = unit(0.4,"cm"))
                                             	+ ggtitle(paste(reg_lab, lt_lab, own_lab, cum_ghg_sheets[i], "Change from Baseline per man area per yr per "))
                                      		)
                                    	}
                                    
                            				p$save_args <- FIGURE_DIMS
                            				#print(p)
                            				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", cum_ghg_sheets[i], "_diffperareaperyr_output.pdf")
                            				do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            			} # end if DiffPerArea and DiffPerAreaPerYr exist
                                } # end if per area effect of individual practice
                          } # end if plotting BC or plotting another GHG species
                        } # end plot cumulative ghg line plot comparisons
                        
                        # plot the annual ghg species bar graphs
                        
                        out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", "ann_ghg_species_output.pdf")
                        
                        # assign the first annual ghg species data ("Total_AnnCO2") to plot_df
                        plot_df = out_ann_ghg_df_list[[start_spec_ann]][out_ann_ghg_df_list[[start_spec_ann]][,"Land_Type"] == 
                                                                          lt_lab & out_ann_ghg_df_list[[start_spec_ann]][,"Ownership"] == own_lab,]
                        # assign "Total_AnnCO2" to Component column
                        plot_df$Component = ann_ghg_sheets[start_spec_ann]
                        
                        # for "Total_AnnCH4eq" and "Total_AnnBCeq" do the following
                        for (i in (start_spec_ann+1):end_spec_ann) {
                          # skip this if not plotting BC and on "Total_AnnBCeq"
                          if (blackC_plot == TRUE | blackC_plot == FALSE & !(names(out_ann_ghg_df_list)[[i]] %in% c("Total_AnnBCeq", "Wildfire_AnnBCeq",          
                                                                                                                    "ManTotEnergy_AnnBCeq", "LCCTotEnergy_AnnBCeq",      
                                                                                                                    "ManFire_AnnBCeq", "LCCFire_AnnBCeq"))) { 
                            # assign the species-specific GHG (CH4 or BC) to temp_df
                            temp_df = out_ann_ghg_df_list[[i]][out_ann_ghg_df_list[[i]][,"Land_Type"] == lt_lab &
                            out_ann_ghg_df_list[[i]][,"Ownership"] == own_lab,]
                            # assign it's name to "Component" column 
                            temp_df$Component = ann_ghg_sheets[i]
                            # add the data to plot_df (this will eventually have all 3 ghg's unless not plotting BC, then only 2)
                            plot_df = rbind(plot_df, temp_df)
                          } # end plot depending on whether BC is excluded from plots
                        } 
                        plot_df = plot_df[(plot_df $Year == 2020 | plot_df $Year == 2030 | plot_df $Year == 2040 | plot_df $Year == 2050),]
                        # use the short scenario names
                        plot_df$Scenario <- factor(plot_df$Scenario, levels = c(scen_lnames, scen_snames))
                        for (i in 1:num_scen_names) {
                            plot_df$Scenario[plot_df$Scenario == scen_lnames[i]] = scen_snames[i]
                        }
                        plot_df_pos = plot_df[plot_df$Value >= 0,]
                        plot_df_pos = plot_df_pos[order(plot_df_pos$Component),]
                        plot_df_neg = plot_df[plot_df$Value < 0,]
                        plot_df_neg = plot_df_neg[order(plot_df_neg$Component),]
                        # convert Component to factor so it has levels
                        #plot_df$Component <- factor(plot_df$Component)
                        #breaks <- c(levels(plot_df$Component))
                        ymax = max(plot_df_pos$Value)
                        ymin = min(plot_df_neg$Value)
                        amax = max(abs(ymax),abs(ymin))
                        if (blackC_plot == TRUE) { 
                          # plot all 3 GHG's 
                        p <- ( ggplot()
                        + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                        + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                        + facet_grid(~Year)
                        + ylab(paste("MMT CO2-eq per year"))
                        + ggtitle(paste(reg_lab, lt_lab, own_lab, ": Annual GWP"))
                        + scale_fill_manual(values=c(Total_AnnBCeq = "firebrick3", Total_AnnCH4eq = "dodgerblue3", Total_AnnCO2 = "gray10"))
                        + theme(axis.text.x = element_text(size=5))
                        + geom_hline(yintercept=0)
                        )
                        } else {
                          # plot only CO2 & CH4 with BC lumped into CO2
                          p <- ( ggplot()
                                 + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                                 + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                                 + facet_grid(~Year)
                                 + ylab(paste("MMT CO2-eq per year"))
                                 + ggtitle(paste(reg_lab, lt_lab, own_lab, ": Annual GWP (*Black C counted as CO2)"))
                                 + scale_fill_manual(values=c(Total_AnnCH4eq = "dodgerblue3", Total_AnnCO2 = "gray10"))
                                 + theme(axis.text.x = element_text(size=5))
                                 + geom_hline(yintercept=0)
                          )
                        } 
                        #print(p)
                        p$save_args <- FIGURE_DIMS
                        do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                        out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", "ann_ghg_species_output.csv")
                        write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                        
                        # plot the cumulative ghg species bar graphs
                        
                        out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", "cum_ghg_species_output.pdf")
                        plot_df = out_cum_ghg_df_list[[start_spec_cum]][out_cum_ghg_df_list[[start_spec_cum]][,"Land_Type"] ==
                        lt_lab & out_cum_ghg_df_list[[start_spec_cum]][,"Ownership"] == own_lab,]
                        plot_df$Component = cum_ghg_sheets[start_spec_cum]
                        for (i in (start_spec_cum+1):end_spec_cum) {
                          # skip this if not plotting BC and on "Total_AnnBCeq"
                          if (blackC_plot == TRUE | blackC_plot == FALSE & !(names(out_cum_ghg_df_list)[[i]] %in% c("Total_CumBCeq", "Wildfire_CumBCeq",          
                                                                                                                    "ManTotEnergy_CumBCeq", "LCCTotEnergy_CumBCeq",      
                                                                                                                    "ManFire_CumBCeq", "LCCFire_CumBCeq"))) { 
                            temp_df = out_cum_ghg_df_list[[i]][out_cum_ghg_df_list[[i]][,"Land_Type"] ==
                            lt_lab & out_cum_ghg_df_list[[i]][,"Ownership"] == own_lab,]
                            temp_df$Component = cum_ghg_sheets[i]
                            plot_df = rbind(plot_df, temp_df)
                          } # end plotting of ghg species dependent on whether BC is selected to be plotted
                        }
                        plot_df = plot_df[(plot_df $Year == 2020 | plot_df $Year == 2030 | plot_df $Year == 2040 | plot_df $Year == 2050),]
                        # use the short scenario names
                        plot_df$Scenario <- factor(plot_df$Scenario, levels = c(scen_lnames, scen_snames))
                        for (i in 1:num_scen_names) {
                            plot_df$Scenario[plot_df$Scenario == scen_lnames[i]] = scen_snames[i]
                        }
                        plot_df_pos = plot_df[plot_df$Value >= 0,]
                        plot_df_pos = plot_df_pos[order(plot_df_pos$Component),]
                        plot_df_neg = plot_df[plot_df$Value < 0,]
                        plot_df_neg = plot_df_neg[order(plot_df_neg$Component),]
                        breaks <- c(levels(plot_df$Component))
                        ymax = max(plot_df_pos$Value)
                        ymin = min(plot_df_neg$Value)
                        amax = max(abs(ymax),abs(ymin))
                        
                        if (blackC_plot == TRUE) { 
                        # plot all 3 GHG's
                        p <- ( ggplot()
                        + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                        + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                        + facet_grid(~Year)
                        + ylab(paste("MMT CO2-eq"))
                        + ggtitle(paste(reg_lab, lt_lab, own_lab, ": Cumulative GWP"))
                        + scale_fill_manual(values=c(Total_CumBCeq = "firebrick3", Total_CumCH4eq = "dodgerblue3", Total_CumCO2 = "gray10"))
                        + theme(axis.text.x = element_text(size=5))
                        + geom_hline(yintercept=0)
                        ) 
                        } else {
                          # plot only CO2 and CH4 as BC is lumped into CO2
                          p <- ( ggplot()
                                 + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                                 + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                                 + facet_grid(~Year)
                                 + ylab(paste("MMT CO2-eq"))
                                 + ggtitle(paste(reg_lab, lt_lab, own_lab, ": Cumulative GWP (*Black C counted as CO2)"))
                                 + scale_fill_manual(values=c(Total_CumCH4eq = "dodgerblue3", Total_CumCO2 = "gray10"))
                                 + theme(axis.text.x = element_text(size=5))
                                 + geom_hline(yintercept=0)
                          ) 
                        }
                        #print(p)
                        p$save_args <- FIGURE_DIMS
                        do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                        out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", "cum_ghg_species_output.csv")
                        write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                        
                        
                        # the data frames for C component bar graphs do not exist for seagrass
                        if (lt_lab != "Seagrass") {
                            
                            # cumulative component bar graph
                            # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_component_output.pdf")
                            plot_df = cum_comp_df[(cum_comp_df$Year == 2020 | cum_comp_df$Year == 2030 | cum_comp_df$Year == 2040 | cum_comp_df$Year == 2050),]
                            plot_df$Component <- factor(plot_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain",
                            "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage",
                            "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                            plot_df$Component[is.na(plot_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] = plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] -
                            plot_df$Value[plot_df$Component == "Net_Wood_Gain"]
                            plot_df_pos = plot_df[plot_df$Value >= 0,]
                            plot_df_pos = plot_df_pos[order(plot_df_pos$Component),]
                            plot_df_neg = plot_df[plot_df$Value < 0,]
                            plot_df_neg = plot_df_neg[order(plot_df_neg$Component),]
                            breaks <- c(levels(plot_df$Component))
                            brew <- c(brewer.pal(length(unique(plot_df$Component)), "Blues"))
                            brew <- brew[order(xtfrm(breaks))]
                            ymax = max(plot_df_pos$Value)
                            ymin = min(plot_df_neg$Value)
                            amax = max(abs(ymax),abs(ymin))
                            p <- ( ggplot()
                            + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + facet_grid(~Year)
                            + ylab(paste("MMT C"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Components of Net Cumulative Carbon Retention"))
                            #+ scale_fill_manual(values=brew, breaks=breaks)
                            + theme(axis.text.x = element_text(size=5))
                            #+ scale_y_continuous(limits=c(-amax,amax))
                            + geom_hline(yintercept=0)
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_component_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            
                            # cumulative component bar graph differences
                            # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_component_diff_output.pdf")
                            temp_df = cum_comp_df[(cum_comp_df$Year == 2020 | cum_comp_df$Year == 2030 | cum_comp_df$Year == 2040 | cum_comp_df$Year == 2050),]
                            temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood",
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC",
                            "Loss_to_Atmos_from_Fire"))
                            temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] -
                            temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_snames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_snames[s]] - temp_df$Value[temp_df$Scenario == scen_snames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            plot_df_pos = plot_df[plot_df$Value >= 0,]
                            plot_df_pos = plot_df_pos[order(plot_df_pos$Component),]
                            plot_df_neg = plot_df[plot_df$Value < 0,]
                            plot_df_neg = plot_df_neg[order(plot_df_neg$Component),]
                            breaks <- c(levels(plot_df$Component))
                            brew <- c(brewer.pal(length(unique(plot_df$Component)), "Blues"))
                            #brew <- brew[order(xtfrm(breaks))]
                            ymax = sum(plot_df_pos$Value)
                            ymin = sum(plot_df_neg$Value)
                            amax = max(abs(ymax),abs(ymin))
                            p <- ( ggplot()
                            + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + facet_grid(~Year)
                            + ylab(paste("Change from Baseline (MMT C)"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Change in Components of Net Cumulative Carbon Retention"))
                            #+ scale_fill_manual(values=brew, breaks=breaks)
                            + theme(axis.text.x = element_text(size=5))
                            #+ scale_y_continuous(limits=c(-amax,amax))
                            + geom_hline(yintercept=0)
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_component_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # also write the whole series of cum component values
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_component_all.csv")
                            write.csv(cum_comp_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # annual component bar graph
                            # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_component_output.pdf")
                            plot_df = ann_comp_df[(ann_comp_df$Year == 2020 | ann_comp_df$Year == 2030 | ann_comp_df$Year == 2040 | ann_comp_df$Year == 2050),]
                            plot_df$Component <- factor(plot_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", 
                            "Loss_to_Atmos_from_Fire"))
                            plot_df$Component[is.na(plot_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] = plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] - 
                            plot_df$Value[plot_df$Component == "Net_Wood_Gain"]
                            plot_df_pos = plot_df[plot_df$Value >= 0,]
                            plot_df_pos = plot_df_pos[order(plot_df_pos$Component),]
                            plot_df_neg = plot_df[plot_df$Value < 0,]
                            plot_df_neg = plot_df_neg[order(plot_df_neg$Component),]
                            breaks <- c(levels(plot_df$Component))
                            brew <- c(brewer.pal(length(unique(plot_df$Component)), "Blues"))
                            brew <- brew[order(xtfrm(breaks))]
                            ymax = max(plot_df_pos$Value)
                            ymin = min(plot_df_neg$Value)
                            amax = max(abs(ymax),abs(ymin))
                            p <- ( ggplot()
                            + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + facet_grid(~Year)
                            + ylab(paste("MMT C per year"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Components of Net Annual Carbon Retention"))
                            #+ scale_fill_manual(values=brew, breaks=breaks)
                            + theme(axis.text.x = element_text(size=5))
                            #+ scale_y_continuous(limits=c(-amax,amax))
                            + geom_hline(yintercept=0)
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_component_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            
                            # annual component bar graph differences
                            # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_component_diff_output.pdf")
                            temp_df = ann_comp_df[(ann_comp_df$Year == 2020 | ann_comp_df$Year == 2030 | ann_comp_df$Year == 2040 | ann_comp_df$Year == 2050),]
                            temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", 
                            "Loss_to_Atmos_from_Fire"))
                            temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - 
                            temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_snames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_snames[s]] - temp_df$Value[temp_df$Scenario == scen_snames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            plot_df_pos = plot_df[plot_df$Value >= 0,]
                            plot_df_pos = plot_df_pos[order(plot_df_pos$Component),]
                            plot_df_neg = plot_df[plot_df$Value < 0,]
                            plot_df_neg = plot_df_neg[order(plot_df_neg$Component),]
                            breaks <- c(levels(plot_df$Component))
                            brew <- c(brewer.pal(length(unique(plot_df$Component)), "Blues"))
                            #brew <- brew[order(xtfrm(breaks))]
                            ymax = max(plot_df_pos$Value)
                            ymin = min(plot_df_neg$Value)
                            amax = max(abs(ymax),abs(ymin))
                            p <- ( ggplot()
                            + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                            + facet_grid(~Year)
                            + ylab(paste("Change from Baseline (MMT C per year)"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Change in Components of Net Annual Carbon Retention"))
                            #+ scale_fill_manual(values=brew, breaks=breaks)
                            + theme(axis.text.x = element_text(size=5))
                            #+ scale_y_continuous(limits=c(-amax,amax))
                            + geom_hline(yintercept=0)
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_component_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # also write the whole series of ann component values
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_component_all.csv")
                            write.csv(ann_comp_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            
                            #### component sum line graphs
                            # these are the changes over time from the initial state
                            # these are the landscape c stock plus the net wood products
                            # and the net annual values are output here as well
                            
                            # net cumulative change line graph
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_change_output.pdf")
                            temp_df = cum_comp_df
                            temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", 
                            "Loss_to_Atmos_from_Fire"))
                            temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - 
                            temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                            plot_df = aggregate(Value ~ Scenario + Land_Type + Units + Year, data=temp_df, FUN=sum)
                            plot_df = plot_df[order(plot_df$Scenario),]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT C" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Landscape and wood C change from ", start_year))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_change_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # net cumulative change, difference from baseline line graph
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_change_diff_output.pdf")
                            temp_df = cum_comp_df
                            temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", 
                            "Loss_to_Atmos_from_Fire"))
                            temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - 
                            temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                            temp_df = aggregate(Value ~ Scenario + Land_Type + Units + Year, data=temp_df, FUN=sum)
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_snames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_snames[s]] - temp_df$Value[temp_df$Scenario == scen_snames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            plot_df = plot_df[order(plot_df$Scenario),]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT C)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Landscape and wood C change from ", start_year, ", wrt Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_cumulative_change_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            
                            # net annual retention line graph
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_retain_output.pdf")
                            temp_df = ann_comp_df
                            temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", 
                            "Loss_to_Atmos_from_Fire"))
                            temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - 
                            temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                            plot_df = aggregate(Value ~ Scenario + Land_Type + Units + Year, data=temp_df, FUN=sum)
                            plot_df = plot_df[order(plot_df$Scenario),]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "MMT C per year" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Landscape and wood C annual retention rate"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_retain_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                            # net annual retention, difference from baseline line graph
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_retain_diff_output.pdf")
                            temp_df = ann_comp_df
                            temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                            "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", 
                            "Loss_to_Atmos_from_Fire"))
                            temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                            temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - 
                            temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                            temp_df = aggregate(Value ~ Scenario + Land_Type + Units + Year, data=temp_df, FUN=sum)
                            plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
                            for (s in 2:num_scen_names) {
                                diff_df = temp_df[temp_df$Scenario == scen_snames[s],]
                                diff_df$Value = temp_df$Value[temp_df$Scenario == scen_snames[s]] - temp_df$Value[temp_df$Scenario == scen_snames[1]]
                                plot_df = rbind(plot_df, diff_df)
                            }
                            plot_df = plot_df[order(plot_df$Scenario),]
                            p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                            + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                            + geom_line(size = 0.3)
                            + geom_point(aes(shape=Scenario), size = 1.5)
                            + ylab( paste( "Change from Baseline (MMT C per year)" ) )
                            + theme(legend.key.size = unit(0.4,"cm"))
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Landscape and wood C annual retention rate, wrt Baseline"))
                            )
                            p$save_args <- FIGURE_DIMS
                            #print(p)
                            do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_annual_retain_diff_output.csv")
                            write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                            
                        } # end if not ocean or seagrass
               #     } # end if All_own or a specific region and land type (not All_region and not All_land)
                   
                } # end o loop over ownerships
                
            } # end if the region-land type combo may exist in the output file
            
        } # end l loop over land types
        
    } # end r loop over regions
    
    cat("Finish plot_caland() at", date(), "\n")
    
} # end function plot_caland()
