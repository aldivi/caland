# plot_caland.r

# make diagnostic plots (.pdf files) and associated text files (.csv files) of two or more CALAND scenario outputs

# this script loops over all the regions and land types
#  the ownerships will be aggregated
#  note that Seagrass has non-zero values only for all c stock == soil c stock, and cum and ann eco c gain

# get only the year columns (not the change column)

# the model output files for plotting are in caland/<data_dir>/
# the plots are put into caland/<data_dir>/<figdir>/ within each region, land type, and ownership directory
#	where <data_dir> and <figdir> are arguments to the function

# plot_caland() has 10 arguments:
#	scen_fnames		array of scenario output file names; assumed to be in data_dir
#	scen_lnames		array of scenario labels associated with scen_fnames
#	scen_snames		array of scenario short lables associated with scen_fnames (up to 8 character labels for bar graphs) *must be different from scen_lnames
#	data_dir		the path to the directory containing the caland output files; do not include the "/" character at the end; default is "./outputs"
#	reg				array of regions to plot; can be any number of available regions (all are below as default)
#	lt				array of land types to plot; can be any number of available types (all are below as default)
#	own				array of ownerships to plot; can be any number of available types (only "All_own" is the default)
#	figdir			the directory within data_dir to write the figures; do not include the "/" character at the end
#	INDIVIDUAL		TRUE = output per area effects of individual practices based on model runs configured for this purpose
# units     TRUE = output units in "ha", FALSE = "ac". Default is "ha".

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
options(java.parameters = "-Xmx4g" )

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
data_dir = "./outputs"
scen_fnames = c("Baseline_frst2Xmort_fire_output_mean.xls","BAU_EcoFlux_frst2Xmort_fire_output_mean.xls") 
scen_lnames = c("Baseline","Eco-Only")
scen_snames = c("BASE","ECO")
lt="All_land"
own = "All_own"
figdir = "figures"
INDIVIDUAL = TRUE
units = FALSE
reg="All_region"
reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast",
        "Ocean", "All_region")
lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow",
       "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass", "All_land")
#own = c("All_own", "BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")

scen_fnames = c("Baseline_frst2Xmort_fire_output_mean.xls", "LowProtect_BaseManage_frst2Xmort_fire_output_mean.xls",
                "HighProtect_BaseManage_frst2Xmort_fire_output_mean.xls", "BaseProtect_LowManage_frst2Xmort_fire_output_mean.xls",
                "BaseProtect_HighManage_frst2Xmort_fire_output_mean.xls")
scen_lnames = c("Baseline", "LowProtect", "HighProtect", "LowManage", "HighManage")
scen_snames = c("BASE", "LPBM", "HPBM", "BPLM", "BPHM")

############# main function

plot_caland <- function(scen_fnames, scen_lnames, scen_snames, data_dir = "./outputs", reg = c("Central_Coast", "Central_Valley",
"Delta", "Deserts", "Eastside", "Klamath",
"North_Coast", "Sierra_Cascades",
"South_Coast", "Ocean", "All_region"),
lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest",
"Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated",  "Developed_all", "Seagrass", "All_land"),
own = c("All_own"), figdir = "figures", INDIVIDUAL = FALSE, units=TRUE) {
    
    cat("Start plot_caland() at", date(), "\n")
    
    outputdir = paste0(data_dir, "/")
    num_scen_names = length(scen_fnames)
    num_lt = length(lt)
    num_reg = length(reg)
    num_own = length(own)
    
    theme_set( theme_bw() )
    
    FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)
    
    # Mg C to Million Metric tons C
    Mg2MMT = 1 / 1000000
    
    # ha (or ac) to Thousand ha
    ha2kha = 1 / 1000
    
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
    for (r in 1:num_reg){
        
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
                    
                    # All_own is the default that sums all the ownerships, and it exists for All_region plus each individual land type
                    #  and for All_land plus each individual region
                    # the individual ownerships are plotted only for individual regions and land types
                    
                    if (own_lab == "All_own" | (reg_lab != "All_region" & lt_lab != "All_land")) {
                        
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
                        dev_man_data_df = NULL
                        
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
                                
                                # convert the 3 area sheets to from ha to ac if units == FALSE
                                if (scen_sheets[i] %in% area_sheets & units == FALSE) {
                                  if (i==1) { 
                                  scen_df_list[[i]][,5:(ncol(scen_df_list[[i]])-1)] <- scen_df_list[[i]][,5:(ncol(scen_df_list[[i]])-1)] * 2.47105
                                  } else {
                                    scen_df_list[[i]][,6:(ncol(scen_df_list[[i]])-1)] <- scen_df_list[[i]][,6:(ncol(scen_df_list[[i]])-1)] * 2.47105
                                  }
                                }
                                
                                # get the stock data
                                if (scen_sheets[i] %in% stock_sheets) {
                                    oind = which(stock_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # extract and convert the values for this region
                                    
                                    # All_own
                                    if (own_lab == "All_own") {
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                        startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                            # aggregate the ownerships
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                        reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                            # there is only one ownership
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                        }
                                    } else { # single ownership
                                        # only one or zero rows of this ownership in this land type and region
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                        lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                            # there is only one ownership
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
                                    out_stock_df_list[[oind]] = rbind(out_stock_df_list[[oind]],temp_df)
                                    
                                } # end get stock data
                                
                                # get the annual data
                                if (scen_sheets[i] %in% ann_sheets) {
                                    oind = which(ann_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # extract and convert the values for this region
                                    if (reg_lab != "Ocean" & lt_lab != "Seagrass") {
                                        
                                        # land
                                        
                                        # All_own
                                        if (own_lab == "All_own") {
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                                val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            }
                                        } else { # single ownership
                                            # only one or zero rows of this ownership in this land type and region
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
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
                                                    
                                                    # All_own
                                                    if (own_lab == "All_own") {
                                                        if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                        startcol:(ncol(temp_df)-1)]) > 1) {
                                                            val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] ==
                                                            reg_lab, startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                        } else if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                        startcol:(ncol(temp_df)-1)]) == 1) {
                                                            val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                            startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                            # this land type does not exist in this region
                                                            val_col = 0
                                                        }
                                                    } else { # single ownership
                                                        # only one or zero rows of this ownership in this land type and region
                                                        if (nrow(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] ==
                                                        reg_lab, startcol:(ncol(temp_df)-1)]) == 1) {
                                                            val_col = Mg2MMT * unlist(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] ==
                                                            lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                            # this ownership does not exist in this land type and region
                                                            val_col = 0
                                                        }
                                                    } # end else extract a single ownership
                                                    
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
                                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                    lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                    startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
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
                                        
                                        # all own
                                        if (own_lab == "All_own") {
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] ==
                                                reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                                val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            }
                                        } else { # single ownership
                                            # only one or zero rows of this ownership in this land type and region
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
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
                                                    
                                                    # all own
                                                    if (own_lab == "All_own") {
                                                        if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) > 1) {
                                                            val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                            startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                                                        } else if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) == 1) {
                                                            val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                            startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                            # this land type does not exist in this region
                                                            val_col = 0
                                                        }
                                                    } else { # single ownership
                                                        # only one or zero rows of this ownership in this land type and region
                                                        if (nrow(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab,
                                                        startcol:(ncol(temp_df)-1)]) == 1) {
                                                            val_col = Mg2MMT * unlist(temp_df[temp_df[,"Ownership"] == own_lab & temp_df[,"Land_Type"] ==
                                                            lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) - val_col
                                                        } else {
                                                            # this ownership does not exist in this land type and region
                                                            val_col = 0
                                                        }
                                                    } # end else extract a single ownership
                                                    
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
                                # store the total area data for All_own; it is needed for the density data
                                # also store the individual management areas for Developed_all
                                if (scen_sheets[i] %in% area_sheets) {
                                    oind = which(area_sheets == scen_sheets[i])
                                    if (scen_sheets[i] == "Area") {
                                        startcol = 5
                                    } else {
                                        startcol = 6
                                    }
                                    
                                    # each land type, including seagrass
                                    
                                    # all own
                                    if (own_lab == "All_own") {
                                        # area and wildfire get summed, non-developed manage gets summed, developed manage is assigned dead_removal and its different management areas stored
                                        # wildfire severity is mutually exclusive, so it can be summed across the region
                                        # developed management generally has overlapping areas, so just use dead_removal if it exists
                                        # need to grab the individual ownerships and regions, even for the "All_region" selection!
                                        # drop the change column
                                        area_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                        if (scen_sheets[i] != "Managed_area" | (lt_lab != "Developed_all" & lt_lab != "All_land")) {
                                            # area and wildfire and non-dev managemenent and non-all_land management
                                            # first sum up the ownerships for the direct area values
                                            # more than one row (which means more than one ownership, and/or multiple fire severities)
                                            #   this >1 shouldn't trigger because the All_own label will have only one record that meets these conditions
                                            if (nrow(area_df[area_df[,"Region"] == reg_lab, ]) > 1) {
                                                # this sum works only for Area and Wildfire and for Management not on Developed_all
                                                val_col = ha2kha * unlist(apply(area_df[area_df[,"Region"] == reg_lab,
                                                startcol:ncol(area_df)], 2, sum))
                                            } else if (nrow(area_df[area_df[,"Region"] == reg_lab, ]) == 1) {
                                                # this captures the "All_region" overall areas, but still need the breakdown below for the density data
                                                val_col = ha2kha * unlist(area_df[area_df[,"Region"] == reg_lab,
                                                startcol:ncol(area_df)])
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            }
                                            # store the total area by ownership to area-weight the density data if All_own
                                            # need all regions if the selection is All_region
                                            # need all land types if All_land
                                            # the same extraction is done below for density, and the output tables are structured identically, so can get away with just the data
                                            
                                            if (scen_sheets[i] == "Area") {
                                                if (lt_lab == "All_land") {
                                                    temp_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] != "All_land" & scen_df_list[[i]][,"Region"] != "All_region" &
                                                    scen_df_list[[i]][,"Region"] != "Ocean", 1:(ncol(scen_df_list[[i]])-1)]
                                                } else {temp_df = area_df}
                                                if (reg_lab == "All_region") {
                                                    area_data = temp_df[temp_df[,"Region"] != reg_lab, startcol:ncol(temp_df)]
                                                } else {
                                                    area_data = temp_df[temp_df[,"Region"] == reg_lab, startcol:ncol(temp_df)]
                                                }
                                            } # end if storing area data for density calcs
                                            
                                        } else {
                                            # developed_all and all_land managed area
                                            if (lt_lab == "Developed_all") {
                                                # developed_all
                                                # val col is just dead removal
                                                # keep the managements separate for per area calcs
                                                area_df = area_df[area_df$Region != "All_region",]
                                                
                                            } else {
                                                # All_land
                                                temp_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] != "All_land" & scen_df_list[[i]][,"Region"] != "All_region" &
                                                scen_df_list[[i]][,"Region"] != "Ocean", 1:(ncol(scen_df_list[[i]])-1)]
                                                temp_df = na.omit(temp_df[order(c(temp_df$Ownership)),])
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, area_df$Management)),])
                                                # if All_land first aggregate the land types, but keep the other variables separate
                                                # this will mainly just change the Land_Type to All_land for all rows, I think
                                                #  because each management is unique to a land type
                                                area_df = aggregate(cbind(unlist(temp_df[startcol])) ~ Region + Ownership + Management, data=temp_df, FUN=sum, na.rm = TRUE)
                                                area_df$Land_Type = "All_land"
                                                area_df$Land_Cat_dummy = -1
                                                area_df = area_df[,c(6,1,5,2,3,4)]
                                                area_df[,colnames(temp_df[startcol])] = area_df$V1
                                                area_df$V1 = NULL
                                                for (tc in (startcol+1):(ncol(temp_df))) {
                                                    temp_agg = aggregate(cbind(unlist(temp_df[tc])) ~ Region + Ownership + Management, data=temp_df, FUN=sum, na.rm = TRUE)
                                                    area_df[,colnames(temp_df[tc])] = temp_agg$V1
                                                }
                                                area_df = na.omit(area_df[order(c(area_df$Ownership)),])
                                                area_df = na.omit(area_df[order(c(area_df$Region, area_df$Management)),])
                                            } # end else all_land managed area
                                            
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
                                                dev_man_data_df = rbind(dev_man_data_df, valcol_df)
                                                # store the baseline df for Urban_forest per area calcs
                                                #if (s == 1) { base_dev_man_data_df = dev_man_data_df }
                                                
                                                # Developed_all managed area is different from the rest as it represents total areas for distinctly different things that can overlap
                                                # so set this val_col to the dead removal area
                                                # and store the individual management areas for later use by the individual practice calculations
                                                
                                                # need to get just the value column, but make sure the order is by year by sorting above
                                                deadrem = valcol_df$value[valcol_df$Management == "Dead_removal"]
                                                
                                                if (length(deadrem) > 0) {
                                                    val_col = ha2kha * deadrem
                                                } else { val_col = 0 }
                                                
                                            } else {
                                                # this land type does not exist in this region
                                                val_col = 0
                                            } # end records exist or not, dev and all_land management area
                                            
                                        } # end else dev and all_land managed for all_own
                                        
                                    } else { # single ownership
                                        # individual ownership is not processed for either All_region or All_land, because these records do not exist in the output file
                                        # if area, only one or zero rows of this ownership in this land type and region
                                        # managed and wildfire area can have multiple ownership rows due to management type or severity
                                        # wildfire gets summed, non-developed manage gets summed, developed manage is assigned dead_removal and its different management areas stored
                                        if (scen_sheets[i] != "Managed_area" | lt_lab != "Developed_all") {
                                            # area and wildfire and non-dev managemenent, more than one row (so can be wildfire or management)
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                                val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                scen_df_list[[i]][,"Region"] == reg_lab,
                                                startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1) {
                                                # one row, so area or wildfire or non-dev management
                                                val_col = ha2kha * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                                lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                            } else {
                                                # this ownership does not exist in this land type and region
                                                val_col = 0
                                            } # end area, wildfire, and non-dev management
                                        } else {
                                            # developed_all managed area
                                            
                                            # just use dead_removal for managed, as long as it exists
                                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 0){
                                                deadrem = unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                scen_df_list[[i]][,"Land_Type"] == lt_lab &
                                                scen_df_list[[i]][,"Region"] == reg_lab &
                                                scen_df_list[[i]][,"Management"] == "Dead_removal",
                                                startcol:(ncol(scen_df_list[[i]])-1)])
                                                if (length(deadrem) > 0) {
                                                    val_col = ha2kha * deadrem
                                                } else { val_col = 0 }
                                                
                                                # store the developed_all managed areas for later use by the individual practice calcs
                                                dev_man_data = scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                                scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                                1:(ncol(scen_df_list[[i]])-1)]
                                                
                                                # need to reshape dev_man_data so that there is only one value column, and order it ultimately by year
                                                temp_df = melt(dev_man_data, variable.name = "Year", id.vars = colnames(dev_man_data)[1:(startcol-1)])
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, temp_df$Land_Type, temp_df$Management, temp_df$Year)),])
                                                
                                                
                                                # if All_region need to aggregate the regions, otherwise just extract the region of interest
                                                # All_region should never get here because a check above restricts the single own, all region combo
                                                if (reg_lab == "All_region") {
                                                    temp_df = aggregate(value ~ Land_Type + Management + Year, data= temp_df, FUN=sum, na.rm = TRUE)
                                                    temp_df$Region ="All_region"
                                                } else {
                                                    temp_df = temp_df[temp_df$Region == reg_lab,]
                                                }
                                                
                                                # order the management area table ultimately by year
                                                temp_df = na.omit(temp_df[order(c(temp_df$Region, temp_df$Land_Type, temp_df$Management, temp_df$Year)),])
                                                # store all the scenarios
                                                temp_df = data.frame(Scenario = rep(scen_lnames[s], length(temp_df$Year)), temp_df, stringsAsFactors = FALSE)
                                                dev_man_data_df = rbind(dev_man_data_df, temp_df)
                                            } else {
                                                # this ownership does not exist in this land type and region
                                                val_col = 0
                                            }
                                        } # end else deveoped_all management for single ownership
                                    } # end else extract a single ownership
                                    
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    if (units==TRUE){
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
                                    # convert units from Mg/ha to Mg/ac if units==FALSE
                                    if (units==FALSE) {
                                      scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]])-1)] <- scen_df_list[[i]][,startcol:(ncol(scen_df_list[[i]])-1)] / 2.47105
                                    }
                                    # all own
                                    # actually need the individual region/ownerships for area weighting;
                                    #  All_own == own_lab will return only a single record with the straight sum of values across ownerships
                                    # assume the output tables are structured exactly the same, and processed the same, so only the data columns are needed and they line up
                                    # need to deal with All_land and All_region
                                    if (own_lab == "All_own") {
                                        
                                        if (lt_lab == "All_land") {
                                            den_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] != "All_land" & scen_df_list[[i]][,"Region"] != "All_region" &
                                            scen_df_list[[i]][,"Region"] != "Ocean", 1:(ncol(scen_df_list[[i]])-1)]
                                        } else {
                                            den_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                        }
                                      
                                        # get the region of interest
                                        if (reg_lab == "All_region") {
                                            den_data = den_df[den_df[,"Region"] != reg_lab, startcol:ncol(den_df)]
                                        } else {
                                            den_data = den_df[den_df[,"Region"] == reg_lab, startcol:ncol(den_df)]
                                        }
                                        
                                        
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
                                    } else { # single ownership
                                    	den_df = scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab & scen_df_list[[i]][,"Ownership"] == own_lab, 1:(ncol(scen_df_list[[i]])-1)]
                                        # only one or zero rows of this ownership in this land type and region; All_region and All_land do not get here
                                        if (nrow(den_df) == 1){
                                            val_col = unlist(den_df[, startcol:(ncol(den_df))])
                                        } else {
                                            # this ownership does not exist in this land type and region
                                            val_col = 0
                                        }
                                    } # end else extract a single ownership
                                    
                                    scen_col = rep(scen_lnames[s], length(val_col))
                                    reg_col = rep(reg_lab, length(val_col))
                                    lt_col = rep(lt_lab, length(val_col))
                                    own_col = rep(own_lab, length(val_col))
                                    if (units == TRUE){
                                      unit_col = rep(dh_lab, length(val_col))
                                    } else {
                                      unit_col = rep(da_lab, length(val_col))
                                    }
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
                                    out_den_df_list[[oind]] = rbind(out_den_df_list[[oind]],temp_df)
                                    
                                } # end get density data
                                
                                # get the annual ghg data
                                if (scen_sheets[i] %in% ann_ghg_sheets) {
                                    oind = which(ann_ghg_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # all own
                                    if (own_lab == "All_own") {
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                        startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                        reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                            reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                        }
                                    } else { # single ownership
                                        # only one or zero rows of this ownership in this land type and region
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                        lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
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
                                    unit_col = rep(g_lab, length(val_col))
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
                                    out_ann_ghg_df_list[[oind]] = rbind(out_ann_ghg_df_list[[oind]],temp_df)
                                    
                                    # fill the stacked line df for this scenario and this region/land type/ownership and the component variables
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
                                    
                                } # end get annual ghg data
                                
                                # get the cumulative ghg data
                                if (scen_sheets[i] %in% cum_ghg_sheets) {
                                    oind = which(cum_ghg_sheets == scen_sheets[i])
                                    startcol = 5
                                    
                                    # all own
                                    if (own_lab == "All_own") {
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                        startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                            startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] ==
                                        reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] ==
                                            lt_lab & scen_df_list[[i]][,"Region"] == reg_lab,
                                            startcol:(ncol(scen_df_list[[i]])-1)])
                                        } else {
                                            # this land type does not exist in this region
                                            val_col = 0
                                        }
                                    } else { # single ownership
                                        # only one or zero rows of this ownership in this land type and region
                                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab & scen_df_list[[i]][,"Land_Type"] ==
                                        lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Ownership"] == own_lab &
                                            scen_df_list[[i]][,"Land_Type"] ==
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
                                    unit_col = rep(g_lab, length(val_col))
                                    year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                    temp_df = data.frame(Scenario=scen_col, Region=reg_col, Land_Type=lt_col, Ownership=own_col, Units=unit_col, Year=year_col,
                                    Value=val_col)
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
                            ghg_colors = c(Eco_AnnCH4eq  = brew_ch4[2], Eco_AnnCO2  = brew_co2[2], LCCTotEnergy_AnnBCeq = brew_bc[2],
                            LCCTotEnergy_AnnCH4eq = brew_ch4[4], LCCTotEnergy_AnnCO2 = brew_co2[3], LCCNonBurn_AnnCO2 = brew_co2[4],
                            ManTotEnergy_AnnBCeq = brew_bc[4], ManTotEnergy_AnnCH4eq = brew_ch4[6], ManTotEnergy_AnnCO2 = brew_co2[5],
                            ManFire_AnnBCeq = brew_bc[6], ManFire_AnnCH4eq = brew_ch4[8], ManFire_AnnCO2 = brew_co2[6],
                            ManNonBurn_AnnCO2 = brew_co2[7], Wildfire_AnnBCeq = brew_bc[8], Wildfire_AnnCH4eq = brew_ch4[9],
                            Wildfire_AnnCO2 = brew_co2[8], Wood_AnnCH4eq = brew_ch4[9], Wood_AnnCO2 = brew_co2[9])
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
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
                            ghg_colors = c(Eco_CumCH4eq  = brew_ch4[2], Eco_CumCO2  = brew_co2[2], LCCTotEnergy_CumBCeq = brew_bc[2],
                            LCCTotEnergy_CumCH4eq = brew_ch4[4], LCCTotEnergy_CumCO2 = brew_co2[3], LCCNonBurn_CumCO2 = brew_co2[4],
                            ManTotEnergy_CumBCeq = brew_bc[4], ManTotEnergy_CumCH4eq = brew_ch4[6], ManTotEnergy_CumCO2 = brew_co2[5],
                            ManFire_CumBCeq = brew_bc[6], ManFire_CumCH4eq = brew_ch4[8], ManFire_CumCO2 = brew_co2[6],
                            ManNonBurn_CumCO2 = brew_co2[7], Wildfire_CumBCeq = brew_bc[8], Wildfire_CumCH4eq = brew_ch4[9],
                            Wildfire_CumCO2 = brew_co2[8], Wood_CumCH4eq = brew_ch4[9], Wood_CumCO2 = brew_co2[9])
                            cum_ghg_comp_df = na.omit(cum_ghg_comp_df[order(c(cum_ghg_comp_df$Component, cum_ghg_comp_df$Year)),])
                            
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
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
                            if (units == TRUE) { 
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
                            if (units == TRUE) {
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
                            if (units == TRUE) {
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
                            if (units == TRUE) {
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
                                      plot_df$units_dpa = "Mg/ha/ha"
                                    } else {
                                      plot_df$units_dpa = "Mg/ac/ac"
                                    }
                                    
                                    if (units == TRUE) {
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
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
                                      plot_df$units_dpa = "MgCO2eq/ha/yr"
                                    } else {
                                      plot_df$units_dpa = "MgCO2eq/ac/yr"
                                    }
                                    
                                    if (units == TRUE) {
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
                                } # end if cumulative per area effect of individual practice
                        
                        
                        } # end plot annual ghg line plot comparisons
                        
                        # plot the cumulative ghg line plot comparisons
                        for (i in 1:num_plot_cum_ghg_sheets) {
                            
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
                            # if quantifying the per area effecs of a single practice simulation
                                # these diffs are scenario minus baseline emissions,so negative values are a benefit
                                # only apply to individual land types
                                if (INDIVIDUAL & lt_lab != "All_land") {
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
                                    
                                    if (units == TRUE) {
                                      plot_df$units_dpa = "MgCO2eq/ha"
                                    } else {
                                      plot_df$units_dpa = "MgCO2eq/ac"
                                    }
                                    
                                    if (units == TRUE) {
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
                                } # end if cumulative per area effect of individual practice
                        
                        } # end plot cumulative ghg line plot comparisons
                        
                        # plot the annual ghg species bar graphs
                        
                        out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", own_lab, "_", "ann_ghg_species_output.pdf")
                        plot_df = out_ann_ghg_df_list[[start_spec_ann]][out_ann_ghg_df_list[[start_spec_ann]][,"Land_Type"] ==
                        lt_lab & out_ann_ghg_df_list[[start_spec_ann]][,"Ownership"] == own_lab,]
                        plot_df$Component = ann_ghg_sheets[start_spec_ann]
                        for (i in (start_spec_ann+1):end_spec_ann) {
                            temp_df = out_ann_ghg_df_list[[i]][out_ann_ghg_df_list[[i]][,"Land_Type"] == lt_lab &
                            out_ann_ghg_df_list[[i]][,"Ownership"] == own_lab,]
                            temp_df$Component = ann_ghg_sheets[i]
                            plot_df = rbind(plot_df, temp_df)
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
                            temp_df = out_cum_ghg_df_list[[i]][out_cum_ghg_df_list[[i]][,"Land_Type"] ==
                            lt_lab & out_cum_ghg_df_list[[i]][,"Ownership"] == own_lab,]
                            temp_df$Component = cum_ghg_sheets[i]
                            plot_df = rbind(plot_df, temp_df)
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
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Landscape and wood C change from 2010"))
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
                            + ggtitle(paste0(reg_lab, "-", lt_lab, "_", own_lab, ": Landscape and wood C change from 2010, wrt Baseline"))
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
                        
                        
                    } # end if All_own or a specific region and land type (not All_region and not All_land)
                    
                } # end o loop over ownerships
                
            } # end if the region-land type combo may exist in the output file
            
        } # end l loop over land types
        
    } # end r loop over regions
    
    cat("Finish plot_caland() at", date(), "\n")
    
} # end function plot_caland()
