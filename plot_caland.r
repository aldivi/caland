# plot_caland.r

# make diagnostic plots (.pdf files) and associated text files (.csv files) of two or more CALAND scenario outputs

# this script loops over all the regions and land types
#  the ownerships will be aggregated
#  note that Seagrass has non-zero values only for all c stock == soil c stock, and cum and ann eco c gain

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
#	reg				array of regions to plot; can be any number of available regions (all are below as default)
#	lt				array of land types to plot; can be any number of available types (all are below as default)
#	figdir			the directory within caland/outputs/ to write the figures; do not include the "/" character at the end

# notes:
# need at least two scenarios for this to work
# the first scenario in the array is assumed to be the baseline
# scen_fnames, scen_lnames, and scen_snames all need to be equal length vectors
# this takes several hours for 5 scenarios
# it is faster to run four separate instances at once via the command line (assuming you have at least 4 cores)
#	this took about 2.5 hours to complete
#		three instancs each with three of the land regions
#		one instance with the Ocean and All_region regions

#### the output files do not include the carbon transfered into and out of a land category due to lcc
#### so the component diagnostics include only land-atmosphere c exchange
#### the carbon going between land categories is not represented in these diagnostics 

# this enables java to use up to 4GB of memory for reading and writing excel files
options(java.parameters = "-Xmx4g" )

# Load all the required packages
libs <- c( "XLConnect", "ggplot2", "grid", "RColorBrewer")
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it for local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

# set these here so the function does not have to be used
scen_fnames = c("Baseline_frst2Xmort_fire_output_mean.xls", "LowProtect_BaseManage_frst2Xmort_fire_output_mean.xls", "HighProtect_BaseManage_frst2Xmort_fire_output_mean.xls", "BaseProtect_LowManage_frst2Xmort_fire_output_mean.xls", "BaseProtect_HighManage_frst2Xmort_fire_output_mean.xls")
scen_lnames = c("Baseline", "LowProtect", "HighProtect", "LowManage", "HighManage")
scen_snames = c("BASE", "LPBM", "HPBM", "BPLM", "BPHM")
reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "Ocean", "All_region")
lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass", "All_land")
figdir = "test_diags"

############# main function

plot_caland <- function(scen_fnames, scen_lnames, scen_snames, reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "Ocean", "All_region"), lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass", "All_land"), figdir = "test_diags") {

cat("Start plot_caland() at", date(), "\n")

outputdir = "outputs/"
num_scen_names = length(scen_fnames)
num_lt = length(lt)
num_reg = length(reg)

theme_set( theme_bw() )

FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)

# Mg C to Million Metric tons C
Mg2MMT = 1 / 1000000

# ha to Thousand ha
ha2kha = 1 / 1000

c_lab = "MMT C"
a_lab = "kha"
d_lab = "MgC/ha"
g_lab = "MMT CO2-eq"

# these are the sheets for plotting C summary data
stock_sheets = c("All_orgC_stock", "All_biomass_C_stock", "Soil_orgC_stock", "Total_Wood_C_stock", "Total_Atmos_CumGain_C_stock")
num_stock_sheets = length(stock_sheets)
ann_sheets = c("Total_Wood_AnnGain_C_stock", "Eco_AnnGain_C_stock", "Total_Atmos_AnnGain_C_stock", "Manage_Atmos_AnnGain_C_stock", "Fire_Atmos_AnnGain_C_stock", "LCC_Atmos_AnnGain_C_stock", "Wood_Atmos_AnnGain_C_stock")
num_ann_sheets = length(ann_sheets)
cum_sheets = c("Total_Wood_CumGain_C_stock", "Eco_CumGain_C_stock", "Total_Atmos_CumGain_C_stock", "Manage_Atmos_CumGain_C_stock", "Fire_Atmos_CumGain_C_stock", "LCC_Atmos_CumGain_C_stock", "Wood_Atmos_CumGain_C_stock")
num_cum_sheets = length(cum_sheets)
area_sheets = c("Area", "Managed_area", "Wildfire_area")
num_area_sheets = length(area_sheets)
den_sheets = c("All_orgC_den", "All_biomass_C_den", "Above_main_C_den", "Below_main_C_den", "Understory_C_den", "StandDead_C_den", "DownDead_C_den", "Litter_C_den", "Soil_orgC_den")
num_den_sheets = length(den_sheets)
bar_plot_labels = c("Wood_Gain_from_Eco", "Ecosystem_Gain_from_Atmos", "Total_Atmosphere_Gain", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_Fire", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Wood")

# these are the sheets for reading/plotting GHG summary data
# do line plots only for the totals
# do stacked plots for the components by scenario
ann_ghg_sheets = c("Total_AnnCO2eq_all", "TotalEnergy_AnnCO2eq_all", "TotalFire_AnnCO2eq_all", "TotalNonBurn_AnnCO2eq_all", "TotalWood_AnnCO2eq_all", "Total_AnnCO2", "Total_AnnCH4eq", "Total_AnnBCeq", "Wood_AnnCO2", "Wildfire_AnnCO2", "ManEnergy_AnnCO2", "LCCEnergy_AnnCO2", "Eco_AnnCO2", "ManFire_AnnCO2", "ManNonBurn_AnnCO2", "LCCNonBurn_AnnCO2", "Wood_AnnCH4eq", "Wildfire_AnnCH4eq", "ManEnergy_AnnCH4eq", "LCCEnergy_AnnCH4eq", "Eco_AnnCH4eq", "ManFire_AnnCH4eq", "Wildfire_AnnBCeq", "ManEnergy_AnnBCeq", "LCCEnergy_AnnBCeq", "ManFire_AnnBCeq")
num_ann_ghg_sheets = length(ann_ghg_sheets)
num_plot_ann_ghg_sheets = 8
start_spec_ann = 6
end_spec_ann = 8
cum_ghg_sheets = c("Total_CumCO2eq_all", "TotalEnergy_CumCO2eq_all", "TotalFire_CumCO2eq_all", "TotalNonBurn_CumCO2eq_all", "TotalWood_CumCO2eq_all", "Total_CumCO2", "Total_CumCH4eq", "Total_CumBCeq", "Wood_CumCO2", "Wildfire_CumCO2", "ManEnergy_CumCO2", "LCCEnergy_CumCO2", "Eco_CumCO2", "ManFire_CumCO2", "ManNonBurn_CumCO2", "LCCNonBurn_CumCO2", "Wood_CumCH4eq", "Wildfire_CumCH4eq", "ManEnergy_CumCH4eq", "LCCEnergy_CumCH4eq", "Eco_CumCH4eq", "ManFire_CumCH4eq", "Wildfire_CumBCeq", "ManEnergy_CumBCeq", "LCCEnergy_CumBCeq", "ManFire_CumBCeq")
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
        
        if ((reg_lab != "All_region" & reg_lab != "Ocean" & lt_lab != "Seagrass") | (reg_lab == "All_region")  | (reg_lab == "Ocean" & lt_lab == "Seagrass")) {
            
            out_dir = paste0(outputdir, figdir, "/", reg_lab, "/", lt_lab, "/")
            
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
                out_stock_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            for (i in 1:num_ann_sheets) {
                out_ann_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            for (i in 1:num_cum_sheets) {
                out_cum_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            for (i in 1:num_area_sheets) {
                out_area_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            for (i in 1:num_den_sheets) {
                out_den_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            for (i in 1: num_ann_ghg_sheets) {
                out_ann_ghg_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            for (i in 1: num_cum_ghg_sheets) {
                out_cum_ghg_df_list[[i]] <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL)
            }
            names(out_stock_df_list) = stock_sheets
            names(out_ann_df_list) = ann_sheets
            names(out_cum_df_list) = cum_sheets
            names(out_area_df_list) = area_sheets
            names(out_den_df_list) = den_sheets
            names(out_ann_ghg_df_list) = ann_ghg_sheets
            names(out_cum_ghg_df_list) = cum_ghg_sheets
            cum_comp_df <- data.frame(Scenario=NULL, Land_Type=NULL, Component=NULL, Units=NULL, Year=NULL, Value=NULL)
            ann_comp_df <- data.frame(Scenario=NULL, Land_Type=NULL, Component=NULL, Units=NULL, Year=NULL, Value=NULL)
            
            # loop over the scenario outputs to read them in and put the data into data frames for plotting
            # aggregate the ownserhips to the land type
            # only get the year columns (not the change column)
            
            for(s in 1:num_scen_names) {
            	ann_ghg_comp_df <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL, Diff=NULL, Component = NULL)
                cum_ghg_comp_df <- data.frame(Scenario=NULL, Land_Type=NULL, Units=NULL, Year=NULL, Value=NULL, Diff=NULL, Component = NULL)
                
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
                    
                    # get the stock data
                    if (scen_sheets[i] %in% stock_sheets) {
                        oind = which(stock_sheets == scen_sheets[i])
                        startcol = 5
                        
                        # extract and convert the values for this region
                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                            # aggregate the ownerships
                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                            # there is only one ownership
                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                        } else {
                        	# this land type does not exist in this region
                        	val_col = 0
                        }
                        
                        scen_col = rep(scen_lnames[s], length(val_col))
                        lt_col = rep(lt_lab, length(val_col))
                        unit_col = rep(c_lab, length(val_col))
                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                        temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                        out_stock_df_list[[oind]] = rbind(out_stock_df_list[[oind]],temp_df)
                        
                    } # end get stock data
                    
                    # get the annual data
                    if (scen_sheets[i] %in% ann_sheets) {
                        oind = which(ann_sheets == scen_sheets[i])
                        startcol = 5
                        
                        if (reg_lab != "Ocean" & lt_lab != "Seagrass") {
                        	# extract and convert the values for this region
                            # land
                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                            } else {
                        	# this land type does not exist in this region
                        	val_col = 0
                        	}

                           	scen_col = rep(scen_lnames[s], length(val_col))
                           	lt_col = rep(lt_lab, length(val_col))
                           	unit_col = rep(c_lab, length(val_col))
                           	year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                           	temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                           	out_ann_df_list[[oind]] = rbind(out_ann_df_list[[oind]],temp_df)
                           
                           	# bar graph of annual components
                           	# not include the total atmos
                           	# all are negative except eco
                           	# get the net wood c stock rather than the gross gain
                           	#  convert this net wood gain to positive so it represents c retention with eco
                           	scen_col2 = rep(scen_snames[s], length(val_col))
                           	if (ann_sheets[oind] != "Total_Atmos_AnnGain_C_stock") {
                           	    ann_comp_col = rep(bar_plot_labels[oind], length(val_col))
                           	    if (ann_sheets[oind] != "Eco_AnnGain_C_stock") {
                           	        if (ann_sheets[oind] == "Total_Wood_AnnGain_C_stock") {
                           	            # subtract the loss to atmosphere
                           	            lind = which(scen_sheets == "Wood_Atmos_AnnGain_C_stock")
                           	            temp_df <- readWorksheet(scen_wrkbk, lind, startRow = 1)
                           	            # remove the Xs added to the front of the year columns, and get the years as numbers only
                           	            yinds = which(substr(names(temp_df),1,1) == "X")
                           	            names(temp_df)[yinds] = substr(names(temp_df),2,5)[yinds]
                           	            if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) > 1) {
                           	                val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                           	            } else if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) == 1) {
                           	                val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) - val_col
                           	            } else {
                           	            	# this land type does not exist in this region
                        					val_col = 0
                           	            }
                           	            ann_comp_col = rep("Net_Wood_Gain", length(val_col))
                           	        } # end if calc net wood stock gain
                           	        temp_df = data.frame(Scenario=scen_col2, Land_Type=lt_col, Component=ann_comp_col, Units=unit_col, Year=year_col, Value=-val_col)
                           	    } else { # end if not eco gain
                           	        temp_df = data.frame(Scenario=scen_col2, Land_Type=lt_col, Component=ann_comp_col, Units=unit_col, Year=year_col, Value=val_col)
                           	    } # end else eco gain
                           	    ann_comp_df = rbind(ann_comp_df, temp_df)
                           	} # end if not total atmos gain

                        } else {
                            # do seagrass only for ocean and all region
                            if (lt_lab == "Seagrass") {
                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                } else {
                                    val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                }
                                scen_col = rep(scen_lnames[s], length(val_col))
                                lt_col = rep(lt_lab, length(val_col))
                                unit_col = rep(c_lab, length(val_col))
                                year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                                out_ann_df_list[[oind]] = rbind(out_ann_df_list[[oind]],temp_df)
                            } # end if region ocean and seagrass lt
                        } # end else ocean or seagrass
                        
                    } # end get annual data
                    
                    # get the cumulative data
                    if (scen_sheets[i] %in% cum_sheets) {
                        oind = which(cum_sheets == scen_sheets[i])
                        startcol = 5
                        
                        if (reg_lab != "Ocean" & lt_lab != "Seagrass") {
                        	# extract and convert the values for this region
                            # land
                            if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                            } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                                val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                            } else {
                            	# this land type does not exist in this region
                        		val_col = 0
                            }

                           	scen_col = rep(scen_lnames[s], length(val_col))
                           	lt_col = rep(lt_lab, length(val_col))
                           	unit_col = rep(c_lab, length(val_col))
                           	year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                           	temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                           	out_cum_df_list[[oind]] = rbind(out_cum_df_list[[oind]],temp_df)
                            
                           	# bar graph of cumulative components
                           	# not include the total atmos
                           	# all are negative except eco
                           	# get the net wood c stock rather than the gross gain
                           	#  convert this net wood gain to positive so it represents c retention with eco
                           	scen_col2 = rep(scen_snames[s], length(val_col))
                           	if (cum_sheets[oind] != "Total_Atmos_CumGain_C_stock") {
                           	    cum_comp_col = rep(bar_plot_labels[oind], length(val_col))
                           	    if (cum_sheets[oind] != "Eco_CumGain_C_stock") {
                           	        if (cum_sheets[oind] == "Total_Wood_CumGain_C_stock") {
                           	            # subtract the loss to atmosphere
                           	            lind = which(scen_sheets == "Wood_Atmos_CumGain_C_stock")
                           	            temp_df <- readWorksheet(scen_wrkbk, lind, startRow = 1)
                           	            # remove the Xs added to the front of the year columns, and get the years as numbers only
                           	            yinds = which(substr(names(temp_df),1,1) == "X")
                           	            names(temp_df)[yinds] = substr(names(temp_df),2,5)[yinds]
                           	            if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) > 1) {
                           	                val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)], 2, sum)) - val_col
                           	            } else if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) == 1) {
                           	                val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab & temp_df[,"Region"] == reg_lab, startcol:(ncol(temp_df)-1)]) - val_col
                           	            } else {
                           	            	# this land type does not exist in this region
                        					val_col = 0
                           	            }
                           	            cum_comp_col = rep("Net_Wood_Gain", length(val_col))
                           	        } # end if calc net wood stock gain
                           	        temp_df = data.frame(Scenario=scen_col2, Land_Type=lt_col, Component=cum_comp_col, Units=unit_col, Year=year_col, Value=-val_col)
                           	    } else { # end if not eco gain
                           	        temp_df = data.frame(Scenario=scen_col2, Land_Type=lt_col, Component=cum_comp_col, Units=unit_col, Year=year_col, Value=val_col)
                           	    } # end if eco gain
                           	    cum_comp_df = rbind(cum_comp_df, temp_df)
                           	} # end if not total atmos gain

                        } else {
                            # do seagrass only for ocean
                            if (lt_lab == "Seagrass") {
                                if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                                    val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                                } else {
                                    val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                                }
                                scen_col = rep(scen_lnames[s], length(val_col))
                                lt_col = rep(lt_lab, length(val_col))
                                unit_col = rep(c_lab, length(val_col))
                                year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                                temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                                out_cum_df_list[[oind]] = rbind(out_cum_df_list[[oind]],temp_df)
                            } # if ocean region and seagrass lt
                        } # end else ocean or seagrass
                        
                    } # end get cumulative data
                    
                    # get the area data
                    if (scen_sheets[i] %in% area_sheets) {
                        oind = which(area_sheets == scen_sheets[i])
                        if (scen_sheets[i] == "Area" ) {
                            startcol = 5
                        } else {
                            startcol = 6
                        }
                        
                        # each land type, including seagrass
                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                            val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                            val_col = ha2kha * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                        } else {
                        	# this land type does not exist in this region
                            val_col = 0
                        }
                        scen_col = rep(scen_lnames[s], length(val_col))
                        lt_col = rep(lt_lab, length(val_col))
                        unit_col = rep(a_lab, length(val_col))
                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                        temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                        out_area_df_list[[oind]] = rbind(out_area_df_list[[oind]],temp_df)
                    } # end get area data
                    
                    # get the density data
                    if (scen_sheets[i] %in% den_sheets) {
                        oind = which(den_sheets == scen_sheets[i])
                        startcol = 5
                        
                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                            val_col = unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                            val_col = unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                        } else {
                        	# this land type does not exist in this region
                            val_col = 0
                        }
                        scen_col = rep(scen_lnames[s], length(val_col))
                        lt_col = rep(lt_lab, length(val_col))
                        unit_col = rep(d_lab, length(val_col))
                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                        temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                        out_den_df_list[[oind]] = rbind(out_den_df_list[[oind]],temp_df)
                        
                    } # end get density data
                    
                    # get the annual ghg data
                    if (scen_sheets[i] %in% ann_ghg_sheets) {
                        oind = which(ann_ghg_sheets == scen_sheets[i])
                        startcol = 5
                        
                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                        } else {
                        	# this land type does not exist in this region
                            val_col = 0
                        }
                        scen_col = rep(scen_lnames[s], length(val_col))
                        lt_col = rep(lt_lab, length(val_col))
                        unit_col = rep(g_lab, length(val_col))
                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                        temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                        out_ann_ghg_df_list[[oind]] = rbind(out_ann_ghg_df_list[[oind]],temp_df)
                        
                        # fill the stacked line df for this scenario and this land type and the component variables
                        if (oind > num_plot_ann_ghg_sheets & num_plot_ann_ghg_sheets <= num_ann_ghg_sheets) {
                        	# get this scenario data - remove duplicate rows
            				temp_df = out_ann_ghg_df_list[[oind]][out_ann_ghg_df_list[[oind]]$Scenario == scen_lnames[s] & out_ann_ghg_df_list[[oind]]$Land_Type == lt_lab,]
            				temp_df = unique(temp_df)
            				# calculate the difference from the baseline scenario
            				temp_df$Diff = temp_df$Value - out_ann_ghg_df_list[[oind]][out_ann_ghg_df_list[[oind]]$Scenario == scen_lnames[1] & out_ann_ghg_df_list[[oind]]$Land_Type == lt_lab, "Value"]
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
                        
                        if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
                            val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
                        } else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
                            val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab & scen_df_list[[i]][,"Region"] == reg_lab, startcol:(ncol(scen_df_list[[i]])-1)])
                        } else {
                        	# this land type does not exist in this region
                            val_col = 0
                        }
                        scen_col = rep(scen_lnames[s], length(val_col))
                        lt_col = rep(lt_lab, length(val_col))
                        unit_col = rep(g_lab, length(val_col))
                        year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
                        temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
                        out_cum_ghg_df_list[[oind]] = rbind(out_cum_ghg_df_list[[oind]],temp_df)
                        
                        # fill the stacked line df for this scenario and this land type and the component variables
                        if (oind > num_plot_cum_ghg_sheets & num_plot_cum_ghg_sheets <= num_cum_ghg_sheets) {
                        	# get this scenario data - remove duplicate rows
            				temp_df = out_cum_ghg_df_list[[oind]][out_cum_ghg_df_list[[oind]]$Scenario == scen_lnames[s] & out_cum_ghg_df_list[[oind]]$Land_Type == lt_lab,]
            				emp_df = unique(temp_df)
            				# calculate the difference from the baseline scenario
            				temp_df$Diff = temp_df$Value - out_cum_ghg_df_list[[oind]][out_cum_ghg_df_list[[oind]]$Scenario == scen_lnames[1] & out_cum_ghg_df_list[[oind]]$Land_Type == lt_lab, "Value"]
            				# add the variable label
            				temp_df$Component = cum_ghg_sheets[oind]
            				# add this variable to the plot df
            				cum_ghg_comp_df = rbind(cum_ghg_comp_df, temp_df)
                        }
                        
                    } # end get cumulative ghg data
                    
                } # end for i loop over the sheets
                names(scen_df_list) = scen_sheets
                
              	# plot the ghg components as stacked line graphs, by scenario and by ann/cum

		# "Wood_AnnCO2", "Wildfire_AnnCO2", "ManEnergy_AnnCO2", "LCCEnergy_AnnCO2", "Eco_AnnCO2", "ManFire_AnnCO2", "ManNonBurn_AnnCO2", "LCCNonBurn_AnnCO2", "Wood_AnnCH4eq", "Wildfire_AnnCH4eq", "ManEnergy_AnnCH4eq", "LCCEnergy_AnnCH4eq", "Eco_AnnCH4eq", "ManFire_AnnCH4eq", "Wildfire_AnnBCeq", "ManEnergy_AnnBCeq", "LCCEnergy_AnnBCeq", "ManFire_AnnBCeq"

            	# annual ghg components
            	
            	acl = unique(ann_ghg_comp_df$Component)
            	acl=acl[order(acl)]
            	breaks <- acl
          	  	brew_ch4 <- c(brewer.pal(9, "Blues"))
          	  	brew_co2 <- c(brewer.pal(9, "Greys"))
          	  	brew_bc <- c(brewer.pal(9, "Reds"))
          	  	ghg_colors = c(Eco_AnnCH4eq  = brew_ch4[2], Eco_AnnCO2  = brew_co2[2], LCCEnergy_AnnBCeq = brew_bc[2], LCCEnergy_AnnCH4eq = brew_ch4[4], LCCEnergy_AnnCO2 = brew_co2[3], LCCNonBurn_AnnCO2 = brew_co2[4], ManEnergy_AnnBCeq = brew_bc[4], ManEnergy_AnnCH4eq = brew_ch4[6], ManEnergy_AnnCO2 = brew_co2[5], ManFire_AnnBCeq = brew_bc[6], ManFire_AnnCH4eq = brew_ch4[8], ManFire_AnnCO2 = brew_co2[6], ManNonBurn_AnnCO2 = brew_co2[7], Wildfire_AnnBCeq = brew_bc[8], Wildfire_AnnCH4eq = brew_ch4[9], Wildfire_AnnCO2 = brew_co2[8], Wood_AnnCH4eq = brew_ch4[9], Wood_AnnCO2 = brew_co2[9])
          	  	#ghg_colors = c(brew_ch4[2], brew_co2[2], brew_bc[2], brew_ch4[4], brew_co2[3], brew_co2[4], brew_bc[4], brew_ch4[6], brew_co2[5], brew_bc[6], brew_ch4[8], brew_co2[6], brew_co2[7], brew_bc[8], brew_ch4[9], brew_co2[8], brew_ch4[9], brew_co2[9])
            	ann_ghg_comp_df = na.omit(ann_ghg_comp_df[order(c(ann_ghg_comp_df$Component, ann_ghg_comp_df$Year)),])
            	
            	# absolute values
            	p <- ( ggplot(ann_ghg_comp_df,aes(x=Year))
				+ geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Value > 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
				+ geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Value < 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
				+ geom_hline(yintercept=0)
				+ ylab("Absolute (MMT CO2-eq per year)")
				+ ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, ": Contribution to net annual GWP"))
				+ scale_fill_manual(values = ghg_colors, breaks = breaks)
				)
				#print(p)
				
				p$save_args <- FIGURE_DIMS	
				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", scen_lnames[s], "_ghg_ann_comp_output.pdf")
				do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", scen_lnames[s], "_ghg_ann_comp_output.csv")
                write.csv(ann_ghg_comp_df, out_file, quote=FALSE, row.names=FALSE)
            	
            	# difference from baseline
            	if (s > 1) {
            		p <- ( ggplot(ann_ghg_comp_df, aes(x=Year))
					+ geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Diff > 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
					+ geom_area(data = ann_ghg_comp_df[ann_ghg_comp_df$Diff < 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
					+ geom_hline(yintercept=0)
					+ ylab("Change from Baseline (MMT CO2-eq per year)")
					+ ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, ": Contribution to change in net annual GWP"))
					+ scale_fill_manual(values = ghg_colors, breaks = breaks)
					)
					#print(p)
					
					p$save_args <- FIGURE_DIMS
					out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", scen_lnames[s], "_ghg_ann_comp_diff_output.pdf")
					do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )	
            	}
            	
            	# cumulative ghg components
            	
            	ccl = unique(cum_ghg_comp_df$Component)
            	ccl=ccl[order(ccl)]
            	breaks <- ccl
          	  	brew_ch4 <- c(brewer.pal(9, "Blues"))
          	  	brew_co2 <- c(brewer.pal(9, "Greys"))
          	  	brew_bc <- c(brewer.pal(9, "Reds"))
          	  	ghg_colors = c(Eco_CumCH4eq  = brew_ch4[2], Eco_CumCO2  = brew_co2[2], LCCEnergy_CumBCeq = brew_bc[2], LCCEnergy_CumCH4eq = brew_ch4[4], LCCEnergy_CumCO2 = brew_co2[3], LCCNonBurn_CumCO2 = brew_co2[4], ManEnergy_CumBCeq = brew_bc[4], ManEnergy_CumCH4eq = brew_ch4[6], ManEnergy_CumCO2 = brew_co2[5], ManFire_CumBCeq = brew_bc[6], ManFire_CumCH4eq = brew_ch4[8], ManFire_CumCO2 = brew_co2[6], ManNonBurn_CumCO2 = brew_co2[7], Wildfire_CumBCeq = brew_bc[8], Wildfire_CumCH4eq = brew_ch4[9], Wildfire_CumCO2 = brew_co2[8], Wood_CumCH4eq = brew_ch4[9], Wood_CumCO2 = brew_co2[9])
            	cum_ghg_comp_df = na.omit(cum_ghg_comp_df[order(c(cum_ghg_comp_df$Component, cum_ghg_comp_df$Year)),])
            	
            	# absolute values
            	p <- ( ggplot(cum_ghg_comp_df, aes(x=Year))
				+ geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Value > 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
				+ geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Value < 0,], aes(x=Year, y=Value, fill = Component), position = 'stack')
				+ geom_hline(yintercept=0)
				+ ylab("Absolute (MMT CO2-eq)")
				+ ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, ": Contribution to net cumulative GWP"))
				+ scale_fill_manual(values = ghg_colors, breaks = breaks)
				)
				#print(p)
					
				p$save_args <- FIGURE_DIMS
				out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", scen_lnames[s], "_ghg_cum_comp_output.pdf")
				do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", scen_lnames[s], "_ghg_cum_comp_output.csv")
                write.csv(cum_ghg_comp_df, out_file, quote=FALSE, row.names=FALSE)
            	
            	# difference from baseline
            	if (s > 1) {
            		p <- ( ggplot(cum_ghg_comp_df, aes(x=Year))
					+ geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Diff > 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
					+ geom_area(data = cum_ghg_comp_df[cum_ghg_comp_df$Diff < 0,], aes(x=Year, y=Diff, fill = Component), position = 'stack')
					+ geom_hline(yintercept=0)
					+ ylab("Change from Baseline (MMT CO2-eq)")
					+ ggtitle(paste(scen_lnames[s], reg_lab, lt_lab, ": Contribution to change in net cumulative GWP"))
					+ scale_fill_manual(values = ghg_colors, breaks = breaks)
					)
					#print(p)
					
					p$save_args <- FIGURE_DIMS
					out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", scen_lnames[s], "_ghg_cum_comp_diff_output.pdf")
					do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
            	}
                
            } # end s loop over scenario output files
            
            # plot the stock
            for (i in 1:num_stock_sheets) {
                
                # land
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", stock_sheets[i], "_output.pdf")
                plot_df = out_stock_df_list[[i]][out_stock_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT C" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, stock_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", stock_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # land diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", stock_sheets[i], "_diff_output.pdf")
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
                + ggtitle(paste(reg_lab, lt_lab, stock_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", stock_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
            } # end plot stock
            
            # plot the annual
            for (i in 1:num_ann_sheets) {
                
                # land
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_sheets[i], "_output.pdf")
                plot_df = out_ann_df_list[[i]][out_ann_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT C per year" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, ann_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # land diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_sheets[i], "_diff_output.pdf")
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
                + ggtitle(paste(reg_lab, lt_lab, ann_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
            } # end plot annual
            
            # plot the cumulative
            for (i in 1:num_cum_sheets) {
                
                # land line
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_sheets[i], "_output.pdf")
                plot_df = out_cum_df_list[[i]][out_cum_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT C" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, cum_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # land diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_sheets[i], "_diff_output.pdf")
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
                + ggtitle(paste(reg_lab, lt_lab, cum_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
            } # end plot cumulative
          
            # plot the area
            for (i in 1:num_area_sheets) {
                
                # land
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", area_sheets[i], "_output.pdf")
                plot_df = out_area_df_list[[i]][out_area_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "Thousand ha" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, area_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", area_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # land diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", area_sheets[i], "_diff_output.pdf")
                temp_df = out_area_df_list[[i]][out_area_df_list[[i]][,"Land_Type"] == lt_lab,]
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
                + ylab( paste( "Change from Baseline (Thousand ha)" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, area_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", area_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
            } # end plot area
            
            # plot the density
            for (i in 1:num_den_sheets) {
                
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", den_sheets[i], "_output.pdf")
                plot_df = out_den_df_list[[i]][out_den_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MgC per ha" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, den_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", den_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", den_sheets[i], "_diff_output.pdf")
                temp_df = out_den_df_list[[i]][out_den_df_list[[i]][,"Land_Type"] == lt_lab,]
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
                + ylab( paste( "Change from Baseline (MgC per ha)" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, den_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", den_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
            } # end plot density
            
            # plot the annual ghg line plot comparisons
            for (i in 1:num_plot_ann_ghg_sheets) {
                
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_ghg_sheets[i], "_output.pdf")
                plot_df = out_ann_ghg_df_list[[i]][out_ann_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT CO2-eq per year" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, ann_ghg_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_ghg_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_ghg_sheets[i], "_diff_output.pdf")
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
                + ggtitle(paste(reg_lab, lt_lab, ann_ghg_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", ann_ghg_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
            } # end plot annual ghg line plot comparisons
            
            # plot the cumulative ghg line plot comparisons
            for (i in 1:num_plot_cum_ghg_sheets) {
                
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_ghg_sheets[i], "_output.pdf")
                plot_df = out_cum_ghg_df_list[[i]][out_cum_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT CO2-eq" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste(reg_lab, lt_lab, cum_ghg_sheets[i]))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_ghg_sheets[i], "_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # diffs
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_ghg_sheets[i], "_diff_output.pdf")
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
                + ggtitle(paste(reg_lab, lt_lab, cum_ghg_sheets[i], "Change from Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_", cum_ghg_sheets[i], "_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
            } # end plot cumulative ghg line plot comparisons
           
            # plot the annual ghg species bar graphs
            
            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_ann_ghg_species_output.pdf")
            plot_df = out_ann_ghg_df_list[[start_spec_ann]][out_ann_ghg_df_list[[start_spec_ann]][,"Land_Type"] == lt_lab,]
            plot_df$Component = ann_ghg_sheets[start_spec_ann]
            for (i in (start_spec_ann+1):end_spec_ann) {
            	temp_df = out_ann_ghg_df_list[[i]][out_ann_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
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
            brew <- c(brewer.pal(length(unique(plot_df$Component)), "Blues"))
            brew <- brew[order(xtfrm(breaks))]
            ymax = max(plot_df_pos$Value)
            ymin = min(plot_df_neg$Value)
            amax = max(abs(ymax),abs(ymin))
            p <- ( ggplot()
                + geom_bar(data=plot_df, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                + facet_grid(~Year)
                + ylab(paste("MMT CO2-eq per year"))
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Annual GWP"))
                #+ scale_fill_manual(values=brew, breaks=breaks)
                + theme(axis.text.x = element_text(size=5))
                #+ scale_y_continuous(limits=c(-amax,amax))
                + geom_hline(yintercept=0)
         	)
          	p$save_args <- FIGURE_DIMS
       		#print(p)
           	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
           	out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_ann_ghg_species_output.csv")
          	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
          	
          	# plot the cumulative ghg species bar graphs
            
            out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cum_ghg_species_output.pdf")
            plot_df = out_cum_ghg_df_list[[start_spec_cum]][out_cum_ghg_df_list[[start_spec_cum]][,"Land_Type"] == lt_lab,]
            plot_df$Component = cum_ghg_sheets[start_spec_cum]
            for (i in (start_spec_cum+1):end_spec_cum) {
            	temp_df = out_cum_ghg_df_list[[i]][out_cum_ghg_df_list[[i]][,"Land_Type"] == lt_lab,]
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
            brew <- c(brewer.pal(length(unique(plot_df$Component)), "Blues"))
            brew <- brew[order(xtfrm(breaks))]
            ymax = max(plot_df_pos$Value)
            ymin = min(plot_df_neg$Value)
            amax = max(abs(ymax),abs(ymin))
            p <- ( ggplot()
                + geom_bar(data=plot_df_neg, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                + geom_bar(data=plot_df_pos, aes(Scenario, Value, fill=Component), stat="identity", position="stack")
                + facet_grid(~Year)
                + ylab(paste("MMT CO2-eq"))
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Cumulative GWP"))
                #+ scale_fill_manual(values=brew, breaks=breaks)
                + theme(axis.text.x = element_text(size=5))
                #+ scale_y_continuous(limits=c(-amax,amax))
                + geom_hline(yintercept=0)
         	)
          	p$save_args <- FIGURE_DIMS
       		#print(p)
           	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
           	out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cum_ghg_species_output.csv")
          	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
            
            
            # the data frames for C component bar graphs do not exist for seagrass
            if (lt_lab != "Seagrass") {
                
                # cumulative component bar graph
                # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_component_output.pdf")
                plot_df = cum_comp_df[(cum_comp_df$Year == 2020 | cum_comp_df$Year == 2030 | cum_comp_df$Year == 2040 | cum_comp_df$Year == 2050),]
                plot_df$Component <- factor(plot_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                plot_df$Component[is.na(plot_df$Component)] = "Ecosystem_Gain_minus_NWG"
                plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] = plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] - plot_df$Value[plot_df$Component == "Net_Wood_Gain"]
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
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Components of Net Cumulative Carbon Retention"))
                #+ scale_fill_manual(values=brew, breaks=breaks)
                + theme(axis.text.x = element_text(size=5))
                #+ scale_y_continuous(limits=c(-amax,amax))
                + geom_hline(yintercept=0)
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_component_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                
                # cumulative component bar graph differences
                # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_component_diff_output.pdf")
                temp_df = cum_comp_df[(cum_comp_df$Year == 2020 | cum_comp_df$Year == 2030 | cum_comp_df$Year == 2040 | cum_comp_df$Year == 2050),]
                temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
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
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Change in Components of Net Cumulative Carbon Retention"))
                #+ scale_fill_manual(values=brew, breaks=breaks)
                + theme(axis.text.x = element_text(size=5))
                #+ scale_y_continuous(limits=c(-amax,amax))
                + geom_hline(yintercept=0)
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_component_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # also write the whole series of cum component values
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_component_all.csv")
                write.csv(cum_comp_df, out_file, quote=FALSE, row.names=FALSE)
                
                # annual component bar graph
                # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_component_output.pdf")
                plot_df = ann_comp_df[(ann_comp_df$Year == 2020 | ann_comp_df$Year == 2030 | ann_comp_df$Year == 2040 | ann_comp_df$Year == 2050),]
                plot_df$Component <- factor(plot_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                plot_df$Component[is.na(plot_df$Component)] = "Ecosystem_Gain_minus_NWG"
                plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] = plot_df$Value[plot_df$Component == "Ecosystem_Gain_minus_NWG"] - plot_df$Value[plot_df$Component == "Net_Wood_Gain"]
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
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Components of Net Annual Carbon Retention"))
                #+ scale_fill_manual(values=brew, breaks=breaks)
                + theme(axis.text.x = element_text(size=5))
                #+ scale_y_continuous(limits=c(-amax,amax))
                + geom_hline(yintercept=0)
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_component_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                
                # annual component bar graph differences
                # subtract net wood gain from the ecosystem gain because the wood is now on the positive side
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_component_diff_output.pdf")
                temp_df = ann_comp_df[(ann_comp_df$Year == 2020 | ann_comp_df$Year == 2030 | ann_comp_df$Year == 2040 | ann_comp_df$Year == 2050),]
                temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
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
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Change in Components of Net Annual Carbon Retention"))
                #+ scale_fill_manual(values=brew, breaks=breaks)
                + theme(axis.text.x = element_text(size=5))
                #+ scale_y_continuous(limits=c(-amax,amax))
                + geom_hline(yintercept=0)
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_component_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # also write the whole series of ann component values
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_component_all.csv")
                write.csv(ann_comp_df, out_file, quote=FALSE, row.names=FALSE)
                
                
                #### component sum line graphs
                # these are the changes over time from the initial state
                # these are the landscape c stock plus the net wood products
                # and the net annual values are output here as well
                
                # net cumulative change line graph
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_change_output.pdf")
                temp_df = cum_comp_df
                temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                plot_df = aggregate(Value ~ Scenario + Land_Type + Units + Year, data=temp_df, FUN=sum)
                plot_df = plot_df[order(plot_df$Scenario),]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT C" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Landscape and wood C change from 2010"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_change_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # net cumulative change, difference from baseline line graph
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_change_diff_output.pdf")
                temp_df = cum_comp_df
                temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
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
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Landscape and wood C change from 2010, wrt Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_cumulative_change_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                
                # net annual retention line graph
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_retain_output.pdf")
                temp_df = ann_comp_df
                temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
                plot_df = aggregate(Value ~ Scenario + Land_Type + Units + Year, data=temp_df, FUN=sum)
                plot_df = plot_df[order(plot_df$Scenario),]
                p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
                + scale_shape_manual(values=1:nlevels(plot_df$Scenario))
                + geom_line(size = 0.3)
                + geom_point(aes(shape=Scenario), size = 1.5)
                + ylab( paste( "MMT C per year" ) )
                + theme(legend.key.size = unit(0.4,"cm"))
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Landscape and wood C annual retention rate"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_retain_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
                # net annual retention, difference from baseline line graph
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_retain_diff_output.pdf")
                temp_df = ann_comp_df
                temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
                temp_df$Component[is.na(temp_df$Component)] = "Ecosystem_Gain_minus_NWG"
                temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] = temp_df$Value[temp_df$Component == "Ecosystem_Gain_minus_NWG"] - temp_df$Value[temp_df$Component == "Net_Wood_Gain"]
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
                + ggtitle(paste0(reg_lab, "-", lt_lab, ": Landscape and wood C annual retention rate, wrt Baseline"))
                )
                p$save_args <- FIGURE_DIMS
                #print(p)
                do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
                out_file = paste0(out_dir, reg_lab, "_", lt_lab, "_annual_retain_diff_output.csv")
                write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
                
            } # end if not ocean or seagrass
            
        } # end if the region-land type combo may exist in the output file
        
    } # end l loop over land types
    
} # end r loop over regions

cat("Finish plot_caland() at", date(), "\n")

} # end function plot_caland()