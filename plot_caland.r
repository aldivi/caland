# plot_caland.r

# make diagnostic plots (.pdf files) and associated text files (.csv files) of two or more CALAND scenario outputs

# this script loops over all the land types
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
#	lt				array of land types to plot; can be any number of available types (all are below as default)
#	figdir			the directory within caland/outputs/ to write the figures; do not include the "/" character at the end

# notes:
# need at least two scenarios for this to work
# the first scenario in the array is assumed to be the baseline
# scen_fnames, scen_lnames, and scen_snames all need to be equal length vectors
# this takes ~25 minutes for 2 scenarios

plot_caland <- function(scen_fnames, scen_lnames, scen_snames, lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Agriculture", "Developed_all", "Seagrass", "All_land"), figdir = "figures") {

cat("Start plot_ca_carbon_model_lt.r at", date(), "\n")

outputdir = "outputs/"
num_scen_names = length(scen_fnames)
num_lt = length(lt)

# Load all the required packages
libs <- c( "XLConnect", "ggplot2", "grid", "RColorBrewer" )
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it for 
            local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)

# Mg C to Million Metric tons C
Mg2MMT = 1 / 1000000

# ha to Thousand ha
ha2kha = 1 / 1000

c_lab = "MMTC"
a_lab = "kha"

# these are the sheets for plotting summary data
stock_sheets = c("All_orgC_stock", "All_biomass_C_stock", "Soil_orgC_stock", "Total_Wood_C_stock", "Total_Atmos_CumGain_C_stock")
num_stock_sheets = length(stock_sheets)
ann_sheets = c("Total_Wood_AnnGain_C_stock", "Eco_AnnGain_C_stock", "Total_Atmos_AnnGain_C_stock", "Manage_Atmos_AnnGain_C_stock", 
               "Fire_Atmos_AnnGain_C_stock", "LCC_Atmos_AnnGain_C_stock", "Wood_Atmos_AnnGain_C_stock", "Total_AnnCO2", 
               "Total_AnnCH4eq", "Total_AnnBCeq", "Total_AnnCO2eq_all")
num_ann_sheets = length(ann_sheets)
cum_sheets = c("Total_Wood_CumGain_C_stock", "Eco_CumGain_C_stock", "Total_Atmos_CumGain_C_stock", "Manage_Atmos_CumGain_C_stock", 
               "Fire_Atmos_CumGain_C_stock", "LCC_Atmos_CumGain_C_stock", "Wood_Atmos_CumGain_C_stock", "Total_CumCO2",
               "Total_CumCH4eq", "Total_CumBCeq", "Total_CumCO2eq_all")
num_cum_sheets = length(cum_sheets)
area_sheets = c("Area", "Managed_area", "Wildfire_area")
num_area_sheets = length(area_sheets)
den_sheets = c("All_orgC_den", "All_biomass_C_den", "Above_main_C_den", "Below_main_C_den", "Understory_C_den", "StandDead_C_den", 
               "DownDead_C_den", "Litter_C_den", "Soil_orgC_den")
num_den_sheets = length(den_sheets)
bar_plot_labels = c("Wood_Gain_from_Eco", "Ecosystem_Gain_from_Atmos", "Total_Atmosphere_Gain", "Loss_to_Atmos_from_Manage", 
                    "Loss_to_Atmos_from_Fire", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Wood", "Net_GWP_CO2_Emissions",
                    "Net_GWP_CH4_Emissions", "Net_GWP_BC_Emissions", "Net_GWP_All")

# loop over the land types
for (l in 1:num_lt) {

lt_lab = lt[l]

out_dir = paste0(outputdir, figdir, "/", lt_lab, "/")

dir.create(out_dir, recursive=TRUE)

# data frames for plotting
out_stock_df_list <- list()
out_ann_df_list <- list()
out_cum_df_list <- list()
out_area_df_list <- list()
out_den_df_list <- list()
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
names(out_stock_df_list) = stock_sheets
names(out_ann_df_list) = ann_sheets
names(out_cum_df_list) = cum_sheets
names(out_area_df_list) = area_sheets
names(out_den_df_list) = den_sheets
cum_comp_df <- data.frame(Scenario=NULL, Land_Type=NULL, Component=NULL, Units=NULL, Year=NULL, Value=NULL)
ann_comp_df <- data.frame(Scenario=NULL, Land_Type=NULL, Component=NULL, Units=NULL, Year=NULL, Value=NULL)

# loop over the scenario outputs to read them in and put the data into data frames for plotting
# aggregate the ownserhips to the land type
# only get the year columns (not the change column)

for(s in 1:num_scen_names) {
	data_file = paste0(outputdir, scen_fnames[s])
	
	# Load the data file
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
			

			# land
			if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)]) > 1) {
				val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)], 2, sum))
			} else {
				val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)])
			}
			scen_col = rep(scen_lnames[s], length(val_col))
			lt_col = rep(lt_lab, length(val_col))
			unit_col = rep(c_lab, length(val_col))
			year_col = as.numeric(names(scen_df_list[[i]])[4:(ncol(scen_df_list[[i]])-1)])
			temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
			out_stock_df_list[[oind]] = rbind(out_stock_df_list[[oind]],temp_df)

		} # end get stock data
		
		# get the annual data
		if (scen_sheets[i] %in% ann_sheets) {
			oind = which(ann_sheets == scen_sheets[i])
			
			if (lt_lab != "Seagrass") {
				# land
				if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)]) > 1) {
					val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)], 2, sum))
				} else {
					val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)])	
				}
				scen_col = rep(scen_lnames[s], length(val_col))
				lt_col = rep(lt_lab, length(val_col))
				unit_col = rep(c_lab, length(val_col))
				year_col = as.numeric(names(scen_df_list[[i]])[4:(ncol(scen_df_list[[i]])-1)])
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
							if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab,4:(ncol(temp_df)-1)]) > 1) {
								val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab,4:(ncol(temp_df)-1)], 2, sum)) - val_col
							} else {
								val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab,4:(ncol(temp_df)-1)]) - val_col
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
			
				# ocean
				if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)]) > 1) {
					val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)], 2, sum))
				} else {
					val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)])
				}
				scen_col = rep(scen_lnames[s], length(val_col))
				lt_col = rep(lt_lab, length(val_col))
				unit_col = rep(c_lab, length(val_col))
				year_col = as.numeric(names(scen_df_list[[i]])[4:(ncol(scen_df_list[[i]])-1)])
				temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
				out_ann_df_list[[oind]] = rbind(out_ann_df_list[[oind]],temp_df)
			} # end ocean
			
		} # end get annual data
		
		# get the cumulative data
		if (scen_sheets[i] %in% cum_sheets) {
			oind = which(cum_sheets == scen_sheets[i])
			
			if (lt_lab != "Seagrass") {
				# land
				if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)]) > 1) {
					val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)], 2, sum))
				} else {
					val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)])
				}
				scen_col = rep(scen_lnames[s], length(val_col))
				lt_col = rep(lt_lab, length(val_col))
				unit_col = rep(c_lab, length(val_col))
				year_col = as.numeric(names(scen_df_list[[i]])[4:(ncol(scen_df_list[[i]])-1)])
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
							if (nrow(temp_df[temp_df[,"Land_Type"] == lt_lab,4:(ncol(temp_df)-1)]) > 1) {
								val_col = Mg2MMT * unlist(apply(temp_df[temp_df[,"Land_Type"] == lt_lab,4:(ncol(temp_df)-1)], 2, sum)) - val_col
							} else {
								val_col = Mg2MMT * unlist(temp_df[temp_df[,"Land_Type"] == lt_lab,4:(ncol(temp_df)-1)]) - val_col
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
			
				# ocean
				if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)]) > 1) {
					val_col = Mg2MMT * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)], 2, sum))
				} else {
					val_col = Mg2MMT * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,4:(ncol(scen_df_list[[i]])-1)])
				}
				scen_col = rep(scen_lnames[s], length(val_col))
				lt_col = rep(lt_lab, length(val_col))
				unit_col = rep(c_lab, length(val_col))
				year_col = as.numeric(names(scen_df_list[[i]])[4:(ncol(scen_df_list[[i]])-1)])
				temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
				out_cum_df_list[[oind]] = rbind(out_cum_df_list[[oind]],temp_df)
			} # end ocean

		} # end get cumulative data
		
		# get the area data
		if (scen_sheets[i] %in% area_sheets) {
			oind = which(area_sheets == scen_sheets[i])
			if (scen_sheets[i] == "Area" ) {
				startcol = 4
			} else {
				startcol = 6
			}
			
			# land
			if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
				val_col = ha2kha * unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == 
				                                                    lt_lab,startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
			} else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
				val_col = ha2kha * unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)])
			} else {
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
			startcol = 4
			
			if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)]) > 1) {
				val_col = unlist(apply(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)], 2, sum))
			} else if (nrow(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)]) == 1){
				val_col = unlist(scen_df_list[[i]][scen_df_list[[i]][,"Land_Type"] == lt_lab,startcol:(ncol(scen_df_list[[i]])-1)])
			} else {
				val_col = 0
			}
			scen_col = rep(scen_lnames[s], length(val_col))
			lt_col = rep(lt_lab, length(val_col))
			unit_col = rep(a_lab, length(val_col))
			year_col = as.numeric(names(scen_df_list[[i]])[startcol:(ncol(scen_df_list[[i]])-1)])
			temp_df = data.frame(Scenario=scen_col, Land_Type=lt_col, Units=unit_col, Year=year_col, Value=val_col)
			out_den_df_list[[oind]] = rbind(out_den_df_list[[oind]],temp_df)

		} # end get density data
		
	} # end for i loop over the sheets
	names(scen_df_list) = scen_sheets
} # end s loop over scenario output files

theme_set( theme_bw() )

# plot the stock
for (i in 1:num_stock_sheets) {
	
	# land	
	out_file = paste0(out_dir, lt_lab, "_", stock_sheets[i], "_output.pdf")
	plot_df = out_stock_df_list[[i]][out_stock_df_list[[i]][,"Land_Type"] == lt_lab,]
	p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
			+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
			+ geom_line(size = 0.3)
			+ geom_point(aes(shape=Scenario), size = 1.5)
			+ ylab( paste( "MMT C" ) )
			+ theme(legend.key.size = unit(0.4,"cm"))
			+ ggtitle(paste(lt_lab, stock_sheets[i]))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", stock_sheets[i], "_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
	
	# land diffs
	out_file = paste0(out_dir, lt_lab, "_", stock_sheets[i], "_diff_output.pdf")
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
			+ ggtitle(paste(lt_lab, stock_sheets[i], "Change from Baseline"))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", stock_sheets[i], "_diff_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end plot stock

# plot the annual
for (i in 1:num_ann_sheets) {
	
	# land
	out_file = paste0(out_dir, lt_lab, "_", ann_sheets[i], "_output.pdf")
	plot_df = out_ann_df_list[[i]][out_ann_df_list[[i]][,"Land_Type"] == lt_lab,]
	p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
			+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
			+ geom_line(size = 0.3)
			+ geom_point(aes(shape=Scenario), size = 1.5)
			+ ylab( paste( "MMT C per year" ) )
			+ theme(legend.key.size = unit(0.4,"cm"))
			+ ggtitle(paste(lt_lab, ann_sheets[i]))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", ann_sheets[i], "_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
	
	# land diffs
	out_file = paste0(out_dir, lt_lab, "_", ann_sheets[i], "_diff_output.pdf")
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
			+ ggtitle(paste(lt_lab, ann_sheets[i], "Change from Baseline"))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", ann_sheets[i], "_diff_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end plot annual

# plot the cumulative
for (i in 1:num_cum_sheets) {
	
	# land line
	out_file = paste0(out_dir, lt_lab, "_", cum_sheets[i], "_output.pdf")
	plot_df = out_cum_df_list[[i]][out_cum_df_list[[i]][,"Land_Type"] == lt_lab,]
	p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
			+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
			+ geom_line(size = 0.3)
			+ geom_point(aes(shape=Scenario), size = 1.5)
			+ ylab( paste( "MMT C" ) )
			+ theme(legend.key.size = unit(0.4,"cm"))
			+ ggtitle(paste(lt_lab, cum_sheets[i]))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", cum_sheets[i], "_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
		
	# land diffs
	out_file = paste0(out_dir, lt_lab, "_", cum_sheets[i], "_diff_output.pdf")
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
			+ ggtitle(paste(lt_lab, cum_sheets[i], "Change from Baseline"))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", cum_sheets[i], "_diff_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end plot cumulative

# plot the area
for (i in 1:num_area_sheets) {

	# land	
	out_file = paste0(out_dir, lt_lab, "_", area_sheets[i], "_output.pdf")
	plot_df = out_area_df_list[[i]][out_area_df_list[[i]][,"Land_Type"] == lt_lab,]
	p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
			+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
			+ geom_line(size = 0.3)
			+ geom_point(aes(shape=Scenario), size = 1.5)
			+ ylab( paste( "Thousand ha" ) )
			+ theme(legend.key.size = unit(0.4,"cm"))
			+ ggtitle(paste(lt_lab, area_sheets[i]))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", area_sheets[i], "_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
	
	# land diffs
	out_file = paste0(out_dir, lt_lab, "_", area_sheets[i], "_diff_output.pdf")
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
			+ ggtitle(paste(lt_lab, area_sheets[i], "Change from Baseline"))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", area_sheets[i], "_diff_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end plot area

# plot the density
for (i in 1:num_den_sheets) {
	
	out_file = paste0(out_dir, lt_lab, "_", den_sheets[i], "_output.pdf")
	plot_df = out_den_df_list[[i]][out_den_df_list[[i]][,"Land_Type"] == lt_lab,]
	p <- ( ggplot(plot_df, aes(Year, Value, color=Scenario))
			+ scale_shape_manual(values=1:nlevels(plot_df$Scenario))
			+ geom_line(size = 0.3)
			+ geom_point(aes(shape=Scenario), size = 1.5)
			+ ylab( paste( "MgC per ha" ) )
			+ theme(legend.key.size = unit(0.4,"cm"))
			+ ggtitle(paste(lt_lab, den_sheets[i]))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", den_sheets[i], "_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

	# diffs
	out_file = paste0(out_dir, lt_lab, "_", den_sheets[i], "_diff_output.pdf")
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
			+ ggtitle(paste(lt_lab, den_sheets[i], "Change from Baseline"))
			)
	p$save_args <- FIGURE_DIMS
	print(p)
	do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
	out_file = paste0(out_dir, lt_lab, "_", den_sheets[i], "_diff_output.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)
} # end plot density

# the data frames for component bar graphs do not exist for seagrass
if (lt_lab != "Seagrass") {

# cumulative component bar graph
# subtract net wood gain from the ecosystem gain because the wood is now on the positive side
out_file = paste0(out_dir, lt_lab, "_cumulative_component_output.pdf")
plot_df = cum_comp_df[(cum_comp_df$Year == 2020 | cum_comp_df$Year == 2030 | cum_comp_df$Year == 2040 | cum_comp_df$Year == 2050),]
plot_df$Component <- factor(plot_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
		+ ggtitle(paste0(lt_lab, ": Components of Net Cumulative Carbon Retention"))
		#+ scale_fill_manual(values=brew, breaks=breaks)
		+ theme(axis.text.x = element_text(size=5))
		#+ scale_y_continuous(limits=c(-amax,amax))
		)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_cumulative_component_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)


# cumulative component bar graph differences
# subtract net wood gain from the ecosystem gain because the wood is now on the positive side
out_file = paste0(out_dir, lt_lab, "_cumulative_component_diff_output.pdf")
temp_df = cum_comp_df[(cum_comp_df$Year == 2020 | cum_comp_df$Year == 2030 | cum_comp_df$Year == 2040 | cum_comp_df$Year == 2050),]
temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
		+ ggtitle(paste0(lt_lab, ": Change in Components of Net Cumulative Carbon Retention"))
		#+ scale_fill_manual(values=brew, breaks=breaks)
		+ theme(axis.text.x = element_text(size=5))
		#+ scale_y_continuous(limits=c(-amax,amax))
		)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_cumulative_component_diff_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

# also write the whole series of cum component values
out_file = paste0(out_dir, lt_lab, "_cumulative_component_all.csv")
write.csv(cum_comp_df, out_file, quote=FALSE, row.names=FALSE)

# annual component bar graph
# subtract net wood gain from the ecosystem gain because the wood is now on the positive side
out_file = paste0(out_dir, lt_lab, "_annual_component_output.pdf")
plot_df = ann_comp_df[(ann_comp_df$Year == 2020 | ann_comp_df$Year == 2030 | ann_comp_df$Year == 2040 | ann_comp_df$Year == 2050),]
plot_df$Component <- factor(plot_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
		+ ggtitle(paste0(lt_lab, ": Components of Net Annual Carbon Retention"))
		#+ scale_fill_manual(values=brew, breaks=breaks)
		+ theme(axis.text.x = element_text(size=5))
		#+ scale_y_continuous(limits=c(-amax,amax))
		)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_annual_component_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)


# annual component bar graph differences
# subtract net wood gain from the ecosystem gain because the wood is now on the positive side
out_file = paste0(out_dir, lt_lab, "_annual_component_diff_output.pdf")
temp_df = ann_comp_df[(ann_comp_df$Year == 2020 | ann_comp_df$Year == 2030 | ann_comp_df$Year == 2040 | ann_comp_df$Year == 2050),]
temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
		+ ggtitle(paste0(lt_lab, ": Change in Components of Net Annual Carbon Retention"))
		#+ scale_fill_manual(values=brew, breaks=breaks)
		+ theme(axis.text.x = element_text(size=5))
		#+ scale_y_continuous(limits=c(-amax,amax))
		)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_annual_component_diff_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

# also write the whole series of ann component values
out_file = paste0(out_dir, lt_lab, "_annual_component_all.csv")
write.csv(ann_comp_df, out_file, quote=FALSE, row.names=FALSE)


#### component sum line graphs
# these are the changes over time from the initial state
# these are the landscape c stock plus the net wood products
# and the net annual values are output here as well

# net cumulative change line graph
out_file = paste0(out_dir, lt_lab, "_cumulative_change_output.pdf")
temp_df = cum_comp_df
temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
				+ ggtitle(paste0(lt_lab, ": Landscape and wood C change from 2010"))
				)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_cumulative_change_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

# net cumulative change, difference from baseline line graph
out_file = paste0(out_dir, lt_lab, "_cumulative_change_diff_output.pdf")
temp_df = cum_comp_df
temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
				+ ggtitle(paste0(lt_lab, ": Landscape and wood C change from 2010, wrt Baseline"))
				)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_cumulative_change_diff_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)


# net annual retention line graph
out_file = paste0(out_dir, lt_lab, "_annual_retain_output.pdf")
temp_df = ann_comp_df
temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
				+ ggtitle(paste0(lt_lab, ": Landscape and wood C annual retention rate"))
				)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_annual_retain_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

# net annual retention, difference from baseline line graph
out_file = paste0(out_dir, lt_lab, "_annual_retain_diff_output.pdf")
temp_df = ann_comp_df
temp_df$Component <- factor(temp_df$Component, levels = c("Ecosystem_Gain_minus_NWG", "Net_Wood_Gain", "Loss_to_Atmos_from_Wood", 
                                                          "Loss_to_Atmos_from_Manage", "Loss_to_Atmos_from_LCC", "Loss_to_Atmos_from_Fire"))
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
				+ ggtitle(paste0(lt_lab, ": Landscape and wood C annual retention rate, wrt Baseline"))
				)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(out_dir, lt_lab, "_annual_retain_diff_output.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end if not seagrass

} # end loop over land types

cat("Finish plot_ca_carbon_model.r at", date(), "\n")

} # end function plot_caland()