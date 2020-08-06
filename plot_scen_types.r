# plot_scen_types.r

####
# California Natural and Working Lands Carbon and Greenhouse Gas
# Model (CALAND) Copyright (c) 2020, The Regents of the University of 
# California, through Lawrence Berkeley National Laboratory (subject to 
# receipt of any required approvals from the U.S. Dept. of Energy).  All 
# rights reserved.
# If you have questions about your rights to use or distribute this software, # please contact Berkeley Lab's Intellectual Property Office at # IPO@lbl.gov. #  # NOTICE.  This Software was developed under funding from the U.S. Department # of Energy and the U.S. Government consequently retains certain rights.  As # such, the U.S. Government has been granted for itself and others acting on # its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the # Software to reproduce, distribute copies to the public, prepare derivative  # works, and perform publicly and display publicly, and to permit others to do so.
####

# This software and its associated input data are licensed under a modified BSD open source license
# Please see license.txt for details

# put a variable for specified land types within a region and ownership on the same plot, for each scenario in 
# the diagnostic output file

# this script reads the csv files produced by plot_caland()

############################################ Overview of `plot_scen_types()` ###########################################

# The `plot_scen_types()` function is designed to use the .csv outputs from `plot_caland()` to plot individual land types 
# for a designated variable and all available scenarios in the .csv file. You also specify which land types, region 
# (or all regions aggregated), and ownership (or all ownerships aggregated) to plot for each scenario.

############################################## Inputs to `plot_scen_types()` ############################################

# The .csv input files, created by `plot_caland()`, are assumed to be in caland/`data_dir`/`figdir`, in the appropriate 
# land type and ownership directories. The `plot_scen_types()` output files will go directly into 
# caland/`data_dir`/`figdir`/`reg`/`own`, which should be the same as used in `plot_caland()`.

########################################### Arguments in `plot_scen_types()`############################################# 

# 1. `varname`: name of variable to plot. See the outputs from `plot_caland()`; the name is between the land type and 
#     "_output" in these file names; do not include the surrounding "_" characters.
# 2. `ylabel`: Label for the y-axis; it should indicate the units and whether it is a difference from baseline.
# 3. `data_dir`: The path to the directory containing the `CALAND()` output files, which also contains `figdir`; do not 
#     include the "/" character at the end; default is `data_dir = "./outputs"`.
# 4. `file_tag`: Additional tag to file name to note what regions, landtypes, and/or ownerships are included; default 
#     is "" (nothing added).
# 5. `reg`: Vector of region names to plot; default is: 
#       `reg = c("All_region", "Central_Coast", "Central_Valley", "Delta", 
#         "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")`.
# 6. `lt`: Vector of land types to plot; can be any number of available land types; default is: 
#       `lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", 
#         "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all")`. 
# 7. `own`: Vector of ownerships to plot; can be any number of available ownerships; default is: `own = c("All_own")`
# 8. `figdir`: The directory within `data_dir` containing the .csv data to plot, and where to save the figures that 
#     `plot_scen_types()` creates; do not include the "/" character at the end.

# Notes on `plot_scen_types()`: 
#   Plotting the Ocean region does not provide any comparison with land types because only Seagrass exists in the Ocean
#   Seagrass has only a subset of the `plot_caland()` output files, so if including All_region make sure that the desired 
#     `varname` is available for Seagrass.
#   Seagrass is not in the default land type list.

########################################### Outputs from `plot_scen_types()`#############################################

# Output files consist of a suite of graphs (.pdf) and corresponding data tables (.csv), which are written to 
# caland/data_dir/figdir/ within each region directory, where data_dir and figdir are arguments to plot_scen_types(). 
# Naming of the .pdf and .csv filenames is automatic and determined from the varname argument, the scenarios present in 
# the source data files, and an optional file_tag argument.

####################################################### start script ####################################################

# setwd("<your_path>/caland/")
setwd("./")

# this enables java to use up to 4GB of memory for reading and writing excel files
options(java.parameters = "-Xmx4g" )

# Load all the required packages
libs <- c( "ggplot2", "grid", "RColorBrewer" )
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it for local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

plot_scen_types <- function(varname, ylabel, data_dir = "./outputs", file_tag="", reg = c("All_region", "Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast"), lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all"), own = c("All_own"), figdir = "figures") {

outputdir = paste0(data_dir, "/")

num_reg = length(reg)
num_lt = length(lt)
num_own = length(own)

# need to add an underscore if an optional file tag is input
if (nchar(file_tag) > 0) { added_file_tag = paste0("_", file_tag)
} else {added_file_tag = file_tag}

for (r in 1:num_reg) {

	reg_lab = reg[r]
	
	# loop over the ownerships
	for (o in 1:num_own) {

		own_lab = own[o]
	
		all_df = NULL

		# loop over land types to get data
		for (i in 1:num_lt) {

			lt_lab = lt[i]

			# Seagrass exists only in Ocean and All_region and (All_own or Other_fed)
			# Seagrass is the only land type in Ocean, and the only ownerships are All_own or Other_fed
			
			if ((reg_lab != "All_region" & reg_lab != "Ocean" & lt_lab != "Seagrass") | reg_lab == "All_region"  | (reg_lab == "Ocean" & lt_lab == "Seagrass" & (own_lab == "All_own" | own_lab == "Other_fed"))) {
				fname = paste0(outputdir, figdir, "/", reg_lab, "/", lt_lab, "/", own_lab, "/", reg_lab, "_", lt_lab, "_", own_lab, "_", varname, "_output.csv")
				in_df = read.csv(fname)
				in_df = in_df[in_df$Region == reg_lab & in_df$Ownership == own_lab,]
				all_df = rbind(all_df, in_df)
			}
	
		} # end for i loop over reading land types

		scen_names = unique(all_df$Scenario)
		num_scen = length(scen_names)

		# make plot for each scenario within the file
		for ( i in 1:num_scen) {
		
			plot_df = all_df[all_df$Scenario == scen_names[i],]
			# drop output forward for label - watch out for scenario distinctions beyond the "output" tag
    		scen_lab = substr(scen_names[i], 1, regexpr("_output", scen_names[i])-1)
			title = paste(reg_lab, own_lab, scen_lab, varname)
			
			# plot the data on a single plot
			FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)
			theme_set(theme_bw())
			out_file = paste0(outputdir, figdir, "/", reg_lab, "/", reg_lab, "_", own_lab, "_", scen_names[i], "_", varname, "_comp", added_file_tag, ".pdf")
			p <- ( ggplot(plot_df, aes(Year, Value, color=Land_Type))
					+ scale_shape_manual(values=1:nlevels(plot_df$Land_Type))
					+ geom_line(size = 0.3)
					+ geom_point(aes(shape= Land_Type), size = 1.5)
					+ ylab( ylabel )
					+ theme(legend.key.size = unit(0.4,"cm"))
					+ ggtitle(title)
					)
			p$save_args <- FIGURE_DIMS
			#print(p)
			do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
			out_file = paste0(outputdir, figdir, "/", reg_lab, "/", reg_lab, "_", own_lab, "_", scen_names[i], "_", varname, "_comp", added_file_tag, ".csv")
			write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

		} # end i loop over scenarios

	} # end for o loop over ownerships

} # end for r loop over regions

} # end plot_scen_types()