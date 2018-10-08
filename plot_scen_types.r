# plot_scen_types.r

# put a variable for specified land types within a region and ownership on the same plot, for each scenario in the diagnostic output file

# this script reads the csv files produced by plot_caland()

# plot_scen_types() has 8 arguments:
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

# Note: plotting the ocean region doesn't provide any comparison with land types because only seagrass exists in the ocean

# Note: Seagrass has only a subset of the output files, so if including for All_region make sure that the desired varname is available
#	Seagrass is not in the default land type list

# make sure that the working directory is caland/
# the csv files are assumed to be in <data_dir>/<figdir>, in the appropriate region and land type and ownership directories
# resulting output will go directly into <data_dir>/<figdir>/<region>/<own>, which should be the same as used in plot_caland()
# setwd("<your_path>/caland/")
setwd("./")

# set these here also so the script can be run without using the function
#scen_lnames = c("Baseline", "LowProtect", "HighProtect", "LowManage", "HighManage")

#varname = "All_orgC_stock_diff"
varname = "Area"

ylabel = "Thousand ha"
#ylabel = "Change from Baseline (MMT C)"

data_dir = "./outputs/oct_3_2018_nwl_v5"
file_tag = ""
reg = c("All_region", "Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")
lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all")
#own = c("All_own", "BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")
own = c("All_own")
figdir = "scen_rcp85_2051"

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

plot_scen_types <- function(scen_lname, varname, ylabel, data_dir = "./outputs", file_tag="", reg = c("All_region", "Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast"), lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all"), own = c("All_own"), figdir = "figures") {

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