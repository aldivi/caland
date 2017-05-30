# plot_scen_types.r

# put a variable for specified land types within a region on the same plot, for a specified scenario

# this script reads the csv files produced by plot_caland()

# make sure that the working directory is caland/
# the csv files are assumed to be in <figdir>, in the appropriate region and land type directories
# resulting output will go directly into <figdir>/<region>, which should be the same as used in plot_caland()
# setwd("<your_path>/caland/")

# plot_scen_types() has 6 arguments:
#	scen_lname		name of scenario as given in plot_caland(scen_lnames)
#	varname			name of variable to plot (see the outputs from plot_caland)
#						this name is between the land type and "_output" in these file names; do not include the surrounding "_" characters
#	ylabel			y label for the plot; this indicates the units and whether it is a difference from baseline
#	reg				array of region names to plot (see below)
#	lt				array of land types to plot; can be any number of available types (all are below as default; All_land is excluded)
#	figdir			the directory within caland/outputs/ to write the figures; do not include the "/" character at the end

# available regions:
#	"Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "All_region"

# Note: plotting the ocean region doesn't provide any comparison with land types because only seagrass exists in the ocean

# set these here also so the script can be run without using the function
scen_lname = "Baseline"
#scen_lnames = c("Baseline", "LowProtect", "HighProtect", "LowManage", "HighManage")
varname = "Area"
ylabel = "Thousand ha"
reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "All_region")
lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass")
figdir = "test_diags"

# Load all the required packages
libs <- c( "ggplot2", "grid", "RColorBrewer" )
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it for local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

plot_scen_types <- function(scen_lname, varname, ylabel, reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", "All_region"), lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass"), figdir = "test_diags") {

outputdir = "outputs/"

num_reg = length(reg)
num_lt = length(lt)

for (r in 1:num_reg) {

	reg_lab = reg[r]
	title = paste(reg_lab, scen_lname, varname)
	plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)

	# loop over land types to get data
	for (i in 1:num_lt) {

		# Seagrass exists only in Ocean and All_region
		# Seagrass is the only land type in Ocean
		lt_lab = lt[i]
		if ((reg_lab != "All_region" & reg_lab != "Ocean" & lt_lab != "Seagrass") | reg_lab == "All_region"  | (reg_lab == "Ocean" & lt_lab == "Seagrass")) {
			fname = paste0(outputdir, figdir, "/", reg_lab, "/", lt_lab, "/", reg_lab, "_", lt_lab, "_", varname, "_output.csv")
			in_df = read.csv(fname)
			in_df = in_df[in_df$Scenario == scen_lname,]
			plot_df = rbind(plot_df, in_df)
		}
	
	} # end for i loop over reading lts

	# plot the data on a single plot
	FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)
	theme_set(theme_bw())
	out_file = paste0(outputdir, figdir, "/", reg_lab, "/", reg_lab, "_", scen_lname, "_", varname, "_comp.pdf")
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
	out_file = paste0(outputdir, figdir, "/", reg_lab, "/", reg_lab, "_", scen_lname, "_", varname, "_comp.csv")
	write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end for r loop over regions

} # end plot_scen_types()