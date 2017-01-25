# plot_scen_types.r

# put a variable for specified land types on the same plot, for a specified scenario

# this script reads the csv files produced by plot_caland()

# make sure that the working directory is caland/
# the csv files are assumed to be in <figdir>, in the appropriate land type directories
# resulting output will go directly into <figdir>, which should be the same as used in plot_caland()
# setwd("<your_path>/caland/")

# plot_scen_types() has 5 arguments:
#	scen_lname		name of scenario as given in plot_caland(scen_lnames)
#	varname			name of variable to plot (see the outputs from plot_caland)
#						this name is between the land type and "_output" in these file names; do not include the surrounding "_" characters
#	ylabel			y label for the plot; this indicates the units and whether it is a difference from baseline
#	lt				array of land types to plot; can be any number of available types (all are below as default; All_land is excluded)
#	figdir			the directory within caland/outputs/ to write the figures; do not include the "/" character at the end

plot_scen_types <- function(scen_lname, varname, ylabel, lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow", "Coastal_marsh", "Fresh_marsh", "Agriculture", "Developed_all", "Seagrass"), figdir = "figures") {

outputdir = "outputs/"
title = paste(scen_lname, varname)

# Load all the required packages
libs <- c( "ggplot2", "grid", "RColorBrewer" )
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, and install it for local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

num_lt = length(lt)

plot_df <- data.frame(Scenario=NULL, Land_Type=NULL, Year=NULL, Value=NULL)

# loop over land types to get data
for (i in 1:num_lt) {

	lt_lab = lt[i]
	fname = paste0(outputdir, figdir, "/", lt_lab, "/", lt_lab, "_", varname, "_output.csv")
	in_df = read.csv(fname)
	in_df = in_df[in_df$Scenario == scen_lname,]
	plot_df = rbind(plot_df, in_df)
	
} # end for i loop over reading lts

# plot the data on a single plot
FIGURE_DIMS <- list(dpi=300, width=2560/300, height=1440/300)
theme_set(theme_bw())
out_file = paste0(outputdir, figdir, "/", scen_lname, "_", varname, "_comp.pdf")
p <- ( ggplot(plot_df, aes(Year, Value, color=Land_Type))
		+ scale_shape_manual(values=1:nlevels(plot_df$Land_Type))
		+ geom_line(size = 0.3)
		+ geom_point(aes(shape= Land_Type), size = 1.5)
		+ ylab( ylabel )
		+ theme(legend.key.size = unit(0.4,"cm"))
		+ ggtitle(title)
		)
p$save_args <- FIGURE_DIMS
print(p)
do.call( ggsave, c(list(filename=out_file, plot=p), p$save_args ) )
out_file = paste0(outputdir, figdir, "/", scen_lname, "_", varname, "_comp.csv")
write.csv(plot_df, out_file, quote=FALSE, row.names=FALSE)

} # end plot_scen_types()