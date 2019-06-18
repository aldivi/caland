# plot_uncertainty.r

# Copyright (c) 2016-2019, Alan Di Vittorio and Maegen Simmonds

# This software and its associated input data are licensed under the 3-Clause BSD open source license
# Please see license.txt for details

# plot the uncertainty range (low and high emsissions) around average values for chosen variable over time for each scenario 
# variables are plotted using the .csv outputs from plot_caland.r 

# Inputs - for each group of scenarios (one or two scenario groups can be plotted)
# Scenarios that are in the same .csv file from plot_caland.r constitute a group
# (1) Mean output folder from plot_caland: <figdir[1]>/<reg_lab>_<lt_lab>_<own_lab>_<varname>_output.csv 
# (2) Low output folder from plot_caland: <figdir[2]>/<reg_lab>_<lt_lab>_<own_lab>_<varname>_output.csv 
# (3) High output folder from plot_caland: <figdir[3]>/<reg_lab>_<lt_lab>_<own_lab>_<varname>_output.csv 
# These files have matching formats
# The three input .csv files for mean, low, and high are assumed to be in caland/outputs/mean, caland/outputs/low, 
# caland/outputs/high, respectively, unless <figdir> is specified differently than the plot_uncertainty.r default: figdir = c("mean","low","high")
# These figdirs can also be the same for plotting Cultivated soil conservation uncertainty

# Outputs
# Scenarios plotted on same graph
# (1) <reg_lab>_<lt_lab>_<own_lab>_<varname>_scen_comp_uncert_bands<added_file_tag>.pdf
# (2) <reg_lab>_<lt_lab>_<own_lab>_<varname>_scen_comp_uncert_bands<added_file_tag>.csv
# Each scenario plotted individually (scen_labels are assigned based on inputs)
# (3) <reg_lab>_<lt_lab>_<own_lab>_<scen_labels[s]>_<varname>_scen_comp_uncert_bands<added_file_tag>.pdf

# Output .pdf and .csv files are written to caland/outputs/<figdir[1]> where figdir[1] is the folder containing the mean plot_caland outputs for the first scenario group

# this script includes 2 functions: wrapper and plot_uncertainty
#	wrapper manipulates text for the plots
# plot_uncertainty generates the plots 

# plot_uncertainty() takes 16 arguments:

##	start_year  year to start plotting
##  end_year    year to end plotting
##  varname		  name of variable to plot (see the outputs from plot_caland)
                  # this name is between the land type and "_output" in these file names; do not include the surrounding "_" characters
##	ylabel		  y label for the plot corresponding to the units of your outputs and whether they are changes from baseline (varname ending in "diff") 
                  # or absolute values. Note that this function does not convert units so the output units of your desired plotting variable must be correctly matched with 
                  # Here are some exmaples:
                  # "Change from Baseline (MMT CO2eq)"
                  # "MMT CO2eq"
                  # "Change from Baseline (MMT C)"
                  # "MMT C"
                  # "Change from Baseline (Mg C/ha)"
                  # "Mg C/ha"
                  # "Change from Baseline (Mg C/ac)"
                  # "Mg C/ac"
##	file_tag	  tag to add to end of new file names (e.g., to note what time period is plotted); default is "" (nothing added)
##	data_dir_a	  the path to the directory containing the three folders of plot_caland outputs (mean, low, and high) for scenario group a; do not include the "/" character at 
                  # the end; default is "./outputs"
##	figdirs_a     the three folders within data_dir_a containing the data to plot. These must be assigned in order of mean, low, and high. The figures will
                  # be written to the folder representing the mean; do not include the "/" character at the end
                  # the csv files are assumed to be in <data_dir_a>/<figdirs_a[f]>, in the appropriate region and land type and ownership directories
##  scenarios_a   one or more scenario names for group 'a' that are listed in the Scenario column in the csv file containing varname for the mean
##  scen_labs_a   scenario labels; must have the same number and correspond directly in order with scenarios_a
##	data_dir_b	  the path to the directory containing the three folders of plot_caland outputs (mean, low, and high) for scenario group b;
				  # use this if plotting two or three groups
				  # NA is the default, which means that only one scenario group (group a) is plotted; do not include the "/" character at 
                  # the end; default is "./outputs"
##	figdirs_b	  the three folders within data_dir_b containing the data to plot. These must be assigned in order of mean, low, and high. The figures will
                  # be written to the folder representing the mean; do not include the "/" character at the end
                  # the csv files are assumed to be in <data_dir_b>/<figdirs_b[f]>, in the appropriate region and land type and ownership directories
##  scenarios_b   one or more scenario names for group 'b' that are listed in the Scenario column in the csv file containing varname for the mean
##  scen_labs_b   scenario labels; must have the same number and correspond directly in order with scenarios_b
##	data_dir_c	  the path to the directory containing the three folders of plot_caland outputs (mean, low, and high) for scenario group c;
				  # use this only if plotting three groups
				  # NA is the default, which means that only group a or groups a and b are plotted; do not include the "/" character at 
                  # the end; default is "./outputs"
##	figdirs_c	  the three folders within data_dir_c containing the data to plot. These must be assigned in order of mean, low, and high. The figures will
                  # be written to the folder representing the mean; do not include the "/" character at the end
                  # the csv files are assumed to be in <data_dir_c>/<figdirs_c[f]>, in the appropriate region and land type and ownership directories
##  scenarios_c   one or more scenario names for group 'c' that are listed in the Scenario column in the csv file containing varname for the mean
##  scen_labs_c   scenario labels; must have the same number and correspond directly in order with scenarios_c
##	reg			    array of region names to plot; default = "All_region", but can be any number of available types:
                  #	"All_region", "Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", 
                  # Note: plotting the ocean region doesn't provide any comparison with land types because only seagrass exists in the ocean
##	lt			    array of land types to plot;  default = "All_land", but can be any number of available types 
                  # Note: Seagrass has only a subset of the output files, so if including for All_region make sure that the desired varname is available
##	own			    array of ownerships to plot;  default = "All_own", but can be any number of available types 

###################################################################################################################################################

# make sure that the working directory is caland/
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


###################################################################################################################################################
################################################### Assign function to wrap text in plot titles ################################################### 
###################################################################################################################################################
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

################################################################################################################################################### 
########################### Assign function to plot variables over time with shaded uncertainty range for each scenario ###########################
###################################################################################################################################################

plot_uncertainty <- function(start_year=2010, end_year=2051, varname, ylabel, file_tag="", 
							data_dir_a = "./outputs", figdirs_a=c("mean","low","high"), scenarios_a, scen_labs_a,
							data_dir_b = NA, figdirs_b = NA, scenarios_b = NA, scen_labs_b = NA,
							data_dir_c = NA, figdirs_c = NA, scenarios_c = NA, scen_labs_c = NA,
                          	reg = "All_region", lt = "All_land", own="All_own") {
  
  outputdir_a = paste0(data_dir_a, "/")
  outputdir_b = paste0(data_dir_b, "/")
  outputdir_c = paste0(data_dir_c, "/")
  num_reg = length(reg)
  num_lt = length(lt)
  num_own = length(own)
  num_scen_a = length(scenarios_a)
  num_scen_b = length(scenarios_b)
  num_scen_c = length(scenarios_c)
  
  if(num_scen_a != length(scen_labs_a)) {
  	stop( "Ensure that the length and content of scen_labs_a matches scenarios_a\n" )
  }
  if(num_scen_b != length(scen_labs_b)) {
  	stop( "Ensure that the length and content of scen_labs_b matches scenarios_b\n" )
  }
  if(num_scen_c != length(scen_labs_c)) {
  	stop( "Ensure that the length and content of scen_labs_c matches scenarios_c\n" )
  }
  
  # determine how many scenario groups to plot
  if(is.na(data_dir_b) & is.na(data_dir_c)) { num_groups = 1
  } else if ( (!is.na(data_dir_b) & is.na(data_dir_c)) | (is.na(data_dir_b) & !is.na(data_dir_c)) ) { num_groups = 2  	
  } else {num_groups = 3}
  
  # need to add an underscore if an optional file tag is input
  if (nchar(file_tag) > 0) { added_file_tag = paste0("_", file_tag)
  } else {added_file_tag = file_tag}
  
  for (r in 1:num_reg) {
    
    reg_lab = reg[r]
    
    # loop over the ownerships
    
    for (o in 1:num_own) {
      
		own_lab = own[o]

		# loop over land types to get data
		for (l in 1:num_lt) {
		  	
		  	lt_lab = lt[l]
		  	
		 	# create empty all_df table to fill with the mean, low and hi outputs below
		  	all_df = NULL

			# loop over the scenario groups for read in
			for (g in 1:num_groups) {
			  
			  # determine which group variables to use
			  if(g==1) {
			  	outputdir = outputdir_a
			  	figdirs = figdirs_a
			  	scenarios = scenarios_a
			  	num_scen = num_scen_a
			  } else if (g==2) {
			  	outputdir = outputdir_b
			  	figdirs = figdirs_b
			  	scenarios = scenarios_b
			  	num_scen = num_scen_b
			  } else {
			  	outputdir = outputdir_c
			  	figdirs = figdirs_c
			  	scenarios = scenarios_c
			  	num_scen = num_scen_c
			  }
			  
			  # create empty group_df table to fill with the mean, low and hi outputs below
		  	  group_df = NULL

			  # read in the 3 files for scenario group a (mean, lo, hi) and select scenarios and rows in start:end_year
			  for (f in 1:3) {
			      fig_lab <- figdirs[f]
				  fname = paste0(outputdir, fig_lab, "/", reg_lab, "/", lt_lab, "/", own_lab, "/", reg_lab, "_", lt_lab, "_", own_lab, "_", varname, "_output.csv")
				  in_df_pre = read.csv(fname)
				  
				  # the mean scenario name is input, so need to find the low and high scenario names
				  # for carbon input uncertainty the different levels will be in different figdirs, so these are the basis for which is which
				  # for cultivated soil conservation uncertainty, the different levels can be in the same file, so need to account for this
				  # save each (mean, lo, hi) as their own column
				  
				  if (f==1) {
				  	# mean
				  	# extract the years specified between start_year and end_year for each desired scenario
          		  	in_df <- in_df_pre[in_df_pre$Scenario %in% scenarios & in_df_pre$Year %in% start_year:end_year,]
				  	# subset the current region and ownership for given land type
				  	in_df = in_df[in_df$Region == reg_lab & in_df$Ownership == own_lab,]
				    # add these to the group df
				    group_df <- rbind(group_df, in_df)
				    colnames(group_df)[ncol(group_df)] <- "Mean"
				  } else if (f==2) {
				    # low
				    if(figdirs[2] == figdirs[1]) { # soil conservation uncertainty
				    	# need to get the low soil conservation cases
				    	snames_all = NULL
				    	for (n in 1:num_scen) {
				    		pre <- substr(scenarios[n], 1, regexpr("_output", scenarios[n])-1)
				    		post <- substr(scenarios[n], regexpr("_BC", scenarios[n]), nchar(scenarios[n]))
				    		snames = grep(pre, in_df_pre$Scenario, value = TRUE)
				    		snames = grep(post, snames, value = TRUE)
				    		snames = grep("S-", snames, value = TRUE)
				    		snames = unique(snames)
				    		snames_all = c(snames_all, snames)
				    	} # end n loop ove desired scenarios
				    	# extract the years specified between start_year and end_year for each desired scenario
          		  		in_df <- in_df_pre[in_df_pre$Scenario %in% snames_all & in_df_pre$Year %in% start_year:end_year,]		    	
				    } else { # carbon input uncertainty
				    	# just match the scenario name, the soil tag, the BC tag, and the NR tag
				    	snames_all = NULL
				    	for (n in 1:num_scen) {
				    		pre <- substr(scenarios[n], 1, regexpr("_output", scenarios[n])-1)
				    		post <- substr(scenarios[n], regexpr("_BC", scenarios[n]), nchar(scenarios[n]))
				    		soil <- substr(scenarios[n], regexpr("_S", scenarios[n]), regexpr("_BC", scenarios[n])-1)
				    		if(regexpr("_S", scenarios[n]) < 0) {soil = "_S=mean"}
				    		snames = grep(pre, in_df_pre$Scenario, value = TRUE)
				    		snames = grep(post, snames, value = TRUE)
				    		snames = grep(soil, snames, value = TRUE)
				    		snames = unique(snames)
				    		snames_all = c(snames_all, snames)
				    	} # end n loop over desired scenarios
				    	# extract the years specified between start_year and end_year for each desired scenario
          		  		in_df <- in_df_pre[in_df_pre$Scenario %in% snames_all & in_df_pre$Year %in% start_year:end_year,]				  		
				    } # end if-else soil cons vs carbon uncertainty
				    # subset the current region and ownership for given land type
				    in_df = in_df[in_df$Region == reg_lab & in_df$Ownership == own_lab,]
				    # add these to the group df
				    group_df <- cbind(group_df,in_df$Value)
				    colnames(group_df)[ncol(group_df)] <- "Min"
				  } else {
				  	# high
				  	if(figdirs[3] == figdirs[1]) { # soil conservation uncertainty
				    	# need to get the high soil conservation cases
				    	snames_all = NULL
				    	for (n in 1:num_scen) {
				    		pre <- substr(scenarios[n], 1, regexpr("_output", scenarios[n])-1)
				    		post <- substr(scenarios[n], regexpr("_BC", scenarios[n]), nchar(scenarios[n]))
				    		snames = grep(pre, in_df_pre$Scenario, value = TRUE)
				    		snames = grep(post, snames, value = TRUE)
				    		snames = grep("S+", snames, value = TRUE)
				    		snames = unique(snames)
				    		snames_all = c(snames_all, snames)
				    	} # end n loop ove desired scenarios
				    	# extract the years specified between start_year and end_year for each desired scenario
          		  		in_df <- in_df_pre[in_df_pre$Scenario %in% snames_all & in_df_pre$Year %in% start_year:end_year,]		    	
				    } else { # carbon input uncertainty
				    	# just match the scenario name, the soil tag, the BC tag, and the NR tag
				    	snames_all = NULL
				    	for (n in 1:num_scen) {
				    		pre <- substr(scenarios[n], 1, regexpr("_output", scenarios[n])-1)
				    		post <- substr(scenarios[n], regexpr("_BC", scenarios[n]), nchar(scenarios[n]))
				    		soil <- substr(scenarios[n], regexpr("_S", scenarios[n]), regexpr("_BC", scenarios[n])-1)
				    		if(regexpr("_S", scenarios[n]) < 0) {soil = "_S=mean"}
				    		snames = grep(pre, in_df_pre$Scenario, value = TRUE)
				    		snames = grep(post, snames, value = TRUE)
				    		snames = grep(soil, snames, value = TRUE)
				    		snames = unique(snames)
				    		snames_all = c(snames_all, snames)
				    	} # end n loop over desired scenarios
				    	# extract the years specified between start_year and end_year for each desired scenario
          		  		in_df <- in_df_pre[in_df_pre$Scenario %in% snames_all & in_df_pre$Year %in% start_year:end_year,]				  		
				    } # end if-else soil cons vs carbon uncertainty
				    # subset the current region and ownership for given land type
				    in_df = in_df[in_df$Region == reg_lab & in_df$Ownership == own_lab,]
				    # add these to the group df
				   	group_df <- cbind(group_df,in_df$Value)
				    colnames(group_df)[ncol(group_df)] <- "Max"
				  } # end if-else for figdirs
			  } # end f loop reading in each file for a scenario group (mean, lo, hi)
			  
			  # add group_df to all_df
			  all_df <- rbind(all_df, group_df)
		    
		    } # end g loop over scenario groups
		    
		    # get full scenario names
			  scen_names <- as.character(unique(all_df$Scenario))
			  # get number of scenarios
			  num_scen_all <- length(scen_names)
			  
			  # extract beginning of scenario full name (use first scenario as sample which is not to be included in the plot title)
			  scen_lab <- substr(scen_names[1], 1, regexpr("_output", scen_names[1])-1)
			  # extract NWL scenario version, NR setting (default=default), and climate
			  file_lab <- paste0("v",gsub("^.*?_v", "", scen_lab))
			  # create plot title combining the geography, version/assumptions, and variable
			  title <- paste(reg_lab, own_lab, lt_lab, file_lab, varname)

			  # replace the names of scenarios for plotting
			  all_df$Scenario <- as.character(all_df$Scenario)
			  for(n in 1:num_scen_all) {
			  	all_df$Scenario[all_df$Scenario==scenarios_a[n]] = scen_labs_a[n]
			  	all_df$Scenario[all_df$Scenario==scenarios_b[n]] = scen_labs_b[n]
			  }
			  
			  # get list of scenario labels
			  scen_labels = as.character(unique(all_df$Scenario))
			    
        ### plot scenarios on one plot  
			  p <- ggplot(all_df) + geom_line(aes(x=Year, y=Mean, linetype=Scenario)) + ylab(ylabel) +
			    geom_ribbon(data=all_df,aes(x=Year, y=Mean, ymin=Min,ymax=Max, group=Scenario),alpha=0.2) +
			    ggtitle(wrapper(title, width=40)) + theme_bw() + theme(legend.text=element_text(size=12),
			                                                              plot.title = element_text(size=8),
			                                                              axis.text=element_text(size=12), 
			                                                              axis.title.y=element_text(size=12),
			                                                              axis.title.x=element_text(size=12),
			                                                              legend.title=element_text(size=12))

			  # save plot as pdf
			  out_file <- paste0(outputdir_a, figdirs_a[1], "/", reg_lab, "/", lt_lab , "/", own_lab, "/", reg_lab, "_", lt_lab, "_", own_lab, "_", varname, "_scen_comp_uncert_bands", added_file_tag, ".pdf")
			  ggsave(out_file,dpi=300, width=2560/300, height=1540/300)
		    # save df as csv
			  out_csv <- gsub(".pdf", ".csv", out_file) 
			  write.csv(all_df, out_csv)
			  
			  #### plot scenarios individually
			  for ( s in 1:num_scen_all) {
			    
			    # create plot title combining the geography, scenario, version/assumptions, and variable
			    title <- paste(reg_lab, own_lab, lt_lab, scen_labels[s], file_lab, varname)
			   
			    p <- ggplot(all_df[all_df$Scenario==scen_labels[s],]) + geom_line(aes(x=Year, y=Mean, linetype=Scenario)) + ylab(ylabel) +
			       geom_ribbon(data=all_df[all_df$Scenario==scen_labels[s],],
			                   aes(x=Year, y=Mean, ymin=Min,ymax=Max, group=Scenario),alpha=0.2) +
			       ggtitle(wrapper(title, width=40)) + theme_bw() + theme(legend.text=element_text(size=12),
			                                                              plot.title = element_text(size=8),
			                                                              axis.text=element_text(size=12), 
			                                                              axis.title.y=element_text(size=12),
			                                                              axis.title.x=element_text(size=12),
			                                                              legend.title=element_text(size=12))
			    
			     out_file = paste0(outputdir_a, figdirs_a[1], "/", reg_lab, "/", lt_lab , "/", own_lab, "/", reg_lab, "_", lt_lab, "_", own_lab, "_", varname,  "_scen_comp_uncert_bands", added_file_tag, "_", scen_labels[s], ".pdf")
			     ggsave(out_file,dpi=300, width=2560/300, height=1540/300)
			     
			     } # end s loop plot individual scenarios
			  } # end for l loop over reading land types
		  } # end o loop over ownerships
    } # end r loop over regions
  } # end plot_uncertainty()
