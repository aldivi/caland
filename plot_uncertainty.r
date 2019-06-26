# plot_uncertainty.r

# Copyright (c) 2016-2019, Alan Di Vittorio and Maegen Simmonds

# This software and its associated input data are licensed under the 3-Clause BSD open source license
# Please see license.txt for details

# plot the uncertainty range (low and high emsissions) around average values for chosen variable over time for each scenario 
# variables are plotted using the .csv outputs from `plot_caland()` 

############################################# Overview of `plot_uncertainty()`#############################################  

# The `plot_uncertainty()` function is designed to plot shaded uncertainty bounds around average values for a single 
# variable over time for each scenario using .csv outputs from `plot_caland()`. This requires having run `CALAND()` and 
# `plot_caland()` three times each for each desired scenario; once for the mean inputs, and the other two times for lower 
# uncertainty bounds, and upper uncertainty bounds. For example, using the following combination of inputs to `CALAND()` 
# will generate minimum and maximum emissions:
#   Low emissions: low initial carbon density (i.e., mean-SD) and high carbon fluxes (i.e., mean+SD)
#   High emissions: high initial carbon density (i.e., mean+SD) and low carbon fluxes (i.e., mean-SD)

################################################ Inputs to `plot_uncertainty()`################################################  

# Inputs to `plot_uncertainty()` are .csv output files from `plot_caland()`. Within each .csv file, there can be any number of 
# scenarios. Each group of scenarios must have a corresponding .csv file for the mean, low, and high emissions. It is possible
# to plot up to three groupings (a, b, c) with `plot_uncertainty()`. Three scenario groups ammounts to a total of nine .csv 
# input files to `plot_uncertainty()`; three input .csv files (mean, low, and high emissions) for each group. All .csv files 
# have matching formats. For a single group ("group a"), they are assumed to be in caland/outputs/mean, caland/outputs/low, 
# caland/outputs/high, respectively, unless `figdir` is specified differently than the default: 
#  `figdir = c("mean","low","high")`. 

############################################### Arguments in `plot_uncertainty()`##############################################  

# 1. `start_year`:  year to start plotting; default is `start_year = 2010`.
# 2. `end_year`: year to end plotting; default is `end_year = 2051`.  
# 3. `varname`: name of a single variable to plot (see the .csv output filenames from `plot_caland()`); the variable name is 
#     between the land type and "_output" in these file names. However, do not include the surrounding "\_" characters.  
# 4. `ylabel`: label for y-axis corresponding to the units of your selected output variable, and whether they are changes from 
#     baseline (i.e., `varname` ending in "diff") or absolute values. Note that this function does not convert units so the 
#     output units of your desired plotting variable must be correctly matched with the .csv file.
#       - Here are some exmaples:
#         - `ylabel = "Change from Baseline (MMT CO2eq)"`
#         - `ylabel = "MMT CO2eq"`
#         - `ylabel = "Change from Baseline (MMT C)"`
#         - `ylabel = "MMT C"`
#         - `ylabel = "Change from Baseline (Mg C/ha)"`
#         - `ylabel = "Mg C/ha"`
#         - `ylabel = "Change from Baseline (Mg C/ac)"`
#         - `ylabel = "Mg C/ac"`
# 5. `file_tag`: tag to add to end of the new file names created by `plot_uncertainty()` (e.g., to note what time period is 
#     plotted); default is `file_tag = ""` (nothing added).
# 6. `data_dir_a`: the path to the directory containing the three folders of `plot_caland()` outputs (mean, low, and high 
#     emissions) for scenario group a; do not include the "/" character at the end; default is `data_dir_a = "./outputs"`.
# 7. `figdirs_a`: a vector of three folder names within `data_dir_a` containing the .csv data files to plot. The folder names 
#     must be assigned in order of mean, low, and high; do not include the "/" character at the end of each folder name. The 
#     default is `figdirs_a = c("mean", "low", "high")`; thus, the .csv files for the mean and lower and upper uncertainty 
#     bounds are assumed to be in `data_dir_a`/mean, `data_dir_a`/low, and `data_dir_a`/high, respectively, and in the 
#     appropriate region, land type, and ownership directories. The figures will be written to the folder representing the 
#     mean. 
# 8. `scenarios_a`: a vector of one or more scenario names for group 'a' that are listed in the Scenario column in the 
#     .csv file containing `varname` for the mean.
# 9. `scen_labs_a`: a vector of one or more scenario labels; must match the same number of elements in `scenarios_a`, and 
#     correspond directly to the order of elements in `scenarios_a`.
# 10. `data_dir_b`: the path to the directory containing the three folders of `plot_caland()` outputs (mean, low, and 
#     high emissions) for scenario group b; do not include the "/" character at the end; default is `data_dir_b = NA` (i.e., 
#     no group b), which will plot only scenario group a.
# 11. `figdirs_b`:  a vector of three folder names within `data_dir_b` containing the .csv data files to plot. The folder 
#     names must be assigned in order of mean, low, and high; do not include the "/" character at the end of each folder 
#     name. The default is `figdirs_b = NA` (i.e., no group b). However if you choose to plot a group b, the location of 
#     the .csv files for the mean and lower and upper uncertainty bounds should follow the same logic as group a. The 
#     figures will be written to the folder representing the mean. 
# 12. `scenarios_b`: a vector of one or more scenario names for group 'b' that are listed in the Scenario column in the 
#     .csv file containing `varname` for the mean.
# 13. `scen_labs_b`: a vector of one or more scenario labels; must match the same number of elements in `scenarios_b`, 
#     and correspond directly to the order of elements in `scenarios_b`.
# 14. `data_dir_c`: the path to the directory containing the three folders of `plot_caland()` outputs (mean, low, and 
#     high emissions) for scenario group c; do not include the "/" character at the end; default is `data_dir_c = NA` 
#     (i.e., no group c), which will only plot scenario group a, or group a and b.
# 15. `figdirs_c`:  a vector of three folder names within `data_dir_c` containing the .csv data files to plot. The 
#     folder names must be assigned in order of mean, low, and high; do not include the "/" character at the end of each 
#     folder name. The default is `figdirs_c = NA` (i.e., no group c). However if you choose to plot a group c, the 
#     location of the .csv files for the mean and lower and upper uncertainty bounds should follow the same logic as 
#     group a. The figures will be written to the folder representing the mean. 
# 16. `scenarios_c`: a vector of one or more scenario names for group 'c' that are listed in the Scenario column in the 
#     .csv file containing `varname` for the mean.
# 17. `scen_labs_c`: a vector of one or more scenario labels; must match the same number of elements in `scenarios_c`, 
#     and correspond directly to the order of elements in `scenarios_c`.
# 18. `reg`: a vector of one or more region names to plot; default is `reg = "All_region"` (i.e., aggregated across all 
#     regions), but can be any number of available regions: "All_region", "Central_Coast", "Central_Valley", "Delta", 
#     "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast". 
# 19. `lt`: a vector of one or more land types to plot; default is `lt = "All_land"` (i.e., aggregated across all land 
#     types), but can be any number of available land types 
# 20. `own`: a vector of one or more ownerships to plot; default = "All_own" (i.e., aggregated across all ownerships), 
#     but can be any number of available ownerships. 

# Notes on `plot_uncertainty()`:
# plotting the ocean region does not provide any comparison with land types because only seagrass exists in the ocean
# Seagrass has only a subset of all the .csv files output from `plot_caland()`; thus, if including Seagrass make sure 
#   that the desired `varname` is available

############################################### Outputs from `plot_uncertainty()` ###############################################

# Output figures (.pdf) and corresponding data (.csv) will be saved to the mean folder for scenario group a.

####################################################### Start script ############################################################

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
			  	group_label = figdirs_a[1]
			  } else if (g==2) {
			  	outputdir = outputdir_b
			  	figdirs = figdirs_b
			  	scenarios = scenarios_b
			  	num_scen = num_scen_b
			  	group_label = figdirs_b[1]
			  } else {
			  	outputdir = outputdir_c
			  	figdirs = figdirs_c
			  	scenarios = scenarios_c
			  	num_scen = num_scen_c
			  	group_label = figdirs_c[1]
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
				    	} # end n loop over desired scenarios
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
			  
			  # add group label column
			  group_df$Group = group_label
			  
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
			  	all_df$Scenario[all_df$Scenario==scenarios_c[n]] = scen_labs_c[n]
			  }
			  
			  # get list of scenario labels
			  scen_labels = as.character(unique(all_df$Scenario))
			    
        ### plot scenarios on one plot  
			  p <- ggplot(all_df) + geom_line(aes(x=Year, y=Mean, linetype=Scenario, color=Scenario)) + ylab(ylabel) +
			    geom_ribbon(data=all_df,aes(x=Year, y=Mean, ymin=Min,ymax=Max, group=Scenario, linetype=Scenario),alpha=0.075) +
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
			  write.csv(all_df, out_csv, row.names=FALSE)
			  
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
