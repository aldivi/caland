# plot_uncertainty.r

# plot the mean output with shaded uncertainty region for a given variable
# the inputs to this function are diagnostic output csv files from plot_caland()
# this script includes 2 functions: wrapper and plot_uncertainty
#	wrapper manipulates text for the plots

# plot_uncertainty() takes 14 arguments:

##	start_year  year to start plotting
##  end_year    year to end plotting
##  varname		name of variable to plot (see the outputs from plot_caland)
                # this name is between the land type and "_output" in these file names; do not include the surrounding "_" characters
##	ylabel		y label for the plot (assign value 1 to 4); this indicates the units and whether it is a difference from baseline. Current options:
                # 1 = "Change from Baseline (MMT CO2e)"
                # 2 = "MMT CO2e"
                # 3 = "Change from Baseline (MMT C)"
                # 4 = "MMT C"
##	data_dir		the path to the directory containing the three folders of plot_caland outputs (mean, low, and high); do not include the "/" character at 
                # the end; default is "./outputs"
##	figdirs		the three folders within data_dir containing the data to plot. These must be assigned in order of mean, low, and high. The figures will
                # be written to the folder representing the mean; do not include the "/" character at the end
                # the csv files are assumed to be in <data_dir>/<figdir>, in the appropriate region and land type and ownership directories
##  scen_labs   scenario labels; must include three, end with the reference baseline, and correspond to files assigned to scen_a, scen_b, and base arguments;
                # default = c("A", "B", "Baseline")
##  scen_a      filename corresponding to first scenario in scen_labs
##  scen_b      filename corresponding to second scenario in scen_labs
##  base        filename corresponding to third scenario in scen_labs; must be the reference baseline scenario
##	file_tag		tag to add to end of new file names (e.g., to note what time period is plotted); default is "" (nothing added)
##	reg			array of region names to plot; default = "All_region", but can be any number of available types:
                #	"All_region", "Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast", 
                # Note: plotting the ocean region doesn't provide any comparison with land types because only seagrass exists in the ocean
##	lt			array of land types to plot;  default = "All_land", but can be any number of available types 
                # Note: Seagrass has only a subset of the output files, so if including for All_region make sure that the desired varname is available
##	own			array of ownerships to plot;  default = "All_own", but can be any number of available types 


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

plot_uncertainty <- function(start_year=2010, end_year=2051, varname, ylabel, data_dir = "./outputs", figdirs, scen_labs = c("A", "B", "Baseline"), 
                             scen_a, scen_b, base, file_tag="", reg = "All_region", lt = "All_land", own="All_own") {
  
  if (length(scen_labs) == 2 & substr(varname, nchar(varname)-3, nchar(varname)) != "diff") {
    cat( "Scenario names are incomplete for variable with absolute units: see scen_labs\n" )
    stop( "Ensure that scenario A, B, and Baseline are assigned to arguments: scen_labs, scen_a, scen_b, and base\n" )
  }
  
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
      
      # create empty all_df table to fill with the mean, low and hi outputs below
		  all_df = NULL

		  # loop over land types to get data
		  for (i in 1:num_lt) {
		    
		    lt_lab = lt[i]

			  # read in the 3 files (mean, lo, hi) and select rows in start:end_year
			  for (f in 1:3) {
			    fig_lab <- figdirs[f]
				  fname = paste0(outputdir, fig_lab, "/", reg_lab, "/", lt_lab, "/", own_lab, "/", reg_lab, "_", lt_lab, "_", own_lab, "_", varname, "_output.csv")
				  in_df_pre = read.csv(fname)
				  # extract the years specified between start_year and end_year for each scenario
          # select rows for each scenario
				  scen_names = as.character(unique(in_df_pre$Scenario))
				  num_scen = length(scen_names)
				  in_df = NULL
				  
				  for (a in 1:num_scen) {
				    scen_lab = scen_names[a]
				    # subset scenarios to get specific year range
				    scen <- in_df_pre[in_df_pre$Scenario==scen_lab,]
				    # subset years in scenario and save each scenario to in_df
				    in_df <- rbind(in_df, scen[scen$Year %in% start_year:end_year,])
				  } # end reading in each of the scenarios in the current file
				  
				  # subset the current region and ownership for given land type
				  in_df = in_df[in_df$Region == reg_lab & in_df$Ownership == own_lab,]
				  
				  # save each (mean, lo, hi) as their own column
				  # mean
				  if (f==1) {
				    all_df <- rbind(all_df, in_df)
				    } else if (f==2) {
				      all_df <- cbind(all_df,in_df$Value)
				      colnames(all_df)[ncol(all_df)] <- "Min"
				    } else {
				      all_df <- cbind(all_df,in_df$Value)
				      colnames(all_df)[ncol(all_df)] <- "Max"
				    } # end reorganizing columns with mean, max, min
			  } # end reading in each file (mean, lo, hi)
		    
		    # get full scenario names
			  scen_names <- as.character(unique(all_df$Scenario))
			  # get number of scenarios
			  num_scen <- length(scen_names)
			  
			  # extract beginning of scenario full name (use first scenario as sample which is deleted)
			  scen_lab <- substr(scen_names[1], 1, regexpr("_output", scen_names[i])-1)
			  # extract NWL scenario version, NR setting (default=default), and climate
			  file_lab <- paste0("v",gsub("^.*?_v", "", scen_lab))
			  # create plot tile combining the geography, version/assumptions, and variable
			  title <- paste(reg_lab, own_lab, lt_lab, file_lab, varname)

			  # replace the names of scenarios for plotting
			  levels(all_df$Scenario)[levels(all_df$Scenario)==scen_a] <- scen_labs[1]
			  levels(all_df$Scenario)[levels(all_df$Scenario)==scen_b] <- scen_labs[2]
        levels(all_df$Scenario)[levels(all_df$Scenario)==base] <- scen_labs[3]
			    
        ### plot scenarios on one plot  
			  p <- ggplot(all_df) + geom_line(aes(x=Year, y=Value, colour=Scenario)) +
			    geom_ribbon(data=all_df,aes(x=Year, y=Value, ymin=Min,ymax=Max, group=Scenario),alpha=0.2) +
			    ggtitle(wrapper(title, width=40)) + theme_bw() + theme(legend.text=element_text(size=12),
			                                                              plot.title = element_text(size=8),
			                                                              axis.text=element_text(size=12), 
			                                                              axis.title.y=element_text(size=12),
			                                                              axis.title.x=element_text(size=12),
			                                                              legend.title=element_text(size=12))
			   # customize y laxis label
			  if (ylabel == 1) {
			    p <- p + ylab(expression("Change from Baseline (MMT CO"["2"]*"e)"))
			    } # end customize y axis label 
			  if (ylabel == 2) {
			    p <- p  + ylab(expression("MMT CO"["2"]*"e"))  
			    }
			  if (ylabel == 3) {
			    p <- p  + ylab(expression("Change from Baseline (MMT C)"))  
			    }
			  if (ylabel == 4) {
			    p <- p  + ylab(expression("MMT C"))  
			    } 
			  
			  
			  out_file = paste0(outputdir, figdirs[1], "/", reg_lab, "/", reg_lab, "_", own_lab, "_", lt_lab, "_", varname, "_scen_comp_uncert_bands", added_file_tag, ".pdf")
			  ggsave(out_file,dpi=300, width=2560/300, height=1540/300)
			  
			  #### plot scenarios individually
			  for ( i in 1:num_scen) {
			    
			    # get scehnario name for labeling
			    scen_lab <- paste0("Scenario_",scen_labs[i])
			    
			    # create plot tile combining the geography, scenario, version/assumptions, and variable
			    title <- paste(reg_lab, own_lab, lt_lab, scen_lab, file_lab, varname)
			   
			    p <- ggplot(all_df[all_df$Scenario==scen_labs[i],]) + geom_line(aes(x=Year, y=Value, colour=Scenario)) +
			       geom_ribbon(data=all_df[all_df$Scenario==scen_labs[i],],
			                   aes(x=Year, y=Value, ymin=Min,ymax=Max, group=Scenario),alpha=0.2) +
			       ggtitle(wrapper(title, width=40)) + theme_bw() + theme(legend.text=element_text(size=12),
			                                                              plot.title = element_text(size=8),
			                                                              axis.text=element_text(size=12), 
			                                                              axis.title.y=element_text(size=12),
			                                                              axis.title.x=element_text(size=12),
			                                                              legend.title=element_text(size=12))
			    
			    # customize y laxis label
			     if (ylabel == 1) {
			       p <- p + ylab(expression("Change from Baseline (MMT CO"["2"]*"e)")) 
			     } # end customize y axis label 
			     if (ylabel == 2) {
			       p <- p  + ylab(expression("MMT CO"["2"]*"e"))  
			     }
			     if (ylabel == 3) {
			       p <- p  + ylab(expression("Change from Baseline (MMT C)"))  
			     }
			     if (ylabel == 4) {
			       p <- p  + ylab(expression("MMT C"))  
			     }
			    
			     out_file = paste0(outputdir, figdirs[1], "/", reg_lab, "/", reg_lab, "_", own_lab, "_", lt_lab, "_", scen_lab, "_", 
			                       varname, "_scen_comp_uncert_bands", added_file_tag, ".pdf")
			     ggsave(out_file,dpi=300, width=2560/300, height=1540/300)
			     
			     } # end plot individual scenarios
			  } # end for i loop over reading land types
		  } # end o loop over ownerships
    } # end r loop over regions
  } # end plot_uncertainty()
