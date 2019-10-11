# write_scaled_raw_scenarios.r

# Copyright (c) 2016-2019, Alan Di Vittorio and Maegen Simmonds

# This software and its associated input data are licensed under the 3-Clause BSD open source license
# Please see license.txt for details

# Converts an area specific (e.g., county) raw scenario file into a scaled version for generating CALAND input files
# Also saves the conversion info for use by scale_caland_outputs.r, which converts CALAND outputs to the area-specific values

# This is designed to generate files for estimating county level effects of management
#   So only the management areas and the county name are needed

# Alternatively, the estimates for a 'project' in a single, specific land category can be estimated by providing more information:
#   total available area and the region

# The input units will be preserved in the scaled output
# This is not needed for Seagrass as it resides in the ocean

# The input and output raw scenario files are assumed to be in the 'raw_data' folder

############# Arguments to write_scaled_raw_scenarios.r ##############

# 1. 'scen_file': name of raw scenario file for a specific area, such as a county; default is amador_example.xls
#    This file must be in the raw_data folder
#    If you want this and the scaled file to be in a folder within raw_data, prefix the file name with the existing folder
#    For counties, the Region(s) can be specified explicitly,
#    OR the entries can have "All" for the Region - in this case the approprieate regions will be determined

# 2. 'county': name of the county; default is Amador; this must match the county name in the county area file
#	 For project level, enter the name of the region instead

# 3. 'units': the area units for the input scenario file; default is 'ac'; could also be 'ha'

# 4. 'ISCOUNTY': flag to denote whether this scenario is for a county (TRUE), or for a specified area (FALSE)
#    Counties are directly supported for checking areas and scaling automatically
#    Other specified areas (ISCOUNTY=FALSE) require additional information about total available area for the project,
#     and are restricted to a single specific land category, e.g. Klamath Private Forest
#     this is for exploratory research only and has not been tested or validated
#      there may be some inconsistencies between project and region for restoration source areas in this mode
#    default is TRUE, and any other value will return an error

# 5. 'avail_area': the total available area of the project within a single land category; default is NA
#    this applies only to project-level, non-county simulations
#    for an annual area practice, this is the total area of the available project area within a land category
#    for a restoration practice, this is the initial area of the restored land type within the available project area
#    this must be > 0

# 6. 'county_category_areas_file': the file containing the breakdown of county areas for the land categories;
#    default is area_lab_sp9_own9_2010lt15_cnty_sqm_stats.csv and this should not be changed


############### Outputs ##################

# Scaled raw scenario file for input to write_caland_inputs()
#   The county name and the units will be appended to the input file name

# A scaled raw scenario file that also includes columns for the county, county area, land cat area, and the scalar for the caland outputs

###################### Notes ######################

# Each scenario must at least have the three urban management practices defined in order to run, with all regions and ownerships defined (can use "All"):
#   Developed_all Dead_removal: must be defined as 1
#	Developed_all Urban_forest: baseline is defined as the statewide value of 0.15, but there are also reigonal values
#	Developed_all Growth: baseline is defined as 1

# The working directory needs to be the main CALAND folder where this file resides

# The output file name will be the same as the intput file name but with '_scaled_' and the units ('ac' or 'ha') appended to it

####################### Start script #####################

# this enables java to use up to 4GB of memory for reading and writing excel files
options(java.parameters = "-Xmx4g" )

# Load all the required packages
libs <- c( "XLConnect" )
for( i in libs ) {
    if( !require( i, character.only=T ) ) {
        cat( "Couldn't load", i, "\n" )
        stop( "Use install.packages() to download this library\nOr use the GUI Package Installer\nInclude dependencies, 
              and install it for local user if you do not have root access\n" )
    }
    library( i, character.only=T )
}

write_scaled_raw_scenarios <- function(scen_file = "amador_example_ac.xls", county = "Amador", units = "ac", ISCOUNTY = TRUE, avail_area = NA,
										county_category_areas_file = "area_lab_sp9_own9_2010lt15_cnty_sqm_stats.csv") {
	
	cat("Start write_scaled_raw_scenario at", date(), "\n")

	in_dir = "./raw_data/"
	out_dir = in_dir
	xltag = ".xls"
	csvtag = ".csv"
	
	# remove all whitespace from the county input string for use in output files names
	# this works on a vector as well
	ctag = gsub("//s", "", county)
	out_file = paste0(out_dir, substr(scen_file, 1, regexpr(".xls", scen_file)-1), "_", ctag, "_", units, xltag)
	out_scalar_file = paste0(out_dir, substr(scen_file, 1, regexpr(".xls", scen_file)-1), "_", ctag, "_", units, "_scalars", csvtag)

	# the column headers are on line 12, for both input and output files
	start_row = 12

	# the header goes from row 1 to row 10, and is only the first column
	last_head_row = 10

	# regions
	reg_names = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")
	num_reg = length(reg_names)

	# project-level estimation is only for exploratory research
	if (!ISCOUNTY) {
		stop("Project-level estimation has not been tested or validated and is for exploratory research only!\n")
	}
		
	# check that county is a region name for project-level and that avail_area > 0
	if (!ISCOUNTY) {
		reg_def = which(reg_names == county)
		if (length(reg_def) == 0) {
			stop("Arguemnt 'county' must be a region for non-county cases\n")
		}
		if (is.na(avail_area) | avail_area <= 0) {
			stop("Arguemnt 'avail_area' must be > 0\n")
		}
	}

	# land types
	lt_names = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow",
	       "Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all", "Seagrass")
	num_lt = length(lt_names)

	# ownerships
	own_names = c("BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")
	num_own = length(own_names)

	# urban
	urban_man = c("Dead_removal", "Urban_forest", "Growth")
	num_urban_man = length(urban_man)

	# restoration
	restoration_man = c("Restoration", "Afforestation", "Reforestation")
	num_restoration_man = length(restoration_man)
	
	# restoration land types
	# the first forest type is for afforestation
	# the second forest land type is for reforestation
	restoration_lt = c("Meadow", "Fresh_marsh", "Coastal_marsh", "Woodland", "Forest", "Forest")
	num_restoration_lt = length(restoration_lt)
	
	# restoration sources
	restoration_sources = array(dim=c(num_restoration_lt, num_lt))
	restoration_sources[1,] = c("Shrubland", "Grassland", "Savanna", "Woodland")
	restoration_sources[2,] = c("Cultivated")
	restoration_sources[3,] = c("Cultivated")
	restoration_sources[4,] = c("Grassland", "Cultivated")
	restoration_sources[5,] = c("Shrubland", "Grassland")
	restoration_sources[6,] = c("Shrubland")
	
	num_restoration_sources = c(4, 1, 1, 2, 2, 1)

	source_totals_names = c("Shrubland", "Grassland", "Savanna", "Woodland", "Cultivated")
	num_sources = length(source_totals_names)
	st_sum = array(dim=num_sources)
	st_sum_scaled = array(dim=num_sources)

	# conversion factors
	sqm2ha = 1/10000
	sqm2ac = 1/4046.8564

	# read in the area file (square meters)
	# then convert it to the desired units
	areas = read.csv(paste0(in_dir, county_category_areas_file), header=FALSE, stringsAsFactors=FALSE)
	colnames(areas) <- c("reg_code", "Region", "lt_code", "Land_Type", "own_code", "Ownership", "cnty_code", "County", "area_sqm")
	# drop the code columns
	areas$reg_code = NULL
	areas$lt_code = NULL
	areas$own_code = NULL
	areas$cnty_code = NULL
	# filter out 'no data' rows
	areas = areas[areas$Region != "no data" & areas$Land_Type != "no data" & areas$Ownership != "no data" & areas$County != "no data",]
	# convert units
	if (units == "ac") {
		areas$area = areas$area_sqm * sqm2ac
	} else {
		areas$area = areas$area_sqm * sqm2ha
	}
	areas$area_sqm = NULL

	scenin_wrkbk = loadWorkbook(paste0(in_dir,scen_file))
	# worksheet/table names
	scenin_sheets = getSheets(scenin_wrkbk)
	num_scenin_sheets = length(scenin_sheets)
	# NA values need to be converted to numeric
	# the warnings thrown by readWorksheet below are ok because they just state that the NA string can't be converted a number so it is 
	# converted to NA value
	scenin_df_list <- list()
	for (i in 1:num_scenin_sheets) {
		scenin_df_list[[i]] <- readWorksheet(scenin_wrkbk, i, startRow = start_row, colTypes = c(rep("character",4), rep("numeric",50)),
		forceConversion = 	TRUE)
	} # end for i loop to read in scenarios

	###### read the scenario headers for the outputs
	scen_head_df_list <- list()
	for (i in 1:num_scenin_sheets) {
		scen_head_df_list[[i]] <- readWorksheet(scenin_wrkbk, i, startRow = 1, endRow = last_head_row, header = FALSE)
	}
	
	# get the column headers in order
	col_order = colnames(scenin_df_list[[1]])
	# get the column headers in order for the scalar output
	col_order_scalar = c(col_order, "County", "cnty_lc_area", "lc_area", "out_scalar")

	# first copy the input worksheets to the outputs to set up the lists
	out_scen_df_list = scenin_df_list
	out_scen_sheets = scenin_sheets
	# this one is for the table with the scalar values
	out_man_df_list = scenin_df_list

	# get the total land category areas
	# there are no NA values
	lc_areas = aggregate(area ~ Region + Land_Type + Ownership, areas, FUN = sum)
	names(lc_areas)[ncol(lc_areas)] <- "lc_area"

	# determine total region-land type areas
	reg_lt_areas = aggregate(area ~ Region + Land_Type, areas, FUN = sum)
	names(reg_lt_areas)[ncol(reg_lt_areas)] <- "reg_lt_area"

	# get the county area values
	# there will always be a county name even if it isn't used for project-level
	# subset county land cat areas
	cnty_lc_areas = areas[areas$County == county,]
	names(cnty_lc_areas)[ncol(cnty_lc_areas)] <- "cnty_lc_area"
	num_cnty_lc = nrow(cnty_lc_areas)
			
	# determine county region-land type areas
	cnty_reg_lt_areas = aggregate(cnty_lc_area ~ Region + Land_Type, cnty_lc_areas, FUN = sum)
	names(cnty_reg_lt_areas)[ncol(cnty_reg_lt_areas)] <- "cnty_reg_lt_area"
	num_cnty_reg_lt = nrow(cnty_reg_lt_areas)
				
	# determine county ownership-land type areas
	cnty_own_lt_areas = aggregate(cnty_lc_area ~ Ownership + Land_Type, cnty_lc_areas, FUN = sum)
	names(cnty_own_lt_areas)[ncol(cnty_own_lt_areas)] <- "cnty_own_lt_area"
	num_cnty_own_lt = nrow(cnty_own_lt_areas)
				
	# determine county land type areas
	cnty_lt_areas = aggregate(cnty_lc_area ~ Land_Type, cnty_lc_areas, FUN = sum)
	names(cnty_lt_areas)[ncol(cnty_lt_areas)] <- "cnty_lt_area"
	num_cnty_lt = nrow(cnty_lt_areas)

	# merge these county data into a single data frame
	cnty_areas = merge(cnty_lc_areas, cnty_reg_lt_areas, by = c("Region", "Land_Type"), all.x = TRUE)
	cnty_areas = merge(cnty_areas, cnty_own_lt_areas, by = c("Ownership", "Land_Type"), all.x = TRUE)
	cnty_areas = merge(cnty_areas, cnty_lt_areas, by = c("Land_Type"), all.x = TRUE)

	# get the names of the regions in this county
	if (!ISCOUNTY) {
		county_regions = county
	} else {
		county_regions = unique(cnty_lc_areas$Region)
	}
	num_county_regions = length(county_regions)

	# the scalar values for the output files are needed for all land cats in the county
	# for project-level only the affected land cats are scaled and used - subset and adjust this at the end as necessary
	output_scalars = merge(cnty_lc_areas, lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
	output_scalars$output_scalar = output_scalars$cnty_lc_area / output_scalars$lc_area

### may not need the next two or the merge
	# determine total region areas
	#reg_areas = aggregate(area ~ Region, areas, FUN = sum)
	#names(reg_areas)[ncol(reg_areas)] <- "reg_area"
	#reg_areas$state_area = sum(reg_areas$reg_area)
			
	# determine total ownership areas by region
	#reg_own_areas = aggregate(area ~ Region + Ownership, areas, FUN = sum)
	#names(reg_own_areas)[ncol(reg_own_areas)] <- "reg_own_area"
	#num_reg_own = array(dim = num_reg)
	#for (r in 1: num_reg) {
	#	num_reg_own[r] = nrow(reg_own_areas[reg_own_areas$Region == reg_areas$Region[r]])
	#}
	
	# merge these into a single data frame
	#tot_areas = merge(lc_areas, reg_own_areas, by = c("Region", "Ownership"), all.x = TRUE)
	#tot_areas = merge(tot_areas, areas, by = c("Region"), all.x = TRUE)
###

	# store the project-level land cats here
	project_lcs = NULL

	# loop over the scenario sheets
	for (s in 1: num_scenin_sheets) {

		# Split annual, restoration, and urban records here to check that project-level operates on only annual or restoration
		#  not sure how to check for urban-only for project-level

		annual = out_scen_df_list[[s]][out_scen_df_list[[s]]$Management!="Restoration" & out_scen_df_list[[s]]$Management!="Afforestation" &
			out_scen_df_list[[s]]$Management!="Reforestation" & out_scen_df_list[[s]]$Land_Type!="Developed_all",]
		
		cumulative = out_scen_df_list[[s]][out_scen_df_list[[s]]$Management=="Restoration" | out_scen_df_list[[s]]$Management=="Afforestation" |
			out_scen_df_list[[s]]$Management=="Reforestation",]
		
		urban = out_scen_df_list[[s]][out_scen_df_list[[s]]$Land_Type == "Developed_all",]

		if (!ISCOUNTY) {
			if (nrow(annual) > 0 & nrow(cumulative) > 0) {
				stop("Specify only annual practices or only a restoration practice, for a single land category, for project-level estimation\n")
			}
		}

		###### annual area practices
		# these are entered as the annual value during each year, including the start and end years
		# these require area scaling based on:
		# new man area =  orig man area * land category area / land category county(or avail-project) area
		# need to save the inverse of these ratios in a table by land category for output scaling	
		# orig man area is checked against available area and is adjusted if necessary, with warnings output to ?terminal/log?
		# counties are distributed proportionally among relevant regions if 'All' is entered in Region column
		# for projects, only a single, specific land catergory (Region, own, land type) is valid, and the avail area is checked and adjusted
		
		if(nrow(annual) > 0) {
		
			if (!ISCOUNTY) {
				# full land category must be specified
				if ( !(all(annual$Region %in% reg_names) & all(annual$Ownership %in% own_names) & all(annual$Land_Type %in% lt_names)) ) {
					stop("For project-level application the land category has to be completely specified for Region, Ownership, and Land Type\n")
				}
				
				# make sure only one land category is annually managed
				check_df = unique(annual[,1:3])
				if (nrow(check_df) > 1) {
					stop("Only one land category can be managed for project-level application\n")
				}

				# make sure the total project available area value does not exceed the total physical area
				check_df = merge(annual, lc_areas, by = c("Region", "Land_Type", "Ownership", ), all.x = TRUE)
				if (avail_area > check_df$lc_area[1]) {
					stop("The avail_area for project-level application must be <= the total initial physical land category area of ", check_df$lc_area[1], units, "\n")
				}

				# make sure that the input region matches the record region
				if (annual$Region[1] != county) {
					stop("The management record region must match the county input argument\n")
				}

				# put available project area here in the same column name as for counties below
				# 	one input area will define the available area because project-level must be specified by a single land category
				# cnty_lc_area is the total available project area for this land type in this ownership and region
				#	this will be available source area for a restoration land type

				annual_reg_own = annual
				annual_reg_own$County = county
				annual_reg_own$cnty_lc_area = avail_area
				
			} else { # county
				
				# separate any "All" region entries so that the appropriate regions can be specified
				# ownerships can be mixed
				# the records will be expanded to full land categories
				#annual_all_reg = annual[annual$Region == "All",]
				#annual_reg = annual[annual$Region != "All",]
				#if(nrow(annual_all_reg) > 0 & nrow(annual_reg) > 0){
				#	stop("Regions must EITHER be specified OR all Region entries must be set to 'All'\n")
				#}
				
				# now separate into the four reg-own cases
				annual_all = annual[annual$Region == "All" & annual$Ownership == "All",]
				annual_all_reg = annual[annual$Region == "All" & annual$Ownership != "All",]
				annual_all_own = annual[annual$Region != "All" & annual$Ownership == "All",]
				annual_reg_own_orig = annual[annual$Region != "All" & annual$Ownership != "All",]
				
				# distribute records across regions if necessary
				# expand the records and adjust management areas
				# then strip the added columns to bind them together
				# then merge the lc area for scalar calc 
				
				annual_reg_own = NULL
				
				# start with the allreg-allown case and append these to the region-allown case below
				if(nrow(annual_all) > 0){
					# distribute the records across regions
					num_recs = nrow(annual_all)
					new_records = NULL
					for (r in 1:num_recs) {
						temp_df = annual_all[r,]
						temp_df = merge(temp_df, 
							unique(cnty_areas[,c("Region", "Land_Type", "cnty_reg_lt_area", "cnty_lt_area")]),
							by = c("Land_Type"), all.x = TRUE)
						temp_df$Region.x = temp_df$Region.y
						temp_df$Region.y = NULL
						colnames(temp_df)[colnames(temp_df) == "Region.x"] = "Region"
						temp_df$start_area = temp_df$start_area * temp_df$cnty_reg_lt_area / temp_df$cnty_lt_area
						temp_df$end_area = temp_df$end_area * temp_df$cnty_reg_lt_area / temp_df$cnty_lt_area
						temp_df$cnty_reg_lt_area = NULL
						temp_df$cnty_lt_area = NULL
						new_records = rbind(new_records, temp_df)
					}
					# make sure the new table has the same column order
					new_records = new_records[,col_order]
					if (nrow(annual_all_own) > 0) {
						annual_all_own = rbind(annual_all_own, new_records)
					} else {
						annual_all_own = new_records
					}	
				} # end if all-all case
				
				# do the reg-allown case
				if(nrow(annual_all_own) > 0){
					# distribute the records across ownerships
					num_recs = nrow(annual_all_own)
					new_records = NULL
					for (r in 1:num_recs) {
						# first make sure that the record regions match the county
						cr_ind = which(county_regions == annual_all_own$Region[r])
						if (length(cr_ind) == 0) {
								stop("Record region ", annual_all_own$Region[r], " is not in county ", county, " region list: ", county_regions, "\n")
						}
						temp_df = annual_all_own[r,]
						temp_df = merge(temp_df,
							unique(cnty_areas[,c("Region", "Land_Type", "Ownership", "cnty_reg_lt_area", "cnty_lc_area")]),
							by = c("Region", "Land_Type"), all.x = TRUE)
						temp_df$Ownership.x = temp_df$Ownership.y
						temp_df$Ownership.y = NULL
						colnames(temp_df)[colnames(temp_df) == "Ownership.x"] = "Ownership"
						temp_df$start_area = temp_df$start_area * temp_df$cnty_lc_area / temp_df$cnty_reg_lt_area
						temp_df$end_area = temp_df$end_area * temp_df$cnty_lc_area / temp_df$cnty_reg_lt_area
						temp_df$cnty_reg_lt_area = NULL
						temp_df$cnty_lc_area = NULL
						new_records = rbind(new_records, temp_df)
					}
					# make sure the new table has the same column order
					new_records = new_records[,col_order]
					annual_reg_own = rbind(annual_reg_own, new_records)
				} # end if region-allown case
				
				# do the allreg-own case
				if(nrow(annual_all_reg) > 0){
					# distribute the records across regions
					num_recs = nrow(annual_all_reg)
					new_records = NULL
					for (r in 1:num_recs) {
						temp_df = annual_all_reg[r,]
						temp_df = merge(temp_df,
							unique(cnty_areas[,c("Region", "Land_Type", "Ownership", "cnty_own_lt_area", "cnty_lc_area")]),
							by = c("Ownership", "Land_Type"), all.x = TRUE)
						temp_df$Region.x = temp_df$Region.y
						temp_df$Region.y = NULL
						colnames(temp_df)[colnames(temp_df) == "Region.x"] = "Region"
						temp_df$start_area = temp_df$start_area * temp_df$cnty_lc_area / temp_df$cnty_own_lt_area
						temp_df$end_area = temp_df$end_area * temp_df$cnty_lc_area / temp_df$cnty_own_lt_area
						temp_df$cnty_own_lt_area = NULL
						temp_df$cnty_lc_area = NULL
						new_records = rbind(new_records, temp_df)
					}
					# make sure the new table has the same column order
					new_records = new_records[,col_order]
					annual_reg_own = rbind(annual_reg_own, new_records)
				} # end if allregion-own case
				
				# bind the newly expanded records with any fully specified records (reg-own case)
				if (nrow(annual_reg_own_orig) > 0) {
					for (r in 1:nrow(annual_reg_own_orig)) {
						# first make sure that the record regions match the county
						cr_ind = which(county_regions == annual_reg_own_orig$Region[r])
						if (length(cr_ind) == 0) {
							stop("Record region ", annual_reg_own_orig$Region[r], " is not in county ", county, " region list: ", county_regions, "\n")
						}
					}
					annual_reg_own = rbind(annual_reg_own, annual_reg_own_orig)
				}
				
				# now all records should be fully expanded to land category
				# add the county and county lc area cols
				annual_reg_own = merge(annual_reg_own, cnty_lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
				
			} # end else county
		
			# now check to make sure that managed areas do not exceed initial available areas
			# just check the max managed area over time against the initial available area
			# this won't capture limitations due to LULCC
			# need to sum simultaneous practices within each land category
			unique_lc = unique(annual_reg_own[,c(1:3)])
			for (lc in 1:nrow(unique_lc)) {
				# extract the records for this land cat
				check_df = annual_reg_own[annual_reg_own$Region == unique_lc$Region[lc] & annual_reg_own$Ownership == unique_lc$Ownership[lc] &
											annual_reg_own$Land_Type == unique_lc$Land_Type[lc],]
				first_year = min(check_df$start_year)
				last_year = max(check_df$end_year)
				# loop over years
				for (y in first_year:last_year) {
					check_df$yearcol = 0
					check_df$yearcol[check_df$start_year <= y & check_df$end_year >= y] =
						apply(check_df[check_df$start_year <= y & check_df$end_year >= y,c("start_area", "end_area")], 1, max)
					# sum the col and check it
					total = sum(check_df$yearcol)
					if (total > check_df$cnty_lc_area[1]) {
						cat("Summed managed annual area ", total, " exceeds initial available area", check_df$cnty_lc_area[1], " for:\n")
						stop("\t", check_df$Region[1], ", ", check_df$Ownership[1], ", ", check_df$Land_Type[1], "\n")
					}
				} # end for y loop over years
			} # end for lc loop to check managed areas within available area
			
			# now scale the management areas to representative regional areas
			# calculate the output scaling ratio and use it here so that it is easily written for later use
			# first get the region level land cat area
			annual_reg_own = merge(annual_reg_own, lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
			annual_reg_own$out_scalar = annual_reg_own$cnty_lc_area / annual_reg_own$lc_area
			# scale the values
			annual_reg_own$start_area = annual_reg_own$start_area / annual_reg_own$out_scalar
			annual_reg_own$end_area = annual_reg_own$end_area / annual_reg_own$out_scalar
			
			if (!ISCOUNTY) {
				# save the info for this scenario so that scalars can be subset and adjusted
				# add the source columns for binding with other scenarios that may have restoration
				annual_reg_own$src_cnty_lc_area = NA
				annual_reg_own$src_lc_area = NA
				project_lcs = rbind(project_lcs, annual_reg_own)
			}
			
		} # end if annual practices

			
	
		###### restoration practices
		# these are entered as cumulative totals by the end of the target end year, so start_area should always be zero
		# these require area scaling based on the restored land type to ensure proper carbon density calculations:
		#  new cum area =  orig cum area * initial area of restored land category / initial area of restored land category in county(or avail-project)
		#  the restored area constitutes a carbon density change, but the converted area maintains its carbon density  
		# need to save the inverse of these ratios in a table by land category
		# orig cum area is checked against available source area and an error is thrown if not enough available area
		# also, the scaled source area needs to be checked to ensure scalability; an error is thrown if there is not enough
		#  this means that in some cases the target restored area for the county may not be able to be simulated
		#  this can occur if a considerable proprtion of the source area is in the county compared to the region as a whole
		# counties are distributed proportionally among relevant regions if 'All' is entered in Region column
		# for projects, the sources are for only a single, specific land catergory (Region, own, land type), and the avail area is checked and adjusted
		
		# Both Reforestation and Afforestion cannot be done at the same time
		#  technically, this is just within a particular land category, but throw error regardless of this
		#  this isn't caught in write_caland_inputs(), but it can crash CALAND
		aff_df = cumulative[cumulative$Management == "Afforestation",]
		ref_df = cumulative[cumulative$Management == "Reforestation",]
		if (nrow(aff_df) > 0 & nrow(ref_df) > 0) {
			cat("Afforestation and Reforestation cannot be prescribed at the same time\n")
			stop("It is recommended to use Reforestation for all forest area expansion activities, unless you are sure that you want to convert some Grassland to Forest\n")
		}
		
		if(nrow(cumulative) > 0) {
		
			# make sure start area is zero
			if (sum(cumulative$start_area) > 0) {
				stop("Start area for all restoration practices must be zero because this is a cumulative definition\n")
			}
		
			if (!ISCOUNTY) {
				# full land category must be specified
				if ( !(all(cumulative$Region %in% reg_names) & all(cumulative$Ownership %in% own_names) & all(cumulative$Land_Type %in% lt_names)) ) {
					stop("For project-level application the land category has to be completely specified for Region, Ownership, and Land Type\n")
				}
				
				# make sure only one land category is cumulatively managed
				check_df = unique(cumulative[,1:3])
				if (nrow(check_df) > 1) {
					stop("Only one land category can be managed for project-level application\n")
				}

				# make sure that the input region matches the record region
				if (cumulative$Region[1] != county) {
					stop("The management record region must match the county input argument\n")
				}

				# put available project area here in the same column name as for counties below
				# 	one input area will define the available area because project-level must be specified by a single land category
				# cnty_lc_area is the total initial area for this restored land type in this ownership and region
				# lc_area will be added below with the region-level total initial area of the restored land type
				# source columns are added and filled below

				cum_reg_own = cumulative
				cum_reg_own$County = county
				cum_reg_own$cnty_lc_area = avail_area
				
			} else { # county
				
				# this expansion could probably be done together with the annual
				
				# separate any "All" region entries so that the appropriate regions can be specified
				# ownerships can be mixed
				# the records will be expanded to full land categories
				#cum_all_reg = cumulative[cumulative $Region == "All",]
				#cum_reg = cumulative[cumulative $Region != "All",]
				#if(nrow(cum_all_reg) > 0 & nrow(cum_reg) > 0){
				#	stop("Regions must EITHER be specified OR all Region entries must be set to 'All'\n")
				#}
				
				# now separate into the four reg-own cases
				cum_all = cumulative[cumulative$Region == "All" & cumulative$Ownership == "All",]
				cum_all_reg = cumulative[cumulative$Region == "All" & cumulative$Ownership != "All",]
				cum_all_own = cumulative[cumulative$Region != "All" & cumulative$Ownership == "All",]
				cum_reg_own_orig = cumulative[cumulative$Region != "All" & cumulative$Ownership != "All",]
				
				# distribute records across regions if necessary
				# expand the records and adjust management areas
				# then strip the added columns to bind them together
				# then merge the lc area for scalar calc 
				
				cum_reg_own = NULL
				
				# start with the allreg-allown case and append these to the region-allown case below
				if(nrow(cum_all) > 0){
					# distribute the records across regions
					num_recs = nrow(cum_all)
					new_records = NULL
					for (r in 1:num_recs) {
						temp_df = cum_all[r,]
						temp_df = merge(temp_df, 
							unique(cnty_areas[,c("Region", "Land_Type", "cnty_reg_lt_area", "cnty_lt_area")]),
							by = c("Land_Type"), all.x = TRUE)
						temp_df$Region.x = temp_df$Region.y
						temp_df$Region.y = NULL
						colnames(temp_df)[colnames(temp_df) == "Region.x"] = "Region"
						temp_df$start_area = temp_df$start_area * temp_df$cnty_reg_lt_area / temp_df$cnty_lt_area
						temp_df$end_area = temp_df$end_area * temp_df$cnty_reg_lt_area / temp_df$cnty_lt_area
						temp_df$cnty_reg_lt_area = NULL
						temp_df$cnty_lt_area = NULL
						new_records = rbind(new_records, temp_df)
					}
					# make sure the new table has the same column order
					new_records = new_records[,col_order]
					if (nrow(cum_all_own) > 0) {
						cum_all_own = rbind(cum_all_own, new_records)
					} else {
						cum_all_own = new_records
					}	
				} # end if all-all case
				
				# do the reg-allown case
				if(nrow(cum_all_own) > 0){
					# distribute the records across ownerships
					num_recs = nrow(cum_all_own)
					new_records = NULL
					for (r in 1:num_recs) {
						# first make sure that the record regions match the county
						cr_ind = which(county_regions == cum_all_own$Region[r])
						if (length(cr_ind) == 0) {
								stop("Record region ", cum_all_own$Region[r], " is not in county ", county, " region list: ", county_regions, "\n")
						}
						temp_df = cum_all_own[r,]
						temp_df = merge(temp_df,
							unique(cnty_areas[,c("Region", "Land_Type", "Ownership", "cnty_reg_lt_area", "cnty_lc_area")]),
							by = c("Region", "Land_Type"), all.x = TRUE)
						temp_df$Ownership.x = temp_df$Ownership.y
						temp_df$Ownership.y = NULL
						colnames(temp_df)[colnames(temp_df) == "Ownership.x"] = "Ownership"
						temp_df$start_area = temp_df$start_area * temp_df$cnty_lc_area / temp_df$cnty_reg_lt_area
						temp_df$end_area = temp_df$end_area * temp_df$cnty_lc_area / temp_df$cnty_reg_lt_area
						temp_df$cnty_reg_lt_area = NULL
						temp_df$cnty_lc_area = NULL
						new_records = rbind(new_records, temp_df)
					}
					# make sure the new table has the same column order
					new_records = new_records[,col_order]
					cum_reg_own = rbind(cum_reg_own, new_records)
				} # end if region-allown case
				
				# do the allreg-own case
				if(nrow(cum_all_reg) > 0){
					# distribute the records across regions
					num_recs = nrow(cum_all_reg)
					new_records = NULL
					for (r in 1:num_recs) {
						temp_df = cum_all_reg[r,]
						temp_df = merge(temp_df,
							unique(cnty_areas[,c("Region", "Land_Type", "Ownership", "cnty_own_lt_area", "cnty_lc_area")]),
							by = c("Ownership", "Land_Type"), all.x = TRUE)
						temp_df$Region.x = temp_df$Region.y
						temp_df$Region.y = NULL
						colnames(temp_df)[colnames(temp_df) == "Region.x"] = "Region"
						temp_df$start_area = temp_df$start_area * temp_df$cnty_lc_area / temp_df$cnty_own_lt_area
						temp_df$end_area = temp_df$end_area * temp_df$cnty_lc_area / temp_df$cnty_own_lt_area
						temp_df$cnty_own_lt_area = NULL
						temp_df$cnty_lc_area = NULL
						new_records = rbind(new_records, temp_df)
					}
					# make sure the new table has the same column order
					new_records = new_records[,col_order]
					cum_reg_own = rbind(cum_reg_own, new_records)
				} # end if allregion-own case
				
				# bind the newly expanded records with any fully specified records (reg-own case)
				if (nrow(cum_reg_own_orig) > 0) {
					# first make sure that the record regions match the county
					cr_ind = which(county_regions == cum_reg_own_orig$Region[r])
					if (length(cr_ind) == 0) {
						stop("Record region ", cum_reg_own_orig$Region[r], " is not in county ", county, " region list: ", county_regions, "\n")
					}
					cum_reg_own = rbind(cum_reg_own, cum_reg_own_orig)
				}
				
				# now all records should be fully expanded to land category
				# add the county and county lc area cols
				cum_reg_own = merge(cum_reg_own, cnty_lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)

			} # end else county
		
			# first get the region level land cat area
			cum_reg_own = merge(cum_reg_own, lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
			# calculate the output scaling ratio and use it here so that it is easily written for later use
			cum_reg_own$out_scalar = cum_reg_own$cnty_lc_area / cum_reg_own$lc_area
			
			# add the source cnty_lc_area and lc_area cols as NA because they will be filled below with total source area
			cum_reg_own$src_cnty_lc_area = NA
			cum_reg_own$src_lc_area = NA
		
			# now check to make sure that managed areas do not exceed initial available areas
			# just check the end managed area against the initial available area of the source categories
			# this won't capture limitations due to LULCC
			# need to sum like source category needs first, by region-ownership, using the county distributions
			# sum the scaled like source category needs, using the regional distributions
			# also reset the src_cnty_lc_area to the resepctive source area totals
			# also sum up the region-level land cat source areas for comparison

			unique_lc = unique(cum_reg_own[,c(1:3)])
			unique_ro = unique(unique_lc[,c(1,3)])
			num_ro = nrow(unique_ro)
			source_totals = array(dim=c(num_ro, num_sources))
			source_totals[,] = 0
			scaled_source_totals = array(dim=c(num_ro, num_sources))
			scaled_source_totals[,] = 0
			
			for (lc in 1:nrow(unique_lc)) {
				# extract the records for this land cat
				check_df = cum_reg_own[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc],]
				# determine restoration type index
				# Forest has two types of restoration, so sum them separately
				rt_ind = which(restoration_lt == unique_lc$Land_Type[lc])
				# determine the region-ownerhsip index for source totals
				ro_reg_ind = which(unique_ro$Region == unique_lc$Region[lc])
				ro_own_ind = which(unique_ro$Ownership == unique_lc$Ownership[lc])
				ro_ind = intersect(ro_reg_ind, ro_own_ind)
				if (length(rt_ind) == 1) {
					# non-forest restoration
					# sum the source areas
					st_den = 0
					st_sum[] = 0
					# sum the scaled source areas
					st_den2 = 0
					st_sum_scaled[] = 0
					sc_tot = 0 # this is to store the region-level total source area for comparison below
					for (src in 1:num_restoration_sources[rt_ind]) {
						st_ind = which(source_totals_names == restoration_sources[rt_ind,src])
						st_num = cnty_lc_areas$cnty_lc_area[cnty_lc_areas$Region == unique_lc$Region[lc] & cnty_lc_areas$Ownership == unique_lc$Ownership[lc] &
															cnty_lc_areas$Land_Type == source_totals_names[st_ind]]
						if(length(st_num) == 0) { st_num = 0 }
						st_den = st_den + st_num
						st_sum[st_ind] = st_sum[st_ind] + sum(check_df$end_area) * st_num
						# sum up the region-level total source area
						temp = lc_areas$lc_area[lc_areas$Region == unique_lc$Region[lc] & lc_areas$Ownership == unique_lc$Ownership[lc] &
															lc_areas$Land_Type == source_totals_names[st_ind]]
						if(length(temp) == 0) { temp = 0 }
						sc_tot = sc_tot + temp
						# calculate the scaled source needs
						st_den2 = st_den2 + temp
						st_sum_scaled[st_ind] = st_sum_scaled[st_ind] + sum(check_df$end_area) / check_df$out_scalar[1] * temp
					} # end for summing up individual sources for non-forest restoration
					# keep track of sources in this region-ownership
					source_totals[ro_ind,] = source_totals[ro_ind,] + st_sum / st_den
					scaled_source_totals[ro_ind,] = scaled_source_totals[ro_ind,] + st_sum_scaled / st_den2
					# set to zero if no source area
					source_totals[ro_ind,] <- replace(source_totals[ro_ind,], is.nan(source_totals[ro_ind,]), 0.0)
					scaled_source_totals[ro_ind,] <- replace(scaled_source_totals[ro_ind,], is.nan(scaled_source_totals[ro_ind,]), 0.0)
					cum_reg_own$src_cnty_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc]] = st_den
					cum_reg_own$src_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc]] = sc_tot
				} else if (length(rt_ind) == 2) {
					#forest restoration
					# split into appropriate table and do the same thing as above
					# afforestation
					aff_df = check_df[check_df$Management == "Afforestation",]
					if(nrow(aff_df) > 0) {
						rt_ind = 5
						# sum the source areas
						st_num = 0
						st_den = 0
						st_sum[] = 0
						# sum the scaled source areas
						st_den2 = 0
						st_sum_scaled[] = 0
						sc_tot = 0 # this is to store the region-level total source area for comparison below
						for (src in 1:num_restoration_sources[rt_ind]) {
							st_ind = which(source_totals_names == restoration_sources[rt_ind,src])
							st_num = cnty_lc_areas$cnty_lc_area[cnty_lc_areas$Region == unique_lc$Region[lc] &
															cnty_lc_areas$Ownership == unique_lc$Ownership[lc] &
															cnty_lc_areas$Land_Type == source_totals_names[st_ind]]
							if(length(st_num) == 0) { st_num = 0 }
							st_den = st_den + st_num
							st_sum[st_ind] = st_sum[st_ind] + sum(aff_df$end_area) * st_num
							# sum up the region-level total source area
							temp = lc_areas$lc_area[lc_areas$Region == unique_lc$Region[lc] & lc_areas$Ownership == unique_lc$Ownership[lc] &
															lc_areas$Land_Type == source_totals_names[st_ind]]
							if(length(temp) == 0) { temp = 0 } 
							sc_tot = sc_tot + temp
							# calculate the scaled source needs
							st_den2 = st_den2 + temp
							st_sum_scaled[st_ind] = st_sum_scaled[st_ind] + sum(check_df$end_area) / check_df$out_scalar[1] * temp
						} # end for summing up individual sources for non-forest restoration
						# keep track of sources in this region-ownership
						source_totals[ro_ind,] = source_totals[ro_ind,] + st_sum / st_den
						scaled_source_totals[ro_ind,] = scaled_source_totals[ro_ind,] + st_sum_scaled / st_den2
						# set to zero if no source area
						source_totals[ro_ind,] <- replace(source_totals[ro_ind,], is.nan(source_totals[ro_ind,]), 0.0)
						scaled_source_totals[ro_ind,] <- replace(scaled_source_totals[ro_ind,], is.nan(scaled_source_totals[ro_ind,]), 0.0)
						cum_reg_own$src_cnty_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc] & cum_reg_own$Management == "Afforestation"] = st_den
						cum_reg_own$src_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc] & cum_reg_own$Management == "Afforestation"] = sc_tot
					} # end if afforestation records
					
					# reforestation
					ref_df = check_df[check_df$Management == "Reforestation",]
					if(nrow(ref_df) > 0) {
						rt_ind = 6
						# sum the source areas
						st_num = 0
						st_den = 0
						st_sum[] = 0
						# sum the scaled source areas
						st_den2 = 0
						st_sum_scaled[] = 0
						sc_tot = 0 # this is to store the region-level total source area for comparison below
						for (src in 1:num_restoration_sources[rt_ind]) {
							st_ind = which(source_totals_names == restoration_sources[rt_ind,src])
							st_num = cnty_lc_areas$cnty_lc_area[cnty_lc_areas$Region == unique_lc$Region[lc] & 
															cnty_lc_areas$Ownership == unique_lc$Ownership[lc] &
															cnty_lc_areas$Land_Type == source_totals_names[st_ind]]
							if(length(st_num) == 0) { st_num = 0 }
							st_den = st_den + st_num
							st_sum[st_ind] = st_sum[st_ind] + sum(ref_df$end_area) * st_num
							# sum up the region-level total source area
							temp = lc_areas$lc_area[lc_areas$Region == unique_lc$Region[lc] & lc_areas$Ownership == unique_lc$Ownership[lc] &
															lc_areas$Land_Type == source_totals_names[st_ind]]
							if(length(temp) == 0) { temp = 0 }
							sc_tot = sc_tot + temp
							# calculate the scaled source needs
							st_den2 = st_den2 + temp
							st_sum_scaled[st_ind] = st_sum_scaled[st_ind] + sum(check_df$end_area) / check_df$out_scalar[1] * temp
						} # end for summing up individual sources for non-forest restoration
						# keep track of sources in this region-ownership
						source_totals[ro_ind,] = source_totals[ro_ind,] + st_sum / st_den
						scaled_source_totals[ro_ind,] = scaled_source_totals[ro_ind,] + st_sum_scaled / st_den2
						# set to zero if no source area
						source_totals[ro_ind,] <- replace(source_totals[ro_ind,], is.nan(source_totals[ro_ind,]), 0.0)
						scaled_source_totals[ro_ind,] <- replace(scaled_source_totals[ro_ind,], is.nan(scaled_source_totals[ro_ind,]), 0.0)
						cum_reg_own$src_cnty_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc] & cum_reg_own$Management == "Reforestation"] = st_den
						cum_reg_own$src_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc] & cum_reg_own$Management == "Reforestation"] = sc_tot
					} # end if reforestation records	
				} else if (length(rt_ind) == 0) {
					stop("Incorrect restoration land type ", unique_lc$Land_Type[lc], "\n")
				} # end if-else restoration type

				# loop over the source types and check against the actual county level source availability
				# for project-level the check is the total source against the total avail area input
				if (!ISCOUNTY) { 
					# check available initial area against physical initial area first
					if (cum_reg_own$cnty_lc_area[1] > cum_reg_own$lc_area[1]) {
						cat("Input initial restoration type area ", cum_reg_own$cnty_lc_area[1], " exceeds physical initial restoration type area ", cum_reg_own$lc_area[1], " for restoration type:\n")
						cat("\t", cum_reg_own$Region[1], ", ", cum_reg_own$Ownership[1], ", cum_reg_own$lc_area[1] ", cum_reg_own$Land_Type[1], "\n")
						stop()
					}
					# check that there is enough local source area
					if (sum(source_totals[1,]) > cum_reg_own$src_cnty_lc_area[1]) {
						cat("Summed restoration source area ", sum(source_totals[1,]), " exceeds initial available source area ", cum_reg_own$src_cnty_lc_area[1], " for restoration type:\n")
						cat("\t", cum_reg_own$Region[1], ", ", cum_reg_own$Ownership[1], ", ", cum_reg_own$Land_Type[1], "\n")
						stop()
					}
					# check that there is enough total source area for scaling
					if (sum(scaled_source_totals[1,]) > cum_reg_own$src_lc_area[1]) {
						cat("Summed scaled restoration source area", sum(scaled_source_totals[1,]), " exceeds physical initial source area", cum_reg_own$lc_area[1], " for restoration type:\n")
						cat("\t", cum_reg_own$Region[1], ", ", cum_reg_own$Ownership[1], ", cum_reg_own$lc_area[1]", cum_reg_own$Land_Type[1], "\n")
						stop()
					}
				} else {
					# county level
					# check that initial area for this land cat restoration type is > 0
					z_ind = which(cum_reg_own$cnty_lc_area == 0)
					if ( length(z_ind) > 0) {
						cat("Initial restoration type county area is zero for land category and management:\n")
						cat("\t", cum_reg_own$Region[z_ind], ", ", cum_reg_own$Ownership[z_ind], ", ", cum_reg_own$Land_Type[z_ind], ",",
							cum_reg_own$Management[z_ind], "\n")
						stop("\tCannot estimate this land category restoration for this county\n")
					}
					
					# loop over source types to check for sufficent local source area
					for (src in 1:length(source_totals[ro_ind,])) {
						source_avail = cnty_lc_areas$cnty_lc_area[cnty_lc_areas$Region == unique_lc$Region[lc] & cnty_lc_areas$Ownership == unique_lc$Ownership[lc] &
															cnty_lc_areas$Land_Type == source_totals_names[src]]
						if(length(source_avail) == 0) { source_avail = 0 }
						if (source_totals[ro_ind, src] > source_avail) {
							cat("Summed restoration source area ", source_totals[ro_ind, src], " exceeds initial available area ", source_avail, " for source type:\n")
							cat("\t", unique_lc$Region[lc], ", ", unique_lc$Ownership[lc], ", ", source_totals_names[src], "\n")
							stop("\tHalted at restoration type ", unique_lc$Land_Type[lc], "\n")
						}
					} # end for loop to check source totals against available area
					
					# loop over source types to check for sufficent scaled areas
					for (src in 1:length(scaled_source_totals[ro_ind,])) {
						source_avail = lc_areas$lc_area[lc_areas$Region == unique_lc$Region[lc] & lc_areas$Ownership == unique_lc$Ownership[lc] &
															lc_areas$Land_Type == source_totals_names[src]]
						if(length(source_avail) == 0) { source_avail = 0 }
						if (scaled_source_totals[ro_ind, src] > source_avail) {
							cat("Summed scaled restoration source area ", scaled_source_totals[ro_ind, src], " exceeds initial physical area ", source_avail, " for source type:\n")
							cat("\t", unique_lc$Region[lc], ", ", unique_lc$Ownership[lc], ", ", source_totals_names[src], "\n")
							stop("\tHalted at restoration type ", unique_lc$Land_Type[lc], "\n")
						}
					} # end for loop to check source totals against available area
				} # end if project else county for checking available source area

			} # end for lc loop to check for managed areas within available area
			
			# now scale the management areas to representative regional areas
			# scale the values
			cum_reg_own$end_area = cum_reg_own$end_area / cum_reg_own$out_scalar
			# this should always be zero
			cum_reg_own$start_area = cum_reg_own$start_area / cum_reg_own$out_scalar

			# remove the src area columns for binding with the other records, but save them in another data frame just in case
			cum_reg_own_src = cum_reg_own
			cum_reg_own$src_cnty_lc_area = NULL
			cum_reg_own$src_lc_area = NULL
			
			if (!ISCOUNTY) {
				# save the info for this scenario so that scalars can be subset and adjusted
				project_lcs = rbind(project_lcs, cum_reg_own)
			}
			
		} # end if restoration practices

		###### urban practices
		# these are entered as fractions of Developed_all area
		# as such, they do not need to be scaled here because they are relative only to the urban area within the desired county(project)
		# so only need to save the ratio: Developed_all within county(project) / Developed_all within greater Region for output scaling
		# urban is always considered as All ownership for practicality
		# Dead_removal and Urban_forest are fractions of Developed_all during the specified year
		# Urban_forest fractions are based on a desired fraction by the end of a target year
		# Growth is fraction of initial Developed_all annual change during the specified year, based on a desired rate change during a target year
		# Region has to be all All or each region has to be defined
		# county and project are processed the same except for the available area because all practices always have to be defined
		
		# this is always true because urban management always has to be defined
		if(nrow(urban) > 0) {
			# the regional urban areas and scaling
			# separate any "All" region entries so that the appropriate regions can be specified
			# ownership should all be All
			# these records will be expanded to regional urban area for All ownership
			
			# first make sure that Ownership is All everywhere
			urb_all_own = urban[urban$Ownership == "All",]
			urb_own = urban[urban$Ownership != "All",]
			if (nrow(urb_own) > 0 | nrow(urb_all_own) != nrow(urban)) {
				stop("Developed_all Ownership must be 'All' for all urban records\n")
			}
			# next check that Region is either All or each region is defined
			urb_all_reg = urban[urban$Region == "All",]
			urb_reg = urban[urban$Region != "All",]
			if (nrow(urb_all_reg) > 0 & nrow(urb_reg) > 0) {
				stop("For Developed_all: Either all records must have 'All' for Region, OR each region needs to be defined for each practice\n")
			}
			# check that each region is defined
			if (nrow(urb_reg) > 0) {
				for (r in 1:reg_names) {
					r_ind = which(unique(urb_reg$Region) == reg_names[r])
					if(length(r_ind) == 0) {
						cat("Region ", reg_names[r], " is not defined for Developed_all\n")
						stop("For Developed_all: Each region needs to be defined for each practice\nAlternatively Region can be 'All' for all urban records\n")
					}
				}
			}

			# now separate into the two reg-own cases
			urb_all = urban[urban$Region == "All" & urban$Ownership == "All",]
			urb_all_own = urban[urban$Region != "All" & urban$Ownership == "All",]
			
			# either distrubute to the regions, or no distribution is necessary
			if (nrow(urb_all) > 0) {	
				# distribute records across regions if necessary
				# expand the records and adjust management areas
				# then strip the added columns to bind them together
				# then merge the lc area for scalar calc 

				urb_all_own = NULL
				num_recs = nrow(urb_all)
				for (r in 1:num_recs) {
					temp_df = urb_all[r,]
					temp_df = merge(temp_df, 
						unique(reg_lt_areas[,c("Region", "Land_Type", "reg_lt_area")]),
						by = c("Land_Type"), all.x = TRUE)
					temp_df$Region.x = temp_df$Region.y
					temp_df$Region.y = NULL
					colnames(temp_df)[colnames(temp_df) == "Region.x"] = "Region"
					temp_df[,c(ncol(temp_df))] = NULL
					urb_all_own = rbind(urb_all_own, temp_df)
				}

			} # end if all-all case
				
			# now all regions should be specified in each case
			# add county col
			# add county/project area and region area
			# use available area only for the relevant regions
			urb_all_own$County = county
			urb_all_own = merge(urb_all_own, 
						unique(reg_lt_areas[,c("Region", "Land_Type", "reg_lt_area")]),
						by = c("Region", "Land_Type"), all.x = TRUE)
			colnames(urb_all_own)[colnames(urb_all_own) == "reg_lt_area"] = "cnty_reg_lt_area"
			urb_all_own$reg_lt_area = urb_all_own$cnty_reg_lt_area
						
			# Replace appropriate values
			if(!ISCOUNTY) {
				urb_all_own$cnty_reg_lt_area[urb_all_own$Region == county] = avail_area
			} else {
				urb_all_own = merge(urb_all_own, 
						unique(cnty_reg_lt_areas[,c("Region", "Land_Type", "cnty_reg_lt_area")]),
						by = c("Region", "Land_Type"), all.x = TRUE)
				for (cr in 1:num_county_regions) {
					urb_all_own$cnty_reg_lt_area.x[urb_all_own$Region == county_regions[cr]] =
						urb_all_own$cnty_reg_lt_area.y[urb_all_own$Region == county_regions[cr]]
				}
				colnames(urb_all_own)[colnames(urb_all_own) == "cnty_reg_lt_area.x"] = "cnty_reg_lt_area"
				urb_all_own$cnty_reg_lt_area.y = NULL
			} # end if project-level or county
			
			# now calculate the output scaling ratio - this isn't necessary any more
			urb_all_own$out_scalar = urb_all_own$cnty_reg_lt_area / urb_all_own$reg_lt_area
			# change the names to match the other area columns for binding so that the values are still saved even though they are no lc areas
			colnames(urb_all_own)[colnames(urb_all_own) == "cnty_reg_lt_area"] = "cnty_lc_area"
			colnames(urb_all_own)[colnames(urb_all_own) == "reg_lt_area"] = "lc_area"

			if (!ISCOUNTY) {
				# save the info for this scenario so that scalars can be subset and adjusted
				# add the source columns for binding with other scenarios that may have restoration
				annual_reg_own$src_cnty_lc_area = NA
				annual_reg_own$src_lc_area = NA
				project_lcs = rbind(project_lcs, urb_all_own)
			}

		} # end if urban records
	
		# bind the records together into one table for output
		# order the columns and rows for binding
		if (nrow(annual) > 0) {
			annual_reg_own = annual_reg_own[,col_order_scalar]
			annual_reg_own = annual_reg_own[order(annual_reg_own$Region, annual_reg_own$Land_Type, annual_reg_own$Ownership, annual_reg_own$Management),]
			if (nrow(cumulative) > 0) {
				cum_reg_own = cum_reg_own[, col_order_scalar]
				cum_reg_own = cum_reg_own[order(cum_reg_own$Region, cum_reg_own$Land_Type, cum_reg_own$Ownership, cum_reg_own$Management),]
				man_table_out = rbind(annual_reg_own, cum_reg_own)
				if (nrow(urban) > 0) {
					urb_all_own = urb_all_own[, col_order_scalar]
					urb_all_own = urb_all_own[order(urb_all_own$Region, urb_all_own$Land_Type, urb_all_own$Ownership, urb_all_own$Management),]
					man_table_out = rbind(man_table_out, urb_all_own)
				}
			} else if (nrow(urban) > 0) {
				man_table_out = rbind(annual_reg_own, urb_all_own)
			}
		} else {
			if (nrow(cumulative) > 0) {
				cum_reg_own = cum_reg_own[,col_order_scalar]
				cum_reg_own = cum_reg_own[order(cum_reg_own$Region, cum_reg_own$Land_Type, cum_reg_own$Ownership, cum_reg_own$Management),]
				man_table_out = rbind(cum_reg_own, urb_all_own)
			} else if (nrow(urban) > 0) {
				urb_all_own = urb_all_own[,col_order_scalar]
				urb_all_own = urb_all_own[order(urb_all_own$Region, urb_all_own$Land_Type, urb_all_own$Ownership, urb_all_own$Management),]
				man_table_out = urb_all_own
			}
		} # end binding the three tables together

		# strip the extra columns for the scenario output to the new raw file
		scen_table_out = man_table_out
		scen_table_out$County = NULL
		scen_table_out$cnty_lc_area = NULL
		scen_table_out$lc_area = NULL
		scen_table_out$out_scalar = NULL
		
		# put the new scenario raw table into the raw output list
		out_scen_df_list[[s]] = scen_table_out
		
		# put the manage table with scalars into a new output list
		out_man_df_list[[s]] = man_table_out

	} # end s loop over scenario sheets

		# subset the scalars for project level based on the scenarios
		# this set of scenarios can operate on only one land category
		# adjust scalars for source area for restoration
		# always subset the Developed_all based on the avail_area input in case there is an urban project
		if (!ISCOUNTY) {
			
			project_nonurb_lcs = project_lcs[project_lcs$Ownership != "Developed_all",]
			project_nonurb_lcs_names = unique(project_nonurb_lcs[,c(1:3)])
			# only one land cat can be used throughout these scenarios
			if (nrow(project_nonurb_lcs_names) > 1) {
				stop("Only one land category for annual management and restoration can be specified across these scenarios for project-level estimation\n")
			}
			
			source_lts = NA
			proj_own = NA
			proj_lt = NA				
			if (nrow(project_nonurb_lcs) > 0) {
			
				project_rest = project_nonurb_lcs[project_nonurb_lcs$Management == "Restoration" |
								project_nonurb_lcs$Management == "Reforestation" | project_nonurb_lcs$Management == "Afforestation",]

				if (nrow(project_rest) > 0) {
					# need to keep source land cats also
					# the number and type of records are already controlled above
					# determine restoration type index
					# Forest has two types of restoration
					rt_ind = which(restoration_lt == project_nonurb_lcs$Land_Type[1])
					if (length(rt_ind) == 2) {
						if (project_nonurb_lcs$Management[1] == "Afforestation") { rt_ind = 5
						} else { rt_ind = 6 }
					}
					source_lts = restoration_sources[rt_ind,]
				} # end if restoration record
				
				# get the ownership and land type
				proj_own_name = project_nonurb_lcs_names$Ownership[1]
				proj_lt_name = project_nonurb_lcs_names$Land_Type[1]
				
			} # end if nonurb records

			# first add avail_area and subset the project region scalars
			project_region = output_scalars[output_scalars$Region == county,]
			project_region$avail_area = avail_area
			# subset for urban
			project_urban = project_region[project_region$Land_Type == "Developed_all",]
			# now subset for ownership and land types of non-urban

			if (!is.na(proj_own_name)) {
				project_own = project_region[project_region$Land_Type != "Developed_all" & project_region$Ownership == proj_own_name,]
				project_lt = project_own[project_own$Land_Type == proj_lt_name,]
				if (!is.na(source_lts)) {
					den_sum = 0
					for (l in 1:length(source_lts)) {
						project_lt = rbind(project_lt, project_own[project_own$Land_Type == source_lts[l],])
						# adjust avail_area proportionally to initial reg-own sources
						den_sum = den_sum + project_own$lc_area[project_own$Land_Type == source_lts[l]]
					}
					for (l in 1:length(source_lts)) {
						project_lt$avail_area[project_lt$Land_Type == source_lts[l]] =  project_lt$avail_area[project_lt$Land_Type == source_lts[l]] *
							project_lt$lc_area[project_lt$Land_Type == source_lts[l]] / den_sum
					}
				}
			} # end if annual manage or restoration for subsetting
			
			# now put these together and calc the scalars based on avail_area
			# there will always be urban records
			if (nrow(project_lt) > 0) {
				output_scalars = rbind(project_lt, project_urban)
			} else {
				output_scalars = project_urban
			}

			output_scalars$cnty_lc_area = output_scalars$avail_area
			output_scalars$output_scalar = output_scalars$cnty_lc_area / output_scalars$lc_area
			output_scalars$avail_area = NULL

		} # end if project-level for calculating scalars

		# write the new raw scenario file
		out_wrkbk =  loadWorkbook(out_file, create = TRUE)
		createSheet(out_wrkbk, name = out_scen_sheets)
		clearSheet(out_wrkbk, sheet = out_scen_sheets)
		writeWorksheet(out_wrkbk, data = scen_head_df_list, sheet = out_scen_sheets, startRow = 1, header = FALSE)
		writeWorksheet(out_wrkbk, data = out_scen_df_list, sheet = out_scen_sheets, startRow = start_row, header = TRUE)	
		# shut off wrap text
		cs <- createCellStyle(out_wrkbk)
		setWrapText(cs, wrap = FALSE)
		for (i in 1:length(out_scen_sheets)) {
			rc = expand.grid(row = 1:(nrow(out_scen_df_list[[i]])+start_row), col = 1:ncol(out_scen_df_list[[i]]))
			setCellStyle(out_wrkbk, sheet = out_scen_sheets[i], row = rc$row, col = rc$col, cellstyle = cs)
		}
		# write the workbook
		saveWorkbook(out_wrkbk)
		
		# write the scalar file
		write.csv(output_scalars, file = out_scalar_file, row.names = FALSE)

	cat("Finish write_scaled_raw_scenario at", date(), "\n")	
	
}