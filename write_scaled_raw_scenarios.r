# write_scaled_raw_scenarios.r

# Copyright (c) 2016-2019, Alan Di Vittorio and Maegen Simmonds

# This software and its associated input data are licensed under the 3-Clause BSD open source license
# Please see license.txt for details

# Converts an area specific (e.g., county) raw scenario file into a scaled version for generating CALAND input files
# Also saves the conversion info for use by scale_caland_outputs.r, which converts CALAND outputs to the area-specific values

# This is designed to generate files for estimating county level effects of management
#   So only the management areas and the county name are needed

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

# 3. 'units': the area units for the input scenario file; default is 'ac'; could also be 'ha'

# 4. 'county_category_areas_file': the file containing the breakdown of county areas for the land categories;
#    default is area_lab_sp9_own9_2010lt15_cnty_sqm_stats.csv and this should not be changed


############### Outputs ##################

# Scaled raw scenario file for input to write_caland_inputs()
#   The county name and the units will be appended to the input file name

# A file containing expected CALAND output scalars
#  this is an excel file with one sheet for each input scenario sheet
#  these sheets will be matched to the output files for scaling
#  each sheet contains scalars only for the county land categories
#  there is a scalar, an expected county area, and an expected region area for each year for each land cat
#  "_scalars" will be appended to the name of the scaled scenario file name to create this file name

################# Limitations ###################

# No baseline LULCC can be happening - need to set this using write_caland_inputs()
#	this is because the land category distributions are different between region and county,
#		which means that the land changes cannot be scaled properly
#	the scaling must be able to estimate the changes in area, otherwise the densities will not be valid

# No wildfire can be happening if restoration is happening - need to set this using write_caland_inputs()
#	this is because it changes the carbon densities based on the regional fire area distribution, which cannot be mapped to the county

# annual activities and restoration activities cannot be prescribed in the same scenario; they must be in separate scenarios
#	this is because carbon densities cannot be changing independently of area changes

# Land protection cannot be estimated because no baseline LULCC can be used

# No non-regeneration because this causes LULCC that cannot be mapped to the county
#	this is set when CALAND is run

# If interactions between restoration practices cause the prescriptions to not be met, the carbon densities may be off
#   This just throws a warnin if this may be the case, but lets it run
#   This is because I can't guarantee that the estimated areas here are exactly the same as in CALAND (even though they should be)

###################### Notes ######################

# Each scenario must at least have the three urban management practices defined in order to run, with all regions and ownerships defined (can use "All"):
#   Developed_all Dead_removal: must be defined as 1
#	Developed_all Growth: must be defined as 1
#	Developed_all Urban_forest: baseline is defined as the statewide value of 0.15, but there are also reigonal values

# The working directory needs to be the main CALAND folder where this file resides

# The output scenarios file name will be the same as the intput file name but with county name and the units ('ac' or 'ha') appended to it
# The output scalars file name will additionally have "scalars" appended to it

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

write_scaled_raw_scenarios <- function(scen_file = "amador_example_ac.xls", county = "Amador", units = "ac",
										county_category_areas_file = "area_lab_sp9_own9_2010lt15_cnty_sqm_stats.csv") {
	
### these have been pulled from the arguments because the writing of output scalars is incorrect and has not been fixed for non-county applications
ISCOUNTY = TRUE
avail_area = NA

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
	
	cat("Start write_scaled_raw_scenarios at", date(), "\n")

	in_dir = "./raw_data/"
	out_dir = in_dir
	xltag = ".xls"
	csvtag = ".csv"
	
	# remove all whitespace from the county input string for use in output files names
	# this works on a vector as well
	ctag = gsub("//s", "", county)
	out_file = paste0(out_dir, substr(scen_file, 1, regexpr(".xls", scen_file)-1), "_", ctag, "_", units, xltag)
	out_scalar_file = paste0(out_dir, substr(scen_file, 1, regexpr(".xls", scen_file)-1), "_", ctag, "_", units, "_scalars", xltag)

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
	restoration_sources = array(dim=c(num_restoration_lt, num_restoration_lt))
	num_restoration_sources = c(4, 1, 1, 2, 2, 1)
	restoration_sources[,] = NA
	restoration_sources[1,1:num_restoration_sources[1]] = c("Shrubland", "Grassland", "Savanna", "Woodland")
	restoration_sources[2,1:num_restoration_sources[2]] = c("Cultivated")
	restoration_sources[3,1:num_restoration_sources[3]] = c("Cultivated")
	restoration_sources[4,1:num_restoration_sources[4]] = c("Grassland", "Cultivated")
	restoration_sources[5,1:num_restoration_sources[5]] = c("Shrubland", "Grassland")
	restoration_sources[6,1:num_restoration_sources[6]] = c("Shrubland")
	meadow_rest_index = 1

	source_totals_names = c("Shrubland", "Grassland", "Savanna", "Woodland", "Cultivated")
	woodland_st_index = 4
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
	col_order_scalar = c(col_order, "County", "cnty_lc_area", "lc_area", "man_scalar")

	# first copy the input worksheets to the outputs to set up the lists
	out_scen_df_list = scenin_df_list
	out_scen_sheets = scenin_sheets
	# this one is for the table with the scalar values
	out_man_df_list = scenin_df_list
	# this is the output scalar file
	out_scalar_df_list = scenin_df_list

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

	# store the project-level land cats here
	project_lcs = NULL

	# loop over the scenario sheets
	for (s in 1: num_scenin_sheets) {

		# need to start with a fresh output scalar df for each scenario
		# the scalar values for the output files are needed for all land cats in the county
		# for project-level only the affected land cats are scaled and used - subset and adjust this at the end as necessary
		output_scalars = merge(cnty_lc_areas, lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
		output_scalars$initial_scalar = output_scalars$cnty_lc_area / output_scalars$lc_area

		# Split annual, restoration, and urban records here to check that project-level operates on only annual or restoration
		#  not sure how to check for urban-only for project-level

		annual = out_scen_df_list[[s]][out_scen_df_list[[s]]$Management!="Restoration" & out_scen_df_list[[s]]$Management!="Afforestation" &
			out_scen_df_list[[s]]$Management!="Reforestation" & out_scen_df_list[[s]]$Land_Type!="Developed_all",]
		
		cumulative = out_scen_df_list[[s]][out_scen_df_list[[s]]$Management=="Restoration" | out_scen_df_list[[s]]$Management=="Afforestation" |
			out_scen_df_list[[s]]$Management=="Reforestation",]
		
		urban = out_scen_df_list[[s]][out_scen_df_list[[s]]$Land_Type == "Developed_all",]

		# first check to make sure that annual and restoration are not happening at the same time
		if (nrow(annual) > 0 & nrow(cumulative) > 0) {
			stop("Specify only annual practices or only restoration practices in a single scenario\n")
		}

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
				# need to make sure that all management land cats are in the county
				bad_inds = which(is.na(annual_reg_own$County))
				if (length(bad_inds) > 0) {
					for (b in 1:length(bad_inds)) {
						cat("County land category does not exist for management: ", annual_reg_own$Region[bad_inds[b]], ",", annual_reg_own$Land_Type[bad_inds[b]], ",", annual_reg_own$Ownership[bad_inds[b]], "\n")
						stop()
					}
				}
				
			} # end else county
		
			# now check to make sure that max annual managed areas do not exceed initial available areas
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
			# calculate the management scaling ratio region:county for upscaling
			# first get the region level land cat area
			annual_reg_own = merge(annual_reg_own, lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
			annual_reg_own$man_scalar = annual_reg_own$lc_area / annual_reg_own$cnty_lc_area
			# scale the values
			annual_reg_own$start_area = annual_reg_own$start_area * annual_reg_own$man_scalar
			annual_reg_own$end_area = annual_reg_own$end_area * annual_reg_own$man_scalar
			
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
		# these require area scaling based on the expected net restored land type area to ensure proper carbon density calculations:
		#  base scaling factor = expected area of land category in region / expected area of land category in county
		#  the restored area constitutes a carbon density change, but the converted area maintains its carbon density
		#    (because there are not other drivers of density change based on the stated limitations)
		#  this expected area is used because the source distributions are different between county and region
		#  the inverse of this base scaling factor should be sufficient for output scaling, but is needed for each year
		#  this base scaling factor needs to be adjusted for management area scaling to make sure that:
		#	the ratio of region change:total is the same as the ratio of county change:total
		#		it is important to get this right for both restoration types and sources as it determines the density values
		# 		  main challenge is to figure out the woodland and meadow scaling because woodland is a source of meadow
		# caounty target cum area is checked against available county source area and an error is thrown if not enough available area
		#  this is based on the county land cat distribution
		# also, the scaled source area needs to be checked to ensure scalability; an error is thrown if there is not enough
		#  this is based on the region land cat distribution
		#  this means that in some cases the target restored area for the county may not be able to be simulated
		#  this can occur if a considerable proprtion of the source area is in the county compared to the region as a whole
		# counties are distributed proportionally among relevant regions if 'All' is entered in Region column
		# for projects, the sources are for only a single, specific land catergory (Region, own, land type), and the avail area is checked and adjusted
		
		if(nrow(cumulative) > 0) {
		
			# Both Reforestation and Afforestion cannot be done at the same time
			#  technically, this is just within a particular land category, but throw error regardless of this
			#  this isn't caught in write_caland_inputs(), but it can crash CALAND
			aff_df = cumulative[cumulative$Management == "Afforestation",]
			ref_df = cumulative[cumulative$Management == "Reforestation",]
			if (nrow(aff_df) > 0 & nrow(ref_df) > 0) {
				cat("Afforestation and Reforestation cannot be prescribed in the same scenario!\n")
				stop("It is recommended to use Reforestation for all forest area expansion activities (which converts Shrubland), unless you are sure you want to afforest Grassland and Shrubland.\n")
			}
		
			# make sure start area is zero
			if (sum(cumulative$start_area) > 0) {
				stop("Start area for all restoration practices must be zero because this is a cumulative definition\n")
			}
		
			# make sure that any delta fresh marsh records are fully specified
			# and add them to output_scalars if so
			fm_df = unique(cumulative[cumulative$Land_Type == "Fresh_marsh", c("Region", "Land_Type", "Ownership")])
			if ( nrow(fm_df) > 0 ) {
				for (r in 1:nrow(fm_df)) {
					if ( !(fm_df$Region[r] != "All" & fm_df$Ownership[r] != "All") ) {
						stop("Delta fresh marsh restoration must be fully specified by region and ownership\n")
					} else {
						# if delta fresh marsh is included it needs to be added to output_scalars
						new_row = output_scalars[1,]
						new_row$Region = fm_df$Region[r]
						new_row$Land_Type = "Fresh_marsh"
						new_row$Ownership = fm_df$Ownership[r]
						new_row$County = county
						new_row$cnty_lc_area = 0
						new_row$lc_area = 0
						new_row$initial_scalar = 1
						output_scalars = rbind(output_scalars, new_row)
					}
				}
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

				# check that this land cat exists
				# if delta fresh marsh it is ok if associated cultivated exists
				cum_lc_area = lc_areas$lc_area[lc_areas$Region == cumulative$Region[1] & lc_areas$Ownership == cumulative$Ownership[1] & lc_areas$Land_Type == cumulative$Land_Type[1]]
				cult_lc_area = lc_areas$lc_area[lc_areas$Region == "Delta" & lc_areas$Ownership == cumulative$Ownership[1] & lc_areas$Land_Type == "Cultivated"]
				if (  !(length(cum_lc_area) > 0) ) {
					if ( !(cumulative$Region[1] == "Delta" & cumulative$Land_Type[1] == "Fresh _marsh" & length(cult_lc_area) > 0) ) {
						cat("Restoration type does not exist, so it cannot be restored:\n")
						cat("\t", cumulative$Region[1], ", ", cumulative$Ownership[1], ", ", cumulative$Land_Type[1], ",",
							cumulative$Management[1], "\n")
						stop("\tCannot estimate this land category restoration\n")
					}
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
					for (r in 1:nrow(cum_reg_own_orig)) {
						cr_ind = which(county_regions == cum_reg_own_orig$Region[r])
						if (length(cr_ind) == 0) {
							stop("Record region ", cum_reg_own_orig$Region[r], " is not in county ", county, " region list: ", county_regions, "\n")
						}
					}
					cum_reg_own = rbind(cum_reg_own, cum_reg_own_orig)
				}
				
				# now all records should be fully expanded to land category
				# add the county and county lc area cols
				cum_reg_own = merge(cum_reg_own, cnty_lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
				# need to make sure that all management land cats are in the county
				# except for delta fresh marsh is ok here; source availability is checked below
				bad_inds = which(is.na(cum_reg_own$County))
				if (length(bad_inds) > 0) {
					for (b in 1:length(bad_inds)) {
						if ( cum_reg_own$Region[bad_inds[b]] == "Delta" & cum_reg_own$Land_Type[bad_inds[b]] == "Fresh_marsh" ) {
							cum_reg_own$County[bad_inds[b]] = county
							cum_reg_own$cnty_lc_area[bad_inds[b]] = 0
						} else {
							cat("County land category does not exist for management: ", cum_reg_own$Region[bad_inds[b]], ",", cum_reg_own$Land_Type[bad_inds[b]], ",", cum_reg_own$Ownership[bad_inds[b]], "\n")
							stop()
						}
					}
				}

			} # end else county
		
			# first get the region level land cat area
			cum_reg_own = merge(cum_reg_own, lc_areas, by = c("Region", "Land_Type", "Ownership"), all.x = TRUE)
			
			# this is the managment scalar for upscaling region:county
			# calculate the management scaling ratio
			# if restoration type is delta fresh marsh need to set the lc area to zero and the management scalar to 1
			cum_reg_own$man_scalar = cum_reg_own$lc_area / cum_reg_own$cnty_lc_area
			cum_reg_own$lc_area[cum_reg_own$Region == "Delta" & cum_reg_own$Land_Type == "Fresh_marsh"] = 0
			cum_reg_own$man_scalar[cum_reg_own$Region == "Delta" & cum_reg_own$Land_Type == "Fresh_marsh"] = 1
			
			# add the source cnty_lc_area and lc_area cols as NA because they will be filled below with total source area
			# the county level source area is based on county land cat distribution
			# the region level source area is based on region land cat distribution
			cum_reg_own$src_cnty_lc_area = NA
			cum_reg_own$src_lc_area = NA
		
			# now check to make sure that total prescribed managed areas do not exceed initial available areas
			# just check the end managed area against the initial available area of the source categories
			#  this won't capture limitations due to LULCC - there shouldn't be baseline LULCC!!!!
			#  but multiple restoration activities may compete for source area
			# need to sum like source category needs first, by region-ownership, using the county distributions
			# sum the scaled like source category needs, using the regional distributions
			# also reset the src_cnty_lc_area to the resepctive source area totals
			# also sum up the region-level land cat source areas to check for availability

			unique_lc = unique(cum_reg_own[,c(1:3)])
			num_lc = nrow(unique_lc)
			unique_ro = unique(unique_lc[,c(1,3)])
			num_ro = nrow(unique_ro)
			source_totals = array(dim=c(num_ro, num_sources))
			scaled_source_totals = array(dim=c(num_ro, num_sources))
			# store the total source areas needed for restoration, by reg-own and source land type, for county and scaled
			# the county areas are base on the county distribution
			# the scaled areas are based on the reg-own distribution
			source_totals[,] = 0
			scaled_source_totals[,] = 0
			# store the total prescribed restoration area for each restoration land cat
			rest_totals = array(dim=num_lc)
			rest_totals[] = 0
			# store the reg-own land category areas of sources by restoration land cat
			reg_own_source_lc_areas = array(dim=num_lc)
			reg_own_source_lc_areas[] = 0
			
			
			for (lc in 1:num_lc) {
				# extract the records for this land cat
				check_df = cum_reg_own[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc],]
				
				# determine the region-ownership index for source totals
				ro_reg_ind = which(unique_ro$Region == unique_lc$Region[lc])
				ro_own_ind = which(unique_ro$Ownership == unique_lc$Ownership[lc])
				ro_ind = intersect(ro_reg_ind, ro_own_ind)
				# determine restoration type index and the management type
				# Forest has two types of restoration, so two rt_ind values will be returned
				#  parse this and deal with the different practice types
				#  afforestation and reforestation are not allowed in the same scenario - this is checked above
				rt_ind = which(restoration_lt == unique_lc$Land_Type[lc])
				if (length(rt_ind) == 2) {
					if (check_df$Management[1] == "Afforestation") {
						rt_ind = 5
						practice = "Afforestation"
					} else {
						rt_ind = 6
						practice = "Reforestation"
					}
				} else if (length(rt_ind) == 1) {
					practice = "Restoration"
				} else if (length(rt_ind) == 0) {
					stop("Incorrect restoration land type ", unique_lc$Land_Type[lc], "\n")
				} # end if-else restoration type
				
				# sum the restoration area
				rest_totals[lc] = sum(check_df$end_area)
				# sum the source areas
				st_den = 0 # this is to store the county-level total source area for comparison below
				st_sum[] = 0
				# sum the scaled source areas
				st_den2 = 0 # this is to store the region-level total source area for comparison below
				st_sum_scaled[] = 0
				for (src in 1:num_restoration_sources[rt_ind]) {
					st_ind = which(source_totals_names == restoration_sources[rt_ind,src])
					st_num = cnty_lc_areas$cnty_lc_area[cnty_lc_areas$Region == unique_lc$Region[lc] & cnty_lc_areas$Ownership == unique_lc$Ownership[lc] &
														cnty_lc_areas$Land_Type == source_totals_names[st_ind]]
					if(length(st_num) == 0) { st_num = 0 }
					st_den = st_den + st_num
					st_sum[st_ind] = st_sum[st_ind] + rest_totals[lc] * st_num
					# sum up the region-level total source area
					temp = lc_areas$lc_area[lc_areas$Region == unique_lc$Region[lc] & lc_areas$Ownership == unique_lc$Ownership[lc] &
														lc_areas$Land_Type == source_totals_names[st_ind]]
					if(length(temp) == 0) { temp = 0 }
					# calculate the scaled source needs
					st_den2 = st_den2 + temp
					st_sum_scaled[st_ind] = st_sum_scaled[st_ind] + rest_totals[lc] * check_df$man_scalar[1] * temp
				} # end for summing up individual sources for non-forest restoration
				# keep track of sources in this region-ownership
				source_totals[ro_ind,] = source_totals[ro_ind,] + st_sum / st_den
				scaled_source_totals[ro_ind,] = scaled_source_totals[ro_ind,] + st_sum_scaled / st_den2
				# store the reg-own land category area of source for this restoration land cat
				reg_own_source_lc_areas[lc] = st_den2
				# set to zero if no source area
				source_totals[ro_ind,] <- replace(source_totals[ro_ind,], is.nan(source_totals[ro_ind,]), 0.0)
				scaled_source_totals[ro_ind,] <- replace(scaled_source_totals[ro_ind,], is.nan(scaled_source_totals[ro_ind,]), 0.0)
				cum_reg_own$src_cnty_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
										cum_reg_own$Land_Type == unique_lc$Land_Type[lc]] = st_den
				cum_reg_own$src_lc_area[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
										cum_reg_own$Land_Type == unique_lc$Land_Type[lc]] = st_den2

				# loop over the source types and check against the actual county level source availability
				# for project-level the check is the total source against the total avail area input
				if (!ISCOUNTY) { 
					# check available initial area against physical initial area
					if (cum_reg_own$cnty_lc_area[1] > cum_reg_own$lc_area[1]) {
						cat("Input initial restoration type area ", cum_reg_own$cnty_lc_area[1], " exceeds physical regional initial restoration type area ",
							cum_reg_own$lc_area[1], " for restoration type:\n")
						cat("\t", cum_reg_own$Region[1], ", ", cum_reg_own$Ownership[1], ", cum_reg_own$lc_area[1] ", cum_reg_own$Land_Type[1], "\n")
						stop()
					}
					# check that there is enough local source area
					if (sum(source_totals[1,]) > cum_reg_own$src_cnty_lc_area[1]) {
						cat("Summed restoration source area ", sum(source_totals[1,]), " exceeds initial available source area ", cum_reg_own$src_cnty_lc_area[1],
							" for restoration type:\n")
						cat("\t", cum_reg_own$Region[1], ", ", cum_reg_own$Ownership[1], ", ", cum_reg_own$Land_Type[1], "\n")
						stop()
					}
					# check that there is enough total source area for scaling
					if (sum(scaled_source_totals[1,]) > cum_reg_own$src_lc_area[1]) {
						cat("Summed scaled restoration source area", sum(scaled_source_totals[1,]), " exceeds physical regional initial source area",
							cum_reg_own$lc_area[1], " for restoration type:\n")
						cat("\t", cum_reg_own$Region[1], ", ", cum_reg_own$Ownership[1], ", cum_reg_own$lc_area[1]", cum_reg_own$Land_Type[1], "\n")
						stop()
					}
				} else {
					# county level
					# check that initial area for any land cat restoration type is > 0
					# if delta fresh marsh it is ok for it to be zero (as set above), as the scaling is just equal to 1 because it doesn't exist anywhere
					z_ind = which(cum_reg_own$cnty_lc_area == 0)
					if ( length(z_ind) > 0 ) {
						for ( z in 1:z_ind) {
							z_df = cum_reg_own[z_ind[z],]
							fm_df = z_df[z_df$Region == "Delta" & z_df$Land_Type == "Fresh_marsh",]
							if ( !(nrow(fm_df) > 0) ) {
								cat("Initial restoration type county area is zero for land category and management:\n")
								cat("\t", cum_reg_own$Region[z_ind[z]], ", ", cum_reg_own$Ownership[z_ind[z]], ", ", cum_reg_own$Land_Type[z_ind[z]], ",",
									cum_reg_own$Management[z_ind[z]], "\n")
								stop("\tCannot estimate this land category restoration for this county\n")
							}
						}
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
			
			# calculate modified management scalars for woodland and meadow restoration if necessary
			# this happens only if both woodland and meadow restoration exist in the same reg-own
			# this is to maintain carbon density fidelity between county and region by:
			#  ensuring that the ratio of scaled restoration area to initial reg-own area is the same as the corresponding county-level ratio
			wl_inds = which(unique_lc$Land_Type == "Woodland")
			md_inds = which(unique_lc$Land_Type == "Meadow")
			if (length(wl_inds) > 0 & length(md_inds) > 0) {
				# check for matching region
				reg_matches = intersect(unique_lc$Region[wl_inds], unique_lc$Region[md_inds])
				if (length(reg_matches) > 0) {
					# check for matching ownership by region
					for ( r in 1:length(reg_matches) ) {
						reg_own_matches = intersect(unique_lc$Ownership[unique_lc[wl_inds, "Region"] == reg_matches[r]],
													unique_lc$Ownership[unique_lc[md_inds, "Region"] == reg_matches[r]])
						if (length(reg_own_matches) > 0) {
							for ( o in 1:length(reg_own_matches) ) {
								# subset this reg-own for woodland and meadow
								# there may be multiple rows for each if different time periods are prescribed
								ro_df = cum_reg_own[cum_reg_own$Region == reg_matches[r] & cum_reg_own$Ownership == reg_own_matches[o] &
													(cum_reg_own$Land_Type == "Woodland" | cum_reg_own$Land_Type == "Meadow"),]
								# calculate and a management adjustment scalar and apply it to man_scalar
								# this is done per region-ownership
								# this should be true at this point, but check anyway
								if (nrow(ro_df) > 0) {
									
									# determine the region-ownership index for source totals
									ro_reg_ind = which(unique_ro$Region == reg_matches[r])
									ro_own_ind = which(unique_ro$Ownership == reg_own_matches[o])
									ro_ind = intersect(ro_reg_ind, ro_own_ind)
									# determine the lc indices for restoration totals
									lc_reg_ind = which(unique_lc$Region == reg_matches[r])
									lc_own_ind = which(unique_lc$Ownership == reg_own_matches[o])
									lc_wl_ind = intersect(lc_reg_ind, lc_own_ind)
									lc_md_ind = lc_wl_ind
									lc_lt_ind = which(unique_lc$Land_Type == "Woodland")
									lc_wl_ind = intersect(lc_wl_ind, lc_lt_ind)
									lc_lt_ind = which(unique_lc$Land_Type == "Meadow")
									lc_md_ind = intersect(lc_md_ind, lc_lt_ind)
									
									# first calculate the net county woodland change in this reg-own: Nc = Cw - Cws,
									#  where Cw is total prescribed county woodland restoration and Cws is total county woodland source for meadow, in this reg-own
									Nc = rest_totals[lc_wl_ind] - source_totals[ro_ind, woodland_st_index]
									
									# calc desired net region change: Nr = man_scalar * Nc,
									#  where man scalar is Rwa/Cwa, initial region-own woodland area / initial county woodland area, in this reg-own
									Nr = ro_df$man_scalar[ro_df$Land_Type == "Woodland"][1] * Nc
									
									# calc desired region-own woodland restoration: Wr = Nr + Ws, where Ws is the region-own woodland source for meadow
									Wr = Nr + scaled_source_totals[ro_ind, woodland_st_index]
									
									# if Wr >= 0 proceed in decreasing scaled woodland restoration
									if ( Wr >= 0) {
										# calc adjustment to scaled restoration: adj_scalar = Wr / (Cw * man_scalar),
										#  where the denominator is the original scaled regional prescription
										adj_scalar = Wr / (rest_totals[lc_wl_ind] * ro_df$man_scalar[ro_df$Land_Type == "Woodland"][1])
										
										# then new values for these reg-own woodland rows are just woodland restoration for each row * man_scalar * adj_scalar
										# so adjust the man_scalar here, as areas are multiplied below
										cum_reg_own$man_scalar[cum_reg_own$Region == reg_matches[r] & cum_reg_own$Ownership == reg_own_matches[o] &
																cum_reg_own$Land_Type == "Woodland"] =
											adj_scalar * cum_reg_own$man_scalar[cum_reg_own$Region == reg_matches[r] & cum_reg_own$Ownership == reg_own_matches[o] &
																cum_reg_own$Land_Type == "Woodland"]
									} else {
										# desired region-own woodland restoration Wr < 0 so:
										# increase meadow restoration instead and zero out scaled woodland restoration
										cum_reg_own$man_scalar[cum_reg_own$Region == reg_matches[r] & cum_reg_own$Ownership == reg_own_matches[o] &
																cum_reg_own$Land_Type == "Woodland"] = 0
										
										# calc desired region-own meadow restoration: Mr = abs(Wr) * region-own meadow source lc area / reg-own woodland area
										Mr = -Wr * reg_own_source_lc_areas[lc_md_ind] / ro_df$lc_area[ro_df$Land_Type == "Woodland"][1]
										
										# calc adjustment to scaled restoration: adj_scalar = Mr / (Cm * man_scalar)
										adj_scalar = Mr / (rest_totals[lc_md_ind] * ro_df$man_scalar[ro_df$Land_Type == "Meadow"][1])
										
										# then new values for these reg-own meadow rows are just meadow restoration for each row * man_scalar * adj_scalar
										# so adjust the man_scalar here, as areas are multiplied below
										cum_reg_own$man_scalar[cum_reg_own$Region == reg_matches[r] & cum_reg_own$Ownership == reg_own_matches[o] &
																cum_reg_own$Land_Type == "Meadow"] =
											adj_scalar * cum_reg_own$man_scalar[cum_reg_own$Region == reg_matches[r] & cum_reg_own$Ownership == reg_own_matches[o] &
																cum_reg_own$Land_Type == "Meadow"]
																
									} # end if decrease woodland restoration else increase meadow restoration

								} # end if calculate adjustment scalars	
							} # end for o loop over matching regions and ownerships for woodland and meadow
						} # end if there are matching ownerships within region r for woodland and meadow
					} # end for r loop over matching regions
				} # end if there are matching regions for woodland and meadow
			} # end if there are both woodland and meadow records

			# now scale the management areas to representative regional areas
			# scale the values
			cum_reg_own$end_area = cum_reg_own$end_area * cum_reg_own$man_scalar
			# this should always be zero
			cum_reg_own$start_area = cum_reg_own$start_area * cum_reg_own$man_scalar

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
		# Dead_removal and Urban_forest are fractions of Developed_all during the specified year, and must always be 1
		# Urban_forest fractions are based on a desired fraction by the end of a target year
		#  urban forest fractions can be changed with either of the above because they don't interact with either
		# Growth is fraction of initial Developed_all annual change during the specified year, based on a desired rate change during a target year
		# Region has to be all All or each region has to be defined
		# county and project are processed the same except for the available area because all practices always have to be defined
		
		# this is always true because urban management always has to be defined
		if(nrow(urban) > 0) {
			# the regional urban areas and scaling
			# separate any "All" region entries so that the appropriate regions can be specified
			# ownership should all be All
			# these records will be expanded to regional urban area for All ownership
			
			# first make sure that all three practices are present
			urb_dr = urban[urban$Management == "Dead_removal",]
			urb_g = urban[urban$Management == "Growth",]
			urb_uf = urban[urban$Management == "Urban_forest",]
			if ( !(nrow(urb_dr) > 0) | !(nrow(urb_g) > 0) | !(nrow(urb_uf) > 0) ) {
				stop("All three urban practices must be defined for all space and all simulation years\n")
			}
			
			# next make sure that Ownership is All everywhere
			urb_all_own = urban[urban$Ownership == "All",]
			urb_own = urban[urban$Ownership != "All",]
			if (nrow(urb_own) > 0 | nrow(urb_all_own) != nrow(urban)) {
				stop("Developed_all Ownership must be 'All' for all urban records\n")
			}
			
			# next check to make sure that Dead_removal and Growth are always 1
			temp_df = urban[urban$Management == "Dead_removal" | urban$Management == "Growth",]
			non_inds = c(which(temp_df$start_area_frac != 1), which(temp_df$end_area_frac != 1))
			if ( length(non_inds) > 0 ) {
				stop("For Developed_all: Dead_removal and Growth must be defined as 1 for all records\n")
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
			
			# now calculate the management scaling ratio region:county for consistency, however it isn't needed here, but mayber later?
			urb_all_own$man_scalar = urb_all_own$reg_lt_area / urb_all_own$cnty_reg_lt_area
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
		scen_table_out$man_scalar = NULL
		
		# put the new scenario raw table into the raw output list
		out_scen_df_list[[s]] = scen_table_out
		
		# put the manage table with scalars into a new output list
		out_man_df_list[[s]] = man_table_out

		############# output scalars are scenario-specific!!
		# but the years should be the same for all scenarios
		# so write a separate worksheet for each scenario - make sure to be able to match them to the output files using the worksheet names
		
		# loop over all simulation years to calculate output scalars
		# annual and urban scalars do not need to be changed from initial_scalar = county lc area / region lc area because lc areas do not change
		# restoration scalars need to be calculated based on the expected area of each lc in each year
		# store the expected areas because write_scaled_outputs may need to update these where restoration causes source limitations
		#	which means that these expected areas may not be the region-level areas in the simulation
		
		# get the simulation years
		first_year = min(c(scenin_df_list[[s]]$start_year, scenin_df_list[[s]]$end_year))
		last_year = max(c(scenin_df_list[[s]]$start_year, scenin_df_list[[s]]$end_year))
		
		# get all target manage years
		man_start = sort(unique(scenin_df_list[[s]]$start_year))
		man_end = sort(unique(scenin_df_list[[s]]$end_year))
		man_years = c(man_start, man_end)
		man_years = sort(unique(man_years))
		num_man_years = length(man_years)
		
		# loop over the simulation years
		for (year in first_year:last_year) {
    		
    		# set some management year labaels
    		ccol = paste0("exp_cnty_lc_area_", year)
    		pcol = paste0("exp_cnty_lc_area_", year-1)
    		
    		# assign the current year labels
    		cnty_col = paste0("exp_cnty_lc_area_", year)
    		reg_col = paste0("exp_reg_lc_area_", year)
    		scalar_col = paste0("exp_output_scalar_", year)
    		# assign the previous year labels
    		prev_cnty_col = paste0("exp_cnty_lc_area_", year-1)
    		prev_reg_col = paste0("exp_reg_lc_area_", year-1)
    		prev_scalar_col = paste0("exp_output_scalar_", year-1)
    		# assign the next year labels
    		next_cnty_col = paste0("exp_cnty_lc_area_", year+1)
    		next_reg_col = paste0("exp_reg_lc_area_", year+1)
    		next_scalar_col = paste0("exp_output_scalar_", year+1)
    		
    		# add the first year column and set it to the initial values because this is the start of the year
    		if (year == first_year) {
    			output_scalars[, cnty_col] = output_scalars$cnty_lc_area
    			output_scalars[, reg_col] = output_scalars$lc_area
    			output_scalars[, scalar_col] = output_scalars$initial_scalar
    		}
    		
    		# add next year's column
    		# this is to store the new area values, which are this year's area + this year's change
    		# the land cat areas represent the beginning of the year, and changes are applied at the end of the year
    		output_scalars[, next_cnty_col] = NA
    		output_scalars[, next_reg_col] = NA
    		output_scalars[, next_scalar_col] = NA

			# if this is an annual practice scenario or just urban, use the initial values for all land cats
			if (nrow(cumulative) == 0) {
				output_scalars[, next_cnty_col] = output_scalars$cnty_lc_area
				output_scalars[, next_reg_col] = output_scalars$lc_area
				output_scalars[, next_scalar_col] = output_scalars$initial_scalar
			} else {
				# need to associate practices with land cats and calc expected areas, based on previous year's distribution
				# restoration land cats should have expected scalars equal to the initial scalars
				# source land cats will have different expected scalars
				# other (unchanging) land cats will have the initial scalars
				# if restoration limitations happen, then write_scaled_outputs has to calculate:
				#	the actual county area for restoration land cats based on the expected scalar
				#	then actual county areas for the source land cats based on the previous years distribution
				#	then actual scalars for source land cats based on actual county areas and actual region areas
				
				# get indices in output_scalars of rows corresponding to active restored land cats and active source land cats
				# also get the restoration and source areas - recall that cum_reg_own contains the adjusted scaled areas
				rest_rows = NULL
				source_rows = NULL
				# store the land cat restoration areas for this year
				rest_totals[] = 0
				scaled_rest_totals = array(dim=num_lc)
				scaled_rest_totals[] = 0
				# store the land cat source areas for this year
				source_totals[,] = 0
				scaled_source_totals[,] = 0
				for( lc in 1:nrow(unique_lc) ) {
					# restoration land cats
					rest_rows = c(rest_rows, which(output_scalars$Region == unique_lc$Region[lc] & output_scalars$Ownership == unique_lc$Ownership[lc] &
									output_scalars$Land_Type == unique_lc$Land_Type[lc]))
					
					# get the restoration amounts for this year, which will be applied to this year's area and the result stored in next year's column
					# calc annual amount for each relevant land cat row, then add them up
					# the annual amounts are applied during start_year to end_year, such that the target is reached at the end of end_year
					# 	for each row: (end_area - start_area) / (end_year - start_year + 1)
					records = cum_reg_own[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc],]
					for (r in 1:nrow(records)) {
						if (year >= records$start_year[r] & year <= records$end_year[r]) {
							scaled_rest_totals[lc] = scaled_rest_totals[lc] + (records$end_area[r] - records$start_area[r]) / (records$end_year[r] - records$start_year[r] + 1)
						}
					}
					rest_totals[lc] = scaled_rest_totals[lc] / cum_reg_own$man_scalar[cum_reg_own$Region == unique_lc$Region[lc] & cum_reg_own$Ownership == unique_lc$Ownership[lc] &
											cum_reg_own$Land_Type == unique_lc$Land_Type[lc]][1]
					
					# get the source amounts for this year, which will be applied to this year's area and the result stored in next year's column
					# these are based on the expected land cat areas for this year
					
					# determine the region-ownership index for source totals
					ro_reg_ind = which(unique_ro$Region == unique_lc$Region[lc])
					ro_own_ind = which(unique_ro$Ownership == unique_lc$Ownership[lc])
					ro_ind = intersect(ro_reg_ind, ro_own_ind)
					# determine restoration type index and the management type
					# Forest has two types of restoration, so two rt_ind values will be returned
					#  parse this and deal with the different practice types
					#  afforestation and reforestation are not allowed in the same scenario - this is checked above
					rt_ind = which(restoration_lt == unique_lc$Land_Type[lc])
					if (length(rt_ind) == 2) {
						if ("Afforestation" %in% cum_reg_own$management) {
							rt_ind = 5
							practice = "Afforestation"
						} else {
							rt_ind = 6
							practice = "Reforestation"
						}
					} else if (length(rt_ind) == 1) {
						practice = "Restoration"
					} else if (length(rt_ind) == 0) {
						stop("Incorrect restoration land type ", unique_lc$Land_Type[lc], "\n")
					} # end if-else restoration type
				
					# sum the source areas
					st_den = 0 # this is to store the county-level total source area for comparison below
					st_sum[] = 0
					# sum the scaled source areas
					st_den2 = 0 # this is to store the region-level total source area for comparison below
					st_sum_scaled[] = 0
					for (src in 1:num_restoration_sources[rt_ind]) {
						st_ind = which(source_totals_names == restoration_sources[rt_ind,src])
						lc_source_row = which(output_scalars$Region == unique_lc$Region[lc] & output_scalars$Ownership == unique_lc$Ownership[lc] &
									output_scalars$Land_Type == restoration_sources[rt_ind, src])
						# this is for subsetting below
						source_rows = c(source_rows, lc_source_row)
						# get the expected county and region areas
						st_num = output_scalars[lc_source_row, cnty_col]
						temp = output_scalars[lc_source_row, reg_col]
						# county
						if(length(st_num) == 0) { st_num = 0 }
						st_den = st_den + st_num
						st_sum[st_ind] = st_sum[st_ind] + rest_totals[lc] * st_num
						# region
						if(length(temp) == 0) { temp = 0 }
						st_den2 = st_den2 + temp
						st_sum_scaled[st_ind] = st_sum_scaled[st_ind] + scaled_rest_totals[lc] * temp
					} # end for summing up individual sources for non-forest restoration
					# keep track of sources in this region-ownership
					source_totals[ro_ind,] = source_totals[ro_ind,] + st_sum / st_den
					scaled_source_totals[ro_ind,] = scaled_source_totals[ro_ind,] + st_sum_scaled / st_den2
					
				} # end for lc loop to get restoration and source rows and areas
				
				# set the unchanging values
				output_scalars[-unique(c(rest_rows, source_rows)), next_cnty_col] = output_scalars$cnty_lc_area[-unique(c(rest_rows, source_rows))]
				output_scalars[-unique(c(rest_rows, source_rows)), next_reg_col] = output_scalars$lc_area[-unique(c(rest_rows, source_rows))]
				output_scalars[-unique(c(rest_rows, source_rows)), next_scalar_col] = output_scalars$initial_scalar[-unique(c(rest_rows, source_rows))]
				
				# calc some values
    			if ( year < (min(man_years)) ) {
    				# if the year is prior to any change then set these values to the initial values
    				# no change is recorded in the first year of management because areas are for the beginning of the year and change happens at the end of the year
    				# this includeds the first year of the sim
    				# the first year of the sim experiences no change because areas are for the beginning of the year and change happens at the end of the year
    				# so set this to the initial values
    				output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col] = output_scalars$cnty_lc_area[unique(c(rest_rows, source_rows))]
					output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] = output_scalars$lc_area[unique(c(rest_rows, source_rows))]
					output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col] = output_scalars$initial_scalar[unique(c(rest_rows, source_rows))]
    			} else if ( year > (max(man_years) ) ) {
    				# if the current year is past the last target year + 1 then use the current year's value because there is no more change
    				# set the values to the previous values
    				output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col] = output_scalars[unique(c(rest_rows, source_rows)), cnty_col]
    				output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] = output_scalars[unique(c(rest_rows, source_rows)), reg_col]
    				output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col] = output_scalars[unique(c(rest_rows, source_rows)), scalar_col]
    			} else  {
					# in the management period
    				# calculate expected areas for restoration land cats
    				for( lc in 1:nrow(unique_lc) ) {
    					# county
						output_scalars[output_scalars$Region == unique_lc$Region[lc] & output_scalars$Ownership == unique_lc$Ownership[lc] &
									output_scalars$Land_Type == unique_lc$Land_Type[lc], next_cnty_col] =
							output_scalars[output_scalars$Region == unique_lc$Region[lc] & output_scalars$Ownership == unique_lc$Ownership[lc] &
									output_scalars$Land_Type == unique_lc$Land_Type[lc], cnty_col] + rest_totals[lc]
						# region
						output_scalars[output_scalars$Region == unique_lc$Region[lc] & output_scalars$Ownership == unique_lc$Ownership[lc] &
									output_scalars$Land_Type == unique_lc$Land_Type[lc], next_reg_col] =
							output_scalars[output_scalars$Region == unique_lc$Region[lc] & output_scalars$Ownership == unique_lc$Ownership[lc] &
									output_scalars$Land_Type == unique_lc$Land_Type[lc], reg_col] + scaled_rest_totals[lc]
					}
    				# calculate the expected areas for source land cats, subtract from the next col in case any of these overlapping with restoration
    				for ( ro in 1:nrow(unique_ro)) {
    					for ( src in 1:num_sources) {
    						# this land cat needs to exist to process it
    						source_lc = output_scalars[output_scalars$Region == unique_ro$Region[ro] &
    									output_scalars$Ownership == unique_ro$Ownership[ro] & output_scalars$Land_Type == source_totals_names[src],]
    						if (nrow(source_lc) > 0) {
    							# if this is a woodland source then check to see if there is restoration in this land cat
    							#  if so, use next column for current area because restoration has been calculated already
    							if ( src == woodland_st_index & !is.na(source_lc[1, next_cnty_col]) ) {
    								# county
									output_scalars[output_scalars$Region == unique_ro$Region[ro] & output_scalars$Ownership == unique_ro$Ownership[ro] &
											output_scalars$Land_Type == source_totals_names[src], next_cnty_col] =
												source_lc[1, next_cnty_col] - source_totals[ro, src]
									# region
									output_scalars[output_scalars$Region == unique_ro$Region[ro] & output_scalars$Ownership == unique_ro$Ownership[ro] &
											output_scalars$Land_Type == source_totals_names[src], next_reg_col] =
												source_lc[1, next_reg_col] - scaled_source_totals[ro, src]			
    							} else {
    								# county
									output_scalars[output_scalars$Region == unique_ro$Region[ro] & output_scalars$Ownership == unique_ro$Ownership[ro] &
											output_scalars$Land_Type == source_totals_names[src], next_cnty_col] =
												source_lc[1, cnty_col] - source_totals[ro, src]
									# region
									output_scalars[output_scalars$Region == unique_ro$Region[ro] & output_scalars$Ownership == unique_ro$Ownership[ro] &
											output_scalars$Land_Type == source_totals_names[src], next_reg_col] =
												source_lc[1, reg_col] - scaled_source_totals[ro, src]		
								} # end if woodland restoration land cat else not
							} # end it this source land cat exists			
    					} # end for src loop
    				} # end for ro loop
    				
    				### throw errors about source areas going to zero, as this indicates that prescribed restoration may not be met
    				
    				# if expected areas go negative set to zero, if region areas are zero set the scaling factor to NA
    				#   the scalar shouldn't matter if region goes to zero because all values should go to zero, so set this to zero also
    				
    				#county
    				cnty_zero_rows = which(output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col] <= 0)
    				if (length(cnty_zero_rows) > 0) {
    					for (rec in 1:length(cnty_zero_rows)) {
    						cat("Error: County land category ", output_scalars[unique(c(rest_rows, source_rows)), "Region"][cnty_zero_rows[rec]], "-",
    													output_scalars[unique(c(rest_rows, source_rows)), "Land_Type"][cnty_zero_rows[rec]], "-",
    													output_scalars[unique(c(rest_rows, source_rows)), "Ownership"][cnty_zero_rows[rec]],
    													"goes to zero for beginning of year ", year+1, "\n")
    						cat("This likely results from interactions between multiple restoration activities.\n")
    						cat("This indicates that the prescribed restoration target(s) for land type(s) drawing from this source may not be met\n")
    						cat("Which means that other land category estimates may be affected, and the total county area may not be preserved\n")
    						cat("Please reduce prescribed restoration values to avoid this situation\n")
    						cat("The restoration types and their sources are:\n")
    						cat("Restoration type:\tSources\n")
    						cat("Meadow:\tShrubland, Grassland, Savanna, Woodland\n")
							cat("Fresh_marsh:\tCultivated\n")
							cat("Coastal_marsh:\tCultivated\n")
							cat("Woodland:\tGrassland, Cultivated\n")
							cat("Forest-Afforestation:\tShrubland, Grassland\n")
							cat("Forest-Reforestation:\tShrubland\n")
							stop()
    					}
    				}
    				output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col] <- replace(output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col],
    						output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col] <= 0, 0.0)
    				
    				# region
    				reg_zero_rows = which(output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] <= 0)
    				if (length(reg_zero_rows) > 0) {
    					for (rec in 1:length(reg_zero_rows)) {
    						cat("Error: Scaled region land category ", output_scalars[unique(c(rest_rows, source_rows)), "Region"][reg_zero_rows[rec]], "-",
    													output_scalars[unique(c(rest_rows, source_rows)), "Land_Type"][reg_zero_rows[rec]], "-",
    													output_scalars[unique(c(rest_rows, source_rows)), "Ownership"][reg_zero_rows[rec]],
    													"goes to zero for beginning of year ", year+1, "\n")
    						cat("This likely results from interactions between multiple restoration activities.\n")
    						cat("This indicates that the scaled prescribed restoration target(s) for land type(s) drawing from this source may not be met\n")
    						cat("Which means that other land category estimates may be affected, and the total county area may not be preserved\n")
    						cat("Please reduce prescribed restoration values to avoid this situation\n")
    						cat("The restoration types and their sources are:\n")
    						cat("Restoration type:\tSources\n")
    						cat("Meadow:\tShrubland, Grassland, Savanna, Woodland\n")
							cat("Fresh_marsh:\tCultivated\n")
							cat("Coastal_marsh:\tCultivated\n")
							cat("Woodland:\tGrassland, Cultivated\n")
							cat("Forest-Afforestation:\tShrubland, Grassland\n")
							cat("Forest-Reforestation:\tShrubland\n")
							stop()
    					}
    				}
    				output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] <- replace(output_scalars[unique(c(rest_rows, source_rows)), next_reg_col],
    						output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] <= 0, 0.0)
    				
      				# calculate the expected scalars for the above rows
      				output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col] =
      					output_scalars[unique(c(rest_rows, source_rows)), next_cnty_col] / output_scalars[unique(c(rest_rows, source_rows)), next_reg_col]
      				
      				### these will either be NaN or Inf, if region goes to zero; zero just means that county area goes to zero
      				output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col] <- replace(output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col],
    						output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] == Inf, 0.0)
    				output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col] <- replace(output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col],
    						output_scalars[unique(c(rest_rows, source_rows)), next_reg_col] == -Inf, 0.0)
    				output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col] <- replace(output_scalars[unique(c(rest_rows, source_rows)), next_scalar_col],
    						is.nan(output_scalars[unique(c(rest_rows, source_rows)), next_reg_col]), 0.0)
      				
 				} # end if set values outside restoration period else calculate expected area during restoration period
				
			} # end if not restoration else restoration scenario for calculating expected areas and scalars
			
			# put this table into the output scalar list
			out_scalar_df_list[[s]] = output_scalars
			
		} # end for year loop over years for output scalars

	} # end s loop over scenario sheets

		
		############## this (!ISCOUNTY) block is not correct and i haven't fixed it
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
			output_scalars$initial_scalar = output_scalars$cnty_lc_area / output_scalars$lc_area
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
		out_wrkbk =  loadWorkbook(out_scalar_file, create = TRUE)
		createSheet(out_wrkbk, name = out_scen_sheets)
		clearSheet(out_wrkbk, sheet = out_scen_sheets)
		writeWorksheet(out_wrkbk, data = out_scalar_df_list, sheet = out_scen_sheets, startRow = 1, header = TRUE)
		# shut off wrap text
		cs <- createCellStyle(out_wrkbk)
		setWrapText(cs, wrap = FALSE)
		for (i in 1:length(out_scen_sheets)) {
			rc = expand.grid(row = 1:(nrow(out_scalar_df_list[[i]])+start_row), col = 1:ncol(out_scalar_df_list[[i]]))
			setCellStyle(out_wrkbk, sheet = out_scen_sheets[i], row = rc$row, col = rc$col, cellstyle = cs)
		}
		# write the workbook
		saveWorkbook(out_wrkbk)

	cat("Finish write_scaled_raw_scenarios at", date(), "\n")	
	
}