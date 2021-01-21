### 1/21/21 Two tests below look at the effect of updating CALAND 3.0.0. (master) with a bug fix to calculations for applying the input soil climate 
### scalars to soil carbon fluxes under cultivated soil conservation when the sign of the historic managed soil carbon flux is flipped from the unmanaged 
### historic flux. Based on the original lc_params inputs, it appears this is only the case for Delta soil conservation when CALAND() is run with mean+SD 
### for soil conservation (i.e, soil flux flips from negative without management to positive with management).
### Test 1 compares the outputs of the original NWL Alt A RCP8.5 scenario with and without the CALAND() updates - both run with soilcon +SD and 
### the original climate scalars. The outputs should be identical since there were no cases where the sign flipped because there was no prescribed Delta 
### soil conservation. The comparison of CO2e outputs in plot_caland show they are identical.
### Test 2 compares the outputs with and without the CALAND() updates using an edited version of the NWL Alt A RCP8.5 scenario with Delta soil conservation
### added, run at mean+SD for soil conservation, and using the original climate scalars. There should be differences between these two outputs. Since the orignal 
### climate scalars were <1 for the Delta cultivated, which decreases the soil c flux (less emissions) (note climate scalars were later corrected for this region), 
### the bug fix is expected to detect the the flipped sign of managed soil conservation soil carbon flux (net sequestration) and consequently adjust the climate scalars 
### so they have the opposite effect - in this case, increase the magnitude of that flux (i.e., greater net sequestration). The comparison of outputs in plot_caland
### show, as expected, that the CO2e sequestration is greater with the bug fix.

#########  1  ###########
# test climate scalar before adjustments on orginal CALAND master 3.0.0
# using original NWL scenario Alt A with setting for soil con +1SD (note there is no prescribed soil conservation)
cd Desktop/LBL/caland_3.0.0_master
git checkout master 
R
setwd("~/Desktop/LBL/caland_3.0.0_master")
source("CALAND.r")  
CALAND(scen_file_arg = "NWL_Alt_A_v6_default_RCP85.xls", c_file_arg = "carbon_input_nwl.xls", 
       indir = "",
       outdir = "", start_year = 2010, end_year = 2051, 
       value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, value_col_soilcon=9, 
       ADD_soilcon = TRUE, NR_Dist = 120, WRITE_OUT_FILE = TRUE, blackC = FALSE) 


# test climate scalar after edits to orginal CALAND master 3.0.0 on the bug fix branch 
# using original NWL scenario Alt A with setting for soil con +1SD (note there is no prescribed soil conservation)
cd Desktop/LBL/caland_3.0.0_master
git checkout climate_scalar_bug_fix_only
R
setwd("~/Desktop/LBL/caland_3.0.0_master")
source("CALAND.r")  
CALAND(scen_file_arg = "NWL_Alt_A_v6_default_RCP85.xls", c_file_arg = "carbon_input_nwl.xls", 
       indir = "",
       outdir = "bug_fix_orig_AltA", start_year = 2010, end_year = 2051, 
       value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, value_col_soilcon=9, 
       ADD_soilcon = TRUE, NR_Dist = 120, WRITE_OUT_FILE = TRUE, blackC = FALSE) 


# compare the two files to make sure the changes did not result in any differences between these 2 runs
R
setwd("~/Desktop/LBL/caland_3.0.0_master")
source("plot_caland.r")
plot_caland(scen_fnames=c("NWL_Alt_A_v6_default_RCP85_output_mean_S+sd_BC1_NR120.xls", "NWL_Alt_A_v6_default_RCP85_output_mean_S+sd_BC1_NR120_bug_fix.xls"), 
            scen_snames=c("Orig_Alt_A_soilcon+SD", "Bug_fix_Alt_A_soilcon+SD"), 
            data_dir = "./outputs", reg = c("All_region","Delta"),
            lt = c("All_land", "Cultivated"),
            own = c("All_own"), figdir = "figdir_comparison_orig_NWL_A", INDIVIDUAL = FALSE, units_ha=TRUE, blackC = FALSE, blackC_plot = FALSE, last_year = 2051)

######### 2 ###########
# test climate scalar before adjustments on orginal CALAND master 3.0.0
# using edited NWL scenario Alt A that now has managed Delta easement soil conservation (private area is depleted after a couple years) for 2010_ha, 2017_ha, 2018_ha, 2019_ha, 2029_ha, 
# 2030_ha: 10266.57	10266.57	10266.57	10266.57	10266.57	10266.57
# with setting for soil con +1SD 
cd Desktop/LBL/caland_3.0.0_master
git checkout master 
R
setwd("~/Desktop/LBL/caland_3.0.0_master")
source("CALAND.r")  
CALAND(scen_file_arg = "NWL_Alt_A_v6_default_RCP85_added_delta_soilcon.xls", c_file_arg = "carbon_input_nwl.xls", 
       indir = "",
       outdir = "", start_year = 2010, end_year = 2051, 
       value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, value_col_soilcon=9, 
       ADD_soilcon = TRUE, NR_Dist = 120, WRITE_OUT_FILE = TRUE, blackC = FALSE) 

# test climate scalar AFTER adjustments on bug fix branch of CALAND master 3.0.0
# using edited NWL scenario Alt A that now has managed Delta easement soil conservation (private area is depleted after a couple years) for 2010_ha, 2017_ha, 2018_ha, 2019_ha, 2029_ha, 
# 2030_ha: 10266.57	10266.57	10266.57	10266.57	10266.57	10266.57
# with setting for soil con +1SD 
cd Desktop/LBL/caland_3.0.0_master
git checkout climate_scalar_bug_fix_only
R
setwd("~/Desktop/LBL/caland_3.0.0_master")
source("CALAND.r")  
CALAND(scen_file_arg = "NWL_Alt_A_v6_default_RCP85_added_delta_soilcon.xls", c_file_arg = "carbon_input_nwl.xls", 
       indir = "",
       outdir = "bug_fix_edited_AltA", start_year = 2010, end_year = 2051, 
       value_col_dens = 7, ADD_dens = TRUE, value_col_accum = 7, ADD_accum = TRUE, value_col_soilcon=9, 
       ADD_soilcon = TRUE, NR_Dist = 120, WRITE_OUT_FILE = TRUE, blackC = FALSE) 

R
setwd("~/Desktop/LBL/caland_3.0.0_master")
source("plot_caland.r")
plot_caland(scen_fnames=c("NWL_Alt_A_v6_default_RCP85_added_delta_soilcon_output_mean_S+sd_BC1_NR120.xls", "NWL_Alt_A_v6_default_RCP85_added_delta_soilcon_output_mean_S+sd_BC1_NR120_bug_fix.xls"), 
            scen_snames=c("Orig_edited_Alt_A_soilcon+SD", "Bug_fix_edited_Alt_A_soilcon+SD"), 
            data_dir = "./outputs", reg = c("All_region","Delta"),
            lt = c("All_land", "Cultivated"),
            own = c("All_own", "Easement"), figdir = "figdir_comparison_edited_NWL_A", INDIVIDUAL = FALSE, units_ha=TRUE, blackC = FALSE, blackC_plot = FALSE, last_year = 2051)
\