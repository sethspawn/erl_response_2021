
#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "readxl"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================
# load GREET and it's CCLUB emission factors
# download GREET manually from: https://greet.es.anl.gov/greet_1_series 
# doi: 10.11578/GREET-Excel-2020/dc.20200912.1

# unzip GREET download
unzip('GREET_2020.zip')

# read the "C-Database" sheet (i.e. CCLUB soil C fluxes) from GREET's "CCLUB_2020_for_GREET1_2020.xlsm" file
df = read_excel('CCLUB_2020_for_GREET1_2020.xlsm', sheet = "C-Database", skip = 24)  # start reading file at line 25

# format column names to reflect CCLUB scenarios: "s[]_[]_cm_Scenario_[]"
colnames(df) = gsub(' ', "_", paste(do.call(rbind.data.frame, strsplit(colnames(df), "[...]"))[,1], df[1,]))
df = df[-1,] # remove first row which contained the scenario numbers

# correct inconsistent naming in CCLUB data sheet
colnames(df) = gsub('sb_100cm_','sb_100_cm_', colnames(df))

# convert from tibble to data.frame
df = as.data.frame(df)

#====================================================================================================
# generate key of assumptions associated with each set of CCLUB scenarios

# load CCLUB scenario key made manually to link each scenario with it's underlying assumptions
key = read.csv('CCLUB_scenario_key.csv')

#----------------------------------------------------------------------------------------------------
# reformat key:

# split key into list of dataframes by depth and assumed productivity, residue, and tillage.
scen_list = unlist(lapply(unlist(lapply(unlist(lapply(split(key, key$Depth), 
                                                      function(df) split(df, df$a.b)), recursive = F), 
                                        function(df) split(df, df$Residue)), recursive = F), 
                          function(df) split(df, df$Tillage)),recursive = F)

# reformat each subset 
scen_list = lapply(scen_list, function(df){
  
  # get the cropland, pasture, and cropland pasture scenario numbers for the final subset; name accordingly
  scens = df$Scenario
  names(scens) = df$Initial
  
  # return vector recording unique assumptions and correspondind scenario numbers
  return(c(Depth = df$Depth[1], AB = df$a.b[1], Residue = df$Residue[1], Tillage = df$Tillage[1], scens))
})

# reduce to data frame
scen_key = do.call(rbind.data.frame, scen_list)
names(scen_key) = names(scen_list[[1]])

# add columns with the actual scenario names corresponding with each set of assumptions
scen_key['s_Cropland'] = paste0('s',scen_key$AB,"_",scen_key$Depth,"_cm_Scenario_",scen_key$Cropland)
scen_key['s_Pasture'] = paste0('s',scen_key$AB,"_",scen_key$Depth,"_cm_Scenario_",scen_key$Pasture)
scen_key['s_CroplandPasture'] = paste0('s',scen_key$AB,"_",scen_key$Depth,"_cm_Scenario_",scen_key$'Cropland-pasture')

# truncate tillage names for plot labeling
scen_key$Tillage = gsub(' tillage', '', scen_key$Tillage)
scen_key$Tillage = gsub('Conventional', 'Conv.', scen_key$Tillage)
scen_key$Tillage = gsub('US Average', 'US Avg.', scen_key$Tillage)
scen_key$Tillage = gsub('Reduced', 'Red.', scen_key$Tillage)

# order rows to match the column order of the "C-Database" sheet for plotting
scen_key = scen_key[with(scen_key, order(as.numeric(Depth), AB, Cropland)), ]