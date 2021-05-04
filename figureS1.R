rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "ggplot2",
  "ggpubr",
  "ggh4x"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================
# source formatted cclub data
source('cclub_formatData.R')

#====================================================================================================
# create figure S1 (density plots for cropland/pasture/cropland-pasture --> corn of all CCLUB scenarios)

# restructure data frame for faceted plot
allPlotDF = do.call(rbind.data.frame, lapply(split(scen_key, seq(nrow(scen_key))), function(scen, df){
  
  # get a data frame of length(df), with the four assumptions of each scenario
  scen_assums = scen[,which(colnames(scen) %in% c('Depth', 'AB', 'Residue', 'Tillage'))]
  assum_df = do.call(rbind.data.frame, lapply(1:nrow(df), function(i) scen_assums))
  
  # extract the land use labels for each scenario
  scen_labs = scen[,grepl('s_', colnames(scen))]
  scen_labs = data.frame('Initial_Landcover' = names(scen_labs), label = unname(t(scen_labs)))
  
  # iterate through each land use category; get values and merge with assuptions dataframe; rbind to large date frame
  scen_full_df = do.call(rbind.data.frame, lapply(split(scen_labs, seq(nrow(scen_labs))), function(scen, df, assum_df){
    
    # create data frame with one column denoting initial landcover and another with the corresponding EFs
    outDF = data.frame('Initial_Landcover' = rep(gsub('s_', '', scen$Initial_Landcover)),
                       'Value' = df[,which(colnames(df) == scen$label)])
    
    # return appended to assumption data frame
    return(cbind(outDF, assum_df))
    
  }, df = df, assum_df = assum_df))
  
}, df = df))

# creating "line" attribute for plotting to display the "cropland-pasture" dotted so it's overlap with "cropland" is more visible
allPlotDF['line'] = rep(NA, nrow(allPlotDF))
allPlotDF$line = as.factor(ifelse(allPlotDF$Initial_Landcover == 'Cropland-Pasture', 2, 1))

# reformat labeling data for plotting
allPlotDF$Tillage = ifelse(allPlotDF$Tillage == 'Conv.', 'Conventional', allPlotDF$Tillage)
allPlotDF$Tillage = ifelse(allPlotDF$Tillage == 'Red.', 'Reduced', allPlotDF$Tillage)
allPlotDF$Tillage = ifelse(allPlotDF$Tillage == 'US Avg.', 'US Average', allPlotDF$Tillage)
allPlotDF$Tillage = paste0(allPlotDF$Tillage, ' Tillage')
allPlotDF$Residue = paste0(allPlotDF$Residue, '%  Residue')
allPlotDF$Depth = paste0(allPlotDF$Depth, 'cm  (Max. Depth)')
allPlotDF$AB = ifelse(allPlotDF$AB == 'a', 'a. Increasing Yield', 'b. Constant Yield')
allPlotDF$Depth =factor(allPlotDF$Depth, levels = c("30cm  (Max. Depth)" , "100cm  (Max. Depth)" ))
allPlotDF$Value = as.numeric(allPlotDF$Value)

# create density plot of "cropland", "pasture", and "cropland-pasture" emissions factors
fs1 = ggplot(allPlotDF, aes(x = Value, fill = Initial_Landcover, colour = Initial_Landcover, linetype = line)) +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed') +  # vertical line division of emission vs. sequestration
  geom_density(alpha = 0.25) +
  scale_fill_manual(values = c("#eca14d", "#da4224", "#096264"), name="Initial Landcover")+
  scale_colour_manual(values = c("#eca14d", "#da4224", "#096264"), name="Initial Landcover")+
  facet_nested(Depth + AB ~ Tillage + Residue)+
  xlab(expression(paste("\nCENTURY-based Emissions Factor ",(MgC~ha^-1~yr^-1))))+
  ylab('Density\n') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size=8),
        legend.title=element_text(size=10))+
  guides(linetype = FALSE,  # change line around cropland-pasture legend item to dotted
         color = FALSE,
         fill = guide_legend(
           order = 1,
           override.aes = list(
             color = c("#eca14d", "#da4224", "#096264"),
             fill = c("#eca14d", "#da4224", "#096264"),
             linetype = c("solid", "dotted", 'solid'))))


# save as pdf 
pdf('FigureS1.pdf', height = 7, width = 16, family = 'Helvetica-Narrow')
print(fs1)
dev.off()
