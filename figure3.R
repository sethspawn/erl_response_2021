rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "ggplot2",
  "ggpubr"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================
# source formatted cclub data
source('cclub_formatData.R')

#====================================================================================================
# define density plot function

cclub_densityPlot = function(df, scen_key, set){
  
  # print out scenario combination corresponding with set
  cat(scen_key$s_Cropland[set], ' --- ', scen_key$s_Pasture[set], ' --- ', scen_key$s_CroplandPasture[set], '\n')
  
  # create sub dataframe with just cropland, cropland-pasture, and grassland data for plotting
  df_sub = data.frame(scn = c(rep('Cropland', nrow(df)), rep('Pasture', nrow(df)), rep('Cropland-Pasture', nrow(df))),
                      val = c(df[,scen_key$s_Cropland[set]], df[,scen_key$s_Pasture[set]], df[,scen_key$s_CroplandPasture[set]]))
  df_sub$val = as.numeric(as.character(df_sub$val)) # convert to numeric
  
  # creating "line" attribute for plotting to display the "cropland-pasture" dotted so it's overlap with "cropland" is more visible
  df_sub['line'] = rep(NA, nrow(df_sub))
  df_sub$line = as.factor(ifelse(df_sub$scn == 'Cropland-Pasture', 2, 1))
  
  # create density plot of "cropland", "pasture", and "cropland-pasture" emissions factors
  p = ggplot(df_sub, aes(x = val, fill = scn, colour = scn, linetype = line)) +
    geom_vline(xintercept = 0, color = 'black', linetype = 'dashed') +  # vertical line division of emission vs. sequestration
    geom_density(alpha = 0.25) +
    scale_fill_manual(values = c("#eca14d", "#da4224", "#096264"), name="Initial Landcover")+
    scale_colour_manual(values = c("#eca14d", "#da4224", "#096264"), name="Initial Landcover")+
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
  
  return(p)
  
}

#====================================================================================================
# create figure 3 (CCLUB EF example); with annotations

# P1 for EFs to a depth of 30cm
p1 = cclub_densityPlot(df, scen_key, set = 1) + 
  theme(legend.position = c(0.75, 0.75)) +
  coord_cartesian(ylim = c(0, 4.2)) + 
  annotate('text', x = 0.05, y = 4.2, label = 'Sequestration', hjust = 0, vjust = 0, size = 2) +
  annotate('segment', x = 0.05, y = 4.2*0.99, xend = 0.75, yend = 4.2*0.99, 
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"))) +
  annotate('text', x = -0.05, y = 4.2, label = 'Emission', hjust = 1, vjust = 0, , size = 2) +
  annotate('segment', x = -0.05, y = 4.2*0.99, xend = -0.75, yend = 4.2*0.99, 
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"))) +
  annotate('text', x = 1, y = 1.25, label = 'CENTURY-based emissions\nfactors for conversion of "cropland"\nand "cropland-pasture" to corn\nare nearly identical and both\nindicate sequestration.', hjust = 0, vjust = 0.5, , size = 2)+
  annotate('curve', x = 1, y = 1.7, xend = 0.4, yend = 2.25, curvature = 0.3,
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"))) +
  ggtitle('30cm Max. Depth')

# P2 for EFs to a depth of 100cm
p2 = cclub_densityPlot(df, scen_key, set = 17) + 
  theme(legend.position = c(0.75, 0.75)) +
  coord_cartesian(ylim = c(0, 4.2)) + 
  annotate('text', x = 0.05, y = 4.2, label = 'Sequestration', hjust = 0, vjust = 0, size = 2) +
  annotate('segment', x = 0.05, y = 4.2*0.99, xend = 0.75, yend = 4.2*0.99, 
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"))) +
  annotate('text', x = -0.05, y = 4.2, label = 'Emission', hjust = 1, vjust = 0, , size = 2) +
  annotate('segment', x = -0.05, y = 4.2*0.99, xend = -0.75, yend = 4.2*0.99, 
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"))) +
  # annotate('text', x = 1, y = 1.25, label = 'CCLUB-estimated emissions\nfor conversion of "cropland" and\n"cropland-pasture" to corn are\nsimilar but distinct; both indicate\nsequestration.', hjust = 0, vjust = 0.5, , size = 2)+
  # annotate('curve', x = 1, y = 1.7, xend = 0.4, yend = 2.25, curvature = 0.3,
  #          arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"))) + 
  ggtitle('100cm Max. Depth')

# combine p1 and p2 as a single figure
f3 = ggarrange(p1, p2, ncol = 2, labels = letters)

f3

# save as pdf 
pdf('Figure3.pdf', height = 3.5, width = 8, family = 'Helvetica-Narrow')
print(f3)
dev.off()
