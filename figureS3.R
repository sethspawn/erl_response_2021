rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "ggplot2",
  "plyr"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================

# load data for fig3 (individual data sources are reported in "notes" column); units are gCO2e/MJ
df = read.csv('alternate_tillage_yield.csv')
colnames(df)[grepl('Scenario', colnames(df))] = 'Scenario'

df$Tillage = factor(df$Tillage, levels = c('conventional', 'reduced', 'no', 'us_average'))
df$Yield = factor(df$Yield, levels = c('sb', 'sa'))

#----------------------------------------------------------------------------------------------------
# Create Stacked barplot.
p1 = ggplot(df, aes(x = Yield, y = d_LUC_CI, fill = Tillage)) + 
  scale_fill_manual(values = c("#eca14d", "#da4224", "#096264", "#5ac1ad"), 
                    name = 'Tillage', labels = c('Conventional', 'Reduced', "No-Till", "US Average"))+
  geom_bar(stat="identity",position="dodge",  width=.6)+
  geom_hline(yintercept = 0)+
  ylab(expression('Carbon Intensity '~(gCO[2]*e~MJ^-1)~'\n'))+
  scale_x_discrete(labels = c('Static Yield', 'Increasing Yield'))+
  xlab("")+
  theme_bw()+
  theme(text=element_text(size=11, family = 'Helvetica-Narrow'))+
  annotate('text', x = 2.225, y = -2.3*0.95, label = 'Assumptions of Scully et al.', angle = 90, hjust = 0, size = 2)
 
# save as a pdf  
pdf('FigureS3.pdf', height = 2.5, width = 4, family = 'Helvetica-Narrow')
print(p1)
dev.off()
