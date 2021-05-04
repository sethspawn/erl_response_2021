rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "ggplot2",
  "plyr",
  "ggpubr"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================

# load data for fig3 (individual data sources are reported in "notes" column); units are gCO2e/MJ
df = read.csv('cclub_scenario_variation.csv')
colnames(df)[grepl('pathway', colnames(df))] = 'pathway'

# set identifiers as factors
df$pathway = as.factor(df$pathway)
df$ef = factor(df$ef, levels = c('winrock', 'woodshole', 'century'))
df$landcover = factor(df$landcover, levels = c('cropland-pasture', 'grassland', 'forest', 'young forest-shrub'))

# calculate net emissions for each pathway/ef combo
df_net = ddply(df, c('pathway', 'ef'), summarize, net = sum(ci, na.rm = T))

#----------------------------------------------------------------------------------------------------
# for woods hole efs that omit cropland-pasture, calculate c/p based on both winrock and century

# duplicate df_net and set woods hole estimates to NA
df_net_noWH = df_net
colnames(df_net_noWH)[colnames(df_net_noWH) == 'net'] = 'net_noWH'
df_net_noWH$net_noWH[df_net_noWH$ef == 'woodshole'] = NA

# join to original df
df = join(df, df_net_noWH, by = c('pathway', 'ef'))

# duplicate df_net and only retain the woods hole estimates
df_net_WH = df_net
colnames(df_net_WH)[colnames(df_net_WH) == 'net'] = 'net_WH'
df_net_WH$net_WH[df_net_WH$ef != 'woodshole'] = NA

# add columns with the net CI if woods hole used either century or winrock to estimate cropland-pasture emissions
wh_cent = join(df[df$ef == 'century' & df$landcover == 'cropland-pasture', c('pathway', 'ci')],
               df_net_WH, by = c('pathway'))[,c('pathway', 'ef', 'ci', 'net_WH')]
wh_cent['netWH_centuryCP'] = rowSums(wh_cent[,c('ci', 'net_WH')])

wh_wnrk = join(df[df$ef == 'winrock' & df$landcover == 'cropland-pasture', c('pathway', 'ci')],
               df_net_WH, by = c('pathway'))[,c('pathway', 'ef', 'ci', 'net_WH')]
wh_wnrk['netWH_winrockCP'] = rowSums(wh_wnrk[,c('ci', 'net_WH')])

wh = join(wh_cent, wh_wnrk, by = c('pathway', 'ef'))[,c('pathway', 'ef', 'net_WH', 'netWH_centuryCP', 'netWH_winrockCP')]

# joins woods hole estimates to df
df = join(df, wh, by = c('pathway', 'ef'))

# add bars to show effects of adding either century or winrock cropland pasture emission factors
df['wh_cent_bar'] = rep(NA, nrow(df))
df$wh_cent_bar[df$ef == 'woodshole'] = df$ci[df$ef == 'woodshole']
df$wh_cent_bar[df$ef == 'woodshole' & df$landcover == 'cropland-pasture'] = df$ci[df$ef == 'century' & df$landcover == 'cropland-pasture']

df['wh_wnrk_bar'] = rep(NA, nrow(df))
df$wh_wnrk_bar[df$ef == 'woodshole'] = df$ci[df$ef == 'woodshole']
df$wh_wnrk_bar[df$ef == 'woodshole' & df$landcover == 'cropland-pasture'] = df$ci[df$ef == 'winrock' & df$landcover == 'cropland-pasture']


#----------------------------------------------------------------------------------------------------
# Create Stacked barplot.

# plot 1 shows results based on the Corn Ethanol 2011 scenario
p1 = ggplot(df[df$pathway == 'corn2011',], aes(x = ef)) + 
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("#eca14d", "#da4224", "#096264", "#5ac1ad"), 
                    name = 'Land source', labels = c('Cropland-pasture', 'Pasture', "Forest", "Young-Forest/Shrub"))+
  geom_bar(aes(fill=landcover, y=ci), position="stack", stat="identity",  width=.6)+
  geom_bar(aes(y = wh_wnrk_bar, fill=landcover), position="stack", stat="identity",  width=.6, alpha = 0.25 )+
  geom_bar(aes(y = wh_cent_bar, fill=landcover), position="stack", stat="identity",  width=.6, alpha = 0.25)+
  geom_errorbar(aes(y = net_noWH, ymin = net_noWH, ymax = net_noWH), colour = "black", size = 1, width = 0.6)+
  geom_errorbar(aes(y = netWH_centuryCP, ymin = netWH_centuryCP, ymax = netWH_centuryCP), size = 0.75, width = 0.6, position="identity", linetype = 'dashed')+
  geom_errorbar(aes(y = netWH_winrockCP, ymin = netWH_winrockCP, ymax = netWH_winrockCP), size = 0.75, width = 0.6, position="identity", linetype = 'dashed')+
  theme_bw()+
  ylab(expression('Carbon Intensity '~(gCO[2]*e~MJ^-1)~'\n'))+
  xlab("")+
  ggtitle('CCLUB Scenario: "Corn Ethanol 2011"')+
  scale_x_discrete(labels = c('Winrock', 'Woods Hole',  'CENTURY/COLE'))+
  scale_y_continuous(limits = c(-10, 17), expand = c(0, 0))+
  theme(panel.border = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),,
        text=element_text(size=11, family = 'Helvetica-Narrow'),
        plot.margin = unit(c(0.50,-10,0,0), "cm"))

p1 = p1 + annotate('segment', x = 0.6, y = 17, xend = 3.4, yend = 17, 
                   arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm")))


# plot 2 shows results based on the Corn Ethanol 2013 scenario
p2 = ggplot(df[df$pathway == 'corn2013',], aes(x = ef)) + 
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("#eca14d", "#da4224", "#096264", "#5ac1ad"), 
                    name = 'Land source', labels = c('Cropland-pasture', 'Pasture', "Forest", "Young-Forest/Shrub"))+
  geom_bar(aes(fill=landcover, y=ci), position="stack", stat="identity",  width=.6)+
  geom_bar(aes(y = wh_wnrk_bar, fill=landcover), position="stack", stat="identity",  width=.6, alpha = 0.25 )+
  geom_bar(aes(y = wh_cent_bar, fill=landcover), position="stack", stat="identity",  width=.6, alpha = 0.25)+
  geom_errorbar(aes(y = net_noWH, ymin = net_noWH, ymax = net_noWH), colour = "black", size = 1, width = 0.6)+
  geom_errorbar(aes(y = netWH_centuryCP, ymin = netWH_centuryCP, ymax = netWH_centuryCP), size = 0.75, width = 0.6, position="identity", linetype = 'dashed')+
  geom_errorbar(aes(y = netWH_winrockCP, ymin = netWH_winrockCP, ymax = netWH_winrockCP), size = 0.75, width = 0.6, position="identity", linetype = 'dashed')+
  theme_bw()+
  ylab("")+
  xlab("")+
  ggtitle('CCLUB Scenario: "Corn Ethanol 2013"')+
  scale_x_discrete(labels = c('Winrock', 'Woods Hole', 'CENTURY/COLE'))+
  scale_y_continuous(limits = c(-10, 17), expand = c(0, 0))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=11, family = 'Helvetica-Narrow'),
        plot.margin = unit(c(0.50,0,0,-10), "cm"))

# annotate to note which option Scully et al choose
p2 = p2 + annotate('segment', x = 0.6, y = 17, xend = 3.4, yend = 17, 
                   arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate('text', x = 2.4, y = -4, 
           label = "Scully et al. adopt the net result of the\nmost anomolous CCLUB configuration\nwithout explaination.", 
           vjust = 1, hjust = 1, size = 3.5)+
  annotate('curve', x = 2.4, y = -3.9, xend = 2.65, yend = -2.3,
           arrow = arrow(ends = "last", angle = 30, length = unit(.25,"cm"), type = 'closed'), 
           curvature = -0.35) 


# combine into side-by-side barplots
outFig = ggarrange(p1, p2, common.legend = T, legend = 'right', align = 'hv')

# save as pdf
pdf('Figure2.pdf', height = 3.5, width = 9, family = 'Helvetica-Narrow')
print(outFig)
dev.off()

