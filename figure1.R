rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "ggplot2",
  "plyr",
  "grid",
  "ggpubr"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================

# load data taken manually from Scully et al figure 2 and Table S1
df = read.csv('data_from_scully_fig2.csv')

# excluding 2019 CARB estimate following Scully et al.
df = df[-which(df$author == 'CARB' & df$year == 2019),]

# add column for ANL vs other
df['source'] = ifelse(df$author %in% c('Wang', 'Dunn', 'ANL', 'ICF', 'USDA'), 'GREET', 'Other')

# if study reports both iLUC and dLUC estimates but not a total dLuc estimate, create one and return it
df = do.call(rbind.data.frame, lapply(split(df, df$study_num), function(df){
  
  if(is.element('iLUC', df$luc_type) & is.element('dLUC', df$luc_type) & !is.element('LUC', df$luc_type)){
    
    # create an LUC entry with all other attributes 
    luc_out = df[1,]
    luc_out['estimate'] = sum(df$estimate)
    luc_out['luc_type'] = 'LUC'
    luc_out['notes'] = 'calculated as the sum of study reported iLUC and dLUC estimates'
    luc_out['model'] = df$model[which(df$luc_type == 'iLUC')] # by default, report the iLUC model used
    
    return(rbind(df, luc_out))
    
  } else{
    
    return(df)
    
  }
  
}))

rownames(df) = NULL

# subset to just include total LUC estimates
luc_df = df[df$luc_type == 'LUC',]

#====================================================================================================
# to create plot showing all data, splice two plots together to include high Searchinger value

# add n = 1 for all points such that they fall in the same boxplot
luc_df['n'] = rep(1, nrow(luc_df))

# create plot 1 truncated to a max CI of 35 gCO2e/MJ
p1 = ggplot(luc_df, aes(x = n, y=estimate, group = n, color = source)) +
  scale_colour_manual(values = c("#eca14d", "#096264"), name="Source Framework", labels =  c("GREET", "Other"))+
  geom_boxplot(size = 1, show.legend = FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2), size = 10, alpha = 0.4, show.legend = FALSE)+
  geom_point(aes(x = 1, y = 3.9), size = 10, colour = 'red')+
  geom_text(aes(x = 1.25, y = 3.9, label = 'Scully et al. (2021)'),hjust=0.5,vjust=0, size = 3, color = 'red')+
  theme_classic()+
  scale_y_continuous(limits = c(0, 35), expand = c(0, 2))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        axis.line.x = element_line(colour = 'black', size = 1),
        text=element_text(size=14, family = 'Helvetica-Narrow'))+
  ylab(expression(paste("Total Land Use Change Carbon Intensity ", (gCO[2]*e~MJ^-1))))+
  coord_flip(ylim = c(-2, 35), xlim = c(0.5, 2.5), expand = F)

# annotate plot 1
p1 = p1 + annotate('segment', x = 1.6, y = min(na.omit(luc_df$estimate)), xend = 1.6, yend = max(p1$coordinates$limits$y), 
                   arrow = arrow(ends = "first", angle = 90, length = unit(.2,"cm")), 
                   size = 1) +
  annotate('curve', x = 1.6, y = mean(c(min(na.omit(luc_df$estimate)), max(p1$coordinates$limits$y))), xend = 1.45, yend = 3.9,
                   #arrow = arrow(ends = "last", angle = 30, length = unit(.5,"cm"), type = 'closed'), 
                   size = 1, linetype = 'dotted') +
  annotate('segment', x = 1.6, y = 4.5, xend = 1.45, yend = 3.9,
           arrow = arrow(ends = "last", angle = 30, length = unit(.5,"cm"), type = 'closed'), 
           size = 1) +
  annotate('text', x = 1.6*1.05, y = mean(c(min(na.omit(luc_df$estimate)), max(p1$coordinates$limits$y))), 
                   label = "Range of the estimates reviewed by Scully et al. (2021)", vjust = 0, hjust = 0)

# create plot 2 truncated to to a CI from 99 to 111 gCO2e/MJ
p2  = ggplot(luc_df, aes(x = n, y=estimate, group = n, color = source)) +
  scale_colour_manual(values = c("#eca14d", "#096264"), name="Source Framework", labels =  c("GREET", "Other"))+
  geom_jitter(shape=16, position=position_jitter(0.2), size = 10, alpha = 0.4)+
  theme_classic()+
  expand_limits(y = 0)+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        legend.title = element_text(face="bold"),
        axis.line.x = element_line(colour = 'black', size = 1),
        text=element_text(size=14, family = 'Helvetica-Narrow'))+
  scale_y_continuous(breaks = seq(0, 110, by = 10), seq(0, 110, by = 10))+
  coord_flip(ylim = c(99, 111), xlim = c(0.5, 2.5), expand = F)

# annotate plot 2
p2 = p2 + annotate('segment', x = 1.6, y = min(p2$coordinates$limits$y), xend = 1.6, yend = max(na.omit(luc_df$estimate)), 
                   arrow = arrow(ends = "last", angle = 90, length = unit(.2,"cm")), 
                   size = 1)

# combine p1 and p2 such that they appear to be a single figure
comb = ggarrange(p1,  p2, ncol = 2, widths = c(3, 1.1), align = 'hv')


# save figure as pdf
pdf('Figure1.pdf', height = 2.75, width = 11.5, family = 'Helvetica-Narrow')
print(comb)
dev.off()
getwd()
