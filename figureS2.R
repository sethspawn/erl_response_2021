rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "ggplot2",
  "plyr",
  "grid",
  "ggpubr",
  "lme4",
  "lmerTest"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================

# load data taken manually from Scully et al figure 2
df = read.csv('data_from_scully_fig2.csv')

# excluding 2019 CARB estimate following Scully et al.
df = df[-which(df$author == 'CARB' & df$year == 2019),]


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
# to create plot showing all data, splice two plots together to include high searchinger value

# add n = 1 for all points such that they fall in the same boxplot
luc_df['n'] = rep(1, nrow(luc_df))
luc_df['Researcher'] = ifelse(luc_df$author %in% c('Wang', 'Dunn', 'ANL', 'ICF', 'USDA'), 'ANL', 'Other')
luc_df$Researcher = ifelse(luc_df$author == 'CARB', 'CARB', luc_df$Researcher)
luc_df$Researcher = ifelse(luc_df$author %in% c('Hertel', 'Tyner', 'Taheripour'), 'Purdue', luc_df$Researcher)
luc_df$Researcher = factor(luc_df$Research, levels = c('ANL', 'CARB', 'Purdue','Other'))  # other is any researcher with no replicate studies

# create regression plot
p1 = ggplot(luc_df, aes(x = year, y = estimate, group = Researcher, fill = Researcher, colour = Researcher))+
  geom_point(size = 3, alpha = 0.7)+
  stat_smooth(data = subset(luc_df,model == 'GTAP-BIO'), aes(x = year, y = estimate, group = Researcher, fill = Researcher, colour = Researcher),
              method=lm, alpha = 0.1)+
  scale_fill_manual(values = c("#eca14d", "#da4224", "#096264","transparent"), name = 'Institute of origin', labels = c('Argonne National Lab', 'CARB', 'Purdue University', 'Other'))+
  scale_color_manual(values = c("#eca14d", "#da4224", "#096264", "#5ac1ad"), name = 'Institute of origin', labels = c('Argonne National Lab', 'CARB', 'Purdue University', 'Other'))+
  theme_bw()+
  theme(text=element_text(size=10, family = 'Helvetica-Narrow'))+
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  xlab('\nYear in which the estimate was generated')+
  ylab(expression(paste("Carbon Intensity   ", (gCO[2]*e~MJ^-1))))

# save as pdf
pdf('FigureS2.pdf', height = 3, width = 5, family = 'Helvetica-Narrow')
print(p1)
dev.off()


#====================================================================================================
# linear mixed effects models

summary(lmer(estimate ~ year + (1|Researcher), data = luc_df)) # p = 0.026
summary(lmer(estimate ~ year + (1|Researcher/author), data = luc_df)) # p = 0.059

summary(lmer(estimate ~ year*Researcher + (1|Researcher), data = luc_df)) # p = 0.948
summary(lmer(estimate ~ year*Researcher + (1|Researcher/author), data = luc_df)) # p = 0.836

summary(lmer(estimate ~ year*Researcher + (1|author), data = luc_df)) # p = 0.836

