rm(list = ls())

#====================================================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "lme4",
  "lmerTest"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

#====================================================================================================
# source formatted cclub data
source('cclub_formatData.R')

#====================================================================================================
# assess statistical difference between classes of each scenario with lineaer mixed effects models

# iterate through all scenarios, create LME model and bootstrapped confidence intervals 
lmes = lapply(1:nrow(scen_key), function(set, df, scen_key){
  
  # create sub dataframe with just cropland, cropland-pasture, and grassland data for plotting
  df_sub = data.frame(fips = rep(df$'_FIPS', 3),
                      scn = c(rep('Cropland', nrow(df)), rep('Pasture', nrow(df)), rep('Cropland-Pasture', nrow(df))),
                      val = c(df[,scen_key$s_Cropland[set]], df[,scen_key$s_Pasture[set]], df[,scen_key$s_CroplandPasture[set]]))
  
  # adjust data type for lmer()
  df_sub$fips = as.factor(df_sub$fips)
  df_sub$scn = as.factor(df_sub$scn)
  df_sub$val = as.numeric(as.character(df_sub$val)) # convert to numeric
  
  # set comparinsons relative to cropland-pasture. (i.e. "are cropland and/or pasture the same as cropland-pasture?")
  df_sub$scn = factor(df_sub$scn, levels = c("Cropland-Pasture", "Cropland", "Pasture"))
  
  #------------------------------------------------------------------------------------
  # get mixed effect model (treating county as a random effect; i.e. repeated measures)
  m = lmer(val ~ scn + (1|fips), df_sub)
  
  # get fixed effects as a table
  tab = data.frame(coef(summary(m))) 
  names = rownames(tab)
  
  # extract fixed effect estimates
  estms = tab$Estimate
  estms = data.frame(estms)
  rownames(estms) = names
  
  #------------------------------------------------------------------------------------
  # boot strap a 95% CI; join to fixed effect estimates
  ci = data.frame(confint(m, method = 'boot', level = 0.95))
  estms = merge(estms, ci, by = 0)
  rownames(estms) <- estms$Row.names; estms$Row.names <- NULL
  colnames(estms) = c('mean', 'p025','p975')
  
  #------------------------------------------------------------------------------------
  # create summary data frame with model coefficients and corresponding scenario assumptions and labels
  summaryDF = estms
  summaryDF['var'] = row.names(summaryDF)
  summaryDF = cbind(summaryDF, do.call(rbind.data.frame, lapply(1:nrow(summaryDF), function(i) scen_key[set,])))
  
  return(list('model' = m, 'confint' = estms, 'scenario' = scen_key[set,], 'summary' = summaryDF))
  
}, df = df, scen_key = scen_key)

# format for table S1
tableS1 = do.call(rbind.data.frame, lapply(lmes, function(m) m$summary))
tableS1 = tableS1[,c('Depth', 'AB', 'Tillage', 'Residue', 'var', 'mean', 'p025', 'p975')]

# assess significance (yes/no based on 95%-CI overlap with 0)
tableS1['signif'] = ifelse(sign(tableS1$p025) == sign(tableS1$p975), 'yes', 'no')

# save output as csv
write.csv(tableS1, 'tableS1.csv', row.names = F)
