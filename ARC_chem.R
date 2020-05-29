# read in data from Shaver et al 2010
# https://arc-lter.ecosystems.mbl.edu/2003-2009gsharvest

library(tidyr)
library(dplyr)

df <- read.csv('/Users/wwieder/Desktop/Working_files/NEON/outside_Data/2003-2009gsharvest.csv', header = T)
names(df)

df = na_if(df, '#N/A')
df$FUNCTIONAL.TYPE[df$FUNCTIONAL.TYPE=='GRM'] = 'G'
df$FUNCTIONAL.TYPE[df$FUNCTIONAL.TYPE=='G '] = 'G'
levels(df$FUNCTIONAL.TYPE)
df %>% filter(TREATMENT !='Fert')
levels(df$TREATMENT)
# convert strings to numeric
df$SLA = as.numeric(as.character(df$SLA))
df$CN = as.numeric(as.character(df$C.N.RATIO))
df$C = as.numeric(as.character(df$CARBON))

mean(df$C[df$FUNCTIONAL.TYPE=='G'], na.rm=T)
length(df$CN[df$FUNCTIONAL.TYPE=='G' & df$TREATMENT !='Fert'])
# looks like functional type D is shrubs (non-ericoid)
(df$SPECIES[df$FUNCTIONAL.TYPE=='P'])

# print mean C:N and SLA from control plots
hist(df$CN[df$FUNCTIONAL.TYPE=='G' & df$TREATMENT !='Fert'])
mean(df$CN[df$FUNCTIONAL.TYPE=='G' & df$TREATMENT !='Fert'], na.rm=T)
sd(df$CN[df$FUNCTIONAL.TYPE=='G' & df$TREATMENT !='Fert'], na.rm=T)
mean(df$CN[df$FUNCTIONAL.TYPE=='D' & df$TREATMENT !='Fert'], na.rm=T)
sd(df$CN[df$FUNCTIONAL.TYPE=='D' & df$TREATMENT !='Fert'], na.rm=T)
length((df$CN[df$FUNCTIONAL.TYPE=='G' & df$TREATMENT !='Fert']))

hist((df$SLA[df$FUNCTIONAL.TYPE=='G'  & df$SLA>0]* 1e-3 / 0.5))
SLA = (df$SLA[df$FUNCTIONAL.TYPE=='G'])
logSLA = log(SLA[SLA>0])
exp(mean(logSLA))* 1e-3 / 0.5

# for all plants
print(levels(df$FUNCTIONAL.TYPE))
mean(df$SLA[df$SLA>0], 
     na.rm=T) * 1e-3 / 0.5

mean(df$SLA[df$FUNCTIONAL.TYPE!='P' & df$SLA>0], 
     na.rm=T) * 1e-3 / 0.5
sd(df$SLA[df$FUNCTIONAL.TYPE=='G' & df$SLA>0], 
   na.rm=T) * 1e-3 / 0.5
mean(df$SLA[df$FUNCTIONAL.TYPE=='D' & df$SLA>0], 
     na.rm=T) * 1e-3 / 0.5
sd(df$SLA[df$FUNCTIONAL.TYPE=='D' & df$SLA>0], 
   na.rm=T) * 1e-3 / 0.5

SLA_C = df$SLA[df$C>0] / df$C[df$C>0] * 1e-3 
FUNCTIONAL.TYPE = df$FUNCTIONAL.TYPE[df$C>0]
mean(SLA_C[FUNCTIONAL.TYPE=='G'], na.rm=T) 

