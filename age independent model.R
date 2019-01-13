# Age independent model 6-18 months
# One ear for development - opposite ears for validation (and also bootstrap as per Myers et al. 2018a)
# WAI have developmental changes - option 1 is to create multiple models - where should the cut off be? The change is smooth
                               # - option 2 - model age - goal of this study
# ordinal reference standard
# compare with a model that doesn't include age - lowest aic wins

library(dplyr)
library(rms)
library(ggplot2)
library(tidyr)
library(cowplot)
library(MyersMisc)
library(pROC)
theme_set(theme_bw(base_size = 10))

# load model and data
six.2 = readRDS('six.2.rds')
six.24 = readRDS('six.24.rds')
twelve.2 = readRDS('twelve.2.rds')
twelve.24 = readRDS('twelve.24.rds')
eighteen.2 = readRDS('eighteen.2.rds')
eighteen.24 = readRDS('eighteen.24.rds')
ages = readRDS('ages.rds')

# Total of 710 subjects recruited at this stage
# need to create reference standard and plot median pass/fail for each age

six.2$age = 'Six months'
six.24$age = 'Six months'
twelve.2$age = 'Twelve months'
twelve.24$age = 'Twelve months'
eighteen.2$age = 'Eighteen months'
eighteen.24$age = 'Eighteen months'

# just use absorbance for this study
six.2 = select(six.2, tymp:abs8000, age)
six.24 = select(six.24, tymp:abs8000, age)
twelve.2 = select(twelve.2, tymp:abs8000, age)
twelve.24 = select(twelve.24, tymp:abs8000, age)
eighteen.2 = select(eighteen.2, tymp:abs8000, age)
eighteen.24 = select(eighteen.24, tymp:abs8000, age)

# remove NA
# get number who attended follow ups from longitudinal paper - then minus to get number of missing 
six.2.final = drop_na(six.2, c(tymp, dpoae, abs250:abs8000))
six.24.final = drop_na(six.24, c(tymp, dpoae, abs226:abs8000))
twelve.2.final = drop_na(twelve.2, c(tymp, dpoae, abs250:abs8000))
twelve.24.final = drop_na(twelve.24, c(tymp, dpoae, abs226:abs8000))
eighteen.2.final = drop_na(eighteen.2, c(tymp, dpoae, abs250:abs8000))
eighteen.24.final = drop_na(eighteen.24, c(tymp, dpoae, abs226:abs8000))

full.2 = rbind.data.frame(six.2.final, twelve.2.final, eighteen.2.final)
full.24 = rbind.data.frame(six.24.final, twelve.24.final, eighteen.24.final)

# create reference standard
full.2$rs <- with(full.2, 
                   ifelse(tymp=="pass" & dpoae=="pass", "Normal", 
                          ifelse(tymp=="type C" & dpoae=="pass", "Mild",
                                 ifelse(tymp=='type C' & dpoae=='refer', 'Mild',
                                        ifelse(tymp=="refer" & dpoae=="pass", "Mild",
                                               ifelse(tymp=="pass" & dpoae=="refer", "Mild",
                                                      ifelse(tymp=="refer" & dpoae=="refer", "Severe", NA)
                                                      )
                                               )
                                        )
                                 )
                          )
                  )
full.2$rs = factor(full.2$rs, levels=c('Normal', 'Mild', 'Severe'))
summary(full.2$rs)

full.24$rs <- with(full.24, 
                  ifelse(tymp=="pass" & dpoae=="pass", "Normal", 
                         ifelse(tymp=="type C" & dpoae=="pass", "Mild",
                                ifelse(tymp=='type C' & dpoae=='refer', 'Mild',
                                       ifelse(tymp=="refer" & dpoae=="pass", "Mild",
                                              ifelse(tymp=="pass" & dpoae=="refer", "Mild",
                                                     ifelse(tymp=="refer" & dpoae=="refer", "Severe", NA)
                                              )
                                       )
                                )
                         )
                  )
)
full.24$rs = factor(full.24$rs, levels=c('Normal', 'Mild', 'Severe'))
summary(full.24$rs)

# plot median normal/mild/severe by age
full.24 = select(full.24, abs226:rs)
colnames(full.24) = c("226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
              "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
              "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
              "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
              "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
              "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
              "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
              "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
              "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
              "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
              "7127.19", "7336.03", "7550.99", "7772.26", "8000.00", "age", "rs")
full.24 <- group_by(full.24, age, rs)
abs.median <- summarise_all(full.24, funs(median))
abs.median.long <- gather(abs.median, Frequency, absorbance, 3:109)
abs.median.long$Frequency = as.numeric(abs.median.long$Frequency)
abs.median.long$age = factor(abs.median.long$age, levels=c('Six months', 'Twelve months', 'Eighteen months'))

abs.median.plot <- ggplot(abs.median.long, aes(x=Frequency, y=absorbance, colour=rs, linetype=age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  scale_colour_manual(values = c("Pass" = "#00BA38", 
                                 "Mild" = "#619CFF", 
                                 "Severe" = "#F8766D")) +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme_bw() +
  theme(legend.title=element_blank())
print(abs.median.plot)






