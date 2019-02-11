# Age independent model 5-24 months
# munge and plot data----
library(tidyverse)
library(rms)
library(cowplot)
library(MyersMisc)
theme_set(theme_bw(base_size = 10))

# load model and data
six.2 = readRDS('six.2.rds')
six.24 = readRDS('six.24.rds')
twelve.2 = readRDS('twelve.2.rds')
twelve.24 = readRDS('twelve.24.rds')
eighteen.2 = readRDS('eighteen.2.rds')
eighteen.24 = readRDS('eighteen.24.rds')

six.2$age_group = '6 months'
six.24$age_group = '6 months'
twelve.2$age_group = '12 months'
twelve.24$age_group = '12 months'
eighteen.2$age_group = '18 months'
eighteen.24$age_group = '18 months'

# just use absorbance for this study
six.2 = select(six.2, tymp:abs8000, age_group)
six.24 = select(six.24, tymp:abs8000, age_group)
twelve.2 = select(twelve.2, tymp:abs8000, age_group)
twelve.24 = select(twelve.24, tymp:abs8000, age_group)
eighteen.2 = select(eighteen.2, tymp:abs8000, age_group)
eighteen.24 = select(eighteen.24, tymp:abs8000, age_group)

# need to set 12 and 18 mth cnt for tymp
twelve.2$tymp = as.character(twelve.2$tymp)
eighteen.2$tymp = as.character(eighteen.2$tymp)

twelve.24$tymp = as.character(twelve.24$tymp)
eighteen.24$tymp = as.character(eighteen.24$tymp)

twelve.2$tymp = ifelse(twelve.2$dpoae=='pass' & is.na(twelve.2$tymp), 'CNT',
                       ifelse(twelve.2$dpoae=='refer' & is.na(twelve.2$tymp), 'CNT',
                              ifelse(twelve.2$dpoae=='CNT' & is.na(twelve.2$tymp), 'CNT', twelve.2$tymp)
                              )
                       )
  
eighteen.2$tymp = ifelse(eighteen.2$dpoae=='pass' & is.na(eighteen.2$tymp), 'CNT',
                       ifelse(eighteen.2$dpoae=='refer' & is.na(eighteen.2$tymp), 'CNT',
                              ifelse(eighteen.2$dpoae=='CNT' & is.na(eighteen.2$tymp), 'CNT', eighteen.2$tymp)
                       )
)

twelve.24$tymp = twelve.2$tymp
eighteen.24$tymp = eighteen.24$tymp

twelve.24$tymp = ifelse(twelve.24$dpoae=='pass' & is.na(twelve.24$tymp), 'CNT',
                       ifelse(twelve.24$dpoae=='refer' & is.na(twelve.24$tymp), 'CNT',
                              ifelse(twelve.24$dpoae=='CNT' & is.na(twelve.24$tymp), 'CNT', twelve.24$tymp)
                       )
)

eighteen.24$tymp = ifelse(eighteen.24$dpoae=='pass' & is.na(eighteen.24$tymp), 'CNT',
                         ifelse(eighteen.24$dpoae=='refer' & is.na(eighteen.24$tymp), 'CNT',
                                ifelse(eighteen.24$dpoae=='CNT' & is.na(eighteen.24$tymp), 'CNT', eighteen.24$tymp)
                         )
)

twelve.2$tymp = factor(twelve.2$tymp, levels = c('pass', 'refer', 'type C', 'CNT'))
eighteen.2$tymp = factor(eighteen.2$tymp, levels = c('pass', 'refer', 'type C', 'CNT'))

twelve.24$tymp = factor(twelve.24$tymp, levels = c('pass', 'refer', 'type C', 'CNT'))
eighteen.24$tymp = factor(eighteen.24$tymp, levels = c('pass', 'refer', 'type C', 'CNT'))

tymp.results.6 = summary(six.2$tymp) # haven't created type C yet
dpoae.results.6 = summary(six.2$dpoae)

tymp.results.12 = summary(twelve.2$tymp)
dpoae.results.12 = summary(twelve.2$dpoae)

tymp.results.18 = summary(eighteen.2$tymp)
dpoae.results.18 = summary(eighteen.2$dpoae)

# remove subjects who didn't attend follow up appointments
six.2.final = drop_na(six.2, c(tymp, dpoae)) 
six.24.final = drop_na(six.24, c(tymp, dpoae))
twelve.2.final = drop_na(twelve.2, c(tymp, dpoae)) 
twelve.24.final = drop_na(twelve.24, c(tymp, dpoae))
eighteen.2.final = drop_na(eighteen.2, c(tymp, dpoae)) 
eighteen.24.final = drop_na(eighteen.24, c(tymp, dpoae))

# tymp and dpoae results (including CNT)
summary(six.2.final$tymp)
summary(six.2.final$dpoae)
summary(six.2.final$abs250)

summary(twelve.2.final$tymp)
summary(twelve.2.final$dpoae)
summary(twelve.2.final$abs250)

summary(eighteen.2.final$tymp)
summary(eighteen.2.final$dpoae)
summary(eighteen.2.final$abs250)

# calculate number of infants who attended each follow up 
six.mth.subjects = length(unique(six.2.final$id.res)) # 271 infants attended 6-mth review
twelve.mth.subjects = length(unique(twelve.2.final$id.res)) # 202 infants attended 12-mth review
eighteen.mth.subjects = length(unique(eighteen.2.final$id.res)) # 126 subjects attended 18-mth review

# demographics of unique infants who attended follow up
unique.dems = rbind(six.2.final, twelve.2.final, eighteen.2.final)
unique.dems = unique.dems %>%
  distinct(id.res, .keep_all = T)
summary(unique.dems$gender)
summary(unique.dems$maternal.ethnicity)

# number of CNT
tymp.cnt.6 = filter(six.2.final, tymp=='CNT') # CNT 18 ears
dpoae.cnt.6 = filter(six.2.final, dpoae=='CNT') # CNT 22 ears
wai.cnt.6 = filter(six.2.final, is.na(abs250)) # CNT 27 ears

tymp.cnt.12 = filter(twelve.2.final, tymp=='CNT') # CNT 0 ears
dpoae.cnt.12 = filter(twelve.2.final, dpoae=='CNT') # CNT 12 ears
wai.cnt.12 = filter(twelve.2.final, is.na(abs250)) # CNT 16 ears

tymp.cnt.18 = filter(eighteen.2.final, tymp=='CNT') # CNT 0 ears
dpoae.cnt.18 = filter(eighteen.2.final, dpoae=='CNT') # CNT 8 ears
wai.cnt.18 = filter(eighteen.2.final, is.na(abs250)) # CNT 9 ears

# number of ears in sample
# drop cnt and NA
levels(six.2.final$tymp)[levels(six.2.final$tymp)=='CNT'] = NA
levels(six.2.final$dpoae)[levels(six.2.final$dpoae)=='CNT'] = NA
levels(twelve.2.final$tymp)[levels(twelve.2.final$tymp)=='CNT'] = NA
levels(twelve.2.final$dpoae)[levels(twelve.2.final$dpoae)=='CNT'] = NA
levels(eighteen.2.final$tymp)[levels(eighteen.2.final$tymp)=='CNT'] = NA
levels(eighteen.2.final$dpoae)[levels(eighteen.2.final$dpoae)=='CNT'] = NA

levels(six.24.final$tymp)[levels(six.24.final$tymp)=='CNT'] = NA
levels(six.24.final$dpoae)[levels(six.24.final$dpoae)=='CNT'] = NA
levels(twelve.24.final$tymp)[levels(twelve.24.final$tymp)=='CNT'] = NA
levels(twelve.24.final$dpoae)[levels(twelve.24.final$dpoae)=='CNT'] = NA
levels(eighteen.24.final$tymp)[levels(eighteen.24.final$tymp)=='CNT'] = NA
levels(eighteen.24.final$dpoae)[levels(eighteen.24.final$dpoae)=='CNT'] = NA

six.2.final = drop_na(six.2.final, c(tymp, dpoae, abs250:abs8000)) # 508 ears
six.24.final = drop_na(six.24.final, c(tymp, dpoae, abs226:abs8000))
twelve.2.final = drop_na(twelve.2.final, c(tymp, dpoae, abs250:abs8000)) # 330 ears
twelve.24.final = drop_na(twelve.24.final, c(tymp, dpoae, abs226:abs8000))
eighteen.2.final = drop_na(eighteen.2.final, c(tymp, dpoae, abs250:abs8000)) # 200 ears
eighteen.24.final = drop_na(eighteen.24.final, c(tymp, dpoae, abs226:abs8000))

# age mean and range for each age group
six.2.final$age[six.2.final$age < 1] = NA
twelve.2.final$age[twelve.2.final$age < 1] = NA
eighteen.2.final$age[eighteen.2.final$age < 40] = NA

six.age = six.2.final %>% 
  distinct(id.res, .keep_all = T) 
summary(six.age$age)

twelve.age = twelve.2.final %>% 
  distinct(id.res, .keep_all = T) 
summary(twelve.age$age)

eighteen.age = eighteen.2.final %>% 
  distinct(id.res, .keep_all = T) 
summary(eighteen.age$age)

# make tymp with negative peak 'type C' for 6 mths
summary(six.2.final$tymp)
# can't have NA in tpp = set to 0
six.2.final$tpp[is.na(six.2.final$tpp)] = 0
six.2.final = six.2.final %>% 
  mutate(tymp = if_else(tpp < (-150), 'type C', if_else(tymp=='refer', 'refer', 'pass') ))
  
six.2.final$tymp = factor(six.2.final$tymp, levels = c('pass', 'type C', 'refer'))
summary(six.2.final$tymp)
summary(six.2.final$dpoae)

summary(twelve.2.final$tymp)
summary(twelve.2.final$dpoae)

summary(eighteen.2.final$tymp)
summary(eighteen.2.final$dpoae)

# create df with all ages
full.2 = rbind.data.frame(six.2.final, twelve.2.final, eighteen.2.final)
full.24 = rbind.data.frame(six.24.final, twelve.24.final, eighteen.24.final)

# ages of infants
# some obviously wrong values - set to missing
full.2$age[full.2$age < 1] = NA
full.2$age[full.2$id.res == 36 & full.2$age_group == '18 months'] = NA
full.2$age[full.2$id.res == 581 & full.2$age_group == '18 months'] = NA
full.2$age_group = factor(full.2$age_group, levels = c('6 months', '12 months', '18 months'))

summary(full.2$age) # 54 ears missing age - impute with median for their age group
age.stats = full.2 %>% 
  group_by(age_group) %>%
  summarise(min = min(age, na.rm = TRUE), 
            first.quartile = quantile(age, 0.25, na.rm = TRUE), 
            median = median(age, na.rm = TRUE), 
            third.quartile = quantile(age, 0.75, na.rm = TRUE), 
            max = max(age, na.rm = TRUE))
age.stats # median, IQR and range of age for each age group

# impute age NAs with the median for their age group (6mth = 28, 12mth = 54.4, 18mth = 80.6)
impute.median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
full.2 = full.2 %>%
  group_by(age_group) %>%
  mutate(age = impute.median(age))

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

# RS results by age_group
number.ears.by.age = full.2 %>% 
  group_by(age_group) %>% 
  tally() # after rm missing - 508 ears in 6mth, 330 ears in 12mth, 200 ears in 18mth.

rs.by.age = full.2 %>%
  group_by(age_group, rs) %>%
  tally()

tymp.by.age = full.2 %>%
  group_by(age_group, tymp) %>%
  tally()

dpoae.by.age = full.2 %>%
  group_by(age_group, dpoae) %>%
  tally()

# plot mean normal/mild/severe by age
abs.24 = select(full.24, abs226:rs)
colnames(abs.24) = c("226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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
abs.24 <- group_by(abs.24, age, rs)
abs.mean <- summarise_all(abs.24, funs(mean))
abs.mean.long <- gather(abs.mean, Frequency, absorbance, 3:109)
abs.mean.long$Frequency = as.numeric(abs.mean.long$Frequency)
abs.mean.long$age = factor(abs.mean.long$age, levels=c('6 months', '12 months', '18 months'))
abs.mean.long$absorbance[abs.mean.long$absorbance < 0] = 0

abs.mean.plot <- ggplot(abs.mean.long, aes(x=Frequency, y=absorbance, colour=rs, linetype=age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  scale_colour_manual(values = c("Normal" = "#00BA38", 
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
print(abs.mean.plot)
#ggsave("mean.plot.jpeg", abs.mean.plot, height=4, width=7.5, dpi=500)

# RS all possible
rs.all.names = c("Pass both, n = 769", "Type A and fail DPOAEs, n = 35", "Type C and pass DPOAEs, n = 21", 
                 "Type C and fail DPOAEs, n = 6", "Type B and pass DPOAEs, n = 56", "Fail both, n = 151")

rs.all.possible.df = full.24 # this is the development sample
rs.all.possible.df$rs.all.possible = with(rs.all.possible.df, 
                                          ifelse(tymp=="pass" & dpoae=="pass", "Pass both, n = 769", 
                                                 ifelse(tymp=="pass" & dpoae=="refer", "Type A and fail DPOAEs, n = 35",
                                                        ifelse(tymp=='type C' & dpoae=='pass', 'Type C and pass DPOAEs, n = 21',
                                                               ifelse(tymp=="type C" & dpoae=="refer", "Type C and fail DPOAEs, n = 6",
                                                                      ifelse(tymp=="refer" & dpoae=="pass", "Type B and pass DPOAEs, n = 56",
                                                                             ifelse(tymp=="refer" & dpoae=="refer", "Fail both, n = 151", NA)
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )

rs.all.possible.df$rs.all.possible = factor(rs.all.possible.df$rs.all.possible, levels=rs.all.names)
summary(rs.all.possible.df$rs.all.possible) 

# Group by rs and create mean
rs.all.possible2 = select(rs.all.possible.df, abs226:abs8000, rs.all.possible)
rs.all.possible2 <- group_by(rs.all.possible2, rs.all.possible)
colnames(rs.all.possible2) = c("226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
                              "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
                              "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
                              "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
                              "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
                              "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
                              "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
                              "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
                              "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
                              "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
                              "7127.19", "7336.03", "7550.99", "7772.26", "8000.00", "rs.all.possible")
abs.median <- summarise_all(rs.all.possible2, funs(median)) # use median because some groups are small
abs.median <- gather(abs.median, Frequency, absorbance, 2:108)
abs.median$absorbance[abs.median$absorbance < 0] = 0
# label with the numbers in each group
abs.rs.all.plot <- ggplot(abs.median) +
  theme_bw() +
  scale_colour_manual(values = c("Pass both, n = 769" = "#00BA38", 
                                 "Type A and fail DPOAEs, n = 35" = "#B79F00", 
                                 "Type C and pass DPOAEs, n = 21" = "#00BFC4", 
                                 "Type C and fail DPOAEs, n = 6" = "#619CFF", 
                                 "Type B and pass DPOAEs, n = 56" = "#F564E3", 
                                 "Fail both, n = 151" = "#F8766D")) +
  geom_line(aes(x = as.numeric(Frequency), y = absorbance, group=rs.all.possible, colour = rs.all.possible))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.title=element_blank() ) +
  #legend.position=c(0.02,0.98)
  #legend.text=element_text(size=12), 
  theme(legend.position="right")
print(abs.rs.all.plot)

# Multiplot
abs.plots <- plot_grid(abs.mean.plot, abs.rs.all.plot, nrow=2, ncol=1, align = "v", labels = c("A", "B"))
ggsave("abs.infant.plots.jpeg", abs.plots, height=6, width=8, dpi=500)

# Create the 90% normal range (1/24 octave) for each age group for the example plots and app
# 6 mth
norm.6 = filter(full.24, age_group == '6 months')
norm.6 = select(norm.6, abs226:abs8000, rs)
norm.6 <- group_by(norm.6, rs)
abs.median <- summarise_all(norm.6, funs(median))
abs.05 <- summarise_all(norm.6, funs(quantile(., probs = (0.05))))
abs.95 <- summarise_all(norm.6, funs(quantile(., probs = (0.95))))
abs.90 <- rbind(abs.median, abs.05, abs.95)
abs.90 <- data.frame(abs.90)
abs.colnames = c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
                 "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
                 "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
                 "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
                 "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
                 "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
                 "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
                 "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
                 "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
                 "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
                 "7127.19", "7336.03", "7550.99", "7772.26", "8000.00")
colnames(abs.90) <- abs.colnames
stats.col <- c("median", "median", "median", "five", "five", "five", "ninety5", "ninety5", "ninety5")
abs.90 <- cbind.data.frame(abs.90, stats.col)
abs.90.long.6 <- gather(abs.90, Frequency, absorbance, 2:108)
abs.90.long.6$absorbance[abs.90.long.6$absorbance < 0] = 0
abs.90.long.6 <- spread(abs.90.long.6, stats.col, absorbance)
abs.90.long.6$Frequency <- as.numeric(abs.90.long.6$Frequency)
abs.90.long.6$rs = as.character(abs.90.long.6$rs)

# filter normal
abs.90.long.6 = filter(abs.90.long.6, rs == 'Normal')

# plot to check it looks right
abs.90.plot <- ggplot(abs.90.long.6, aes(x=Frequency, y=median, ymin=five, ymax=ninety5)) +
  geom_ribbon(linetype=0, alpha = 0.4) +
  scale_fill_grey(start=0.6, end=0.2) +
  geom_line(size=0.8)  +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
#theme(legend.position="none")
print(abs.90.plot)

# save 90% range for the app
# saveRDS(abs.90.long.6, "sixMth90range.rds")

# 12 mths
norm.12 = filter(full.24, age_group == '12 months')
norm.12 = select(norm.12, abs226:abs8000, rs)
norm.12 <- group_by(norm.12, rs)
abs.median <- summarise_all(norm.12, funs(median))
abs.05 <- summarise_all(norm.12, funs(quantile(., probs = (0.05))))
abs.95 <- summarise_all(norm.12, funs(quantile(., probs = (0.95))))
abs.90 <- rbind(abs.median, abs.05, abs.95)
abs.90 <- data.frame(abs.90)
colnames(abs.90) <- abs.colnames
stats.col <- c("median", "median", "median", "five", "five", "five", "ninety5", "ninety5", "ninety5")
abs.90 <- cbind.data.frame(abs.90, stats.col)
abs.90.long.12 <- gather(abs.90, Frequency, absorbance, 2:108)
abs.90.long.12$absorbance[abs.90.long.12$absorbance < 0] = 0
abs.90.long.12 <- spread(abs.90.long.12, stats.col, absorbance)
abs.90.long.12$Frequency <- as.numeric(abs.90.long.12$Frequency)
abs.90.long.12$rs = as.character(abs.90.long.12$rs)

# filter normal
abs.90.long.12 = filter(abs.90.long.12, rs == 'Normal')

# plot to check it looks right
abs.90.plot <- ggplot(abs.90.long.12, aes(x=Frequency, y=median, ymin=five, ymax=ninety5)) +
  geom_ribbon(linetype=0, alpha = 0.4) +
  scale_fill_grey(start=0.6, end=0.2) +
  geom_line(size=0.8)  +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
#theme(legend.position="none")
print(abs.90.plot)

# save 90% range for the app
# saveRDS(abs.90.long.12, "twelveMth90range.rds")

# 18 mths
norm.18 = filter(full.24, age_group == '18 months')
norm.18 = select(norm.18, abs226:abs8000, rs)
norm.18 <- group_by(norm.18, rs)
abs.median <- summarise_all(norm.18, funs(median))
abs.05 <- summarise_all(norm.18, funs(quantile(., probs = (0.05))))
abs.95 <- summarise_all(norm.18, funs(quantile(., probs = (0.95))))
abs.90 <- rbind(abs.median, abs.05, abs.95)
abs.90 <- data.frame(abs.90)
colnames(abs.90) <- abs.colnames
stats.col <- c("median", "median", "median", "five", "five", "five", "ninety5", "ninety5", "ninety5")
abs.90 <- cbind.data.frame(abs.90, stats.col)
abs.90.long.18 <- gather(abs.90, Frequency, absorbance, 2:108)
abs.90.long.18$absorbance[abs.90.long.18$absorbance < 0] = 0
abs.90.long.18 <- spread(abs.90.long.18, stats.col, absorbance)
abs.90.long.18$Frequency <- as.numeric(abs.90.long.18$Frequency)
abs.90.long.18$rs = as.character(abs.90.long.18$rs)

# filter normal
abs.90.long.18 = filter(abs.90.long.18, rs == 'Normal')

# plot to check it looks right
abs.90.plot <- ggplot(abs.90.long.18, aes(x=Frequency, y=median, ymin=five, ymax=ninety5)) +
  geom_ribbon(linetype=0, alpha = 0.4) +
  scale_fill_grey(start=0.6, end=0.2) +
  geom_line(size=0.8)  +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
#theme(legend.position="none")
print(abs.90.plot)

# save 90% range for the app
# saveRDS(abs.90.long.18, "eighteenMth90range.rds")

# modeling
# calibration plot function for the test set----
my.val.prob = function (p, y, logit, group, weights = rep(1, length(y)), normwt = FALSE, 
                        pl = TRUE, smooth = TRUE, logistic.cal = TRUE, xlab = "Predicted Probability", 
                        ylab = "Actual Probability", lim = c(0, 1), m, g, cuts, emax.lim = c(0, 
                                                                                             1), legendloc = lim[1] + c(0.55 * diff(lim), 0.27 * diff(lim)), 
                        statloc = c(0, 0.99), riskdist = NULL, 
                        cex = 0.7, mkh = 0.02, connect.group = FALSE, connect.smooth = TRUE, 
                        g.group = 4, evaluate = 100, nmin = 0) 
{
  if (missing(p)) 
    p <- plogis(logit)
  else logit <- qlogis(p)
  if (length(p) != length(y)) 
    stop("lengths of p or logit and y do not agree")
  names(p) <- names(y) <- names(logit) <- NULL
  riskdist <- match.arg(riskdist)
  Spi <- function(p, y) {
    z <- sum((y - p) * (1 - 2 * p))/sqrt(sum((1 - 2 * p) * 
                                               (1 - 2 * p) * p * (1 - p)))
    P <- 2 * pnorm(-abs(z))
    c(Z = z, P = P)
  }
  if (!missing(group)) {
    if (length(group) == 1 && is.logical(group) && group) 
      group <- rep("", length(y))
    if (!is.factor(group)) 
      group <- if (is.logical(group) || is.character(group)) 
        as.factor(group)
    else cut2(group, g = g.group)
    names(group) <- NULL
    nma <- !(is.na(p + y + weights) | is.na(group))
    ng <- length(levels(group))
  }
  else {
    nma <- !is.na(p + y + weights)
    ng <- 0
  }
  logit <- logit[nma]
  y <- y[nma]
  p <- p[nma]
  if (ng > 0) {
    group <- group[nma]
    weights <- weights[nma]
    return(val.probg(p, y, group, evaluate, weights, normwt, 
                     nmin))
  }
  if (length(unique(p)) == 1) {
    P <- mean(y)
    Intc <- qlogis(P)
    n <- length(y)
    D <- -1/n
    L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
    L.cal <- -2 * sum(y * Intc - logb(1 + exp(Intc)), na.rm = TRUE)
    U.chisq <- L01 - L.cal
    U.p <- 1 - pchisq(U.chisq, 1)
    U <- (U.chisq - 1)/n
    Q <- D - U
    spi <- unname(Spi(p, y))
    stats <- c(0, 0.5, 0, D, 0, 1, U, U.chisq, U.p, Q, mean((y - 
                                                               p[1])^2), Intc, 0, 0, 0, rep(abs(p[1] - P), 2), spi)
    names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                      "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                      "Slope", "Emax", "E90", "Eavg", "S:z", "S:p")
    return(stats)
  }
  i <- !is.infinite(logit)
  nm <- sum(!i)
  if (nm > 0) 
    warning(paste(nm, "observations deleted from logistic calibration due to probs. of 0 or 1"))
  f.fixed <- lrm.fit(logit[i], y[i], initial = c(0, 1), maxit = 1L)
  f.recal <- lrm.fit(logit[i], y[i])
  stats <- f.fixed$stats
  n <- stats["Obs"]
  predprob <- seq(emax.lim[1], emax.lim[2], by = 5e-04)
  Sm <- lowess(p, y, iter = 0)
  cal.smooth <- approx(Sm, xout = p, ties = mean)$y
  er <- abs(p - cal.smooth)
  eavg <- mean(er)
  emax <- max(er)
  e90 <- unname(quantile(er, 0.9))
  if (pl) {
    plot(0.5, 0.5, xlim = lim, ylim = lim, type = "n", xlab = xlab, 
         ylab = ylab)
    abline(0, 1, lty = 2, lwd = 1, col = 'black')
    lt <- 2
    leg <- "Ideal"
    marks <- -1
    lwd <- 1
    col <- 'black'
    if (logistic.cal) {
      lt <- c(lt, 1)
      leg <- c(leg, "Logistic calibration")
      lwd <- c(lwd, 1)
      col <- c(col, "black")
      marks <- c(marks, -1)
    }
    if (smooth) {
      if (connect.smooth) {
        lines(Sm, lty = 1)
        lt <- c(lt, 3)
        lwd <- c(lwd, 1)
        col <- c(col, "black")
        marks <- c(marks, -1)
      }
      else {
        points(Sm)
        lt <- c(lt, 0)
        lwd <- c(lwd, 2)
        col <- c(col, "black")
        marks <- c(marks, 1)
      }
      leg <- c(leg, "Actual")
    }
    if (!missing(m) | !missing(g) | !missing(cuts)) {
      if (!missing(m)) 
        q <- cut2(p, m = m, levels.mean = TRUE, digits = 7)
      else if (!missing(g)) 
        q <- cut2(p, g = g, levels.mean = TRUE, digits = 7)
      else if (!missing(cuts)) 
        q <- cut2(p, cuts = cuts, levels.mean = TRUE, 
                  digits = 7)
      means <- as.numeric(levels(q))
      prop <- tapply(y, q, function(x) mean(x, na.rm = TRUE))
      points(means, prop, pch = 2)
      if (connect.group) {
        lines(means, prop)
        lt <- c(lt, 1)
      }
      else lt <- c(lt, 0)
      leg <- c(leg, "Grouped observations")
      col <- c(col, "black")
      lwd <- c(lwd, 1)
      marks <- c(marks, 2)
    }
  }
  lr <- stats["Model L.R."]
  p.lr <- stats["P"]
  D <- (lr - 1)/n
  L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
  U.chisq <- L01 - f.recal$deviance[2]
  p.U <- 1 - pchisq(U.chisq, 2)
  U <- (U.chisq - 2)/n
  Q <- D - U
  Dxy <- stats["Dxy"]
  C <- stats["C"]
  R2 <- stats["R2"]
  B <- mean((p - y)^2)
  spi <- unname(Spi(p, y))
  stats <- c(Dxy, C, R2, D, lr, p.lr, U, U.chisq, p.U, Q, B, 
             f.recal$coef, emax, e90, eavg, spi)
  names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                    "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                    "Slope", "Emax", "E90", "Eavg", "S:z", "S:p")
  if (pl) {
    logit <- seq(-7, 7, length = 200)
    prob <- plogis(logit)
    pred.prob <- f.recal$coef[1] + f.recal$coef[2] * logit
    pred.prob <- plogis(pred.prob)
    if (logistic.cal) 
      lines(prob, pred.prob, lty = 1)
    lp <- legendloc
    if (!is.logical(lp)) {
      if (!is.list(lp)) 
        lp <- list(x = lp[1], y = lp[2])
      legend(lp, leg, lty = c(2, 1), pch = marks, cex = cex, 
             lwd = lwd, col = col, bty = "n")
    }
    if (!is.logical(statloc)) {
      dostats <- c("Dxy", "C (ROC)", "R2", "D", "U", "Q", 
                   "Brier", "Intercept", "Slope", "Emax", "E90", 
                   "Eavg", "S:z", "S:p")
      leg <- format(names(stats)[dostats])
      leg <- paste(leg, ":", format(stats[dostats]), sep = "")
      if (!is.list(statloc)) 
        statloc <- list(x = statloc[1], y = statloc[2])
      text(statloc, paste(format(names(stats[dostats])), 
                          collapse = "\n"), adj = c(0, 1), cex = cex)
      text(statloc$x + 0.225 * diff(lim), statloc$y, paste(format(round(stats[dostats], 
                                                                        3)), collapse = "\n"), adj = c(1, 1), cex = cex)
    }
    if (is.character(riskdist)) {
      if (riskdist == "calibrated") {
        x <- f.recal$coef[1] + f.recal$coef[2] * qlogis(p)
        x <- plogis(x)
        x[p == 0] <- 0
        x[p == 1] <- 1
      }
      else x <- p
      bins <- seq(lim[1], lim[2], length = 101)
      x <- x[x >= lim[1] & x <= lim[2]]
      f <- table(cut(x, bins))
      j <- f > 0
      bins <- (bins[-101])[j]
      f <- f[j]
      f <- lim[1] + 0.15 * diff(lim) * f/max(f)
      segments(bins, 0, bins, f)
    }
  }
  stats
}

## need a train and validation sample (one ear)----
# make df of ears that only have results for one ear
full.2 = select(full.2, age:rs)
full.2$ear.id = 1:1038
full.2 = group_by(full.2, age_group, id.res)
training = sample_n(full.2, 1)
testing = full.2[-training$ear.id,]
training <- ungroup(training)
testing <- ungroup(testing)

# check assumption of ordinality 
# The solid lines are the simple stratified means, and the dashed lines are the expected values if the assumption of proportional odds is met. 
#ord.plot = plot.xmean.ordinaly(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, cr=F, topcats=2, subn = F, data = training)

jpeg("ordinality.plots.jpeg", width = 9, height = 6 , units = 'in', res = 500)
par(mfrow=c(2,3))
par(mar=c(5,4,1,1))
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs1000, cr=F, topcats=2, subn = F, data = full.2, xlab = "Reference Standard", ylab = "Absorbance 1000 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs1414, cr=F, topcats=2, subn = F, data = full.2, xlab = "Reference Standard", ylab = "Absorbance 1414 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs2000, cr=F, topcats=2, subn = F, data = full.2, xlab = "Reference Standard", ylab = "Absorbance 2000 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs2828, cr=F, topcats=2, subn = F, data = full.2, xlab = "Reference Standard", ylab = "Absorbance 2828 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs4000, cr=F, topcats=2, subn = F, data = full.2, xlab = "Reference Standard", ylab = "Absorbance 4000 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs5657, cr=F, topcats=2, subn = F, data = full.2, xlab = "Reference Standard", ylab = "Absorbance 5657 Hz")
dev.off()


# modeling----
set.seed(17)
train.dd <- datadist(training)
options(datadist="train.dd")

label(training$abs1000) <- "Absorbance 1000 Hz"
label(training$abs1414) <- "Absorbance 1414 Hz"
label(training$abs2000) <- "Absorbance 2000 Hz"
label(training$abs2828) <- "Absorbance 2828 Hz"
label(training$abs4000) <- "Absorbance 4000 Hz"
label(training$abs5657) <- "Absorbance 5657 Hz"
label(training$age) <- "Age"
units(training$age) <- "weeks"

gamma = function (x, model.name='model.name') {
  lrchi2 = x$stats[3]
  df = x$stats[4]
  gamma = (lrchi2 - df) / lrchi2
  names(gamma) = model.name
  gamma
}

base_model <- lrm(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = training, x = T, y = T)
base_model.r = robcov(base_model, training$id.res) 
base_aic = AIC(base_model.r)
base_gamma = gamma(base_model.r, "base model")

base_model.nl <- lrm(rs ~ rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs2828, 5) + rcs(abs4000, 5) + rcs(abs5657, 5), 
                     data = training, x = T, y = T)
base_model.nl.r = robcov(base_model.nl, training$id.res) 
base_aic.nl = AIC(base_model.nl.r)
base_gamma.nl = gamma(base_model.nl.r, "base NL model")

age_model = lrm(rs ~ age * (abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657), data = training, x = T, y = T)
age_model.r = robcov(age_model, training$id.res) 
age_aic = AIC(age_model.r)
age_gamma = gamma(age_model.r, "age model")

age_model.nl = lrm(rs ~ age * (rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs2828, 5) + rcs(abs4000, 5) + rcs(abs5657, 5)), 
                   data = training, x = T, y = T)
age_model.nl.r = robcov(age_model.nl, training$id.res) 
age_aic.nl = AIC(age_model.nl.r)
age_nl.gamma = gamma(age_model.nl.r, "age NL model")

# interaction = age * (3 and 4kHz)
age_model.semi_nl = lrm(rs ~ rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs5657, 5) + age * (rcs(abs4000, 5) + rcs(abs2828)), 
                        data = training, x = T, y = T)
age_model.semi_nl.r = robcov(age_model.semi_nl, training$id.res) 
age_aic.semi_nl = AIC(age_model.semi_nl.r)
age_semi.nl.gamma = gamma(age_model.semi_nl.r, "age semiNL model")

# or remove 3 and 4 kHz
base_model.no34k = lrm(rs ~ rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs5657, 5), 
                      data = training, x = T, y = T)
base_model.no34k.r = robcov(base_model.no34k, training$id.res) 
base_aic.no34k = AIC(base_model.no34k.r)
base_no34k.gamma = gamma(base_model.no34k.r, "base model NL no34k")

# choose best model
aics = rbind(base_aic, base_aic.nl, age_aic, age_aic.nl, age_aic.semi_nl, base_aic.no34k) 
gammas = rbind(base_gamma, base_gamma.nl, age_gamma, age_nl.gamma, age_semi.nl.gamma, base_no34k.gamma)

final_model = base_model.no34k.r

# save equation and model
options(prType = 'plain') # change to "latex" if want latex output
latex(final_model, file = "") # replace "abs" with: \textit{A}  and paste into Rsweave file then compile pdf
saveRDS(final_model, 'InfantModel.rds')

# explore/interpret final model 
anova(final_model)
plot(anova(final_model, vnames='labels'), 'chisq', rm.totals = T)

plot(Predict(final_model, fun = plogis, kint = 1))
plot(Predict(final_model, fun = plogis, kint = 2))

val <- validate(final_model, B=500)
full <- val[[1]]
train <- val[[12]]
test <- val[[23]]
## write a function to convert Dxy to AUC
dxy.to.auc <- function(x) {0.5*(x+1)}

#use fn to convert dxy to auc for full model, train and test sets
auc.full <- dxy.to.auc(full)
auc.train <- dxy.to.auc(train)
auc.test <- dxy.to.auc(test)
opt <- auc.train - auc.test
opt.cor <- auc.full - opt
auc.df <- c(auc.full, auc.train, auc.test, opt, opt.cor)
auc.df.names <- c("full", "train", "test", "opt", "opt.cor")
auc.res <- cbind.data.frame(auc.df.names, auc.df)

cal1 = calibrate(final_model, B = 500, kint = 1) # calibrate for Y >= Mild
# it wouldn't let me set riskdist to "F" so I used the scat1d.opts - and set to 0 to supress the distribution of predictions in margin
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")
cal2 = calibrate(final_model, B = 500, kint = 2) # calibrate for Y >= Severe
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")

# validate with opposite ears
pred.opp_ears = as.data.frame(predict(final_model, testing, type = 'fitted'))
mild.preds = pred.opp_ears[,1] # select predictions for >= Mild
y.mild = as.numeric(testing$rs)
y.mild <- replace(y.mild, y.mild==1, 0) # make normal 0, diseased 1
y.mild <- replace(y.mild, y.mild==2, 1) 
y.mild <- replace(y.mild, y.mild==3, 1) # set severe to mild 
mild.stats = val.prob(mild.preds, y.mild) # get the stats
cal.plot.test.mild = my.val.prob(mild.preds, y.mild, logistic.cal = F, statloc = F)

val.prob(mild.preds, y.mild, logistic.cal = F) # get c-index (>= mild)

severe.preds = pred.opp_ears[,2] # select predictions for >= Mild
y.severe = as.numeric(testing$rs)
y.severe <- replace(y.severe, y.severe==1, 0) # make normal 0, diseased 1
y.severe <- replace(y.severe, y.severe==2, 0) # set mild to normal 
y.severe <- replace(y.severe, y.severe==3, 1) 
severe.stats = val.prob(severe.preds, y.severe) # get the stats
cal.plot.test.severe = my.val.prob(severe.preds, y.severe, logistic.cal = F, statloc = F)

val.prob(severe.preds, y.severe, logistic.cal = F) # get c-index for >=severe

tiff("calPlot.jpeg", width = 10, height = 10, units = 'in', res = 500)
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE), widths = c(5,5), heights = c(5,5))
par(mar=c(4, 5, 1, 1))
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, legend = F, xlab = "Predicted Probability", ylab = "Actual Probability")
legend("bottomright", inset = 0.05, box.lty=0, legend=c("Ideal", "Apparent", "Bias-corrected"),
       col=c("Black", "black", "black"), lty=c(2,3,1), cex=1)
mtext("A", 2, adj = 4.5, las = 1, padj = -12.4, font = 2, cex = 1.3)
par(mar=c(4, 5, 1, 2))
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, legend = F, xlab = "Predicted Probability", ylab = "Actual Probability")
legend("bottomright", inset = 0.05, box.lty=0, legend=c("Ideal", "Apparent", "Bias-corrected"),
       col=c("Black", "black", "black"), lty=c(2,3,1), cex=1)
mtext("B", 2, adj = 4.5, las = 1, padj = -12.4, font = 2, cex = 1.3)
par(mar=c(5, 5, 1, 1))
my.val.prob(mild.preds, y.mild, logistic.cal = F, statloc = F, cex = 1, legendloc = c(0.65, 0.15))
mtext("C", 2, adj = 4.5, las = 1, padj = -12.4, font = 2, cex = 1.3)
par(mar=c(5, 5, 1, 2))
my.val.prob(severe.preds, y.severe, logistic.cal = F, statloc = F, cex = 1, legendloc = c(0.65, 0.15))
mtext("D", 2, adj = 4.5, las = 1, padj = -12.4, font = 2, cex = 1.3)
dev.off()

# predict class membership in the test set
pred.ind = predict(final_model, testing, type = "fitted.ind")
pred.ind = as.data.frame(round(pred.ind, digits =2))
# use some function to select the class with the highest prob (drop the other 2)

max.pred = as.data.frame(max.col(pred.ind, ties.method = "last"))
summary(as.factor(max.pred$`max.col(pred.ind, ties.method = "last")`))

max.pred <- max.col(pred.ind, "last")
value <- pred.ind[cbind(1:nrow(pred.ind), max.pred)]
cluster <- names(pred.ind)[max.pred]
pred <- data.frame(value, cluster)
pred$cluster = as.character(pred$cluster)
pred$cluster[pred$cluster == "rs=Normal"] = "Normal"
pred$cluster[pred$cluster == "rs=Mild"] = "Mild"
pred$cluster[pred$cluster == "rs=Severe"] = "Severe"
pred$cluster = factor(pred$cluster, levels = c("Normal", "Mild", "Severe"))

## compare to rs label 
pred.compare = cbind(pred, testing$rs)

cont.tab = table(pred.compare$cluster, pred.compare$`testing$rs`) # columns are the original label (the truth), and rows are the max predictions

# what is the median and range of predictions?
mean = pred.compare %>% 
  group_by(cluster) %>% 
  summarise(mean = mean(value)) 
mean

min = pred.compare %>% 
  group_by(cluster) %>% 
  summarise(min = min(value)) 
min

max = pred.compare %>% 
  group_by(cluster) %>% 
  summarise(max = max(value)) 
max

# examples
id = "192" 
ear.side = "L"
age.group = "18 months"

eg1 = filter(full.2, id.res==id, ear==ear.side, age_group==age.group) 
eg.prob.ind1 = round(predict(final_model, eg1, type = "fitted.ind"), 2)
eg.prob.ind1
eg.prob.fit1 = round(predict(final_model, eg1, type = "fitted"), 2)
eg.mild = eg.prob.fit1[1]
eg.severe = eg.prob.fit1[2]
prob.ind.norm1 = eg.prob.ind1[1]
prob.ind.mild1 = eg.prob.ind1[2]
prob.ind.sev1 = eg.prob.ind1[3]
prob.fit.mild1 = eg.prob.fit1[1]
prob.fit.sev1 = eg.prob.fit1[2]
eg.abs1 = filter(full.24, id.res==id, ear==ear.side, age_group==age.group) 
eg.abs1 = dplyr::select(eg.abs1, abs226:abs8000)
freq.num = c(226.00, 257.33, 280.62, 297.30, 324.21, 343.49, 363.91, 385.55, 408.48, 432.77, 458.50,
             471.94, 500.00, 514.65, 545.25, 561.23, 577.68, 594.60, 629.96, 648.42, 667.42, 686.98,
             707.11, 727.83, 749.15, 771.11, 793.70, 816.96, 840.90, 865.54, 890.90, 917.00, 943.87,
             971.53, 1000.00, 1029.30, 1059.46, 1090.51, 1122.46, 1155.35, 1189.21, 1224.05, 1259.92, 
             1296.84, 1334.84, 1373.95, 1414.21, 1455.65, 1498.31, 1542.21, 1587.40, 1633.92, 1681.79,
             1731.07, 1781.80, 1834.01, 1887.75, 1943.06, 2000.00, 2058.60, 2118.93, 2181.02, 2244.92,
             2310.71, 2378.41, 2448.11, 2519.84, 2593.68, 2669.68, 2747.91, 2828.43, 2911.31, 2996.61,
             3084.42, 3174.80, 3267.83, 3363.59, 3462.15, 3563.59, 3668.02, 3775.50, 3886.13, 
             4000.00, 4117.21, 4237.85, 4362.03, 4489.85, 4621.41, 4756.83, 4896.21, 5039.68, 5187.36,
             5339.36, 5495.81, 5656.85, 5822.61, 5993.23, 6168.84, 6349.60, 6535.66, 6727.17, 6924.29,
             7127.19, 7336.03, 7550.99, 7772.26, 8000.00)
names(eg.abs1) = freq.num
eg.abs.long1 <- gather(eg.abs1, Frequency, Absorbance, 1:107)
eg.abs.long1$Frequency = as.numeric(eg.abs.long1$Frequency)
eg.abs.long1$Absorbance[eg.abs.long1$Absorbance < 0] = 0
# Right ear 23-mth male with type B and passed DPOAE
eg.plot.1 = ggplot(eg.abs.long1) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long1, colour="blue") +
  geom_ribbon(data=abs.90.long.18, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.90), label = ("Left ear of a 18-month-old male"), hjust = 0) +
  annotate("text", x = 250, y = c(0.80), label = paste("ME \u2265 mild", eg.mild), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.70), label = paste("ME \u2265 severe", eg.severe), parse=F, hjust=0) 
  #annotate("text", x = 250, y = c(0.60), label = paste("Normal = ",  prob.ind.norm1), parse=F, hjust=0) +
  #annotate("text", x = 250, y = c(0.50), label = paste("Mild = ",  prob.ind.mild1), parse=F, hjust=0) +
  #annotate("text", x = 250, y = c(0.40), label = paste("Severe = ",  prob.ind.sev1), parse=F, hjust=0) 
eg.plot.1

ggsave("eg.plot.jpeg", eg.plot.1, height=4, width=6, dpi=500)

# RCS knot locations - 0.05, 0.275, 0.5, 0.725, and 0.95 percentiles
knots1k = quantile(training$abs1000, probs = c(0.05, 0.275, 0.5, 0.725, 0.95))
knots1.4k = quantile(training$abs1414, probs = c(0.05, 0.275, 0.5, 0.725, 0.95))
knots2k = quantile(training$abs2000, probs = c(0.05, 0.275, 0.5, 0.725, 0.95))
knots6k = quantile(training$abs5657, probs = c(0.05, 0.275, 0.5, 0.725, 0.95))
