## neonate validation study
library(plyr)
library(dplyr)
library(rms)
library(ggplot2)
library(tidyr)
library(cowplot)
library(MyersMisc)
library(pROC)
theme_set(theme_bw(base_size = 10))

# load model and data
load("neonateModel.rda")
load("newborn_training.Rda")
neonate.1 = readRDS('newborn.1.rds')
neonate.2 = readRDS('newborn.2.rds')
neonate.24 = readRDS('newborn.24.rds')

neonate_model = f.final
neonate_training = training
rm(f.final, training)

# 629 recruited to model development study (612 ears in final dataset after removal of ineligible and missing data)
# Total number recruited = 753
# therefore, number of subjects in validataion study = 124
# include both ears in validation study
# number of ears with missing data = 248-238 = 10
# number of ears in validation set after removing ears with missing data = 238

neonate.1 = neonate.1[order(neonate.1$id.res),]
# remove subjects that were in the training study (629 * 2 = 1258)
neonate_validation = neonate.1[-c(1:1258),] # 124 neonates in validation sample
summary(neonate_validation$tymp) # CNT = 1
summary(neonate_validation$dpoae) # cnt = 0
summary(neonate_validation$abs250) # cnt = 10

neonate_dems = neonate_validation %>% 
  distinct(id.res, .keep_all = T)

summary(neonate_dems$gender)  
summary(neonate_dems$ga)
summary(neonate_dems$type.of.birth)
summary(neonate_dems$birth.weight)
summary(neonate_dems$maternal.ethnicity)
summary(neonate_dems$age)

neonate_validation.final = drop_na(neonate_validation, c(tymp, dpoae, abs250:pha8000))

## reference tests
summary(neonate_validation.final$tymp)
neonate_validation.final$tymp = droplevels(neonate_validation.final$tymp)
summary(neonate_validation.final$dpoae)
neonate_validation.final$dpoae = droplevels(neonate_validation.final$dpoae)
summary(neonate_validation.final$aabr)

# create reference standard 
neonate_validation.final$rs <- with(neonate_validation.final, 
                                    ifelse(tymp=="pass" & dpoae=="pass", "Pass", 
                                    ifelse(tymp=="refer" | dpoae=="refer", "Refer", NA)))
neonate_validation.final$rs = as.factor(neonate_validation.final$rs)
str(neonate_validation.final$rs)

# now plot train and test at 24 oct freq res
## make reference standard for 24 octave data
## include both ears of the training sample subjects even though only one ear was used to develop model
summary(neonate.24$tymp)
summary(neonate.24$dpoae)
neonate.24$rs <- with(neonate.24, 
                      ifelse(tymp=="pass" & dpoae=="pass", "Pass", 
                      ifelse(tymp=="refer" | dpoae=="refer", "Refer", NA)))
neonate.24$rs = as.factor(neonate.24$rs)

neonate_train.24 = neonate.24[1:1258,]
neonate_test.24 = neonate.24[-c(1:1258),]

neonate_train.24 = drop_na(neonate_train.24, c(rs, abs226:pha8000))
neonate_test.24 = drop_na(neonate_test.24, c(rs, abs226:pha8000))

neonate_train.24$rs = revalue(neonate_train.24$rs, c('Pass'='Pass', 'Refer'='Fail'))
neonate_test.24$rs = revalue(neonate_test.24$rs, c('Pass'='Pass', 'Refer'='Fail'))

# median pass/fail training and test sets
wai.names = c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

abs.train.24 = select(neonate_train.24, rs, starts_with('abs'))
abs.test.24 = select(neonate_test.24, rs, starts_with('abs'))
colnames(abs.train.24) = wai.names
colnames(abs.test.24) = wai.names

mag.train.24 = select(neonate_train.24, rs, starts_with('mag'))
mag.test.24 = select(neonate_test.24, rs, starts_with('mag'))
colnames(mag.train.24) = wai.names
colnames(mag.test.24) = wai.names

pha.train.24 = select(neonate_train.24, rs, starts_with('pha'))
pha.test.24 = select(neonate_test.24, rs, starts_with('pha'))
colnames(pha.train.24) = wai.names
colnames(pha.test.24) = wai.names

abs.train.24$sample = 'Myers et al. (2018a)'
abs.test.24$sample = 'Validation sample'

mag.train.24$sample = 'Myers et al. (2018a)'
mag.test.24$sample = 'Validation sample'

pha.train.24$sample = 'Myers et al. (2018a)'
pha.test.24$sample = 'Validation sample'

abs.24 = rbind.data.frame(abs.train.24, abs.test.24)
mag.24 = rbind.data.frame(mag.train.24, mag.test.24)
pha.24 = rbind.data.frame(pha.train.24, pha.test.24)

abs.24 <- group_by(abs.24, sample, rs)
mag.24 <- group_by(mag.24, sample, rs)
pha.24 <- group_by(pha.24, sample, rs)

abs.median <- summarise_all(abs.24, funs(median))
mag.median <- summarise_all(mag.24, funs(median))
pha.median <- summarise_all(pha.24, funs(median))

abs.median.long <- gather(abs.median, Frequency, absorbance, 3:109)
mag.median.long <- gather(mag.median, Frequency, magnitude, 3:109)
pha.median.long <- gather(pha.median, Frequency, phase, 3:109)

abs.median.long$Frequency = as.numeric(abs.median.long$Frequency)
mag.median.long$Frequency = as.numeric(mag.median.long$Frequency)
pha.median.long$Frequency = as.numeric(pha.median.long$Frequency)

abs.median.plot <- ggplot(abs.median.long, aes(x=Frequency, y=absorbance, colour=rs, linetype=sample)) +
  geom_line()  +
  scale_color_manual(values=c("#00BA38", "#F8766D")) +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme_bw() +
  theme(legend.position = 'none') +
  theme(plot.margin=unit(c(5,5,5,5),"mm"))
print(abs.median.plot)

mag.median.plot <- ggplot(mag.median.long, aes(x=Frequency, y=magnitude, colour=rs, linetype=sample)) +
  geom_line()  +
  scale_color_manual(values=c("#00BA38", "#F8766D")) +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|,", " mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 1, 2, 3, 4), limits=c(0, 4)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(5,5,5,5),"mm"))

pha.median.plot <- ggplot(pha.median.long, aes(x=Frequency, y=phase, colour=rs, linetype=sample)) +
  geom_line()  +
  scale_color_manual(values=c("#00BA38", "#F8766D")) +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 30, 60, 90), limits=c(0, 90)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme_bw() +
  theme(legend.position = 'none') +
  theme(plot.margin=unit(c(5,5,5,5),"mm"))
print(pha.median.plot)

neonate_median_plots <- plot_grid(abs.median.plot, mag.median.plot, pha.median.plot, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C")) 
ggsave("neonate_median_plots.jpeg", neonate_median_plots, height=9, width=6, dpi=500)

# Model predictions on the test set
pred.test <- predict(neonate_model, neonate_validation.final, type="fitted")
roc.test <- roc(neonate_validation.final$rs, pred.test, print.auc=TRUE, ci=TRUE, plot=TRUE, print.thres = c(0.1088, 0.2076, 0.3716, 0.4599882, 0.6, 0.7), col="purple", 
                main="External Validation") # 0.12 is where se = sp, 0.32 is sp = 0.9
auc(roc.test)
# in development study apparent AUC was 0.88 and bias-corrected 0.85. This compares well with AUC of 0.84 found in validation study. 
ci.auc(roc.test)

y <- as.numeric(neonate_validation.final$rs) # needs to be numeric
y <- replace(y, y==1, 0)
y <- replace(y, y==2, 1)

jpeg("neonate_validate_calibration_plot.jpeg", width = 5, height = 5 , units = 'in', res = 500)
MyersMisc:::MyCalPlot(pred.test, y, smooth = T, logistic.cal = F, pl=T, riskdist = "predicted", statloc = T,
                      legendloc=c(0.7,0.3), cex = 1)
dev.off()
