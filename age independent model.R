# Age independent model 6-18 months
# One ear for development - opposite ears for validation (and also bootstrap as per Myers et al. 2018a)
# WAI have developmental changes - option 1 is to create multiple models - where should the cut off be? The change is smooth
                               # - option 2 - model age - goal of this study

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

six.2$age_group = 'Six months'
six.24$age_group = 'Six months'
twelve.2$age_group = 'Twelve months'
twelve.24$age_group = 'Twelve months'
eighteen.2$age_group = 'Eighteen months'
eighteen.24$age_group = 'Eighteen months'

# just use absorbance for this study
six.2 = select(six.2, tymp:abs8000, age_group)
six.24 = select(six.24, tymp:abs8000, age_group)
twelve.2 = select(twelve.2, tymp:abs8000, age_group)
twelve.24 = select(twelve.24, tymp:abs8000, age_group)
eighteen.2 = select(eighteen.2, tymp:abs8000, age_group)
eighteen.24 = select(eighteen.24, tymp:abs8000, age_group)

# remove subjects who didn't attend
six.2.final = drop_na(six.2, c(tymp, dpoae)) 
six.24.final = drop_na(six.24, c(tymp, dpoae))
twelve.2.final = drop_na(twelve.2, c(tymp, dpoae)) 
twelve.24.final = drop_na(twelve.24, c(tymp, dpoae))
eighteen.2.final = drop_na(eighteen.2, c(tymp, dpoae)) 
eighteen.24.final = drop_na(eighteen.24, c(tymp, dpoae))

# calculate number of infants who attended each follow up 
six.mth.subjects =  length(unique(six.2.final$id.res)) # 271 infatns attended 6-mth review
twelve.mth.subjects = length(unique(twelve.2.final$id.res)) # 180 infants attended 12-mth review
eighteen.mth.subjects = length(unique(eighteen.2.final$id.res)) # 110 subjects attended 18-mth review

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

# make tymp with negative peak 'type C' for 6 mths
summary(six.2.final$tymp)
# can't have NA in tpp = set to 0
six.2.final$tpp[is.na(six.2.final$tpp)] = 0
six.2.final = six.2.final %>% 
  mutate(tymp = if_else(tpp < (-150), 'type C', if_else(tymp=='refer', 'refer', 'pass') ))
  
six.2.final$tymp = factor(six.2.final$tymp, levels = c('pass', 'type C', 'refer'))
summary(six.2.final$tymp)

# create df with all ages
full.2 = rbind.data.frame(six.2.final, twelve.2.final, eighteen.2.final)
full.24 = rbind.data.frame(six.24.final, twelve.24.final, eighteen.24.final)

summary(full.2$age) # 42 missing age - impute with median for their age group
full.2$age[full.2$age == 0] = NA

## also need to check age for outliers - there are a couple of 18 mths with age 29 and 33 weeks - these are wrong

# calculate age median, IQR and range for each age group
# use dplyr group by for this


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
print(abs.median.plot)

# modeling
## need a train and validation sample (one ear)
# make df of ears that only have results for one ear
full.2 = select(full.2, age:rs)
full.2$ear.id = 1:1059
full.2 = group_by(full.2, age_group, id.res)
training = sample_n(full.2, 1)
testing = full.2[-training$ear.id,]
training <- ungroup(training)
testing <- ungroup(testing)

# check assumption of ordinality 
# The solid lines are the simple stratified means, and the dashed lines are the expected values if the assumption of proportional odds is met. 
#ord.plot = plot.xmean.ordinaly(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, cr=F, topcats=2, subn = F, data = training)

#jpeg("fig.2.ord.plots.jpeg", width = 9, height = 6 , units = 'in', res = 500)
#par(mfrow=c(2,3))
#par(mar=c(5,4,2,2)+0.1)
#MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs1000, cr=F, topcats=2, subn = F, data = abs.2, xlab = "Reference Standard", ylab = "Absorbance 1000 Hz")
#MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs1414, cr=F, topcats=2, subn = F, data = abs.2, xlab = "Reference Standard", ylab = "Absorbance 1414 Hz")
#MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs2000, cr=F, topcats=2, subn = F, data = abs.2, xlab = "Reference Standard", ylab = "Absorbance 2000 Hz")
#MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs2828, cr=F, topcats=2, subn = F, data = abs.2, xlab = "Reference Standard", ylab = "Absorbance 2828 Hz")
#MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs4000, cr=F, topcats=2, subn = F, data = abs.2, xlab = "Reference Standard", ylab = "Absorbance 4000 Hz")
#MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs5657, cr=F, topcats=2, subn = F, data = abs.2, xlab = "Reference Standard", ylab = "Absorbance 5657 Hz")
#dev.off()

train.dd <- datadist(training)
options(datadist="train.dd")
base_model <- lrm(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = training, x = T, y = T)
base_model.r = robcov(base_model, training$id.res) 
base_model.r                              
base_aic = AIC(base_model.r)
base_aic
base_gamma = (259.40 - 6) / 259.40

age_model = lrm(rs ~ age * (abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657), data = training, x = T, y = T)
age_model.r = robcov(age_model, training$id.res) 
age_model.r                              
age_aic = AIC(age_model.r)
age_aic

# shrinkage coefficient for age model
age_gamma = (290.83 - 20) / 290.83
plot(anova(age_model.r))

options(prType = 'plain') # change to "latex" if want latex output
latex(age_model.r, file = "") # replace "abs" with: \textit{A}  and paste into Rsweave file then compile pdf
saveRDS(age_model.r, 'AgeModel.rds')

pred <- predict(age_model.r, type = "fitted")

validate(age_model.r, B = 500)
cal1 = calibrate(age_model.r, B = 500, kint = 1) # calibrate for Y >= Mild
# it wouldn't let me set riskdist to "F" so I used the scat1d.opts - and set to 0 to supress the distribution of predictions in margin
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")
cal2 = calibrate(age_model.r, B = 500, kint = 2) # calibrate for Y >= Severe
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")

# validate opposite ears
pred.opp_ears = as.data.frame(predict(age_model.r, testing, type = 'fitted'))
mild.preds = pred.opp_ears[,1] # select predictions for >= Mild
y.mild = as.numeric(testing$rs)
y.mild <- replace(y.mild, y.mild==1, 0) # make normal 0, diseased 1
y.mild <- replace(y.mild, y.mild==2, 1) 
y.mild <- replace(y.mild, y.mild==3, 1) # set severe to mild 
cal.plot.test.mild = val.prob(mild.preds, y.mild)

severe.preds = pred.opp_ears[,2] # select predictions for >= Mild
y.severe = as.numeric(testing$rs)
y.severe <- replace(y.severe, y.severe==1, 0) # make normal 0, diseased 1
y.severe <- replace(y.severe, y.severe==2, 0) # set mild to normal 
y.severe <- replace(y.severe, y.severe==3, 1) 
cal.plot.test.severe = val.prob(severe.preds, y.severe)


plot(Predict(base_model.r, fun=plogis, kint = 1))
plot(Predict(base_model.r, fun=plogis, kint = 2))

## try to plot the age * freq interaction for the most important??? see neonate article

##########################
# up to here
##########################


jpeg("fig.3.cal.plots.jpeg", width = 10, height = 5 , units = 'in', res = 500)
par(mar=c(5,5,2,2)+0.1)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(5,5), heights = c(5))
#Mild Plot
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, legend = F, xlab = "Predicted Probability", ylab = "Actual Probability")
legend("bottomright", inset = 0.05, box.lty=0, legend=c("Apparent", "Bias-corrected", "Ideal"),
       col=c("Black", "black", "black"), lty=c(3,1,2), cex=0.8)
mtext("A", 2, adj = 5, las = 1, padj = -10.4, font = 2, cex = 1.3)
#Severe Plot
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, legend = F, xlab = "Predicted Probability", ylab = "Actual Probability")
legend("bottomright", inset = 0.05, box.lty=0, legend=c("Apparent", "Bias-corrected", "Ideal"),
       col=c("Black", "black", "black"), lty=c(3,1,2), cex=0.8)
mtext("B", 2, adj = 5.5, las = 1, padj = -10.4, font = 2, cex = 1.3)
dev.off()

n <- sum(r$freq)
quantile(predict(r, type='fitted'), c(100/n, 1-100/n), na.rm=TRUE)

# resid(b, 'score.binary' , pl=TRUE) # they look ok - not too far from horizontal dashed line
# resid(b, 'partial' , pl=TRUE , label.curves=FALSE)
#plot(Predict(b, fun = plogis))

# library(mRMRe) # use this for auc because proc uses a different method than rms 
# rs.ordered = ordered(training$rs)
# auc.train = correlate(rs.ordered, pred.train.penal, "cindex")
# auc.train # this is the same as rms!

# pred.test.penal <- predict(f, testing, type="lp")
# # roc.test.penal <- multiclass.roc(testing$rs, pred.test.penal) 
# # roc.test.penal 
# 
# rs.ordered.test = ordered(testing$rs)
# auc.test = correlate(rs.ordered.test, pred.test.penal, "cindex")
# auc.test 

plot(Predict(r, fun = plogis,  kint = 1))
plot(Predict(r, fun = plogis, kint = 2))

val <- validate(r, B=500)
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

# y <- as.numeric(df$rs) # needs to be numeric
# y <- replace(y, y==1, 0)
# y <- replace(y, y==2, 1)
# cal.plot <- MyCalPlot(b.pred, y, smooth = T, logistic.cal = F, pl=T, riskdist = "predicted", statloc = F, 
#                       legendloc=c(0.8,0.1), cex = 1)

# predict class membership
pred.ind = predict(r, type = "fitted.ind")
pred.ind = as.data.frame(round(pred.ind, digits =2))
# use some function to select the class with the highest prob (drop the other 2)

max.pred = as.data.frame(max.col(pred.ind, ties.method = "last"))
summary(as.factor(max.pred$`max.col(pred.ind, ties.method = "last")`))

max.pred <- max.col(pred.ind, "last")
value <- pred.ind[cbind(1:nrow(pred.ind), max.pred)]
cluster <- names(pred.ind)[max.pred]
pred <- data.frame(value, cluster)
pred$cluster = as.character(pred$cluster)
pred$cluster[pred$cluster == "rs=Pass"] = "Pass"
pred$cluster[pred$cluster == "rs=Mild"] = "Mild"
pred$cluster[pred$cluster == "rs=Severe"] = "Severe"
pred$cluster = factor(pred$cluster, levels = c("Pass", "Mild", "Severe"))

# report the class and prob on the graph in the app

## compare to rs label 
pred.compare = cbind(pred, abs.2$rs)

cont.tab = table(pred.compare$cluster, pred.compare$`abs.2$rs`) # columns are the original label (the truth), and rows are the max predictions
# explore any that the model said was normal, but RS said Severe

# what is the median and range of predictions?
detach(package:plyr)
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

# predict class membership P >= j|X using 0.5 cutoff
pred.fit = predict(r, type = "fitted")
pred.fit = as.data.frame(round(pred.fit, digits =2))
names(pred.fit) = c("mild", "severe")
pred.fit$rs = abs.2$rs
pred.fit$label = NA

pred.fit$label = with(pred.fit, 
                      ifelse(severe > 0.5, "Severe",
                             ifelse(severe < 0.5 & mild < 0.5, "Normal", "Mild")))
pred.fit$label = factor(pred.fit$label, levels = c('Normal', 'Mild', 'Severe'))

cont.tab2 = table(pred.fit$label, pred.fit$rs) # columns are the original label (the truth), and rows are the max predictions

# example
eg1 = filter(abs.2, sub.id==534, ear=="L") 
eg.prob.ind1 = round(predict(r, eg1, type = "fitted.ind"), 2)
eg.prob.ind1
eg.prob.fit1 = round(predict(r, eg1, type = "fitted"), 2)
eg.prob.fit1
prob.ind.norm1 = eg.prob.ind1[1]
prob.ind.mild1 = eg.prob.ind1[2]
prob.ind.sev1 = eg.prob.ind1[3]
prob.fit.mild1 = eg.prob.fit1[1]
prob.fit.sev1 = eg.prob.fit1[2]
eg.abs1 = filter(abs.24, sub.id==534, ear=="L") 
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

eg.plot.1 = ggplot(eg.abs.long1) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long1, colour="blue") +
  geom_ribbon(data=abs.90.long, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.90), label = ("Left ear of a 55-week-old male"), hjust = 0) +
  annotate("text", x = 250, y = c(0.80), label = ("'ME' >= ~'mild'~ 0.73"), parse=TRUE, hjust=0) +
  annotate("text", x = 250, y = c(0.70), label = ("'ME' >= ~'severe'~ 0.26"), parse=TRUE, hjust=0) +
  annotate("text", x = 250, y = c(0.60), label = paste("Normal = ",  prob.ind.norm1), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.50), label = paste("Mild = ",  prob.ind.mild1), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.40), label = paste("Severe = ",  prob.ind.sev1), parse=F, hjust=0) 
eg.plot.1

eg2 = filter(abs.2, sub.id==416, ear=="R") 
eg.prob.ind2 = round(predict(r, eg2, type = "fitted.ind"), 2)
eg.prob.ind2
prob.ind.sev2 = eg.prob.ind2[3]
eg.abs2 = filter(abs.24, sub.id==416, ear=="R") 
eg.abs2 = dplyr::select(eg.abs2, abs226:abs8000)
names(eg.abs2) = freq.num
eg.abs.long2 <- gather(eg.abs2, Frequency, Absorbance, 1:107)
eg.abs.long2$Frequency = as.numeric(eg.abs.long2$Frequency)

eg.plot.2 = ggplot(eg.abs.long2) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long2, colour="red") +
  geom_ribbon(data=abs.90.long, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.90), label = ("Right ear of a 53-week-old male"), hjust = 0) +
  annotate("text", x = 250, y = c(0.80), label = paste("Severe = ",  prob.ind.sev2), parse=F, hjust=0) 

eg.plot.2

eg.plots <- plot_grid(eg.plot.1, eg.plot.2, nrow=2, ncol=1, align = "v", labels = c("A", "B"))
ggsave("fig.4.eg.plots.jpeg", eg.plots, height=6, width=6, dpi=500)

# Demographics model
f.dem <- lrm(rs ~ ear + gender + ethnicity + abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = abs.2, x = T, y = T)
r.dem = robcov(f.dem, abs.2$sub.id) 
r.dem                  
gamma.dem = (302.03 - 9) / 302.03
aic.dem = AIC(r.dem) # model without dems has lower AIC so use that one

# nonlinear model
f.nl <- lrm(rs ~ rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs2828, 5) + rcs(abs4000, 5) + rcs(abs5657, 5), data = abs.2, x = T, y = T)
r.nl = robcov(f.nl, abs.2$sub.id) 
r.nl                                
gamma.nl = (324.50 - 24) / 324.50
aic.nl = AIC(r.nl)





