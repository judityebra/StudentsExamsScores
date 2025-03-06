#LIBRARY USED
library(tidyverse)
library(readxl)
library(boot)
library(ggplot2)

# INICIALIZATION OF THE DATASET
data = read.csv("C:/Users/Judit/Desktop/uab/segon/segon_semestre/adc/prac_final/Original_data_with_more_rows.csv")

#first we will remove the id
data2 <- data[,-1]

#now we will add the three marks as a final mark and delete the others
data3 <- mutate(data2, MeanScore= rowMeans(data2[6:8]))
data3 <- data3[,-c(6,7,8)]

#now we will do the One-Hot Encoding
dummy <- dummyVars("~.", data = data3)
data4 <- data.frame(predict(dummy, newdata = data3))
head(data4)

#backward selection
# Fit a linear regression model
fit = lm(MeanScore ~ Genderfemale + Gendermale + EthnicGroupgroup.A + EthnicGroupgroup.B + 
           EthnicGroupgroup.C + EthnicGroupgroup.D + EthnicGroupgroup.E + ParentEducassociate.s.degree + ParentEducbachelor.s.degree 
         + ParentEduchigh.school +  ParentEducmaster.s.degree +  ParentEducsome.college + ParentEducsome.high.school + LunchTypefree.reduced 
         + LunchTypestandard + TestPrepcompleted + TestPrepnone, data = data4)

# Perform stepwise selection using AIC criterion
model_backward <- stepAIC(fit, trace = TRUE, direction = "backward")
summary(model_backward)



#plots
plot(resid(model_backward))

coef_data <- data.frame(
  coef_name = names(coef(fit)),
  coef_value = coef(fit)
)

ggplot(coef_data, aes(x = coef_name, y = coef_value)) +
  geom_bar(stat = "identity", fill = "hotpink") +
  xlab("Variable") +
  ylab("Coefficient Estimate") +
  ggtitle("Coefficients of Linear Regression Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot(data4$MeanScore)
hist(data4$MeanScore)

non_param_mean = function(x){
  x = mean(sample(x, size = length(x), replace = TRUE))
  return (x)
}
stats_mean = replicate(1000, non_param_mean(data4$'MeanScore'))
boxplot(stats_mean)
hist(stats_mean)

non_param_median = function(x){
  x = median(sample(x, size = length(x), replace = TRUE))
  return (x)
}
stats_median = replicate(1000, non_param_median(data4$'MeanScore'))
boxplot(stats_median)
hist(stats_median)



#NON-PARAMETRIC 
#we will take into account the parent's education (all the cases)
#SOME HIGHSCHOOL
cond1 <- data4$Genderfemale == 1
cond2 <- data4$EthnicGroupgroup.A == 1
cond3 <- data4$ParentEducsome.high.school == 1
cond4 <- data4$LunchTypefree.reduced == 1
cond5 <- data4$TestPrepcompleted == 1

#filter with the conditions
SHSMark <- data4 %>%
  filter(cond1, cond2, cond3, cond4, cond5)

#now we will do the mean with non parametric bootstrap and the IC 95%
mean_SHSMark <- boot(SHSMark, statistic = function(data, index) mean(data$MeanScore[index]), R = 1000)
print(mean_SHSMark)

SHS_IC <- boot.ci(mean_SHSMark, type = "bca", R=1000)
print(SHS_IC)
#we can see that an IC of 95% is (58, 67) approximately

#histogram to see the distribution
hist(mean_SHSMark) 

#HIGHSCHOOL
cond1 <- data4$Genderfemale == 1
cond2 <- data4$EthnicGroupgroup.A == 1
cond3 <- data4$ParentEduchigh.school == 1
cond4 <- data4$LunchTypefree.reduced == 1
cond5 <- data4$TestPrepcompleted == 1

#filter with the conditions
HSMark <- data4 %>%
  filter(cond1, cond2, cond3, cond4, cond5)

#now we will do the mean with non parametric bootstrap and the IC 95%
mean_HSMark <- boot(HSMark, statistic = function(data, index) mean(data$MeanScore[index]), R = 1000)
print(mean_HSMark)

HS_IC <- boot.ci(mean_HSMark, type = "bca", R=1000)
print(HS_IC)
#we can see that an IC of 95% is (59, 67) approximately

#SOME COLLEGE
cond1 <- data4$Genderfemale == 1
cond2 <- data4$EthnicGroupgroup.A == 1
cond3 <- data4$ParentEducsome.college == 1
cond4 <- data4$LunchTypefree.reduced == 1
cond5 <- data4$TestPrepcompleted == 1

#filter with the conditions
SCMark <- data4 %>%
  filter(cond1, cond2, cond3, cond4, cond5)

#now we will do the mean with non parametric bootstrap and the IC 95%
mean_SCMark <- boot(SCMark, statistic = function(data, index) mean(data$MeanScore[index]), R = 1000)
print(mean_SCMark)

SC_IC <- boot.ci(mean_SCMark, type = "bca", R=1000)
print(SC_IC)
#we can see that an IC of 95% is (59, 71) approximately

#ASSOCIATE'S DEGREE
cond1 <- data4$Genderfemale == 1
cond2 <- data4$EthnicGroupgroup.A == 1
cond3 <- data4$ParentEducassociate.s.degree == 1
cond4 <- data4$LunchTypefree.reduced == 1
cond5 <- data4$TestPrepcompleted == 1

#filter with the conditions
ADMark <- data4 %>%
  filter(cond1, cond2, cond3, cond4, cond5)

#now we will do the mean with non parametric bootstrap and the IC 95%
mean_ADMark <- boot(ADMark, statistic = function(data, index) mean(data$MeanScore[index]), R = 1000)
print(mean_ADMark)

AD_IC <- boot.ci(mean_ADMark, type = "bca", R=1000)
print(AD_IC)
#we can see that an IC of 95% is (65, 74) approximately

#BACHELOR'S DEGREE
cond1 <- data4$Genderfemale == 1
cond2 <- data4$EthnicGroupgroup.A == 1
cond3 <- data4$ParentEducbachelor.s.degree == 1
cond4 <- data4$LunchTypefree.reduced == 1
cond5 <- data4$TestPrepcompleted == 1

#filter with the conditions
BDMark <- data4 %>%
  filter(cond1, cond2, cond3, cond4, cond5)

#now we will do the mean with non parametric bootstrap and the IC 95%
mean_BDMark <- boot(BDMark, statistic = function(data, index) mean(data$MeanScore[index]), R = 1000)
print(mean_BDMark)

BD_IC <- boot.ci(mean_BDMark, type = "bca", R=1000)
print(BD_IC)
#we can see that an IC of 95% is (67, 76) approximately

#MASTERS DEGREE
cond1 <- data4$Genderfemale == 1
cond2 <- data4$EthnicGroupgroup.A == 1
cond3 <- data4$ParentEducmaster.s.degree == 1
cond4 <- data4$LunchTypefree.reduced == 1
cond5 <- data4$TestPrepcompleted == 1

#filter with the conditions
MDMark <- data4 %>%
  filter(cond1, cond2, cond3, cond4, cond5)

#now we will do the mean with non parametric bootstrap and the IC 95%
mean_MDMark <- boot(MDMark, statistic = function(data, index) mean(data$MeanScore[index]), R = 1000)
print(mean_MDMark)

MD_IC <- boot.ci(mean_MDMark, type = "bca", R=1000)
print(MD_IC)
#we can see that an IC of 95% is (67, 80) approximately



#PARAMETRIC BOOTSTRAP- NORMAL DISTRIBUTION

#FREE / REDUCED LUNCH TYPE
# Estimate the parameters
mean_free <- mean(data4$MeanScore[data4$LunchTypefree.reduced == 1])
std_free <- sd(data4$MeanScore[data4$LunchTypefree.reduced == 1])

# Size of the sample
n_free <- sum(data4$LunchTypefree.reduced == 1)

# Parametric bootstrap
boot_free <- rnorm(1000, mean = mean_free, sd = std_free / sqrt(n_free))

# IC 95% 
ic_free <- quantile(boot_free, c(0.025, 0.975))

print(mean_free)
print(std_free)
cat("95% Confidence Interval for FREE/REDUCED:", ic_free[1], "-", ic_free[2], "\n")
#we can see that an IC of 95% is (61.7, 62.4) approximately

#STANDARD LUNCH TYPE
mean_standard <- mean(data4$MeanScore[data4$LunchTypestandard == 1])
std_standard <- sd(data4$MeanScore[data4$LunchTypestandard == 1])

# Size of the sample
n_standard <- sum(data4$LunchTypestandard == 1)

# Parametric bootstrap
boot_standard <- rnorm(1000, mean = mean_standard, sd = std_standard / sqrt(n_standard))

# IC 95% 
ic_standard <- quantile(boot_standard, c(0.025, 0.975))

print(mean_standard)
print(std_standard)
cat("95% Confidence Interval for FREE/REDUCED:", ic_standard[1], "-", ic_standard[2], "\n")
#we can see that an IC of 95% is (71.4, 71.8) approximately
