install.packages("MASS")
install.packages("bootStepAIC")
install.packages('caret')

library(ggplot2)
library(GGally)
library(ggpubr)
library(readr)
library(tidyverse)
library(fastDummies)
library(Hmisc)
library(caTools)
library(MASS)
library(bootStepAIC)
library(car)
library(caret)
library(pROC)
library(moments)
library(usethis)
library(devtools)
library(ggplot2)
library(grid)
library(gridExtra)
library(nortest)
library(corrplot)
library(graphics)
library(yardstick)
library(cvms)
library(tibble)

#DATASET
heart <- read_csv("heart_failure_clinical_records_dataset.csv")
head(heart)

#quick view 
str(heart)

summary<-skim(heart)
summary
as.data.frame(summary)

#Check for NA 
colSums(is.na(heart))


#tidy verse section, GRAPHICAL PLOTTING 
# set the plot size
options(repr.plot.width=16, repr.plot.height=10)

# use map to create a list of plots
rawdata_plots_list <- map2(.x = heart, .y = names(heart), .f = ~ggplot(heart, aes(x = .x)) +
                               geom_bar(fill = "navy", col = "black", alpha = 0.25, bins = 15, width = 0.2) +
                             labs(x = .y) +
                             theme_bw())

#Plot of certain graph just to understand if the map above works or not 
#T
ggplot(heart, aes( x=age)) +
  geom_bar(fill = palette[4])+
  theme_minimal()

ggplot(heart, aes( x=anaemia)) +
  geom_bar(fill = palette[3])+
  theme_minimal()

ggplot(heart, aes( x=platelets)) +
  geom_bar(fill = palette[4])+
  theme_minimal()

# display all the plots together
ggpubr::ggarrange(plotlist = rawdata_plots_list)


#display the feature with the if function 
# Function built with the help of Introduction of Statistical Learning 

#Starting with the numeric features, we need to take a look to their 
#correlation with the death event feature, and is it shown in the next 
#graph, we can see that by grouping them by death event, some of them
#visualy do not change so much based on that they have similar results 
#for both conditions (their means apparently are at the same level). 
#This is the case in example, for creatinine phoshokinase or platelets;
#nevertheless, we see that some others apparently do have differences, 
#as it´s seen in ejection fraction, serum creatinine, serum sodium and time:

# graphics to see correlation between features and DEATH EVENT
#now the features are numeric on a continue scale but later will be Yes or No
#  Age by death event
graph_age <- ggplot(heart, aes(x=DEATH_EVENT, y=age, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# creatinine phosphokinase by death event
graph_cph <- ggplot(heart, aes(x=DEATH_EVENT, y=creatinine_phosphokinase, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# ejection fraction by death event
graph_ef <- ggplot(heart, aes(x=DEATH_EVENT, y=ejection_fraction, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# Platelets by death event
graph_plat <- ggplot(heart, aes(x=DEATH_EVENT, y=platelets, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# serum creatinine by death event
graph_sc <- ggplot(heart, aes(x=DEATH_EVENT, y=serum_creatinine, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# serum sodium by death event
graph_ss <- ggplot(heart, aes(x=DEATH_EVENT, y=serum_sodium, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# time by death event
graph_time <- ggplot(heart, aes(x=DEATH_EVENT, y=time, group=DEATH_EVENT)) +
  geom_boxplot(aes(fill=DEATH_EVENT))
# combine all graphs in one graph ( TO DO )
((graph_age + graph_cph) / (graph_ef + graph_plat) / (graph_sc + graph_ss)) +
  plot_annotation(title = "Boxplots of Numeric Features with Death Event")

grid.arrange(graph_age,
             graph_cph,
             graph_ef,
             graph_plat,
             graph_sc,
             graph_time)
#PLOT OF THE NORMAL DISTRIBUTION TEST
#write about the lillie.test: Lilliefors (Kolmogorov-Smirnov) test 
#for normality

numeric_features <- heart %>%
  group_by(DEATH_EVENT) %>%
  select_if(is.numeric)
normality_test <-  data.frame(p.value = sapply(numeric_features[2:8],
                                               function(x) round(lillie.test(x)$p.value, 4))) 
print(normality_test)

#As none of them are normal distributions (all are < 0.05), 
#we should take a look to the outliers as probably they are 
#messing all things up:


#GRAPHIC TO SHOW OUTLIER 
age_bp <- ggplot(heart, aes(x=age)) + geom_boxplot()
cpk_bp <- ggplot(heart, aes(x=creatinine_phosphokinase)) + geom_boxplot()
ef_bp <- ggplot(heart, aes(x=ejection_fraction)) + geom_boxplot()
plat_bp <- ggplot(heart, aes(x=platelets)) + geom_boxplot()
serumc_bp <- ggplot(heart, aes(x=serum_creatinine)) + geom_boxplot()
serums_bp <- ggplot(heart, aes(x=serum_sodium)) + geom_boxplot()
time_bp <- ggplot(heart, aes(x=time)) + geom_boxplot()
#Add all in one graph (TO DO)
grid.arrange(age_bp,
             cpk_bp,
             ef_bp,
             plat_bp,
             serumc_bp,
             serums_bp,
             time_bp)


#set the plot size
options(repr.plot.width=10, repr.plot.height=10)


#The plots above give us some insight into which features are more strongly associated with a deceased patient. Here is a list of observations.




#Now we move on to look at correlations between the various predictor
#variables. Since every column of the raw dataset is numeric,
#we'll use that to create the correlation matrix. The corrplot package
#has a nice function to plot the correlation matrix. The plot below 
#shows that there are not strong correlations between the non-time-related predictors.
#This means that multi-collinearity is less likely to be an issue

# create correlation plot
# (we need to convert the booleans and factor variables to numeric before we can feed the data frame into corrplot)
corrplot(cor(heart %>% mutate(across(everything(), ~as.numeric(.x)))),
         method = "pie", type = "upper", tl.col = "black", diag = FALSE)


#ENCODING 
heart$sex<-as.factor(heart$sex)
heart$diabetes <- as.factor(heart$diabetes)
heart$high_blood_pressure<- as.factor(heart$high_blood_pressure)
heart$smoking<- as.factor(heart$smoking)

#Adjust Plateletes ( too large)
heart$platelets <- heart$platelets/1000
#Scaling is a way to compare data that is not measured in the same way.
heart$creatinine_phosphokinase <- scale(heart$creatinine_phosphokinase, scale= FALSE)

#Cross tabulate select fields 
table1 <- table(heart$high_blood_pressure, heart$DEATH_EVENT)
table1

#check if the categorical variable has significant correlation between them 
#This test allow us to understand whether both variable in out Table1 are independent
#P-value > 0.05 we do not reject the 
#null hyphotesis (no relationship exist on the categorical variables--> they are independent)

chi1 <- chisq.test(table1)
chi1

#Now we establish the assumption for the expected count. 
# The assumption for EXPECTED COUNTS requires that 80% of the cells to have at least 
#an expected count greater than five 

chi1$expected


#LOGISTIC REGRESSION 
# the dependent variable is given by the DEATH_EVENT ( which is binary 0= no , 1= yes)
#remember that dot means ( go through all the explanatory variable I have)

#family= it's important to indicate it cause it means the it will be the logistic
#regression model 
Logistic <- glm(DEATH_EVENT ~ .
                , data= heart, family = "binomial")

#from the summary we can see by the ** in the Pr(>|z|) if the variable is statistical significantly or not
summary(Logistic)


#What we can do about this model is that we're using all the predictors we have but 
#we could also use some other techniques, a sort of MODEL SELECTION technique in order to select 
#the right number of feature 

#Also in theoretical parts there's written in the LECTURE 8 the model technique --> STEPWISE 

#We're gonna use the backwards stepwise regression. We install the package MASS 

#The backward stepwise start with the satured model first and then in each step will remove one variable up it 
# reaches a model where all the variable are significant ( it works like LASSO and RIDGE penalizing the non significant variable)
#What the model do ? It will compare the AIC of each model and select the lowest AIC 
# LOWER value indicates an improved fit for the model based on the data being used 

logistic_step <- stepAIC(Logistic,
                         direction = "backward", trace = FALSE)

par(mfrow=c(2,2))
plot(logistic_step)

logistic_step

#Standardized and Studentized Residual Distribution
summary(logistic_step$residuals)

shapiro.test(logistic_step$residuals)

ggqqplot(logistic_step$residuals)


#The reason why I selected backward is because it start with a fully saturate model. 

#The best model suggested is this one -->DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + 
#serum_sodium + time, family = "binomial", data = heart)

#the problem of step AIC is that leads to inflated R square and overfitting. Most of the time is suggested to use 
#classic method in which it will be defined the training and test. 

#ALTERNATIVE WAY to step AIC is the bootstrap resampling. We use the bootstrap re sampling 
#with replacement method to assess consistency predictors selected with stepwwise 

#I will take a sample of the data set that I'm using 299 partecipant and I'll do this a certain amount of times 
#and I'll run this step AIC over and over again on each sample and I'm sampling replacement and 
#then what it's going to provide is a diagnostic on the consistency of the variables because there could be variable
#that are significant in one sample or in another sample. 

mod_boot <- boot.stepAIC(Logistic, heart, B= 50 )

mod_boot
# B= 50 it basically means the number in which I run the resample, in this case I'll run the resample 50 times

#So the boot.stepAIC is basically telling me, for each variable in the saturated model how often (in  % of times) the 
#predictor is selected . We can see the table with all the values and just see how it works. 

#Investigate on the + and - value in the output 

#let's run the FINAL model suggested by the output.
logistic_2 <- glm(DEATH_EVENT ~ age+ejection_fraction + serum_creatinine + serum_sodium + 
                    time, 
                  data= heart, family = "binomial")


summary(logistic_2)

#From the summary we can notice how the result plotted are the log probability( the estimates in log probbaility ) that are 
#interpreting as for example:  age for every one year increase of 0.04 log probability but what I want it's an probability ratio 
# I'm gonna use: 

#Return Exponentiated coefficients to get the Odds Ratio 
OR <- data.frame(exp(logistic_2$coefficients))

#Produce Percent Probbaility
OR_Perc <- (OR-1)*100

OR_Perc

#From the output of OR_Perc, let's interpret the coefficient age= 4.338089e+00
#this means that for every additional year older the odds of death(probabilità di morte) increases by 4.3 percent.
#So one year old  may just be 4.33 % increase, and I don't it's increase because is positive and not negative. 
#If it was negative you'd be interpreting as decrease

# Therefore, for every 10 year older the odds of death increases by 53% 
#while controlling for all other predictors in the model.



#CHECK FOR MULTICOLLINEARITY

#check if the independent variable are not too correlated, in the specific, they are not linear dependent on one other 
#If we have multicollinearity basically what we'll have is that, in the model there will be an increase in the standard
#error and a decrease in the reliability of the coefficient estimates. 

#In order to detect multicollinearity we'll take a look at the Variables inflation Factor (VIF) if the VIF is > than 5 
#this result would suggest high correlation 

vif(logistic_2)



# TEST THE MODEL with a generic dataset 
#Let's create a dataframe in which we put some features in order to understand if our model works or not 

trial_data <- data.frame(age= 61, ejection_fraction = 38, serum_creatinine= 1.4,
                         serum_sodium= 136, time=130)
trial_data

predict(logistic_2, trial_data, type = 'response')

#Same data_set but I increase age up to 95 years old
trial_data1 <- data.frame(age= 95, ejection_fraction = 38, serum_creatinine= 1.4,
                          serum_sodium= 136, time=4)
trial_data1
predict(logistic_2, trial_data1, type = 'response')

#As we can see there is a probability of death that increase at 94% when I increase the number of years old 



#TRAIN AND TEST DATA 
#We will do a stratified random sampling based on the DEATH_EVENTto generate train data from 70%
#of the full dataset and test data from 30% of the full dataset.
set.seed(100)
trainIndex <- createDataPartition(heart$DEATH_EVENT, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)


hftrain<-heart[trainIndex,]
hftest<-heart[-trainIndex,]
hftest

#Probability & Confusion Matrix
#After the model is built, we will use the same dataset as a testing dataset, and try to predict the
#probability of DEATH_EVENT. After the probabilities are generated, we need to classify the binary events
#(death or alive) based on the probability. 0.5 (50%) probability is commonly used as the benchmark of the
#binary events, which means any probability above 50% will be considered as death, vice-versa.

hftest$probability<-predict(logistic_2,hftest,type='response')
hftest$death_pred<-ifelse(hftest$probability>=0.5,1,0)

#From the CONFUSION MATRIX we want to see how the model predict. 
#from the table we can see how the 52 prediction are right and also 23 prediction are right, so we have 
#75 right classification out of 89. 
#From this score we are able to compute the accuracy of our model, 0.84%

CM <- table(hftest$death_pred,hftest$DEATH_EVENT)
CM

true<- as.factor(hftest$death_pred)
pred<-as.factor(hftest$DEATH_EVENT)

#CM statistics 
cm <- caret::confusionMatrix(pred, true)
print(cm) 


predicted <- cbind(data.frame(pred), true)

ConfusionTableR::binary_visualiseR(train_labels = predicted$pred,
                                   truth_labels= predicted$true,
                                   class_label1 = "", 
                                   class_label2 = "",
                                   quadrant_col1 = "#28ACB4", 
                                   quadrant_col2 = "#4397D2", 
                                   custom_title = " Confusion Matrix", 
                                   text_col= "black")

#AUC in order to plot the to see the specificity and sensitivity of the model
#search the specifity for AUC in order to understand how do that 

plot(roc(hftest$DEATH_EVENT,hftest$death_pred),col='red')

fourfoldplot(CM, color = c("cyan", "pink"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")





#Execute The Area under the ROC, TRUE LABEL vs PREDICTED ONE 
auc(roc(hftest$DEATH_EVENT,hftest$death_pred))

#Regression Plot 
#In conclusion, the accuracy and AUC values for this model indicate that the 
#stepwise logistic regression model is a decent model to predict mortality by 
#heart disease.

hftest %>% 
  arrange(probability) %>% 
  mutate(rank=rank(probability),Event=ifelse(probability>=0.5,'Dead','Alive')) %>% 
  ggplot(aes(rank,probability))+
  geom_point(aes(color=Event))+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) +
  ggtitle('Stepwise Logistic Regression',subtitle='Predicting Mortality by Heart Failure')




