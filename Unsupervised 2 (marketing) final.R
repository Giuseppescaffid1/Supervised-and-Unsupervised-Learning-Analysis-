marketing <- marketing_campaign
rm(marketing_campaign)

library(skimr)
library(tidyverse)
library(dplyr)
library(knitr)
library(corrplot)
library(lubridate)
library(forecast)
library(recipes)
library(janitor)
library(ggplot2)
library(factoextra)
library(rattle)
library(reshape2)


marketing <- marketing_campaign

#we use skim instead of Summary ( this provide better statistics)
Data_summary<- skim(marketing)
Data_summary

#Detect NA values 
colSums(is.na(marketing))


#replace the NA values with the mean 
marketing$Income[is.na(marketing$Income)] <- mean(marketing$Income, na.rm = TRUE)


#MARKETING CAMPAIGN ANALYSIS from 1 up to 5 mktg cmp
explore_0<- marketing %>% select("AcceptedCmp1","AcceptedCmp2","AcceptedCmp3",
                        "AcceptedCmp4","AcceptedCmp5","Response")

explore_0[sample(nrow(explore_0), 15), ]



#Customer Spending Behavior / Customer product Behavior 

explore_1 <- marketing %>% select(MntWines,MntFruits,MntMeatProducts,
                              MntFishProducts,MntSweetProducts,MntGoldProds)
explore_1[sample(nrow(explore_1), 15), ]

#DATA EXPLORATION
palette <- palette.colors(9)

#Teenhome Variable
explore <- marketing
explore$Teenhome <- as.factor(explore$Teenhome)
ggplot(explore) + geom_bar(mapping = aes(x=Teenhome),fill = palette[3])

#Kidhome Variable
explore$Kidhome <- as.factor(explore$Kidhome)
ggplot(explore) + geom_bar(mapping = aes(x=Kidhome),fill= palette[4])

#Complain Variable
table(marketing$Complain)


#Recency Variable
ggplot(marketing) + geom_histogram(mapping = aes(x=Recency),bins=100,fill=palette[3])

#Marital Status 
marketing$Marital_Status <- as.factor(marketing$Marital_Status)
ggplot(marketing) + geom_bar(mapping = aes(x=Marital_Status),fill=palette[7])


#Education Variable
marketing$Education <- as.factor(marketing$Education)
ggplot(marketing) + geom_bar(mapping = aes(x=Education), fill=palette[3])


#Income Variable 
ggplot(marketing) + geom_histogram(mapping = aes(x=Income),bins=50,na.rm = T,fill= palette[2])

#From the plot we can see an outlier, let's check it 
max(marketing$Income,na.rm = T)

#remove the outlier, setting it equal to 0 
marketing$Income[is.na(marketing$Income)|marketing$Income == 666666] <- 0


#Exploring Dt_Customer variable
head(marketing$Dt_Customer,30)

#Year_Birth
table(marketing$Year_Birth)
#From the output of the printed values it can be seen how there are some outlier concerning some unrealistic year
#of birht, just for example we can take a look to the 1893-1899-1900. I remove it

#Plus, Extracting age variable from this 
marketing <- marketing %>% filter(Year_Birth >= 1940)
marketing$reference <- 2015
marketing$Age <- marketing$reference - marketing$Year_Birth
marketing <- marketing %>% select(-c("Year_Birth","reference"))

table(marketing$Age)

#Little check to change the chr variable
str(marketing)

#Categorical into Numerical (Dt_customer)
marketing$Dt_Customer <- dmy(marketing$Dt_Customer)
marketing$Dt_Customer <- year(marketing$Dt_Customer)
marketing$Dt_Customer <- as.factor(marketing$Dt_Customer)

marketing$Dt_Customer <- fct_collapse(marketing$Dt_Customer,
                                      "3" = "2014",
                                      "1" = "2012",
                                      "2" = "2013")

marketing$Dt_Customer <- as.numeric(levels(marketing$Dt_Customer))[marketing$Dt_Customer]

#Categorical into Numerical (Education)
x<-table(marketing$Education)
print(x)  

#Chr EDUCATION
Education <- as.factor(marketing$Education)
marketing$Education<- as.numeric(Education)

#Chr INCOME
Income<- as.factor(marketing$Income)
marketing$Income<- as.numeric(Income)

#Chr MARITAL STATUS 
Marital_Status <- as.factor(marketing$Marital_Status)
marketing$Marital_Status<- as.numeric(Marital_Status)

table(marketing$Marital_Status)


# PRINCIPAL COMPONENET ANALYSIS  

#The problem here is that the column variance is equal to zero. I can check which column
#of a data frame is constant this way.
Marketing_sn<-remove_constant(marketing, na.rm = T)

############################################################################################

#Applying some certain measure in order to avoid the noise from certain variables 
#Here the core is the recipe function 
process_pca <- recipe(~., data = Marketing_sn)
#Next
process_pca <- process_pca %>% step_rm("ID","Complain")
#Next
process_pca <- process_pca %>% step_zv(all_numeric_predictors())
#Next
process_pca <- process_pca %>% step_orderNorm(all_numeric_predictors())
#Next
process_pca <- process_pca %>% step_normalize(all_numeric_predictors())
#Next
check_pca <- process_pca %>% prep() %>% bake(new_data = NULL) %>% prcomp()
tidy(check_pca, matrix = "eigenvalues")
#Next
#Step_pca
#step_pca creates a specification of a recipe step that will convert numeric data into 
#one or more principal components.
process_pca <- process_pca %>% step_pca(all_numeric_predictors(), num_comp = 8) %>% prep() %>%
  bake(new_data=NULL)
############################################################################################


#Prin Comp cannot deal with NA values(remember to remove it , is it in the pre processing)
#PCA<- princomp(Marketing_sn, cor = T)


#SCREE PLOT 
#The scree plot is used to determine the number of factors to retain in an exploratory
#factor analysis (FA) or principal components to keep in a principal component analysis (PCA).
#From the scree plot we can analyze how the first 3 component explain the variance
#inside our model recalling that the variance is cumulative
screeplot(check_pca)

#Other graph for SCREE PLOT
fviz_eig(check_pca)

fviz_eig(check_pca, addlabels=TRUE, hjust = -0.3, barcolor = "mistyrose4", barfill="mistyrose4") +
  ylim(0, 70)

#compute the percentage of cumulative variance in order to understand the %
check_pca$sdev

pr.var=check_pca$sdev^2
pr.var

pve=pr.var/sum(pr.var)
pve

#PLOT of cumulative proportion of Variance 
#The proportion of variance is the variance divided by the sum of all variances.
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

#SCORE PLOT 
plot(check_pca$scores)
text(check_pca$scores, rownames(marketing))
abline(h=0, v=0)


#LOADING PLOT
#Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_var(check_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#BI-PLOT
biplot(check_pca)



#Contribution of the variable to the 1st Dimension
fviz_contrib(check_pca,choice="var",axes=1,top=10)
#Contribution of the variable to the 2nd Dimension
fviz_contrib(check_pca,choice="var",axes=2,top=10)
#Contribution of the variable to the 3rd Dimension
fviz_contrib(check_pca,choice="var",axes=3,top=10)
#Contribution of the variable to the 4rd Dimension
fviz_contrib(check_pca,choice="var",axes=4,top=10)
#Contribution of the variable to the 5rd Dimension
fviz_contrib(check_pca,choice="var",axes=5,top=10)
#Contribution of the variable to the 6rd Dimension
fviz_contrib(check_pca,choice="var",axes=6,top=10)
#Contribution of the variable to the 7rd Dimension
fviz_contrib(check_pca,choice="var",axes=7,top=10)
#Contribution of the variable to the 8rd Dimension
fviz_contrib(check_pca,choice="var",axes=8,top=10)



#K MEAN
scale_marketing<- scale(Marketing_sn)
marketing_data <- dist(Marketing_sn)

#the method are 2: the first in which I apply the normal scaling of the data in order
#to compute the distance . The second one which is a bit theoretical and has no much 
#reliability which is applyng the procces_pca ( data of the pca already scaled with only 
#the number of components that contributes) and compute the k mean with this value. 
#The aim will be to deep with this 2 system and understand what type of these 3
#works better 

#Number of cluster equal to 5 
#compute number of K 
fviz_nbclust(process_pca, kmeans, method = "wss")+
  labs(subtitle = "Elbow Method")

#K Means 
set.seed(1313)
km.out<- kmeans(process_pca, centers = 5, nstart = 200)
print(km.out)


#Print K means 
autoplot(km.out, data  = process_pca)
fviz_cluster(km.out, data = scale_marketing)
#With exploration of each variable, I will share only key observations. All the observations of all the clusters will be
#shared in the end where the clusters are compiled. This will follow a few recommendations as well.




# Inspection of the clusters
marketing_clustered <- marketing %>% mutate(cluster = km.out$cluster)


#Education Exploration
explore <- marketing_clustered
explore$Education <- as.factor(explore$Education)
explore$cluster <- as.factor(explore$cluster)
ggplot(data = explore) + geom_jitter(mapping = aes(x=cluster,y=Education))



#Marital_status
explore$Marital_Status <- as.factor(explore$Marital_Status)
explore$cluster <- as.factor(explore$cluster)
ggplot(data = explore) + geom_jitter(mapping = aes(x=cluster,y=Marital_Status))


#Income
ggplot(explore) + geom_histogram(mapping = aes(x=Income),bins=50) + facet_wrap(~cluster)


#Platform USage
marketing_clustered%>%
  select(cluster, NumWebPurchases, NumCatalogPurchases, NumStorePurchases, NumDealsPurchases, NumWebVisitsMonth)%>%
  melt(id='cluster')%>%
  ggplot(aes(as_factor(cluster), value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol = 5)

#Amount spent on different items
marketing_clustered%>%
  select(cluster, MntWines, MntFruits, MntMeatProducts, MntSweetProducts, MntGoldProds)%>%
  melt(id='cluster')%>%
  ggplot(aes(as_factor(cluster), value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol=5)

#advertisement campaign 

marketing_clustered %>% 
  select(AcceptedCmp1,AcceptedCmp2,AcceptedCmp3,AcceptedCmp4,AcceptedCmp5,Response,cluster) %>%
  group_by(cluster) %>%
  summarise(AcceptedCmp1 = sum(AcceptedCmp1),
            AcceptedCmp2 = sum(AcceptedCmp2),
            AcceptedCmp3 = sum(AcceptedCmp3),
            AcceptedCmp4 = sum(AcceptedCmp4),
            AcceptedCmp5 = sum(AcceptedCmp5),
            Response = sum(Response))

#From the output it can be seen how the If we look for sure-shot trends, Cluster 3 responds very high to 3rd and last campaigns.
#3rd campaign is also prefered by cluster 5, but so is 4th and last campaign, though not that high. 2nd campaign is the least
#responded campaign. Cluster 1, 2 and 4 responded on all campaigns, though with a preference.

#AGE

marketing_clustered %>%
  ggplot(aes(as_factor(cluster),Age))+
  geom_boxplot()

#TeenHome
marketing_clustered$Teenhome <- as.factor(marketing_clustered$Teenhome)
ggplot(data = marketing_clustered) + geom_jitter(mapping = aes(x=cluster,y=Teenhome))


#Kidhome
marketing_clustered$Kidhome <- as.factor(marketing_clustered$Kidhome)
ggplot(data = marketing_clustered) + geom_jitter(mapping = aes(x=cluster,y=Kidhome))

#Dt_customer
marketing_clustered$Dt_Customer <- as.factor(marketing_clustered$Dt_Customer)
ggplot(data = marketing_clustered) + geom_jitter(mapping = aes(x=cluster,y=Dt_Customer))


#do the the resume and plot all the features for the clusters 
#DONE

# Write about the theoretical parts related to the
#DONE





