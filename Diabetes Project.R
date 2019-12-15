---
  title: "Exploring the BRFSS data"
output: 
  html_document: 
  fig_height: 4
highlight: pygments
theme: spacelab
---
  
  ## Setup
  
  ### Load packages
  
  library(ggplot2)
library(dplyr)
library (RColorBrewer)

# ### Load data
load("brfss2013.RData")

names(brfss2013)

* * *
  
  * * *
  
  ## Part 1: Data
  
  1.1 Data Collection
BRFSS (The Behavioral Risk Factor Surveillance System) is the nation's system that collect state data about U.S. residents regarding their health-related risk behaviors, chronic health conditions, and use of preventive services. The details about data collection procedures can be found from https://www.cdc.gov/brfss/.
Since 2011, the data were collected by landline telephone and Cell Phone-based surveys at the state and local level in all 50 states as well as the District of Columbia and three U.S. territories. The states used standardized core (fixed and rotating) questionnaire, optional modules, and state-added questions for adults (>=18years old).

1.2 Generalizability
The data for the sample can be generalized to the population. Disproportionate stratified sampling (DSS) has been used for the landline sample since 2003. Only one person was randomly selected per household for interview; Cellular telephone respondents are randomly selected with each having equal probability of selection. Random sampling (Random Digit Dialing (RDD) techniques) was used across all participated states during the data collection. Using both landline telephone and cell phone surveys can access to different age groups, racial and ethnic groups.
However, some biases also exist in data analysis such as non-response bias, information bias. Refused respondents/Missing values in the dataset may lose some important information about the severe health status. 


1.3 Causality
BRFSS is a big ongoing observational study with random sampling but no random assignment, thus the study can only provide potential associtaion between exposures and disease. No causality can be established using this data.





* * *
  
  ## Part 2: Research questions
  
  **Research quesion 1:**
  What is the prevalence of diabetes in different races in the sample? Does sex play a role of getting diabetes?
  Dependent variable: Prevalence rate of diabetes
Independent variables: race(_imprace)
Potential confounder: sex


**Research quesion 2:**
  What is the distribution of diabetic people in terms of age groups?
  Dependent variable: Prevalence rate of diabetes
Independent variables: AGE(_ageg5yr)
Potential confounder: sex


**Research quesion 3:**
  What is the difference of BMI between females and males? Is there an association between BMI and prevalence of diabetes?  
  Dependent variable: Prevalence of diabetes
Independent variables: body weight(X_bmi5)
Potential confounder: sex


* * *
  
  ## Part 3: Exploratory data analysis
  Diabetes is a disease when the body glucose is higher than 7 mmol/L. Differenct types of diabetes have different causes. The known factors that cause diabetes include BMI, gender, ethnic background, smoking, alcohol consumption, diet, life styles, etc. Diabetes is still one of the most severe diseases affecting the normal life of people. Therefore, it is of great significance to report the prevalence of diabetes annually. 


**Research quesion 1:**
  
  Prevalence of diabetes varies among races. This part is to show which race has the highest prevalence rate of diabetes and if the prevalence rate of diabetes differs between genders. 

# Calculate the diabetes distribution in 6 race groups
Diabetes1 <- brfss2013 %>%
  filter(!is.na(brfss2013$diabete3), !is.na(brfss2013$sex), !is.na(brfss2013$X_imprace)) %>%
  group_by(X_imprace,sex,diabete3) %>%
  summarize(n = n())

# Calculate the prevalence of diabetes in 6 race groups
Diabetes2 <- brfss2013 %>%
  filter(!is.na(brfss2013$diabete3), !is.na(brfss2013$sex), !is.na(brfss2013$X_imprace)) %>%
  group_by(X_imprace,sex,diabete3) %>%
  summarize(n = n())%>%
  mutate(pct_diabetes = n/sum(n),
         posn_pct = cumsum(pct_diabetes)-0.5*pct_diabetes)

# Plot the distribution of diabetes by races
ggplot(Diabetes1, aes(x = X_imprace, y = n, fill = diabete3)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  facet_wrap(~sex,nrow=2)+
  scale_fill_brewer(palette = "Accent")+
  coord_flip()+
  scale_y_continuous("count", breaks = seq(0, 400000, by = 10000)) +
  labs(title = "Diabetes distribution in 6 race groups")+
  theme(axis.text.x =   element_text(angle = 270))

The bar chart above shows that there are 6 ethnic groups and 4 diabetic/non-diabetic states including Non-diabetic, Prediabetic, Diabetic, Diabetic (Gestational diabetes). Majority of the sample is White (Non-Hispanic). Overall, in each ethnic group there are more females than males.

# Plot the prevalence rate of diabetes by races and genders
ggplot(Diabetes2, aes(x = X_imprace, y = pct_diabetes, fill = diabete3)) +
  geom_bar(stat = "identity", alpha = 0.6, col = "dark grey") +
  scale_fill_brewer(palette = "Accent")+
  facet_wrap(~sex , nrow = 2) +
  geom_text(aes(label = paste0(sprintf("%.1f", pct_diabetes*100),"%"), y=1-posn_pct),angle=30,
            vjust=1,hjust=0.3, size=3,col = "black") +
  labs(title = "Prevalence of diabetes by sex in each race") +
  scale_x_discrete("diabete3") +
  scale_y_continuous(labels = scales::percent)+
  # scale_y_continuous("pct_diabetes") +
  coord_flip()

The bar chart above shows that in females, Black-Non-Hispanic race has the highest prevalence rate of diabetes (20.9%) but in males American indian/Alaskan Native race has the highest prevalence rate (18.7%). Asian race has the lowest prevalence rate of diabetes in both female (8.0) and male groups (9.7%).
This indicate that diabetes is associated with genetic background
With each ethnic group, there is also some small differences of prevalence rate of diabetes between genders. Since the sample size of male and female group is different in each group, furthur analysis should adjusted by gender if there is sex effect on diabetes is statistically significant.  


**Research quesion 2:**
  
  The risk of getting diabetes increases as people get older. Here, the prevalence of diabetes in adults is compared among 13 age groups in five-year age categories

Diabetes_Age <- brfss2013 %>%
  filter(brfss2013$diabete3 %in% "Yes",!is.na(brfss2013$diabete3),!is.na(brfss2013$X_imprace),!is.na(brfss2013$sex),!is.na(brfss2013$X_ageg5yr)) %>%
  group_by(X_imprace,sex,X_ageg5yr) %>%
  summarize(n = n())%>%
  mutate(pct_diabetes = n/sum(n),
         posn_pct = cumsum(pct_diabetes)-0.5*pct_diabetes)


#plot  the diabetes prevalence by age groups and races
ggplot(Diabetes_Age, aes(x = X_ageg5yr, y = pct_diabetes,col=sex,fill=sex)) +
  geom_bar(stat = "identity", position = "dodge",width=0.6, alpha = 0.8) +
  facet_wrap(~X_imprace,nrow = 6)+
  # coord_flip()+
  geom_text(aes(label = paste0(sprintf("%.1f", pct_diabetes*100),"%")),position = position_dodge(0.9), 
            angle=0, vjust=-0.2,hjust=0.5, size=3,col = "black") +
  labs(title = "Prevalence of diabetes by sex in each race") +
  scale_x_discrete("X_ageg5yr") +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x =   element_text(angle = 270))


From the plot above, we can see that in age groups (>=70yrs), the prevalence of diabetes in females is up to 11% higher than that in males (e.g., 14% female vs 3.1%male). But in middle age groups (Age 40~69), opposite prevalence of diabetes is observed between genders. 
However, there clould be some bias in this data interpretation. The average life expectancy of female is longer than male which can cause a different prevalence rate of diabetes in older age groups. 

**Research quesion 3:**
  
  Diabetes is highly prevalent in obese people, therefore, I would like to plot the BMI vs diabetic status.  
Diabetes_BMI <- brfss2013 %>%
  filter(!is.na(brfss2013$diabete3),!is.na(brfss2013$X_imprace),!is.na(brfss2013$sex),!is.na(brfss2013$X_ageg5yr), !is.na(brfss2013$X_bmi5)) 

df3_Diabetes<- as.data.frame(cbind(as.character(Diabetes_BMI$X_imprace),as.character(Diabetes_BMI$sex),as.character(Diabetes_BMI$X_age65yr),Diabetes_BMI$X_bmi5,as.character(Diabetes_BMI$diabete3)))
colnames(df3_Diabetes)<-c("Race","Sex", "Age","Weight","Diabetes")  
df3_Diabetes$Group<-paste(df3_Diabetes$Diabetes,df3_Diabetes$Sex)
df3_Diabetes$Weight<-as.numeric(df3_Diabetes$Weight)

ggplot(df3_Diabetes, aes(x =Group, y =Weight,fill=Sex)) +
  geom_boxplot()+
  facet_wrap(~Race)+
  coord_flip()


The boxplots tell us the median BMI distribution in disease/non-disease groups is as below: diabetic people > pre-diabetic people >non-diabetic people.
For example, In American Indian/Alaskan Native people, the median BMI of non-diabetic group is ~1,200, with pre-diabetic group ~1,500, and diabetic group >1,500. Outliers may cause some problem, with which should be carefully dealt. This pattern is similar among all the ethic/race groups. 
This indicates that diabetes is associated with body weight/BMI. 
We can also see the data in a different way, e.g., comparing the IQR of BMI among different ethnic/race groups.   



