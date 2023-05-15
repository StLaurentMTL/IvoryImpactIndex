library(tidyverse)
library(VGAM)
Master=read.csv(file.choose())
Master[is.na(Master)]=0
#I am going to create index for each respective variables. I will do it 
#10% percentiles to be more granular. 
Master$median_household_income=as.numeric(Master$median_household_income)
quantile(Master$median_household_income,seq.int(.1,1,.1))
Master=Master %>% mutate(AMI_Score=
                           case_when(median_household_income <= 49323~1,
                                     median_household_income <= 63465~2,
                                     median_household_income <= 76417~3,
                                     median_household_income <= 85729~4,
                                     median_household_income <= 97195~5,
                                     median_household_income <= 107930~6,
                                     median_household_income <= 124338~7,
                                     median_household_income <= 140688~8,
                                     median_household_income <= 169313~9,
                                     median_household_income <= 239094~10))

#Percent over 21. I did transform any of these variables into a binary. Though 
#you could easily. I will only do 20% percentiles. 
Master$Percent_Over21=as.numeric(Master$Percent_Over21)
quantile(Master$Percent_Over21,seq.int(.2,1,.20))
Master=Master %>% mutate(Percent21Score=
                           case_when(Percent_Over21<=0.7495194~1,
                                     Percent_Over21<=0.7786012~2,
                                     Percent_Over21<=0.8086285~3,
                                     Percent_Over21<=0.8504875~4,
                                     Percent_Over21<=1.0000000~5))
#Homeowner Vacancy Rates 
Master$Homeowner_Vacancy_Rate=as.numeric(Master$Homeowner_Vacancy_Rate)
quantile(Master$Homeowner_Vacancy_Rate,seq.int(.2,1,.20))
#The percentiles are significant until the 80th percentile. Hence I will only make 
#2 scores. Areas with a 0% vacancy rate will just get 1. 
Master=Master %>% mutate(Homeowner_Vacancy_Score=
                           case_when(Homeowner_Vacancy_Rate<=0~1,
                                     Homeowner_Vacancy_Rate<=.92~4,
                                     Homeowner_Vacancy_Rate<=22.3~5))
#Rental Vacancy Rates
Master$Rental_Vacancy_Rate=as.numeric(Master$Rental_Vacancy_Rate)
quantile(Master$Rental_Vacancy_Rate,seq.int(.2,1,.20))
#Similar to Homeowner Vacancy Rates in terms of significant percentiles. 
Master=Master %>% mutate(Rental_Vacancy_Score=
                           case_when(Homeowner_Vacancy_Rate<=0~1,
                                     Homeowner_Vacancy_Rate<=2.7~2,
                                     Homeowner_Vacancy_Rate<=5.42~3,
                                     Homeowner_Vacancy_Rate<=28.2~4))

#Two Plus Bedrooms
Master=Master %>% mutate(Two_Plus_Bedrooms=X1_bedroom+
                           X2_bedrooms+
                           X3_bedrooms+
                           X4_bedrooms+
                           X5_bedrooms_plus)

Master$Two_Plus_Bedrooms=as.numeric(Master$Two_Plus_Bedrooms)
quantile(Master$Two_Plus_Bedrooms,seq.int(.2,1,.20))                         
Master=Master %>% mutate(Bedroom_Score=
                           case_when(Two_Plus_Bedrooms<=839.4~1,
                                     Two_Plus_Bedrooms<=1115.0~2,
                                     Two_Plus_Bedrooms<=1419.0~3,
                                     Two_Plus_Bedrooms<=1806.6~4,
                                     Two_Plus_Bedrooms<=3434.0~5))

#Non_White Score
quantile(Master$Non_White,seq.int(0,1,.1))
Master=Master %>% mutate(Non_White_Score=
                           case_when(Non_White<=19~0,
                                     Non_White<=1962.4~1,
                                     Non_White<=2442.2~2,
                                     Non_White<=2900.8~3,
                                     Non_White<=3410.8~4,
                                     Non_White<=3803.5~5,
                                     Non_White<=4373.6~6,
                                     Non_White<=4835.2~7,
                                     Non_White<=5586.2~8,
                                     Non_White<=6894.7~9,
                                     Non_White<=12721.0~10))

#Ownership score
Master$homeownership_rate=as.numeric(Master$homeownership_rate)
Master$homeownership_rate=(Master$homeownership_rate)*100
quantile(Master$homeownership_rate,seq.int(0,1,.2))

Master=Master %>% mutate(ownership_score=
                           case_when(homeownership_rate<=92.30259~1,
                                     homeownership_rate<=94.48145~2,
                                     homeownership_rate<=96.15494~3,
                                     homeownership_rate<=97.89445~4,
                                     homeownership_rate<=100.00000~5))

#EPA D4A (This measure is in meters)
quantile(Master$D4A,seq.int(.2,1,.2))
Master=Master %>% mutate(D4A_Score=
                           case_when(D4A<=133.306~5,
                                     D4A<=246.230~4,
                                     D4A<=358.616~3,
                                     D4A<=556.560~2,
                                     D4A<=1192.260~1))
#EPA D5AR Jobs within 45 minutes
quantile(Master$D5AR,seq.int(.2,1,.2))
Master=Master %>% mutate(D5AR_Score=
                           case_when(D5AR<=98798.6~1,
                                     D5AR<=129129.4~2,
                                     D5AR<=151211.4~3,
                                     D5AR<=182666.0~4,
                                     D5AR<=250979.0~5))

#Cummalative Score (Prototype 1)
Master=Master %>% mutate(Cumulative_Score_1=
                           (Percent21Score*.15)+
                           (ownership_score*.20)+
                           (Rental_Vacancy_Score*.20)+
                           (Bedroom_Score*.15)+
                           (Non_White_Score*.15)+
                           (D4A_Score*.10)+
                           (D5AR_Score*.05))
Master$Cumulative_Score_1

#Cumulative Score 2
Master=Master %>% mutate(Cumulative_Score_2=
                           (Percent21Score*.05)+
                           (ownership_score*.2)+
                           (Rental_Vacancy_Score*.15)+
                           (Bedroom_Score*.20)+
                           (Non_White_Score*.15)+
                           (D4A_Score*.15)+
                           (D5AR_Score*.10))

#Cumulative Score 3
Master=Master %>% mutate(Cumulative_Score_3=
                           (Percent21Score*0)+
                           (ownership_score*.25)+
                           (Rental_Vacancy_Score*.15)+
                           (Bedroom_Score*.25)+
                           (Non_White_Score*.05)+
                           (D4A_Score*.25)+
                           (D5AR_Score*.05))

Scores=Master %>% select(Percent21Score,
                         ownership_score,
                         Rental_Vacancy_Score,
                         Bedroom_Score,
                         Non_White_Score,
                         D4A_Score,
                         D5AR_Score,
                         Cumulative_Score_1,
                         Cumulative_Score_2,
                         Cumulative_Score_3,
                         Census.Tract,ï..id)

write.csv(Master,"Final_Master.csv")
#If we decided to switch up the weight, how could the cumulative score change?
hist(Master$Cummulative_Score)

#Writing Everything down as a chart. 
write.csv(Master,"MasterWithScores.csv")
#Doing a plain-jane OLS model, this is what we get

PlugAndPlayOLS=lm(Cummulative_Score~
                    Percent_Over21+
                    Homeowner_Vacancy_Rate+
                    +I(Rental_Vacancy_Rate^(1/2))+
                    Rental_Vacancy_Rate+
                    Two_Plus_Bedrooms,data = Master)


summary(PlugAndPlayOLS)
plot(Master$Rental_Vacancy_Rate,Master$Cummulative_Score)
Master %>% 
  ggplot(aes(x=Rental_Vacancy_Rate,y=log(Cummulative_Score)))+
  geom_smooth(method = "lm",se=FALSE)+
  geom_point()

#Here is the Tobit's Model 
TobitModel=vglm(Cummulative_Score~Percent_Over21+
                  Homeowner_Vacancy_Rate+
                  Rental_Vacancy_Rate+
                  Two_Plus_Bedrooms,tobit(Upper=25),data = Master)

summary(TobitModel)


#Cummulative=Index[Weight Adjustable]+Index2[Weight Adjustable]
#Option for slider that creat