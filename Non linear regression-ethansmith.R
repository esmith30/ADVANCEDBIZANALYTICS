library(tidyverse)
library(dplyr)
#1A
plot(wages$Age,wages$Wage,main="Wages vs Age",xlab="Age",ylab="Wage",col="blue")

#1B
reg1<-lm(wages$Wage~wages$Age+wages$Educ)
summary(reg1)

#1C
wages1<-wages%>%
  mutate(agesq=Age^2)
reg2<-lm(wages1$Wage~wages1$Age+wages1$agesq+wages1$Educ)
summary(reg2)
#This code supprots our answer in A as agesq has a negative coefficient when age has a positive


#1D
#model predicted wage = -22.72+1.35 (Age)-0.013 (agesq)+1.254 (Educ)
#set education = 16 the model has a predicted wage = -22.72+1.35(Age)-0.013(agesq)+20.064
#which is equal to -2.666+1.35(age)-0.013(agesq)
# 30 years old

-2.666+1.35(30)-0.013(30^2)
=26.134 #expected wage for someone who is 30 years old is $26.134

#50 years old
-2.666+1.35(50)-0.013(50^2)
=32.344 #expected wage for someone who is 50 years old is $32.344

#70 years old
-2.666+1.35(70)-0.013(70^2)
=28.134 #The expected age for someone who is 70 years old is $28.134

#1E
#Turning point of quadratic regression is B1/-2B2
#Turning point=1.35/-(2*-0.013)
#Turning point= 
(1.35)/0.026
=51.92
#Someone with 16 years of education will attain the highest wages at approxometly 52 years old


#2A
plot(AnnArbor$Beds,AnnArbor$Rent,main="Bedrooms vs Rent",xlab="Bedrooms",ylab="Rent",col="gold")
# A log transformation would work better here as it doesn't follow a linear pattern
plot(AnnArbor$Baths,AnnArbor$Rent,main="Bathrooms vs Rent",xlab="Bathrooms",ylab="Rent",col="red")
# A log transformation would work better here as it doesn't follow a linear pattern
plot(AnnArbor$Sqft,AnnArbor$Rent,main="Square footage vs Rent",xlab="Sqaure Feet",ylab="Rent",col="green")
# This data follows a more linear pattern

#2B
AnnArbor1<-AnnArbor%>%
  mutate(lnbaths=log(Baths))
AnnArbor2<-AnnArbor1%>%
  mutate(lnbeds=log(Beds))
reg3<-lm(AnnArbor2$Rent~AnnArbor2$lnbeds+AnnArbor2$lnbaths+AnnArbor2$Sqft)
summary(reg3)
# The model predicted rent = 499.22+383.80log(Beds)+140.80log(Baths)+0.30(Sqft)
# In this model the coefficients for log(Beds) and log(Baths) represent a 100% increase in Beds and Baths
#So to predict rent for a 1600sqft 3 bed and 2 bath property we plug in accordingly
499.22+383.80ln(3)+140.80ln(2)+0.30(1600)
=1498.46
# The predicted rent for a 1600 sqft, 3 bed and 2 bath property is approximately $1498.46