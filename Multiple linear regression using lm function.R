setwd("D:/")

library(readxl)
regression <- read_excel("D:/Rfregression.xlsx")
View(regression)

#Check multicollinearity prior to conducting multiple regression- with the help of 
#correlation matrix,tolerance value, and variance inflation factor

pairs(regression[2:7]) #reading data from column 2 to 7 only
cor(regression)
# Multiple regression using lm function
Multiple <- lm(regression$LST~ regression$RVI+regression$IPVI+
                   regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
                 data = regression)
Multiple
summary(Multiple)

# Confidence interval

confint.default(Multiple)

# Odd ratio

exp(coef(Multiple))

# odd ration and C.I

exp(cbind(OR=coef(Multiple), confint.default(Multiple)))


# Adding predicted logits to the dataframe
regression<- data.frame(regression, PredLogit=predict(Multiple))

# add the predicted odds to the dataframe
regression<- data.frame(regression, PredOdds= exp(predict(Multiple)))

# add predicted probabilities to the dataframe
regression<- data.frame(regression, PredProb=regression$PredOdds/(1+regression$PredOdds))
str(regression)                        

# Correlation between predicted probabilities and group membership on the DV
r <- cor(regression$PredProb,regression$LSTBinary)

# Squaring correlation
r2 <- r^2
r
r2

# Likelihood test ratio of the predictors

# full model
Multiple <- lm(regression$LST~ regression$RVI+regression$IPVI+
                 regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
               data = regression)
summary(Multiple)

# testing RVI: RVI removed from model
Model1 <- lm(regression$LST~ regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
             data = regression)
summary(Model1)

#testing model 1 with full model
anova(Multiple, Model1)

# testing IPVI: IPVI removed from model
Model2 <- lm(regression$LST~ regression$RVI+
                regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
              data = regression)
summary(Model2)
#testing model 2 with full model
anova(Multiple, Model2)

# Testing NDVI: NDVI removed from model
Model3 <- lm(regression$LST~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDWI+regression$NDMI, 
              data = regression)
summary(Model3)
#testing model 3 with full model
anova(Multiple, Model3)

# Testing DVI: DVI removed from model
Model4 <- lm(regression$LST~ regression$RVI+regression$IPVI
              +regression$NDVI+regression$NDWI+regression$NDMI, 
              data = regression)
summary(Model4)

#testing model 4 with full model
anova(Multiple, Model4)

# Testing NDWI: NDWI removed from model
Model5 <- lm(regression$LST~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDMI, 
              data = regression)
summary(Model5)

#testing model 5 with full model
anova(Multiple, Model5)

# Testing NDMI: NDMI removed from model
Model6 <- lm(regression$LST~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDWI, 
              data = regression)
summary(Model6)

#testing model 6 with full model

anova(Multiple, Model6)

# checking multicollinearity
# Checking for RVI: auxillary regression
RVI <- lm(regression$RVI~ regression$RVI+regression$IPVI+
                 regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
               data = regression)

summary(RVI)
(1-0.1989)   #Tolerance value (1-R-squared): Multicollinearity exists if T value is less than 0.1
(1/(1-0.1989)) #Variance Inflation Factor (1/(1-R-squared)): Multicollinearity exists if VIF is more than 10.

# Checking for IPVI: auxillary regression
IPVI <- lm(regression$IPVI~ regression$RVI+
                 regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
               data = regression)
summary(IPVI)
(1-1)
(1/(1-1))
# Checking for DVI: auxillary regression
DVI <- lm(regression$DVI~ regression$RVI+regression$IPVI
                 +regression$NDVI+regression$NDWI+regression$NDMI, 
               data = regression)
summary(DVI)
(1-0.9658)
(1/(1-0.9658))
# Checking for NDVI: auxillary regression
NDVI <- lm(regression$NDVI~ regression$RVI+regression$IPVI+
                 regression$DVI+regression$NDWI+regression$NDMI, 
               data = regression)
summary(NDVI)
(1-1)
(1/(1-1))
# Checking for NDWI: auxillary regression
NDWI <- lm(regression$NDWI~ regression$RVI+regression$IPVI+
                 regression$DVI+regression$NDVI+regression$NDMI, 
               data = regression)
summary(NDWI)
(1-0.949)
(1/(1-0.949))
# Checking for NDMI: auxillary regression
NDMI <- lm(regression$NDMI~ regression$RVI+regression$IPVI+
                 regression$DVI+regression$NDVI+regression$NDWI, 
               data = regression)
summary(NDMI)
(1- 0.7761)
(1/(1- 0.7761))

# Testing Homoscadesticity and Heteroscadesticity using Breusch-Pagan Test 
install.packages("lmtest")
library(lmtest) #null hypothesis: There's no constant variance in residuals (P>0.05)
bptest(regression$LST~ regression$RVI+IPVI+DVI+NDVI+NDWI+NDMI, data = regression)
# My result shows p value (0.004908)< 0.05, therefore, null hypothesis is rejected and it shows there is homoscadesticity