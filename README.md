# Regression-models-with-fixed-and-random-effect
Home assignment 2017 autumn semester PSYP13

###########################################################
#                                                         #
#          Home assignement ZK research question 1        #
#                                                         #
###########################################################

#loading data
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")

#install packages
require(lsr)
require(sciplot)
require(psych) # for describe
require(lm.beta) # for lm.beta
require(dplyr)
require(gsheet)
require(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
require(ggplot2) # for ggplot
#require(rgl) -> not working for MAC

#describe dataset, look for errors
summary(data_sample_1)
describe(data_sample_1)
who(T) #after checking data: ppt 15 put in 3 instead of male/female


#create new dataset without errors (column 15)
data_sample_1 [-15,] -> data_sample_new
data_sample_new<-data_sample_new[!data_sample_new$mindfulness<=1,] #ppt 24,25,66 who scored <1 on Mindfulness were excluded
who(T)
summary(data_sample_new)
describe(data_sample_new)

#creating histograms 
hist(data_sample_new$age, breaks = 20)
hist(data_sample_new$STAI_trait, breaks = 20)
hist(data_sample_new$pain_cat, breaks = 20)
hist(data_sample_new$cortisol_serum, breaks = 20)
hist(data_sample_new$cortisol_saliva, breaks = 20)
hist(data_sample_new$mindfulness, breaks = 20)
hist(data_sample_new$weight, breaks = 20)
#all look normally distributed


#scatterplots in order to check for first pattern between variables and pain
plot(age ~ pain, data = data_sample_new)
plot(STAI_trait ~ pain, data = data_sample_new)
plot(pain_cat ~ pain, data = data_sample_new)
plot(cortisol_serum ~ pain, data = data_sample_new)
plot(cortisol_saliva ~ pain, data = data_sample_new)
plot(mindfulness ~ pain, data = data_sample_new)
plot(weight ~ pain, data = data_sample_new)
plot(sex ~ pain, data = data_sample_new)

#multiple regression
#fit regression model
mod_pain1= lm(pain ~ age + sex, data = data_sample_new)
##Results: what to report in home assignment
summary(mod_pain1)

#plotting the scatterplots for each predictor separately
#with the simple regression lines
plot(pain ~ age, data = data_sample_new)
abline(lm(pain ~ age, data = data_sample_new))
plot(pain ~ sex, data = data_sample_new)
abline(lm(pain ~ sex, data = data_sample_new))

#found outliers in the male group
#exclude outliers from the dataset
#excluding outliers from the dataset can mess up the data
#to see how the outliers influence the data is called: Cook's distance

#Cooks distance to see outliers
cooks.distance(mod_pain1)
plot(mod_pain1, which = 4) #plot cooks distance to see outliers
#plot is showing us the 3 highest values: cooks distance criteria: value greater 1 = outlier,
#outliers showing in the plot are not greater 1 so can stay in the dataset
#ppt.112 (influential outliers) has a much bigger value than others --> but not greater 1

#the regression equation: Y(pain)=b0 + b1*X1+b2*X2
#b0 = intercept
#b1 = age
#b2 = sex

####Model 1: mod_pain1####
#run regression 
predict(mod_pain1) 

#interpreting the results and regression weights
summary(mod_pain1)
AIC(mod_pain1)
confint(mod_pain1)
lm.beta(mod_pain1)
#Model1: sex and age influence only 10% of the variance of pain
#--> where do 10% come from? Summary of mod_pain1: Adjusted R-squared: 0.1022



#Multiple Regression Model 2
#predicting pain by sex, age, STAI-State, Pain Cat, MAAS, Cortisol
#Fit regression model
mod_pain2<- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_new)

#Cooks distance to see potencial outliers in Model 2
cooks.distance(mod_pain2)
plot(mod_pain2, which = 4) #plot cooks distance to see our outliers
#plot is showing us the 3 highest values: cooks distance criteria: value greater 1 is outliers,
#these three outliers showing in the plot are not greater 1 so can stay in the dataset

#the regression equation: Y(pain)= b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 * b7*x7
#b0 = intercept
#b1 = age
#b2 = sex
#b3 = STAI
#b4 = Pain Cat
#b5 = Cortisol_serum
#b6 = Cortisol_saliva
#b7 = Mindfulness

####Model 2: mod_pain2
##Run regression 
predict(mod_pain2) 

#interpreting the results and regression weights
summary(mod_pain2)
AIC(mod_pain2)
confint(mod_pain2)
lm.beta(mod_pain2)
#Model2: sex,age & all other variables influences only 46.93% of the variance of pain

##Comparing Models

## compare models with the anova function
anova(mod_pain1, mod_pain2)

#interpreting results
#Comparison is significant p= <0.001, F(5,148) = 22.21


# compare with AIC
# smaller AIC means more information
# when difference in AIC of the two models is <2, they contain roughly the same information about the outcome
AIC(mod_pain1) #AIC = 527.60
AIC(mod_pain2) #AIC = 450.28
#Interpreting: Model 2 is better, because AIC value is smaller


##Checking assumptions Model 2 -->Assumptions illustrated in table; Mulitcollinearity explained in text bc that's the important difference

#Normality assumption
#QQ plot
plot(mod_pain2, which = 2)
# skew and kurtosis
describe(residuals(mod_pain2)) #skew = 0.06 < 2
# histogram
hist(residuals(mod_pain2), breaks = 20)
#No violation of Normality

#Linearity assumption
# redicted values against actual values
pred <- predict( object = mod_pain2 )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain2)
##Linear

#Homoscedasticty assumption (homogeneity of variance)
plot(mod_pain2, which = 3)
ncvTest(mod_pain2)
##p = 0.07 = not significant = no violation of Homoscedasticty

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended
vif(mod_pain2)
pairs.panels(data_sample_new[,c("age","sex","STAI_trait", "pain_cat","cortisol_saliva","cortisol_serum", "mindfulness")], col = "red", lm = T)
##Result VIF Value: cortisol_serum(5.3582) and cortisol_saliva (5.6413): highly correlate so we can exclude one of the cortisol


####cortisol_serum out: mod_pain3_cortisolserum_out
#fit regression model
mod_pain3_cortisolserum_out = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = data_sample_new)

#run regression 
predict(mod_pain3_cortisolserum_out)

#look for outliers in model 3
#Cooks distance to see outliers
cooks.distance(mod_pain3_cortisolserum_out)
plot(mod_pain3_cortisolserum_out, which = 4) 
#plot is showing us the 3 highest values: cooks distance criteria: value greater 1 is outliers,
#these three outliers showing in the plot are not greater 1 so can stay in the dataset

#interpret results and regression weights
summary(mod_pain3_cortisolserum_out) #R^2 = 0.4728
AIC(mod_pain3_cortisolserum_out) #448.29
confint(mod_pain3_cortisolserum_out)
lm.beta(mod_pain3_cortisolserum_out) ###compare this model with model 1 pain ~ age + sex###

###cortisol_saliva out: mod_pain3_cortisolaliva_out
#fit regression model
mod_pain3_cortisolsaliva_out = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_new)

#run regression including outliers
predict(mod_pain3_cortisolsaliva_out)

###look for outliers in model 3_SALIVA OUT###
#Cooks distance to see outliers
cooks.distance(mod_pain3_cortisolsaliva_out)
plot(mod_pain3_cortisolsaliva_out, which = 4) #plot cooks distance to see our outliers
#no outlier found with value greater than 1

#interpret results and regression weights
summary(mod_pain3_cortisolsaliva_out) #R^2 = 0.4315
AIC(mod_pain3_cortisolsaliva_out) #AIC = 460.05
confint(mod_pain3_cortisolsaliva_out)
lm.beta(mod_pain3_cortisolsaliva_out) ###compare this model with model 1 pain ~ age + sex###

#######cortisol saliva--> explains variance higher than if serum is included
######if serum is excluded than adjusted R^2 = .4728
######cortisol serum often describes as more reliable related to stress in medical research
######therefore (also based on literature) exclude cortisol saliva
#####-> final model : mod_pain3_cortisolsaliva_out

###Assumptions for mod_pain3_cortisaliva_out### --> REPORT 

#Normality assumption
#QQ plot
plot(mod_pain3_cortisolsaliva_out, which = 2)
# skew and kurtosis
describe(residuals(mod_pain3_cortisolsaliva_out)) #skew = -0.07
# histogram
hist(residuals(mod_pain3_cortisolsaliva_out), breaks = 20)
#No violation of Normality

#Linearity assumption
# redicted values against actual values
pred <- predict( object = mod_pain3_cortisolsaliva_out )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain3_cortisolsaliva_out, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain3_cortisolsaliva_out)
#No violation of Linearity, Tukey Test: -0.744 p>0.5 = no significant = no violation of linearity

#Homoscedasticty assumption (homogeneity of variance)
plot(mod_pain3_cortisolsaliva_out, which = 3)
ncvTest(mod_pain3_cortisolsaliva_out)
#p = 0.115 = >0.05, no violation of Homoscedasticity

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended 
vif(mod_pain3_cortisolsaliva_out)
pairs.panels(data_sample_new[,c("age", "sex", "STAI_trait", "pain_cat", "cortisol_serum" , "mindfulness")], col = "red", lm = T)
#No violation of Multicollinearity
#Vif values: age = 1.23, sex = 1.24, STAI = 2.39, Pain = 1.81, Cortisol_serum = 1.39, Mindfulness = 1.55


#Anova to compare model 1 with final model
anova(mod_pain1, mod_pain3_cortisolsaliva_out)
#P-value: <6.325e-15*** (p<0.001) models are highly significant from each other
#F(4,149) = 23.21

#AIC to compare models
AIC(mod_pain3_cortisolsaliva_out) #460.05
AIC(mod_pain1) #527.60
#final model explains variance of pain better

#########################################################
#                                                       #
#        Home assignement ZK research question 2        # 
#                                                       #
#########################################################

#loading data
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
#dataset = our new dataset with ppt. 15, 24, 25, 66 excluded = data_sample_new

##backward regression: running regression several times and exclude one variable after another
#to see which one has the most impact on pain: Book page 15.10.1
#define regression model: 
mod_back_pain <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + weight + mindfulness, data = data_sample_new) 

#results from initial model
summary(mod_back_pain)
AIC(mod_back_pain)
confint(mod_back_pain)
lm.beta(mod_back_pain)

#backward regression
step(mod_back_pain)

#save model as new object: output is best model after backward regression
mod_back_pain_best <- lm(formula = pain ~ age + pain_cat + cortisol_serum + mindfulness, 
                         data = data_sample_new)

#regression equation: Y = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4
#interpreting results from regression
summary(mod_back_pain_best) #Result: R square= 0.4374%
AIC(mod_back_pain_best) #Result: 456.5106
confint(mod_back_pain_best) #Result: 95% Confidenceintervals: up(age)-0.05, l(age):-0.12
lm.beta(mod_back_pain_best) #Regression weights: higher value, more impact on curve 

#####Checking Assumptions: Backward regression Model

#Normality assumption
#QQ plot
plot(mod_back_pain_best, which = 2)
# skew and kurtosis
describe(residuals(mod_back_pain_best)) #skew = -0.06
# histogram
hist(residuals(mod_back_pain_best), breaks = 20)
#No violation of Normality

#Linearity assumption
# redicted values against actual values
pred <- predict( object = mod_back_pain_best )
plot( x = pred, y = data_sample_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_back_pain_best, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_back_pain_best)
#No violation of Linearity, Tukey Test: -0.734,  p>0.5 = no significant = no violation of linearity

#Homoscedasticty assumption (homogeneity of variance)
plot(mod_back_pain_best, which = 3)
ncvTest(mod_back_pain_best)
#p = 0.123 = >0.05, no violation of Homoscedasticity

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended 
vif(mod_back_pain_best)
pairs.panels(data_sample_new[,c("age", "pain_cat", "cortisol_serum" , "mindfulness")], col = "red", lm = T)
#No violation of Multicollinearity


###Comparing Models###

#Comparing backward regression: mod_back_pain (inital model) and mod_back_pain_best (backward model)
AIC(mod_back_pain, mod_back_pain_best) #mod_back_pain_best slightly better AIC: 456.5106
#Anova regression
anova(mod_back_pain, mod_back_pain_best) #pValue: 0.7402 --> not significant
#don't vary significantly, no model is significantly better than another

###Comparison between mod_pain3 (theory based model) and mod_back_pain_best (backward model)
AIC(mod_pain3_cortisolsaliva_out, mod_back_pain_best)
##-AIC mod3: 460.05, Modbest: 456.51 
#########Backward model slighlty better than theory based model

#Anova regression
anova(mod_pain3_cortisolsaliva_out, mod_back_pain_best)
#interpret:
#AIC slightly different: 460.05(theory based model) und 456.51 (backward model) 
#p-Value: 0.8007: no significant difference between models 

#############################################################################

##### opening new dataset! #####
#Part 2 Research qu. 2: After this, you decide to put the two models to the test on some new data. 
#You collected new data from another 160 participants in the same way as you did in the 
#first study described in Assignment 1.
#apply theory based model on another dataset
home_sample_2 = read.csv ("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")
summary(home_sample_2)
describe(home_sample_2)

#->exclude ppt. scored on mindfulness below 1
home_sample_new<-home_sample_2[!home_sample_2$mindfulness<1,]
summary(home_sample_new)

###Compare the predicted values with the actual pain ratings. Which model was able to predict the actual pain ratings in data file 2 better?###
#fit regression models on new dataset (home_sample2)

##regression equations:
#mod_pain2(theory based model) y(pain)= b0 +b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 + b7*x7
#->pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness
#mod_back_pain_best (backward model) y(pain)= b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4
#->pain ~ age + pain_cat + cortisol_serum + mindfulness

##Report the prediction performance of the backward model 
#and the theory-based model on the new data (data file 2)##:
#test performance of backward regression model on theory-based model
#NOTE that we did not refit the models on the test set, we use the models fitted
#on the training set for the prediction

pred_test <- predict(mod_pain3_cortisolsaliva_out, home_sample_new) #deleting intervals, no need for that
pred_test_back <- predict(mod_back_pain_best, home_sample_new)

#calculate the sum of squared residuals
RSS_test = sum((home_sample_new["pain"] - pred_test)^2)
RSS_test_back = sum((home_sample_new["pain"] - pred_test_back)^2) 
RSS_test #230.06
RSS_test_back #232.24
#errors in theory based model: 230.06 are slightly lower than in backward model: 232.24

#Assumptions: theory based model is better on data file 1 and 2 because of less errors (RSS is lower)
#in clinical context maybe mod_back_pain_best because less variables: less money and time consuming
#but also: more variables,more to research about. On other hand: less variables, more precise but to do that you need to find out the most influencing factors

###########################################
                                          #
#####Research Question 3#####             # 
                                          #
###########################################

##Load data
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

#install packages
require(psych) #for describe
require(car) #for residualPlots, vif, pair.panels. ncvTest
require(ggplot2) #for ggplot
require(cAIC4) #for cAIC
require(r2glmm) #for r2beta
require(influence.ME) #for influence
require(lattice) #for qqmath
require(reshape2) #for melt function
require(lme4)
require(lmerTest)

#Check data
summary(data_sample_3)
describe(data_sample_3) 
who(T) #ok

#Histograms to illustrate the distribution 
hist(data_sample_3$pain_cat, breaks = 15)
hist(data_sample_3$cortisol_serum, breaks = 20) 
hist(data_sample_3$STAI_trait, breaks = 20) 
hist(data_sample_3$cortisol_saliva, breaks = 20) 
hist(data_sample_3$mindfulness, breaks = 20) 
hist(data_sample_3$weight, breaks = 20) 
hist(data_sample_3$pain1, breaks = 20) 
hist(data_sample_3$pain2, breaks = 20) 
hist(data_sample_3$pain3, breaks = 20) 
hist(data_sample_3$pain4, breaks = 20) 
#None looks normally distributed, but skew & kurtosis is fine, none above 1

#Show variables
names(data_sample_3)

#And designate which are the repeated measures
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")

#Correlation of repeated variables
cor(data_sample_3[,repeated_variables]) #Correlation lower the more time between ratings

#Transform data from wide to long format using melt function
data_pain_long = melt(data_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")
#order data frame by participant ID(not necessary, just makes the dataframe look more intuitive)
data_pain_long = data_pain_long[order(data_pain_long[,"ID"]),]
#change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)

#look how the data looks like in long format
data_pain_long

#######################################################
#                                                     #
#                    Analysis                         #
#                                                     #   
#######################################################

#mod_rep_int= random intercept model = Intercept(Intercept of function with Y axis) 
#Intercept can be random (variable) or fixed

#Build the mixed effect models
#Intercept model: random intercept, fixed slope
mod_rep_int = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (1|ID), data = data_pain_long)

# Slope model containing fixed effects, a random intercept as well as a random slope 
mod_rep_slope = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + (time|ID), data = data_pain_long)

#Results
summary(mod_rep_int)
summary(mod_rep_slope)

###Adjusted R squared to report:
r2beta(mod_rep_int, method="nsj") ##how to report those R2? , many R2 values 
r2beta(mod_rep_slope, method="nsj") 

###Model Comparison to see wheter using random slope or random intercept model###

#plot the regression line (prediction)
#save the predictions of both models to variables
data_pain_long$pred_int = predict(mod_rep_int)
data_pain_long$pred_slope = predict(mod_rep_slope)
# random intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)

#random slope and intercept model
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='blue', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)

#compare models with cAIC
cAIC(mod_rep_int)$caic #211.39
cAIC(mod_rep_slope)$caic #175.83
#mod_rep_slope better

#compare models with anova
anova(mod_rep_int, mod_rep_slope)
##models are significantly different: p value: 3.149e-05*** (p<0.001)
##Chi square (X2) = (2, 13) 20.73 , #Teststatistics

###adding a quadratic term of time to the slope model
#to account for curved relationship between time and pain rating
#since the slope model is better than only the intercept we continue working with slope model
mod_rep_slope_quad = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + time + I(time^2) + (time|ID), data = data_pain_long)

#Teststatistics report
summary(mod_rep_slope_quad)
confint(mod_rep_slope_quad)

#standardized coefficient (ß)
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
stdCoef.merMod(mod_rep_slope_quad)

#marginal R^2
r2beta(mod_rep_slope_quad, method="nsj") 

#plot the results
#save prediction of the model to new variable
data_pain_long$pred_slope_quad = predict(mod_rep_slope_quad)

#random slope model with quadratic term
ggplot(data_pain_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='green', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)
#random slope model including quadra. term of time best describes pain change over time

#compare models with cAIC
cAIC(mod_rep_slope)$caic #175.83
cAIC(mod_rep_slope_quad)$caic #121.96

# compare models with anova
anova(mod_rep_slope, mod_rep_slope_quad)
###highly significant p value 4.584e-08*** = <0.001
##Chi square (X2) = (1, 14) 29.89

###########################################################
#              Data and model diagnostics                 #
###########################################################

#checking for influential outliers
influence(mod_rep_slope_quad, group = "ID")$alt.fixed    #no influential outlier

influence(mod_rep_slope_quad, obs = T)$alt.fixed # this can take a minute or so
###if there would be a very influential case, you could spot it by having a look at the data (column I(time^2).
##varibility of the cases are really small so no influential outlier

####Checking assumptions####

###normality assumption
# QQ plot
qqmath(mod_rep_slope_quad, id=0.05) # this might require the lattice package, but you can get the same graph wit the qqnorm() function
#Output looks normal

###linearity assumption
# linearity of prediction and standardized residuals
plot(mod_rep_slope_quad)
#linearity of each predictor and the standardized residual
#visualize the linearity of connection of each predictor

predictors=c("sex","age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")

for(i in 1:length(predictors)){
  predictor_to_test = data_pain_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rep_slope_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}

#homoscedasticty assumption (homogeneity of variance), tests if variance is homogene
#look for funnel shape on this graph
plot(mod_rep_slope_quad)
#look at the overall model F and p, if it is significant, there may be violation of heteroscedasticity
summary(lm(residuals(mod_rep_slope_quad)^2 ~ data_pain_long[,"ID"]))
#p>.05, not significant, homoscedasticity not violated 

#multicollinearity
pairs.panels(data_pain_long[,c("sex", "age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")], col = "red", lm = T)
#no variable is highly correlated to each other.. highest value : -.59, (to exclude variable they need a correlation of .8)
#so there is no multicollinearity between the predictors
