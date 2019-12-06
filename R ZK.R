library(lsr)
library(psych)
library(car)
library(HSAUR2)
library(sciplot)
library(tools)
library(tidyverse)
library(lmtest)
library(sandwich)
library(gridExtra)
library(lm.beta)

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")

View(data_sample_1)

describe(data_sample_1)

plot(pain ~ age, data = data_sample_1)

clean_data <- data_sample_1 %>% mutate(
  household_income = as.numeric(replace(household_income, household_income == "-4562",45620)),
  STAI_trait = as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35)))

mod1 <- lm(pain ~ sex + age, data = clean_data)
print(mod1)

mod1 %>% 	
  residualPlots()

# QQ plot	
mod1 %>% 	
  plot(which = 2)	
# histogram

mod1%>% 	
  bptest() # Breush-Pagan test

mod1 %>% 
  vif() MultiC

AIC(mod1)	

summary(mod1)

describe(residuals(mod1))

mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = clean_data)	

summary(mod2)

mod2 %>% 	
  residualPlots()

# QQ plot	
mod2 %>% 	
  plot(which = 2)	
# histogram

mod2%>% 	
  bptest() # Breush-Pagan test

mod2 %>% 
  vif() MultiC


####Multicoll found, removing SERUM 

mod3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = clean_data)	


mod3 %>% 
  vif()	#Multicoll

# QQ plot	
mod3 %>% 	
  plot(which = 2)	
# histogram

mod3 %>% 	
  residualPlots()

mod3%>% 	
  bptest() # Breush-Pagan test	


describe(residuals(mod3))	

mod3 %>% #COOKS DISTANCE
  ggplot() +	
  aes(x = pain, y = STAI_trait + pain_cat + cortisol_saliva + mindfulness) +	
  geom_point() +	
  geom_smooth(method = "lm")	

mod3 %>% 	
  plot(which = 5)	

mod3 %>% 	
  plot(which = 4)

cooks.distance(mod3)
summary(mod3)
describe(residuals(mod3))
AIC(mod3)	

sm=summary(mod1)
sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	

sm_table = cbind(as.data.frame(round(cbind(coef(mod1), confint(mod1), c(0, lm.beta(mod1)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	



sm_table	 


sm=summary(mod3)
sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	

sm_table = cbind(as.data.frame(round(cbind(coef(mod3), confint(mod3), c(0, lm.beta(mod3)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	

sm_table	 
anova(mod1, mod3)

