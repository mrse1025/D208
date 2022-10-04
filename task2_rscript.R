library(tidyverse)
library(broom)
library(PerformanceAnalytics)
library(caret)
library(pROC)
source("cleaner_copy.R")


#Bringing in the data and cleaning the variable names by removing the capitals letters and replaces spaces with _. 
#The cleaning function removes outlier observations that are more or less than 3 standard deviations from the mean. 
#The clearning functions also imputes missing values by using the mean or median, which were applied based on applicability. 
data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  janitor::clean_names() %>%                          
  churn_cleaning() 

data_selected <- data %>% 
                select(churn, outage_sec_perweek, port_modem, device_protection, tenure, 
                       monthly_charge, bandwidth_gb_year)

#data reduction based on variables that can be influenced by stakeholders or have a perceived influence 
#on churn outcome. 

reduced_scaled <- data_selected %>% 
                mutate(across(where(is.numeric) &!c(churn), log1p))%>% 
                mutate(churn = as.numeric(ifelse(data$churn == 'Yes',1,0)))

write.csv(reduced_scaled, "reduced_scaled_csv")

logit_1 <- glm(churn ~ ., data = reduced_scaled, family = "binomial")

summary(logit_1)

var_imp <- varImp(logit_1, scale = TRUE) %>% View() #scale = TRUE complete the normalization step. 

#The next reduction was based on variable importance, any variable with a value greater than one was selected. 

reduction <- reduced_scaled %>%
                select(churn, bandwidth_gb_year, tenure, monthly_charge, techie, item3:item5)
                
#univariate visualizations
boxplot(reduction %>% select_if(is.numeric))
        
factor_viz <- reduction %>% select_if(is.factor)

ggplot(data = factor_viz, aes(x = techie)) +
  geom_bar()

logit_2 <- glm(churn ~ ., data = reduction, family = "binomial")
summary(logit_2)

logit_final <- glm(churn ~ bandwidth_gb_year + tenure + monthly_charge + techie, data = reduced_scaled, family = "binomial")
summary(logit_final)


#ROC and AUC
plot(x = reduction$monthly_charge, y = reduction$churn)
glm.fit = glm(reduction$churn ~ reduction$monthly_charge, family = binomial)
#lines(reduction$monthly_charge, glm.fit$fitted.values)
par(pty = "s")
roc(reduction$churn, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
    xlab = "False Positive Percent", ylab = "True Positive Percent", print.auc = TRUE)
plot.roc(reduction$churn, logit_1$fitted.values, percent = TRUE, col = "red", print.auc = TRUE, add = TRUE, print.auc.y = 70)
plot.roc(reduction$churn, logit_2$fitted.values, percent = TRUE, col = "blue", print.auc = TRUE, add = TRUE, print.auc.y = 85)


#confusion matrix
predicted <- as.factor(predict(logit_final, reduction, type = "response"))  #without as.factor, class is numeric

#sensitivity <- sensitivity(data$churn, predicted)  input data must have the same two levels???

