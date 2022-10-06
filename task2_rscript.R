library(tidyverse)
library(broom)
library(PerformanceAnalytics)
library(caret)
library(MASS)
library(pROC)
source("cleaner_copy.R")


#Bringing in the data and cleaning the variable names by removing the capitals letters and replaces spaces with _. 
#The cleaning function removes outlier observations that are more or less than 3 standard deviations from the mean. 
#The clearning functions also imputes missing values by using the mean or median, which were applied based on applicability. 
data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  janitor::clean_names() %>%                          
  churn_cleaning() 

data_no_demo <- data %>% mutate(churn = as.numeric(ifelse(data$churn == "Yes", 1, 0))) %>% select(children:item8)

#Selecting Variables 

aov <- aov(formula = churn ~ . , data = data_no_demo)
summary(aov)

#Using the p-value and the potential for influence by stakeholders the following variables from the AOV results
#have been selected to be used in the model: email, techie, contract, internet_service, phone, multiple, online_backup,
#online_protection, tech_support, streaming_tv, streaming_movies, payment_method, tenure, monthly_charge

data_selected_scaled <- data_no_demo %>% 
                select(churn, email, techie, contract, internet_service, phone, multiple, online_backup,
                       device_protection, tech_support, streaming_tv, streaming_movies, payment_method, 
                       tenure, monthly_charge)%>%
                mutate(across(where(is.numeric) &!c(churn), log1p))

#Summary Statistics 
skimr::skim(data_selected_scaled)

num_var <- data_selected_scaled %>% select_if(is.double)%>% colnames() %>% as.list()

#univariate visualizations
for (i in 1:length(num_var)) {
  g <- ggplot(data = data_selected_scaled, aes(data_var[[i]])) +
    geom_histogram()+
    xlab( num_var[[i]])
  print(g)
}

factor_var <- data_selected_scaled %>% select_if(is.factor) %>% colnames() %>% as.list()

for (i in 1:length(factor_var)) {
  f <- ggplot(data = data_selected_scaled, aes(data_selected_scaled[[i]])) +
    geom_bar()+
    xlab( factor_var[[i]])
  print(f)
}

boxplot(data_selected_scaled %>% select_if(is.numeric))

#Preparing the Data

one_hot_dummy <- dummyVars(" ~.", data = (data_selected_scaled %>% select_if(is.factor))) 
dummy_df <- data.frame(predict(one_hot_dummy, newdata = (data_selected_scaled %>% select_if(is.factor))))

final_data_set <- dummy_df %>% bind_cols(data_selected_scaled) %>% #put together numeric with dummied variables
                  select(-(email:payment_method))%>%  #remove all non-dummied variables 
                  janitor::clean_names()

write.csv(final_data_set, "final_data_set.csv")

logit_1 <- glm(churn ~ ., data = final_data_set, family = "binomial") 
summary(logit_1)
coef(logit_1)
print(logit_1$aic)

step_model <- logit_1 %>% stepAIC (trace = FALSE)
coef(step_model)
print(step_model$aic)


#ROC and AUC
roc(final_data_set$churn, logit_1$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
xlab = "False Positive Percent", ylab = "True Positive Percent", print.auc = TRUE,print.auc.y = 95)
plot.roc(final_data_set$churn, step_model$fitted.values, percent = TRUE, col = "blue", print.auc = TRUE, add = TRUE, print.auc.y = 85)


#confusion matrix
predicted <- as.factor(predict(logit_final, reduction, type = "response"))  #without as.factor, class is numeric

#sensitivity <- sensitivity(data$churn, predicted)  input data must have the same two levels???

