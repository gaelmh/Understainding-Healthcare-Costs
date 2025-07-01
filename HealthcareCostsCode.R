# Libraries
library(broom)
library(car)
library(caret)
library(dplyr)
library(fastDummies)
library(lmtest)
library(MASS)
library(psych)
library(ResourceSelection)
library(tidyverse)

setwd("C:/Users/gaelm/OneDrive - DePaul University/Gael/Escuela/Classes/3. Junior/2. Winter/Generalized Linear Models/FinalProject")

set.seed(123)

# Data
  data <- read.csv("insurance.csv")
  view(data)

# Normality
  # Descriptive statistics
    desc <- describe(data)
    write.csv(desc, file = "descriptive_statistics.csv", row.names = TRUE)
    
  # Frequency of categoricals
    # Sex
    sex_freq <- table(data$sex)
    
    sex_percent <- prop.table(sex_freq) * 100
    
    sex_distribution <- data.frame(
      Number = c(as.numeric(sex_freq), sum(sex_freq)),
      Percentage = c(as.numeric(sex_percent), 100)
    )
    
    rownames(sex_distribution) <- c("Female", "Male", "Total")
    
    write.csv(sex_distribution, file = "sex_dist.csv", row.names = TRUE)
    
    # Smoker
    smoker_freq <- table(data$smoker)
    
    smoker_percent <- prop.table(smoker_freq) * 100
    
    smoker_distribution <- data.frame(
      Number = c(as.numeric(smoker_freq), sum(smoker_freq)),
      Percentage = c(as.numeric(smoker_percent), 100)
    )
    
    rownames(smoker_distribution) <- c("No", "Yes", "Total")
  
    write.csv(smoker_distribution, file = "smoker_dist.csv", row.names = TRUE)
    
    # Region
    region_freq <- table(data$region)
  
    region_percent <- prop.table(region_freq) * 100
    
    region_distribution <- data.frame(
      Number = c(as.numeric(region_freq), sum(region_freq)),
      Percentage = c(as.numeric(region_percent), 100)
    )
    
    rownames(region_distribution) <- c("Northeast", "Northwest", "Southeast", "Southwest", "Total")
    
    write.csv(region_distribution, file = "region_dist.csv", row.names = TRUE)
    
  # Histograms
    hist(data$age, main = "Age Distribution", xlab = "Age")
    hist(data$bmi, main = "BMI Distribution", xlab = "BMI")
    hist(data$children, breaks = (min(data$children)-0.5):(max(data$children)+0.5), main = "Num. Children Distribution", xlab = "Num. Children")
    hist(data$charges, main = "Insurance Charge Distribution", xlab = "Charges")

  # Normalizing
    transformed_data <- data
    
    # Log Transformations
    transformed_data$log_charges <- log(data$charges)
    transformed_data$log_children <- log(data$children + 1) # +1 where children = 0
    
    # Square Root Transformations
    transformed_data$sqrt_charges <- sqrt(data$charges)
    transformed_data$sqrt_children <- sqrt(data$children)
    
    view(transformed_data)
    
    # Histogram comparison
      # Charges - Use LOG transformation
      ggplot(transformed_data, aes(x = log_charges)) +
        geom_histogram(binwidth = 0.65) +
        labs(title = "Histogram of Log-transformed Charges")
      
      ggplot(transformed_data, aes(x = sqrt_charges)) +
        geom_histogram() +
        labs(title = "Histogram of Root-transformed Charges")
    
      # Children - Use LOG transformation
      ggplot(transformed_data, aes(x = log_children)) +
        geom_histogram(binwidth = 1) +
        labs(title = "Histogram of Log-transformed Children")
    
      ggplot(transformed_data, aes(x = sqrt_children)) +
        geom_histogram(binwidth = 1) +
        labs(title = "Histogram of Root-transformed Children")
      
    # QQ comparison
      # Children
      ggplot(transformed_data, aes(sample = log_children)) +
        stat_qq() +
        labs(title = "QQ Plot of Log-transformed Children")
    
      ggplot(transformed_data, aes(sample = sqrt_children)) +
        stat_qq() +
        labs(title = "QQ Plot of Root-transformed Children")
    
    # Add log transformed variables to original dataset
      data <- cbind(data, transformed_data[, c("log_charges", "log_children")])
      view
      
  # Multicollinearity
    # Categoricals as factors
    data$sex <- as.factor(data$sex)
    data$smoker <- as.factor(data$smoker)
    data$region <- as.factor(data$region)
    
    #Test
    lm_model <- lm(charges ~., data = data)
    
    # Values
    vif_values <- car::vif(lm_model)
    view(vif_values)
    
    write.csv(vif_values, file = "vif.csv", row.names = TRUE)
    
    # Remove log_children and re-test
    data <- data[-c(9)]
    view(data)

    #Test
    lm_model2 <- lm(charges ~., data = data)
    
    # Values
    vif_values2 <- car::vif(lm_model2)
    view(vif_values2)
    
    write.csv(vif_values2, file = "vif2.csv", row.names = TRUE)
    
  # Linear Regression testing
    # Prepare variables
    dataLin <- data
    dataLin$sex <- ifelse(dataLin$sex == "female", 1, 0)
    dataLin$sex <- as.factor(dataLin$sex)
    dataLin$smoker <- ifelse(dataLin$smoker == "yes", 1, 0)
    dataLin$smoker <- as.factor(dataLin$smoker)
    dataLin$region <- ifelse(dataLin$region == "northeast", 1,
                          ifelse(dataLin$region == "northwest", 2,
                                 ifelse(dataLin$region == "southeast", 3,4)))
    dataLin$region <- as.factor(dataLin$region)
    
    # Testing
    weights <- ifelse(dataLin$smoker == "yes", 0.8, 0.2)
    
    weighted_linear_model <- lm(log_charges ~ age + sex + region + bmi + log_children + smoker, data = dataLin, weights = weights)
    
    summary(weighted_linear_model)
  
  # Logistic Regression testing for Costs
    # Prepare variables
    dataLog <- data
    dataLog$sex <- ifelse(dataLog$sex == "female", 1, 0)
    dataLog$sex <- as.factor(dataLog$sex)
    dataLog$smoker <- ifelse(dataLog$smoker == "yes", 1, 0)
    dataLog$smoker <- as.factor(dataLog$smoker)
    dataLog$region <- ifelse(dataLog$region == "northeast", 1,
                          ifelse(dataLog$region == "northwest", 2,
                                 ifelse(dataLog$region == "southeast", 3,4)))
    dataLog$region <- as.factor(dataLog$region)
    dataLog$new_charges <- ifelse(dataLog$charges >= 9500, 1, 0)
    
    # Create train and test data
    N = nrow(dataLog)
    n = round(N*0.8, 0)
    splitdata <- sample(1:nrow(dataLog), n, replace = FALSE)
    train_data <- dataLog[splitdata,] 
    test_data <- dataLog[-splitdata,]
    
    # Selection of logistic model
    null_model <- glm(new_charges ~ 1, train_data, family = binomial)
    summary(null_model)
    
    full_model <- glm(new_charges ~ . -charges, train_data, family = binomial)
    summary(full_model)
    
    adjusted_model <- glm(new_charges ~ age + bmi + smoker, train_data, family = binomial)
    summary(adjusted_model)
    
    # Best fit
    LLNull <- logLik(null_model) 
    LLFull <- logLik(full_model)
    LLAdjusted <- logLik(adjusted_model)
    
    AIC_values <- AIC(null_model, full_model, adjusted_model)
    BIC_values <- BIC(null_model, full_model, adjusted_model)
    
    best_fit <- data.frame(
      Model  = c("Null Model", "Full Model", "Adjusted Model"),
      LogLikelihood = c(as.numeric(LLNull), as.numeric(LLFull), as.numeric(LLAdjusted)),
      AIC = AIC_values$AIC,
      BIC = BIC_values$BIC
    )
    best_fit
    
    # Goodness of fit
    hl_gof <- hoslem.test(full_model$y, fitted(full_model))
    hl_gof
    
    # Predictions
    test_data$predictions <- predict(full_model, newdata = test_data, type = "response")
    head(test_data$predictions)
    
    # Confusion Matrix 
    confusionMatrix(as.factor(test_data$predictions), as.factor(test_data$new_charges), mode="everything", positive="1")
    
  # ANOVA Testing based on Sex, Smoker, Region
    anova <- aov(charges ~ sex + smoker + region, data = data)

    summary(anova)
    
    # Testing again with weights for Smoker
    weights <- ifelse(data$smoker == "yes", 0.8, 0.2)
    
    # Fit a linear model with weighted ANOVA
    weighted_lm <- lm(charges ~ sex + smoker + region, data = data, weights = weights)
    
    # Display ANOVA table
    anova_weighted <- aov(weighted_lm)
    
    summary(anova_weighted)
    
    # Tukey results
    tukey <- TukeyHSD(anova_weighted)
    
    tukey
    
  # ANOVA testing with interactions
    # Change factors to numerical
    data <- cbind(ifelse(data$sex == "female", 1, 0),data)
    data <- cbind(ifelse(data$smoker == "yes", 1, 0),data)
    data <- cbind(fastDummies::dummy_cols(data$region),data)
    
    data <- data[-c(1,9,12:13,15)]
    data <- data %>% rename_at(1, ~'northeast')
    data <- data %>% rename_at(2, ~'northwest')
    data <- data %>% rename_at(3, ~'southeast')
    data <- data %>% rename_at(4, ~'southwest')
    data <- data %>% rename_at(5, ~'smoker')
    data <- data %>% rename_at(6, ~'sex')
    
    int <- data$sex * data$smoker * data$northeast * data$northwest * data$southeast * data$southwest
  
    anovaint <- aov(charges ~ sex + smoker + northeast + northwest + southeast + southwest + int, data = data)
    
    summary(anovaint)
    
    # Testing again with weights for Smoker
    weights <- ifelse(data$smoker == 1, 0.2, 0.8)
    
    # Fit a linear model with weighted ANOVA
    weighted_lmint <- lm(charges ~ sex + smoker + northeast + northwest + southeast + southwest, data = data, weights = weights)
    
    # Display ANOVA table
    anova_weightedint <- aov(weighted_lmint)
    
    summary(anova_weightedint)
    
    # Tukey results
      # Set as factors so Tukey runs correctly
      data$sex <- as.factor(data$sex)
      data$smoker <- as.factor(data$smoker)
      data$northeast <-  as.factor(data$northeast)
      data$northwest <- as.factor(data$northwest)
      data$southeast <-  as.factor(data$southeast)
      data$southwest <- as.factor(data$southwest)
      
      tukeyint <- TukeyHSD(anova_weightedint)
      
      tukeyint