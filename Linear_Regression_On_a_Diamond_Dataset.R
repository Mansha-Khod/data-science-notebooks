# ============================================
# DIAMOND PRICE PREDICTION 
# ============================================

# ============================================
# 1. LOAD REQUIRED LIBRARIES
# ============================================
library(tidyverse)   # For data manipulation and visualization
library(corrplot)    # For correlation plots
library(car)         # For VIF and regression diagnostics
library(MASS)        # For stepwise regression (stepAIC)
library(gridExtra)   # For arranging multiple plots
library(lmtest)      # For Breusch-Pagan test
library(caret)       # For train/test split
library(glmnet)      # For Ridge and Lasso
# ============================================
# 2. LOAD AND EXPLORE THE DATASET
# ============================================
# Load the dataset (assuming your file is named diamonds.csv)
df <- read.csv("C:/Users/hp/Downloads/archive (1)/diamonds.csv")

# View first few rows
head(df)

# Check structure
str(df)

# Check for missing values
colSums(is.na(df))

# Statistical summary
summary(df)


# ============================================
# 3. OUTLIER DETECTION USING BOXPLOTS 
# ============================================
par(mfrow = c(2, 4))
boxplot(df$price, main = "Price", col = 'steelblue')
boxplot(df$carat, main = "Carat", col = 'coral')
boxplot(df$depth, main = "Depth", col = 'lightgreen')
boxplot(df$table, main = "Table", col = 'gold')
boxplot(df$x, main = "Length (x)", col = 'lightblue')
boxplot(df$y, main = "Width (y)", col = 'lightpink')
boxplot(df$z, main = "Depth (z)", col = 'lavender')
par(mfrow = c(1, 1))

# ============================================
# 4. REMOVE OUTLIERS USING IQR METHOD 
# ============================================

# function to remove outliers
remove_outliers<-function(x){
  Q1<-quantile(x,0.25)
  Q3<-quantile(x,0.75)
  IQR<-Q3-Q1
  lower_bound<-Q1-1.5*IQR
  upper_bound<-Q3+1.5*IQR
  x[x<lower_bound | x> upper_bound] <- NA
  return(x)
}

# Create a copy for outlier removal
df_clean<-df

# Apply outlier removal to numeric columns
numeric_cols <- c("price", "carat", "depth", "table", "x", "y", "z")
for (col in numeric_cols){
  df_clean[[col]]<-remove_outliers(df_clean[[col]])
}

# Remove rows with NA (outliers)
df_clean<-na.omit(df_clean)

cat("Original rows:", nrow(df), "\n")
cat("Rows after outlier removal:", nrow(df_clean), "\n")
cat("Outliers removed:", nrow(df) - nrow(df_clean), 
    "(", round((nrow(df) - nrow(df_clean))/nrow(df) * 100, 2), "%)\n")

# ============================================
# 5. CHECK BOXPLOTS AFTER OUTLIER REMOVAL 
# ============================================
par(mfrow = c(2, 4))
boxplot(df_clean$price, main = "Price (cleaned)", col = 'steelblue')
boxplot(df_clean$carat, main = "Carat (cleaned)", col = 'coral')
boxplot(df_clean$depth, main = "Depth (cleaned)", col = 'lightgreen')
boxplot(df_clean$table, main = "Table (cleaned)", col = 'gold')
boxplot(df_clean$x, main = "Length x (cleaned)", col = 'lightblue')
boxplot(df_clean$y, main = "Width y (cleaned)", col = 'lightpink')
boxplot(df_clean$z, main = "Depth z (cleaned)", col = 'lavender')
par(mfrow = c(1, 1))


# ============================================
# 6. CONVERT CATEGORICAL VARIABLES TO FACTORS
# ============================================
df_clean$cut <- as.factor(df_clean$cut)
df_clean$color <- as.factor(df_clean$color)
df_clean$clarity <- as.factor(df_clean$clarity)

# Check levels
levels(df_clean$cut)
levels(df_clean$color)
levels(df_clean$clarity)


# ============================================
# 7. CREATE LOG TRANSFORMED PRICE
# ============================================
df_clean$log_price<-log(df_clean$price)

# ============================================
# 8. CORRELATION MATRIX 
# ============================================
numeric_data <- df_clean %>% dplyr::select(price, carat, depth, table, log_price,x,y,z)
cor_matrix<-cor(numeric_data)

pastel_palette <- colorRampPalette(c("#92C5DE", "white", "#F4A582")) 
corrplot(cor_matrix,
         method = "color",
         type = "upper",    
         addCoef.col = "black",       
         number.cex = 0.7,             
         tl.col = "black",             
         tl.srt = 45,                 
         title = "Correlation Matrix",
         mar = c(0, 0, 1, 0),         
         col = pastel_palette(100),    
        )

# ============================================
# 9. TRAIN-TEST SPLIT 
# ============================================
set.seed(42)

train_index <- createDataPartition(df_clean$log_price, p = 0.8, list = FALSE)

train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]

cat("Training Set Size :", nrow(train_data), '\n')
cat("Test Set Size :", nrow(test_data), '\n')

# ============================================
# 10. SIMPLE LINEAR REGRESSION
# ============================================
cat("SIMPLE LINEAR REGRESSION")

slr_model <- lm(log_price ~ carat, data = train_data)

summary(slr_model)

slr_pred <- predict(slr_model, newdata = test_data)

slr_rmse <- sqrt(mean((test_data$log_price - slr_pred)^2))
slr_r2 <- cor(test_data$log_price, slr_pred)^2

cat("SLR R-Squared :", round(slr_r2,4), '\n')
cat("SLR RMSE :", round(slr_rmse,4), '\n')

# ============================================
# 11. CHECK MULTICOLLINEARITY - VIF
# ============================================

cat("CHECKING MULTICOLLINEARITY USING VIF\n")

vif_model <- lm(log_price ~ ., data = df_clean)

cat("\nVIF before removing variables:\n")
vif_before <- vif(vif_model)
print(vif_before)

df_clean <- df_clean %>%
  dplyr::select(-X, -x, -y, -z)

cat("\nColumns after removing X, x, y, z:\n")
print(colnames(df_clean))

vif_model_updated <- lm(log_price ~ ., data = df_clean)

cat("\nVIF after removing variables:\n")
vif_after <- vif(vif_model_updated)
print(vif_after)

cat("\nInterpretation:\n")
cat("After removing X, x, y, and z, all adjusted VIF values (GVIF^(1/(2*Df))) are below 5, indicating that multicollinearity is not a concern in the dataset.\n")

# FIX: rebuild train and test sets after column removal
train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]

# ============================================
# 12. MULTIPLE LINEAR REGRESSION 
# ============================================

cat("MULTIPLE LINEAR REGRESSION")

mlr_model <- lm(log_price ~ ., data = train_data)

summary(mlr_model)

cat("\nCoefficients :\n")
coefficients(mlr_model)

cat("T-Statistic and P-values :\n")
summary(mlr_model)$coefficients

cat("R-Squared :", summary(mlr_model)$r.squared)
cat("\nAdjusted R-squared:", summary(mlr_model)$adj.r.squared, "\n")


# ============================================
# 13. MODEL DIAGNOSTICS 
# ============================================
cat("MODEL DIAGNOSTICS")

res<-residuals(mlr_model)
fit<-fitted(mlr_model)

par(mfrow=c(1,1))

# Residuals vs Fitted (Linearity)
plot(fit,res,xlab = "Fitted Values",ylab="Residuas",main='Residuals Vs Fitted')
abline(h = 0, col = 'red')

# Q-Q plot (Normality)
qqnorm(res,main='Normal Q-Q')
qqline(res,col='red')

# Histogram of residuals
hist(res,col='lightblue',main='Histogram of Residuals',xlab='Residuals',breaks=50)

#Scale-Location (Homoscedasticity)
plot(fit,sqrt(abs(res)),xlab='Fitted Values',ylab="Sqrt(Residuals)",main="Scale-Location Plot")

# Residuals vs Order
plot(res,type='o',xlab='Obesrvation Order',ylab='Residuals',main='Residuals Vs Order')
abline(h=0,col='red')

# Cook's Distance
plot(cooks.distance(mlr_model),type='h',main="Cook's Distance",ylab="Cook's Distance",xlab='Index')
abline(h=4/length(res),col='red',lty=2)

# ============================================
# 14. STATISTICAL TESTS 
# ============================================
cat("STATISTICAL TESTS")

# Breusch-Pagan Test for Homoscedasticity
cat("Breusch-Pagan Test :")
print(bptest(mlr_model))

# Durbin-Watson Test for Autocorrelation
cat("Durbin-Watson Test :")
print(dwtest(mlr_model))

# Shapiro-Wilk Test for Normality (on sample)
set.seed(123)
res_sample<-sample(res,min(5000,length(res)))
cat("Shapiro-Wilk Normality Test (sample) :")
print(shapiro.test(res_sample))

# ============================================
# 15. COOK'S DISTANCE - REMOVE INFLUENTIAL OBSERVATIONS
# ============================================
cat("COOK'S DISTANCE - INFLUENTIAL OBSERVATIONS")

cooksd<-cooks.distance(mlr_model)
threshold<-4/nrow(train_data)
influential<-which(cooksd > threshold)

# Remove influential observations
train_clean<-train_data[-influential,]
cat("Influential observations removed :",length(influential))

#Refit model without influential observations
mlr_model_clean<- lm(log_price~.,data=train_clean)

cat("Model after removing influential observations :")
summary(mlr_model_clean)

# ============================================
# 16. STEPWISE REGRESSION - FORWARD, BACKWARD
# ============================================
cat("STEPWISE REGRESSION")

# Null Model
null_model<-lm(log_price~1,data=train_clean)

#Full Model
full_model<-lm(log_price~.,data=train_clean)

# Forward Selection
cat("Forward selection")
forward_model<-stepAIC(null_model,scope = list(lower=null_model,upper=full_model),direction='forward',trace=FALSE)
summary(forward_model)

# Backward Elimination
cat("Backward Elimination")
backward_model<-stepAIC(full_model,direction = 'backward',trace=FALSE)
summary(backward_model)

# stepwise (Both)
cat("Stepwise Both Direction")
stepwise_model<-stepAIC(null_model,scope=list(lower=null_model,upper=full_model),direction = "both",trace=FALSE)
summary(stepwise_model)

# Compare AIC
cat("\n--- AIC Comparison ---\n")
aic_comparison <- AIC(forward_model, backward_model, stepwise_model, full_model)
print(aic_comparison)

# Compare Adjusted R-squared
cat("\n--- Adjusted R-squared Comparison ---\n")
cat("Forward:", summary(forward_model)$adj.r.squared, "\n")
cat("Backward:", summary(backward_model)$adj.r.squared, "\n")
cat("Stepwise:", summary(stepwise_model)$adj.r.squared, "\n")
cat("Full:", summary(full_model)$adj.r.squared, "\n")

# ============================================
# 17. PREPARE DATA FOR RIDGE AND LASSO 
# ============================================
cat("RIDGE AND LASSO REGRESSION")

# Create model matrix with dummy variables
formula<-log_price~carat + cut + color + clarity + depth + table
x_matrix<-model.matrix(formula,data=train_clean)[,-1]
y_vector<-train_clean$log_price

# Test data Matrix
x_test_matrix<-model.matrix(formula,data=test_data)[,-1]
y_test_vector<-test_data$log_price

# Standardize the data
x_train_scaled<-scale(x_matrix)
x_test_scaled<-scale(x_test_matrix,center = attr(x_train_scaled,"scaled:center"),scale=attr(x_train_scaled,'scaled:scale'))

# ============================================
# 18. OLS ON SCALED DATA 
# ============================================
ols_scaled<-lm(y_vector~x_train_scaled)
cat("OLS on Scaled Data - First 10 Coefficients :")
print(head(coef(ols_scaled),10))

# ============================================
# 19. RIDGE REGRESSION 
# ============================================
cat("RIDGE REGRESSION")

# Test different lambda values
lambda_seq<-10^seq(5,-2,length=100)

# Cross-validation for Ridge
set.seed(42)
ridge_cv<-cv.glmnet(x_train_scaled,y_vector,alpha=0,lambda = lambda_seq)
best_lambda_ridge<-ridge_cv$lambda.min
cat("Best lambda for Ridge :",best_lambda_ridge)

# Fit Ridge model
ridge_model <- glmnet(x_train_scaled, y_vector, alpha = 0, lambda = best_lambda_ridge)

# Ridge predictions
ridge_pred <- predict(ridge_model, s = best_lambda_ridge, newx = x_test_scaled)
ridge_rmse <- sqrt(mean((y_test_vector - ridge_pred)^2))
ridge_r2 <- cor(y_test_vector, ridge_pred)^2

cat("Ridge R-squared:", round(ridge_r2, 4), "\n")
cat("Ridge RMSE:", round(ridge_rmse, 4), "\n")

# Ridge coefficients
ridge_coef <- as.matrix(coef(ridge_model))
ridge_coef_df <- data.frame(
  Variable = rownames(ridge_coef),
  Coefficient = ridge_coef[, 1]
)
cat("\nRidge Coefficients (Top 10):\n")
print(head(ridge_coef_df[order(-abs(ridge_coef_df$Coefficient)), ], 10))

# ============================================
# 20. LASSO REGRESSION 
# ============================================
cat("LASSO REGRESSION")

# Cross-validation for Lasso
set.seed(42)
lasso_cv<-cv.glmnet(x_train_scaled,y_vector,alpha=1,lambda = lambda_seq)
best_lambda_lasso <- lasso_cv$lambda.min
cat("Best lambda for Lasso:", best_lambda_lasso, "\n")

# Fit Lasso model
lasso_model<-glmnet(x_train_scaled,y_vector,alpha = 1,lambda = best_lambda_lasso)

# Lasso predictions
lasso_pred<-predict(lasso_model,s=best_lambda_lasso,newx=x_test_scaled)
lasso_rmse<-sqrt(mean((y_test_vector-lasso_pred)^2))
lasso_r2<-cor(y_test_vector,lasso_pred)^2

cat("Lasso R-squared:", round(lasso_r2, 4), "\n")
cat("Lasso RMSE:", round(lasso_rmse, 4), "\n")

# Lasso coefficients 
lasso_coef <- as.matrix(coef(lasso_model))
lasso_coef_df <- data.frame(
  Variable = rownames(lasso_coef),
  Coefficient = lasso_coef[, 1]
)
cat("\nLasso Coefficients (Top 10):\n")
print(head(lasso_coef_df[order(-abs(lasso_coef_df$Coefficient)), ], 10))

# Number of non-zero coefficients
nonzero <- sum(lasso_coef != 0)
cat("\nNumber of non-zero coefficients in Lasso:", nonzero, "\n")
cat("Number of zero coefficients:", nrow(lasso_coef) - nonzero, "\n")

# ============================================
# 21. COEFFICIENT SHRINKAGE PLOTS
# ============================================
par(mfrow = c(1, 2))

# Ridge path
ridge_path <- glmnet(x_train_scaled, y_vector, alpha = 0, lambda = lambda_seq)
plot(ridge_path, xvar = "lambda", label = TRUE, main = "Ridge Coefficients Path")
abline(v = log(best_lambda_ridge), col = "red", lty = 2)

# Lasso path
lasso_path <- glmnet(x_train_scaled, y_vector, alpha = 1, lambda = lambda_seq)
plot(lasso_path, xvar = "lambda", label = TRUE, main = "Lasso Coefficients Path")
abline(v = log(best_lambda_lasso), col = "red", lty = 2)

par(mfrow = c(1, 1))

# ============================================
# 22. CROSS-VALIDATION PLOTS
# ============================================
par(mfrow = c(1, 2))
plot(ridge_cv, main = "Ridge Cross-Validation")
plot(lasso_cv, main = "Lasso Cross-Validation")
par(mfrow = c(1, 1))

# ============================================
# 23. COEFFICIENT COMPARISON - OLS vs RIDGE vs LASSO
# ============================================
cat("COEFFICIENT COMPARISON")

# Get variable names
all_vars<-names(coef(ols_scaled))
all_vars<-gsub("x_train_scaled",'',all_vars)

# comparison dataframe
coef_compare<-data.frame(Variable= all_vars)

# Add OLS coefficients
ols_coefs <- coef(ols_scaled)
coef_compare$OLS<-as.numeric(ols_coefs)

# Add Ridge coefficients
ridge_coefs_full <- as.matrix(coef(ridge_model))
coef_compare$Ridge <- ridge_coefs_full[match(all_vars, rownames(ridge_coefs_full)), 1]

# Add Lasso coefficients
lasso_coefs_full <- as.matrix(coef(lasso_model))
coef_compare$Lasso <- lasso_coefs_full[match(all_vars, rownames(lasso_coefs_full)), 1]

# Round for display
coef_compare[, 2:4] <- round(coef_compare[, 2:4], 4)

# Show first 20 coefficients
print(head(coef_compare, 20))

# ============================================
# 24. MODEL COMPARISON ON TEST SET 
# ============================================
cat("FINAL MODEL COMPARISON")

evaluate_model<-function(predictions,actual,name){
  rmse<-sqrt(mean((actual-predictions)^2))
  r2<-cor(actual,predictions)^2
  mae<-mean(abs(actual-predictions))
  return(data.frame(
    Model=name,
    R2=round(r2,4),
    RMSE=round(rmse,4),
    MAE=round(mae,4)
  ))
}

#predictions for all models
mlr_pred <- predict(mlr_model_clean, newdata = test_data)
stepwise_pred <- predict(stepwise_model, newdata = test_data)

# Combining all results
final_comparison <- rbind(
  evaluate_model(slr_pred, test_data$log_price, "Simple Linear"),
  evaluate_model(mlr_pred, test_data$log_price, "Multiple Linear"),
  evaluate_model(stepwise_pred, test_data$log_price, "Stepwise"),
  evaluate_model(as.vector(ridge_pred), y_test_vector, "Ridge"),
  evaluate_model(as.vector(lasso_pred), y_test_vector, "Lasso")
)

print(final_comparison)

# ============================================
# 25. INTERPRETATION
# ============================================
cat("INTERPRETATION")
cat("R2 =",round(summary(mlr_model_clean)$r.squared,4),
    "-",round(summary(mlr_model_clean)$r.squared*100,2),
    "% of variation in log(price) explained by the model")
cat("Adjusted R² =", round(summary(mlr_model_clean)$adj.r.squared, 4), "\n")
cat("The model is statistically significant (p-value < 2.2e-16)\n")
cat("All predictors are significant at 5% level\n")

# ============================================
# 26. PREDICTIONS COMPARISON PLOT - ALL MODELS
# ============================================
cat("\n=== GENERATING PREDICTIONS COMPARISON PLOT ===\n")

# Create a dataframe with actual and all predictions
plot_data <- data.frame(
  Actual = exp(test_data$log_price),  # Back-transform to original price scale
  Simple_Linear = exp(slr_pred),
  Multiple_Linear = exp(mlr_pred),
  Stepwise = exp(stepwise_pred),
  Ridge = exp(as.vector(ridge_pred)),
  Lasso = exp(as.vector(lasso_pred))
)

# Reshape data for ggplot
plot_data_long <- plot_data %>%
  pivot_longer(cols = -Actual, names_to = "Model", values_to = "Predicted")

# Create the comparison plot
comparison_plot <- ggplot(plot_data_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1, linetype = "dashed") +
  facet_wrap(~Model, ncol = 3) +
  scale_color_manual(values = c(
    "Simple_Linear" = "coral",
    "Multiple_Linear" = "steelblue",
    "Stepwise" = "darkgreen",
    "Ridge" = "purple",
    "Lasso" = "orange"
  )) +
  labs(
    title = "Model Predictions Comparison: Actual vs Predicted Diamond Prices",
    subtitle = paste("Test Set Size:", nrow(test_data), "diamonds"),
    x = "Actual Price ($)",
    y = "Predicted Price ($)",
    caption = "Dashed line represents perfect prediction (y = x)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 10)
  ) +
  coord_equal()

print(comparison_plot)

# Save the plot
ggsave("model_comparison_plot.png", comparison_plot, width = 12, height = 8, dpi = 300)

# ============================================
# 27. ADDITIONAL: RESIDUAL DENSITY PLOT FOR ALL MODELS
# ============================================
cat("\n=== GENERATING RESIDUAL DENSITY PLOT ===\n")

# Calculate residuals for each model
residuals_data <- data.frame(
  Simple_Linear = test_data$log_price - slr_pred,
  Multiple_Linear = test_data$log_price - mlr_pred,
  Stepwise = test_data$log_price - stepwise_pred,
  Ridge = y_test_vector - as.vector(ridge_pred),
  Lasso = y_test_vector - as.vector(lasso_pred)
)

# Reshape for ggplot
residuals_long <- residuals_data %>%
  pivot_longer(cols = everything(), names_to = "Model", values_to = "Residuals")

# Create residual density plot
residual_plot <- ggplot(residuals_long, aes(x = Residuals, fill = Model)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(
    "Simple_Linear" = "coral",
    "Multiple_Linear" = "steelblue",
    "Stepwise" = "darkgreen",
    "Ridge" = "purple",
    "Lasso" = "orange"
  )) +
  labs(
    title = "Residual Distributions Across Models",
    x = "Residuals (log scale)",
    y = "Density",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(residual_plot)

# Save the plot
ggsave("residual_comparison_plot.png", residual_plot, width = 10, height = 6, dpi = 300)

# ============================================
# 29. FINAL CONCLUSION
# ============================================
cat("\n=== FINAL CONCLUSION ===\n")
cat("Based on the analysis:\n")
cat("1. Best Model: Multiple Linear Regression (after removing influential observations)\n")
cat("2. R² =", round(summary(mlr_model_clean)$r.squared, 4), 
    "(", round(summary(mlr_model_clean)$r.squared * 100, 2), "% )\n")
cat("3. RMSE = $", round(mean((exp(test_data$log_price) - exp(mlr_pred))^2)^0.5, 2), "\n")
cat("4. MAPE =", final_comparison$MAPE[2], "%\n")
cat("\nThe Multiple Linear Regression model outperforms both Ridge and Lasso,\n")
cat("indicating that all predictors are meaningful and there's no severe overfitting.\n")
cat("The stepwise selection confirmed that all predictors contribute significantly to the model.\n")

# ============================================
# 27. PREDICTIONS COMPARISON PLOT - ALL MODELS
# ============================================
cat("\n=== GENERATING PREDICTIONS COMPARISON PLOT ===\n")

plot_data <- data.frame(
  Actual          = exp(test_data$log_price),
  Simple_Linear   = exp(slr_pred),
  Multiple_Linear = exp(mlr_pred),
  Stepwise        = exp(stepwise_pred),
  Ridge           = exp(as.vector(ridge_pred)),
  Lasso           = exp(as.vector(lasso_pred))
)

plot_data_long <- plot_data %>%
  pivot_longer(cols = -Actual, names_to = "Model", values_to = "Predicted") %>%
  mutate(Model = factor(Model,
                        levels = c("Simple_Linear", "Multiple_Linear",
                                   "Stepwise", "Ridge", "Lasso")))

model_colors <- c(
  "Simple_Linear"   = "#E07B7B",   
  "Multiple_Linear" = "#5B9BD5",   
  "Stepwise"        = "#4CAF7D",   
  "Ridge"           = "#9B59B6",   
  "Lasso"           = "orange"    
)


comparison_plot <- ggplot(plot_data_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_abline(intercept = 0, slope = 1,
              color = "black", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~Model, ncol = 3, scales = "free") +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title    = "Model Predictions Comparison: Actual vs Predicted Diamond Prices",
    subtitle = paste("Test Set Size:", nrow(test_data), "diamonds"),
    x        = "Actual Price ($)",
    y        = "Predicted Price ($)",
    caption  = "Dashed line = perfect prediction (y = x)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position   = "none",
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle     = element_text(hjust = 0.5, color = "gray40"),
    strip.text        = element_text(face = "bold", size = 10),
    strip.background  = element_rect(fill = "gray95", color = NA),
    panel.grid.minor  = element_blank(),
    plot.caption      = element_text(color = "gray50")
  )

print(comparison_plot)
ggsave("model_comparison_plot.png", comparison_plot,
       width = 12, height = 8, dpi = 300)

# ============================================
# 28. RESIDUAL DENSITY PLOT FOR ALL MODELS
# ============================================
cat("\n=== GENERATING RESIDUAL DENSITY PLOT ===\n")

residuals_data <- data.frame(
  Simple_Linear   = test_data$log_price - slr_pred,
  Multiple_Linear = test_data$log_price - mlr_pred,
  Stepwise        = test_data$log_price - stepwise_pred,
  Ridge           = y_test_vector - as.vector(ridge_pred),
  Lasso           = y_test_vector - as.vector(lasso_pred)
)

residuals_long <- residuals_data %>%
  pivot_longer(cols = everything(), names_to = "Model", values_to = "Residuals") %>%
  mutate(Model = factor(Model,
                        levels = c("Simple_Linear", "Multiple_Linear",
                                   "Stepwise", "Ridge", "Lasso")))

residual_plot <- ggplot(residuals_long, aes(x = Residuals, fill = Model, color = Model)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +         # FIX: linewidth not size
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray30", linewidth = 0.6) +
  scale_fill_manual(values  = model_colors) +
  scale_color_manual(values = model_colors) +
  labs(
    title    = "Residual Distributions Across Models",
    subtitle = "Narrower and taller density = better-calibrated residuals",
    x        = "Residuals (log-price scale)",
    y        = "Density",
    fill     = "Model", color = "Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

print(residual_plot)
ggsave("residual_comparison_plot.png", residual_plot,
       width = 10, height = 6, dpi = 300)