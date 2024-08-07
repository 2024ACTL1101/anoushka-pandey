

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# Initialise columns returns_AMD and returns_GSPC
df$returns_AMD <- 0.0
df$returns_GSPC <- 0.0

# Use a for loop to calculate the daily returns for AMD and S&P 500
for (i in 1:nrow(df)) {
  if(i == 1) {
    df$returns_AMD[i] <- 0.0
    df$returns_GSPC[i] <- 0.0
    
  } else {
   df$returns_AMD[i] =  (df$AMD[i] - df$AMD[i - 1]) / df$AMD[i - 1]
   df$returns_GSPC[i] =  (df$GSPC[i] - df$GSPC[i - 1]) / df$GSPC[i - 1]
   
  }
}
```

## Code Explanation 

- Initialize and create the columns for `returns_AMD` and `returns_GSPC`
- Make sure that the first row is initialised as '0'
- Use a for-loop to traverse each row and calculate the returns



- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
    # Initialise and create a column for daily risk free rates
    df$daily_risk_free_rate <- 0.0
    # Use a for loop to traverse through each row and
    # calculate the daily risk-free rate as per the formula above
    for (i in 1:nrow(df)) {
      df$daily_risk_free_rate[i] = (1 +  (df$RF[i]/100))^(1/360) - 1
    }
```

## Code Explanation 

- Initialise and create the column for daily risk free rate.
- Use a for-loop to traverse each row and calculate the daily risk free rate using the formula given above.


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
#fill the code
    # Initialise and create a column for excess AMD returns
    df$excess_AMD_returns <- 0.0
    # Initialise and create a column for excess S&P 500 returns
    df$excess_GSPC_returns <- 0.0
    # Use a for loop to traverse through each row and update the two columns
    for (i in 2:nrow(df)) {
        df$excess_AMD_returns[i] =  as.numeric(df$returns_AMD[i], digits=30) - 
        as.numeric(df$daily_risk_free_rate[i], digits=30)
        df$excess_GSPC_returns[i] =   as.numeric(df$returns_GSPC[i], digits=30) - 
        as.numeric(df$daily_risk_free_rate[i], digits=30)
    }

```
## Code Explanation 

- Initialise and create the columns for `excess_returns_AMD` and `excess_returns_GSPC`
- Use a for-loop, starting from row 2, to traverse each row and calculate the returns using the formula given

- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Create a model with excess AMD returns and excess S&P 500 returns
model_AMD_GSPC <- lm(df$excess_AMD_returns ~ df$excess_GSPC_returns, data = df)
(summary(model_AMD_GSPC))
beta1 <- summary(model_AMD_GSPC)$coefficients[2,1]

sprintf("The beta value is %f ", beta1)

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The beta value in a CAPM model indicates how volatile a stock's price is in comparison to the overall stock market.It is often characterised as a measurement of its volatility of returns relative to the entire market and is used as a measure of risk. A company with a higher beta has greater risk and also greater expected returns.The beta value calculated for this model was 1.5700013.If beta is greater than 1 this indicates a stock's price is more volatile than the overall market, if beta is less than 1 then stock's price is less volatile than the market and if beta is equal to 1 the stock moves identically to the overall market. 

The beta value calculated by this model was approximately 1.57, which is greater than 1 and indicates that the AMD's stock price is more volatile than the overall market.This indicates that for every percentage point the market increases, AMD would increase by 1.57 percentage points and the reverse occurs when the market falls. Since volatility measures how quickly markets move, it is an important factor investors must consider as it allows them to assess the level of risk as a high beta may increase the risk of a portfolio, but it may also increase gains. 
 
Additionally, we can ensure there is a significant statistical relationship that exists between excess AMD returns and excess GSPC returns as the p-value is 2e-16, which is significantly less than the threshold of 5%. Thus, we can reject the proposition of the null hypothesis wherein there is no relationship between excess AMD returns and the excess S&P 500 returns.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Use ggplot to plot excess S&P 500 excess returns against AMD returns
ggplot(df, aes(x = excess_GSPC_returns, y = excess_AMD_returns)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between excess AMD returns and excess GSPC returns",
       y = "Excess AMD Returns",
       x = "Excess GSPC Returns")
```


### Step 3: Predictions Interval



*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
# Extracting beta0 and beta1 values from summary table
beta0 <- summary(model_AMD_GSPC)$coefficients[1,1]
beta <- summary(model_AMD_GSPC)$coefficients[2,1]

# Initialise and assign values of the expected S&P 500 returns and risk free rate
expected_gspc_return <- 13.3/100
risk_free_rate <- 5/100
expected_AMD_return <- risk_free_rate + beta * (expected_gspc_return - risk_free_rate)

# Compute the number of 'samples'
n <- nrow(df) 
# Compute the mean of excess S&P 500 returns
mean_gspc <- mean(df$excess_GSPC_returns)

# Use the standard error formula to calculate standard error estimate
se <- sqrt(sum(residuals(model_AMD_GSPC)^2) / (n - 1 - 1))

# Calculate the Sum of Squares of X
SSX <- sum((df$excess_AMD_returns - mean_gspc)^2)

# Calculate sf_daily
sf_daily <- se * sqrt(1 + 1/n + (expected_gspc_return - mean_gspc)^2 / SSX)

# Multiply sf_daily by the square root of s52 to compute sf_annual
sf_annual <- sf_daily * sqrt(252)

# Assign alpha with 0.1 for 90% prediction interval
alpha <- 0.1

# Obtain t value for 90% prediction level with n - 1 - 1 degrees of freedom
t_value <- qt(1 - alpha / 2, df = n - 2)

# Calculating upper and lower bound of interval
(lower_bound <- expected_AMD_return - t_value * sf_annual)
(upper_bound <- expected_AMD_return + t_value * sf_annual)

lower_bound_percent <- lower_bound*100
upper_bound_percent <- upper_bound *100

# Print the results 
cat(sprintf("The 90%% prediction interval for AMD's annual return is [%f%%, %f%%].
", lower_bound, upper_bound))

cat(sprintf("When rounded and converted to a percentage this is [%.2f%%, %.2f%%].",
lower_bound_percent, upper_bound_percent))


```
## Code Explanation 

- First, I extracted the Beta_1 value from the summary table.
- I then initialized the GSPC return rate and the risk-free rate to 0.133 and 0.050, respectively.
- Using the given CAPM equation, I calculated the expected AMD return.
-  Next, I determined the number of rows and computed the mean GSPC excess return.
- Subsequently, I calculated the standard error (SE) and the sum of squares of X (SSX) to use in the equation for SF_daily.
- To find SF_annual, I multiplied SF_daily by the square root of 252.
-  Finally, after calculating the t-value, I successfully determined the lower and upper bounds of the confidence interval, by initialising 'alpha' to be 0.1.




## Final Discussion:

The confidence interval for the expected return is given as approximately [-49, 85]. This implies that with a 90% level of confidence, the true expected return lies somewhere between -49% and 85%. This wide interval suggests high uncertainty in the expected return estimate.

This can have many implications as the wide prediction interval indicates high volatility and uncertainty in the asset's returns. Additionally, this may make the asset less attractive to risk-averse investors, who  prefer more stable investments with narrower prediction intervals. Despite the risks associated, the wide prediction interval also indicates the possibility for higher returns. We must also consider the beta value of 1.57 which suggests that the asset is more sensitive to market movements. For every 1% change in the market return, AMD's return is expected to change by 1.57%. This high beta further emphasises the potential for both higher returns and higher risks compared to the market.

