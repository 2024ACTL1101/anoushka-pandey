
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates.

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

#This creates a data frame with two column, including the close price and the date

amd_df <- amd_df[, c("date", "close")]

```



##Plotting the Data Plot the closing prices over time to visualize the price movement.

```{r plot}
#This plots the close price against the date
plot(amd_df$date, amd_df$close,'l')
```
**Code Explanations - Step One**
In step one, I loaded the data from the file  provided. Then, I converted the 'type' of each column by assigning the date as a 'Date type' and the adjusted close as the 'Numeric type'. Additionally, these columns were renamed as 'date' and 'close' respectively. Next, I used the plot function to plot the close price (y - axis) against the date (x- axis).


## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

-   Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
-   Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
    -   If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
    -   Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
    -   You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
    -   If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

```{r trading}

# Initialize and create  columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0.0  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
#Previous price refers to the price the day before
previous_price <- 0
#The share size has been set at a value of 100
share_size <- 100
#Accumulated shares is the total number of shares bought
accumulated_shares <- 0

#This is a for loop which will traverse each row in the data frame to implement the trading algorithm
#when applicable
for (i in 1:nrow(amd_df)) {
  #the current price is set to the close price of the current date 
  current_price <- amd_df$close[i]
  
  #This if statement implements the mechanism when it is the first day
  if(i == 1) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -current_price*share_size
    accumulated_shares <- accumulated_shares + share_size
  
  #This else- if statement implements the mechanism when the current price is lower than the previous price

  } else if (current_price < previous_price) {
     amd_df$trade_type[i] <- 'buy'
     amd_df$costs_proceeds[i] <- -share_size*current_price
     accumulated_shares <- accumulated_shares + share_size
  }
  # This if statement implements the algorithm when it is the last day
   if (i == nrow(amd_df)) {
      amd_df$trade_type[i] <- 'sell'
      amd_df$costs_proceeds[i] <- accumulated_shares*current_price
      accumulated_shares <- 0
   }
  #This updates the previous price and accumulated shares everytime the for loop traverses each day
  previous_price <- current_price
  amd_df$accumulated_shares[i] <- accumulated_shares
 
}

```
**Code Explanations - Step Two**
In step two, I implemented a loop and multiple if statements to traverse through each row of the data frame.

  1. Firstly, I created and initialised three more columns, including the trade type, accumulated shares and the costs proceeds.
  
  2. Next, I also created multiple new variable such as previous_price, accumulated_shares and share_size to help me implement the trading algorithm and keep track of all the data.
  
  3. I created a for loop with the condition 'i in 1:nrow(amd_df)' to ensure that it iterates through every row of the data frame to implement the trading algorithm
  
  4. The 'if(i == 1)' statement checks whether it is the first day of trading, and if the condition is  successfully met then the trading algorithm is implemented and 100 shares are bought.
  
  5. This 'else if (current_price < previous_price) ' statement checks the condition required for the trading algorithm to be implemented and if it is satsified then 100 shares will be bought

  6. This 'if (i == nrow(amd_df)) ' checks whether it is the last day of trading and if satisfied, all the shares will be sold.
  
  7. Finally, I update the variables and column values at the end of the 'for loop' before traversing the next row in the data frame 


  
## Step 3: Customize Trading Period

-   Define a trading period you wanted in the past five years

```{r period}

#create a new dataframe so we can customise the trading period
custom_trading_period <- read.csv("AMD.csv")
custom_trading_period$date <- as.Date(custom_trading_period$Date)
custom_trading_period$close <- as.numeric(custom_trading_period$Adj.Close)

custom_trading_period <- custom_trading_period[, c("date", "close")]

#The lines below allow the trading period to be customised from 2022-05-05 to 2023-05-05 
start_date <- as.Date('2022-05-05')
end_date <- as.Date('2023-05-05') 
custom_trading_period <- subset(custom_trading_period, date >= start_date & date <= end_date)

#Reset the values of each column
custom_trading_period$trade_type <- NA
custom_trading_period$costs_proceeds <- 0.0 
custom_trading_period$accumulated_shares <- 0  
new_accumulated_shares <- 0


for (i in 1:nrow(custom_trading_period)) {
  current_price_2 = custom_trading_period$close[i]
   if (i == nrow(custom_trading_period)) {
      custom_trading_period$trade_type[i] <- 'sell'
      custom_trading_period$costs_proceeds[i] <- new_accumulated_shares*current_price_2
      new_accumulated_shares = 0
  }
  else if(i == 1) {
    custom_trading_period$trade_type[i] <- 'buy'
    custom_trading_period$costs_proceeds[i] <- -current_price_2*share_size
    new_accumulated_shares <- new_accumulated_shares + share_size
    
    
  } else if (current_price_2 < previous_price2) {
     custom_trading_period$trade_type[i] <- 'buy'
     custom_trading_period$costs_proceeds[i] <- -share_size*current_price_2
     new_accumulated_shares <- new_accumulated_shares + share_size
  
  } 
 
  previous_price2 <- current_price_2
  custom_trading_period$accumulated_shares[i] <- new_accumulated_shares
}
 
```
**Code Explanations - Step Three**
This code section is quite similar to Step 2, however, the trading period has been customised, and thus the trading algorithm is only applied to a chosen time period.

  1. Firstly, I created a new data frame called custom_trading_period,  then I reset the columns and initialised three more columns, including the trade type, accumulated shares and the costs proceeds.
  
  2. Most importantly, I chose a start date of '2022-05-05' using the code 'start_date <- as.Date('2022-05-05')' and an end date 'end_date <- as.Date('2023-05-05')' (YYYY/MM/DD). Then, I removed all other  dates in the data frame that were not in the time period using 'subset(custom_trading_period, date >= start_date & date <= end_date)'
  
  3. Next, I also created multiple new variable such as previous_price2, current_price2,accumulated_shares and share_size to help me implement the trading algorithm and keep track of all the data.
  
  4. I created a for loop with the condition 'i in 1:nrow(custom_trading_period)' to ensure that it iterates through every row of the data frame to implement the trading algorithm
  
  5. The 'if(i == 1)' statement checks whether it is the first day of trading, and if the condition is  successfully met then the trading algorithm is implemented and 100 shares are bought.
  
  6. This 'else if (current_price2 < previous_price2) ' statement checks the condition required for the trading algorithm to be implemented and if it is satsified then 100 shares will be bought

  7. This 'if (i == nrow(custom_trading_period)) ' checks whether it is the last day of trading and if satisfied, all the shares will be sold.
  
  8. Finally, I update the variables and column values at the end of the 'for loop' before traversing the next row in the data frame 



## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

-   Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
-   Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
-   ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}

#declare new variables and initialise their value to 0
total_profit_or_loss <- 0
total_capital_invested <- 0
# sum the total profit or loss by summing all the cells in the costs proceeds column
total_profit_or_loss <- sum(custom_trading_period$costs_proceeds)
cat(round(total_profit_or_loss,2), "is the total profit. \n")
total_capital_invested <- -sum(custom_trading_period$costs_proceeds[custom_trading_period$costs_proceeds < 0])
cat(round(total_capital_invested,2), "is the total capital invested. \n")

#ROI
return_on_investment <- (total_profit_or_loss / total_capital_invested) *100
cat(round(return_on_investment,2), "% is return on investment. \n")

```
**Code Explanations - Step Four**
In Step 4, I implement a variety of code to calculate important financial metrics such as Total Profit/Loss, Total Capital Invested and Return on Investment.

1. I declared and initialised variables to store the Total Profit/Loss and the Total Capital Invested.

2. I calculated the Total Profit by using the sum function to add all the cells in the costs proceeds column, using the code ' sum(custom_trading_period$costs_proceeds)'. Then, I printed the result.

3. Similarly, I calculated the Total Capital Invested by only summing cost_proceeds < 0 and then printed out the result

4. To calculate ROI, I used the formula given above and then printed out the result


## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

-   Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
-   Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

```{r option}

# Create a new data frame 
strat_amd_df <- read.csv("AMD.csv")
strat_amd_df$date <- as.Date(strat_amd_df$Date)
strat_amd_df$close <- as.numeric(strat_amd_df$Adj.Close)

strat_amd_df <- strat_amd_df[, c("date", "close")]

# Define the trading period
start_date <- as.Date('2022-05-05')
end_date <- as.Date('2023-05-05')

strat_amd_df <- subset(strat_amd_df, date >= start_date & date <= end_date)
strat_amd_df$trade_type <- 0.0
strat_amd_df$costs_proceeds <- 0.0
strat_amd_df$accumulated_shares <- 0
strat_accumulated_shares <- 0
share_size <- 100  
profit_taking_percentage <- 1.15

avg_purchase_price <- 0.0
accum_cost <- 0.0
accum_shares <- 0

#Traverse each day of the customised trading period
for (i in 1:nrow(strat_amd_df)) {
  
  #Initialise the current price to be the close price of the current day
  strat_current_price <- strat_amd_df$close[i]
  
  #Implement previous strategy when it is the first day of period
  if (i == 1) {
    strat_amd_df$trade_type[i] <- 'buy'
    strat_amd_df$costs_proceeds[i] <- -strat_current_price * share_size
    strat_accumulated_shares <- strat_accumulated_shares + share_size

    #Sell all shares when it is the last day of the period
  } else if (i == nrow(strat_amd_df)) {
    strat_amd_df$trade_type[i] <- 'sell'
    strat_amd_df$costs_proceeds[i] <- strat_accumulated_shares * strat_current_price
    strat_accumulated_shares <- 0

    #Implement new profit- taking strategy where shares are sold when the current price is 15% more
    # than the average purchase
  } else if (strat_current_price > avg_purchase_price * profit_taking_percentage 
             && strat_accumulated_shares > 0) {
    #Calculate the shares sold by dividing the accumulated shares by 2
    shares_sold <- strat_accumulated_shares / 2
    #Modify the trade type to sell
    strat_amd_df$trade_type[i] <- 'sell'
    #Calculate the costs proceeds by multiplying shares sold by current price
    strat_amd_df$costs_proceeds[i] <- shares_sold * strat_current_price
    #Calculate the accumulated shares by subtracting the shares sold
    strat_accumulated_shares <- strat_accumulated_shares - shares_sold

    
  } else if (strat_current_price < strat_previous_price) {
    strat_amd_df$trade_type[i] <- 'buy'
    strat_amd_df$costs_proceeds[i] <- -share_size * strat_current_price
    strat_accumulated_shares <- strat_accumulated_shares + share_size
  }

  if (strat_amd_df$trade_type[i] == 'buy') {
    accum_cost <- avg_purchase_price*(strat_accumulated_shares - share_size) + (strat_current_price*share_size)
    avg_purchase_price <- accum_cost / strat_accumulated_shares
  }
  #Update the value of each variable for the date before next iteration of loop
  strat_previous_price <- strat_current_price
  strat_amd_df$accumulated_shares[i] <- strat_accumulated_shares
  strat_amd_df$average_cost[i] <- avg_purchase_price
  }


```
**Code Explanations - Step Five**
This code section implements another trade strategy in conjuction with the trading algorithm in Step 5.

  1. Firstly, I created a new data frame called strat_amd_df,  then I reset the columns and initialised three more columns, including the trade type, accumulated shares and the costs proceeds. Additionally, I reset the trade period to the custom dates chosen in Step 3.
  
  3. Next, I also created multiple new variable such as strat_previous_price, strat_current_price,strat_accumulated_shares, share_size, avg_purchase_price and profit_taking_percentage to assist me in implementing the new strategy.
  
  4. Once again, I created a for loop with the condition 'i in 1:nrow(custom_trading_period)' to ensure that it iterates through every row of the data frame to implement the trading algorithm. I set the strat_current_price to be equal to the close price of the current day.
  
  5. The 'if(i == 1)' statement checks whether it is the first day of trading, and if the condition is  successfully met then the trading algorithm is implemented and 100 shares are bought.
  6. This 'if (i == nrow(custom_trading_period)) ' checks whether it is the last day of trading and if satisfied, all the shares will be sold.
  
  7. Next, the  'else if (strat_current_price > avg_purchase_price * profit_taking_percentage 
&& strat_accumulated_shares > 0)' checks to see if the current price is greater than 1.15 x average price and accumulated shares are greater than 0. If these conditions are met, the new profit-taking strategy may be implemented. To implement this strategy, 'shares_sold' is calculated by dividing the acuumulated shares by half. Then, the trade type is changed to 'sell',  the cost proceeds are calculated and the accumulated shares is updated
  
  8. This 'else if (current_price2 < previous_price2) ' statement checks the condition required for the trading algorithm to be implemented and if it is satsified then 100 shares will be bought

  9. Next, I update the avg_purchase_price if shares have been bought. I calculate this by using the previous average purchase price, current price and updated accumulated shares which ensures that my average is correctly weighted.
  
  10. Finally, I update the variables and column values at the end of the 'for loop' before traversing the next row in the data frame 


## Step 6: Summarize Your Findings

-   Did your P/L and ROI improve over your chosen period?
-   Relate your results to a relevant market event and explain why these outcomes may have occurred.

```{r}
# Fill your code here and discuss
#Declare and initialise variables to store profit and capital invested

total_profit_or_loss_strat <- 0
total_capital_invested_strat <- 0

#Calculate the profit and capital invested using the sum functions
total_profit_or_loss_strat <- sum(strat_amd_df$costs_proceeds)
total_capital_invested_strat <- -sum(strat_amd_df$costs_proceeds[strat_amd_df$costs_proceeds < 0])

#Use values calculated above to compute the ROI, using formula (total profit)/ (capital invested) x 100
return_on_investment_2 <- (total_profit_or_loss_strat / total_capital_invested_strat) *100
#Print out the results
cat (round(total_profit_or_loss_strat,2), "is the total profit. \n")
cat(round(total_capital_invested_strat,2), "is the capital invested. \n")
cat(round(return_on_investment_2,2), "% is the return on investment. \n")
```
**Code Explanations - Step Six**

In Step 6, I implement code similar to Step 4 to calculate Total Profit/Loss, Total Capital Invested and Return on Investment.

  1. I declared and initialised variables to store the Total Profit/Loss and the Total Capital Invested.

  2. I calculated the Total Profit by using the sum function to add all the cells in the costs proceeds column, using the code ' sum(strat_amd_df$costs_proceeds)'. Then, I printed the result.

  3. Similarly, I calculated the Total Capital Invested by only summing cost_proceeds < 0 and then printed out the result

  4. To calculate ROI, I used the formula given above and then printed out the result and rounded it to two decimal places



Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.


 

## Discussion

**Strategy 1 :**   

The first strategy entailed the development of a trading algorithm aimed at purchasing shares on the initial trading day, then acquiring additional shares if the current day's price was lower than the preceding day's. All shares were to be selling on the final trading day. While this approach boasted simplicity in terms of implementation and execution, it bore certain limitations. Solely selling on the final day might prove suboptimal, neglecting potential higher prices at intermediate points. Furthermore, the absence of risk management measures within this trading algorithm leaves it susceptible to significant losses in the event of continued price decline. Overall, the results of this strategy can be seen below:  

Total Profit or Loss: $121015.90. 

Total Capital Invested: $1082840. 

Return on Investment: 11.18%. 

**Strategy 2:** 

Upon implementing Strategy 2, an additional profit-taking mechanism was introduced to build upon Strategy 1. This mechanism involves the strategic selling of assets, particularly shares and securities, when market prices experience an increase. In pursuing this strategy, a fixed percentage was selected to trigger the sale of shares, specifically at 15% above the average purchase price. This approach is advantageous as it maximises profit potential and mitigates the risk of price declines prior to selling. Moreover, the predetermined percentage ensures consistency in profit generation.  
Overall, the results of this strategy can be seen below:  

Total Profit or Loss: $169292.90. 

Total Capital Invested: $941923. 

Return on Investment: 17.97%. 


Therefore, it can be seen that Strategy 2 was successful as it allowed the ROI to increase from 11.18% to 17.97%, resulting in an approximate 38% increase in ROI. The ROI is a crucial financial metric which evaluates the profitability of an investment, where a higher ROI is considered a more profitable investment. It is an important metric which assists investors and businesses to make well-planned financial decisions by considering the expected efficiency/profitability of an investment. Additionally, the profit increased from $121015.90 to $169292.90 which is nearly a 40% increase. Furthermore, this outcome can be realised by examining the peak share price during the customised period, such as 2022-06-02, where shares were sold for approximately $108 which results in greater profitability, as opposed to Strategy 1 where we were only able to sell on the last day (2023-05-05) where share prices were not at the peak ($89.84).  

**Market Events:** 


Advanced Micro Devices, Inc. (AMD) is an American multinational corporation and semi-conductor which is known for developing computer processors and graphics technologies for business and consumer markets. Throughout 05/05/2022 to 05/05/2023 market events and AMD’s developments may have caused shifts in the share prices which ultimately impacted the ROI.  


Despite releasing and launching the expanded line of the ‘Radeon RX 6000’ series throughout in January and April of 2022 and experiencing high share prices of over $100, however a few months later, starting from 05/05/2022 the share prices for AMD became increasingly volatile and began to fluctuate, ranging from the mid $50s to low $90s (specifically see period 2022-06-01 to 2022-10-20). However, this was likely due to external events such as the Ukraine-Russia conflict. Although this may not have had a significant direct impact, AMD has been affected indirectly. This can be noted as AMD decided to ban all chip shipments to Russia in late March, and the effects of this persist in the long term throughout 2022 as seen through the lower share prices. Additionally, the conflict created uncertainty in the market which resulted in fluctuations in stock markets and contributed to lower share prices for AMD as many investors had reduced confidence in the semiconductor industry.  



However, share prices slowly began to recover at the end of 2022 and the beginning of 2023, mostly due to new advancements for AMD in the market. An example of this could be seen in the small jump in share prices from $58 to $60.11 on 2022-11-03 when the Radeon RX 7000 series GPU was announced, which resulted in share prices rising for the next few days. This can also be noted through observing the higher share prices throughout August 2022 after AMD officially published their lineup of CPUs based on the new Zen 4 microarchitecture. Additionally, the gradual improvement in share prices and ROI can also be attributed to factors such as the COVID recovery period which allowed the semiconductor industry to recover. As a result, AMD faced fewer supply chain issues and disruptions which lowered costs and manufacturing expenses. Furthermore, the exponential growth for AI and IoT applications has greatly encouraged growth within the semiconductor industry which has been felt by AMD and is seen through the rising share prices in early 2023.   


Thus, the increase in ROI calculated using the profit-taking strategy may be attributed to multiple market events and economic conditions.
