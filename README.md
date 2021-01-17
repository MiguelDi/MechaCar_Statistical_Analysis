# MechaCar_Statistical_Analysis

## Overview
The purpose of this analysis is to review the production data for insights that may help the manufacturing team.  To facilitate the analysis, the following tasks were applied:
- Perform a multiple linear regression analysis to identify which variables in the dataset predict the MPG of MechCar prototypes.
- Collect summary statistics on the pounds per square inch (PSI) of the suspencion coils from the manufacturing lots.
- Perform t-tests to determine if the manufacturing lots are statistically different from the mean population.
- Design a statistical study to compare vehicle performance of the MechaCar vehicles against vehicles from other manufacturers.  Write a summary interpretation of the findings 
for each statistical analysis.

## Resources
- Data Source:
  - Suspension_Coil.csv [Suspension_Coil.csv](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Suspension_Coil.csv)
  - MechCar_mpg.csv [MechaCar.csv](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/MechaCar_mpg.csv)
  - Rstudio
  - Dyplyr

## Linear Regression to Predict MPG

### Linear Regression Model for all variables:
![Linear Regression](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/Linear_Regression.png)

### Statistical Summary of the linear Regression Model with the Intended P-Values.
![Summary PR](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/Summary_P_R.png)

### Summary
The MechaCar_mpg.csv dataset was used to perform this linear progression.  The dataset contains mpg tests for 50 prototype MechaCars. Multiple metrics, such as vehicle length, vehicle weight, spoiler angle, drivetrain, and ground clearance were collected for each vehicle.  A linear model was designed to predict the MPG of MechaCar prototypes using the aforementioned variables.

- Which variables/coefficients provided a non-random amount of variance to the mpg values?
 - Two variables, vehicle length and ground clearance showed a non-random amount of variance and therefore have a significant impact on MPG.  All other coefficients, vehicle weight, spoiler angle and drive train showed a random amount of variance.  It should be also noted that the intercept (or contant) was also statistically significant and shows that there are other variables and factors that impact the MPG.  These variables may or may not be contained within the dataset.
- Is the slope of the linear model considered to be zero?
  - The slope of the linear model is not zero since at least two of independent coefficients (vehicle length and ground clearance) had a significant impact on MPG.
- Does this linear model predict mpg of MechaCar prototypes effectively? 
  - The multiple R-Squared value of 0.7149 which means that the model will predict the MPG correctly at least 71% of the time. As noted earlier by our intercept which is statistically significant, there are other variables and factors that may or not be contained within the dataset that help to improve this models success at MPG prediction.

## Summary Statistics on Suspension Coils

### Total Summary Chart
![Total Summary](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/Total_SummaryDe2.png)

### Lot Summary Chart
![Lot Summary](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/Lot_Summary2.png)

### Summary
The MechaCar Suspension_Coil.csv dataset contains the results from multiple productions lots.  In this dataset, the weight capaciities of multiple suspension coils were tested to determine if the manufacturing process is consistance across production lots.  A summary statistics table was designed to show:
- The supsensions coil's PSI continuous variable across all manufacturing lots.
- The following PSI metrics for each lot:
  - mean
  - median
  - variance
  - standard deviation
- The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually?
 - Reviewing the summary statistics of all three manufacturing lots, a variance of 62.29356 for PSI is well within the specifications of 100 lbs PSI.  However, while manufacturing lots 1 and 2 are significantly well within design specifications, manufacturing lot 3's variance of 170.286 PSI is  far outside the design specifications of 100 lbs PSI.

## T-Tests on Suspension Coils

### T-Test All Manufacturing Lots
![T-test(All Lots)](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/T_TestALL.png)

### T-Test Lot 1
![lot 1](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/T_Testlot1.png)

### T-Test Lot 2
![Lot 2](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/T_testlot2.png)

### T-Test Lot 3
![Lot 3](https://github.com/MiguelDi/MechaCar_Statistical_Analysis/blob/main/Resources%20/T_testlot3.png)

### T-Test Summary of Findings

- Using the t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
 - The p-value for all manufacturing lots was .06028 and above the significance level of 0.05 therefore the summary of all manufacturing lots are statistically similar.
- Using the t.test() function and its subset() argument, determine if the PSI for each individual manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch.
 - Lot 1
  - The p-value for manufacturing lot 1 was 1.0 and well above the significance level of 0.05, therefore the two means are statistically similar.
 - Lot 2
  - The p-value for manufacturing lot 2 was 0.6072 and above the significance level of 0.05, therefore the two means are statistically similar.
 - Lot 3
  - The p-value for manufacturing lot 3 was 0.04168 and is below the significance level of 0.05, therefore the two means are statistically not similar. 

## Study Design: MechaCar vs Competition

Suggested statistical study that can quantify how the MechaCar performs against the competition. In your study design, think critically about what metrics would be of interest to a consumer: for a few examples, cost, city or highway fuel efficiency, horse power, maintenance cost, or safety rating.
- What metric or metrics are you going to test?
- What is the null hypothesis or alternative hypothesis?
- What statistical test would you use to test the hypothesis? And why?
- What data is needed to run the statistical test?

Suggested statistical study that can quantify how the MechaCar cost ver safety rating performs against the competition.  The hypothesis is that there are significant differences in performance between MechCar and their competition.  To perform the study, data from both MechaCar and all competitive companies would needed for cost of vehicle and the  overall saftely rating as well as federal and state requirements for overall safety standards.

To perform this analysis we would do a simple linear regression for each car company comparing the cost of their vehicles to their safetly rating. We would also do a t-test to find out if the mean safety rating for each company is equal to the mean federal overall safety standards.  After these findings we would use a chi-squared test to discover is there categorical frequencies between the different manufacturers.


