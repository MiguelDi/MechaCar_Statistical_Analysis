#Deliverable 1: Linear Regression to Predict MPG.

#Load Dplyr Package
library(dplyr)

#Read Cvs File
MechCar <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

#Perform Multiple Linear Regression.
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MechCar)

#Perform Summary To Determinate P-value and R-Squared Value.
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MechCar))

#Deliverable 2: Create Visualizations for the Trip Analysis.
Suspen_Coil <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# Create a Dataframe total_summary using summarize() to get the mean, median, variance,
# and standard deviation of the suspension coil's PSI column.

Total_summary <- Suspen_Coil %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# Create a lot_Summary dataframe using group_by() and summarize() to group each
# manufacturing lot by mean, median, variance, and standard deviation.

lot_summary <- Suspen_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# Deliverable 3:  T-tests on Suspensions Coils
t.test(Suspen_Coil$PSI, mu=1500)

# T-test (Lot 1)
t.test(subset(Suspen_Coil,Manufacturing_Lot=="Lot1")$PSI, mu=1500)

# T-test (Lot 2)
t.test(subset(Suspen_Coil,Manufacturing_Lot=="Lot2")$PSI, mu=1500)

# T-test (Lot 3)
t.test(subset(Suspen_Coil,Manufacturing_Lot=="Lot3")$PSI, mu=1500)