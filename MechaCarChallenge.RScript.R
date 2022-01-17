## Challenge - Deliverable 1

# add dplyr package 
library(dplyr)

# load in CSV
mecha_mpg <-read.csv(file='./Resources/MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(mecha_mpg)

# perform linear regression 
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)

# perform summary to determine p-value and r-squared value for linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)) 

## Challenge - Deliverable 2 

# load in CSV
suspension_coil <- read.csv(file = './Resources/Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
head(suspension_coil)

#summarize the dataframe 
full_summary <- suspension_coil %>% summarize(Mean = mean(PSI), 
                                              Median = median(PSI), 
                                              Variance = var(PSI), 
                                              SD = sd(PSI))

#summarize by lot
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), 
                                                                             Median = median(PSI), 
                                                                             Variance = var(PSI), 
                                                                             SD = sd(PSI), .groups = 'keep')

## Challenge - Deliverable 3

# Run T-Tests on suspension coils 

# t-test to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 PSI
t.test(suspension_coil$PSI, mu=1500)

#t-tests to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 PSI
t.test(subset(suspension_coil,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(suspension_coil,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(suspension_coil,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)