## little calculations to grasp the difference in percentage yearly increase

# Define the initial amount
P10 <- 10  # 

P20 <- 20  # 

P30 <- 30  #

P40 <- 40

# Define the growth rates
r1 <- 0.005  # 0.6% yearly increase
r2 <- 0.004  # 0.4% yearly increase

# Define the time periods
t <- 100   # 100 years

# Calculate the amount after 100 years for both rates at 10 % initial cover 
P10_A1_t <- P10 * (1 + r1)^t
P10_A1_t
P10_A2_t <- P10 * (1 + r2)^t
P10_A2_t
cat("Difference:", P10_A1_t - P10_A2_t, "\n")

# Calculate the amount after 100 years for both rates at 20 % initial cover 
P20_A1_t <- P20 * (1 + r1)^t
P20_A1_t
P20_A2_t <- P20 * (1 + r2)^t
P20_A2_t
cat("Difference:", P20_A1_t - P20_A2_t, "\n")

# Calculate the amount after 100 years for both rates at 30 % initial cover 
P30_A1_t <- P30 * (1 + r1)^t
P30_A1_t
P30_A2_t <- P30 * (1 + r2)^t
P30_A2_t
cat("Difference:", P30_A1_t - P30_A2_t, "\n")

# Calculate the amount after 100 years for both rates at 40 % initial cover 
P40_A1_t <- P40 * (1 + r1)^t
P40_A1_t
P40_A2_t <- P40 * (1 + r2)^t
P40_A2_t
cat("Difference:", P40_A1_t - P40_A2_t, "\n")

