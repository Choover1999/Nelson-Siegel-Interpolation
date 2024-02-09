#Building the Nelson Siegel Interpolation Curve

Nelson_Siegel_path <- "C:/Users/charl/Downloads/HW1_data_for_Problem_4_S24.csv"

Nelson_Siegel_data <- read.csv(Nelson_Siegel_path)

#Setting initial constants
principal <- 100
B_0 <- 0.1
B_1 <- 0.1
B_2 <- 0.1
lamda <- 0.1

#Find Number of payments remaining and add it to the data table

for (i in 1:length(Nelson_Siegel_data$Time.to.maturity)) {
  Nelson_Siegel_data$Number_pmt_remaining[i] <- 1 + (Nelson_Siegel_data$Time.to.maturity[i] -Nelson_Siegel_data$Time.to.next.payment[i]) *2
}

Nelson_Siegel_data$Coupon.rate <- as.numeric(sub("%", "", Nelson_Siegel_data$Coupon.rate)) / 100 #Convert the coupon rate to a percentage in decimal form

#Calculate the dirty price of the bond

for (i in 1:length(Nelson_Siegel_data$Time.to.maturity)) {
  delta <- 0.5
  coupon <- delta * principal * Nelson_Siegel_data$Coupon.rate[i]
  Nelson_Siegel_data$Dirty_Price[i] <- Nelson_Siegel_data$Clean.Price[i] + (coupon * (delta - Nelson_Siegel_data$Time.to.next.payment[i]) / delta)
}

#Calculate the value of each coupon payment

for (i in 1:length(Nelson_Siegel_data$Time.to.maturity)) {
  delta = 0.5
  Nelson_Siegel_data$coupon_pmt[i] <- delta * principal * Nelson_Siegel_data$Coupon.rate[i]
}


#Create a function for time remaining to next payment

time_to_pmt <- function(time_to_first_coupon, coupon_number) {
  year_time <- time_to_first_coupon + (coupon_number - 1) * delta
  return(year_time)
}

#Create objective function to be minimized

minimized <- function(parameters) {
  B_0 <- parameters[1]
  B_1 <- parameters[2]
  B_2 <- parameters[3]
  lamda <- parameters[4]
  #Create a function of the Nelson Siegel model
  
  Nelson_Siegel_function <- function(time) {
    NS_yield <- B_0 + B_1 * ((1 - exp(-lamda * time)) / (lamda * time)) + B_2 * (((1 - exp(-lamda * time)) / (-lamda * time)) - exp(-lamda * time))
    return(NS_yield)
  }
  
  #Calculate the Theoretical Price of each bond using the initial parameters
  
  for (i in 1:length(Nelson_Siegel_data$Time.to.maturity)) {
    Coupon_Value <- 0
    for (j in 1:Nelson_Siegel_data$Number_pmt_remaining[i]){
      Coupon_Value <- Nelson_Siegel_data$coupon_pmt[i] * exp(-Nelson_Siegel_function(time_to_pmt(Nelson_Siegel_data$Time.to.next.payment[i], j)) * (time_to_pmt(Nelson_Siegel_data$Time.to.next.payment[i], j))) + Coupon_Value
    }
    Principal_Value <- principal * exp(-Nelson_Siegel_function(Nelson_Siegel_data$Time.to.maturity[i]) * Nelson_Siegel_data$Time.to.maturity[i])
    Nelson_Siegel_data$Nelson_Siegel_Theoretical_Price[i] <- Principal_Value + Coupon_Value
  }
  sum_squared_diff <- sum((Nelson_Siegel_data$Dirty_Price - Nelson_Siegel_data$Nelson_Siegel_Theoretical_Price)^2)
  return(sum_squared_diff)
}

#Set initial guess for minimizer function

init_guess <- c(B_0, B_1, B_2, lamda)

#Set constraints for variables

lower_bounds <- c(0, -Inf, 0, 0)  
upper_bounds <- c(Inf, Inf, Inf, Inf)

#Apply minimzer function to find optimal constants

result <- optim(par = init_guess, fn = minimized, method = "L-BFGS-B",
                lower = lower_bounds, upper = upper_bounds)

# Extract the optimal parameters

optimal_parameters <- result$par

#Estimate of parameters and plot of the curve

# Use the optimal parameters obtained from the optimization
optimal_B_0 <- optimal_parameters[1]
optimal_B_1 <- optimal_parameters[2]
optimal_B_2 <- optimal_parameters[3]
optimal_lamda <- optimal_parameters[4]

# Define the Nelson Siegel function with optimal parameters
optimal_Nelson_Siegel_function <- function(time) {
  NS_yield <- optimal_B_0 + optimal_B_1 * ((1 - exp(-optimal_lamda * time)) / (optimal_lamda * time)) +
    optimal_B_2 * (((1 - exp(-optimal_lamda * time)) / (-optimal_lamda * time)) - exp(-optimal_lamda * time))
  return(NS_yield)
}

# Plot the Nelson Siegel function using the curve function
curve(optimal_Nelson_Siegel_function(x), from = 0, to = 30, col = "blue", lwd = 2,
      xlab = "Time", ylab = "Nelson Siegel Yield",
      main = "Nelson Siegel Function with Optimal Parameters")

cat("Optimal B_0:", optimal_B_0, "Optimal B_1:", optimal_B_1, "Optimal B_2:", optimal_B_2, "Optimal Lamda:", optimal_lamda, "\n")