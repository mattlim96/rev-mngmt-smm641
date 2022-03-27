# Setting up the LP Formulation:
# This problem has two decision variables
# x1: Amount of Red Ripe to produce
# x2: Amount of Sassy Spicy to produce
# Objective Function Coefficients
# Profit contribution of Red Ripe is 3.
# Profit contribution of Sassy Spicy is 2. obj.fun <- c(3,2);
# Constraint Coefficients
constr <- matrix(c(1,2,2,1,-1,1,0,1), ncol=2, byrow=TRUE) # Alternatively, and especially for larger problems,
# you can build the constraint coefficient matrix in parts # and combine them afterwards. For example,
# Pepper usage: x1+2*x2
pepper<-c(1,2)
# Tomato usage: 2*x1+x2
tomato<-c(2,1)
# Diversity: -x1+x2
diversity<-c(-1,1)
# Demand: x2
demand<-c(0,1) constr<-rbind(pepper,tomato,diversity,demand)
# Constraint directions: Equality and/or Inequality # For each row of constraint, indicate the
# constraint direction, "<=", ">=", or "=". constr.dir <- c("<=", "<=", "<=", "<=")
# Constraint Right Hand Side
# For each row of constraint, indicate the
# corresponding right hand side value.
# First two are pepper and tomato availability: # x1+2*x2 <= 6 and 2*x1+x2 <= 8
# The next is for diversity (-x1+x2 <= 1)
# The last is for demand (x2 <= 2)
rhs <- c(6,8,1,2)