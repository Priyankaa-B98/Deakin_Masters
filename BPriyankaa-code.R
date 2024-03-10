

                                          Title:  'ASSESSMENT 3 - END-TERM ASSESSMENT' 


####################################################################################################
                                                     'QUESTION 2'
####################################################################################################

# A factory makes three products called Bloom, Amber, and Leaf, from three materials containing Cotton, Wool and Nylon. The following table provides details on the sales price, production cost and purchase cost per ton of products and materials respectively.

#           Sales price Production cost           Purchase price
# Bloom             $60         $5        Cotton      $40
# Amber             $55         $4         Wool       $45
# Leaf              $60         $5        Nylon       $30

# The maximal demand (in tons) for each product, the minimum cotton and wool proportion in each product are as follows:

#                        Demand           min Cotton proportion      min Wool proportion
# Bloom                    4200                      50%                    40%
# Amber                    3200                      60%                    40%
# Leaf                     3500                      50%                    30%
  


# b) Solve the model using R/R Studio. Find the optimal profit and optimal values of the decision variables.

# Initial descriptions : 

# Let xij ≥ 0 be a decision variable that denotes the number of tons of products j for j ∈ {1 = Bloom, 2 = Amber, 3 = Leaf} to be produced from Materials i ∈ {C=Cotton, W=Wool, N=Nylon}. 

## The following below are the decision variables:

# XC1, XC2,XC3 are the Bloom, Amber and leaf products made from cotton.
# XW1, XW2,XW3 are the Bloom, Amber and leaf products made from wool.
# XN1, XN2,XN3 are the Bloom, Amber and leaf products made from nylon.


# Creating a linear program model for the problem described in the "BPriyankaa.pdf"

# Set the working directory

setwd('C:/PriyankaaB/1.1 MS DATASCIENCE/2. SIG718 Real World Analytics/8. EndTermAssess')

## Get the working directory:
getwd()

# Reading the necessary library
library(lpSolveAPI)

# Initialise the factory model with 0 constraints initially and 9 decision variables:

FactoryLPModel1 <- make.lp(0, 9)

# The objective function here is to maximise the profits which is the difference of sales price, production cost and purchase price.

# Max. Profit = Sales Price - Production Cost - Purchase Price

lp.control(FactoryLPModel1, sense='maximize')

# The next step is to create the objective function: 

# Max. Profit = [15XC1 + 11XC2 + 15XC3 + 10XW1 +6XW2 + 10XW3 + 25XN1 + 21XN2 + 25XN3]

set.objfn(FactoryLPModel1, c(15, 11, 15, 10, 6, 10, 25, 21, 25)) # This is for the above equation

# Construct the code for demand and materials constraints:

# Demand Constraints:

# XC1 + XW1 + XN1 <= 4200
# XC2 + XW2 + XN2 <= 3200
# XC3 + XW3 + XN3 <= 3500

add.constraint(FactoryLPModel1, c(1,0,0,1,0,0,1,0,0), '<=', 4200)
add.constraint(FactoryLPModel1, c(0,1,0,0,1,0,0,1,0), '<=', 3200)
add.constraint(FactoryLPModel1, c(0,0,1,0,0,1,0,0,1), '<=', 3500)

# Cotton proportion constraints:

# 0.5XC1 - 0.5XW1 – 0.5XN1  >= 0
# 0.4XC2 - 0.6XW2 – 0.6XN2  >= 0
# 0.5XC3 - 0.5XW3 – 0.5XN3  >= 0

# Wool proportion constraints:

#  -0.4XC1 + 0.6XW1 – 0.4XN1  >= 0
#  -0.4XC2 + 0.6XW2 – 0.4XN2  >= 0
#  -0.3XC3  + 0.7XW3 – 0.3XN3  >= 0

add.constraint(FactoryLPModel1, c(0.5,0,0,-0.5,0,0,-0.5,0,0), '>=', 0)
add.constraint(FactoryLPModel1, c(-0.4,0,0,0.6,0,0,-0.4,0,0), '>=', 0)
add.constraint(FactoryLPModel1, c(0,0.4,0,0,-0.6,0,0,-0.6,0), '>=', 0)
add.constraint(FactoryLPModel1, c(0,-0.4,0,0,0.6,0,0,-0.4,0), '>=', 0)
add.constraint(FactoryLPModel1, c(0,0,0.5,0,0,-0.5,0,0,-0.5), '>=', 0)
add.constraint(FactoryLPModel1, c(0,0,-0.3,0,0,0.7,0,0,-0.3), '>=', 0)


## Non - Negativity Constraints
add.constraint(FactoryLPModel1, c(1,0,0,0,0,0,0,0,0), '>=', 0) # XC1 >= 0
add.constraint(FactoryLPModel1, c(0,1,0,0,0,0,0,0,0), '>=', 0) # XC2 >= 0
add.constraint(FactoryLPModel1, c(0,0,1,0,0,0,0,0,0), '>=', 0) # XC3 >= 0
add.constraint(FactoryLPModel1, c(0,0,0,1,0,0,0,0,0), '>=', 0) # XW1 >= 0
add.constraint(FactoryLPModel1, c(0,0,0,0,1,0,0,0,0), '>=', 0) # XW2 >= 0
add.constraint(FactoryLPModel1, c(0,0,0,0,0,1,0,0,0), '>=', 0) # XW3 >= 0
add.constraint(FactoryLPModel1, c(0,0,0,0,0,0,1,0,0), '>=', 0) # XN1 >= 0
add.constraint(FactoryLPModel1, c(0,0,0,0,0,0,0,1,0), '>=', 0) # XN2 >= 0
add.constraint(FactoryLPModel1, c(0,0,0,0,0,0,0,0,1), '>=', 0) # XN 3 >= 0

constraint.names <- c("Demand constraint for Bloom", "Demand constraint for Amber", "Demand constraint for Leaf","Proportion of cotton in Bloom",
                      "Proportion of cotton in Amber","Proportion of cotton in Leaf","Proportion of wool in Bloom","Proportion of cotton in Amber","Proportion of cotton in Leaf",
                      "XC1", "XC2", "XC3","XW1", "XW2", "XW3","XN1", "XN2", "XN3")

decisionvariables.names <- c("XC1", "XC2", "XC3","XW1", "XW2", "XW3","XN1", "XN2", "XN3")

dimnames(FactoryLPModel1) <- list(constraint.names, decisionvariables.names)

# The next step is to solve and get the objective values:
solve(FactoryLPModel1)
get.objective(FactoryLPModel1) # Get the optimal profit
get.variables(FactoryLPModel1) # optimal values of the decision variables
get.constraints(FactoryLPModel1)
FactoryLPModel1

## Inference: The optimal profit is 141850 and optimal values of the decision variables are 2100, 1920, 1750, 1680,
##            1280, 1050, 420, 0, 700


####################################################################################################
                                                              'QUESTION 3'
####################################################################################################

# 3. Two construction companies, Giant and Sky, bid for the right to build in a field. 
## The possible bids are $ 10 Million, $ 20 Million, $ 30 Million, $ 35 Million and $ 40 Million. 
## The winner is the company with the higher bid. 
## The two companies decide that in the case of a tie (equal bids), Giant is the winner and will get the field.
## Giant has ordered a survey and, based on the report from the survey, concludes that getting the field for more than $ 35 Million is as bad as not getting it (assume loss), except in case of a tie (assume win). Sky is not aware of this survey. 

# (e) Produce an appropriate code to solve the linear programming model in part (d). 


library(lpSolveAPI) # This is the necessary library to create a linear programming model and for consistency

BidModel <- make.lp(6, 6) # Linear programming model with 6 constraints and 6 decision variables

#  Construct a linear programming model for Company Sky.

lp.control(BidModel, sense= "min") # This is the objective function for Company Sky, v - minimizing the returns. 

set.objfn(BidModel, c(0,0,0,0,0,1)) # y1, y2, y3, y4, y5, v

# y1, y2, y3, y4, y5 - constraints.

set.row(BidModel, 1, c(1,-1,-1,-1,-1,1), indices = c(1,2,3,4,5,6)) # Player 1 chosen Strategy 1 
set.row(BidModel, 2, c(1,1,-1,-1,-1,1), indices = c(1,2,3,4,5,6)) # Player 1 chosen Strategy 2 
set.row(BidModel, 3, c(1,1,1,-1,-1,1), indices = c(1,2,3,4,5,6)) # Player 1 chosen Strategy 3 
set.row(BidModel, 4, c(1,1,1,1,-1,1), indices = c(1,2,3,4,5,6)) # Player 1 chosen Strategy 4 
set.row(BidModel, 5, c(-1,-1,-1,-1,1,1), indices = c(1,2,3,4,5,6)) # Player 1 chosen Strategy 5 

set.row(BidModel, 6, c(1,1,1,1,1,0), indices = c(1,2,3,4,5,6)) # Sum of the Probabilities

# Set the RHS values:
set.rhs(BidModel, c(0,0,0,0,0,1))

# Setting the conditions

set.constr.type(BidModel, c(">=", ">=", ">=", ">=", ">=", "="))

# The next step is to solve and get the objective values:
solve(BidModel)

get.objective(BidModel) # Get the optimal value

get.variables(BidModel) # optimal values of the decision variables

get.constraints(BidModel)
BidModel

# Produced an appropriate code to solve the linear programming model- BidModel for the game of Company Sky.

# ************************************************************************************************************************************************************



























