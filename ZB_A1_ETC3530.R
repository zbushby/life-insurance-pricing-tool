

#libraries
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
rsconnect::setAccountInfo(name='9ckwl1-zachary-bushby',
                          token='29CE7D1CFEC88D3AE41C6952710F6845',
                          secret='GK1vXzCquwSdI53v44Cv9/v1CATz1DzVd90vaGB8')




#-----------------------Mortality Values------------------------
# Define the data
x <- 20:100
l_x_s <- c(
  99978, 99927, 99876, 99822, 99762, 99694, 99635, 99580, 99513, 99438,
  99372, 99295, 99214, 99128, 99048, 98963, 98870, 98772, 98664, 98548,
  98426, 98293, 98148, 98009, 97852, 97669, 97489, 97328, 97111, 96905,
  96632, 96344, 96050, 95707, 95413, 95032, 94652, 94188, 93720, 93183,
  92611, 91964, 91231, 90463, 89635, 88738, 87748, 86675, 85635, 84422,
  83128, 81609, 80126, 78591, 76574, 74429, 72731, 70392, 68017, 65500,
  62996, 60358, 57135, 53277, 50056, 46011, 41230, 37209, 33119, 28951,
  24402, 20284, 16765, 13551, 10382, 7835, 5379, 3382, 1848, 694, 149
)
l_x_minus1_s_plus_1 <- c(
  NA, 99943, 99892, 99838, 99777, 99708, 99649, 99594, 99526, 99451,
  99384, 99307, 99226, 99140, 99059, 98974, 98881, 98783, 98675, 98560,
  98438, 98306, 98161, 98024, 97868, 97687, 97509, 97351, 97137, 96935,
  96666, 96383, 96095, 95759, 95473, 95101, 94732, 94280, 93826, 93306,
  92752, 92125, 91416, 90674, 89876, 89013, 88060, 87028, 86026, 84855,
  83605, 82130, 80692, 79204, 77230, 75127, 73474, 71173, 68836, 66352,
  63878, 61266, 58057, 54197, 50979, 46914, 42092, 38034, 33897, 29672,
  25044, 20846, 17255, 13967, 10718, 8101, 5571, 3508, 1920, 722, 156
)
l_x <- c(
  1e+05, 99948, 99896, 99842, 99781, 99712, 99652, 99597, 99529, 99454,
  99387, 99310, 99229, 99143, 99062, 98977, 98884, 98787, 98679, 98564,
  98442, 98310, 98166, 98029, 97873, 97692, 97515, 97357, 97143, 96941,
  96672, 96390, 96102, 95766, 95480, 95109, 94740, 94289, 93836, 93317,
  92765, 92141, 91435, 90698, 89906, 89050, 88107, 87087, 86094, 84935,
  83697, 82235, 80811, 79337, 77377, 75288, 73651, 71364, 69040, 66569,
  64108, 61508, 58307, 54451, 51238, 47173, 42342, 38278, 34131, 29891,
  25242, 21023, 17411, 14102, 10828, 8189, 5635, 3551, 1945, 732, 158
)

# Create a data frame
mortality_ultimate <- data.frame(x, l_x_s, l_x_minus1_s_plus_1, l_x)
mortality_ultimate


#------------------------------mortality tables------------------------------

mortality_select <- function(age, table){
  if (table == 'select'){
    mortality_select <- mortality_ultimate %>% 
      filter(x >= age)
    mortality_select[1,4] <-mortality_select[1,2]#lx_1 == lx_s
    mortality_select[2,4] <-mortality_select[2,3]#lx_2 == lx_s +1
    return(mortality_select[,c(1,4)])
  }
  if (table == 'ultimate'){
    return(mortality_ultimate)
  }
}

#------------------------------variables------------------------------

interestRate = 0 
age <- 20
lifeTable <- mortality_select(age,"select")
term <- 10
sumAssured <- 1

#expenses ([0,50%])
initialExpense <- 0 #as % of gross premium
renewalExpense <- 0 #as % of gross premium (excludes first premium
claimExpense <- 0 #as % of benefit amount
  
j <- 0
B <- 0
b <- 0



#------------------------------Basic Definitions------------------------------
# Set basic constants

#basic definitions come from:
#Assignment pg2
#slide: 12,13,14,15,57 & 58 week 2 lecture


getLx <- function(age) {
  return(lifeTable$l_x[lifeTable$x == age])
}

getQx <- function(age) {
  lx = getLx(age)
  lx_next = getLx(age + 1)
  return((lx - lx_next) / lx)
}

# Function to calculate Prob(Kx = k) for each age and k
calculateProbKx <- function() {
  df <- data.frame()
  
  for (x in lifeTable$x) {
    l_x <- getLx(x)
    
    # Get the range of possible k values for the given x
    k_values <- 0:(max(lifeTable$x) - x)
    
    for (k in k_values) {
      l_x_k <- getLx(x + k)
      kpx <- l_x_k / l_x
      qx_k <- if (k == max(k_values)) 0 else getQx(x + k)  # Set qx_k to 0 at the maximum age
      
      # Store the results in the data frame
      df <- rbind(df, data.frame(age = x, k = k, kpx = kpx, qx_k = qx_k, ProbKx = kpx * qx_k))
    }
  }
  
  return(df)
}

probKx <- data.frame(calculateProbKx())

# Actuarial Discount Factor
actuarialDiscountFactor <- function(age, interestRate, term) {
  probabilityOfSurvival = getLx(age + term) / getLx(age)
  termDiscount = (1 / (1 + interestRate))^term
  return(probabilityOfSurvival * termDiscount)
}

# Force of Interest
forceOfInterest <- function(interestRate){
  return(log(1 + interestRate))
}

# Whole Life Annuity in Arrears
wholeLifeAnnuityArrears <- function(age, interestRate) {
  maxAge <- max(lifeTable$x)
  length <- (maxAge - age)
  probabilityOfSurvival <- numeric(length)
  for (i in (age+1):maxAge) {
    probabilityOfSurvival[i - age] <- getLx(i) / getLx(age)
  }
  discountRates <- (1 / (1 + interestRate))^(1:length(probabilityOfSurvival))
  x <- sum(discountRates * probabilityOfSurvival)
  return(x)
}


# Whole Life Annuity in Advance
wholeLifeAnnuityAdvance <- function(age, interestRate) {
  x<- wholeLifeAnnuityArrears(age, interestRate) + 1
  return(x)
}

# Whole Life Annuity Continuously
wholeLifeAnnuityContinuous <- function(age, interestRate){
  return(wholeLifeAnnuityAdvance(age, interestRate) - 0.5)
}


#----------------Monthly----------------------

# Whole Life Annuity Monthly in Arrears 
#approximation W2
wholeLifeAnnuityArrearsMonthly <- function(age, interestRate) {
   x <- wholeLifeAnnuityArrears(age, interestRate) + (11/24)
}

# Whole Life Annuity Monthly in Advance 
#approximation W2
wholeLifeAnnuityAdvanceMonthly <- function(age, interestRate) {
  x <- wholeLifeAnnuityAdvance(age, interestRate) - (11/24)
  return(x)
}

#---------------------------------------------

# Temporary Annuity in Arrears
temporaryAnnuityArrears <- function(age, interestRate, term){
  TempAnnArrears <- (wholeLifeAnnuityArrears(age,interestRate) - (actuarialDiscountFactor(age,interestRate,term)*wholeLifeAnnuityArrears((age+term),interestRate)))
  return(TempAnnArrears)
}

# Temporary Annuity in Advance
temporaryAnnuityAdvance <- function(age, interestRate, term){
  TempAnnAdvance <- (wholeLifeAnnuityAdvance(age,interestRate) - (actuarialDiscountFactor(age,interestRate,term)*wholeLifeAnnuityAdvance((age+term),interestRate)))
  return(TempAnnAdvance)
}

# Temporary Annuity Payable Immediately
temporaryAnnuityImmediate <- function(age, interestRate, term){
  TempAnnImmediate <- (wholeLifeAnnuityContinuous(age,interestRate) - (actuarialDiscountFactor(age,interestRate,term)*wholeLifeAnnuityContinuous((age+term),interestRate)))
  return(TempAnnImmediate)
}

#----------------Monthly----------------------
temporaryAnnuityArrearsMonthly <- function(age, interestRate, term){
  x <- temporaryAnnuityArrears(age, interestRate, term) + ((11/24)*(actuarialDiscountFactor(age,interestRate,term)))
  return(x)
}

temporaryAnnuityAdvanceMonthly <- function(age, interestRate, term){
  x <- temporaryAnnuityAdvance(age, interestRate, term) - ((11/24)*(actuarialDiscountFactor(age,interestRate,term)))
    return(x)
}
#---------------------------------------------


# Whole Life Assurance
wholeLifeAssurance <- function(age, interestRate) {
  annuity = wholeLifeAnnuityAdvance(age, interestRate)
  deltaValue = interestRate / (1 + interestRate)  # Make sure to define deltaValue appropriately
  x <- 1 - (deltaValue * annuity)
  return(x)
}

# Whole Life Assurance Paid Immediately After Death
wholeLifeAssuranceImmediate <- function(age, interestRate) {
  forceInt = forceOfInterest(interestRate)
  immediateAnnuity = wholeLifeAnnuityAdvance(age, interestRate) - 0.5
  return(1 - (forceInt * immediateAnnuity))
}

#----------------Monthly----------------------
# Whole Life Assurance
wholeLifeAssuranceMonthly <- function(age, interestRate) {
  annuity = wholeLifeAnnuityAdvanceMonthly(age, interestRate)
  deltaValue = interestRate / (1 + interestRate)
  x <- 1 - (deltaValue * annuity)
  return(x)
}
# Whole Life Assurance Paid Immediately After Death Monthly
wholeLifeAssuranceImmediateMonthly <- function(age, interestRate) {
  forceInt = forceOfInterest(interestRate)
  immediateAnnuity = wholeLifeAnnuityAdvanceMonthly(age, interestRate) - 0.5
  return(1 - (forceInt * immediateAnnuity))
}
#---------------------------------------------

# EPV of Term Assurance
termAssurance <- function(age, interestRate, term) {
  lifeAssurance = wholeLifeAssurance(age, interestRate)
  adjustment = wholeLifeAssurance(age + term, interestRate) * actuarialDiscountFactor(age, interestRate, term)
  return(lifeAssurance - adjustment)
  
}

# EPV of Term Assurance Paid Immediately After Death
#based on assignment approximation
termAssuranceImmediate <- function(age, interestRate, term) {
  return((1 + interestRate)^0.5 * termAssurance(age, interestRate, term))
}

#----------------Monthly----------------------
# EPV of Term Assurance
termAssuranceMonthly <- function(age, interestRate, term) {
  lifeAssurance = wholeLifeAssuranceMonthly(age, interestRate)
  adjustment = wholeLifeAssuranceMonthly(age + term, interestRate) * actuarialDiscountFactor(age, interestRate, term)
  return(lifeAssurance - adjustment)
}

# EPV of Term Assurance Immediate
termAssuranceImmediateMonthly <- function(age, interestRate, term) {
  return((1 + interestRate)^0.5 * termAssuranceMonthly(age, interestRate, term))
}
#---------------------------------------------

# EPV of Pure Endowment
pureEndowment <- function(age, interestRate, term){
  return(actuarialDiscountFactor(age, interestRate, term))
}

# EPV of Endowment
endowmentAssurance <- function(age, interestRate, term) {
  return(termAssurance(age, interestRate, term) + pureEndowment(age, interestRate, term))
}

# EPV of Endowment monthly
endowmentAssuranceMonthly <- function(age, interestRate, term) {
  return(termAssuranceMonthly(age, interestRate, term) + pureEndowment(age, interestRate, term))
}

# EPV of Endowment monthly
endowmentAssuranceImmediateMonthly <- function(age, interestRate, term) {
  return(termAssuranceImmediateMonthly(age, interestRate, term) + pureEndowment(age, interestRate, term))
}

# EPV of Endowment Immediate
#slide 14 week 2
endowmentAssuranceImmediate <- function(age, interestRate, term){
  EndAssImmediate <- (termAssuranceImmediate(age,interestRate,term) + pureEndowment(age, interestRate, term))
  return(EndAssImmediate)
}


#---------------------------------  Premiums  ---------------------------------


# Print the results
Kx <- probKx %>% filter(age == 60) %>% mutate(Kx = (age+k)*ProbKx) %>%  select(Kx) %>% sum()




#------------------------------ Whole Life assurance --------------------------
#single
# whole life single end of year
WholeLife_Single_EOY <- function(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((sumAssured + Bonus(age,100-age,B,j,b))* wholeLifeAssurance(age, interestRate) * (1 + claimExpense))
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}

#whole life single immediate
WholeLife_Single_IM <- function(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((sumAssured + Bonus(age,100-age,B,j,b)) * wholeLifeAssuranceImmediate(age, interestRate) * (1 + claimExpense))
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}

# level

#whole life level annual end of year
WholeLife_Level_EOY <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((sumAssured + Bonus(age,100-age,B,j,b)) * wholeLifeAssurance(age, interestRate) * (1 + claimExpense))
  PremiumEPV = (wholeLifeAnnuityAdvance(age, interestRate) 
                - initialExpense 
                - (renewalExpense * (wholeLifeAnnuityArrears(age, interestRate))))
  P = BenefitEPV / PremiumEPV
  return(P)
}

#whole life level annual immediate
WholeLife_Level_IM <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((sumAssured + Bonus(age,100-age,B,j,b)) * wholeLifeAssuranceImmediate(age, interestRate) * (1 + claimExpense))
  PremiumEPV = (wholeLifeAnnuityAdvance(age, interestRate) 
                - initialExpense 
                - (renewalExpense * (wholeLifeAnnuityArrears(age, interestRate))))
  P = BenefitEPV / PremiumEPV
  return(P)
}


# monthly
#whole life monthly end of year

WholeLife_Monthly_EOY <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((sumAssured + Bonus(age,100-age,B,j,b)) * wholeLifeAssuranceMonthly(age, interestRate) * (1 + claimExpense))
  PremiumEPV = (wholeLifeAnnuityAdvanceMonthly(age, interestRate) 
                - initialExpense 
                - (renewalExpense * (wholeLifeAnnuityArrearsMonthly(age, interestRate))))
  P = BenefitEPV / PremiumEPV
  return(P/12)
}

# whole life monthly immediate

WholeLife_Monthly_IM <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((sumAssured + Bonus(age,100-age,B,j,b)) * wholeLifeAssuranceImmediateMonthly(age, interestRate) * (1 + claimExpense))
  PremiumEPV = (wholeLifeAnnuityAdvanceMonthly(age, interestRate) 
                - initialExpense 
                - (renewalExpense * (wholeLifeAnnuityArrearsMonthly(age, interestRate))))
  P = BenefitEPV / PremiumEPV
  return(P/12)
}




#-----------------------------  Pure Endowments  ------------------------------

#level

PureEndowmentLevel <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured) {
  
  BenefitEPV = (sumAssured * pureEndowment(age, interestRate, term) *(1 + claimExpense))
  PremiumEPV = (temporaryAnnuityAdvance(age, interestRate, term) #
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrears(age, interestRate, term-1))))
  P = BenefitEPV / PremiumEPV
  return(P)
}


#monthly
PureEndowmentMonthly <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured) {
  BenefitEPV = (sumAssured * (pureEndowment(age, interestRate, term))*(1 + claimExpense))
  PremiumEPV = (temporaryAnnuityAdvanceMonthly(age, interestRate, term) #
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrearsMonthly(age, interestRate, term-1))))
  P = BenefitEPV / PremiumEPV
  return(P/12)
}


#single

PureEndowmentSingle <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured) {
  BenefitEPV = sumAssured * pureEndowment(age, interestRate, term) * (1 + claimExpense)
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}
#PureEndowmentSingle(40, 0.06, 0, 0, 0, 100) 

#------------------------------ Term assurance --------------------------------

#level 

Bonus <- function(a,term,B,j,b){
  Kx <- probKx %>% filter(age == a) %>% mutate(Kx = (k)*ProbKx) %>%  select(Kx) %>% sum() %>% floor()
  Bonus <- 0
  if(Kx >=term){
    if (term == 1){
      Bonus <- B
    }
    Bonus <- ((1 + j)^(term - 1)) * B * (1 + (term - 1) * b)
    
  }
  if(Kx < term){
    if (Kx == 1){
      Bonus <- B
    }
    Bonus <- ((1 + j)^(Kx - 1)) * B * (1 + (Kx - 1) * b)
  }
  return(Bonus)
}

#EOY
Term_Level_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  
  BenefitEPV = ((Bonus(age,term,B,j,b) + sumAssured) * termAssurance(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = (temporaryAnnuityAdvance(age, interestRate, term) 
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrears(age, interestRate, term-1)))
  )
  P = BenefitEPV / PremiumEPV
  
  return(P)
}

#immediate
Term_Level_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = (Bonus(age,term,B,j,b) + sumAssured) * termAssuranceImmediate(age, interestRate, term) * (1 + claimExpense)
  PremiumEPV = (temporaryAnnuityAdvance(age, interestRate, term) 
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrears(age, interestRate, term-1))))
  P = BenefitEPV / PremiumEPV
  return(P)
}

#-------------------Monthly--------------------
Term_Monthly_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((Bonus(age,term,B,j,b) + sumAssured) * termAssuranceMonthly(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = (temporaryAnnuityAdvanceMonthly(age, interestRate, term) 
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrearsMonthly(age, interestRate, term-1)))
  )
  P = BenefitEPV / PremiumEPV
  return(P/12)
}
#immediate
Term_Monthly_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = (Bonus(age,term,B,j,b) + sumAssured) * termAssuranceImmediateMonthly(age, interestRate, term) * (1 + claimExpense)
  PremiumEPV = (temporaryAnnuityAdvanceMonthly(age, interestRate, term) 
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrearsMonthly(age, interestRate, term-1))))
  P = BenefitEPV / PremiumEPV
  return(P/12)
}
#---------------------------------------------





#single

#EOY

Term_Single_EOY <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((Bonus(age,term,B,j,b) + sumAssured) * termAssurance(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}


#immediate
Term_Single_IM <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b) {
  BenefitEPV = ((Bonus(age,term,B,j,b) + sumAssured) * termAssuranceImmediate(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}


#--------------------------- endowment assurance ------------------------------


#level

#EOY

Endowment_Level_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured) {
  BenefitEPV = ((sumAssured) * endowmentAssurance(age, interestRate, term)* (1 + claimExpense))
  PremiumEPV = (temporaryAnnuityAdvance(age, interestRate, term) 
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrears(age, interestRate, term-1)) ) 
  )
  P = BenefitEPV / PremiumEPV
  return(P)
}


#immediate

Endowment_Level_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured) {
  BenefitEPV = (sumAssured * endowmentAssuranceImmediate(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = temporaryAnnuityAdvance(age, interestRate, term) - initialExpense - 
    (renewalExpense * (temporaryAnnuityArrears(age, interestRate, term-1)))
  P = BenefitEPV / PremiumEPV
  return(P)
}

#------------------------Monthly--------------------------
Endowment_Monthly_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured) {
  BenefitEPV = (sumAssured * endowmentAssuranceMonthly(age, interestRate, term)* (1 + claimExpense))
  PremiumEPV = (temporaryAnnuityAdvanceMonthly(age, interestRate, term) 
                - initialExpense 
                - (renewalExpense * (temporaryAnnuityArrearsMonthly(age, interestRate, term-1)) ) 
  )
  P = BenefitEPV / PremiumEPV
  return(P/12)
}


#immediate

Endowment_Monthly_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured) {
  BenefitEPV = (sumAssured * endowmentAssuranceImmediateMonthly(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = temporaryAnnuityAdvanceMonthly(age, interestRate, term) - initialExpense - 
    (renewalExpense * (temporaryAnnuityArrearsMonthly(age, interestRate, term-1)))
  P = BenefitEPV / PremiumEPV
  return(P/12)
}




#---------------------------------------------





#single

#EOY

Endowment_Single_EOY <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured) {
  BenefitEPV = sumAssured * endowmentAssurance(age, interestRate, term) * (1 + claimExpense)
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}


#immediate

Endowment_Single_IM <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured) {
  BenefitEPV = (sumAssured * endowmentAssuranceImmediate(age, interestRate, term) * (1 + claimExpense))
  PremiumEPV = (1 - initialExpense)
  P = BenefitEPV / PremiumEPV
  return(P)
}



#---------------------------------  Reserves  ---------------------------------
#Prospective Approach



#-----------------------------  Pure Endowments  -----------------------------

#level

Reserve_PureEndowmentLevel <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t) {
  P <- PureEndowmentLevel(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)
  PremiumEPVT <- (P * temporaryAnnuityAdvance(age + t, interestRate, term-t))
  
  BenefitEPVT <- sumAssured*pureEndowment(age+t, interestRate, term-t)
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrears(age+t, interestRate, term-1-t)))
                    + claimExpense*BenefitEPVT
    )
  }
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_PureEndowmentLevel(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, 0)

#single

Reserve_PureEndowmentSingle <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, t) {
  P <- PureEndowmentSingle(age, interestRate, term, initialExpense, claimExpense, sumAssured)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  BenefitEPVT <- sumAssured*pureEndowment(age+t, interestRate, term-t)
  
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if(t > 0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_PureEndowmentSingle(age, interestRate, term, initialExpense, claimExpense, sumAssured, 0) 

Reserve_PureEndowmentMonthly <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, t) {
  Reserve_func <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, t){
    P <- 12*PureEndowmentMonthly(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)
    
    PremiumEPVT <- (P * temporaryAnnuityAdvanceMonthly(age + t, interestRate, term-t))
    
    BenefitEPVT <- sumAssured*pureEndowment(age+t, interestRate, term-t)
    
    if (t == 0){
      ExpenseEPVT <- initialExpense*P
    }
    if (t > 0){
      ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrearsMonthly(age+t, interestRate, term-1-t)))
                      + claimExpense*BenefitEPVT)
    }
    Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
    return(Reserve)
  }
  t1 <- Reserve_func(age, interestRate, floor(term/12), initialExpense, claimExpense, sumAssured, floor(t/12))
  t2 <- Reserve_func(age, interestRate, ceiling(term/12), initialExpense, claimExpense, sumAssured, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
#Reserve_PureEndowmentMonthly(age, interestRate, term*12, initialExpense, claimExpense, sumAssured, term*12) 

#------------------------------ Term assurance --------------------------------

#
#level

#EOY
Reserve_Term_Level_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t) {
  
  P <- Term_Level_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- (P * temporaryAnnuityAdvance(age + t, interestRate, term-t))
  BenefitEPVT <- ((sumAssured + Bonus(age+t,term-t,B,j,b))* termAssurance(age +t, interestRate, term-t)) 
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrears(age+t, interestRate, term-1-t)))
                    +claimExpense*BenefitEPVT)
  }
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Term_Level_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, term)

#immediate
Reserve_Term_Level_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t) {
  P <- Term_Level_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- (P * temporaryAnnuityAdvance(age + t, interestRate, term-t))
  BenefitEPVT <- (sumAssured + Bonus(age+t,term-t,B,j,b))* termAssuranceImmediate(age+t, interestRate, term-t)
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrears(age+t, interestRate, term-1-t)))
                    +claimExpense*BenefitEPVT)
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Term_Level_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, 0)

#single

#EOY

Reserve_Term_Single_EOY <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b, t) {
  P <- Term_Single_EOY(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  
  BenefitEPVT <- ((sumAssured + Bonus(age+t,term-t,B,j,b))* termAssurance(age +t, interestRate, term-t))
  
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t >0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Term_Single_EOY(age, interestRate, term, initialExpense, claimExpense, sumAssured, 0)

#immediate
Reserve_Term_Single_IM <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b, t) {
  P <- Term_Single_IM(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  
  BenefitEPVT <- ((sumAssured + Bonus(age+t,term-t,B,j,b)) * termAssuranceImmediate(age+t, interestRate, term-t))
  
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t >0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Term_Single_IM(age, interestRate, term, initialExpense, claimExpense, sumAssured, 0)


#monthly
#term monthly EOY
Reserve_Term_Monthly_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t) {
  Reserve_func<- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t){
      P <- 12*Term_Monthly_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
      t <- floor(t/12)
      
      PremiumEPVT <- (P * temporaryAnnuityAdvanceMonthly(age + t, interestRate, term-t))
      
      BenefitEPVT <- (Bonus(age+t,term-t,B,j,b) + sumAssured) * termAssuranceMonthly(age+t, interestRate, term-t)
      
      if (t == 0){
        ExpenseEPVT <- initialExpense*P
      }
      if (t > 0){
        ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrearsMonthly(age+t, interestRate, term-1-t)))
                        + claimExpense*BenefitEPVT)
      }
      Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
      return(Reserve)
      }
  t1 <- Reserve_func(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, floor(t/12))
  t2 <- Reserve_func(age, interestRate, ceiling(term/12), initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
#Reserve_Term_Monthly_EOY(age, interestRate, 120, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, 120)


#immediate
Reserve_Term_Monthly_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t) {
  Reserve_func <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t){
    P <- 12*Term_Monthly_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
    t <- floor(t/12)
    
    PremiumEPVT <- (P * temporaryAnnuityAdvanceMonthly(age + t, interestRate, term-t))
    
    BenefitEPVT <- (Bonus(age+t,term-t,B,j,b) + sumAssured) * termAssuranceImmediateMonthly(age+t, interestRate, term-t)
    
    if (t == 0){
      ExpenseEPVT <- initialExpense*P
    }
    if (t > 0){
      ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrearsMonthly(age+t, interestRate, term-1-t)))
                      + claimExpense*BenefitEPVT)
    }
    Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
    return(Reserve)
    }
  t1 <- Reserve_func(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, floor(t/12))
  t2 <- Reserve_func(age, interestRate, ceiling(term/12), initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
#Reserve_Term_Monthly_IM(age, interestRate, 120, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, 0)



#--------------------------- endowment assurance ------------------------------


#level

#EOY

Reserve_Endowment_Level_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t) {
  P <- Endowment_Level_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)
  PremiumEPVT <- (P * temporaryAnnuityAdvance(age + t, interestRate, term-t))
  BenefitEPVT <- (sumAssured * endowmentAssurance(age+t, interestRate, term-t))
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrears(age+t, interestRate, term-1-t)))
                    +claimExpense*BenefitEPVT)
  }
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Endowment_Level_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, 0)

#immediate

Reserve_Endowment_Level_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t) {
  P <- Endowment_Level_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)
  PremiumEPVT <- (P * temporaryAnnuityAdvance(age + t, interestRate, term-t))
  BenefitEPVT <- sumAssured * endowmentAssuranceImmediate(age+t, interestRate, term-t)
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrears(age+t, interestRate, term-1-t)))
                    +claimExpense*BenefitEPVT)
  }
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Endowment_Level_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, 0)



#single

#EOY

Reserve_Endowment_Single_EOY <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, t) {
  P <- Endowment_Single_EOY(age, interestRate, term, initialExpense, claimExpense, sumAssured)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  
  BenefitEPVT <- (sumAssured * endowmentAssurance(age +t, interestRate, term-t))
  
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Endowment_Single_EOY(age, interestRate, term, initialExpense, claimExpense, sumAssured, 0)

#immediate

Reserve_Endowment_Single_IM <- function(age, interestRate, term, initialExpense, claimExpense, sumAssured, t) {
  P <- Endowment_Single_IM(age, interestRate, term, initialExpense, claimExpense, sumAssured)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  
  BenefitEPVT <- (sumAssured * endowmentAssuranceImmediate(age +t, interestRate, term-t))
  
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_Endowment_Single_IM(age, interestRate, term, initialExpense, claimExpense, sumAssured, 0)


Reserve_Endowment_Monthly_EOY <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t) {
  Reserve_func <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t){
    P <- 12*Endowment_Monthly_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)
    t <- floor(t/12)
    
    PremiumEPVT <- (P * temporaryAnnuityAdvanceMonthly(age + t, interestRate, term-t))
    
    BenefitEPVT <- (sumAssured) * endowmentAssuranceMonthly(age+t, interestRate, term-t)
    
    if (t == 0){
      ExpenseEPVT <- initialExpense*P
    }
    if (t > 0){
      ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrearsMonthly(age+t, interestRate, term-1-t)))
                      + claimExpense*BenefitEPVT)
    }
    Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
    return(Reserve)
    }
  
  t1 <- Reserve_func(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, floor(t/12))
  t2 <- Reserve_func(age, interestRate, ceiling(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
#Reserve_Endowment_Monthly_EOY (age, interestRate, 120, initialExpense, renewalExpense, claimExpense, sumAssured, 120)


#immediate

Reserve_Endowment_Monthly_IM <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t) {
  Reserve_func <- function(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t){
    P <- 12*Endowment_Monthly_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)
    PremiumEPVT <- (P * temporaryAnnuityAdvanceMonthly(age + t, interestRate, term-t))
    
    BenefitEPVT <- (sumAssured) * endowmentAssuranceImmediateMonthly(age+t, interestRate, term-t)
    
    if (t == 0){
      ExpenseEPVT <- initialExpense*P
    }
    if (t > 0){
      ExpenseEPVT <- (P*(renewalExpense * (temporaryAnnuityArrearsMonthly(age+t, interestRate, term-1-t)))
                      + claimExpense*BenefitEPVT)
    }
    Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
    return(Reserve)
  }
  t1 <- Reserve_func(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, floor(t/12))
  t2 <- Reserve_func(age, interestRate, ceiling(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
#Reserve_Endowment_Monthly_IM(age, interestRate, 80, initialExpense, renewalExpense, claimExpense, sumAssured, 80*12)









#------------------------------ Whole Life assurance --------------------------
#single
# whole life single end of year
Reserve_WholeLife_Single_EOY <- function(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b, t) {
  P <- WholeLife_Single_EOY(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  
  BenefitEPVT <- ((sumAssured+Bonus(age+t,100-age-t,B,j,b)) * wholeLifeAssurance(age +t, interestRate))
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t >0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_WholeLife_Single_EOY(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b, 0) 
  
#whole life single immediate
Reserve_WholeLife_Single_IM <- function(age, interestRate, initialExpense, claimExpense, sumAssured,B, j, b, t) {
  P <- WholeLife_Single_IM(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- 0
  if (t==0){
    PremiumEPVT <- (P)
  }
  
  BenefitEPVT <- ((sumAssured+Bonus(age+t,100-age-t,B,j,b)) * wholeLifeAssuranceImmediate(age +t, interestRate))
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t >0){
    ExpenseEPVT <- claimExpense*BenefitEPVT
  }
  
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_WholeLife_Single_IM(age, interestRate, initialExpense, claimExpense, sumAssured,B, j, b, 0)
  
# level

#whole life level annual end of year
Reserve_WholeLife_Level_EOY <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, t) {
  P <- WholeLife_Level_EOY(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- (P * wholeLifeAnnuityAdvance(age + t, interestRate))
  BenefitEPVT <- ((sumAssured+Bonus(age+t,100-age-t,B,j,b)) * wholeLifeAssurance(age+t, interestRate))
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (wholeLifeAnnuityArrears(age+t, interestRate)))
                    +claimExpense*BenefitEPVT)
  }
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_WholeLife_Level_EOY(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, 0)
  
#whole life level annual immediate
Reserve_WholeLife_Level_IM <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t) {
  P <- WholeLife_Level_IM(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
  PremiumEPVT <- (P * wholeLifeAnnuityAdvance(age + t, interestRate))
  BenefitEPVT <- ((sumAssured+Bonus(age+t,100-age-t,B,j,b)) * wholeLifeAssuranceImmediate(age+t, interestRate))
  if (t == 0){
    ExpenseEPVT <- initialExpense*P
  }
  if (t > 0){
    ExpenseEPVT <- (P*(renewalExpense * (wholeLifeAnnuityArrears(age+t, interestRate)))
                    +claimExpense*BenefitEPVT)
  }
  Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
  return(Reserve)
}
#Reserve_WholeLife_Level_IM(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b,0) 

# monthly
#whole life monthly end of year

Reserve_WholeLife_Monthly_EOY <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, t) {
  Reserve_func <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, t){
    P <- 12*WholeLife_Monthly_EOY(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
    t <- floor(t/12)
    
    PremiumEPVT <- (P * wholeLifeAnnuityAdvanceMonthly(age + t, interestRate))
    
    BenefitEPVT <- (sumAssured+Bonus(age+t,100-age-t,B,j,b))*wholeLifeAssuranceMonthly(age+t, interestRate)
    
    if (t == 0){
      ExpenseEPVT <- initialExpense*P
    }
    if (t > 0){
      ExpenseEPVT <- (P*(renewalExpense * (wholeLifeAnnuityArrearsMonthly(age+t, interestRate)))
                      + claimExpense*BenefitEPVT)
    }
    Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
    return(Reserve)
    }
  t1 <- Reserve_func(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, floor(t/12))
  t2 <- Reserve_func(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
Reserve_WholeLife_Monthly_EOY(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, 1200-20*12)

# whole life monthly immediate

Reserve_WholeLife_Monthly_IM <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, t) {
  Reserve_func <- function(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, t) {
    P <- 12*WholeLife_Monthly_IM(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)
    t <- floor(t/12)
    
    PremiumEPVT <- (P * wholeLifeAnnuityAdvanceMonthly(age + t, interestRate))
    
    BenefitEPVT <- (sumAssured+Bonus(age+t,100-age-t,B,j,b))*wholeLifeAssuranceImmediateMonthly(age+t, interestRate)
    
    if (t == 0){
      ExpenseEPVT <- initialExpense*P
    }
    if (t > 0){
      ExpenseEPVT <- (P*(renewalExpense * (wholeLifeAnnuityArrearsMonthly(age+t, interestRate)))
                      + claimExpense*BenefitEPVT)
    }
    Reserve <- BenefitEPVT + ExpenseEPVT - PremiumEPVT
    return(Reserve)
    }
  t1 <- Reserve_func(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, floor(t/12))
  t2 <- Reserve_func(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, ceiling(t/12))
  weight <- t/12 - floor(t/12)
  Final_Reserve <- (1 - weight) * t1 + weight * t2
  
  return(Final_Reserve)
}
#Reserve_WholeLife_Monthly_IM(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured,B, j, b, 80*12)







#---------------------------------  Fail Attempt  --------------------------------




#UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
        code {
            display: block;
            padding: 20px;
            margin: 20px 0;
            font-size: 25px;
            line-height: 1.6;
            word-break: break-all;
            word-wrap: break-word;
            white-space: pre-wrap;
            background-color: #2d2d2d;
            border: 1px solid rgba(0,0,0,0.15);
            border-radius: 10px; 
            color: #d1d1d1;
            font-family: 'Courier New', monospace;
            box-shadow: 0 2px 5px rgba(0,0,0,0.2);
            text-align: center;
        }
    "))
    ),
  
  
  
  
  #set theme
  theme = shinytheme("yeti"),
  
  useShinyjs(),
  
  #title
  titlePanel( code("Insurance Product Valuation Tool")
              
  ),
  
  #side panel - setting inputs
  sidebarPanel(
    #group
    radioButtons("mortality", label = "Mortality Table", choices = list("Ultimate", "Select"), selected = "Ultimate"),
    
    
    
    #assured amount
    numericInput("sumAssured", label = "Benefit Amount Assured ($)", value = 100, min = 0, max = 10^7),
    
    #product benefit options
    selectInput("policy", label = "Benefit Type", 
                choices = list("Endowment Assurance", "Term Assurance", "Pure Endowment", "Whole Life Assurance"),
                selected = "Endowment Assurance" ),
    
    #payment timing - 1 - all
    selectInput("benefit_type", label = "Benefit Payment Timing", 
                choices = c("End of Year of Death", "Immediately after Death")
                , selected =  "Immediately after Death"  
    ),
    
    #premium payment style -1 - all
    selectInput("premium_type", label = "Premium Payment Method",
                choices = c("Annual", "Monthly","Single Lump Sum")
                , selected = "Single Lump Sum"
    ),
    
    
    
    #age (both group premium_type- show for all except featured product)
    sliderInput("age", label = "Age of PolicyHolder ", value = 20, min = 20, max = 80),
    
    #interest rate
    sliderInput("interestRate", label = " Effective Interest Rate (in %) ", value = 4, min = 0, max = 10),
    
    #contract term
    sliderInput("term", label = "Term of Contract", value = 10, min = 5, max = 80),
    
    #initial expense (% of premium)
    numericInput("initialExpense", label = " Initial Policy Expense (as % of Premium) ", 
                 value = 0, min = 0, max = 50),
    
    #renewal expense (% of premium)
    numericInput("renewalExpense", label = " Renewal Expenses (as % of Premium) ", 
                 value = 0, min = 0, max = 50),
    
    #Claim expenses (% of Benefit Amt)
    numericInput("claimExpense", label = " Claim Expenses (as % of Benefit Amt) ", 
                 value = 0, min = 0, max = 50),
    
    #Bonus initial sum , B, j, b
    numericInput("B", label = "Bonus Initial Sum", 
                 value = 0, min = 0, max = 50),
    #Bonus Inflation
    numericInput("j", label = "Bonus Inflation", 
                 value = 0, min = 0, max = 50),
    #Bonus % increase
    numericInput("b", label = "Bonus Yearly Multiplier", 
                 value = 0, min = 0, max = 50)
    
    
  ),
  
  

  mainPanel( 
    
    br(),
    
    h5 ( span( textOutput("OutputText2"), style = "color:grey" ) , align = "center"),
    
    h5 ( span( textOutput("OutputText3"), style = "color:grey") , align = "center" ),
    
    h5 ( p("The policyholder must pay a", align = "center", style = "color:grey")),
    
    strong (h4 ( textOutput("OutputText"), align = "center" ) ) ,
    
    br(),
    
    
    plotOutput("plot"),
    
    br(), 

    tags$head(
      tags$style(
        HTML(
          ".grey-bubble {
                  display: inline-block;
                  padding: 10px 160px;
                  background-color: grey;
                  border-radius: 25px;
                  color: white;
                  text-align: center;
              }"
        )
      )
    ),
    uiOutput("OutputText4")
    

    
    
    
  )  
  
  
  
  
)

#---------------------------------- SERVER ------------------------------------

server = function(input, output, session) {
  
  
  #output text - premium calculation (need to define variables)
  output$OutputText = renderText( { 
    
    age = input$age
    sumAssured = input$sumAssured
    interestRate = input$interestRate / 100
    term = input$term
    #mortality
    if(input$mortality == "Ultimate") {lifeTable <<- mortality_select(age,"ultimate")} else {lifeTable <<- mortality_select(age,"select")}
    
    #Expenses:
    initialExpense = input$initialExpense / 100
    renewalExpense = input$renewalExpense / 100
    claimExpense = input$claimExpense / 100
    
    #Bonuses
    B = input$B
    j = input$j/100
    b = input$b/100
    
    #definitions
    EA = "Endowment Assurance" ; TA = "Term Assurance" ; PE = "Pure Endowment" ; WA = "Whole Life Assurance"
    EOY = "End of Year of Death" ; IM = "Immediately after Death"
    Level = "Annual" ; Single = "Single Lump Sum" ; Monthly = "Monthly"
    
    policy <- input$policy
    benefit_type <- input$benefit_type
    premium_type <- input$premium_type
    
    
    
    #level
    
    #endowment assurance, EOY, level
    if(policy == EA & benefit_type == EOY & premium_type == Level) 
    {Premium = Endowment_Level_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)}
    #endowment assurance, IM, level
    if(policy == EA & benefit_type == IM & premium_type == Level) 
    {Premium = Endowment_Level_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)}
    #term assurance, EOY, level
    if(policy == TA & benefit_type == EOY & premium_type == Level) 
    {Premium = Term_Level_EOY(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    #term assurance, IM, level
    if(policy == TA & benefit_type == IM & premium_type == Level) 
    {Premium = Term_Level_IM(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    #Wholel life assurance, EOY, level
    if(policy == WA & benefit_type == EOY & premium_type == Level) 
    {Premium = WholeLife_Level_EOY(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    #Whole life assurance, IM , level
    if(policy == WA & benefit_type == IM & premium_type == Level) 
    {Premium = WholeLife_Level_IM(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    
    #monthly
    #endowment assurance, EOY, Monthly
    if(policy == EA & benefit_type == EOY & premium_type == Monthly) 
    {Premium = Endowment_Monthly_EOY(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured)}
    #endowment assurance, IM, Monthly
    if(policy == EA & benefit_type == IM & premium_type == Monthly) 
    {Premium = Endowment_Monthly_IM(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured)}
    #term assurance, EOY, Monthly
    if(policy == TA & benefit_type == EOY & premium_type == Monthly) 
    {Premium = Term_Monthly_EOY(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    #term assurance, IM, Monthly
    if(policy == TA & benefit_type == IM & premium_type == Monthly) 
    {Premium = Term_Monthly_IM(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    #whole life assurance, EOY, Monthly
    if(policy == WA & benefit_type == EOY & premium_type == Monthly) 
    {Premium = WholeLife_Monthly_EOY(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    #whole life assurance, IM, Monthly
    if(policy == WA & benefit_type == IM & premium_type == Monthly) 
    {Premium = WholeLife_Monthly_IM(age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b)}
    
    #single
    #endowment assurance, EOY, single
    if(policy == EA & benefit_type == EOY & premium_type == Single) 
    {Premium = Endowment_Single_EOY(age, interestRate, term, initialExpense, claimExpense, sumAssured)}
    #endowment assurance, IM, single
    if(policy == EA & benefit_type == IM & premium_type == Single) 
    {Premium = Endowment_Single_IM(age, interestRate, term, initialExpense, claimExpense, sumAssured)}
    #term assurance, EOY, single
    if(policy == TA & benefit_type == EOY & premium_type == Single)
    {Premium = Term_Single_EOY(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b) }
    #term assurance, IM, single
    if(policy == TA & benefit_type == IM & premium_type == Single) 
    {Premium = Term_Single_IM(age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b) }
    #whole life assurance, EOY, single
    if(policy == WA & benefit_type == EOY & premium_type == Single)
    {Premium = WholeLife_Single_EOY(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b) }
    #whole life assurance, IM, single
    if(policy == WA & benefit_type == IM & premium_type == Single) 
    {Premium = WholeLife_Single_IM(age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b) }
    
    #pure endowment
    
    #Pure Endowment, Single
    if(policy == PE & premium_type == Single) 
    {Premium = PureEndowmentSingle(age, interestRate, term, initialExpense, claimExpense, sumAssured)}
    #Pure Endowment, Monthly
    if(policy == PE & premium_type == Monthly) 
    {Premium = PureEndowmentMonthly(age, interestRate, floor(term/12), initialExpense, renewalExpense, claimExpense, sumAssured)}
    #Pure Endowment, Level
    if(policy == PE & premium_type == Level) 
    {Premium = PureEndowmentLevel(age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured)}
    
    #----------------HIDING ITEMS-----------------
    observe({
      maxTerm <- 100 - input$age
      updateSliderInput(session, "term",
                        max = ifelse(maxTerm > 5, maxTerm, 5),
                        value = ifelse(input$term > maxTerm, maxTerm, input$term))})
    
    if(premium_type == Monthly){
      observe({
        maxTerm <- 100*12 - input$age*12
        updateSliderInput(session, "term",
                          max = ifelse(maxTerm > 5*12, maxTerm, 5*12),
                          value = ifelse(input$term > maxTerm, maxTerm, input$term))})
      
    }
    
    

    observe({toggle(id = "benefit_type", condition = input$policy != PE) })
    observe({toggle(id = "term", condition = input$policy != WA) })
    observe({toggle(id = "renewalExpense", condition = input$policy != WA) })
    
    if(input$policy == WA || input$policy == TA){
    observe({toggle(id = "B", condition = input$policy == input$policy) })
    observe({toggle(id = "j", condition = input$policy == input$policy) })
    observe({toggle(id = "b", condition = input$policy == input$policy) })
    }
    if(input$policy == PE || input$policy == EA){
      observe({toggle(id = "B", condition = input$policy != input$policy) })
      observe({toggle(id = "j", condition = input$policy != input$policy) })
      observe({toggle(id = "b", condition = input$policy != input$policy) })
    }
      
      
      
      
      
      
    if(premium_type == Single) {text1 = "Lump Sum Premium of $"}
    
    if(premium_type == Level) {text1 = "Annual Premium of $"} 
    
    if(premium_type == Monthly) {text1 = "Monthly Premium of $"} 
    
    Value = paste(text1, round(Premium, digits = 2), sep = "")
    
    print(Value)
    
    
  } ) 
  
  

  output$OutputText2 <- renderText({
    
    sumAssured = input$sumAssured
    
    benefit_type = input$benefit_type
    EOY = "End of Year of Death" ; IM = "Immediately after Death"
    
    if(benefit_type == EOY) {timing = "at the end of year of death"}
    if(benefit_type == IM) {timing = "immediately after death"}
    
    Text = paste("For a benefit amount of $", sumAssured, "," ,sep = "")

    print(Text)
    
    
    
  } )
  
  output$OutputText3 <-renderText({
    
    benefit_type = input$benefit_type
    EOY = "End of Year of Death" ; IM = "Immediately after Death"
    
    if(benefit_type == EOY) {timing = "at the end of year of death"}
    if(benefit_type == IM) {timing = "immediately after death"}
    
    
    Text2 = paste("That is paid ", timing, ",", sep = "")
    
    print(Text2)
    
    
  } )
  
  

  output$plot <- renderPlot({
    
    age = input$age
    sumAssured = input$sumAssured
    interestRate = input$interestRate / 100
    term = input$term
    #mortality
    if(input$mortality == "Ultimate") {lifeTable <<- mortality_select(age,"ultimate")} else {lifeTable <<- mortality_select(age,"select")}
    
    #Expenses:
    initialExpense = input$initialExpense / 100
    renewalExpense = input$renewalExpense / 100
    claimExpense = input$claimExpense / 100
    
    #Bonuses
    B = input$B
    j = input$j/100
    b = input$b/100
    
    #definitions
    EA = "Endowment Assurance" ; TA = "Term Assurance" ; PE = "Pure Endowment" ; WA = "Whole Life Assurance"
    EOY = "End of Year of Death" ; IM = "Immediately after Death"
    Level = "Annual" ; Single = "Single Lump Sum" ; Monthly = "Monthly"
    
    policy <- input$policy
    benefit_type <- input$benefit_type
    premium_type <- input$premium_type
    
    
    
    #level
    
    #endowment assurance, EOY, level
    if(policy == EA & benefit_type == EOY & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Level_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #endowment assurance, IM, level
    if(policy == EA & benefit_type == IM & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Level_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #term assurance, EOY, level
    if(policy == TA & benefit_type == EOY & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Term_Level_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #term assurance, IM, level
    if(policy == TA & benefit_type == IM & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Term_Level_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #Whole life assurance, EOY, level
    if(policy == WA & benefit_type == EOY & premium_type == Level) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Level_EOY, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #Whole life assurance, IM , level
    if(policy == WA & benefit_type == IM & premium_type == Level) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Level_IM, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    
    #monthly
    #endowment assurance, EOY, Monthly
    if(policy == EA & benefit_type == EOY & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Monthly_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #endowment assurance, IM, Monthly
    if(policy == EA & benefit_type == IM & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Monthly_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #term assurance, EOY, Monthly
    if(policy == TA & benefit_type == EOY & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Term_Monthly_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #term assurance, IM, Monthly
    if(policy == TA & benefit_type == IM & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Term_Monthly_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #whole life assurance, EOY, Monthly
    if(policy == WA & benefit_type == EOY & premium_type == Monthly) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Monthly_EOY, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #whole life assurance, IM, Monthly
    if(policy == WA & benefit_type == IM & premium_type == Monthly) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Monthly_IM, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    
    #single
    #endowment assurance, EOY, single
    if(policy == EA & benefit_type == EOY & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Single_EOY, age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #endowment assurance, IM, single
    if(policy == EA & benefit_type == IM & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Single_IM, age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #term assurance, EOY, single
    if(policy == TA & benefit_type == EOY & premium_type == Single)
    {Reserve <- function(t) {mapply(Reserve_Term_Single_EOY, age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    #term assurance, IM, single
    if(policy == TA & benefit_type == IM & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_Term_Single_IM, age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    #whole life assurance, EOY, single
    if(policy == WA & benefit_type == EOY & premium_type == Single)
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Single_EOY, age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    #whole life assurance, IM, single
    if(policy == WA & benefit_type == IM & premium_type == Single) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Single_IM, age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    
    #pure endowment
    
    #Pure Endowment, Single
    if(policy == PE & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_PureEndowmentSingle,age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #Pure Endowment, Monthly
    if(policy == PE & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_PureEndowmentMonthly,age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #Pure Endowment, Level
    if(policy == PE & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_PureEndowmentLevel,age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    
    
    plot_df2 <- data.frame(
      term = seq(0, term),
      Reserve_values = Reserve(seq(0,term))
    )
    # Plotting the Reserve values over terms with ggplot
    plot2 <- ggplot(plot_df2, aes(x = term, y = Reserve_values)) +
      geom_line(color = '#69b3a2', size = 1.5) +
      geom_point(color = '#404080', size = 3) +
      labs(
        x = "Term",
        y = "Reserve Value ($'s)",
        title = "Evolution of Reserve Value over Terms",
        subtitle = paste("A ",age," year old with a ", policy)
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = '#d3d3d3'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#f7f7f7'),
        plot.background = element_rect(fill = '#f0f0f0')
      ) +
      scale_y_continuous(labels = scales::dollar) +
      scale_x_continuous(breaks = seq(0, term, by = 1))
    

    plot2
    
    

  }) 
  
  output$OutputText4 <-renderUI({

    age = input$age
    sumAssured = input$sumAssured
    interestRate = input$interestRate / 100
    term = input$term
    #mortality
    if(input$mortality == "Ultimate") {lifeTable <<- mortality_select(age,"ultimate")} else {lifeTable <<- mortality_select(age,"select")}
    
    #Expenses:
    initialExpense = input$initialExpense / 100
    renewalExpense = input$renewalExpense / 100
    claimExpense = input$claimExpense / 100
    
    #Bonuses
    B = input$B
    j = input$j /100
    b = input$b /100
    
    #definitions
    EA = "Endowment Assurance" ; TA = "Term Assurance" ; PE = "Pure Endowment" ; WA = "Whole Life Assurance"
    EOY = "End of Year of Death" ; IM = "Immediately after Death"
    Level = "Annual" ; Single = "Single Lump Sum" ; Monthly = "Monthly"
    
    policy <- input$policy
    benefit_type <- input$benefit_type
    premium_type <- input$premium_type
    
    
    
    #level
    
    #endowment assurance, EOY, level
    if(policy == EA & benefit_type == EOY & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Level_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #endowment assurance, IM, level
    if(policy == EA & benefit_type == IM & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Level_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #term assurance, EOY, level
    if(policy == TA & benefit_type == EOY & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Term_Level_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #term assurance, IM, level
    if(policy == TA & benefit_type == IM & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_Term_Level_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #Whole life assurance, EOY, level
    if(policy == WA & benefit_type == EOY & premium_type == Level) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Level_EOY, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #Whole life assurance, IM , level
    if(policy == WA & benefit_type == IM & premium_type == Level) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Level_IM, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    
    #monthly
    #endowment assurance, EOY, Monthly
    if(policy == EA & benefit_type == EOY & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Monthly_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #endowment assurance, IM, Monthly
    if(policy == EA & benefit_type == IM & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Monthly_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    #term assurance, EOY, Monthly
    if(policy == TA & benefit_type == EOY & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Term_Monthly_EOY, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #term assurance, IM, Monthly
    if(policy == TA & benefit_type == IM & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_Term_Monthly_IM, age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #whole life assurance, EOY, Monthly
    if(policy == WA & benefit_type == EOY & premium_type == Monthly) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Monthly_EOY, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    #whole life assurance, IM, Monthly
    if(policy == WA & benefit_type == IM & premium_type == Monthly) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Monthly_IM, age, interestRate, initialExpense, renewalExpense, claimExpense, sumAssured, B, j, b, t)}}
    
    #single
    #endowment assurance, EOY, single
    if(policy == EA & benefit_type == EOY & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Single_EOY, age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #endowment assurance, IM, single
    if(policy == EA & benefit_type == IM & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_Endowment_Single_IM, age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #term assurance, EOY, single
    if(policy == TA & benefit_type == EOY & premium_type == Single)
    {Reserve <- function(t) {mapply(Reserve_Term_Single_EOY, age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    #term assurance, IM, single
    if(policy == TA & benefit_type == IM & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_Term_Single_IM, age, interestRate, term, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    #whole life assurance, EOY, single
    if(policy == WA & benefit_type == EOY & premium_type == Single)
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Single_EOY, age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    #whole life assurance, IM, single
    if(policy == WA & benefit_type == IM & premium_type == Single) 
    {term <- 100-age
      Reserve <- function(t) {mapply(Reserve_WholeLife_Single_IM, age, interestRate, initialExpense, claimExpense, sumAssured, B, j, b, t) }}
    
    #pure endowment
    
    #Pure Endowment, Single
    if(policy == PE & premium_type == Single) 
    {Reserve <- function(t) {mapply(Reserve_PureEndowmentSingle,age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #Pure Endowment, Monthly
    if(policy == PE & premium_type == Monthly) 
    {Reserve <- function(t) {mapply(Reserve_PureEndowmentMonthly,age, interestRate, term, initialExpense, claimExpense, sumAssured, t)}}
    #Pure Endowment, Level
    if(policy == PE & premium_type == Level) 
    {Reserve <- function(t) {mapply(Reserve_PureEndowmentLevel,age, interestRate, term, initialExpense, renewalExpense, claimExpense, sumAssured, t)}}
    
    
    R0 = paste("The reserve at time 0 is: $", round(Reserve(0)), sep = "")
    R1 = paste("The reserve at time 1 is: $", round(Reserve(1),2), sep = "")
    R2 = paste("The reserve at time 2 is: $", round(Reserve(2),2), sep = "")
    R3 = paste("The reserve at time 3 is: $", round(Reserve(3),2), sep = "")
    RT_1 = paste("The reserve at time ",(term -1)," is: $", round(Reserve((term -1)),2), sep = "")
    RT = paste("The reserve at time ",(term)," is: $", round(Reserve((term)),2), sep = "")
    div(class = "grey-bubble", HTML(paste(R0, "<br/>", R1, "<br/>", R2, "<br/>", R3, "<br/>", RT_1, "<br/>", RT, sep = "")))
  } )
  
} 

shinyApp(ui = ui , server = server)

#rsconnect::deployApp('//Users//zachbushby//Documents//Uni//2023//Sem 2//ETC3530//Assignment//ZB_A1_ETC3530.R')
#rsconnect::deployApp('ZB_A1_ETC3530.R')





