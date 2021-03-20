library(stringr)
###########


BaseAddPrincipal <- 1000
AddPrincipal <- 0
#OrigPrincipal <- AddPrincipal #Only use if Principal will increase by year
OrigLoanAmount <- 189000.00
AnnualInterestRate <- 0.03875
BaseLoanPayment <- 888.75
LoanPayment <- BaseLoanPayment

Actual <- FALSE
MonthlyInterestRate <- AnnualInterestRate / 12
PayNum <- 1
Year <- 0
Interest <- OrigLoanAmount * MonthlyInterestRate
Principal <- LoanPayment - Interest
Balance <- OrigLoanAmount - Principal - AddPrincipal
CumInterest <- Interest
PresentValue_CumInterest <- CumInterest
Date_Month <- 1
Date_Year <- 18

AddPrincipal <- NA


#########
#HISTORY#
#########


MortgagePaymentHistory <- read.csv("/media/mike/UNTITLED/Secure/MortgagePaymentHistory.csv", 
                                   stringsAsFactors=FALSE)

if(length(MortgagePaymentHistory$PayNum) > 0){
LoanPayment <- MortgagePaymentHistory[PayNum,]$TOTAL_PAYMENT - MortgagePaymentHistory[PayNum,]$ESCROW
Date_Month <- MortgagePaymentHistory[PayNum,]$MONTH
Date_Year <- MortgagePaymentHistory[PayNum,]$YEAR
Actual <- TRUE
}
AmortSchedule <- data.frame(Actual, PayNum, Date_Month , Date_Year , AnnualInterestRate , LoanPayment, Principal, Interest, AddPrincipal, Balance, CumInterest, PresentValue_CumInterest)


while(PayNum < length(MortgagePaymentHistory$PayNum)){
  PayNum <- PayNum + 1
  if(PayNum > 11){Year <- PayNum %/% 12}
  Date_Month <- MortgagePaymentHistory[PayNum,]$MONTH
  Date_Year <- MortgagePaymentHistory[PayNum,]$YEAR
  
  #Only use if Principal will increase by year
  #AddPrincipal <- OrigPrincipal + Year * (OrigPrincipal*.15) 
  Interest <- Balance * MonthlyInterestRate
  Principal <- LoanPayment - Interest
  Balance <- Balance - Principal 
  CumInterest <- CumInterest + Interest
  PresentValue_Interest <- Interest
  if(PayNum > 11){PresentValue_Interest <- Interest * (1 /(1 + .04)^Year)} 
  PresentValue_CumInterest <- PresentValue_CumInterest + PresentValue_Interest
  LoanPayment <- MortgagePaymentHistory[PayNum,]$TOTAL_PAYMENT - MortgagePaymentHistory[PayNum,]$ESCROW
  
  AmortScheduleNewRow <- data.frame(Actual, PayNum, Date_Month , Date_Year , AnnualInterestRate , LoanPayment, Principal, Interest, AddPrincipal, Balance, CumInterest, PresentValue_CumInterest)
  AmortSchedule <- rbind(AmortSchedule, AmortScheduleNewRow)  
  

  
  
}

AddPrincipal <- BaseAddPrincipal
LoanPayment <- BaseLoanPayment
Actual <- FALSE

while(Balance > 0){
PayNum <- PayNum + 1
Date_Month <- Date_Month + 1
if(Date_Month > 12){
Date_Month <- 1
Date_Year <- Date_Year + 1
}

if(PayNum > 11){Year <- PayNum %/% 12}
#Only use if Principal will increase by year
#AddPrincipal <- OrigPrincipal + Year * (OrigPrincipal*.15) 
Interest <- Balance * MonthlyInterestRate
Principal <- LoanPayment - Interest
Balance <- Balance - Principal - AddPrincipal 
CumInterest <- CumInterest + Interest
PresentValue_Interest <- Interest
if(PayNum > 11){PresentValue_Interest <- Interest * (1 /(1 + .04)^Year)} 
PresentValue_CumInterest <- PresentValue_CumInterest + PresentValue_Interest

AmortScheduleNewRow <- data.frame(Actual, PayNum, Date_Month , 
                              Date_Year , AnnualInterestRate , LoanPayment, 
                              Principal, Interest, AddPrincipal, Balance,
                              CumInterest, PresentValue_CumInterest)
AmortSchedule <- rbind(AmortSchedule, AmortScheduleNewRow)
}
AmortSchedule$PrincipalReduction <- AmortSchedule$Principal + AmortSchedule$AddPrincipal

AmortSchedule[PayNum,]$Date_Month
AmortSchedule[PayNum,]$Date_Year

AmortSchedule$Date <- as.Date(paste("20", Date_Year, "-",  str_pad(Date_Month, width = 2, side = c("left"), pad = "0"),"-01", sep = ""))
AmortSchedule$Date < as.Date()- 

  
  
  
  
#write.csv(AmortSchedule, file = "AmortSchedule.csv")
