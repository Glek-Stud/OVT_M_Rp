pR <- function(R1, R2){
  R1 * R2 /(R1 + R2)
}

cal_A <- function(Ra){
  R = Ri[1] + pR(Ri[2], Ri[3]) + Ra + Ri[4] # Змініть індекси Ri
  I = E/R
  
  #для ділянки з R4 та A
  Ra4 = Ra + Ri[4]
  Ua4 = E - I*(Ri[1] + pR(Ri[2], Ri[3])) # Змініть індекси Ri
  
  Ia = Ua4 / Ra4
  return(Ia)
}

cal_V <- function(Rv){
  RIRv = Ri[2] # Змініть індекс Ri
  
  if(Rv != 0){
    RIRv = pR(Ri[2], Rv) # Змініть індекс Ri
  }
  
  R = Ri[1] + pR(RIRv, Ri[3]) + Ri[4]
  I = E/R
  
  #для ділянки з R2 та V
  U_not_R2 = I * (Ri[1] + Ri[4] + pR(RIRv, Ri[3]) - RIRv)
  UI = E - U_not_R2 
  
  cat("UI: \t", UI, "tB\n")
  return(UI)
}