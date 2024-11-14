E = 100 #B
Ri = c(20,25,25,30) #Om
Ra = 2 #Om
Rv = 1000 #Om

pR <- function(R1, R2){
  R1 * R2 /(R1 + R2)
}

#round
around <- function(number, digits){
  #отримуємо модуль
  module = abs(number)
  #отримуємо знак
  sign = number / abs(number)
  #отримуємо останню цифру по модулю
  digit = as.integer(module * 10^digits)%%10
  #round округлює з 6, а не з 5, тому додаемо похибку
  p = 0.1
  #якщо 0-2, то до наступного
  if(digit < 3){
    digits = digits + 1
    #точності 0.1 то р = 0.01
    p = p^(digits + 1)
    return(sign * round(module + p,digits))
    #iнакше до даної точності 
  } else {
    return(sign * round(module, digits))
  }
}

# 
arv <- function(vector, digits){
  for(i in seq(length(vector))){
    vector[i] = around(vector[i], digits)
  }
  return(vector)
}



#I
cat("dДля А: \n\n")
Ka = 2
Imax = 2

#Межі основної інструментальної похибки А амперметра
cal_Ka <- function(Ka, Imax){
  Ka * Imax / 100
}

#Скорегування значення вимірювального струму та результат 
cal_A_res <- function(Ia, Dm){
  I = Ia + Dm
  #Похибка
  Ip = cal_Ka(Ka, Imax)
  cat("Ip:t+-", Ip, "A\n")
  cat("I(без урахування): \t", Ia, "\t A \n")
  cat("I t", I, "t+-", Ip, "\tA \n")
  
  #зюереження поза контекстом функції (як глобальні змінні) та округлення
  Ip <<-around(Ip, 1)
  Ia <<-around(Ia, 1)
  I_res <<- around(I, 1)
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

#Оцінити значення методичної похибки викликаної внутрішнім опором ЗВ
met_pox_I <- function(){
  Ia = cal_A(Ra)
  I = cal_A(0)
  cat("Ia:\t", Ia, "\tA \n")
  cat("I (без А):\t", I, "\t A:\n")
  
  #Обчислення похибки
  #Ia - з урахуванням опору на ділянці кола
  #I - без урахування опору на ділянці кола
  Dm = Ia - I
  dm = Dm/I * 100
  cat("?m:\t", Dm, "\tA\n")
  cat("бm:\t", dm, "t%\n")
  
  cal_A_res(Ia, Dm)
  
  #збереження поза контекстом функції(як глобальні змінні)
  #та округлення
  Dma <<- around(Dm, 1)
  dma <<- around(dm, 1)
}

met_pox_I()
#U
cat("dДля V: \n\n")
Kv = 1
Umax = 100 #В

#Межі основної інструментальної похибки V вольтметра
cal_KV <- function(Kv, Umax){
  Kv * Umax / 100
}

#Скорегування значення виміряного струму
#та результат TODO
cal_V_res <- function(Uv, Dm){
  U = Uv + Dm
  #похибка
  Up = cal_KV(Kv, Umax)
  cat("Up:\t+-", Up, "\tB\n")
  cat("U(без урахування):\t", Uv, "t+-", Up, "\tВ\n")
  cat("Up:\t", U, "t+-", Up, "\tB\n")
  #збереження поза контекстом функціх (як глобальні змінн)
  #то округлення
  Up <<- around(Up, 1)
  Uv <<- around(Uv, 1)
  U <<- around(U, 1)
}


cal_V <- function(Rv){
  RIRv = Ri[2] # Змініть індекс Ri
  
  if(Rv != 0){
    RIRv = pR(Ri[2], Rv) # Змініть індекс Ri
  }
  
  R = Ri[1] + pR(RIRv, Ri[3]) + Ri[4]
  I = E/R
  
  #для ділянки з R2 та V
  U_not_R2 = I * (Ri[1] + Ri[4] + pR(Ri[2], Ri[3]) - Ri[2])
  UI = E - U_not_R2 
  
  cat("UI: \t", UI, "tB\n")
  return(UI)
}

#Оцінити значення методичної похибкиб викликаної внутрішнім опором Зв
met_pox_U <- function(){
  #ділянки без V
  Ud = cal_V(0)
  #ділянки з V
  #UI = cal_V(Rv)
  
  #Обчислення похибки
  RI = Ri[1]
  Dm = -Ud/(1 + Rv/RI)
  dm = Dm/Ud * 100
  cat("?m:\t", Dm, "\tB\n")
  cat("?m:\t", dm, "\t%\n")
  
  cal_V_res(Ud, Dm)
  
  #збереження поза контекстом функції(як глобальні змінні) та округлення
  Dmv <<- around(Dm, 1)
  dmv <<- around(dm, 1)
}

met_pox_U()

#rounding
cat("\n Округлення: \n")

cat("\n Для А: \n")

#Значення похибки
cat("?m:\t", Dma, "\tA\n")
cat("?m:\t", dma, "\t%\n")

#результат вимірювання
cat("Ip:\t+-", Ip, "\tA\n")
cat("I(без урахування):\t", Ia, "t+-", Ip, "\tA\n")
cat("I:\t", I_res, "t+-", Ip, "\tA\n")

cat("\n Для V:\n")

#Значення похибки
cat("?m:\t", Dmv, "\tB\n")
cat("?m:\t", dmv, "\t%\n")
#Результат вимірювання
cat("Up:\t+-", Up, "\tB\n")
cat("U(без урахування):\t", Uv, "t+-", Up, "\tВ\n")
cat("Up:\t", U, "t+-", Up, "\tB\n")