E = 19 #B
Ri = c(11,16,20,14) #Om
Ra = 0.8 #Om
Rv = 120 #Om

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
Ka = 0.2

#Межі основної інструментальної похибки А амперметра
cal_Ka <- function(Ka, I){
  Ka * I / 100
}

#Скорегування значення вимірювального струму та результат 
cal_A_res <- function(Ia, Dm){
  I = Ia + Dm
  #Похибка
  Ip = cal_Ka(Ka, Ia)
  cat("Ip:t+-", Ip, "A\n")
  cat("I(без урахування): \t", Ia, "\t A \n")
  cat("I t", I, "t+-", Ip, "\tA \n")
  
  #зюереження поза контекстом функції (як глобальні змінні) та округлення
  Ip <<-around(Ip, 5)
  Ia <<-around(Ia, 1)
  I_res <<- around(I, 1)
}

#Повертає струм амперметра
cal_A <- function(Ra){
  R = Ri[1] + pR(Ri[2], Ri[3] + Ra) + Ri[4]
  I = E/R
  
  #для ділянки з R3 та A
  Ra3 = Ra + Ri[3]
  Ua3 = E - I*(Ri[1] + Ri[4])
  
  Ia = Ua3 / Ra3
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
  cat("Δm:\t", Dm, "\tA\n")
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
Kv = 1.5
Umax = 10 #В

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

#Струм вольтметра
cal_V <- function(Rv){
  RIRv = Ri[1]

#для не ідеального 
if(Rv != 0){
  RIRv = pR(Ri[1])
}

R23 = pR(Ri[2], Ri[3])
R4 = Ri[4]
R = RIRv + R23 + R4
I = E/R

#для ділянки з R1 та V
U23 = I * R23
U4 = I * Ri[4]
UI = E - U23 - U4

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
  cat("Δm:\t", Dm, "\tB\n")
  cat("δm:\t", dm, "\t%\n")
  
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
cat("Δm:\t", Dma, "\tA\n")
cat("δm:\t", dma, "\t%\n")

#результат вимірювання
cat("Ip:\t+-", Ip, "\tA\n")
cat("I(без урахування):\t", Ia, "t+-", Ip, "\tA\n")
cat("I:\t", I_res, "t+-", Ip, "\tA\n")

cat("\n Для V:\n")

#Значення похибки
cat("Δm:\t", Dmv, "\tB\n")
cat("δm:\t", dmv, "\t%\n")
#Результат вимірювання
cat("Up:\t+-", Up, "\tB\n")
cat("U(без урахування):\t", Uv, "t+-", Up, "\tВ\n")
cat("Up:\t", U, "t+-", Up, "\tB\n")