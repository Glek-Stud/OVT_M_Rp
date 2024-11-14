# Input data
nums_20 <- c(80.98, 79.26, 80.88, 80.03, 79.73, 80.98, 79.93, 81.43, 78.05, 81.89,
             78.31, 78.81, 80.35, 80.05, 80.62, 74.46, 81.86, 79.73, 80.30, 81.86)


nums_5 <- c(74.46, 81.86, 79.73, 80.30, 81.86)


n <- length(nums_inp)
print(paste("The count of numbers is:", n))

print("q - 0.05")

romanovsky <- function(sample) {
  n <- length(sample)
  nums <- sort(sample)
  #µ = (Σx_i) / N; среднее U- для табл 
  x_ <- mean(nums)
  #σ = sqrt(Σ(x_i - µ)^2 / (N-1)). Среднее квадратичное 
  Sx <- sd(nums)
  
  if (n == 20) {
    # Первое действие
    print("Выборка размером 20")
    Bt_20 <- 2.78
    
    filter_nums <- nums[(abs(nums - x_)/Sx) < Bt_20]
    table_solve(filter_nums)
    
  } else if (n == 5) {
    # Второе действие
    print("Выборка размером 5")
    Bt_5 <- 1.905
    
    filter_nums <- nums[(abs(nums - x_)/Sx) < Bt_5]
  
    table_solve(filter_nums)
    
  } else {
    # Третье действие
    print("Выборка другого размера")
  }
}

table_solve <- function(fd_sample){
  
  cat("Количество чисел:", length(fd_sample) , "\n")
  x_fd <- mean(fd_sample)
  Sx_fd <- sd(fd_sample)
  
  cat("U, B", x_fd , "\n")
  cat("S, B", Sx_fd , "\n")
  
  Su = Sx_fd / sqrt(length(fd_sample))
  cat("S, U", Su , "\n")
  
  a <- c(0.05, 0.01) # Уровень значимости
  size_n <- length(fd_sample) # Размер выборки
  df <- size_n - 1  # Число степеней свободы
  
  t <- round(qt(1 - a/2, df) , 2)
  cat("t:", t, "\n")
  
  epsilon <- t * Su 
  cat("e| 0.95 and 0.99:", epsilon, "\n")
  
  #ΔV
  delta_V <- (0.2 - 0.05)/100 * x_fd + (0.05 / 100) * 150
  cat("V:", delta_V, "\n")
  
  #SΣ
  S_E <- sqrt(sum((delta_V^2 / 3) + Su^2))
  cat("SΣ:", S_E, "\n")
  
  #K
  K <- (epsilon + delta_V) / (Su + sqrt(sum(delta_V^2 / 3)))
  cat("K| 0.95 and 0.99:", K, "\n")
  
  #Δ
  delta <- S_E * K
  cat("Δ| 0.95 and 0.99:", delta, "\n")
}

romanovsky(nums_20)
romanovsky(nums_5)