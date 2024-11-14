
# Input data
nums_inp <- c(80.98, 79.26, 80.88, 80.03, 79.73, 80.98, 79.93, 81.43, 78.05, 81.89,
              78.31, 78.81, 80.35, 80.05, 80.62, 74.46, 81.86, 79.73, 80.30, 81.86,
              80.33, 78.33, 77.11, 81.19, 78.09, 79.36, 77.26, 79.02, 83.64, 81.14,
              83.01, 79.25, 79.90, 80.17, 76.32, 77.92, 79.15, 82.43, 79.84, 79.24,
              80.26, 86.26, 81.16, 80.36, 81.12, 79.97, 79.41, 81.06, 78.08, 80.87,
              79.83, 80.29)


count <- length(nums_inp)
print(paste("The count of numbers is:", count))

nums <- sort(nums_inp)
print(nums)

#Среднее значение / 
mean_nums <- mean(nums)
print(mean_nums)

#Средне квадратичное уровнение / s B, для таблицы
sd_nums <- sd(nums)
print(sd_nums)

#Я сигма
th_sigma <- 3 * sd_nums

# Верхня межа та нижня
lower_bound <- mean_nums - 3 * sd_nums
upper_bound <- mean_nums + 3 * sd_nums

#1. Поиск промахов:  значення < нижньої межі або значення > верхньої межі
misses <- nums[abs(nums - mean_nums) > th_sigma]

#2. Тоже самое только через другую формулу
outliers <- nums[nums < lower_bound | nums > upper_bound]

print(paste("Промахи:", misses))

#Результат. Наши промахи мы убираем с отсортированой выборки
filtered_nums <- nums[!(nums %in% misses)]
filtered_nums

#My 51 - значит интервалов 7
k <- 7

#Диапазоны = (Мах - Мин / интервал). Получаем дельта Хі как дистанцию между интервалами
#Но записывать Xi не будем так как границами между точками интервалов и будут границы для прямоугольников
bin_edges <- seq(min(filtered_nums), max(filtered_nums), length.out = k + 1)
bin_edges

#Pi * n 
expected_freq <- diff(pnorm(bin_edges, mean = mean(filtered_nums), sd = sd(filtered_nums))) * length(filtered_nums)
expected_freq

#Наши частоты
observed_freq <- hist(filtered_nums, breaks = bin_edges, plot = FALSE)$counts
observed_freq

#Pi
P_i <- observed_freq / length(filtered_nums)
P_i

# Проверка и установка
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Создаем датафрейм данных для фильтрованных_чисел
data <- data.frame(value = filtered_nums)

library(ggplot2)

library(ggplot2)

hist_plot <- ggplot(data, aes(x = value)) +
  geom_histogram(aes(y = ..count../sum(..count..)), breaks = bin_edges, color = "black", fill = "#D6D6D6") +
  stat_function(fun = dnorm, args = list(mean = mean(filtered_nums), sd = sd(filtered_nums)), color = "red", size = 1) +
  theme_minimal() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        aspect.ratio = 5 / 8) +
  #Горизонтальная стрелка 
  annotate("segment", x = min(filtered_nums) - 1 , y = 0, xend = max(filtered_nums) + 2.5, yend = 0, arrow = arrow(type = "closed", length = unit(0.15, "inches")))+
  # Надпись "Pi"
  annotate("text", x = min(filtered_nums) - 1, y = 0.5, label = "Pi", vjust = 0.5, hjust = 0.5) +
  #Вертикальная стрелка 
  annotate("segment", x = min(filtered_nums) - 0.37, y = 0, xend = min(filtered_nums)  - 0.37, yend = 0.5, arrow = arrow(type = "closed", length = unit(0.15, "inches")))+
  # Надпись "i"
  annotate("text", x = max(filtered_nums) + 1, y = 0, label = "ΔXi", vjust = -1, hjust = -2) 
  # Добавляем метки с высотами прямоугольников
  #geom_text(aes(y = ..count../sum(..count..), label = format(round(..count../sum(..count..), 4), nsmall = 4)), stat = "bin", breaks = bin_edges, vjust = -0.5, size = 3)

print(hist_plot)


#µ = (Σx_i) / N; среднее U- для табл 
mean_filtered_nums <- mean(filtered_nums)
cat("U, B :", mean_filtered_nums, "\n")

#σ = sqrt(Σ(x_i - µ)^2 / (N-1)). Среднее квадратичное 
sd_filtered_nums <- sd(filtered_nums)
cat("Sx:", sd_filtered_nums, "\n")

bin_edges
# Функция для вычисления вероятности попадания в интервал с помощью функции Лапласа
laplace_prob <- function(lower, upper, mean, sd) {
  return(pnorm(upper, mean = mean, sd = sd) - pnorm(lower, mean = mean, sd = sd))
}

# Вычисление вероятностей для каждого интервала
Prob <- sapply(1:(length(bin_edges) - 1), function(i) {
  laplace_prob(bin_edges[i], bin_edges[i + 1], mean_filtered_nums, sd_filtered_nums)
})

Prob

#Формула X^2 - тоже не как в методичке, где expected_freq - Pi * n 
chi_sq_stat <- sum((observed_freq - (Prob * length(filtered_nums)))^2 / (Prob * length(filtered_nums)))
chi_sq_stat

#Вычислить степени свободы k
#k = количество_интервалов - количество_параметров_распределения - 1
#Для нормального распределения мы оцениваем 2 параметра (среднее и стандартное отклонение), 
#поэтому формула становится:
df <- length(observed_freq) - 3
df

#Значения что будем искать по таблице как попросил препод
#Xk^2; 1/q 
#Xk^2; 1 - 1/2 q

#Но сдесь оно считаеться адекватно через одну функцию
#Формула проверки через функцию qchisq
critical_value <- qchisq(0.95, df)
critical_value

if (chi_sq_stat < critical_value) {
  cat("Выборка следует нормальному распределению при доверительном уровне 95%.\n")
} else {
  cat("Выборка не соответствует нормальному распределению при доверительном уровне 95%.\n")
}

# s_x - стандартная ошибка среднего - s / sqrt(n) / s_U-
sd_filtered_nums_xi = sd_filtered_nums / sqrt(length(filtered_nums))
cat("s_u:", sd_filtered_nums_xi, "\n")

#Первое 95, второе 99
P <- c(0.95, 0.99)

# ОН доебеться до 3.105807 
# Критическое значение t-распределения Стьюдента
t_value <- qt(1 - (1 - P) / 2, df = n - 1)
cat("t-распределения:", t_value, "\n")

#e
epsilon <- t_value * sd_filtered_nums_xi
cat("e:", epsilon, "\n")

# Доверительные границы для среднего значения выборки
lower_bound <- mean_filtered_nums - epsilon
upper_bound <- mean_filtered_nums + epsilon
cat("Доверительные границы e для верхней:", upper_bound, "\n", "Для нижней:", lower_bound, "\n")

#ΔV
delta_V <- (0.2 - 0.05)/100 * mean_filtered_nums + (0.05 / 100) * 150
cat("V:", delta_V, "\n")

#SΣ
S_E <- sqrt(sum((delta_V^2 / 3) + sd_filtered_nums_xi^2))
cat("SΣ:", S_E, "\n")

#K
K <- (epsilon + delta_V) / (sd_filtered_nums_xi + sqrt(sum(delta_V^2 / 3)))
cat("K 0.9 and 0.95:", K, "\n")

#Δ
delta <- S_E * K
cat("Δ 0.9 and 0.95:", delta, "\n")