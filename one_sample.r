library(datasets)
library(graphics)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Load the data from the CSV file
df <- read.csv("Data_Clean.csv", header = TRUE, stringsAsFactors = FALSE)

# In ra histogram
hist(df$memory_speed_value, main = "Histogram", xlab = "Values", col = "skyblue", border = "white")

# Tìm cột có phân phối gần phân phối chuẩn nhất
# Tính p-value Shapiro-Wilk cho từng cột số trong df
p_values <- sapply(df, function(x) {
  if(is.numeric(x)) {
    return(shapiro.test(x)$p.value)
  } else {
    return(NA)  # Nếu cột không phải số thì bỏ qua
  }
})

# In giá trị p của từng cột ra
print(p_values)

# Tìm tên cột có giá trị p lớn nhất (tức là gần đúng phân phối chuẩn nhất)
best_col <- names(p_values)[which.max(p_values)]
cat("Cột có phân phối gần chuẩn nhất là:", best_col, "với p-value =", p_values[best_col], "\n")

attach(df)
plot(density(memory_speed_value)) # Biểu đồ plot
qqnorm(memory_speed_value) # Biểu đồ QQ
qqline(memory_speed_value, col="red") # Biểu đồ QQ

# Kiểm định một mẫu
res <- t.test(memory_speed_value, data=df, mu = 1000, alternative = "less", conf.level = 0.95)
print(res)
t_value <- res$statistic # Giá trị quan sát
print(t_value)
qt(p = 0.05, df = 3339) # Tìm ra miền bác bỏ

