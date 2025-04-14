library(datasets)
library(graphics)
library(dplyr)
library(tidyr)
library(zoo)
library(janitor)
library(ggplot2)
library(ggpubr)
library(car)
library(agricolae)

data = read.csv("C:/Users/PC/Downloads/Data_Clean.csv")

apply(is.na(data),2,mean)

boxplot(memory_speed_value ~ manufacturer, data,
        main=expression(bold("Boxplot of Memory_Speed and Manufacturer")),
        col="lightblue")
table(data$manufacturer)
# Shapiro-Wilk test cho từng nhóm Manufacturer
# Lấy danh sách các nhà sản xuất duy nhất
manufacturers <- unique(data$manufacturer)

# Thiết lập layout 2x2 cho 4 biểu đồ trên cùng một trang
par(mfrow = c(2, 2))

# Lặp qua từng nhà sản xuất và vẽ Q-Q plot
for (manu in manufacturers) {
  cat("Shapiro-Wilk test for", manu, "\n")
  print(shapiro.test(data$memory_speed_value[data$manufacturer == manu]))
  
  qqnorm(data$memory_speed_value[data$manufacturer == manu], main = paste("Q-Q Plot for", manu))
  qqline(data$memory_speed_value[data$manufacturer == manu], col = "red")
}

# Reset lại layout về mặc định sau khi vẽ xong
par(mfrow = c(1, 1))

leveneTest(memory_speed_value ~ manufacturer, data=data)

anova_res <- aov(memory_speed_value ~ manufacturer, data = data)
summary(anova_res)

lsd_test <- LSD.test(anova_res, "manufacturer", p.adj = "none", group = FALSE) 
print(lsd_test)
# plot(lsd_test, main = "Fisher's LSD Test for Manufacturer")
