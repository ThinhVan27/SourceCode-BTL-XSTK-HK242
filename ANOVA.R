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

# Lấy danh sách các nhà sản xuất duy nhất
manufacturers <- unique(data$manufacturer)

# Thiết lập layout 2x2 cho 4 biểu đồ trên cùng một trang
par(mfrow = c(2, 2))

# Lặp qua từng nhà sản xuất và vẽ Q-Q plot
for (manu in manufacturers) {
  qqnorm(data$memory_speed_value[data$manufacturer == manu], main = paste("Q-Q Plot for", manu))
  qqline(data$memory_speed_value[data$manufacturer == manu], col = "red")
}

# Reset lại layout về mặc định sau khi vẽ xong
par(mfrow = c(1, 1))

leveneTest(memory_speed_value ~ manufacturer, data=data)

anova_res <- aov(memory_speed_value ~ manufacturer, data = data)
summary(anova_res)

hsd_res <- TukeyHSD(anova_res)
print(hsd_res)
plot(hsd_res)