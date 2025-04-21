library(readr)
GPU <- read_csv("C:/Users/pc/Downloads/Data_Clean.csv")
#1.Các đặc trưng của mẫu (cho biến định lượng)
head(GPU,5)
DL<-GPU[,c("core_speed_value","max_power_value","memory_value",
           "memory_bandwidth_value", "memory_speed_value",
           "memory_bus_value","texture_rate_value")]
summary(DL)
summary(GPU)
dactrung <- apply(DL, 2, function(x) c(summary(x),Sd=sd(x),Var=var(x)))

#2.Bảng tần số


freq(GPU$architecture, plain.ascii = FALSE, style = "rmarkdown")
# Bảng tần suất cho biến best_resolution
freq(GPU$best_resolution, plain.ascii = FALSE, style = "rmarkdown")

# Bảng tần suất cho biến manufacturer
freq(GPU$manufacturer, plain.ascii = FALSE, style = "rmarkdown")




tao_nhieu_bang_tan_suat <- function(input_df, cac_bien) {
  ds_bang <- list()
  
  for (bien in cac_bien) {
    freq_tbl <- freq(input_df[[bien]], plain.ascii = TRUE, style = "simple")
    df_freq <- as.data.frame(freq_tbl)
    ds_bang[[bien]] <- df_freq
  }
  
  return(ds_bang)
}
cac_bien_phan_loai <- c("architecture", "best_resolution", "manufacturer")

# Gọi hàm với dữ liệu đầu vào là GPU
ds_bang_tan_suat <- tao_nhieu_bang_tan_suat(GPU, cac_bien_phan_loai)

# Xem kết quả bảng tần suất cho manufacturer
print(ds_bang_tan_suat$manufacturer)
# Xem kết quả bảng tần suất cho architecture
print(ds_bang_tan_suat$architecture)
# Xem kết quả bảng tần suất cho best_resolution
print(ds_bang_tan_suat$best_resolution)




#3.Histogram 
hist(GPU$max_power_value, main = "Histogram của năng lượng cực đại", 
     xlab = "Giá trị", 
     ylab = "Tần suất", 
     col = "skyblue", 
     labels = T,ylim=c(0,600))

#4.Boxplot
boxplot(GPU$max_power_value~GPU$best_resolution)
#5. Pairplot 
head(DL,1)
p <- ggpairs(DL[, c("core_speed_value", "max_power_value", "memory_value","memory_bandwidth_value",
                    "memory_speed_value","memory_bus_value","texture_rate_value")],
             title = "Pair Plot cho các giá trị liên quan đến GPU")
p + theme(plot.title = element_text(hjust = 0.5))
