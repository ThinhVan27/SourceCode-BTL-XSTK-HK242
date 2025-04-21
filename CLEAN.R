library(tidyr)
library(dplyr)
library(janitor)
# đọc và lấy dữ liệu từ file
GPU = read.csv("ALL_GPUs.csv", na.strings = c("", "N/A"))
GPU<-GPU[,c("Architecture", "Best_Resolution","Core_Speed", "Max_Power", "Memory",
"Memory_Bandwidth" , "Memory_Speed" , "Manufacturer", "Memory_Bus", "Texture_Rate")]
# lưu giá trị vào biến GPU2
GPU2 <- clean_names(GPU)

# Tách các cột có giá trị và đơn vị thành 2 cột riêng lẻ
GPU2 <- remove_empty(GPU, which = c("rows", "cols"), quiet = FALSE)
GPU2 <- separate(GPU ,col = Core_Speed , into = c("Core_Speed_value",
"Core_Speed_unit "),sep=" ", fill ="right")
GPU2$Core_Speed_value <- as.numeric(GPU2$Core_Speed_value)
GPU2 <- separate(GPU2 ,col = Max_Power , into = c("Max_Power_value",
"Max_Power_unit "),sep=" ", fill ="right")
GPU2$Max_Power_value <- as.numeric(GPU2$Max_Power_value)
GPU2 <- separate(GPU2 ,col = Memory , into = c("Memory_value",
"Memory_unit "),sep=" ", fill ="right")
GPU2 <- separate(GPU2 ,col = Memory_Bandwidth , into =
c("Memory_Bandwidth_value", "Memory_Banwidth_unit "),
sep="(?<=\\d)(?=[A-Za-z])", fill ="right")
GPU2$Memory_Bandwidth_value <- as.numeric(GPU2$Memory_Bandwidth_value)
GPU2 <- separate(GPU2 ,col = Memory_Speed , into =
c("Memory_Speed_value", "Memory_Speed_unit "),sep=" ", fill ="right")
GPU2$Memory_Speed_value <- as.numeric(GPU2$Memory_Speed_value)
GPU2 <- separate(GPU2 ,col = Memory_Bus , into =
c("Memory_Bus_value", "Memory_Bus_unit "),sep=" ", fill ="right")
GPU2$Memory_Bus_value <- as.numeric(GPU2$Memory_Bus_value)
GPU2 <- separate(GPU2 ,col = Texture_Rate , into =
c("Texture_Rate_value", "Texture_Rate_unit "),sep=" ", fill ="right")
GPU2$Texture_Rate_value <- as.numeric(GPU2$Texture_Rate_value)
# kiểm tra số lượng đơn vị của mỗi thuộc tính
    table(GPU2$Core_Speed_unit)
    table(GPU2$Max_Power_unit)
    table(GPU2$Memory_unit)
    table(GPU2$Memory_Bandwidth_unit)
    table(GPU2$Memory_Speed_unit)
    table(GPU2$Memory_Bus_unit)
    table(GPU2$Texture_Rate_unit)
# chuyển đổi các cột số thành median


GPU2 <- GPU2 %>%
mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
# lưu các dữ liệu vào biến tạm và tiến hành xóa các thuộc tính có dữ liệu khuyết bị ít
temp <- clean_names(GPU2)
temp = temp[!is.na(GPU2$Memory_Speed_value)]
temp = temp[!is.na(GPU2$Memory_Speed_value), ]
temp = temp[!is.na(GPU2$Architecture), ]
temp = temp[!is.na(GPU2$Memory_Bandwidth_value), ]
temp = temp[!is.na(GPU2$Memory_Bus_value), ]

# Lấp đầy các cột đơn vị và xóa các thuộc tính có dữ liệu khuyết lớn
    temp$Best_Resolution[is.na(temp$Best_Resolution)] = "4096x2160" 
    temp$memory_bus_unit[is.na(temp$memory_bus_unit) | temp$memory_speed_unit
    == ""] <- "Bit"
    temp$memory_speed_unit[is.na(temp$memory_speed_unit) | temp$memory_speed_unit == ""] <- "MHz"
    
    temp$Core_Speed_value[is.na(temp$Core_Speed_value)] = median(temp$Core_Speed_value, na.rm=T)
    temp$Core_Speed_unit[is.na(temp$Core_Speed_unit)] = "MHz"
    
    temp$Max_Power_value[is.na(temp$Max_Power_value)] = median(temp$Max_Power_value, na.rm=T)
    temp$Max_Power_unit[is.na(temp$Max_Power_unit)] = "Watts"
    
    temp$Memory_value[is.na(temp$Memory_value)] = median(temp$Memory_value, na.rm=T)
    temp$Memory_unit[is.na(temp$Memory_unit)] = "MB"
    
    temp$Texture_Rate_value[is.na(temp$Texture_Rate_value)] = median(temp$Texture_Rate_value, na.rm=T)
    temp$Texture_Rate_unit[is.na(temp$Texture_Rate_unit)] = "GTexel/s"

    temp$memory_banwidth_unit[is.na(temp$memory_banwidth_unit) | temp$memory_banwidth_unit == ""] <- "GB/sec"
    temp = temp[temp$Memory_Bandwidth_unit != "MB/sec", ]

    # kiểm tra lại tỷ lệ khuyết
    GPU2 <- GPU2[rownames(temp),]
    apply(is.na(GPU2),2,mean)
    
    # ghi dữ liệu đã clean ra file
    write.csv(GPU, "Data_Clean.csv", row.names = FALSE)