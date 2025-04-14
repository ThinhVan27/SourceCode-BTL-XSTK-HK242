# Đọc dữ liệu từ file CSV
data <- read.csv("Data_Clean.csv")

head(data)

# Chọn các cột cần thiết
selected_data <- data[, c("texture_rate_value", "core_speed_value", "memory_value", 
                          "memory_bandwidth_value", "memory_bus_value")]

# Đổi tên cột
colnames(selected_data) <- c("TextureRate", "CoreSpeed", "Memory", "MemoryBandwidth", "MemoryBus")

# Xử lý dữ liệu thiếu
selected_data <- na.omit(selected_data)

# Xây dựng mô hình hồi quy tuyến tính bội
model <- lm(TextureRate ~ CoreSpeed + Memory + MemoryBandwidth + MemoryBus, data = selected_data)

# Xem kết quả
summary(model)