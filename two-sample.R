GPU = read.csv("GPUs_Clean.csv")
data = GPU[, c("core_speed_value", "manufacturer", "memory_value")]

amd_core_speed <- data[data$manufacturer == "AMD" & data$memory_value == 2048, c("core_speed_value")]
nvidia_core_speed <- data[data$manufacturer == "Nvidia" & data$memory_value == 2048, c("core_speed_value")]

# Q-Q Plot for 2048-memory-speed AMD GPUs
plot.new()
qqnorm(amd_core_speed, col = "black", pch = 1, main = "Q-Q Plot for 2048-memory-speed AMD GPUs")
qqline(amd_core_speed, col = "red")

#Q-Q Plot for 2048-memory-speed Nvidia GPUs
plot.new()
qqnorm(nvidia_core_speed, col = "black", pch = 1, main = "Q-Q Plot for 2048-memory-speed Nvidia GPUs")
qqline(nvidia_core_speed, col = "red")

# F-test for comparing variance
var.test(amd_core_speed, nvidia_core_speed, level = 0.95)
# two-sample t-test
t.test(amd_core_speed, nvidia_core_speed, alternative="two.sided", var.equal = FALSE, level = 0.95)
