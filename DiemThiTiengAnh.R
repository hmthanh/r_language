library(ggplot2)
diemThiTiengAnh <- read.csv(file="Engish.csv", encoding="UTF-8", stringsAsFactors=FALSE, header=TRUE, sep=",")
View(diemThiTiengAnh)

mean(diemThiTiengAnh$Diem, na.rm = TRUE)
sd(diemThiTiengAnh$Diem, na.rm = TRUE)
mean(diemThiTiengAnh$DiemCong, na.rm = TRUE)
mean(diemThiTiengAnh$TongDiem, na.rm = TRUE)

# Do lech chuan
(sd(diemThiTiengAnh$Diem, na.rm = TRUE))
# Phuong sai
(var(diemThiTiengAnh$Diem, na.rm = TRUE))
length(diemThiTiengAnh$MSSV)
# Biểu đồ
ggplot(data=diemThiTiengAnh, aes(x=Diem, y=Tuoi, na.rm = TRUE),na.rm = TRUE)+
  scale_x_continuous(breaks=seq(0, 10, 1))+
  geom_point(aes(col=GioiTinh),na.rm = TRUE)

ggplot(data=diemThiTiengAnh, aes(x=Diem, y=DiemCong, na.rm = TRUE),na.rm = TRUE)+
  scale_x_continuous(breaks=seq(0, 10, 1))+
  geom_point(aes(col=GioiTinh),na.rm = TRUE)
####################################################################
# KIỂM ĐỊNH TỶ LỆ
# Tính khoảng tin cậy :
soBanDau <- sum(diemThiTiengAnh$Diem >= 5, na.rm=TRUE)
n <- length(diemThiTiengAnh$Diem)
(anpha <- 1 - 0.95)
(p_hat <- soBanDau/n)
(z <- qnorm(1 - anpha/2))
(se <- sqrt(p_hat*(1-p_hat)/n))
(khoangtincay <- p_hat + c(-z*se, z*se))
# Vậy tỷ lệ các bạn đậu nằm trong khoảng 0.5351997 0.7419087

prop.test(soBanDau, n, conf.level = 1-anpha)


# Kiểm định thống kê: "Khoảng 60% lớp mình sẽ đậu Tiếng Anh"
# Gọi p là tỷ lệ đậu Tiếng Anh cuối kỳ của các bạn
# Ta kiểm định :
#   H0 : p = 0.6
#   H1 : p > 0.6 với anpha = 5%
soBanDau <- sum(diemThiTiengAnh$Diem >= 5, na.rm=TRUE)
n <- length(diemThiTiengAnh$Diem)
(p_hat <- soBanDau/n)
(p0 <- 0.6)
anpha <- 0.05
(se <- sqrt(p0*(1 - p0)/n))
(z <- (p_hat - p0)/se)
(p_value <- 1 - pnorm(z))
(crit_val <- qnorm(1 - anpha)) # Gía trị giới hạn
(p_value < anpha)
(crit_val < z)

prop.test(soBanDau, n, p = 0.6, conf.level = 1-anpha, alternative = "greater")

####################################################################
#KIỂM ĐỊNH TRUNG BÌNH
# Khoảng tin cậy
data <- diemThiTiengAnh$Diem
n <- length(diemThiTiengAnh$Diem)
(x_bar <- mean(data, na.rm=TRUE))
(anpha <- 1 - 0.95)
(se <- sd(data, na.rm = TRUE)/sqrt(n))
(z <- qnorm(1 - anpha/2))
(zconf_int <- x_bar + c(-z*se, z*se))

(t <- qt(1 - anpha/2, df = n - 1)) # Khi n nhỏ hơn 30 thì phân phối student
(tconf_int <- x_bar + c(-t*se, t*se))

t.test(data, conf.level = 1-anpha)

t.test(data, conf.level = 0.1)
# Thống kê kiểm định

data <- diemThiTiengAnh$Diem
n <- length(diemThiTiengAnh$Diem)
mu0 <- 6.0
anpha <- 0.05
(x_bar <- mean(data, na.rm = TRUE))
(se <- sd(data, na.rm = TRUE)/sqrt(n))
(t <- (x_bar-mu0)/se)
(p_value <- 2*(1 - pt(t, df = n-1)))
(crit_val <- qt(1-anpha, df = n-1))
p_value > anpha
crit_val > t