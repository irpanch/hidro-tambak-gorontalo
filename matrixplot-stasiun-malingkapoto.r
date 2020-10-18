# set working directory : ctrl+shift+h

# remove list history
rm(list=ls())

# load library
library(ggplot2)
library(dplyr)
library(extremeStat)
library(lubridate)
library(hydroTSM)

# nama pos
nama_pos <- "Stasiun Hujan Malingkapoto"

# 1. import data
data <- read.csv("data-hujan-sta1.csv")
data <- data[,1:2]

# 2. Ganti nama kolom
names(data) <- c("Tanggal","Curah_Hujan")

# 3. Pilih hanya yang mempunyai data.
data <- subset(data,data[,2] != "-")

# 4. Rubah menjadi format yang benar (date dan numeric)
data$Tanggal <- as.Date(data[,1],format="%d-%b-%y") #cek format tanggal dari data original.
data[,2] <- as.numeric(as.character(data[,2])) #JANGAN LUPA as.character.

# 5. Cek struktur data. Apa sudah benar?
View(data)
str(data)

# 6. create a summary.
summary(data)

# Hilangkan baris NA

data <- data[complete.cases(data),]

# 7. buat matrixplot
zoo_rr_data <- zoo(data[,2],data$Tanggal)
sapply(zoo_rr_data, class)

smry(zoo_rr_data)

total_year_observation <- yip("2008-01-01", "2019-12-31", out.type = "nmbr") # tanggal mulai dan akhir lihat di summary
total_year_observation

## ini tambahan, cari rata2 hujan harian per tahun
rata2pertahun <- daily2annual(zoo_rr_data, FUN=mean, na.rm=T)
rata2pertahun


# extract total monthly precipitation
jumlah_bulanan <- daily2monthly(zoo_rr_data, FUN=sum, na.rm = T)
rata2_bulanan <- daily2monthly(zoo_rr_data, FUN=mean, na.rm = T)

# buat matrix hujan bulanan

mat_jumlah_bulanan <- matrix(jumlah_bulanan, ncol=12, byrow = T)
mat_rata2_bulanan <- matrix(rata2_bulanan, ncol=12, byrow = T)

# rename the matrix column as the 12 months
# and the matrix row as a unique time of data

colnames(mat_jumlah_bulanan) <- month.abb
rownames(mat_jumlah_bulanan) <- unique(format(time(jumlah_bulanan), "%Y"))

colnames(mat_rata2_bulanan) <- month.abb
rownames(mat_rata2_bulanan) <- unique(format(time(rata2_bulanan), "%Y"))

# load "lattice" and plotting matrixplot

require(lattice)

# matrixplot jumlah bulanan
print(matrixplot(mat_jumlah_bulanan, ColorRamp = "Precipitation",main=c("Jumlah Curah Hujan Bulanan (mm)")))

# matrixplot rata2 bulanan
print(matrixplot(mat_rata2_bulanan, ColorRamp = "Precipitation",main=c("Rata-rata Curah Hujan Bulanan (mm)")))


# tandai data maksimum dan minimum
index_max <- data$Curah_Hujan == max(data$Curah_Hujan)
max_plot <- ggplot(data,aes(x=Tanggal, y=Curah_Hujan),label="Curah Hujan (mm)")+geom_line(color="blue3") + 
  labs(title="Data Curah Hujan (mm)",subtitle = nama_pos,x = "Waktu",y="Curah Hujan (mm)") +
  geom_point(data=data[index_max,],aes(Tanggal,Curah_Hujan,color="MAX"))+
  geom_text(aes(label=ifelse(Curah_Hujan==max(data$Curah_Hujan),as.character(Curah_Hujan),'')),hjust=-0.5,vjust=0.5)

max_plot+theme_update(plot.title=element_text(hjust=0.5))+
  theme_update(plot.subtitle=element_text(hjust=0.5))+
  theme_update(axis.title.y=element_text(angle=90))

# cari tanggal berapa dia maksimum dan minimum?
data[index_max,]
