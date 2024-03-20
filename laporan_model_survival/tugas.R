#########################
# code for model survival
#########################

# Let 0 < 𝑇 < 20, 𝑛 big point time
t <- seq(0, 20, length=1000)

# syntax 'dexp' has mean = 1/rate
# setting mean from rate 0.5, 2, 4, 10 to 2, .5, .25, .1
ft <- dexp(t, rate = 2)
Rate <- c(.5, .25, .1)

# atur warna & label
colors <- c("red", "blue", "green")
labels <- c("mean = 0.5", "mean = 2","mean = 4","mean = 10")

#########################################################

#Plot fungsi kepadatan pdf distribusi exp dari waktu survival, T

#########################################################

plot(t, ft, type="l", lty=1, lwd=2,
     ylim = c(0,1),
     xlab="Waktu survival",
     ylab="Density", main="Fungsi kepadatan peluang")

for (i in 1:length(Rate)){
  
  lines(t, dexp(t,Rate[i]), lwd=2, col=colors[i])
  
}
legend("topright", inset=.05, title="",
       
       labels, lwd=2, lty=c(1, 1, 1), col=c("black",colors), bty="n")

#########################################################

#Plot fungsi survival exp dari T; S(t)=exp(-lambda.t)

#########################################################

St <- 1 - pexp(t, rate = 2)

plot(t, St, type="l", lty=1, lwd=2,
     
     xlab="Waktu survival, T",
     
     ylab="S(t)", main="Fungsi Survival")

fs.survival <- function(t,Rate){
  St <- 1 - pexp(t, Rate)
}

for (i in 1:length(Rate)){
  lines(t, fs.survival(t,Rate[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="",
       labels, lwd=2, lty=c(1, 1, 1), col = c("black",colors), bty="n")

#########################################################

# Membandingkan dengan empiris

#########################################################

# load library yang dibutuhkan
library(ggplot2)

# set seed for repoducibility
set.seed(123)

# Generate survival times from an exponential distribution with mean = 0.5
N <- 1000
data1 <- rexp(N, rate = 2) # rate = 1/mean

# Generate a histogram and density function for the data
hist_data1 <- ggplot(data.frame(time = data1), aes(x = time)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  stat_function(fun = dexp, args = list(rate = 2), color = "red", size = 1) +
  labs(title = "Data 1: Histogram and Density Function",
       x = "Survival Time",
       y = "Frequency")


hist_data1

# Generate survival times from an exponential distribution with mean = 0.5 again
data2 <- rexp(N, rate = 2)

# Generate a histogram and density function for the new data
hist_data2 <- ggplot(data.frame(time = data2), aes(x = time)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  stat_function(fun = dexp, args = list(rate = 2), color = "red", size = 1) +
  labs(title = "Data 2: Histogram and Density Function",
       x = "Survival Time",
       y = "Frequency")

hist_data2


# Compare the two graphs
# From the two graphs, we can see that the histograms and density functions are very similar,
# which suggests that the two sets of survival times are likely to have come from the same distribution.
# Specifically, they both appear to follow an exponential distribution with a mean of 0.5.



#########################################################

# Fungsi kepadatan pdf lambda 1 shape berubah ubah Weibull

#########################################################

t <- seq(0, 20, length=10000)

ft <- dweibull(t, shape = 0.5, scale = 1)

#Untuk 𝜆 = 1, plot fungsi kepadatan distribusi dari 𝑇 untuk:
#𝛼 = 0,5
#𝛼 = 2
#𝛼 = 4
#𝛼 = 10

dshape <- c(2, 4, 10)

colors <- c("red", "blue", "green")

labels <- c("shape= 0.5", "shape = 2","shape = 4","shape = 10")

#########################################################

#Plot fungsi kepadatan distribusi dari waktu survival, T

#########################################################
plot(t, ft, type="l", lty=1, lwd=2,
     xlim = c(0,5),
     ylim = c(0,1),
     xlab="Waktu survival",
     
     ylab="Density", main="PDF Weibull scale 1")

for (i in 1:length(dshape)){
  
  lines(t, dweibull(t,dshape[i], scale = 1), lwd=2, col=colors[i])
  
}
legend("topright", inset=.05, title="",
       
       labels, lwd=2, lty=c(1, 1, 1), col=c("black",colors), bty="n")

#########################################################

#Plot fungsi  survival dari T

#########################################################

St<- 1 - pweibull(t, shape = 0.5, scale = 1)

plot(t, St, type="l", lty=1, lwd=2,
     
     xlab="Waktu survival, T",
     
     ylab="S(t)", main="Fungsi Survival")

fs.survival <- function(t,dshape){
  St <- 1 - pweibull(t, dshape, scale = 1)
}

for (i in 1:length(dshape)){
  lines(t, fs.survival(t,dshape[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="",
       labels, lwd=2, lty=c(1, 1, 1), col = c("black",colors), bty="n")

#########################################################

#Plot fungsi Hazard dari T

#########################################################
ht<- dweibull(t, shape = 0.5, scale = 1)/(1 - pweibull(t, shape = 0.5, scale = 1))

plot(t, ht, type="l", lty=1, lwd=2,
     ylim = c(0,30),
     
     xlab="Waktu survival, T",
     
     ylab="h(t)", main="Fungsi Hazard")

fs.survival <- function(t,dshape){
  ht <- dweibull(t, dshape, scale = 1)/(1 - pweibull(t, dshape, scale = 1))
}

for (i in 1:length(dshape)){
  lines(t, fs.survival(t,dshape[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="",
       labels, lwd=2, lty=c(1, 1, 1), col = c("black",colors), bty="n")

#########################################################

# Fungsi kepadatan Weibull alpha 1 scale berubah ubah

#########################################################

t <- seq(0, 20, length=10000)

ft <- dweibull(t, scale = 2, shape =  1)

# Untuk 𝛼 = 1, plot fungsi kepadatan distribusi dari 𝑇 untuk 
# a. 𝜆 = 0,5 
# b. 𝜆 = 2 
# c. 𝜆 = 4 
# d. 𝜆 = 10 


dscale <- c(0.5, 0.25, 0.1)

colors <- c("red", "blue", "green")

labels <- c("scale = 0.5", "scale = 2","scale = 4","scale = 10")

#########################################################

#Plot fungsi kepadatan distribusi dari waktu survival, T

#########################################################

plot(t, ft, type="l", lty=1, lwd=2,
     ylim = c(0,1),
     xlab="Waktu survival",
     
     ylab="Density", main="PDF Weibull shape 1")

for (i in 1:length(dscale)){
  
  lines(t, dweibull(t,dscale[i],shape =1 ), lwd=2, col=colors[i])
  
}
legend("topright", inset=.05, title="",
       
       labels, lwd=2, lty=c(1, 1, 1), col=c("black",colors), bty="n")

#########################################################

#Plot fungsi  survival dari T

#########################################################

St<- 1 - pweibull(t, shape = 0.5, scale = 1)

plot(t, St, type="l", lty=1, lwd=2,
     #ylim = c(0,2),
     
     xlab="Waktu survival, T",
     
     ylab="S(t)", main="Fungsi Survival")

fs.survival <- function(t,dscale){
  St <- 1 - pweibull(t, dscale, shape = 1)
}

for (i in 1:length(dscale)){
  lines(t, fs.survival(t,dscale[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="",
       labels, lwd=2, lty=c(1, 1, 1), col = c("black",colors), bty="n")

#########################################################

#Plot fungsi Hazard dari T

#########################################################

ht<- dweibull(t, shape = 0.5, scale = 1)/(1 - pweibull(t, shape = 0.5, scale = 1))

plot(t, ht, type="l", lty=1, lwd=2,
     ylim = c(0,30),
     
     xlab="Waktu survival, T",
     
     ylab="h(t)", main="Fungsi Hazard")

fs.survival <- function(t,dscale){
  ht <- dweibull(t, dscale, shape = 1)/(1 - pweibull(t, dscale, shape = 1))
}

for (i in 1:length(dscale)){
  lines(t, fs.survival(t,dscale[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="",
       labels, lwd=2, lty=c(1, 1, 1), col = c("black",colors), bty="n")


rm(list = ls(all.names = TRUE))
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)

df <- read.csv("https://raw.githubusercontent.com/zakizulham/belajar/main/cancer%20data%20M%20Pagano%20and%20K%20Gauvreau%20-%20Sheet1.csv")


# Perintahnya suruh buat fungsi survival dari dengan metode Kaplan Meier dan tampilkan plot nya

# Sebelumnya 
head(df)
colnames(df)

# unique(df$time)

# df <- df[order(df$time),]
# unique(df$time)

km <- with(df, Surv(time, censor))

head(km, 80)

km_fit <- survfit(Surv(time, censor) ~ 1, data=df)

plot(km_fit)

summary(km_fit)

# str(summary(km_fit))

# cols <- lapply(c(2:7, 14:15) , function(x) res[x])
# Combine the columns into a data frame
# tbl <- do.call(data.frame, cols)
# str(tbl)
# tbl

# save.df <- as.data.frame(tbl)

# nomor 1
# write.csv(save.df, file = "./file.csv", row.names = FALSE)

autoplot(km_fit)

# Nomor 2
km_trt_fit <- survfit(Surv(time, censor) ~ group, data=df)
autoplot(km_trt_fit)

# Nomor 3
km_trt_fit_1 <- survfit(Surv(time, censor) ~ number, data=df)

autoplot(km_trt_fit_1)

# Nomor 4
# Buat hanya Group 1

head(df, 8)
dfGroup1 <- subset(x = df, subset = group == 1)

km_fit <- survfit(Surv(time, censor) ~ 1, data=dfGroup1)

plot(km_fit)
autoplot(km_fit)

km_trt_fit_1 <- survfit(Surv(time, censor) ~ number, data=dfGroup1)
autoplot(km_trt_fit_1)

# Buat hanya Group 2
names(df)
dfGroup2 <- subset(x = df, subset = group == 2)

km_fit <- survfit(Surv(time, censor) ~ 1, data=dfGroup2)

plot(km_fit)
autoplot(km_fit)

km_trt_fit_1 <- survfit(Surv(time, censor) ~ number, data=dfGroup2)
autoplot(km_trt_fit_1)


# Nomor 6
head(df)

km_fit <-survfit(Surv(time, censor) ~ group ,data = df)
km_fit

summary(km_fit)
summary(km_fit)$table

k <- length(km_fit$surv)
m = l <- km_fit$surv

m
m[1] <- log(1/l[1])
for (i in 2:k) {
  m[i] <- log(l[i-1]) - log(l[i])
}
m

plot(km_fit$time[1:27], m[1:27],
     main= 'Plot Hazard berdasarkan Grup',
     xlab = 'Time',
     ylab = 'Hazard Rate',
     col = "#E7B800",
     lwd = 3,
     type="l")

lines(km_fit$time[28:51],m[28:51], lwd=3, col= "#2E9FDF")

legend("topright", 
       inset=.05, 
       title="",
       c("Grup 1", "Grup 2"),
       lwd=2,
       lty=1, 
       col=c("#E7B800", "#2E9FDF"),
       bty="n")

# Dipisah berdasarkan number
km_fit <-survfit(Surv(time, censor) ~ number ,data = df)

km_fit
summary(km_fit)
summary(km_fit)$table

k <- length(km_fit$surv)
m = l <- km_fit$surv
m
m[1] <- log(1/l[1])
for (i in 2:k) {
  m[i] <- log(l[i-1]) - log(l[i])
}
m
plot(km_fit$time[1:27], m[1:27],
     main= 'Plot Hazard berdasarkan Number',
     xlab = 'Time',
     ylab = 'Hazard Rate',
     col = "#E7B800",
     lwd = 3,
     type="l")

lines(km_fit$time[28:51],m[28:51], lwd=3, col= "#2E9FDF")

legend("topright", 
       #inset=.05, 
       title="",
       c("Number 1", "Number 2"),
       lwd=2,
       lty=1, 
       col=c("#E7B800", "#2E9FDF"),
       bty="n")
