library(ggplot2)
library(ggthemes)
library(dplyr)
library(patchwork)

##Creat the histgram of all columns in the dataset and combine them into one figure
data <- read_csv("Three_datasets.csv")
plot1 <- data |>
  ggplot(aes(x = V1))+
  geom_histogram(fill = "red", colour = "black", bins = 10)+
  labs(title = "Dataset 1",
       x = "",
       y = "Frequency")+
  theme_classic()

plot2 <- data |>
  ggplot(aes(x = V2))+
  geom_histogram(fill = "blue", colour = "black", bins = 10)+
  labs(title = "Dataset 2",
       x = "",
       y = "Frequency")+
  theme_classic()

plot3 <- data |>
  ggplot(aes(x = V3))+
  geom_histogram(fill = "green", colour = "black", bins = 10)+
  labs(title = "Dataset 3",
       x = "",
       y = "Frequency")+
  theme_classic()

plot1 + plot2 + plot3

##Using ggplot2 to creat the boxplot of all columns in the same dataset
#Rowbind the last two columes to  the first column
data_trans <- data.frame(x = 1 : (length(data$V1) + 
                                    length(data$V2) + 
                                    length(data$V3)), 
                         y = 1 : 2)

data_trans$x[(1 : length(data$V1))] <- data$V1

data_trans$x[(length(data$V1) + 1) : (length(data$V1) + 
                                        length(data$V2))] <- data$V2

data_trans$x[(length(data$V1) + 
                length(data$V2) + 1) : 
               (length(data$V1) + 
                  length(data$V2) + 
                  length(data$V3))] <- data$V3

#Assign each number to the name of its original column
data_trans$y[1 : length(data$V1)] <- "V1"

data_trans$y[(length(data$V1) + 1) : (length(data$V1) + 
                                        length(data$V2))] <- "V2"

data_trans$y[(length(data$V1) + 
                length(data$V2) + 1) : 
               (length(data$V1) + 
                  length(data$V2) + 
                  length(data$V3)) ] <- "V3"

#Draw the plot
data_trans |>
  ggplot(aes(x = y, y = x, group = y))+
  geom_boxplot(aes(fill = y))+
  labs(title = "A boxplot",
       x = "",
       y = "",
       fill = "Group")+
  theme_classic()+
  scale_color_colorblind()


##Using basic R to draw the plot
boxplot(data, names = c(1, 2, 3), col = c("red", "blue", "green"))
title(main = "A box plot")



##Import the new dataset, calculate the mean and sd for each column and visualise
data2 <- read_csv("Datasaurus_data.csv")
head(data2)


apply(data2, 2, mean)
apply(data2, 2, sd)
boxplot(data2)

par(mfrow = c(2,1))
v1hist <- hist(data2$V1, main = "Histogram of data2[,1]", xlab = "", ylab = "")
v2hist <- hist(data2$V2, main = "Histogram of data2[,2]", xlab = "", ylab = "")

plotd <- plot(data2$V1, data2$V2, col = rainbow(nrow(data2)), xlab = "", ylab = "")

##Home Exercise
#Calculate the Mean Absolute Deviation for each set of data
#Three_datasets.csv
sum(abs(data$V1 - mean(data$V1)))/length(data$V1)
sum(abs(data$V2 - mean(data$V2)))/length(data$V2)
sum(abs(data$V1 - mean(data$V3)))/length(data$V3)

#Datasaurus_data.csv
sum(abs(data2$V1 - mean(data2$V1)))/length(data2$V1)
sum(abs(data$V2 - mean(data$V2)))/length(data2$V2)
