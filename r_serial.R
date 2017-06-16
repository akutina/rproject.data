y <- read.csv("~/Desktop/2verbCUTimp.csv", header = TRUE)
y

library(ggplot2)
library(tidyverse)
library(party)
library(lattice)

#1.correlation between punctuation and mood
y <- read.csv("~/Desktop/2cutmp.csv", header = TRUE)

a <- ggplot(data = y, aes(x = V2, fill = IMP)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and mood") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
a

#2.correlation between punctuation and semantic type
y <- read.csv("~/Desktop/2cutmp.csv", header = TRUE)

b <- ggplot(data = y, aes(x = V2, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
b

#3.correlation between punctuation and construction
c <- ggplot(data = y, aes(x = V2, fill = construction)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and construction") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

c + theme_classic()

#4.correlation between punctuation and different features (GLM)
fit <- glm(V2~IMP, data = y, family = "binomial")
summary(fit)

fit <- glm(V2~EL, data = y, family = "binomial")
summary(fit)

fit <- glm(V2~type, data = y, family = "binomial")
summary(fit)

fit <- glm(V2~type + IMP, data = y, family = "binomial")
summary(fit)

#5.correlation between punctuation and type+IMP (ctree) 

y.ctree=ctree(V2~type+IMP, y)
plot(y.ctree) 

#6.correlation between punctuation and EL
y <- read.csv("~/Desktop/2verbCUTel.csv", header = TRUE)
p <- ggplot(data = y, aes(x = V2, fill = EL)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and construction") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

p

#7.RNC
y <- read.csv("~/Desktop/nrc.2verbconstr.csv", header = TRUE)
d <- ggplot(data = y, aes(x = V2, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

d

#8.triplets
y <- read.csv("~/Desktop/tripletsnotCUT.csv", header = TRUE)
c <- ggplot(data = y, aes(x = V2, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V2") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

c

#9.triplets
y <- read.csv("~/Desktop/tripletsnotCUT.csv", header = TRUE)
c <- ggplot(data = y, aes(x = V3, fill = type)) +
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Correlation between punctuation and type") +
  xlab("comma before V3") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

c





