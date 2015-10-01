mu<-2
si<-0.6
bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si)) 
uniform<-runif(2000,-4,4)
normal<-rnorm(2000,0,3)

pdf("inst/figures/comparison.pdf", width = 10)
par(mfrow = c(1,3))

labels <- c("bimodal", "uniform", "normal")

boxplot(bimodal,uniform,normal, names = c("", "", ""), main = "Box and Whisker")
text(x = 1:length(labels), y = par("usr")[3] - 1, srt = 45, adj = 1,
     labels = c("bimodal", "uniform", "normal"), xpd = TRUE)

vioplot(bimodal,uniform,normal, names = c("", "", ""))
title("Violin")
text(x = 1:length(labels), y = par("usr")[3] - 1, srt = 45, adj = 1,
     labels = c("bimodal", "uniform", "normal"), xpd = TRUE)

sina <- sinaplot(c(bimodal, uniform, normal), c(rep("bimodal", 2000), rep("uniform", 2000),
                                        rep("normal", 2000)), bw = TRUE, scale = F, plot = F)
plot(sina[[1]], xlim = c(0,4), xaxt = "n", xlab = "", ylab = "", main = "Sina", pch = 20)
axis(1, at = 1:3, labels = FALSE)
text(x = 1:length(labels), y = par("usr")[3] - 1, srt = 45, adj = 1,
     labels = c("bimodal", "uniform", "normal"), xpd = TRUE)
dev.off()

library(beeswarm)
sinaplot(df[,2], df[,1], ytitle = "log2 expression")


p1 <- sinaplot(df[,2], df[,1], size = 1.5) + ggtitle("Sina") + theme_bw()
p1 <- p1 + geom_point(colour="black") + theme(axis.text.x = element_blank()) + ylab("log2 expression")

p2 <- ggplot(aes(V1, V2),data = df) + geom_violin(fill = "grey80") + theme_bw()
p2 <- p2 + theme(axis.text.x=element_blank()) + guides(fill=FALSE) 
p2 <- p2 + ylab("") + xlab("") + ggtitle("Violin")

p3 <- ggplot(aes(V1, V2),data = df) + geom_boxplot(fill = "grey80") + theme_bw()
p3 <- p3 + theme(axis.text.x=element_blank()) + guides(fill=FALSE)
p3 <- p3 + ylab("log2 expression") + xlab("") + ggtitle("Box and Whisker")

beeswarm2 <- beeswarm(df[,2] ~ df[,1])
p4 <- ggplot(beeswarm2, aes(x, y)) + geom_point() + theme_bw()
#p4 <- ggplot(beeswarm2, aes(x, y)) + geom_point(aes(colour = df$V1)) + scale_x_discrete(limits = labels)
p4 <- p4 + theme(axis.text.x = element_blank()) + guides(colour=FALSE) 
p4 <- p4 + ylab("") + xlab("") + ggtitle("Bee Swarm") + xlim(c(0.5,5.5))

p5 <- ggplot(df, aes(V1, V2)) + geom_jitter() + theme_bw()
p5 <- p5 + theme(axis.text.x=element_text()) + guides(fill=FALSE) + scale_x_discrete(limits = labels)
p5 <- p5 + ylab("log2 expression") + xlab("") + ggtitle("Jitter")

p6 <- ggplot(df, aes(V1, V2)) + geom_point() + theme_bw()
p6 <- p6 + theme(axis.text.x=element_text()) + guides(fill=FALSE) + scale_x_discrete(limits = labels)
p6 <- p6 + ylab("") + xlab("") + ggtitle("Stripchart")


pdf("inst/figures/comparison.pdf", width = 10)
png("inst/figures/comparison.png", width = 650, height = 800, res = 72)
#grid.arrange(p1, p2, p4, p3, ncol = 2)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
dev.off()
df[,1] <- factor(df[,1], levels = unique(df[,1]))

######################################
library(sinaplot)
data(blood)

a <- data$value[data$type == "CLL"]
dens <- density(a)

yFraction <- 0.04

window_size <- (max(a) - min(a)) * yFraction

yBins <- c()
for (i in 0:ceiling(1/yFraction)) {
    yBins <- c(yBins, ymin + i * window_size)
}

par(mfrow=c(1,3))
plot(rep(1, length(a)), a, xlim = c(0,2), ylim = c(4,10), ylab = "", xlab = "",
     xaxt = "n", yaxt = "n", bty="n", pch = 20)
points(dens$y + 1, dens$x, type="l")
points(-dens$y + 1, dens$x, type="l")

abline(h = yBins, lty = 2, lwd = 0.5)
axis(1, at = seq(0, 2, 0.5), labels = FALSE)
axis(2, at = c(4,6,8,10), labels = FALSE)

b <- rep(1, length(a))
p <- findInterval(a, yBins[10:11]) == 1

cur_bin <- yBins[10:11]
xMax <-mean(dens$y[findInterval(dens$x, cur_bin)==1])
xTranslate <- runif(sum(p), -xMax, xMax )
b[p] <- b[p] + xTranslate

plot(b, a, xlim = c(0,2), ylim = c(4,10), ylab = "", xlab = "",
     xaxt = "n", yaxt = "n", bty="n", pch = 20)
points(dens$y + 1, dens$x, type="l")
points(-dens$y + 1, dens$x, type="l")

abline(h = yBins, lty = 2, lwd = 0.5)
axis(1, at = seq(0, 2, 0.5), labels = FALSE)
axis(2, at = c(4,6,8,10), labels = FALSE)

c <- sinaplot(a, rep("CLL", length(a)), plot = FALSE)
plot(c[[1]]$x, c[[1]]$y, xlim = c(0,2), ylim = c(4,10), ylab = "", xlab = "",
     xaxt = "n", yaxt = "n", bty="n", pch = 20)
points(dens$y + 1, dens$x, type="l")
points(-dens$y + 1, dens$x, type="l")

abline(h = yBins, lty = 2, lwd = 0.5)
axis(1, at = seq(0, 2, 0.5), labels = FALSE)
axis(2, at = c(4,6,8,10), labels = FALSE)
