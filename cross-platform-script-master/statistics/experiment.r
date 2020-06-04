#library(lattice)
#library(PMCMR)

precision <- head(read.csv('../output/precision.csv', sep=';', header=T), 10)[,2:10]
recall <- head(read.csv('../output/recall.csv', sep=';', header=T), 10)[,2:10]
fscore <- head(read.csv('../output/fscore.csv', sep=';', header=T), 10)[,2:10]

print('Precision')
png('precision.png', height=1000, width=1000, units='px')
par(mfrow=c(3, 3))
plot(density(precision[, 'c5.target']), main='Target C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.b10.target']), main='Target C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.b20.target']), main='Target C5-B20', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.crosscheck']), main='CrossCheck C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.b10.crosscheck']), main='CrossCheck C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.b20.crosscheck']), main='CrossCheck C5-B20', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.browserbite']), main='Browserbite C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.b10.browserbite']), main='Browserbite C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(precision[, 'c5.b20.browserbite']), main='Browserbite C5-B20', ylim=c(0,3), xlim=c(0, 1))
dev.off()
#s_t <- shapiro.test(precision[,'c5.target'])
#print(s_t)
#s_c <- shapiro.test(precision[,'c5.crosscheck'])
#print(s_c)
#s_c <- shapiro.test(precision[,'c5.browserbite'])
#print(s_c)
#s_t <- shapiro.test(precision[,'c5.b10.target'])
#print(s_t)
#s_c <- shapiro.test(precision[,'c5.b10.crosscheck'])
#print(s_c)
#s_c <- shapiro.test(precision[,'c5.b10.browserbite'])
#print(s_c)
#s_t <- shapiro.test(precision[,'c5.b20.target'])
#print(s_t)
#s_c <- shapiro.test(precision[,'c5.b20.crosscheck'])
#print(s_c)
#s_c <- shapiro.test(precision[,'c5.b20.browserbite'])
#print(s_c)

print(' --- Anova on precision --- ')
precision <- c(precision[, 'c5.target'], precision[, 'c5.crosscheck'], precision[, 'c5.browserbite'],
               precision[, 'c5.b10.target'], precision[, 'c5.b10.crosscheck'], precision[, 'c5.b10.browserbite'],
               precision[, 'c5.b20.target'], precision[, 'c5.b20.crosscheck'], precision[, 'c5.b20.browserbite'])
classifiers <- c('c5.target', 'c5.crosscheck', 'c5.browserbite',
                 'c5.b10.target', 'c5.b10.crosscheck', 'c5.b10.browserbite',
                 'c5.b20.target', 'c5.b20.crosscheck', 'c5.b20.browserbite')
classifiers <- rep(classifiers, each=10)
data <- data.frame(precision, classifiers)
anova <- aov(data$precision ~ data$classifiers)
print(anova)
summary(anova)


print(' --- recall --- ')
png('recall.png', height=1000, width=1000, units='px')
par(mfrow=c(3, 3))
plot(density(recall[, 'c5.target']), main='Target C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.b10.target']), main='Target C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.b20.target']), main='Target C5-B20', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.crosscheck']), main='CrossCheck C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.b10.crosscheck']), main='CrossCheck C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.b20.crosscheck']), main='CrossCheck C5-B20', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.browserbite']), main='Browserbite C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.b10.browserbite']), main='Browserbite C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(recall[, 'c5.b20.browserbite']), main='Browserbite C5-B20', ylim=c(0,3), xlim=c(0, 1))
dev.off()
#s_t <- shapiro.test(recall[,'c5.target'])
#print(s_t)
#s_c <- shapiro.test(recall[,'c5.crosscheck'])
#print(s_c)
#s_c <- shapiro.test(recall[,'c5.browserbite'])
#print(s_c)

print(' --- Anova on recall --- ')
recall <- c(recall[, 'c5.target'], recall[, 'c5.crosscheck'], recall[, 'c5.browserbite'],
            recall[, 'c5.b10.target'], recall[, 'c5.b10.crosscheck'], recall[, 'c5.b10.browserbite'],
            recall[, 'c5.b20.target'], recall[, 'c5.b20.crosscheck'], recall[, 'c5.b20.browserbite'])
classifiers <- c('c5.target', 'c5.crosscheck', 'c5.browserbite',
                 'c5.b10.target', 'c5.b10.crosscheck', 'c5.b10.browserbite',
                 'c5.b20.target', 'c5.b20.crosscheck', 'c5.b20.browserbite')
classifiers <- rep(classifiers, each=10)
data <- data.frame(recall, classifiers)
anova <- aov(data$recall ~ data$classifiers)
print(anova)
summary(anova)

print(' --- fscore --- ')
png('fscore.png', height=1000, width=1000, units='px')
par(mfrow=c(3, 3))
plot(density(fscore[, 'c5.target']), main='Target C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.b10.target']), main='Target C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.b20.target']), main='Target C5-B20', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.crosscheck']), main='CrossCheck C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.b10.crosscheck']), main='CrossCheck C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.b20.crosscheck']), main='CrossCheck C5-B20', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.browserbite']), main='Browserbite C5', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.b10.browserbite']), main='Browserbite C5-B10', ylim=c(0,3), xlim=c(0, 1))
plot(density(fscore[, 'c5.b20.browserbite']), main='Browserbite C5-B20', ylim=c(0,3), xlim=c(0, 1))
dev.off()
#s_t <- shapiro.test(fscore[,'c5.target'])
#print(s_t)
#s_c <- shapiro.test(fscore[,'c5.crosscheck'])
#print(s_c)
#s_c <- shapiro.test(fscore[,'c5.browserbite'])
#print(s_c)

print(' --- Anova on fscore --- ')
fscore <- c(fscore[, 'c5.target'], fscore[, 'c5.crosscheck'], fscore[, 'c5.browserbite'],
            fscore[, 'c5.b10.target'], fscore[, 'c5.b10.crosscheck'], fscore[, 'c5.b10.browserbite'],
            fscore[, 'c5.b20.target'], fscore[, 'c5.b20.crosscheck'], fscore[, 'c5.b20.browserbite'])
classifiers <- c('c5.target', 'c5.crosscheck', 'c5.browserbite',
                 'c5.b10.target', 'c5.b10.crosscheck', 'c5.b10.browserbite',
                 'c5.b20.target', 'c5.b20.crosscheck', 'c5.b20.browserbite')
classifiers <- rep(classifiers, each=10)
data <- data.frame(fscore, classifiers)
anova <- aov(data$fscore ~ data$classifiers)
print(anova)
summary(anova)
