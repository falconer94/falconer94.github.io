getwd()
Snails <- read.delim ("../Data_Course/Data/ITS_mapping.csv")

png(filename = "./Assignments/Assignment_4/silly_boxplot.png")
plot(x = Snails$Ecosystem, y = Snails$Lat)

dev.off()

