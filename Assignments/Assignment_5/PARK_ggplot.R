library(tidyverse)
?ggplot()
?aes()

df <- iris

# iris_fig1.png
png("iris_fig1.png")
ggplot(df,aes(x=Sepal.Length,y=Petal.Length,color=Species))+
  geom_smooth(method="lm")+
  geom_point(alpha=1)+
  labs(title="Sepal length vs petal length",subtitle = "for three iris species")+
  theme_minimal()

dev.off()

# iris_fig2.png
png("iris_fig2.png")
ggplot(df,aes(x=Petal.Width,fill=Species))+
  geom_density(alpha=.5)+
  labs(title="Distribution of Petal Widths",subtitle = "for three iris species",
       x="Petal Width")+
  theme_minimal()
dev.off()


# iris_fig3.png

png("iris_fig3.png")
ggplot(df,aes(x=Species,y=Petal.Width/Sepal.Width,fill=Species))+
  geom_boxplot()+
  labs(title="Sepal- to Petal-Width Ratio",subtitle = "for three iris species",
       x="Species",y="Ratio of Sepal Width to Petal Width")+
  theme_minimal()
dev.off()



# iris_fig4.png
# x = Deviance = Sepal.Length - mean(Sepal.Length)
png("iris_fig4.png")
ggplot(df,aes(x=Sepal.Length,
              y=Sepal.Length - mean(Sepal.Length),
              fill=Species,))+
  geom_col(position=position_dodge())+
  labs(title= "Sepal length deviance from the mean of all observations",
       x=NULL,y="Deviance from the Mean") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank(),
        axis.text.y = element_blank())+
  coord_flip()
dev.off()
