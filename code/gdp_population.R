2+2

library(tidyverse)
#use readr, as opposed to base R, to load data in tidyverse
#different kinds of plots require different kinds of data -- we can see information about the data when it's first loaded under "column specification"

gapminder_1997 <- read_csv("gapminder_1997.csv")

name <- "Ben" #naming in R is case-sensitive, can't start with a number, no spaces, avoid dots
age <- 26
pi <- 3.1415
rounded_pi <- round(pi, digits = 1)
rounded_pi

sys.date() #this outputs the current date
getwd() #outputs the current working directory

#Plotting
ggplot(data=gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp, color = continent, size=pop/1000000) +
  labs(x="GDP per Capita", y="Life Expectancy", title = "Do people in wealthy countries live longer?", size="Population in millions") +
  geom_point() +
  scale_color_brewer(palette="spalette")

#Plotting for data exploration
gapminder_data <- read_csv("gapminder_data.csv")
ggplot(data=gapminder_data) +
  aes(x=year, y=lifeExp, color=continent) + 
  geom_point()

gapminder_data <- read_csv("gapminder_data.csv")
ggplot(data=gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group=country) + 
  geom_line()

#Box plot
gapminder_data <- read_csv("gapminder_data.csv")
ggplot(data=gapminder_data) +
  aes(x=continent, y=lifeExp, color=continent) + 
  geom_boxplot()

#Violin plot (using 1997 data)
gapminder_data <- read_csv("gapminder_data.csv")
ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp)) + #does aesthetics for the entire plot, aesthetic can be passed to single geoms
  geom_violin(aes(fill=continent)) + #positioning of jittered points is random
  geom_jitter(alpha=0.7) #ordering of functions matters, in this case the violin plot is lying over the points, plot is being built in layers
#can ask R for colors with colors(), can also sample colors with sample(colors(), size=X)

ggplot(data=gapminder_1997) +
  aes(x=lifeExp) +
  geom_density(bins=20) #bins in this sense sets the number of bins to fit all the data in, bin width, or something like that would be the number of items in a bin

#ggplot2 Themes
ggplot(data=gapminder_1997) +
  aes(x=lifeExp) +
  geom_histogram() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, hjust=1))

#Facet
ggplot(data=gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() + 
  facet_wrap(vars(continent))

ggplot(data=gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() + 
  facet_grid(rows = vars(continent))

#Saving
ggsave("awesomeplot.jpg", 
       width=6, 
       height=4)

violin_plot <- ggplot(data=gapminder_1997) +
  aes(x=continent, y=lifeExp) +
  geom_violin(aes(fill=continent))

violin_plot <- violin_plot + theme_bw()
ggsave(plot=violin_plot,
       filename="awesome_violin_plot.jpg",
       width=6,
       height=4) #inches is the default measure in ggplot

#Animated plots
install.packages(c("gganimate", "gifski"))
library(gganimate)
library(gifski)

ggplot(data=gapminder_data) +
  aes(x=log(gdpPercap), y=lifeExp, size=pop, color=continent) +
  geom_point()


staticHansPlot <- ggplot(data=gapminder_data) +
  aes(x=log(gdpPercap), y=lifeExp, size=pop/1000000, color=continent) +
  geom_point(alpha=0.5) +
  scale_color_brewer(palette="Set1") +
  labs(x="GDP Per Capita", y="Life Expectancy", color="Continent", size="Population in millions") +
  theme_classic()

staticHansPlot

animatedHansPlot <- staticHansPlot +
  transition_states(year, transition_length = 1, state_length = 1) +
  ggtitle("{closest_state}") #curly brackets is inserting the value of a variable into a text string in ggplot, but this is probably coming from other packages

animatedHansPlot

anim_save("HansAnimatedPlot.gif",
          plot=animatedHansPlot,
          renderer=gifski_renderer())

#Attitude

library(tidyverse)
library(ggplot2)

head(attitude)
data(attitude)

ggplot(data=attitude) +
  aes(x=privileges, y=rating) +
  geom_point(color="sienna1") +
  theme_dark() +
  labs(x="Privileges", y="Rating", title="How rating changes with privileges")
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, hjust=1))
  
data(iris)
view(iris)

ggplot(data=iris) +
  aes(x=Species, y=Petal.Length) +
  geom_boxplot(aes(color="pink")) +
  theme_classic() +
  labs(x="Species", y="Petal Length", title="Petal length by species")
