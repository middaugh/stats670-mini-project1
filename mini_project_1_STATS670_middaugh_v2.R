#Esmé Middaugh
#STATS 670
#mini-project 1 

#Packages to Install
library(ggplot2)
library(tidyverse)
library(broom)
library(dplyr)
install.packages("ggthemes") # Install 
library(ggthemes) # Load
install.packages("viridis")
library(viridis)

install.packages("gapminder")
??gapminder

#Function Area 
#box_cox function copied from Prof. Fukuyama's slides 
box_cox = function(y, tau) {
  return((y^tau - 1) / tau)
}


#Getting our main data set that we are interested in 
gapminder_data = gapminder::gapminder
View(gapminder_data)


#GDP and Life expectancy in 2007 
gapminder_2007 = gapminder_data %>%
  subset(year==2007)

lm_2007 = lm(lifeExp ~ gdpPercap, data=gapminder_2007)
tidy(lm_2007)

#Figure 1: 2007 GDP vs. Life Expectancy 
ggplot(data = gapminder_2007, aes(x=gdpPercap, y=lifeExp, color=continent)) + 
  geom_point() + 
  xlab('GDP per Capita') + 
  ylab('Average Life Expectancy') + 
  ggtitle('2007: Life Expectancy vs GDP per Capita') + 
  scale_color_viridis_d() +  #The _d means discrete + 
  theme_hc() 


#Clearly this distribution does not follow a simple line. 
#The distribution looks to be hollow down, so tried box-cox transformations up the ladder, bigger tau
#A little better, but interpretability isn't great (box_cox of 4) and still isn't much of a fit to a linear model
#Try instead to look for a loess model that will work:

#Figure 2 LOESS 2007: Life Expectancy vs GDP per Capita
ggplot(data = gapminder_2007, aes(x=gdpPercap, y=lifeExp)) + 
  theme_minimal() + 
  geom_point() + 
  geom_smooth(method = "loess", span = .70, method.args = list(degree = 2), se = FALSE) + 
  xlab('GDP per Capita') + 
  ylab('Average Life Expectancy') + 
  ggtitle('LOESS 2007: Life Expectancy vs GDP per Capita') +
  theme_minimal()

loess.gdp_2007= loess(lifeExp ~ gdpPercap, data = gapminder_2007, span = .4, degree = 1)
#Looks like it fits reasonably well, so lets check residuals 

#Appendix, Figure 10 
ggplot(augment(loess.gdp_2007, data = gapminder_2007, )) +
  geom_point(aes(x = gdpPercap, y = .resid, color=continent)) + 
  ggtitle('Residuals for LOESS Model (span = .7, degree=2) for 2007')+ 
  ylab('Residuals') + 
  xlab('GDP per Capita') + 
  theme_hc() + 
  scale_color_viridis_d() #The _d means discrete

#Clearly not a good fit, but perhaps because of the different continents involed -- Africa seems to have a lot of structure
#However, after about 15,000 it starts to work ---- perhaps because of different continents?
#Try to break out by facet wrap 

#Oceania only has two data points, so let's drop 
gapminder_2007_no_oceania = gapminder_2007 %>% subset(continent != 'Oceania')

ggplot(data = gapminder_2007_no_oceania, aes(x=gdpPercap, y=lifeExp, color=continent)) + 
  geom_point() + 
  theme_hc() + 
  xlab('GDP per Capita') + 
  ylab('Average Life Expectancy') + 
  ggtitle('2007: Life Expectancy vs GDP per Capita, broken by Continent') +
  facet_wrap(~continent ) + 
  scale_color_viridis_d() #The _d means discrete 

#Figure 3
#Now model with loess and facetwrap 
ggplot(data = gapminder_2007_no_oceania, aes(x=gdpPercap, y=lifeExp)) + 
  theme_minimal() + 
  geom_point() + 
  geom_smooth(method = "loess", span = .4, method.args = list(degree = 1), se = FALSE, color='lightgreen') + 
  facet_wrap(~continent) + 
  xlab('GDP per Capita') + 
  ylab('Average Life Expectancy') + 
  ggtitle('2007: Life Expectancy vs GDP per Capita with LOESS Line')

# Looks like its working for eveyone but Africa; try the main graph again without Africa
no_africa = gapminder_2007 %>% subset(continent != 'Africa')

#Figure 4 
ggplot(data = no_africa, aes(x=gdpPercap, y=lifeExp)) + 
  theme_minimal() + 
  geom_point() + 
  geom_smooth(method = "loess", span = .4, method.args = list(degree = 1), se = FALSE) + 
  xlab('GDP per Capita') + 
  ylab('Average Life Expectancy') + 
  ggtitle('2007: Life Expectancy vs GDP per Capita without Africa')

#Looks like it fits reasonably well, so lets check residuals 


#Appendix, Figure 11 
loess.no_africa= loess(lifeExp ~ gdpPercap, data = no_africa, span = .4, degree = 1)
#Residuals of the model without Africa 
ggplot(augment(loess.no_africa, data = no_africa, )) +
  geom_point(aes(x = gdpPercap, y = .resid, color=continent)) + 
  theme_minimal() + 
  ggtitle('Residuals for LOESS Fit, without Africa') + 
  ylab('Residuals') + 
  xlab('GDP per Capita') + 
  theme_hc() + 
  scale_color_viridis_d() 
#Tinsy bit of structure, but overall a good fit 

################################################################
#2Life Expectancy over time  By Continent -- a little messy 


ggplot(data = gapminder_data, aes(x=year, y=lifeExp, color=continent)) + 
  geom_point() + 
  theme_minimal() + 
  facet_wrap(~continent) +
  ggtitle('Life Expectancy over time by Continent: 1952-2007') + 
  ylab('Life Expectancy') 

#Figure 5
  #All on one graph with smoothers
  ggplot(data=gapminder_data, aes(x=year, y=lifeExp, color=continent)) +
  geom_point() +
  ylab('Life Expectancy over Time by Continent: 1952-2007')  + 
  ggtitle('Life Expectancy over time by continent: 1952-2007')+
  stat_smooth(method = "loess", se = FALSE) + 
  theme_hc() + 
  scale_color_viridis_d() 

## Still A little confusing, lets graph only the weighted means for each continent 

#Figure 6
#Graphing Mean Life Expectancy by Continent by Year 
ggplot(data = life_exp_continent_year, aes(x=year, y=w_avgLifeExp, color=continent)) + 
  geom_point() + 
  theme_minimal() + 
  ggtitle('Average Life Expectancy over Time by Continent: 1952-2007') + 
  ylab('Weighted Mean Life Expectancy') + 
  theme_hc() + 
  stat_smooth(method = "loess", se = FALSE) +
  scale_color_viridis_d() 


#Calculating Mean Life Expectency by Continent by Year 
#Weight Weighted Life Expectancy 
life_exp_continent_year = gapminder_data %>% group_by(continent, year) %>%
  mutate(w_avgLifeExp = weighted.mean(lifeExp, pop))

#Figure 7 
#Investigating Africa more in depth 
africa_timeseries = life_exp_continent_year %>% subset(continent=='Africa')
africa_timeseries
ggplot(data = africa_timeseries, aes(x=year, y=lifeExp)) + 
  geom_point() + 
  theme_minimal() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~country) +
  ggtitle('Life Expectancy over time in Africa: 1952-2007') + 
  ylab('Life Expectancy') 

#With this facet graph you see there are two main trends: 
#One of general ascencion, one with a dip:



#Two main trends: 1 going up, other with a dip around 1990s. 
#AIDS crisis
#'Botswana', 'Burundi', 'Cameroon', 'Central Africa Republic', 'Gabon', 'Kenya', 'Lesotho', 'Malawi', 'Namibia', 'Swaziland', 'Zambia', 'Zimbabwe' 

ggplot(data = africa_timeseries, aes(x=year, y=lifeExp)) + 
  geom_point() + 
  theme_minimal() + 
  ggtitle('Life Expectancy over time in Africa: 1952-2007') + 
  ylab('Life Expectancy') 

######################################################################
#Question 3 
#Changes in the relationship between GDP and life expectancy over
#time

#Ugly ugly coplot
coplot(
  lifeExp~ gdpPercap | year, data = gapminder_data, number = 10)

####Beginning of stealing from PROF FUKUYAMA's slides! 
make_coplot_df = function(data_frame, faceting_variable, number_bins = 6) {
  ## co.intervals gets the limits used for the conditioning intervals
  intervals = co.intervals(data_frame[[faceting_variable]], number = number_bins)
  ## indices is a list, with the ith element containing the indices of the
  ## observations falling into the ith interval
  indices = apply(intervals, 1, function(x)
    which(data_frame[[faceting_variable]] <= x[2] & data_frame[[faceting_variable]] >= x[1]))
  ## interval_descriptions is formatted like indices, but has interval
  ## names instead of indices of the samples falling in the index
  interval_descriptions = apply(intervals, 1, function(x) {
    num_in_interval = sum(data_frame[[faceting_variable]] <= x[2] & data_frame[[faceting_variable]] >= x[1])
    interval_description = sprintf("(%.2f, %.2f)", x[1], x[2])
    return(rep(interval_description, num_in_interval))
  })
  ## df_expanded has all the points we need for each interval, and the
  ## 'interval' column tells us which part of the coplot the point should
  ## be plotted in
  df_expanded = data_frame[unlist(indices),]
  df_expanded$interval = factor(unlist(interval_descriptions),
                                levels = unique(unlist(interval_descriptions)), ordered = TRUE)
  return(df_expanded)
}
### End of stealing 


#Figure 8
#Life Expectancy ~ Log GDP per capita given Continent 
gapminder_expanded = make_coplot_df(gapminder_data, "year", 10)
ggplot(gapminder_expanded, aes(y = lifeExp, x = log(gdpPercap), color=interval)) +
  facet_wrap(~ continent, ncol = 6) +
  geom_point()+
  stat_smooth(method = "loess", se = FALSE, span = 1, method.args = list(degree = 1, parametric = "gdpPercap", drop.square = "gdpPercap", family = "symmetric")) +
  #scale_x_continuous(breaks = seq(0, 10000, by=1000)) +
  ggtitle("Coplot of Life Expectancy ~ log(GDP per capita) given Continent") + 
  scale_color_viridis_d() +
  theme_hc() 


#Figure 9
#Take log of GDP per capita to help make it a better fit 
ggplot(gapminder_expanded, aes(y = lifeExp, x = log(gdpPercap), color=continent)) +
  facet_wrap(~ interval, ncol = 6) +
  stat_smooth(method = "loess", se = FALSE, span = 1, method.args = list(degree = 1, parametric = "gdpPercap", drop.square = "gdpPercap", family = "symmetric")) +
  scale_x_continuous(breaks = seq(0, 60000, by=10000)) +
  ggtitle("Coplot of Life Expectancy ~ log(GDP per capita) given Year") + 
  scale_color_viridis_d() +
  theme_hc()

