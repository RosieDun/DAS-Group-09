@ -0,0 +1,207 @@
  ---
  title: "Group_09_Analysis.qmd"
format: pdf
editor: visual
---
  
  ## Introduction
  
  The following analysis aims to understand the relationship between a set of descriptive variables about a film and its success measured by its respective IMDB rating.

The central question around this analysis will be the following: **Which properties of films influence whether they are rated by IMDB as greater than 7 or not?**
  
  From this question it is established that the target variable will be binary and hence a Logistic Regression model seems reasonable for this scenario. It is also established that missing variables ( in case they are found ) will be inputted with a summary statistic like mean or median if the distribution of this subset is similar to that of the complete data set, otherwise they will be deleted if they do not represent a large portion of the data set.

Throughout this analysis a full model will be fitted taking into account all numerical and categorical variables in the data set. Then the best performing model will be selected and it will only include those variables which are found to be significant.

Finally, a short summary of the model and answers to the analysis question will be found in the conclusion section.

## Data Cleaning

The film data set obtained from IMDB contains the following variables:
  
  -   film.id - The unique identifier for the film

-   year - Year of release of the film in cinemas

-   length - Duration (in minutes)

-    budget - Budget for the films production (in \$1000000s)

-   votes - Number of positive votes received by viewers

-   genre - Genre of the film

-   rating - IMDB rating from 0-10

```{r}
library(tidyverse)
install.packages("skimr")
library(skimr)

#Read data set
film <- read.csv("dataset09.csv") %>% 
  #Define target variable
  mutate(target = ifelse(rating>7,1,0))

#Create summary
film %>% skim()
```

It is now established that film_id will not be used as an explanatory variable since it is only an identifier for the film, rather than an informative feature about it. Genre is the only categorical variable contained in the data set. Year, length, budget, and votes are the numerical explanatory variables to be tested in this analysis.

When it comes to the data set, there seems to be an issue with the length variable as there are 127 rows where this information is missing.

```{r}
#Skim for the rows with lenght == NA
film %>% filter(is.na(length)) %>% skim()
```

From the summary above it is easy to see that the distribution of these 127 rows is fairly similar to that of the complete data set ( This is made evident when comparing the histograms of the filtered data set to those of the complete data set ).

```{r}
film %>% group_by(genre) %>% select(genre, length) %>% skim()
```

It is evident in from the summary table above that the length distribution is not equal amongst different film genres and therefore the missing film lengths will be handled by adding the median film length by genre to its corresponding missing columns (the mean is not used to avoid outlier influence). The different behaviour between genre and film length was expected, especially because one category is called "Short".

```{r}
film.median <- film %>% group_by(genre) %>% select(genre, length) %>% summarise(median.length = median(length, na.rm=TRUE))
film.median
```

```{r}
#input corresponding genre median for length missing values 
film <- film %>% inner_join(film.median,by=join_by(genre)) %>% mutate(length=ifelse(is.na(length),median.length,length)) %>% select(-median.length) 
```

## Exploratory Analysis

The last step before fitting the Logistic Regression model is analysing the data set to identify possible patterns.

```{r}
#These are just some simple plots. Please add more plots as necessary. 
#If a certain plot is needed in the power point presentation please write its code here. 
```

```{r}
library(ggplot2)
#plot target variable against year covariate 
film %>% ggplot(aes(x=factor(target),y=year, colour=factor(target))) + 
  geom_boxplot() + 
  theme(legend.position="none") +
  labs(x="target")

```

```{r}
library(ggplot2)
#plot target variable against length covariate 
film %>% ggplot(aes(x=factor(target),y=length, colour=factor(target))) + 
  geom_boxplot() + 
  theme(legend.position="none") +
  labs(x="target")
```

```{r}
library(ggplot2)
#plot target variable against budget covariate 
film %>% ggplot(aes(x=factor(target),y=budget, colour=factor(target))) + 
  geom_boxplot() + 
  theme(legend.position="none") +
  labs(x="target")
```

```{r}
library(ggplot2)
#plot target variable against votes covariate 
film %>% ggplot(aes(x=factor(target),y=votes, colour=factor(target))) + 
  geom_boxplot() + 
  theme(legend.position="none") +
  labs(x="target")
```

```{r}
#count by genre
film %>% 
  ggplot(aes(x=genre, colour=genre)) +
  geom_bar() +
  theme(legend.position="none") +
  labs(y="Count", x="Film genre")
```

```{r}
#proportion of films with rating >7 by genre
film %>% group_by(genre) %>% 
  summarise(prop = mean(target)) %>% 
  arrange() %>% 
  ggplot(aes(x=genre, y=prop, colour=genre)) +
  geom_col() +
  theme(legend.position="none") +
  labs(y="Proportion with rating > 7", x="Film genre")

```

```{r}

```

## Model Fitting 

```{r}
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(kableExtra)
```

```{r}
# Fit a full model with all possible covariates
model1 <- glm(target ~ year + length + budget + votes + genre , data = film, 
              family = binomial(link = "logit"))
model1 %>%
  summary()
```

The year the film was released is not significant in the model above. This is expected as the year a certain movie was released might not say much about its success on IMDB.

```{r}
#Fit without year, change votes to thousands so the scale is easier to interpret
model2 <- glm(target ~ length + budget + votes + genre , data = film %>% mutate(votes=votes/1000), 
              family = binomial(link = "logit"))
model2 %>%
  summary()
```


```{r}
mod2coefs <- round(coef(model2), 3)
mod2coefs
```

```{r}
#confint(model) 
```

```{r}
plot_model(model2., type = "pred", title = "",
           axis.title = c("Ethnicity", "Prob. of instructor being male"))
```

```{r}
film <- film %>% mutate(probs=fitted(model2))
```

```{r}
#obtain the model equation and display it using latek to be able to determine which variables influence a film's success the most 
```

## Conclusion

```{r}
#Write a conclusion about which variables were non-significant, which were significant and how much they influence the probability of having a score >7
```