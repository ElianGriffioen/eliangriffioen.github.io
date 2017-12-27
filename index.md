# Elian Griffioen

Hello, I am Elian Griffioen, second year Methodology & Statistics student at Utrecht University. Currently, I spend my time on a master thesis in which I try to find individualized donor strategies for anti-RhD donors. Anti-RhD antibodies are necessary to prevent haemolytic disease of the newborn, a disease that might occur when RhD-negative mothers are pregnant of a RhD-positive foetus. [Sanquin](https://www.sanquin.nl/en/), the Dutch National Blood Bank, collects these antibodies from a donor pool. My research focuses on deriving individual donor treatments based on their personal characteristics such that the total production of antibodies increases compared to the previous situation in which all donors share a similar treatment.

# More about me
Before my master in Methods & Statistics, I finished the bachelor programmes Chemistry and Psychology (both cum laude) at Utrecht University. I decided to go for statistics because of the combination of using advanced statistical methods based on mathematical foundations and researching real world problems. After one year of in depth courses about statistics like Statistical Programming in R, Bayesian Statistics, Multilevel and Structural Equation Modelling, I choose to focus my electives on data science and econometrics.Therefore, I followed elective courses on machine learning and data mining and joined courses in econometrics. Next to my thesis work, I am student assistant of [Rens van de Schoot](https://www.rensvandeschoot.com/elian-griffioen/) and helps with his work as a member of the [Young Academy](https://www.dejongeakademie.nl/en?set_language=en) in the field of scientific integrity. Furthermore, I am secretary of the [Young Statisticians](http://youngstatisticians.nl/index.html) Board, a group of people that aims to organize events for all statisticians in the Netherlands on a montly basis.

# Simulation

```markdown
---
title: "Exercise 5"
author: "Elian Griffioen"
date: "13 november 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

In this exercise, a simulation is performed to learn about confidence intervals.
100 samples of size 100 are drawn from the standard normal ditribution,
95-% confidence intervals and the coverage of the population mean are 
calculated. It is hypothesized that the coverage is close 
to 95 %.

## Drawing samples

First, hundred samples of hundred observations were drawn from a standard normal distribution Don't forget to set a seed, since you want to do your work in a 
reproducible manner.

$$X \sim \mathcal{N}(0,\,1)$$
Using this code, 100 samples of size 100 are drawn from the standard normal
distribution and stored in a list called datasets.
```{r, echo=TRUE}
# set seed for reproducibility
set.seed(123456789)

# create list to store datasets
datasets<-list()

# sample hundred times from standard normal distribution sample of size 100
for(i in 1:100){
datasets[[i]]<-rnorm(100,mean=0,sd=1)
}
```

## Simulation of confidence intervals

For each sample the absolute bias, the standard error and the lower and upper
bound of the confidence interval are calculated:
$$ b_{abs} = | \bar{x} - 0 | = | \frac{1}{n} \sum_{i=1}^{n}x_{i} |$$ 
$$se_{\bar{x}} = \frac{\sigma_{x}}{\sqrt{n}} = \frac{ \sqrt{\frac{\sum\limits_{i=1}^{n} \left(x_{i} - \bar{x}\right)^{2}} {n-1}}}{\sqrt{n}}$$


$$ 95 \%CI = \bar{x} \pm z_{.975} \cdot se_{\bar{x}} $$
```{r measures, echo=TRUE}
# Define vector to store results for each dataset
abs.bias <- c()
se <- c()
n <- length(datasets)
lowerbound <- c()
upperbound <- c()

# Calculate absolute bias, standard error of the mean and confidence interval for
# each dataset
for(i in 1:n){
  abs.bias[i] <- abs(mean(datasets[[i]]))
  se[i] <- sd(datasets[[i]])/sqrt(n)
  lowerbound[i] <- mean(datasets[[i]]) - 1.96*se[i]
  upperbound[i] <- mean(datasets[[i]]) + 1.96*se[i]

}
confidence.intervals <- cbind(lowerbound,upperbound)
```



## Plotting confidence intervals

The confidence intervals are plotted together with the population mean to get
an impression of the number of confidence interval including the population 
mean.
```{r plot, echo=TRUE}
# Define a plot with the range of confidence intervals at the x-axis and the
# dataset numbers at the y-axis
plot(range(confidence.intervals), c(0, n), type = "n",
     xlab = "Confidence interval", ylab = "Dataset")

# Add a line segment for each confidence interval
for(i in 1:100){
  segments(confidence.intervals[i,1],i,confidence.intervals[i,2],i,col="darkorange")
}

# Define an verticle line for the population mean of zero
abline(v = 0, lwd = 2, lty = 2,col="blue")
```

## Analyzing the confidence interval

The coverage is calculated as the proportion of confidence interval containing
the population mean. In this way, the hypothesis that the coverage is 
approximately equal to 95 %. Afterwards, the datasets that do not contain the
population mean are tabulated in Table 1.

```{r}
# Calculate number of confidence interval that do not contain population mean
out <- sum(confidence.intervals[,1] < 0  & confidence.intervals[,2] < 0) + 
  sum(confidence.intervals[,1] > 0  & confidence.intervals[,2] > 0) 

# Coverage is proportion of confidence intervals that contain population mean
(coverage <- (n-out)/n)

# Acquire indices for confidence interval without population mean
indices.low <- confidence.intervals[,1] < 0  & confidence.intervals[,2] < 0
indices.high <- confidence.intervals[,1] > 0  & confidence.intervals[,2] > 0

# Bind indices 
indices <- rep(NA,100)
indices[indices.low==TRUE] <- TRUE
indices[indices.high==TRUE] <- TRUE
indices[is.na(indices)] <- FALSE

# Get datasets that do not contain population mean
out.datasets <- datasets[indices]
out.datasets.dataframe <- cbind.data.frame(out.datasets[[1]],out.datasets[[2]],
                                           out.datasets[[3]],out.datasets[[4]],
                                           out.datasets[[5]],out.datasets[[6]])
colnames(out.datasets.dataframe) <- rep(1:100)[indices]

# Tabulate these datasets
knitr::kable(out.datasets.dataframe,format="html",caption = "Table 1. Overview of datasets that do not contain population mean of zero")

```


## Conclusion

The coverage is .94, which is in agreement with the hypothesis.So the 95 % in 
95-% confidence interval can be interpreted as the proportion of samples from a
population that contain the true population mean, i.e. the long-run probability
of a sample that contains the population mean.
```

# Contact

You can contact me via [Facebook](https://www.facebook.com/elian.griffioen?ref=bookmarks), [LinkedIn](https://www.linkedin.com/in/elian-griffioen-5b7818103/) and email via e.griffioen@students.uu.nl

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/ElianGriffioen/eliangriffioen.github.io/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
