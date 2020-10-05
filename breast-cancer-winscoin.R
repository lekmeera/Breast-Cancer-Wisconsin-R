---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
```{r}
library(mlbench)
library(caret)
```

```{r}
dat<- read.csv('D:\\Data Science\\Projects\\breast-cancer-wisconsin-data\\orginal\\diagnostic-wbc.csv',header=FALSE,na = '?')
names(dat) <- c('Id','Outcome','radius_mean', 'texture_mean', 'perimeter_mean',
       'area_mean', 'smoothness_mean', 'compactness_mean', 'concavity_mean',
       'concave points_mean', 'symmetry_mean', 'fractal_dimension_mean',
       'radius_se', 'texture_se', 'perimeter_se', 'area_se', 'smoothness_se',
       'compactness_se', 'concavity_se', 'concave points_se', 'symmetry_se',
       'fractal_dimension_se', 'radius_worst', 'texture_worst',
       'perimeter_worst', 'area_worst', 'smoothness_worst',
       'compactness_worst', 'concavity_worst', 'concave points_worst',
       'symmetry_worst', 'fractal_dimension_worst')
dat
```
```{r}
dat1 <- read.csv('D:\\Data Science\\Projects\\breast-cancer-wisconsin-data\\orginal\\prognostic-wbc.csv',header = FALSE,na = '?')
names(dat1) <- c('Outcome','Time','radius_mean', 'texture_mean', 'perimeter_mean',
       'area_mean', 'smoothness_mean', 'compactness_mean', 'concavity_mean',
       'concave points_mean', 'symmetry_mean', 'fractal_dimension_mean',
       'radius_se', 'texture_se', 'perimeter_se', 'area_se', 'smoothness_se',
       'compactness_se', 'concavity_se', 'concave points_se', 'symmetry_se',
       'fractal_dimension_se', 'radius_worst', 'texture_worst',
       'perimeter_worst', 'area_worst', 'smoothness_worst',
       'compactness_worst', 'concavity_worst', 'concave points_worst',
       'symmetry_worst', 'fractal_dimension_worst','Tumor size','Lymph node status')
dat1
```


```{r}
drops <- c("Tumor size","Lymph node status","Time")
dat1 = dat1[ , !(names(dat1) %in% drops)]
```
```{r}
drops1 <- c("Id")
dat = dat[ , !(names(dat) %in% drops1)]
```
```{r}
dat
```
```{r}
dat1
```
```{r}
df = merge(dat, dat1, by="Outcome", all=TRUE)
df
```
```{r}
#df$Outcome <- str_replace_all(df$Outcome, 'M', '2')
levels(df$Outcome) <- sub("M", "2", levels(df$Outcome))
levels(df$Outcome) <- sub("B", "1", levels(df$Outcome))
df
```

```{r}
df$Outcome <- as.numeric(as.character(df$Outcome))
tail(df)
```
```{r}
dim(df)
```
```{r}
sapply(df, class)
```
```{r}
df[is.na(df)] <- 0
```
```{r}
names(df)
```
```{r}
#df['?']=NaN:df
#for(i in 1:9) {
    #df[,i] <- df['?']=NaN
for(i in 1:9) {
    df[,i] <- as.numeric(as.character(df[,i]))
}
df
```
```{r}
data_omit <- na.omit(df)                            
data_omit
```
```{r}
sapply(data_omit, mean, na.rm=TRUE)
```
```{r}
summary(data_omit)
```

```{r}
lapply(data_omit, function(data_omit) sum(is.na(data_omit)))
```
```{r}
list_na <- colnames(data_omit)[ apply(data_omit, 2, anyNA) ]
list_na
```
```{r}
str(data_omit)
```
```{r}
library("Hmisc")
res2 <- rcorr(as.matrix(data_omit))
res2
```
```{r}
#install.packages("corrplot")
library(corrplot)
library(Hmisc)
matriz <-rcorr(as.matrix(data_omit), type=c("spearman"))
matriz
```
```{r}
corr_mat <- cor(data_omit[,3:ncol(data_omit)])
corrplot(corr_mat, order = "hclust", tl.cex = .5, addrect = 9)
```
```{r}
par(mfrow=c(3,3))
for(i in 1:9) {
    hist(data_omit[,i], main=names(data_omit)[i])
}

```
```{r}
# density plot for each attribute
par(mfrow=c(3,3))
complete_cases <- complete.cases(data_omit)
for(i in 1:9) {
    plot(density(data_omit[complete_cases,i]), main=names(data_omit)[i])
}

```
```{r}
# boxplots for each attribute
par(mfrow=c(3,3))
for(i in 1:9) {
    boxplot(data_omit[,i], main=names(data_omit)[i])
}
```
```{r}
# scatterplot matrix
jittered_x <- sapply(data_omit[,1:9], jitter)
pairs(jittered_x, names(data_omit[,1:9]), col=data_omit$Outcome)
```
```{r}

# bar plots of each variable by class
par(mfrow=c(3,3))
for(i in 1:9) {
    barplot(table(data_omit$Outcome,data_omit[,i]), main=names(data_omit)[i], legend.text=unique(data_omit$Outcome))
}
```