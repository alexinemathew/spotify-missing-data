---
title: 'Missing Data: Final Project'
author: "Alexine Mathew"
date: "3/12/2022"
output:
  html_document:
    keep_md: true
geometry: margin=0.25in
---





```r
# original dataset - no missingness
spotify <- read.csv("spotify.csv")
set.seed(123)
nrow(spotify) # 1545 observations 
```

```
## [1] 1545
```

```r
# x1: Popularity - numeric
# x2: Tempo - numeric
# x3: feature - dichotomous
# x4: Artist.Followers - numeric, scaled  
# x5: Duration..ms - numeric, scaled 
# y: Number.of.Times.Charted - numeric

# regression summary - this is our benchmark against other models 
reg <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + as.factor(Feature) + scale(Duration.ms), data=spotify)
summary(reg)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     as.factor(Feature) + scale(Duration.ms), data = spotify)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.596  -8.777  -6.180   1.708 127.487 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -3.14907    2.53506  -1.242    0.214    
## Popularity               0.24029    0.02592   9.270   <2e-16 ***
## Tempo                   -0.02295    0.01374  -1.670    0.095 .  
## scale(Artist.Followers) -0.06600    0.42141  -0.157    0.876    
## as.factor(Feature)1     -0.81121    0.98574  -0.823    0.411    
## scale(Duration.ms)       0.27799    0.41316   0.673    0.501    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.94 on 1539 degrees of freedom
## Multiple R-squared:  0.05664,	Adjusted R-squared:  0.05357 
## F-statistic: 18.48 on 5 and 1539 DF,  p-value: < 2.2e-16
```

```r
# SEs
reg.sd <- coef(summary(reg))[, "Std. Error"]
reg.sd
```

```
##             (Intercept)              Popularity                   Tempo 
##              2.53505871              0.02592200              0.01373744 
## scale(Artist.Followers)     as.factor(Feature)1      scale(Duration.ms) 
##              0.42140532              0.98574344              0.41316218
```
I scaled the variables Artist.Followers and Duration.ms to have them on comparable magnitudes to the rest of the variables. 
```
Estimate equation: Number.of.Times.Charted = -3.14907 + 0.24029*Popularity 
- 0.02295*Tempo - 0.06600*Artist.Followers -0.81121*Factor + 0.27799*Duration.ms
```
If we use an alpha level of 0.05 to determine which predictors are significant in this model, we would say that only Popularity is statistically significant. 


```r
# generate MAR missingness 
set.seed(123)
spotify.miss = ampute(spotify, prop = 0.6, mech = "MAR")$amp
# leave one numerical variable as completely observed (Duration.ms)
spotify.miss = cbind(spotify.miss[,1:5],spotify[,6])
colnames(spotify.miss)[6] = "Duration.ms"
# refactor Feature as factor for new df with missingness
spotify.miss$Feature <- as.factor(spotify.miss$Feature)
```
### 2. Summary Statistics 

```r
# summary statistics  
summary(spotify.miss)
```

```
##  Number.of.Times.Charted   Popularity         Tempo        Feature    
##  Min.   :  1.0           Min.   :  0.00   Min.   : 46.72   0   :1060  
##  1st Qu.:  1.0           1st Qu.: 65.00   1st Qu.: 98.01   1   : 321  
##  Median :  4.0           Median : 73.00   Median :122.02   NA's: 164  
##  Mean   : 10.6           Mean   : 69.96   Mean   :122.76              
##  3rd Qu.: 12.0           3rd Qu.: 80.00   3rd Qu.:143.03              
##  Max.   :142.0           Max.   :100.00   Max.   :205.27              
##  NA's   :142             NA's   :145      NA's   :146                 
##  Artist.Followers    Duration.ms    
##  Min.   :    4883   Min.   : 30133  
##  1st Qu.: 1890510   1st Qu.:169266  
##  Median : 6837946   Median :193591  
##  Mean   :14609912   Mean   :197941  
##  3rd Qu.:22683756   3rd Qu.:218902  
##  Max.   :83337783   Max.   :588139  
##  NA's   :156
```

```r
# histograms for numerical variables 
hist <- data.frame(spotify.miss$Number.of.Times.Charted, spotify.miss$Popularity, spotify.miss$Tempo, spotify.miss$Artist.Followers, spotify.miss$Duration.ms)
Hmisc::hist.data.frame(hist) # obvious skews for Number.of.Times.Charted and Artist.Followers
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# boxplot for categorical variable
# distribution of 0 and 1 seems similar, outliers in 0 (no feature)
boxplot(Number.of.Times.Charted~Feature,data=spotify.miss,
   xlab="Feature", ylab="Number.of.Times.Charted",col=c("orange" , "seagreen"))  
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# % missing per variable
miss_var_summary(spotify.miss) 
```

```
## # A tibble: 6 × 3
##   variable                n_miss pct_miss
##   <chr>                    <int>    <dbl>
## 1 Feature                    164    10.6 
## 2 Artist.Followers           156    10.1 
## 3 Tempo                      146     9.45
## 4 Popularity                 145     9.39
## 5 Number.of.Times.Charted    142     9.19
## 6 Duration.ms                  0     0
```

```r
# percent complete cases
colMeans(!is.na(spotify.miss))
```

```
## Number.of.Times.Charted              Popularity                   Tempo 
##               0.9080906               0.9061489               0.9055016 
##                 Feature        Artist.Followers             Duration.ms 
##               0.8938511               0.8990291               1.0000000
```
### 3. Mean/Mode Imputation

```r
# numerical variables 
str(spotify)
```

```
## 'data.frame':	1545 obs. of  6 variables:
##  $ Number.of.Times.Charted: int  8 3 11 5 1 18 16 10 8 10 ...
##  $ Popularity             : int  100 99 99 98 96 97 94 95 96 95 ...
##  $ Tempo                  : num  134 170 167 126 150 ...
##  $ Feature                : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Artist.Followers       : int  3377762 2230022 6266514 83293380 5473565 5473565 8640063 6080597 36142273 3377762 ...
##  $ Duration.ms            : int  211560 141806 178147 231041 212000 137876 208867 199604 206710 173347 ...
```

```r
# copy dataset with missingness 
spotify.imputed <- spotify.miss
# mean imputation function
# The input is a data vector to be imputed 
mean.imp <- function (a)
  {
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  # Output the imputed vector
  return (imputed)
}
# mean imputation for numerical variables 
spotify.imputed$Popularity <- mean.imp(spotify.imputed$Popularity)
spotify.imputed$Tempo <- mean.imp(spotify.imputed$Tempo)
spotify.imputed$Number.of.Times.Charted <- mean.imp(spotify.imputed$Number.of.Times.Charted)
spotify.imputed$Artist.Followers <- mean.imp(spotify.imputed$Artist.Followers)
```


```r
# mode function
mode = function(x)
{
 ta = table(x)
 tam = max(ta)
 if (all(ta == tam))
 mod = NA
 else
 mod = names(ta)[ta == tam]
 return(mod)
}
# mode imputation
mode.imp <- function (a)
{
 missing <- is.na(a)
 a.obs <- a[!missing]
 imputed <- a
 imputed[missing] <- mode(a.obs)
 # Output the imputed vector
 return (imputed)
}
# mode imputation for categorical variable
# Feature 
spotify.imputed$Feature <- mode.imp(spotify.imputed$Feature)
```


```r
##  comparing results 
# mean/mode imputed data
set.seed(123)
reg.mean <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + Feature + scale(Duration.ms), data=spotify.imputed)
summary(reg.mean)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     Feature + scale(Duration.ms), data = spotify.imputed)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.278  -8.529  -5.615   1.618 128.412 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -1.52939    2.51249  -0.609    0.543    
## Popularity               0.21184    0.02532   8.366   <2e-16 ***
## Tempo                   -0.02161    0.01384  -1.561    0.119    
## scale(Artist.Followers) -0.13976    0.39710  -0.352    0.725    
## Feature1                -0.17624    0.96828  -0.182    0.856    
## scale(Duration.ms)       0.22494    0.39039   0.576    0.565    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.14 on 1539 degrees of freedom
## Multiple R-squared:  0.04635,	Adjusted R-squared:  0.04325 
## F-statistic: 14.96 on 5 and 1539 DF,  p-value: 2.308e-14
```

```r
# total change 
sum(abs(reg.mean$coef - reg$coef)) 
```

```
## [1] 2.411273
```

```r
# average percent change in coefficients - absolute terms 
mean(abs(reg.mean$coef - reg$coef)/abs(reg$coef)) 
```

```
## [1] 0.4637475
```

```r
# SE
reg.mean.sd <- coef(summary(reg.mean))[, "Std. Error"]
# average percent change in SE
mean((reg.mean.sd - reg.sd)/(reg.sd))
```

```
## [1] -0.02587219
```

```r
# average percent change in SE - absolute terms 
mean(abs(reg.mean.sd - reg.sd)/abs(reg.sd))
```

```
## [1] 0.0283127
```


```r
## Density of Tempo,Duration.ms,Number.of.Times.Charted pre and post imputation ##
par(mfrow = c(1, 3))
# observed Tempo
plot(density(spotify$Tempo),
     lwd = 2, 
     ylim = c(0, 0.020),
     main="",
     xlab = "Tempo")
# imputed Tempo
points(density(spotify.imputed$Tempo), 
       lwd = 2, 
       ylim = c(0, 0.020),
       type = "l", 
       col = "red")
# observed Popularity
plot(density(spotify$Popularity),
     lwd = 2, 
     ylim = c(0, 0.05),
     main="",
     xlab = "Popularity")
# imputed Popularity
points(density(spotify.imputed$Popularity), 
       lwd = 2, 
       ylim = c(0, 0.05),
       type = "l", 
       col = "red")
# observed Number.of.Times.Charted
plot(density(spotify$Number.of.Times.Charted),
     lwd = 2, 
     main="",
     xlab = "Number.of.Times.Charted")
# imputed Number.of.Times.Charted
points(density(spotify.imputed$Number.of.Times.Charted), 
       lwd = 2, 
       ylim = c(0, 0.05),
       type = "l", 
       col = "red")
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
## Correlations ##
# only numerical variables 
cor.spotify <- data.frame(spotify$Number.of.Times.Charted, spotify$Popularity, spotify$Tempo, spotify$Artist.Followers, spotify$Duration.ms)
cor.spotify.miss <- data.frame(spotify.miss$Number.of.Times.Charted, spotify.miss$Popularity, spotify.miss$Tempo, spotify.miss$Artist.Followers, spotify.miss$Duration.ms)
# correlations between variables in observed and imputed data-sets
r <- data.frame(cor(cor.spotify))
r.miss <- data.frame(cor(cor.spotify.miss, use ="na.or.complete"))
names(r) <- c("Number.of.Times.Charted", "Popularity", "Tempo", "Artist.Followers","Duration.ms")
names(r.miss) <- c("Number.of.Times.Charted", "Popularity", "Tempo", "Artist.Followers","Duration.ms")
r %>%
  kbl(caption = "Observed Dataset") %>%
  kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Observed Dataset</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Number.of.Times.Charted </th>
   <th style="text-align:right;"> Popularity </th>
   <th style="text-align:right;"> Tempo </th>
   <th style="text-align:right;"> Artist.Followers </th>
   <th style="text-align:right;"> Duration.ms </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> spotify.Number.of.Times.Charted </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.2327956 </td>
   <td style="text-align:right;"> -0.0483073 </td>
   <td style="text-align:right;"> 0.0274583 </td>
   <td style="text-align:right;"> 0.0339804 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Popularity </td>
   <td style="text-align:right;"> 0.2327956 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0249509 </td>
   <td style="text-align:right;"> 0.1043577 </td>
   <td style="text-align:right;"> 0.0820957 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Tempo </td>
   <td style="text-align:right;"> -0.0483073 </td>
   <td style="text-align:right;"> -0.0249509 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0198811 </td>
   <td style="text-align:right;"> -0.0046714 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Artist.Followers </td>
   <td style="text-align:right;"> 0.0274583 </td>
   <td style="text-align:right;"> 0.1043577 </td>
   <td style="text-align:right;"> -0.0198811 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.1421446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Duration.ms </td>
   <td style="text-align:right;"> 0.0339804 </td>
   <td style="text-align:right;"> 0.0820957 </td>
   <td style="text-align:right;"> -0.0046714 </td>
   <td style="text-align:right;"> 0.1421446 </td>
   <td style="text-align:right;"> 1.0000000 </td>
  </tr>
</tbody>
</table>

```r
r.miss %>%
  kbl(caption = "Imputed Dataset") %>%
  kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Imputed Dataset</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Number.of.Times.Charted </th>
   <th style="text-align:right;"> Popularity </th>
   <th style="text-align:right;"> Tempo </th>
   <th style="text-align:right;"> Artist.Followers </th>
   <th style="text-align:right;"> Duration.ms </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> spotify.miss.Number.of.Times.Charted </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.1861257 </td>
   <td style="text-align:right;"> -0.0169547 </td>
   <td style="text-align:right;"> -0.0599380 </td>
   <td style="text-align:right;"> 0.0225561 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.miss.Popularity </td>
   <td style="text-align:right;"> 0.1861257 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0291190 </td>
   <td style="text-align:right;"> 0.1150416 </td>
   <td style="text-align:right;"> 0.0995576 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.miss.Tempo </td>
   <td style="text-align:right;"> -0.0169547 </td>
   <td style="text-align:right;"> -0.0291190 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0376671 </td>
   <td style="text-align:right;"> -0.0030918 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.miss.Artist.Followers </td>
   <td style="text-align:right;"> -0.0599380 </td>
   <td style="text-align:right;"> 0.1150416 </td>
   <td style="text-align:right;"> -0.0376671 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.1149550 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.miss.Duration.ms </td>
   <td style="text-align:right;"> 0.0225561 </td>
   <td style="text-align:right;"> 0.0995576 </td>
   <td style="text-align:right;"> -0.0030918 </td>
   <td style="text-align:right;"> 0.1149550 </td>
   <td style="text-align:right;"> 1.0000000 </td>
  </tr>
</tbody>
</table>

  
The first imputation method that was used was mean/mode imputation. Mean imputation was used for the numerical variables' missing values and mode was used for the categorical variable, Feature. 
The average percent change in coefficients, compared to complete data, was 46.37%.
The average percent change in SEs, compared to complete data, decreased by 2.59%.

```
Estimate equation: Number.of.Times.Charted = -1.5293 + 0.2118*Popularity 
- 0.0216*Tempo - 0.1397*Artist.Followers - 0.1762*Factor + 0.2249*Duration.ms
```
The plots above display the density of Tempo, Popularity, and Number.of.Times.Charted, before (in black) and after (in red) the mean imputation.
The imputation seemed to create higher or additional peaks in all of these variables.\ 
Against my expectation, while the average % chg in SE decreased, the SE for Tempo actually increased by 0.0001.
As seen on the correlation tables, correlation decreased for all pairs after imputation, except Tempo & Number.of.Times.Charted, Duration.ms & Popularity and Artist.Followers & Popularity. 
A general issue of mean imputation is that it does not preserve the relationships between coefficients, and we can see that illustrated here. 
  
### 4. Random Imputation

```r
# copy dataset with missingness
spotify.random <- spotify.miss
# random imputation function 
random.imp <- function (a)
{
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
  return (imputed)
}
# random imputation for all variables 
spotify.random$Popularity <- random.imp(spotify.random$Popularity)
spotify.random$Tempo <- random.imp(spotify.random$Tempo)
spotify.random$Number.of.Times.Charted <- random.imp(spotify.random$Number.of.Times.Charted)
spotify.random$Artist.Followers <- random.imp(spotify.random$Artist.Followers)
spotify.random$Feature <- random.imp(spotify.random$Feature)
```


```r
## comparing results 
# imputed data
set.seed(123)
reg.random <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + Feature + scale(Duration.ms), data=spotify.random)
summary(reg.random)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     Feature + scale(Duration.ms), data = spotify.random)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.642  -8.565  -6.297   1.791 128.756 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -1.88626    2.46841  -0.764    0.445    
## Popularity               0.20069    0.02510   7.996  2.5e-15 ***
## Tempo                   -0.01309    0.01353  -0.967    0.334    
## scale(Artist.Followers) -0.25668    0.40943  -0.627    0.531    
## Feature1                -0.46029    0.96195  -0.479    0.632    
## scale(Duration.ms)       0.22071    0.40356   0.547    0.585    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.65 on 1539 degrees of freedom
## Multiple R-squared:  0.04126,	Adjusted R-squared:  0.03815 
## F-statistic: 13.25 on 5 and 1539 DF,  p-value: 1.166e-12
```

```r
# total change 
sum(abs(reg$coef - reg.random$coef)) 
```

```
## [1] 1.911147
```

```r
# average percent change in coefficients
mean(abs(reg$coef - reg.random$coef)/abs(reg$coef)) 
```

```
## [1] 0.7538827
```

```r
# SE
reg.random.sd <- coef(summary(reg.random))[, "Std. Error"]
# average percent change in SE
mean((reg.random.sd - reg.sd)/(reg.sd)) 
```

```
## [1] -0.02477256
```

```r
# average percent change in SE - absolute terms 
mean(abs(reg.random.sd - reg.sd)/abs(reg.sd)) 
```

```
## [1] 0.02477256
```


```r
## Density of Tempo,Duration.ms,Number.of.Times.Charted pre and post random imputation ##
par(mfrow = c(1, 3))
# observed Tempo
plot(density(spotify$Tempo),
     lwd = 2, 
     ylim = c(0, 0.020),
     main="",
     xlab = "Tempo")
# imputed Tempo
points(density(spotify.random$Tempo), 
       lwd = 2, 
       ylim = c(0, 0.020),
       type = "l", 
       col = "red")
# observed Popularity
plot(density(spotify$Popularity),
     lwd = 2, 
     ylim = c(0, 0.05),
     main="",
     xlab = "Popularity")
# imputed Popularity
points(density(spotify.random$Popularity), 
       lwd = 2, 
       ylim = c(0, 0.05),
       type = "l", 
       col = "red")
# observed Number.of.Times.Charted
plot(density(spotify$Number.of.Times.Charted),
     lwd = 2, 
     main="",
     xlab = "Number.of.Times.Charted")
# imputed Number.of.Times.Charted
points(density(spotify.random$Number.of.Times.Charted), 
       lwd = 2, 
       ylim = c(0, 0.05),
       type = "l", 
       col = "red")
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# only numerical variables 
cor.random <- data.frame(spotify.random$Number.of.Times.Charted, spotify.random$Popularity, spotify.random$Tempo, spotify.random$Artist.Followers, spotify.random$Duration.ms)
# correlations between variables in observed and imputed data-sets
r.random <- data.frame(cor(cor.random, use ="na.or.complete"))
names(r.random) <- c("Number.of.Times.Charted", "Popularity", "Tempo", "Artist.Followers","Duration.ms")
r %>%
  kbl(caption = "Observed Dataset Correlations") %>%
  kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Observed Dataset Correlations</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Number.of.Times.Charted </th>
   <th style="text-align:right;"> Popularity </th>
   <th style="text-align:right;"> Tempo </th>
   <th style="text-align:right;"> Artist.Followers </th>
   <th style="text-align:right;"> Duration.ms </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> spotify.Number.of.Times.Charted </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.2327956 </td>
   <td style="text-align:right;"> -0.0483073 </td>
   <td style="text-align:right;"> 0.0274583 </td>
   <td style="text-align:right;"> 0.0339804 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Popularity </td>
   <td style="text-align:right;"> 0.2327956 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0249509 </td>
   <td style="text-align:right;"> 0.1043577 </td>
   <td style="text-align:right;"> 0.0820957 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Tempo </td>
   <td style="text-align:right;"> -0.0483073 </td>
   <td style="text-align:right;"> -0.0249509 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0198811 </td>
   <td style="text-align:right;"> -0.0046714 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Artist.Followers </td>
   <td style="text-align:right;"> 0.0274583 </td>
   <td style="text-align:right;"> 0.1043577 </td>
   <td style="text-align:right;"> -0.0198811 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.1421446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.Duration.ms </td>
   <td style="text-align:right;"> 0.0339804 </td>
   <td style="text-align:right;"> 0.0820957 </td>
   <td style="text-align:right;"> -0.0046714 </td>
   <td style="text-align:right;"> 0.1421446 </td>
   <td style="text-align:right;"> 1.0000000 </td>
  </tr>
</tbody>
</table>

```r
r.random %>%
  kbl(caption = "Imputed Dataset Correlations") %>%
  kable_classic(full_width = F, html_font = "Cambria") 
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Imputed Dataset Correlations</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Number.of.Times.Charted </th>
   <th style="text-align:right;"> Popularity </th>
   <th style="text-align:right;"> Tempo </th>
   <th style="text-align:right;"> Artist.Followers </th>
   <th style="text-align:right;"> Duration.ms </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> spotify.random.Number.of.Times.Charted </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.2005868 </td>
   <td style="text-align:right;"> -0.0271972 </td>
   <td style="text-align:right;"> 0.0078169 </td>
   <td style="text-align:right;"> 0.0265166 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.random.Popularity </td>
   <td style="text-align:right;"> 0.2005868 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0162245 </td>
   <td style="text-align:right;"> 0.0959419 </td>
   <td style="text-align:right;"> 0.0749957 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.random.Tempo </td>
   <td style="text-align:right;"> -0.0271972 </td>
   <td style="text-align:right;"> -0.0162245 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> -0.0389199 </td>
   <td style="text-align:right;"> -0.0161260 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.random.Artist.Followers </td>
   <td style="text-align:right;"> 0.0078169 </td>
   <td style="text-align:right;"> 0.0959419 </td>
   <td style="text-align:right;"> -0.0389199 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.1207405 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spotify.random.Duration.ms </td>
   <td style="text-align:right;"> 0.0265166 </td>
   <td style="text-align:right;"> 0.0749957 </td>
   <td style="text-align:right;"> -0.0161260 </td>
   <td style="text-align:right;"> 0.1207405 </td>
   <td style="text-align:right;"> 1.0000000 </td>
  </tr>
</tbody>
</table>

\
The next imputation method that was used was random imputation, which randomly samples from observed data.
The average percent change in coefficients was 75.4%, which was higher than mean/mode imputation. 
SEs decreased for all variables, with an overall average decrease of 2.48%. 
```
Estimate equation: Number.of.Times.Charted = -1.8862 + 0.2007*Popularity 
-0.0131*Tempo -0.2567*Artist.Followers -0.4603*Factor + 0.2207*Duration.ms
```
As expected, the density plots of random imputation almost perfectly match that of the observed data. 
The correlations changed slightly for most of the pairs, which implies that random imputation (like mean imputation) can also ignore relationships between variables.\


### 5. Hotdecking

```r
# copy dataset with missingness
spotify.hotdeck <- spotify.miss
# hotdecking 
spotify.hotdeck <- hotdeck(spotify.hotdeck, imp_var = FALSE)
```


```r
##  comparing results 
# imputed data
set.seed(123)
reg.hotdeck <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + Feature + scale(Duration.ms), data=spotify.hotdeck)
summary(reg.hotdeck)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     Feature + scale(Duration.ms), data = spotify.hotdeck)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.036  -8.663  -6.051   1.953 128.397 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              0.46662    2.45594   0.190   0.8493    
## Popularity               0.19873    0.02518   7.894 5.53e-15 ***
## Tempo                   -0.03141    0.01321  -2.378   0.0175 *  
## scale(Artist.Followers) -0.45951    0.40683  -1.129   0.2589    
## Feature1                -0.02092    0.94613  -0.022   0.9824    
## scale(Duration.ms)       0.37271    0.40160   0.928   0.3535    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.57 on 1539 degrees of freedom
## Multiple R-squared:  0.04449,	Adjusted R-squared:  0.04138 
## F-statistic: 14.33 on 5 and 1539 DF,  p-value: 9.755e-14
```

```r
# total change 
sum(abs(reg$coef - reg.hotdeck$coef)) 
```

```
## [1] 4.944241
```

```r
# average percent change in coefficients
mean(abs(reg$coef - reg.hotdeck$coef)/abs(reg$coef)) 
```

```
## [1] 1.494594
```

```r
# SE
reg.hotdeck.sd <- coef(summary(reg.hotdeck))[, "Std. Error"] 
# average percent change in SE
mean((reg.hotdeck.sd - reg.sd)/(reg.sd)) 
```

```
## [1] -0.03354412
```

```r
# average percent change in SE - absolute terms
mean(abs(reg.hotdeck.sd - reg.sd)/abs(reg.sd))
```

```
## [1] 0.03354412
```


```r
## Density of Tempo,Duration.ms,Number.of.Times.Charted pre and post imputation ##
par(mfrow = c(1, 3))
# observed Tempo
plot(density(spotify$Tempo),
     lwd = 2, 
     ylim = c(0, 0.020),
     main="",
     xlab = "Tempo")
# imputed Tempo
points(density(spotify.hotdeck$Tempo), 
       lwd = 2, 
       ylim = c(0, 0.020),
       type = "l", 
       col = "red")
# observed Popularity
plot(density(spotify$Popularity),
     lwd = 2, 
     ylim = c(0, 0.05),
     main="",
     xlab = "Popularity")
# imputed Popularity
points(density(spotify.hotdeck$Popularity), 
       lwd = 2, 
       ylim = c(0, 0.05),
       type = "l", 
       col = "red")
# observed Artist.Followers
plot(density(spotify$Artist.Followers),
     lwd = 2, 
     main="",
     xlab = "Artist.Followers")
# imputed Artist.Followers
points(density(spotify.hotdeck$Artist.Followers), 
       lwd = 2, 
       #ylim = c(0, 0.05),
       type = "l", 
       col = "red")
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

The next imputation method that was used was hotdecking, which uses information that is available in the data (within other variables) to “fill in”
information that is missing for a given predictor. Using hotdecking, the average percent change in coefficients was 149.46%, which was significantly higher than prior methods. It seem that the main issue is the Artist.Followers coefficient, which increased by > 500%. SEs decreased for all variables, with an overall average decrease of 3.35%.\
As expected, the density plots of the hotdecking imputations almost perfectly match that of the observed data. 
Interestingly, Tempo now became statistically significant at the 0.05 alpha level. 

### 6. Regression Imputation

```r
# copy dataset with missingness
spotify.reg <- spotify.miss
##  linear regression
  # Popularity 
  Popularity.imp = mice.impute.norm.predict(spotify.reg$Popularity, !is.na(spotify.reg$Popularity), spotify.reg$Duration.ms)
  spotify.reg$Popularity[is.na(spotify.reg$Popularity)] = Popularity.imp
  # Tempo
  Tempo.imp = mice.impute.norm.predict(spotify.reg$Tempo, !is.na(spotify.reg$Tempo), spotify.reg$Duration.ms)
  spotify.reg$Tempo[is.na(spotify.reg$Tempo)] = Tempo.imp
  # Artist.Followers
  Artist.Followers.imp = mice.impute.norm.predict(spotify.reg$Artist.Followers, !is.na(spotify.reg$Artist.Followers), spotify.reg$Duration.ms)
  spotify.reg$Artist.Followers[is.na(spotify.reg$Artist.Followers)] = Artist.Followers.imp
  # Number.of.Times.Charted
  Number.of.Times.Charted.imp = mice.impute.norm.predict(spotify.reg$Number.of.Times.Charted, !is.na(spotify.reg$Number.of.Times.Charted), spotify.reg$Duration.ms)
  spotify.reg$Number.of.Times.Charted[is.na(spotify.reg$Number.of.Times.Charted)] = Number.of.Times.Charted.imp
## logistic for categorical variable Feature
    # logreg - manual
    Ry = as.numeric(!is.na(spotify.reg$Feature))
    spotify.reg.cc = spotify.reg[Ry == 1, ]
    spotify.reg.dropped = spotify.reg[Ry == 0, ]
    # Now build the logistic model:
    mylogit <- glm(Feature ~ Popularity + Tempo + scale(Artist.Followers) + scale(Duration.ms) + Number.of.Times.Charted, data = spotify.reg.cc, family = "binomial")
    # We now use the model to predict the missing
    y.imp <- predict(mylogit, newdata = spotify.reg.dropped, type = "response")
    # Note this returns the probability so we need to convert it to binary response before imputing it:
    # without noise
    spotify.reg$Feature[Ry == 0] = round(y.imp,0)
```


```r
# regression analysis 
set.seed(123)
reg.reg <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + Feature + scale(Duration.ms), data=spotify.reg)
summary(reg.reg)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     Feature + scale(Duration.ms), data = spotify.reg)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.235  -8.522  -5.627   1.660 128.369 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -1.41884    2.51468  -0.564    0.573    
## Popularity               0.21048    0.02534   8.307   <2e-16 ***
## Tempo                   -0.02154    0.01384  -1.556    0.120    
## scale(Artist.Followers) -0.11326    0.39842  -0.284    0.776    
## Feature1                -0.25266    0.96862  -0.261    0.794    
## scale(Duration.ms)       0.25489    0.39222   0.650    0.516    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.15 on 1539 degrees of freedom
## Multiple R-squared:  0.04601,	Adjusted R-squared:  0.04291 
## F-statistic: 14.84 on 5 and 1539 DF,  p-value: 3.001e-14
```

```r
# total change 
sum(abs(reg$coef - reg.reg$coef)) 
```

```
## [1] 2.390362
```

```r
# average percent change in coefficients
mean((abs(reg$coef - reg.reg$coef))/abs(reg$coef)) 
```

```
## [1] 0.3704315
```

```r
# SE
reg.reg.sd <- coef(summary(reg.reg))[, "Std. Error"]
reg.sd
```

```
##             (Intercept)              Popularity                   Tempo 
##              2.53505871              0.02592200              0.01373744 
## scale(Artist.Followers)     as.factor(Feature)1      scale(Duration.ms) 
##              0.42140532              0.98574344              0.41316218
```

```r
reg.reg.sd
```

```
##             (Intercept)              Popularity                   Tempo 
##              2.51467762              0.02533755              0.01384134 
## scale(Artist.Followers)                Feature1      scale(Duration.ms) 
##              0.39841638              0.96861508              0.39221823
```

```r
# average percent change in SE - absolute terms
mean(abs(reg.reg.sd - reg.sd)/abs(reg.sd)) 
```

```
## [1] 0.02679517
```
Using regression imputation (without noise), the average percent change in coefficients was 37.04%. 
SEs decreased for all variables except Tempo; SEs had an overall average percent change of 2.68%. 
Popularity was the only statistically significant variable, at the 0.05 alpha level. 

### 7. Regression imputation with noise on all variables (numerical, dichotomous and multinomial).

```r
# copy dataset with missingness
spotify.noise <- spotify.miss
## linear regression for numerical variables 
# norm.nob for noise 
  # Popularity 
  Popularity.imp = mice.impute.norm.nob(spotify.noise$Popularity, !is.na(spotify.noise$Popularity), spotify$Duration.ms)
  spotify.noise$Popularity[is.na(spotify.noise$Popularity)] = Popularity.imp
  # Tempo
  Tempo.imp = mice.impute.norm.nob(spotify.noise$Tempo, !is.na(spotify.noise$Tempo), spotify$Duration.ms)
  spotify.noise$Tempo[is.na(spotify.noise$Tempo)] = Tempo.imp
  # Artist.Followers
  Artist.Followers.imp = mice.impute.norm.nob(spotify.noise$Artist.Followers, !is.na(spotify.noise$Artist.Followers), spotify$Duration.ms)
  spotify.noise$Artist.Followers[is.na(spotify.noise$Artist.Followers)] = Artist.Followers.imp
  # Number.of.Times.Charted
  Number.of.Times.Charted.imp = mice.impute.norm.nob(spotify.noise$Number.of.Times.Charted, !is.na(spotify.noise$Number.of.Times.Charted), spotify$Duration.ms)
  spotify.noise$Number.of.Times.Charted[is.na(spotify.noise$Number.of.Times.Charted)] = Number.of.Times.Charted.imp
## logistic regression for categorical variable Feature
  # logreg - manual
    Ry2 = as.numeric(!is.na(spotify.noise$Feature))
    spotify.noise.cc = spotify.noise[Ry == 1, ]
    spotify.noise.dropped = spotify.noise[Ry == 0, ]
    # Now build the logistic model:
    mylogit2 <- glm(Feature ~ Popularity + Tempo + scale(Artist.Followers) + Duration.ms + Number.of.Times.Charted, data = spotify.noise.cc, family = "binomial")
    # We now use the model to predict the missing
    y.imp2 <- predict(mylogit2, newdata = spotify.noise.dropped, type = "response")
    # Note this returns the probability so we need to convert it to binary response before imputing it:
    # with noise
    spotify.noise$Feature[Ry2 == 0] = rbinom(sum(Ry2==0), 1, y.imp2)
```


```r
# regression analysis 
set.seed(123)
reg.noise <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + Feature + scale(Duration.ms), data=spotify.noise)
summary(reg.noise)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     Feature + scale(Duration.ms), data = spotify.noise)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -47.138  -8.861  -6.095   3.654 128.049 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -0.11707    2.52041  -0.046   0.9630    
## Popularity               0.19852    0.02535   7.831 8.95e-15 ***
## Tempo                   -0.02446    0.01384  -1.768   0.0773 .  
## scale(Artist.Followers) -0.05984    0.41558  -0.144   0.8855    
## Feature1                -0.20739    0.97816  -0.212   0.8321    
## scale(Duration.ms)       0.26099    0.40802   0.640   0.5225    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.82 on 1539 degrees of freedom
## Multiple R-squared:  0.04186,	Adjusted R-squared:  0.03874 
## F-statistic: 13.45 on 5 and 1539 DF,  p-value: 7.403e-13
```

```r
# total change 
sum(abs(reg$coef - reg.noise$coef)) 
```

```
## [1] 3.702263
```

```r
# average percent change in coefficients
mean(abs(reg$coef - reg.noise$coef)/abs(reg$coef)) 
```

```
## [1] 0.3502226
```

```r
# SE
reg.noise.sd <- coef(summary(reg.noise))[, "Std. Error"]
# average percent change in SE
mean((reg.noise.sd - reg.sd)/(reg.sd)) 
```

```
## [1] -0.009101628
```

```r
# average percent change in SE - absolute terms 
mean(abs(reg.noise.sd - reg.sd)/abs(reg.sd)) 
```

```
## [1] 0.0114973
```


```r
# only numerical variables 
gg.cor.spotify.noise<- data.frame(spotify.noise$Number.of.Times.Charted, spotify.noise$Popularity, spotify.noise$Tempo, spotify.noise$Artist.Followers, spotify.noise$Duration.ms)
# correlations between variables in observed and imputed data-sets
r.noise <- cor(gg.cor.spotify.noise, use ="na.or.complete")
# plots 
```

Regression imputation with noise adds back in most of the variability that standard regression imputation removes. 
The average percent change in coefficients was 35.02%. 
The average percent change in SEs decreased by 0.91%, which is in line with the expectation of adding more variability bad to the model. 
Average % change in abolute terms, was 1.15% for SE. 


### 8. Multiple imputation

```r
# Create a missing_data object, then look at the data and the missing data patterns
mdf = missing_data.frame(spotify.miss)
summary(mdf)
```

```
##  Number.of.Times.Charted   Popularity         Tempo        Feature    
##  Min.   :  1.0           Min.   :  0.00   Min.   : 46.72   0   :1060  
##  1st Qu.:  1.0           1st Qu.: 65.00   1st Qu.: 98.01   1   : 321  
##  Median :  4.0           Median : 73.00   Median :122.02   NA's: 164  
##  Mean   : 10.6           Mean   : 69.96   Mean   :122.76              
##  3rd Qu.: 12.0           3rd Qu.: 80.00   3rd Qu.:143.03              
##  Max.   :142.0           Max.   :100.00   Max.   :205.27              
##  NA's   :142             NA's   :145      NA's   :146                 
##  Artist.Followers    Duration.ms    
##  Min.   :    4883   Min.   : 30133  
##  1st Qu.: 1890510   1st Qu.:169266  
##  Median : 6837946   Median :193591  
##  Mean   :14609912   Mean   :197941  
##  3rd Qu.:22683756   3rd Qu.:218902  
##  Max.   :83337783   Max.   :588139  
##  NA's   :156
```

```r
image(mdf)
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
hist(mdf) # Number.of.Times.Charted and Artist.Followers skewed, Popularity has outliers that are negative 
# missing data patterns
tabulate(mdf@patterns)
```

```
## [1] 792 146 164 142 145 156
```

```r
levels(mdf@patterns)
```

```
## [1] "nothing"                 "Tempo"                  
## [3] "Feature"                 "Number.of.Times.Charted"
## [5] "Popularity"              "Artist.Followers"
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-20-2.png)<!-- -->


```r
# check  data types and methods
show(mdf) 
```

```
## Object of class missing_data.frame with 1545 observations on 6 variables
## 
## There are 6 missing data patterns
## 
## Append '@patterns' to this missing_data.frame to access the corresponding pattern for every observation or perhaps use table()
## 
##                               type missing method  model
## Number.of.Times.Charted continuous     142    ppd linear
## Popularity              continuous     145    ppd linear
## Tempo                   continuous     146    ppd linear
## Feature                     binary     164    ppd  logit
## Artist.Followers        continuous     156    ppd linear
## Duration.ms             continuous       0   <NA>   <NA>
## 
##                           family     link transformation
## Number.of.Times.Charted gaussian identity    standardize
## Popularity              gaussian identity    standardize
## Tempo                   gaussian identity    standardize
## Feature                 binomial    logit           <NA>
## Artist.Followers        gaussian identity    standardize
## Duration.ms                 <NA>     <NA>    standardize
```

```r
# changing Number.of.Times.Charted, Artist.Followers  to type positive-contiguous
# does not make sense for observations these variables to be <= 0  
 mdf <- change(mdf, y = c("Number.of.Times.Charted", "Artist.Followers"), what = "type", to = "positive-continuous")
 show(mdf)
```

```
## Object of class missing_data.frame with 1545 observations on 6 variables
## 
## There are 6 missing data patterns
## 
## Append '@patterns' to this missing_data.frame to access the corresponding pattern for every observation or perhaps use table()
## 
##                                        type missing method  model
## Number.of.Times.Charted positive-continuous     142    ppd linear
## Popularity                       continuous     145    ppd linear
## Tempo                            continuous     146    ppd linear
## Feature                              binary     164    ppd  logit
## Artist.Followers        positive-continuous     156    ppd linear
## Duration.ms                      continuous       0   <NA>   <NA>
## 
##                           family     link transformation
## Number.of.Times.Charted gaussian identity            log
## Popularity              gaussian identity    standardize
## Tempo                   gaussian identity    standardize
## Feature                 binomial    logit           <NA>
## Artist.Followers        gaussian identity            log
## Duration.ms                 <NA>     <NA>    standardize
```






```r
par(mfrow = c(2, 3))
# traceplots 
# traceplot - number of times charted 
mean_Number.of.Times.Charted = converged[, , 1]
ts.plot(mean_Number.of.Times.Charted[,1], col=1)
lines(mean_Number.of.Times.Charted[,2], col= 2)
lines(mean_Number.of.Times.Charted[,3], col= 3)
lines(mean_Number.of.Times.Charted [,4], col= 4)
# traceplot - Popularity
mean_Popularity = converged[, , 2]
ts.plot(mean_Popularity[,1], col=1)
lines(mean_Popularity[,2], col= 2)
lines(mean_Popularity[,3], col= 3)
lines(mean_Popularity[,4], col= 4)
# traceplot - Tempo
mean_Tempo = converged[, , 3]
ts.plot(mean_Tempo[,1], col=1)
lines(mean_Tempo[,2], col= 2)
lines(mean_Tempo[,3], col= 3)
lines(mean_Tempo [,4], col= 4)
# traceplot - Feature
mean_Feature = converged[, , 4]
ts.plot(mean_Feature[,1], col=1)
lines(mean_Feature[,2], col= 2)
lines(mean_Feature[,3], col= 3)
lines(mean_Feature [,4], col= 4)
# traceplot - Artist.Followers
mean_Artist.Followers = converged[, , 5]
ts.plot(mean_Artist.Followers[,1], col=1)
lines(mean_Artist.Followers[,2], col= 2)
lines(mean_Artist.Followers[,3], col= 3)
lines(mean_Artist.Followers [,4], col= 4)
# traceplot - Duration.ms
mean_Duration.ms= converged[, , 5]
ts.plot(mean_Duration.ms[,1], col=1)
lines(mean_Duration.ms[,2], col= 2)
lines(mean_Duration.ms[,3], col= 3)
lines(mean_Duration.ms [,4], col= 4)
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Convergence looks good for all variables so I will not continue with more imputations. 

```r
# check r-hats
Rhats(imputations) 
```

```
## mean_Number.of.Times.Charted              mean_Popularity 
##                    0.9954599                    0.9944169 
##                   mean_Tempo                 mean_Feature 
##                    1.0107070                    0.9970437 
##        mean_Artist.Followers   sd_Number.of.Times.Charted 
##                    1.0027148                    0.9998957 
##                sd_Popularity                     sd_Tempo 
##                    0.9932590                    0.9950869 
##                   sd_Feature          sd_Artist.Followers 
##                    0.9971922                    0.9950410
```

R-hats are ~1 for most variables so I will not make further changes to the imputations. 

```r
# plot diagnostics 
plot(imputations)
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-26-1.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-26-2.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-26-3.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-26-4.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-26-5.png)<!-- -->


```r
# histograms of imputations 
hist(imputations)
```

![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-27-2.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-27-3.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-27-4.png)<!-- -->![](Spotify-missing-data-markdown_files/figure-html/unnamed-chunk-27-5.png)<!-- -->


```r
## run pool analysis 
set.seed(123, sample.kind = "Rounding")
```

```
## Warning in set.seed(123, sample.kind = "Rounding"): non-uniform 'Rounding'
## sampler used
```

```r
mipoolresult <- pool(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + as.factor(Feature) + scale(Duration.ms), imputations, m = 5)
display(mipoolresult)
```

```
## bayesglm(formula = Number.of.Times.Charted ~ Popularity + Tempo + 
##     scale(Artist.Followers) + as.factor(Feature) + scale(Duration.ms), 
##     data = imputations, m = 5)
##                         coef.est coef.se
## (Intercept)             -1.53     3.23  
## Popularity               0.23     0.03  
## Tempo                   -0.03     0.02  
## scale(Artist.Followers) -0.03     0.86  
## as.factor(Feature)1     -0.15     1.13  
## scale(Duration.ms)       0.24     0.48  
## n = 1539, k = 6
## residual deviance = 382210.9, null deviance = 405927.9 (difference = 23717.0)
## overdispersion parameter = 248.4
## residual sd is sqrt(overdispersion) = 15.76
```

```r
# average percent change in coefficients
mean(abs(reg$coef - coef(mipoolresult))/abs(reg$coef)) 
```

```
## [1] 0.411203
```

```r
# SEs
pool.sd <- coef(summary(mipoolresult))[, "Std. Error"]
# average percent change in SE
mean(abs(reg.sd - pool.sd)/abs(reg.sd)) 
```

```
## [1] 0.3519089
```

When checking data types, I decided to change the variables Number.of.Times.Charted and Artist.Followers to type positive-contiguous, as it would not not make sense for these observations to be <= 0. The original model families seemed appropriate. I then ran the multiple imputation with 5 chains and 60 iterations. The convergence traceplots looked reasonable for all variables so I decided to not continue with more imputations. R-hats were approximately 1 for all variables so I did not make further changes to the imputations. The diagnostic plots confirmed this decision.\
Multiple imputation with the MI library yielded a 41.12% average percent change in coefficients and a 35.19% average percent change in SE.
SEs appears to increase for most variables (Tempo, Artist.Followers, Feature, Duration.ms).\


### 9. kNN Imputation (VIM)

```r
# copy dataset with missingness
spotify.kNN <- spotify.miss

spotify.kNN <- kNN(spotify.kNN, k = 5, imp_var = FALSE)
```


```r
# comparing results 
# imputed data
set.seed(123)
reg.kNN <- lm(Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + Feature + scale(Duration.ms), data=spotify.kNN)
summary(reg.kNN)
```

```
## 
## Call:
## lm(formula = Number.of.Times.Charted ~ Popularity + Tempo + scale(Artist.Followers) + 
##     Feature + scale(Duration.ms), data = spotify.kNN)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.233  -8.201  -5.723   1.712 127.823 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -2.74493    2.48031  -1.107   0.2686    
## Popularity               0.24788    0.02496   9.930   <2e-16 ***
## Tempo                   -0.03442    0.01362  -2.526   0.0116 *  
## scale(Artist.Followers) -0.34473    0.40192  -0.858   0.3912    
## Feature1                -0.33644    0.95359  -0.353   0.7243    
## scale(Duration.ms)       0.18498    0.39286   0.471   0.6378    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.19 on 1539 degrees of freedom
## Multiple R-squared:  0.06556,	Adjusted R-squared:  0.06252 
## F-statistic: 21.59 on 5 and 1539 DF,  p-value: < 2.2e-16
```

```r
# total change 
sum(abs(reg$coef - reg.kNN$coef)) 
```

```
## [1] 1.269729
```

```r
# average percent change in coefficients
mean(abs(reg$coef - reg.kNN$coef)/abs(reg$coef)) 
```

```
## [1] 0.9671728
```

```r
# SE
kNN.sd <- coef(summary(reg.kNN))[, "Std. Error"] 
# average percent change in SE
mean(abs(kNN.sd - reg.sd)/abs(reg.sd)) 
```

```
## [1] 0.03247187
```

I was curious to see if the kNN method (which we didn't thoroughly discuss in class) would yield better results. The kNN imputation method uses the kNN algorithm to search the entire dataset for the k number of the most similar cases, or neighbors, that show the same patterns as the row with the missing data.
The average percent change in coefficients was 96.72% and the average percent change in SEs was 3.25%. 




















