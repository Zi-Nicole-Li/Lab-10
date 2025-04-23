Lab 10 - Grading the professor, Pt. 2
================
Zi Li
4/23

## Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

## Exercise 1- Part 1 & 2.

``` r
library(tidyverse)
library(broom)
#> Warning: package 'broom' was built under R version 4.4.3
library(openintro)

# Part 1
m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
summary(m_bty)$r.squared
```

    ## [1] 0.03502226

``` r
summary(m_bty)$adj.r.squared 
```

    ## [1] 0.03292903

``` r
# Intercept (3.88) is the expected course evaluation score for a female professor with a beauty score of 0 (since female is the reference group).
# slope (0.067):For every one-point increase in beauty rating, the professor's evaluation score is predicted to increase by about 0.067 points.
#only about 3.5% of the variance in professor evaluation scores is explained by beauty rating alone.


# Part 2: Multiple linear regression with gender
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

``` r
summary(m_bty_gen)$r.squared
```

    ## [1] 0.05912279

``` r
summary(m_bty_gen)$adj.r.squared # Adding gender increased the adjusted R². 
```

    ## [1] 0.05503202

``` r
coef(m_bty)["bty_avg"]
```

    ##    bty_avg 
    ## 0.06663704

``` r
coef(m_bty_gen)["bty_avg"] # The slope increased slightly when gender was added, meaning beauty’s effect got a bit stronger when accounting for gender. 
```

    ##    bty_avg 
    ## 0.07415537

``` r
# Equation for male professors (male is the reference group)
coef(m_bty_gen) # For two professors who received the same beauty rating, Male professors tend to get higher rating. 
```

    ## (Intercept)     bty_avg  gendermale 
    ##  3.74733824  0.07415537  0.17238955

``` r
# beauty influences evaluation scores pretty much equally across genders, but male professors seems to have a higher baseline.


# with Rank
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

``` r
# bty_avg (0.068): Each additional beauty point increases score by 0.068 points, holding rank constant.
# ranktenure_track (-0.161): Tenure-track professors score 0.161 points lower than non-tenure-track, holding beauty constant.
# ranktenured (-0.126): Tenured professors score 0.126 points lower than non-tenure-track, holding beauty constant.
```

## Exercise 2-Part 3

``` r
# I would expect ethnicity to be the worst predictor. 

m_ethnicity <- lm(score ~ ethnicity, data = evals)
summary(m_ethnicity) # P=0.1; nice, ethnicity it is. 
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8912 -0.3816  0.1088  0.4088  0.9281 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.07187    0.06786  60.003   <2e-16 ***
    ## ethnicitynot minority  0.11935    0.07310   1.633    0.103    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5429 on 461 degrees of freedom
    ## Multiple R-squared:  0.005749,   Adjusted R-squared:  0.003593 
    ## F-statistic: 2.666 on 1 and 461 DF,  p-value: 0.1032

``` r
# cls_did_eval should be exclude; if we wanna include cls_perc_eval and cls_students. Due to the multicollinearity, we need make sure our predictor shouldn't be too correlated. 

full_model <- lm(score ~ rank + ethnicity + gender + language + age +
                   cls_perc_eval + cls_students + cls_level + cls_profs +
                   cls_credits + bty_avg, data = evals)
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following objects are masked from 'package:openintro':
    ## 
    ##     housing, mammals

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
step_model <- stepAIC(full_model, direction = "backward", k = log(nrow(evals)))
```

    ## Start:  AIC=-567.87
    ## score ~ rank + ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_level + cls_profs + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - rank           2    0.4325 114.74 -578.39
    ## - cls_profs      1    0.0071 114.31 -573.98
    ## - cls_level      1    0.0288 114.34 -573.89
    ## - language       1    0.3501 114.66 -572.59
    ## - cls_students   1    0.3923 114.70 -572.42
    ## - age            1    1.1818 115.49 -569.24
    ## - ethnicity      1    1.4771 115.78 -568.06
    ## <none>                       114.31 -567.87
    ## - gender         1    3.0515 117.36 -561.81
    ## - cls_perc_eval  1    3.4284 117.74 -560.32
    ## - bty_avg        1    3.4287 117.74 -560.32
    ## - cls_credits    1    4.8017 119.11 -554.95
    ## 
    ## Step:  AIC=-578.39
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_level + cls_profs + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_profs      1    0.0103 114.75 -584.49
    ## - cls_level      1    0.0173 114.76 -584.46
    ## - cls_students   1    0.3645 115.11 -583.06
    ## - language       1    0.5568 115.30 -582.29
    ## - age            1    0.8918 115.63 -580.95
    ## <none>                       114.74 -578.39
    ## - ethnicity      1    1.7046 116.44 -577.70
    ## - gender         1    3.1469 117.89 -572.00
    ## - cls_perc_eval  1    3.5245 118.27 -570.52
    ## - bty_avg        1    3.5642 118.31 -570.37
    ## - cls_credits    1    5.6754 120.42 -562.18
    ## 
    ## Step:  AIC=-584.49
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_level + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_level      1    0.0162 114.77 -590.56
    ## - cls_students   1    0.3731 115.12 -589.13
    ## - language       1    0.5552 115.31 -588.39
    ## - age            1    0.8964 115.65 -587.03
    ## <none>                       114.75 -584.49
    ## - ethnicity      1    1.8229 116.57 -583.33
    ## - gender         1    3.1375 117.89 -578.14
    ## - cls_perc_eval  1    3.5166 118.27 -576.65
    ## - bty_avg        1    3.5547 118.31 -576.50
    ## - cls_credits    1    5.8278 120.58 -567.69
    ## 
    ## Step:  AIC=-590.56
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_students   1    0.3569 115.12 -595.26
    ## - language       1    0.5390 115.31 -594.53
    ## - age            1    0.8828 115.65 -593.15
    ## <none>                       114.77 -590.56
    ## - ethnicity      1    1.8948 116.66 -589.12
    ## - gender         1    3.1222 117.89 -584.27
    ## - cls_perc_eval  1    3.5266 118.29 -582.69
    ## - bty_avg        1    3.5461 118.31 -582.61
    ## - cls_credits    1    6.2703 121.04 -572.07
    ## 
    ## Step:  AIC=-595.26
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - language       1    0.6192 115.74 -598.92
    ## - age            1    0.9342 116.06 -597.66
    ## <none>                       115.12 -595.26
    ## - ethnicity      1    1.8997 117.02 -593.82
    ## - cls_perc_eval  1    3.1769 118.30 -588.80
    ## - gender         1    3.4709 118.59 -587.65
    ## - bty_avg        1    4.0096 119.13 -585.55
    ## - cls_credits    1    6.1046 121.23 -577.48
    ## 
    ## Step:  AIC=-598.92
    ## score ~ ethnicity + gender + age + cls_perc_eval + cls_credits + 
    ##     bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - age            1    0.9645 116.71 -601.21
    ## <none>                       115.74 -598.92
    ## - ethnicity      1    2.9096 118.65 -593.56
    ## - cls_perc_eval  1    3.1928 118.94 -592.46
    ## - gender         1    3.3804 119.12 -591.73
    ## - bty_avg        1    3.9968 119.74 -589.34
    ## - cls_credits    1    6.5916 122.33 -579.41
    ## 
    ## Step:  AIC=-601.21
    ## score ~ ethnicity + gender + cls_perc_eval + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## <none>                       116.71 -601.21
    ## - gender         1    2.7053 119.41 -596.74
    ## - ethnicity      1    2.7477 119.46 -596.58
    ## - cls_perc_eval  1    3.3244 120.03 -594.35
    ## - bty_avg        1    5.5674 122.28 -585.77
    ## - cls_credits    1    6.8241 123.53 -581.04

``` r
summary(step_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + cls_perc_eval + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8857 -0.3294  0.1066  0.3774  1.0540 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3.137381   0.146450  21.423  < 2e-16 ***
    ## ethnicitynot minority 0.233794   0.071275   3.280 0.001117 ** 
    ## gendermale            0.157832   0.048493   3.255 0.001219 ** 
    ## cls_perc_eval         0.005208   0.001443   3.608 0.000343 ***
    ## cls_creditsone credit 0.541067   0.104669   5.169 3.52e-07 ***
    ## bty_avg               0.073644   0.015773   4.669 3.98e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5053 on 457 degrees of freedom
    ## Multiple R-squared:  0.146,  Adjusted R-squared:  0.1366 
    ## F-statistic: 15.62 on 5 and 457 DF,  p-value: 3.338e-14

``` r
formula(step_model)
```

    ## score ~ ethnicity + gender + cls_perc_eval + cls_credits + bty_avg

``` r
# final equation would be : score ~ ethnicity(not minority) + gender + cls_perc_eval + cls_credits + bty_avg
# based on my result, Credits (one-credit course)   have a slop of +0.54, One-credit courses are rated 0.54 points higher than multi-credit ones. This is a large effect.

# Although my results are fairly reliable, I feel that more analysis is needed, such as finding causal effects, etc., if they are to be generalized to a larger population.
```
