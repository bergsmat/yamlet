# Print Decorated Xtable

Prints a decorated xtable. Supplies a footnote. Experimental.

## Usage

``` r
# S3 method for class 'decorated_xtable'
print(x, ...)
```

## Arguments

- x:

  decorated

- ...:

  passed to other methods

## Value

character

## Examples

``` r
library(magrittr)
library(xtable)
set.seed(0)
x <- data.frame(
 auc = rnorm(4, mean = 2400, sd = 200),
 bmi = rnorm(4, mean = 20, sd = 5),
 gen = 0:1
)
x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
x %>% resolve
#>        auc      bmi    gen
#> 1 2652.591 22.07321 Female
#> 2 2334.753 12.30025   Male
#> 3 2665.960 15.35716 Female
#> 4 2654.486 18.52640   Male
x %>% resolve %>% xtable
#> \begin{table}[ht]
#> \centering
#> \begin{tabular}{rrrl}
#>   \hline
#>  & auc & bmi & gen \\ 
#>   \hline
#> 1 & 2652.59 & 22.07 & Female \\ 
#>   2 & 2334.75 & 12.30 & Male \\ 
#>   3 & 2665.96 & 15.36 & Female \\ 
#>   4 & 2654.49 & 18.53 & Male \\ 
#>    \hline
#> \end{tabular}
#> 
#> 
#> auc:\(\mathrm{\textrm{AUC}{}_{\textrm{\scriptsize 0-24}} \textrm{ } \textrm{(ng} {\cdot} \textrm{h/mL)}}\); bmi:\(\mathrm{\textrm{Body} \textrm{ } \textrm{Mass} \textrm{ } \textrm{Index} \textrm{ } \textrm{(kg/m}{}^{\textrm{\scriptsize 2}} \textrm{)}}\); gen:\(\mathrm{\textrm{Gender}}\)
#> \end{table}

```
