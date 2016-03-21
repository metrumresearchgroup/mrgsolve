``` r
library(mrgsolve)
```

Use the house model

``` r
code <- '
$GLOBAL

$PARAM CL=1, VC=5, F1 = 1, D1 = 5, R1=10

$CMT CENT

$MAIN
F_CENT = F1;
D_CENT = D1;
R_CENT = R1;

$ODE
dxdt_CENT  = -(CL/VC)*CENT;

'

mod <- mread("infusion", tempdir(), code)
```

    ## Compiling infusion.cpp.cpp ...

    ## done.

    ## Loading: infusion6e1140057cde.so

Run with `rate=-1`: `F` and `amt` determine duration

``` r
data <- expand.ev(amt=100,F1=seq(0.1,1,0.1),rate=-1)

mod %>%
  data_set(data) %>%
  mrgsim(end=36) %>% 
  plot
```

![](img/unnamed-chunk-4-1.png)<!-- -->

Run with `rate=-2`: duration is set by `D`

``` r
data <- expand.ev(amt=100,F1=seq(0.1,1,0.1),rate=-2)

mod %>%
  data_set(data) %>%
  mrgsim(end=36) %>% 
  plot
```

![](img/unnamed-chunk-5-1.png)<!-- -->
