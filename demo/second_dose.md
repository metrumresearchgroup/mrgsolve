``` r
library(mrgsolve)
library(dplyr)
```

Use the house model

``` r
mod <- mrgsolve:::house() %>% update(delta=0.1) %>% Req(CP)
```

The default time grid

``` r
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  mrgsim %>%
  plot
```

![](img/second_dose-unnamed-chunk-4-1.png)<!-- -->

We can start at 24 and end at 48; but this doesn't quite look right; we still get the dose at `time=0`

``` r
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  mrgsim(start=24, end=48) %>%
  plot
```

![](img/second_dose-unnamed-chunk-5-1.png)<!-- -->

Drop the dose records from the output

``` r
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  obsonly %>%
  mrgsim(start=24, end=48) %>%
  plot
```

![](img/second_dose-unnamed-chunk-6-1.png)<!-- -->

Another way to do it: set `end=-1` to get rid of that simulation time grid and the give `mrgsolve` an ad-hoc vector of times (`add`) to output.

You will still have to drop the dosing record to avoid seeing that record in the output.

``` r
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  obsonly %>%
  mrgsim(end=-1, add=seq(24,48,0.1)) %>%
  plot
```

![](img/second_dose-unnamed-chunk-7-1.png)<!-- -->

Via data set

``` r
data <- expand.ev(time=seq(0,48,1), amt=0,evid=0) %>% mutate(ID=1)
data <- bind_rows(data, data %>% mutate(ID=2))
doses <- data %>% filter(time==0) %>% mutate(amt=100,evid=1,cmt=1)
data <- bind_rows(doses,data) %>% arrange(ID,time,evid)
data <- data %>% filter(ID==1 & time <= 24 | (ID==2 & time >= 24 | evid==1))
```

``` r
mod %>% 
  data_set(data) %>%
  obsonly %>%
  mrgsim() %>%
  plot(CP~time|ID)
```

![](img/second_dose-unnamed-chunk-9-1.png)<!-- -->

``` r
sessionInfo()
```

    ## R version 3.2.3 (2015-12-10)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: OS X 10.9.5 (Mavericks)
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     grDevices utils     datasets  graphics  methods   base     
    ## 
    ## other attached packages:
    ## [1] knitr_1.12.3         dplyr_0.4.3          mrgsolve_0.5.11.9005
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.3     lattice_0.20-33 digest_0.6.9    assertthat_0.1 
    ##  [5] grid_3.2.3      R6_2.1.2        DBI_0.3.1       formatR_1.2.1  
    ##  [9] magrittr_1.5    evaluate_0.8    stringi_1.0-1   lazyeval_0.1.10
    ## [13] rmarkdown_0.9.2 tools_3.2.3     stringr_1.0.0   yaml_2.1.13    
    ## [17] parallel_3.2.3  htmltools_0.3
