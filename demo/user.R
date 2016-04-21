##' ---
##' output:
##'    md_document:
##'      variant: markdown_github
##' ---

##' 
##' # A user-defined function
##' 
##' This  uses just `C++` for now.  
user <- '
#include <vector>
#include <numeric>

extern "C" { double myfun(std::vector<double>& a) {
  double b = accumulate(a.begin(), a.end(), 0.0);
    return(b);
  }
}
'

##' ## Just standard `R` procedure for compiling and loading

##' It seem like you could / should utilize the convenience of
##'  `sourceCpp` here.  But we need to know the name of the `dll` (see below).
##'  I wasn't sure how to do that.  So the example just
##'  follows the sort of thing I do in `mrgsolve`.

##' A function to build and load the user function
build <- function(code,stem,func) {
  cpp <- paste0(stem, ".cpp")
  cat(code, file=cpp)
  system(paste("R CMD SHLIB ",cpp))
  dyn.load(paste0(stem, .Platform$dynlib.ext))
  return(getNativeSymbolInfo(func,PACKAGE=stem))
}

#+
x <- build(user,"foo", "myfun")

##' Our dll is loaded
getLoadedDLLs()


##' The address of the user-function
x
x$address


##' 
##' 
##' # The main code base
##' 
##' Now, write the "code base" that will do everything except 
##' what the user must supply in the user-defined function .  This will
##' take in `x$address` as `SEXP`, use `R_ExternalPtrAddr` to 
##' create a pointer to my function.
##' 
##' The following mimics code in an R package.  Please use your imagination
##' here.  But I think the important thing here is that this large base
##' of code gets compiled once.  When different user functions are 
##' needed, they will get compiled and then passed into the package.
##' 
##' There is one function called `pkgfun` that does a bunch of cool 
##' stuff, but the user needs to supply a function to call.
##' 
##' ## A "package"
##' 

library(Rcpp)

package <- '
#include <Rcpp.h>
#include <Rinternals.h>

typedef double user_func(std::vector<double>& a);

// Like the user function, but embedded with the "package"
extern "C" { double house(std::vector<double>& a) {
    double b = accumulate(a.begin(), a.end(), 0.0);
    return(b);
  }
}


//[[Rcpp::export]]
Rcpp::List pkgfun(SEXP fun_,Rcpp::NumericVector input_, Rcpp::NumericVector call) {

  user_func* myfun = (user_func*) R_ExternalPtrAddr(fun_);
  std::vector<double> input = Rcpp::as<std::vector<double> >(input_);
   
  // Call the user-supplied function
  double result = myfun(input);
  
  // if call is 0, use myfun(input); if not, use house(input)
  for(int i=0; i < 30000; i++) {
    result = call[0]==0 ?  myfun(input) : house(input);
  }

  Rcpp::List ans;
  ans["result"] = result;
  return(ans);
}
'

##'  
##' ## Mimics building the package
##'  
sourceCpp(code=package)



##' 
##' ## Use the package
##' 
##' Generate some input
set.seed(220201)
a <- rnorm(1000)

##' Call the package code, passing in the user-supplied function
pkgfun(x$address, a, 0)
sum(a)

##' ## Another user-supplied function.  
##' 
##' Okay to have another `myfun` around. 
##' It will be in a different `so`.  But I usually still generate
##' a unique name for this.
user2 <- '

#include <vector>
#include <numeric>

extern "C" { double myfun(std::vector<double>& a) {
  double b = accumulate(a.begin(), a.end(), 0.0)/double(a.size());
    return(b);
  }
}
'

x2 <- build(user2,"foo2", "myfun")

##' Call the same function in the "package", but with a different 
##' user supplied function.
pkgfun(x2$address,a, 0)
mean(a)


##' 
##' 
##' # Benchmark against function "internal" to the package
##' 
library(rbenchmark)

##' 
##' Get the `house` function in the package
##' We don't __need__ to specify the `PACKAGE`; `R` will find it
##' But usually, specify `PACKAGE` to avoid confusion
##' 
h <- getNativeSymbolInfo("house")

##+
benchmark(pkgfun(x$address,a,0),
          pkgfun(h$address,a,0),
          pkgfun(h$address,a,1), 
          replications=1000, 
          columns=c("test", "replications","elapsed", "relative"))






