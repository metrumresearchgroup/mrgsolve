__Please see the latest release__: [v0.6.1](https://github.com/metrumresearchgroup/mrgsolve/releases/tag/v0.6.1)

# Since 0.6.1

## Features
* Added `as_data_set` to convert one or more event objects into a data frame that can be passed to `data_set`.  Does something similar to `expand.ev`, but more control.
* For special column names (`time`, `amt`, `rate`, `evid`, `ii`, `addl` ,`ss`, `cmt`) either lower case or upper case names are recognized.  The determination is made on the `time` / `TIME` column (always required when using a data set).  If `time` `mrgsolve` will continue looking for lower case names; if `TIME` it will look for upper case names.  A warning is issued in case both upper and lower case names are included.
* Added `$PLUGIN` to let users extend their model specification file.  Valid plugins include `simeta`, `Rcpp`, `RcppArmadillo`, and `BH`.  When a plugin is used, `mrgsolve` will link back the the appropriate package and possibly include appropriate header files when compiling the model.  For example, `simeta` will link back to `mrgsolve` and `RcppArmadillo` and allow the modeler to simulate a new set of `ETA`s.  Use `Rcpp`  plugin to simulate random variates from common distibutions in `R`(e.g. `rnorm`, `rexp` etc ... ).  

## Bugs fixed
* Fixed issue with `ev` where no rows were returned if `amt` wasn't supplied ([issue 44](../../issues/44)).
* Shortened the path for both the shared object and the name of the `.cpp.cpp` file when compiling.
* Fixed bug in `touch_funs` when large number (`> 25`) of ETAs in the model ([issue 68](../../issues/68)).

## Important changes
* When using `$PKMODEL` with `ncmt=2` and `depot=FALSE`, the default PK parameters are `CL`, `V1` (central volume), `Q`, `V2` (peripheral volume).  This is a change where the previous volumes were `V2` (central) and `V3` (peripheral).
* `$CAPTURE` now saves output items to slots in `std::vector<double>`, rather than `std::map<std::string,double>`.  We've known for a while that the `std::map` wasn't very efficient especially with large simulations.  Currently, items in `$TABLE` are still saved into `std::map` with `table()` macro.  The plan going forward is to eliminate that `table` `map` and force output variables into `$CAPTURE`.
* Due to major changes to `dplyr`, now requiring `dplyr >= 0.5.0` ([issue 69](../../issues/69))
* The `data` slot in `mrgsims` objects is now `data.frame`
* The `knobs` function and `plot` method has been re-written.  Overall behavior for most applications should be the same.

## Under the hood
* `C++` symbols for model functions are now stored in the model object (`funs` slot)
* The status of the model object (function names and compile status) can be checked with `mrgsolve:::funset(mod)`
* A model is considered to be loaded and ready to go if all functions in `funs` can be found with `is.loaded`
* Model shared objects are still stored within the `soloc` directory (by default `tempdir()`), but `mrgsolve` will create a subdirectory structure to organize compilation artifacts.  The outer directory is keyed based on the current `mrgsolve` version number and the computer platform.  Inner directories are based on the model name (`model(mod)`).  
* A source file is created based on the `model` name and the shared object is created based on that name.  If the compilation is successful, the shared object (`.so` on mac/unix, `.dll` on Windows) is copied to a `.so` or `.dll` file with a unique stem (e.g. `model2lj239wsfo.so`).  This unique shared object is loaded into the `R` process for use with the model.  
* Every time the model is rebuilt, the build directory is scanned for shared object files.  Excluding the main model shared object (unchanging name based on the model), old shared object files are deleted and, if currently loaded (`getLoadedDLLs()`), are attempted to be `dyn.unload`ed.
* Upon model rebuild (via `mread` or `mcode`), if there are no changes to the source `.cpp` file, the source is not overwritten.  In that case, `make` will not re-build the shared object.  Using the `preclean` argument will force re-compilation (see `R CMD SHLIB`).
* The header files `modelheader.h` and `mrgsolv.h` are no longer copied into the project directory.  But `CLINK_CPPFLAGS` environment variable is modlifed to include `<path-to-mrgsolve-package>/inst/base` so that these may be linked.
* The `R CMD SHLIB` build process always uses `intern=TRUE` so that output is suppressed on both `Windows` and `mac/unix`.  The user may still request to view build output with the `ignore.stdout` argument.


## Deprecated
* The entire `complog` system, including:
    * `comp_forget` a message is issued
    * `complog` no message is issued
* `trequest` argument to `mrgsim`


# Since 0.6.0

## Bugs fixed
* Fixed a bug when an infusion was attempted with `rate > 0` and `amt==0`.  Additionally, an error is generated when an infusion is attempted with zero `amt` ([issue 43](../../issues/43)).

# Since 0.5.12

## Bugs fixed
* Fixed a bug where infusion dosing events with `evid 4` were not properly implemented.  While investigating that issue, also found similar issue with `evid 4` infusions getting scheduled with `addl` ([issue 31](../../issues/31)).
* Removed NSE evaluation for arguments passed into  `ev` to avoid unintended issues in evaluation ([issue 29](../../issues/29)).

## New features
* Added `mcode` function as alternative to using `mread` when your model is written in an `R` string.  Note the order of the arguments:
first `model`, then `code`, then `project`.  `project` defaults to `tempdir`.  So the call is: `mod <- mcode("mymodel", code)`.  The
equivalent `mread` call is: `mod <- mread("mymodel", tempdir(),code)`.
* `carry.out()` and `Req()` now take `newname = oldname` as input.  Use this syntax in `carry.out` when you want to copy a column from the input data set into the simulated data set, changing the column to `newname` from `oldname`.  Use this syntax in `Req` when you want to change the names of compartments or output variables spcified in `$TABLE` / `$CAPTURE`. 
* Added `pkmodel` function for easy loading and simulating from 1- and 2-compartment models ([issue 39](../../issues/39)).
* Added new code block: `$PKMODEL` for simulating PK model with analytical solutions.  The main option for this block is `ncmt`, which picks the number of compartments for the pk model.  See `?PKMODEL` for more information and other options ([issue 34](../../issues/34)).

## Under the hood
* Added `code` attribute to `mrgmod` objects.  The actual source code stays with the model object. `see` was modified to look at `x@code` first when showing the model code.
* Added a tokenizer function (`get_tokens`), a wrapper for the boost tokenizer) to help checking the model specification file.
* Data items entered in `$FIXED` are now implemented as C++ preprocessor directives by default rather than `const double` variables.  Use `$SET fixed_type = "define"` or `$SET fixed_type = "const"` to select between the approaches.


# Since 0.5.11
## Bugs fixed
* Added missing example model specification files (popExample, viralExample, others)
* Added `mindt` attribute to `mrgmod` objects with default value of `.Machine$double.eps*10`. When the problem includes an infusion, the calculated end of the infusion
may come too close to another record.  Usually the solver will fail with the message `DLSODA- TOUT(=R1) too close to T(=R2) to start integration.`.  To fix this, set `mindt` to be greater than zero but small ... maybe 1E-12.  When `mindt` is greater than zero and `tto - tfrom` (the times of two adjacent records) is less than `mindt`, `mrgsolve` will set `tto` equal to `tfrom` ([issue 9](../../issues/9)).
* `zero.re` didn't properly update the `$SIGMA` list when one matrix was named and another was unnamed.  This has been fixed.  ([issue 16](../../issues/16))
* Fixed a bug where infusions with `ss`=1 caused `mrgsolve`/`R` to crash when the infusion duration was equal to or some multiple of the dosing interval. ([issue 19](../../issues/19))
* Fixed a bug where setting `F_CENT` to zero gave undefined behavior.  `mrgsolve` will issue an error if `F_CMT` is set to zero and the `ss` flag is set to 1. ([issue 22](../../issues/16))
* Fixed bug where dosing records with `evid=4` (reset the system and dose)  and `addl > 0` reset the system for all subsequent doses. Additional doses coming from records with `evid=4` will not do system reset. ([issue 23](../../issues/23))

## Important changes
* New arguments for `$NMXML` (see `?nmxml`) that are easier to understand and consistent with new prefixes and labels for `ETA` and `EPS`.  `name` argument is removed.  Use `tname` (to provide a prefix for `THETAs`), `oname` (to name the `OMEGA` matrix), and `sname` (to name the `SIGMA` matrix) instead.  In general, set `theta` to be `TRUE` to import `THETAs`, set `omega` to be `TRUE` to import `OMEGA`, and set `sigma` to be `TRUE` to import `SIGMA`.  Specifying character names `tname`, `oname`, and `sname` will imply `theta=TRUE`, `omega=TRUE`, and `sigma=TRUE`, respectively.


## New features
* New aliases available for setting bioavailability, lag time and infusion duration and rate.  For a compartment called `DEPOT` use: `F_DEPOT` (bioavailability), `ALAG_DEPOT` (dosing lag time), `D_DEPOT` (infusion duration), and / or `R_DEPOT` (infusion rate). ([issue 13](../../issues/13))
* Added *slightly* more informative messages when `DLSODA` fails, including clear identification if the value of `istate`, which is 2 when the solver succeeds and negative when the solver fails.
* Added `labels` and `prefix` options to `$OMEGA` and `$SIGMA`.  These allow descriptive aliases for ETAs ... e.g. using `ETA_CL` rather than `ETA(1)`.  ([issue 15](../../issues/15))
* Added `dplyr::slice` method for `mrgsims` objects ([issue 11](../../issues/11))
* New argument to `mread`: `quiet`.  Setting `quiet` to `TRUE` will prevent printing messages when `mread` is called.  The default is `getOptions("mrgsolve_mread_quiet",FALSE)` ... so you can call `options(mrgsolve_mread_quiet = TRUE)` to globally turn off messages from `mread`.

# Since 0.5.001

## Bugs Fixed
* Fixed paths for project (`project`) and shared object (`soloc`) so that "short" paths are used when compiling the model on Windows  platforms (info [issue 4](https://github.com/metrumresearchgroup/mrgsolve/issues/4) and [here](https://github.com/metrumresearchgroup/mrgsolve/wiki/Windows-issues#spaces-in-project-directory-path))
* Fixed a bug where bioavailability fraction was not accounted for when setting infusion duration in `$MAIN` with `rate=-2` (see [issue 3](https://github.com/metrumresearchgroup/mrgsolve/issues/3))
* `mrgsolve_example` prints a message telling the user to use `mrgmod` to read and compile the model.  The user should use `mread`.  The message has been updated. (info [issue 5](https://github.com/metrumresearchgroup/mrgsolve/issues/5))
* Fixed bug where parameters were not correctly read from first row of a data set when certain `recsort` options were selected ([issue 6](https://github.com/metrumresearchgroup/mrgsolve/issues/6))

## New Features
*  Added `$ADVAN2` and `$ADVAN4` for implementing one- and two-compartment PK models with analytical solutions rather than ODEs (thanks to contributions by Bill Gillespie and Charles Margossian)
* Added `soloc` attribute added to `mrgmod` and argument added to `mread`, giving user control over where the shared object is stored; by default it is in `tempdir()`.  `soloc` needs to be set to local directory when using `qapply`
* Added generics for various `dplyr_` functions so that `mrgsims` objects can be piped to `mutate`, `group_by`, `filter`, `summarise`, `do`, `select`, and `summarise.each` (it's a dot not an underscore).  So: `mod %>% mrgsim %>% mutate(group=1)`
* You can now set initial conditions though `idata`; for compartment `CMT`, include a column in `idata` called `CMT_0`
* Modified update policy for `$OMEGA` and `$SIGMA`: if all incoming matrices are unnamed and the signature matches the model object, the update will happen
* Added `mcRNG` function as alias to `base::RNGkind("L'Ecuyer-CMRG")`

## Important changes
* Changes to `recsort`: 1 and 2 will put explicit doses after observations at the same time, 3 and 4 will put explicit doses before observations at the same time.  2 and 4 will put doses scheduled through `addl` after observations at the same time; 1 and 3 put doses scheduled through `addl` before observations at the same tile.

## Under the hood
* Changed dosing lag time mechanism so that lag times are calculated as the simulation progresses and dose times in the simulated output are as in the input data
* Model shared objects are given random names every time the model is compiled and loaded
* Various fixes and expansions to documentation
* Changed `R` dependency to `>= 3.1.2`
* All `Ops` involving `mrgmod` objects are now deprecated
* `loadso` now returns the model object (invisibly)
* Compartment names are automatically removed from table map
* Model specification files in need of compilation are detected via md5sum
* Minimum of 2 sec wait time is required before re-compiling a model
* Added `init` method with signature `mrgmod`,`ANY`, with `ANY` getting coerced to `list`
