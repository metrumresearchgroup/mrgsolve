__Please see the latest release__: [v0.5.11](https://github.com/metrumresearchgroup/mrgsolve/releases/tag/v0.5.11)

# Since 0.5.11
## Bugs fixed
* Added missing example model specification files (popExample, viralExample, others)
* Added `mindt` attribute to `mrgmod` objects with default value of `.Machine$double.eps*10`. When the problem includes an infusion, the calculated end of the infusion 
may come too close to another record.  Usually the solver will fail with the message `DLSODA- TOUT(=R1) too close to T(=R2) to start integration.`.  To fix this, set `mindt` to be greater than zero but small ... maybe 1E-12.  When `mindt` is greater than zero and `tto - tfrom` (the times of two adjacent records) is less than `mindt`, `mrgsolve` will set `tto` equal to `tfrom` ([issue 9](../issues/9)).
* `zero.re` didn't properly update the `$SIGMA` list when one matrix was named and another was unnamed.  This has been fixed.  ([issue 16](../issues/16))
* Fixed a bug where infusions with `ss`=1 caused `mrgsolve`/`R` to crash when the infusion duration was equal to or some multiple of the dosing interval. ([issue 19](../issues/19))
* Fixed a bug where setting `F_CENT` to zero gave undefined behavior.  `mrgsolve` will issue an error if `F_CMT` is set to zero and the `ss` flag is set to 1. ([issue 22](../issues/16))


# New features
* New aliases available for setting bioavailability, lag time and infusion duration and rate.  For a compartment called `DEPOT` use: `F_DEPOT` (bioavailability), `ALAG_DEPOT` (dosing lag time), `D_DEPOT` (infusion duration), and / or `R_DEPOT` (infusion rate).  
* Added *slightly* more informative messages when `DLSODA` fails, including clear identification if the value of `istate`, which is 2 when the solver succeeds and negative when the solver fails. 
* Added `labels` and `prefix` options to `$OMEGA` and `$SIGMA`.  These allow descriptive aliases for ETAs ... e.g. using `ETA_CL` rather than `ETA(1)`.  ([issue 15](../issues/15))
* Added `dplyr::slice` method for `mrgsims` objects ([issue 11](../issues/11))

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
