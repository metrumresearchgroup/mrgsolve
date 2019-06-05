# mrgsolve 0.9.1

- Fixed bug in TAD plugin where time of last dose never moved from default
- Added option in `mrgsolve_q` to call the standard mrgsolve simulation 
  rather than the streamlined function; a decision will have to be made about 
  whether or not the streamlined function will be retained
- Fixed a bug in the model parser where commented lines with (all) leading
  spaces were not handled properly #450
- Objects of class `valid_data_set` now retain the matrix attribute #448
- Block names are now coreced to upper case letters
- Change default for simcall to 0 in mrgsim_q
- Added recover argument to mread
- Add `@as_object` option to the following blocks: PARAM, INIT, CMT, 
  OMEGA, SIGMA; experimental 
- Allow multiple `ODE` blocks
- Add `mtime` function to `self` object for simpler mtime implementation
- Add `@param` block option for ODE blocks; use comma-separated name=value
  pairs to add to the parameter list from within ODE
- When the model filename ends in `.Rmd`, `mread` will expect blocks code
  to be inside Rmarkdown code chunks and will parse accordingly; this 
  is an experimental feature and is subject to change

# mrgsolve 0.9.0
- Added `logy` and `logbr` arguments to `plot.mrgsims` so results y-axis
outputs can easily be plotted on log scale
- release

# mrgsolve 0.8.12.9000

## New functions
- Added `numerics_only` function to drop non-numeric columns from 
the input data set after optionally converting logical columns
to integer
- Added `ev_rx` function to write dosing interventions in notation similar
to a prescription 'sig'

## New features
- Added `$PRED` block for models that don't utilize any compartments
- Added `pred1` to the internal model library (`modlib()`)
- Added `mrgsim_q` function for simulation from a model object 
with quicker turnaround time
- `mread` will take `soloc` and `project` arguments from `options()` as
`mrgsolve.soloc` and `mrgsolve.project`, respectively
- Added `output` argument to `mrgsim` so that e.g. data.frame can be returned, 
without creating the usual `mrgsims` object
- The directory name passed to the `soloc` argument of `mread` (or `mcode` or 
cache versions of both) will be created if it doesn't exist
- Added `pk2iv` model as convenience option
- Added `tinf` argument to `ev()` constructor function

## New behavior
- `time/TIME` is no longer required in a data set when `$PRED` is in use
- `cmt/CMT` is no longer required in a data set; a default value of 0 will 
be assigned in case it is missing and an error will continue to be generated
when dosing into an invalid compartment (0 is always an invalid index
for dosing compartment)
- An error will be generated if missing values (`NA`) are detected in an 
input data set with the following names: `ID`, `time/TIME`,
or any column that shares a name with an item in the parameter list
- An `index` argument was added to the `$NMXML` block to allow results
selection when multiple estimation blocks were used.  The new default 
is to use the last result.
- `modlib` is now able to function as a wrapper to `mread` for models in 
the model library so that, for example,  `modlib("pk1")` is equivalent to 
`mread("pk1", modlib())`.

## Bugs fixed
- Fixed bug related to record sort order #406

## Deprecated
- `drop.re` and `drop_re`; use `zero_re` instead

## Breaking changes
- An undocumented function called `report` was previously available to use in 
model code.  This function has been moved to a namespace and is now available
as `mrg::report`.  This feature continues to be undocumented.

# mrgsolve 0.8.12
- Minor changes to namespace for CRAN

# mrgsolve 0.8.11
- Internal release
- Removed the function `s` and replaced with `s_`; this was not a problem 
created by mrgsolve but rather by ggplot2, which calls `mgcv::s` via 
`geom_smooth` under certain circumstances

# mrgsolve 0.8.10.9015
- Re-configured the list of data coming from `as.list.mrgmod` so that
the names match the names that you would pass to `update.mrgmod`; also
added some items so that all updatable slots in the model object
are exported by calling `as.list` #354

# mrgsolve 0.8.10.9014
- Add OMEGA and SIGMA matrices to `as.list.mrgmod` output under the names
omat and smat, respectively
- It is no longer an error to attempt to update slots for OMEGA
and SIGMA matrices when there is no matrices in the slot as long
as the update is with an empty list or a matrix with zero rows
- Fix segmentation fault when both data set and idata set are used
in a simulation with certain mis-matches in the ID content
between the two #352

# mrgsolve 0.8.10.9013
- Removes more mtime code (wasn't in use)

# mrgsolve 0.8.10.9012
- Use `dplyr::filter_` for `filter_.ev`

# mrgsolve 0.8.10.9011
- mtime clear bug

# mrgsolve 0.8.10.9010
- Fixed bug in `filter.ev` method

# mrgsolve 0.8.10.9009
- Added `mutate_sims` and `filter_sims` that work on `mrgsims` (output)
objects, modifying the `data` slot and returning a modified `mrgsims` object
rather than `data.frame`
- Added `plot_sims` function that takes a data frame of simulated output
and generates a plot using the `mrgsims` method

# mrgsolve 0.8.10.9008
- Changed `id` argument to `ID` in `ev_rep` and `ev_seq`; a warning
is issued if `id` is used
- Added `data_qsim` for creating input data sets to use with `qsim`
- Fixed a bug where time after dose was not correctly
calculated for additional doses with a lag time #327
- Added `read_nmext` to read in nonmem model output from
the .ext file

# mrgsolve 0.8.10.9007
- Clean up and re-organize Rd files; more-logical grouping and
ordering

# mrgsolve 0.8.10.9006

- Stop importing assertthat and lazyeval

# mrgsolve 0.8.10.9005

## New Features

- Added better support for writing model files with any
extension.  For example, `mread("mymodel.txt")` will read
from the file `mymodel.txt` if it exists.  However, the default
behavior remains unchanged so that `mread("mymodel")` will
expect to find the model in the file `mymodel.cpp`.

## Bug Fixes

- Fixed bug where the MD5 value on the model file could not
be calculated under certain `project` path formulations #315

## Important changes

- The `realize_addl` function was re-factored to better
account for time-varying data items; more option are
provided for seeing where rows were inserted into the
data set and whether to make assumptions about other
data in those rows or not

- mrgsolve no longer utilizes any functions from the
XML package.  All previous functionality that depended
on XML now depends on xml2.  As such, xml2 is listed
under the Suggests dependency.

- When using `$NMXML`, the `$OMEGA` and `$SIGMA` matrices
are now loaded by default; see new default arguments
to `mrgsolve:::nmxml`

## Documentation
- Added `solversettings` help topic that identifies some of the
DLSODA inputs you can tweak
- Improved documentation for `update` method as well as
`mrgmod-class`

# mrgsolve 0.8.10.9003

## New Features
- Bioavailability specified in `$MAIN` is accounted for when simulating with
`qsim`; there is still no bioavailability adjustment for infusions or
lag times adjustments to doses

- Added capability to rename data items in `$CAPTURE`; also,
names are partially sanitized, removing parens and brackets.  
For example `$CAPTURE WT = WGT ETA(1) TVCL = THETA1`

- Added `qsim_df` function, returning data frame rather than
matrix

- Added `as.list` method for `mrgsims` objects

- Added `deep` argument for `as.list` method for `mrgmod` object;
it was taking a lot of time to return the function set, so now
you only get it if `deep = TRUE`

- Added `mrgsim` variant functions with explicit input requirements
written into the function name.  For example, call `mrgsim_e`
to simulate from an event object, `mrgsim_d` to simulate from
a data frame.  All of these functions are called by `mrgsim`.

- Added method so that event objects can be passed to `data_set`; also,
coercing event objects to `data_set` when passed in as `data`

- Added `all.equal.mrgmod` function to compare two
model objects. The function returns logical (only)

- Added `env_get_env` that always just returns the
model environment; it has identical result as
`env_get(mod, tolist = FALSE)`

- Change `mread_cache` and `mcode_cache` so that the
cache is invalidated when `preclean` argument is `TRUE`

## Bug Fixes
- Fixed bug preventing simulation with `qsim` with no event

## Functions removed
- `mrgsolve_example` and `mrgsolve_template`; these had been deprecated
previously with warning; use `modlib()` models instead

# mrgsolve 0.8.10.9002
- Added `mrgsim_df` function to return data frame rather than `mrgsims` object

# mrgsolve 0.8.10.9001

- The model environment is automatically imported as `_env` when
`mrgx` plugin is invoked

# mrgsolve 0.8.10
- Release to CRAN

# mrgsolve 0.8.9.9004
- Added `file` argument to `mread` to allow coding model
specification files with any extension.  The current
behavior continues to be assuming that the model
is in a `.cpp` file, but using the `file` argument
allows any file name for model specification.

- Added `nocb` argument to `mrgsim`.  If `nobc` is
`TRUE` (default), `mrgsim` continues to use
next observation carried forward to advance the system
when there are time-varying parameters (including covariates).  If `nocb` is
`FALSE`, `mrgsim` will use last observation carried forward (`locf`) to advance
the system when there are time-varying parameters.

# mrgsolve 0.8.9.9003
- Minor improvements to documentation

# mrgsolve 0.8.9.9002

## Bug Fix
- Fixed bug where deslist was created in the wrong order
- Fixed bug where infusion duration was incorrect when paired with non-zero lag
time (test added)
- Fixed bug where `self` object was not correctly updated for the
first record for an individual (#273)

## Important changes
- Add support for dosing records with both lag time and ss flag; an error
message will be generated if lag time is greater than ii or if lag time +
infusion duration is greater than ii.
- The behavior of `ev_assign` is changed so that the unique values of `evgroup`
are sorted prior to making event assignments.  Details about the new behavior
are now included in the R help topic.


# mrgsolve 0.8.6.9000

## Important changes
- The bioavailability parameter now gets updated  with each and every dose,
regardless of whether it was explicitly coded in the data set or implicitly
via `addl`.  The previous behavior had bioavailability parameter locked at the
value at the time the initiating dose was implemented
- Updated package dependency requirements.  Notably, mrgsolve now requires
`Rcpp >= 0.12.12` and `dplyr >= 0.7.1`

## New Features
- Dosing records with `ss=2` are recognized, allowing combining of
steady-state dosing regimens under linear kinetics (e.g. 10 mg QAM and 20 mg
QPM) (#221)
- Added function (`inventory`) that reconciles model parameters with names
in an object (e.g. a simulation data set) verify that required parameters can
be found in the data object.

## Bugs fixed
- PR #214 from @dastoor fixes compatibility issue with new `dplyr`
- Fixed bug in `deslist` implementation #222
- PR #238 fixes incorrect steady-state values when dose is associated with
lagtime (#239)

#  mrgsolve 0.8.4
- Reconfigure use of function pointer in unit tests

#  mrgsolve 0.8.3
- Bug fix that prevented installation on Solaris

#  mrgsolve 0.8.2
- The first release on CRAN

# mrgsolve 0.7.7

## New Authors
- Devin Pastoor is now listed as a contributor.  Thanks for all of your help!

## New Features
- Added automatic, on-demand output of time-after-dose (`tad`) in the simulated
output.  Use `mrgsim(tad=TRUE)`.
- Several new functions added to `$PLUGIN mrgx`, including `mrgx::get<T>` for
getting objects out of `$ENV` or a package namespace and `mrgx::mt_fun()` that
is just a function that you can assign when declaring `Rcpp::Function`.
- Added `object` argument to `idata_set` and `data_set` to get a `data.frame`
(or function to call that returns `data.frame`) out of `$ENV` to use for
simulation.

## Changes / additions
- Added `cmt` argument to `$PKMODEL`.  When `cmt` is set to a character vector
or a comma-separated string, `$PKMODEL` infers the number of compartments and
declares them in the model.  This means a separate `$CMT` block is not required
when using `$PKMODEL`.
- Added `cols` argument to `as_bmat` and `as_dmat` so that a character vector
of names can be specified (rather than regular expression) to select data for
creating matrix.
- The `preclean` argument now causes `unlink` to be called on the model build
directory.
- Added several functions to help work with `$ENV`: `ls_env`, `get_env`,
`re_eval_env`, `update_env`.
- When a dose is administered into a compartment that is off, the compartment
is now turned on and the dosing is allowed to proceed.  This is a change
from previous behavior, where an error was generated.


# mrgsolve 0.7.6

## Important changes
- The `table()` macro in `$TABLE` is now deprecated
(https://github.com/metrumresearchgroup/mrgsolve/pull/129).  To get derived
values into the simulated output, users should assign
to type `double` and list that variable name in `$CAPTURE`.  See also the
`capture` typedef introduced below.
- The `mrgx` plugin was completely removed.  
- Parameter updates via `param` method with signature `missing` will check
names of input parameters against names of existing parameters.  An error is
generated if a user attempts to update a parameter that doesn't exist.  Note
that this does not apply for the `param` method with signature `list` (#144).
- The git repository was re-organized so that the package lives in the
base directory (PR #171).  

## Features
- Added `@` macros for indicating block options in model specification file.
- Added `qsim` function for quick(er) simulation runs with just one parameter
set.  
- Added `recmatrix` that creates matrix simulation template for `qsim`.
- Added `mrgsolve:::render` to create a document with overview of model
contents.  Methods for both `mrgmod` objects and `character` strings pointing
to a model file.
- Use `mrgsolve:::details` to extract model annotation.
- Added `capture` typedef in the model specification file.  Variables that are
type `capture` are doubles and are automatically appended to `$CAPTURE`.  
The `capture` typedef is not allowed in `$ODE` and probably should be reserved
for `$TABLE`.
- `simeta` is available in `$MAIN` and `simeps` is available in `$TABLE` by
default, no `$PLUGIN` is required.
- Better support for including `R` objects in the model via `$ENV` (#158).
- Added `assign_ev` function to help build simulation data sets from event
objects (https://github.com/metrumresearchgroup/mrgsolve/pull/164).
- Added `as_data_frame` method from the `tibble` package (#166).
- When annotating model blocks, mrgsolve takes the __last__ parens item  as
the "units" and the __last__ bracketed item as "options"
- Added `$` operator for `mrgmod` objects to return the value of a parameter.
- Added `mread_cache` and `mcode_cache` functions to build and cache a model
(#143).

## Bugs fixed
- Fixed documentation issue in `PKMODEL`.  The volumes for two-compartment
model with no depot should be `V1`/`V2`.
- Fixed bug in `knobs` where output column names are malformed when a user
`$CAPTURE`s a parameter that is also being tweaked as a knob.
- Fixed bug in annotated model specification when multiple unit or option
specifications are made.

## Under the hood
- User-declared `double/int/bool` in `$MAIN`, `$ODE`, `$TABLE` are kept in
unnamed namespace and are local to the file.
- Started to re-organize the `.R` files.
- `mrgsolve:::details` returns a data frame of information regardless of
whether the model was annotated or not (#165).
- `mrgsolve::details` has additional arguments to help control output.
- Removed `pkevent` class; all records are `datarecord`.

# mrgsolve 0.7.5

## Features

- Added annotated code blocks for `$PARAM`, `$FIXED`, `$THETA`, `$CMT`,
`$INIT`, and `$VCMT`. (#107)
- `mrgsolve:::house()` model re-coded as an annotated model.
- Re-implemented `$ENV` to allow users to create `R` objects that can be used
at certain points
when parsing the model. (#115)
- Added `>>` signifier to code blocks that allow options; `>>` at the
beginning of the line indicates that the `name=value` statements that follow
are to be parsed as block options.
- Added `object` argument for the following blocks: `$PARAM`, `$OMEGA`,
`$SIGMA`, `$FIXED`, `$CMT`.  When `object` is set to a character string
naming an object in `$ENV`, that object will be used to form the output from
the block.

## Bugs fixed

- Fixed a bug which caused simulation run to hang when implementing a dose
with a __very__ small lag time. (#109)
- Fixed a bug where `valid.numericlist` wasn't returning `FALSE` for
improperly-formed objects.

## Under the hood

- Now using an `environment` to collect objects when parsing the model
specification file.
- Some small changes to `C++` code that calculates compartment amounts for
closed form one- and two-compartment models resulting in faster simulation runs.

# mrgsolve 0.7.4
- The `modmrg` package was discontinued.  All of the pre-coded models are
now available in mrgsolve.  Simply call `mread` with the model stem (e.g.
`pk1cmt`, `irm3`, etc ...) and call `modlib()` as the `project` argument.  
For example: `mod <- mread("emax", modlib())` will compile the `emax` model
and return the model object.

# mrgsolve 0.7.3
- Tests re-configured
- Fixed issue with record sorting for lagged doses when using full `data_set
`and `obsaug=TRUE` (#102)
- Fixed issue where `idata_set` wasn't handled properly when it was passed
in as `tbl` (#100)


# mrgsolve 0.7.2
- Addressed an issue where model compilation on `Windows` systems failed when
certain symbol names were used in the model (#97).  In this release, a
`dllname-win.def` file is created in `soloc` to export only the functions that
mrgsolve needs to use.  This is *only* relevant to `Windows` platform.
- Added a check on the `project` argument to `mread`: if newline(s) are found,
an error is generated and the user is prompted to use `mcode` instead.
- Several changes under the hood
(https://github.com/metrumresearchgroup/mrgsolve/pull/99)


# mrgsolve 0.7.1
- Fixed bug where requested columns were not properly named in certain
circumstances (#86).

# mrgsolve 0.7.0
- Revert back to previous behavior where `cwd` to `soloc` is not required to
build the model.  This was only required on `Windows` systems where there was
a space in the file name.   Correctly rendering the path for the build
directory now.

# mrgsolve 0.6.1

## Features
* Added `as_data_set` to convert one or more event objects into a data frame
that can be passed to `data_set`.  Does something similar to `expand.ev`,
but more control.
* For special column names (`time`, `amt`, `rate`, `evid`, `ii`, `addl` ,`ss`,
`cmt`) either lower case or upper case names are recognized.  The determination
is made on the `time` / `TIME` column (always required when using a data set).
If `time` mrgsolve will continue looking for lower case names; if `TIME` it
will look for upper case names.  A warning is issued in case both upper and
lower case names are included.
* Added `$PLUGIN` to let users extend their model specification file.  Valid
plugins include `simeta`, `Rcpp`, `RcppArmadillo`, and `BH`.  When a plugin
is used, mrgsolve will link back the the appropriate package and possibly
include appropriate header files when compiling the model.  For example,
`simeta` will link back to mrgsolve and `RcppArmadillo` and allow the modeler
to simulate a new set of `ETA`s.  Use `Rcpp`  plugin to simulate random variates
from common distributions in `R`(e.g. `rnorm`, `rexp` etc ... ).  

## Bugs fixed
* Fixed issue with `ev` where no rows were returned if `amt` wasn't supplied
(#44).
* Shortened the path for both the shared object and the name of the `.cpp.cpp`
file when compiling.
* Fixed bug in `touch_funs` when large number (`> 25`) of ETAs in the model
(#68).

## Important changes
* When using `$PKMODEL` with `ncmt=2` and `depot=FALSE`, the default PK
parameters are `CL`, `V1` (central volume), `Q`, `V2` (peripheral volume).  
This is a change where the previous volumes were `V2` (central) and `V3`
(peripheral).
* `$CAPTURE` now saves output items to slots in `std::vector<double>`, rather
than `std::map<std::string,double>`.  We've known for a while that the
`std::map` wasn't very efficient especially with large simulations.  
Currently, items in `$TABLE` are still saved into `std::map` with `table()`
macro.  The plan going forward is to eliminate that `table` `map` and force
output variables into `$CAPTURE`.
* Due to major changes to `dplyr`, now requiring `dplyr >= 0.5.0` (#69)
* The `data` slot in `mrgsims` objects is now `data.frame`
* The `knobs` function and `plot` method has been re-written.  Overall behavior
for most applications should be the same.

## Under the hood
* `C++` symbols for model functions are now stored in the model object
(`funs` slot)
* The status of the model object (function names and compile status) can be
checked with `mrgsolve:::funset(mod)`
* A model is considered to be loaded and ready to go if all functions in
`funs` can be found with `is.loaded`
* Model shared objects are still stored within the `soloc` directory (by
default `tempdir()`), but mrgsolve will create a subdirectory structure to
organize compilation artifacts.  The outer directory is keyed based on the
current mrgsolve version number and the computer platform.  Inner directories
are based on the model name (`model(mod)`).  
* A source file is created based on the `model` name and the shared object is
created based on that name.  If the compilation is successful, the shared
object (`.so` on mac/unix, `.dll` on Windows) is copied to a `.so` or `.dll`
file with a unique stem (e.g. `model2lj239wsfo.so`).  This unique shared object
is loaded into the `R` process for use with the model.  
* Every time the model is rebuilt, the build directory is scanned for shared
object files.  Excluding the main model shared object (unchanging name based
on the model), old shared object files are deleted and, if currently loaded
(`getLoadedDLLs()`), are attempted to be `dyn.unload`ed.
* Upon model rebuild (via `mread` or `mcode`), if there are no changes to the
source `.cpp` file, the source is not overwritten.  In that case, `make` will
not re-build the shared object.  Using the `preclean` argument will force
re-compilation (see `R CMD SHLIB`).
* The header files `modelheader.h` and `mrgsolv.h` are no longer copied into
the project directory.  But `CLINK_CPPFLAGS` environment variable is modified
to include `<path-to-mrgsolve-package>/inst/base` so that these may be linked.
* The `R CMD SHLIB` build process always uses `intern=TRUE` so that output is
suppressed on both `Windows` and `mac/unix`.  The user may still request to
view build output with the `ignore.stdout` argument.
* Model build always links-to the `project` directory to look for `C++` header
files.  When including a header file that may change from build to build,
always run with `preclean=TRUE`.
* mrgsolve now changes the working directory prior to building a model.  
The working directory is restored on exit from `mread`.

## Deprecated
* The entire `complog` system, including:
    * `comp_forget` a message is issued
    * `complog` no message is issued
* `trequest` argument to `mrgsim`


# mrgsolve 0.6.0

## Bugs fixed
* Fixed a bug when an infusion was attempted with `rate > 0` and `amt==0`.  
Additionally, an error is generated when an infusion is attempted with zero
`amt` ([issue 43](../../issues/43)).

# mrgsolve 0.5.12

## Bugs fixed
* Fixed a bug where infusion dosing events with `evid 4` were not properly
implemented.  While investigating that issue, also found similar issue with
`evid 4` infusions getting scheduled with `addl` ([issue 31](../../issues/31)).
* Removed NSE evaluation for arguments passed into  `ev` to avoid unintended
issues in evaluation ([issue 29](../../issues/29)).

## New features
* Added `mcode` function as alternative to using `mread` when your model is
written in an `R` string.  Note the order of the arguments:
first `model`, then `code`, then `project`.  `project` defaults to `tempdir`.  
So the call is: `mod <- mcode("mymodel", code)`.  The
equivalent `mread` call is: `mod <- mread("mymodel", tempdir(),code)`.
* `carry.out()` and `Req()` now take `newname = oldname` as input.  Use this
syntax in `carry.out` when you want to copy a column from the input data set
into the simulated data set, changing the column to `newname` from `oldname`.  
Use this syntax in `Req` when you want to change the names of compartments or
output variables specified in `$TABLE` / `$CAPTURE`.
* Added `pkmodel` function for easy loading and simulating from 1- and
2-compartment models ([issue 39](../../issues/39)).
* Added new code block: `$PKMODEL` for simulating PK model with analytical
solutions.  The main option for this block is `ncmt`, which picks the number
of compartments for the pk model.  See `?PKMODEL` for more information and
other options ([issue 34](../../issues/34)).

## Under the hood
* Added `code` attribute to `mrgmod` objects.  The actual source code stays
with the model object. `see` was modified to look at `x@code` first when
showing the model code.
* Added a tokenizer function (`get_tokens`), a wrapper for the boost tokenizer)
to help checking the model specification file.
* Data items entered in `$FIXED` are now implemented as C++ preprocessor
directives by default rather than `const double` variables.  Use `$SET
fixed_type = "define"` or `$SET fixed_type = "const"` to select between
the approaches.


# mrgsolve 0.5.11
## Bugs fixed
* Added missing example model specification files (popExample, viralExample,
others)
* Added `mindt` attribute to `mrgmod` objects with default value of
`.Machine$double.eps*10`. When the problem includes an infusion, the
calculated end of the infusion may come too close to another record.  Usually
the solver will fail with the message `DLSODA- TOUT(=R1) too close to T(=R2)
to start integration.`.  To fix this, set `mindt` to be greater than zero but
small ... maybe 1E-12.  When `mindt` is greater than zero and `tto - tfrom`
(the times of two adjacent records) is less than `mindt`, mrgsolve will set
`tto` equal to `tfrom` ([issue 9](../../issues/9)).
* `zero.re` didn't properly update the `$SIGMA` list when one matrix was
named and another was unnamed.  This has been fixed.  
([issue 16](../../issues/16))
* Fixed a bug where infusions with `ss`=1 caused mrgsolve/`R` to crash when
the infusion duration was equal to or some multiple of the dosing interval.
([issue 19](../../issues/19))
* Fixed a bug where setting `F_CENT` to zero gave undefined behavior.  
mrgsolve will issue an error if `F_CMT` is set to zero and the `ss` flag is
set to 1. ([issue 22](../../issues/16))
* Fixed bug where dosing records with `evid=4` (reset the system and dose)  
and `addl > 0` reset the system for all subsequent doses. Additional doses
coming from records with `evid=4` will not do system reset.
([issue 23](../../issues/23))

## Important changes
* New arguments for `$NMXML` (see `?nmxml`) that are easier to understand
and consistent with new prefixes and labels for `ETA` and `EPS`.  `name`
argument is removed.  Use `tname` (to provide a prefix for `THETAs`),
`oname` (to name the `OMEGA` matrix), and `sname` (to name the `SIGMA` matrix)
instead.  In general, set `theta` to be `TRUE` to import `THETAs`, set `omega`
to be `TRUE` to import `OMEGA`, and set `sigma` to be `TRUE` to import `SIGMA`.  
Specifying character names `tname`, `oname`, and `sname` will imply
`theta=TRUE`, `omega=TRUE`, and `sigma=TRUE`, respectively.


## New features
* New aliases available for setting bioavailability, lag time and infusion
duration and rate.  For a compartment called `DEPOT` use: `F_DEPOT`
(bioavailability), `ALAG_DEPOT` (dosing lag time), `D_DEPOT` (infusion
duration), and / or `R_DEPOT` (infusion rate). ([issue 13](../../issues/13))
* Added *slightly* more informative messages when `DLSODA` fails, including
clear identification if the value of `istate`, which is 2 when the solver
succeeds and negative when the solver fails.
* Added `labels` and `prefix` options to `$OMEGA` and `$SIGMA`.  These allow
descriptive aliases for ETAs ... e.g. using `ETA_CL` rather than `ETA(1)`.  
([issue 15](../../issues/15))
* Added `dplyr::slice` method for `mrgsims` objects
([issue 11](../../issues/11))
* New argument to `mread`: `quiet`.  Setting `quiet` to `TRUE` will prevent
printing messages when `mread` is called.  The default is
`getOptions("mrgsolve_mread_quiet",FALSE)` ... so you can call
`options(mrgsolve_mread_quiet = TRUE)` to globally turn off messages
from `mread`.

# 0.5.001

## Bugs Fixed
* Fixed paths for project (`project`) and shared object (`soloc`) so that
"short" paths are used when compiling the model on Windows  platforms
(info [issue 4](https://github.com/metrumresearchgroup/mrgsolve/issues/4) and [here](https://github.com/metrumresearchgroup/mrgsolve/wiki/Windows-issues#spaces-in-project-directory-path))
* Fixed a bug where bioavailability fraction was not accounted for when
setting infusion duration in `$MAIN` with `rate=-2`
(see [issue 3](https://github.com/metrumresearchgroup/mrgsolve/issues/3))
* `mrgsolve_example` prints a message telling the user to use `mrgmod` to read
and compile the model.  The user should use `mread`.  The message has been
updated. (info [issue 5](https://github.com/metrumresearchgroup/mrgsolve/issues/5))
* Fixed bug where parameters were not correctly read from first row of a
data set when certain `recsort` options were selected
([issue 6](https://github.com/metrumresearchgroup/mrgsolve/issues/6))

## New Features
*  Added `$ADVAN2` and `$ADVAN4` for implementing one- and two-compartment PK
models with analytical solutions rather than ODEs (thanks to contributions by
Bill Gillespie and Charles Margossian)
* Added `soloc` attribute added to `mrgmod` and argument added to `mread`,
giving user control over where the shared object is stored; by default it is
in `tempdir()`.  `soloc` needs to be set to local directory when using `qapply`
* Added generics for various `dplyr_` functions so that `mrgsims` objects can
be piped to `mutate`, `group_by`, `filter`, `summarise`, `do`, `select`, and
`summarise.each` (it's a dot not an underscore).  So:
`mod %>% mrgsim %>% mutate(group=1)`
* You can now set initial conditions though `idata`; for compartment
`CMT`, include a column in `idata` called `CMT_0`
* Modified update policy for `$OMEGA` and `$SIGMA`: if all incoming
matrices are unnamed and the signature matches the model object, the update
will happen
* Added `mcRNG` function as alias to `base::RNGkind("L'Ecuyer-CMRG")`

## Important changes
* Changes to `recsort`: 1 and 2 will put explicit doses after observations at
the same time, 3 and 4 will put explicit doses before observations at the same
time.  2 and 4 will put doses scheduled through `addl` after observations at
the same time; 1 and 3 put doses scheduled through `addl` before observations
at the same tile.

## Under the hood
* Changed dosing lag time mechanism so that lag times are calculated as the
simulation progresses and dose times in the simulated output are as in the input
data
* Model shared objects are given random names every time the model is
compiled and loaded
* Various fixes and expansions to documentation
* Changed `R` dependency to `>= 3.1.2`
* All `Ops` involving `mrgmod` objects are now deprecated
* `loadso` now returns the model object (invisibly)
* Compartment names are automatically removed from table map
* Model specification files in need of compilation are detected via md5sum
* Minimum of 2 sec wait time is required before re-compiling a model
* Added `init` method with signature `mrgmod`,`ANY`, with `ANY` getting coerced
to `list`
