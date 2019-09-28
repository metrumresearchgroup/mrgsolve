|file                  |context                              |test                                                                      | nb| failed|skipped |error | warning| passed|
|:---------------------|:------------------------------------|:-------------------------------------------------------------------------|--:|------:|:-------|:-----|-------:|------:|
|test-annot.R          |test-annot                           |parse line - name : value : text                                          | 14|      0|FALSE   |FALSE |       0|     14|
|test-annot.R          |test-annot                           |parse line - name  : text                                                 |  9|      0|FALSE   |FALSE |       0|      9|
|test-annot.R          |test-annot                           |parse line - value  : text                                                |  5|      0|FALSE   |FALSE |       0|      5|
|test-annot.R          |test-annot                           |Full specification - $PARAM                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-annot.R          |test-annot                           |Full specification - $THETA                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-annot.R          |test-annot                           |Full specification - $CMT                                                 |  3|      0|FALSE   |FALSE |       0|      3|
|test-annot.R          |test-annot                           |Full specification - $INIT                                                |  4|      0|FALSE   |FALSE |       0|      4|
|test-annot.R          |test-annot                           |Full specification - $FIXED                                               |  4|      0|FALSE   |FALSE |       0|      4|
|test-annot.R          |test-annot                           |Full specification - $VCMT                                                |  3|      0|FALSE   |FALSE |       0|      3|
|test-annot.R          |test-annot                           |Full specification - $CAPTURE                                             |  4|      0|FALSE   |FALSE |       0|      4|
|test-as_data_set.R    |test-as_data_set                     |as_data_set basic                                                         |  4|      0|FALSE   |FALSE |       0|      4|
|test-as_list_mrgmod.R |test-as_list_mrgmod                  |check items in as.list output                                             | 38|      0|FALSE   |FALSE |       0|     38|
|test-cache.R          |test-cache                           |model caches via mread_cache                                              |  2|      0|FALSE   |FALSE |       0|      2|
|test-cache.R          |test-cache                           |model caches via mcode_cache                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-carry_out.R      |test-carry_out                       |carry_out from complete data set                                          |  2|      0|FALSE   |FALSE |       0|      2|
|test-carry_out.R      |test-carry_out                       |carry_out from idata set                                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-carry_out.R      |test-carry_out                       |carry_out from condensed data set                                         |  2|      0|FALSE   |FALSE |       0|      2|
|test-carry-out.R      |test-carry-out                       |carry_out amt                                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-carry-out.R      |test-carry-out                       |carry_out rate                                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-carry-out.R      |test-carry-out                       |carry_out rate                                                            |  1|      0|FALSE   |FALSE |       0|      1|
|test-carry-out.R      |test-carry-out                       |carry_out mixed                                                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-carry-out.R      |test-carry-out                       |carry_out mixed, rename                                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-carry-out.R      |test-carry-out                       |carry_out mixed, some rename                                              |  2|      0|FALSE   |FALSE |       0|      2|
|test-data_set.R       |test-data_set                        |Same result from upper and lower case names                               | 10|      0|FALSE   |FALSE |       0|     10|
|test-data_set.R       |test-data_set                        |Warning is generated when mixed upper/lower names                         |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |Filter out ID                                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |ID is required                                                            |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |cmt is required                                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |time is required                                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |Improperly sorted records produces error                                  |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |Properly sorted records produces no error                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-data_set.R       |test-data_set                        |Data set column order gives same answer                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-data_set.R       |test-data_set                        |numerics_only                                                             |  6|      0|FALSE   |FALSE |       0|      6|
|test-data_set.R       |test-data_set                        |missing value in param column is message                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-data_set.R       |test-data_set                        |missing value in time/rate/ID is error                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-data_set.R       |test-data_set                        |observations expand                                                       |  3|      0|FALSE   |FALSE |       0|      3|
|test-deslist.R        |test-deslist                         |as_deslist                                                                |  7|      0|FALSE   |FALSE |       0|      7|
|test-deslist.R        |test-deslist                         |tgrid_id                                                                  |  1|      0|FALSE   |FALSE |       0|      1|
|test-deslist.R        |test-deslist                         |tgrid_matrix                                                              |  3|      0|FALSE   |FALSE |       0|      3|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to tibble                                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to mutate                                                            |  3|      0|FALSE   |FALSE |       0|      3|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to filter                                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to summarise                                                         |  3|      0|FALSE   |FALSE |       0|      3|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to select                                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to group_by                                                          |  2|      0|FALSE   |FALSE |       0|      2|
|test-dplyr-generics.R |test-dplyr-generics                  |Pipe to slice                                                             |  2|      0|FALSE   |FALSE |       0|      2|
|test-dplyr-generics.R |test-dplyr-generics                  |filter_mrgsims                                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-dplyr-generics.R |test-dplyr-generics                  |mutate_mrgsims                                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-env.R            |test-env                             |$ENV                                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-env.R            |test-env                             |$ENV sub into $PARAM                                                      |  3|      0|FALSE   |FALSE |       0|      3|
|test-env.R            |test-env                             |$ENV sub into $INIT                                                       |  3|      0|FALSE   |FALSE |       0|      3|
|test-env.R            |test-env                             |Get $OMEGA matrix from $ENV                                               |  4|      0|FALSE   |FALSE |       0|      4|
|test-env.R            |test-env                             |$ENV sub into $FIXED                                                      |  4|      0|FALSE   |FALSE |       0|      4|
|test-env.R            |test-env                             |env-funs                                                                  |  5|      0|FALSE   |FALSE |       0|      5|
|test-ev_assign.R      |test-ev_assign                       |Input error                                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev_assign.R      |test-ev_assign                       |Assignment on sorted values                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_days.R        |test-ev_days                         |Input error                                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev_days.R        |test-ev_days                         |Schedule with days argument                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev_days.R        |test-ev_days                         |Schedule with missing arguments                                           |  3|      0|FALSE   |FALSE |       0|      3|
|test-ev_rx.R          |test-ev_rx                           |parse dose only - bolus                                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_rx.R          |test-ev_rx                           |parse dose only - infusion                                                |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev_rx.R          |test-ev_rx                           |parse dose plus additional - bolus                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_rx.R          |test-ev_rx                           |parse dose plus additional - infusion                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_rx.R          |test-ev_rx                           |parse multiple - infusion / bolus                                         |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev_rx.R          |test-ev_rx                           |parse dose into compartment                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_rx.R          |test-ev_rx                           |dose can be in decimal or scientific                                      |  3|      0|FALSE   |FALSE |       0|      3|
|test-ev_rx.R          |test-ev_rx                           |infusion duration can be decimal                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_rx.R          |test-ev_rx                           |infusion duration can be decimal                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev_rx.R          |test-ev_rx                           |after parameter can be decimal                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev.R             |test-ev                              |observations are not allowed                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |doses are required                                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |ev.ev                                                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |event requirements and defaults                                           |  4|      0|FALSE   |FALSE |       0|      4|
|test-ev.R             |test-ev                              |collection of events                                                      |  3|      0|FALSE   |FALSE |       0|      3|
|test-ev.R             |test-ev                              |realized events                                                           |  6|      0|FALSE   |FALSE |       0|      6|
|test-ev.R             |test-ev                              |realized event error                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |sequence of event objects                                                 |  5|      0|FALSE   |FALSE |       0|      5|
|test-ev.R             |test-ev                              |replicate an event object                                                 |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev.R             |test-ev                              |events with without rate                                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev.R             |test-ev                              |coerce to data frame                                                      |  6|      0|FALSE   |FALSE |       0|      6|
|test-ev.R             |test-ev                              |get names                                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |mutate an ev object                                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev.R             |test-ev                              |filter an ev object                                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |misc methods                                                              |  2|      0|FALSE   |FALSE |       0|      2|
|test-ev.R             |test-ev                              |as.ev                                                                     |  6|      0|FALSE   |FALSE |       0|      6|
|test-ev.R             |test-ev                              |ev_repeat                                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-ev.R             |test-ev                              |create ev with evaluation issue-512                                       |  4|      0|FALSE   |FALSE |       0|      4|
|test-ev.R             |test-ev                              |tinf issue-513                                                            |  4|      0|FALSE   |FALSE |       0|      4|
|test-ev.R             |test-ev                              |total  issue-513                                                          |  4|      0|FALSE   |FALSE |       0|      4|
|test-ev.R             |test-ev                              |until  issue-513                                                          |  3|      0|FALSE   |FALSE |       0|      3|
|test-evid4.R          |test-evid4                           |evid4 bolus dosing is the same as evid1                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-evid4.R          |test-evid4                           |evid4 infusion dosing is the same as evid1                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-fixed-cmtn.R     |test-fixed-cmtn                      |FIXED items are excluded from param                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-fixed-cmtn.R     |test-fixed-cmtn                      |FIXED items can be recovered                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-funset.R         |test-funset                          |funset                                                                    |  3|      0|FALSE   |FALSE |       0|      3|
|test-idata_set.R      |test-data_set                        |event with idata set                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-idata_set.R      |test-data_set                        |data set with idata                                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-infusion.R       |test-infusion                        |Infusion with amt == 0                                                    |  1|      0|FALSE   |FALSE |       0|      1|
|test-infusion.R       |test-infusion                        |Infusion with large rate and small amount                                 |  3|      0|FALSE   |FALSE |       0|      3|
|test-infusion.R       |test-infusion                        |Infusion ends at the proper time                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-infusion.R       |test-infusion                        |Consecutive infusions act as one long infusion                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-infusion.R       |test-infusion                        |Same results from addl and explicit doses                                 |  2|      0|FALSE   |FALSE |       0|      2|
|test-infusion.R       |test-infusion                        |Infusion with duration a multiple of ii                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-infusion.R       |test-infusion                        |Infusion with no obs overlap                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-infusion.R       |test-infusion                        |Infusion with obs overlap                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-infusion.R       |test-infusion                        |Infusion executes with ss flag and ii==dur                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-init.R           |init                                 |inits are constructed                                                     |  5|      0|FALSE   |FALSE |       0|      5|
|test-init.R           |init                                 |inits are shown                                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-inventory-too.R  |test-inventory-too                   |inventory conditions                                                      |  8|      0|FALSE   |FALSE |       0|      8|
|test-inventory.R      |test-inventory                       |inventory works                                                           |  3|      0|FALSE   |FALSE |       0|      3|
|test-inventory.R      |test-inventory                       |inventory errors when missing required params                             |  4|      0|FALSE   |FALSE |       0|      4|
|test-iv-po.R          |test-iv-po                           |Simulation output is of class mrgsims                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-iv-po.R          |test-iv-po                           |The simulation model can be recovered from output                         |  2|      0|FALSE   |FALSE |       0|      2|
|test-iv-po.R          |test-iv-po                           |CP from oral model is identical to closed form result                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-iv-po.R          |test-iv-po                           |CP from iv model is identical to closed form result                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-iv-po.R          |test-iv-po                           |Error on dosing into non-existant compartment                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-knobs.R          |test-knobs                           |knobs() returns object of class batch_mrgsims                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-knobs.R          |test-knobs                           |plotting batch_mrgsims objects                                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-knobs.R          |test-knobs                           |Moving knobs are correctly identified                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-knobs.R          |test-knobs                           |CL knob is correctly captured in output as CL                             |  2|      0|FALSE   |FALSE |       0|      2|
|test-knobs.R          |test-knobs                           |A false knob does not appear in simulated output                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-matlist.R        |test-matlist                         |Indexing OMEGA matrix elements                                            |  3|      0|FALSE   |FALSE |       0|      3|
|test-matlist.R        |test-matlist                         |Indexing SIGMA matrix elements                                            |  3|      0|FALSE   |FALSE |       0|      3|
|test-matlist.R        |test-matlist                         |Indexing OMEGA matrix elements with multiple matrices                     |  4|      0|FALSE   |FALSE |       0|      4|
|test-matlist.R        |test-matlist                         |Update a model with no matrix                                             |  3|      0|FALSE   |FALSE |       0|      3|
|test-matlist.R        |test-matlist                         |Update a model matrix                                                     |  2|      0|FALSE   |FALSE |       0|      2|
|test-matlist.R        |test-matlist                         |valid matlist                                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-matlist.R        |test-matlist                         |new_omat                                                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-matlist.R        |test-matlist                         |new_smat                                                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-matrix.R         |test-matrix                          |Testing modMATRIX                                                         |  5|      0|FALSE   |FALSE |       0|      5|
|test-modlib.R         |test-modlib                          |code extraction                                                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-modlib.R         |test-modlib                          |list models                                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |matrix data is parsed                                                     |  2|      0|FALSE   |FALSE |       0|      2|
|test-modspec.R        |test-modspec                         |capture data is parsed                                                    |  4|      0|FALSE   |FALSE |       0|      4|
|test-modspec.R        |test-modspec                         |cmt block is parsed                                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |theta block is parsed                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-modspec.R        |test-modspec                         |Using table macro generates error                                         |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: THETA                                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: PARAM                                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: CMT                                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: FIXED                                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: CAPTURE                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: INIT                                                         |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: OMEGA                                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Empty block: SIGMA                                                        |  1|      0|FALSE   |FALSE |       0|      1|
|test-modspec.R        |test-modspec                         |Commented model                                                           |  4|      0|FALSE   |FALSE |       0|      4|
|test-modspec.R        |test-modspec                         |at options are parsed                                                     | 12|      0|FALSE   |FALSE |       0|     12|
|test-modspec.R        |test-modspec                         |specMATRIX                                                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |ETA(n) in $ODE is error                                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |Warning with no $CMT or $INIT                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |read in rmd file                                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgindata.R      |test-mrgindata                       |valid_data_set warns for character columns                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgindata.R      |test-mrgindata                       |valid_data_set subs character cmt                                         |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run with no input                                                         |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run with no input - and nid                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgindata.R      |test-mrgindata                       |Run ev event                                                              |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run ev event - character cmt                                              |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run bad data sets                                                         |  5|      0|FALSE   |FALSE |       0|      5|
|test-mrgindata.R      |test-mrgindata                       |Run ev event - and nid                                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run with data set - data.frame                                            |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgindata.R      |test-mrgindata                       |Run with data set - tbl                                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run idata set                                                             |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Run idata set with ev                                                     |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgindata.R      |test-mrgindata                       |Duplicate ID in idata_set gives error                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgmod.R         |test-mrgmod                          |methods                                                                   | 11|      0|FALSE   |FALSE |       0|     11|
|test-mrgsim_q.R       |test-mrgsim_q                        |simulation with a complete data set                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgsim_q.R       |test-mrgsim_q                        |simcall=1 is deprecated                                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgsim_q.R       |test-mrgsim_q                        |qsim                                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-mrgsim.R         |test-mrgsim                          |mrgsim_df                                                                 |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with ev                                                            |  4|      0|FALSE   |FALSE |       0|      4|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with ev and idata                                                  |  3|      0|FALSE   |FALSE |       0|      3|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with ev and ID and idata                                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with data and idata                                                |  6|      0|FALSE   |FALSE |       0|      6|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with ev and ID                                                     |  4|      0|FALSE   |FALSE |       0|      4|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with data                                                          |  5|      0|FALSE   |FALSE |       0|      5|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with data and ev                                                   |  3|      0|FALSE   |FALSE |       0|      3|
|test-mrgsim.R         |test-mrgsim                          |mrgsim with nid                                                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgsim.R         |test-mrgsim                          |update arguments are passed                                               |  6|      0|FALSE   |FALSE |       0|      6|
|test-mrgsim.R         |test-mrgsim                          |no data generates error                                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgsim.R         |test-mrgsim                          |no idata no problem generates error                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-mrgsims.R        |test-mrgsims                         |mrgsims class                                                             |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |Model spec with $NMXML block can be parsed                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |THETAS are imported into the parameter list                               |  3|      0|FALSE   |FALSE |       0|      3|
|test-nmxml.R          |test-nmxml                           |OMEGAS are imported into the omega list                                   |  3|      0|FALSE   |FALSE |       0|      3|
|test-nmxml.R          |test-nmxml                           |SIGMA are imported into the sigma list                                    |  3|      0|FALSE   |FALSE |       0|      3|
|test-nmxml.R          |test-nmxml                           |Loading OMEGA from multiple sources                                       |  4|      0|FALSE   |FALSE |       0|      4|
|test-nmxml.R          |test-nmxml                           |Correlation in corr matrix is converted to covariance                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |When use=FALSE, variance is 0                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |Matrices are properly named                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |Loading SIGMA from multiple sources                                       |  4|      0|FALSE   |FALSE |       0|      4|
|test-nmxml.R          |test-nmxml                           |update OMEGA by name                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |Update SIGMA by name                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |error is generated for incompatible dimensions                            |  3|      0|FALSE   |FALSE |       0|      3|
|test-nmxml.R          |test-nmxml                           |A warning is generated when nothing is updated                            |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |A single unnamed matrix is updated                                        |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |Warning issued if updating unnamed matrix with named matrix               |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |No matrices when name not given                                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |Get theta and omega                                                       |  3|      0|FALSE   |FALSE |       0|      3|
|test-nmxml.R          |test-nmxml                           |Model compiles                                                            |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |Labels are assigned to $OMEGA and $SIGMA                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |zero_re zeros all matrices                                                |  2|      0|FALSE   |FALSE |       0|      2|
|test-nmxml.R          |test-nmxml                           |Mixed labels / no labels and prefix                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-nmxml.R          |test-nmxml                           |read_nmext returns estimates                                              |  5|      0|FALSE   |FALSE |       0|      5|
|test-numericlist.R    |test-numericlist                     |numericlist                                                               |  4|      0|FALSE   |FALSE |       0|      4|
|test-obsonly-obsaug.R |test-obsonly-obsaug                  |Using of obsonly with data set                                            |  3|      0|FALSE   |FALSE |       0|      3|
|test-obsonly-obsaug.R |test-obsonly-obsaug                  |Using obsonly with events object                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-obsonly-obsaug.R |test-obsonly-obsaug                  |Use of obsaug returns augmented set of observations                       |  4|      0|FALSE   |FALSE |       0|      4|
|test-obsonly-obsaug.R |test-obsonly-obsaug                  |Use of obsaug doesn't affect simulation without data                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-opts.R           |test-opts                            |Options where they don't belong                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-opts.R           |test-opts                            |Scrape and call                                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-param.R          |param                                |params are constructed                                                    |  6|      0|FALSE   |FALSE |       0|      6|
|test-param.R          |param                                |params are accessed                                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-param.R          |param                                |params are updated                                                        |  9|      0|FALSE   |FALSE |       0|      9|
|test-param.R          |param                                |params are shown                                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-rcpp_globals.R   |test-rcpp_globals                    |rcpp_globals                                                              |  5|      0|FALSE   |FALSE |       0|      5|
|test-realize.R        |test-ev                              |dosing without ss                                                         |  1|      0|FALSE   |FALSE |       0|      1|
|test-realize.R        |test-ev                              |dosing with ss                                                            |  1|      0|FALSE   |FALSE |       0|      1|
|test-realize.R        |test-ev                              |data frame                                                                |  3|      0|FALSE   |FALSE |       0|      3|
|test-records.R        |test-records                         |Run via idata and separate                                                |  2|      0|FALSE   |FALSE |       0|      2|
|test-relabel.R        |test-relabel                         |rename, from vector                                                       |  3|      0|FALSE   |FALSE |       0|      3|
|test-relabel.R        |test-relabel                         |rename, from string                                                       |  3|      0|FALSE   |FALSE |       0|      3|
|test-relabel.R        |test-relabel                         |ren rename                                                                |  2|      0|FALSE   |FALSE |       0|      2|
|test-rename.R         |test-rename                          |tran item is renamed                                                      |  4|      0|FALSE   |FALSE |       0|      4|
|test-rename.R         |test-rename                          |Item carried from data set is renamed                                     |  2|      0|FALSE   |FALSE |       0|      2|
|test-rename.R         |test-rename                          |Item carried from data set is renamed                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-rng.R            |test-rng                             |Different seeds give different results without call to set.seed()         |  1|      0|FALSE   |FALSE |       0|      1|
|test-rng.R            |test-rng                             |Different seeds give different results with different calls to set.seed() |  1|      0|FALSE   |FALSE |       0|      1|
|test-rng.R            |test-rng                             |Same seeds give same results with call to set.seed()                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-tad.R            |test-tad                             |tad                                                                       |  7|      0|FALSE   |FALSE |       0|      7|
|test-tad.R            |test-tad                             |tad recognizes evid 1 and 4 issue-502                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-tgrid.R          |test-tgrid                           |tgrid                                                                     |  5|      0|FALSE   |FALSE |       0|      5|
|test-update.R         |test-update                          |model object updates through update and %>% operator                      |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Simulation times update properly via update                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Simulation times update when passed into mrgsim                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Parameter updates when passed to mrgsim                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Parameter updates when added inside mrgsim call                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Initials update via init and list                                         |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Initials update via init                                                  |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Initials update via init and data.frame                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Initial conditions update via update()                                    |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting hmin updates                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting hmax updates                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting maxsteps updates                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting ixpr updates                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting mxhnil updates                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting atol updates                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Solver setting rtol updates                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Recover items from simulated data when passed in as idata                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Recover items from simulated data when passed in as data                  |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |CP is equal when simulating from events or data                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Time-varying data items in data are properly carried into output          |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |merge two lists, open                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-utils.R          |test-utils                           |merge two lists, closed                                                   |  3|      0|FALSE   |FALSE |       0|      3|
|test-utils.R          |test-utils                           |combine_list                                                              |  4|      0|FALSE   |FALSE |       0|      4|
|test-utils.R          |test-utils                           |Corecing simulated output to data.frame                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing simulated output to matrix                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing parameters to list                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing parameters to numeric                                            |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing initials to list                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing initials to numeric                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing parameters to data.frame                                         |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |Corecing initials to data.frame                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |stime correctly generates simulation times                                |  3|      0|FALSE   |FALSE |       0|      3|
|test-utils.R          |test-utils                           |Negative end time gives simulations at add only                           |  2|      0|FALSE   |FALSE |       0|      2|
|test-utils.R          |test-utils                           |If no simulation times can be rendered time=0 only is simulated           |  1|      0|FALSE   |FALSE |       0|      1|
|test-utils.R          |test-utils                           |expand.ev issue-513                                                       |  5|      0|FALSE   |FALSE |       0|      5|
|test-utils.R          |test-utils                           |tovec                                                                     |  2|      0|FALSE   |FALSE |       0|      2|
|test-utils.R          |test-utils                           |cvec                                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-workflow.R       |test-workflow                        |workflow                                                                  |  3|      0|FALSE   |FALSE |       0|      3|
|test-alag.R           |test-alag                            |Lagged bolus                                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-alag.R           |test-alag                            |Very small lag time doesn't crash                                         |  6|      0|FALSE   |FALSE |       0|      6|
|test-alag.R           |test-alag                            |Lag time on SS record - bolus                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-alag.R           |test-alag                            |Lag time on SS record - infusion                                          |  2|      0|FALSE   |FALSE |       0|      2|
|test-alag.R           |test-alag                            |Error lagtime+duration >= ii for infusion                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-alag.R           |test-alag                            |ss dose with lag time, different arrangements                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-bioav.R          |test-bioav                           |Bioav test with doses at time=0                                           |  3|      0|FALSE   |FALSE |       0|      3|
|test-capture.R        |test-request                         |Renamed captured items are properly named                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-D-R-F.R          |test-D-R-F                           |Infusion rate is set by R_CMT                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-D-R-F.R          |test-D-R-F                           |Error when rate = -2 and R_CMT set instead of D_CMT                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-D-R-F.R          |test-D-R-F                           |Infusion rate is set by D_CMT                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-D-R-F.R          |test-D-R-F                           |Error when rate = -1 and D_CMT set instead of R_CMT                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration when D_CMT and F_CMT are set                            |  6|      0|FALSE   |FALSE |       0|      6|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration (D_) with lag                                           |  3|      0|FALSE   |FALSE |       0|      3|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration (D_) with lag, multiple                                 |  3|      0|FALSE   |FALSE |       0|      3|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration (D_) with lag and F                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration when R_CMT and F_CMT are set                            |  6|      0|FALSE   |FALSE |       0|      6|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration (R_) lag time, multiple                                 |  3|      0|FALSE   |FALSE |       0|      3|
|test-D-R-F.R          |test-D-R-F                           |Infusion duration (R_) with lag time and F                                |  3|      0|FALSE   |FALSE |       0|      3|
|test-fail.R           |test-fail                            |build_fails                                                               |  1|      0|FALSE   |FALSE |       0|      1|
|test-initials.R       |test-initials                        |Set initials via init                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-initials.R       |test-initials                        |Set initials via $MAIN                                                    |  9|      0|FALSE   |FALSE |       0|      9|
|test-initials.R       |test-initials                        |Set initials via idata                                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-mevent.R         |mevent                               |mevent - time-varying KA                                                  |  4|      0|FALSE   |FALSE |       0|      4|
|test-modlib.R         |test-modlib models                   |Lagged bolus                                                              | 32|      0|FALSE   |FALSE |       0|     32|
|test-mread.R          |test-mread                           |Parameters are parsed properly with mread                                 |  3|      0|FALSE   |FALSE |       0|      3|
|test-mread.R          |test-mread                           |Compartments are parsed properly with mread                               |  3|      0|FALSE   |FALSE |       0|      3|
|test-mread.R          |test-mread                           |Settings are parsed properly with mread                                   |  4|      0|FALSE   |FALSE |       0|      4|
|test-mread.R          |test-mread                           |mread output had class mrgmod                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |Tabled items are in simulated data                                        |  2|      0|FALSE   |FALSE |       0|      2|
|test-mread.R          |test-mread                           |Omega matrices are properly parsed                                        |  4|      0|FALSE   |FALSE |       0|      4|
|test-mread.R          |test-mread                           |Sigma matrices are properly parsed                                        |  3|      0|FALSE   |FALSE |       0|      3|
|test-mread.R          |test-mread                           |EPS values have proper variance                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |Error when code is passed as project                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |Model name with spaces is error                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-mread.R          |test-mread                           |Error with duplicate blocks                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-nocb.R           |test-nocb                            |simulation with nocb                                                      |  2|      0|FALSE   |FALSE |       0|      2|
|test-nocb.R           |test-nocb                            |simulation with locf                                                      |  2|      0|FALSE   |FALSE |       0|      2|
|test-pk.R             |One compartment model tests          |one-compartment, bolus                                                    |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |One compartment model tests          |one-compartment, bolus, ss                                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |One compartment model tests          |one-compartment, oral, first                                              |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |One compartment model tests          |one-compartment, oral, first, ss                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |One compartment model tests          |one-compartment, infusion                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |One compartment model tests          |one-compartment, infusion                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Two compartment model tests          |two-compartment, bolus                                                    |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Two compartment model tests          |two-compartment, bolus, ss                                                |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Two compartment model tests          |two-compartment, bolus, first                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Two compartment model tests          |two-compartment, bolus, first, ss                                         |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Two compartment model tests          |two-compartment, infusion                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Two compartment model tests          |two-compartment, infusion, ss                                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Multiple dosing - 1 cmt              |one-compartment, bolus, multiple                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Multiple dosing - 1 cmt              |one-compartment, infusion, multiple                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Multiple dosing - 1 cmt              |one-compartment, oral, multiple                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Multiple dosing - 2 cmt              |two-compartment, bolus, multiple                                          |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Multiple dosing - 2 cmt              |two-compartment, infusion, multiple                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-pk.R             |Multiple dosing - 2 cmt              |two-compartment, oral, multiple                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN2 same as ODE - initial condition                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN2 same as ODE - GUT,bolus,addl                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN2 same as ODE - GUT,infus,addl                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN2 same as ODE - CENT,infus,addl                                      |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN2 same as ODE - CENT,infus,ss,addl                                   |  1|      0|FALSE   |FALSE |       0|      1|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN2 same as ODE - GUT,bolus,ss,addl                                    |  1|      0|FALSE   |FALSE |       0|      1|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - initial condition                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - initial condition                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - GUT,bolus,addl                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - GUT,infus,addl                                       |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - CENT,infus,addl                                      |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - CENT,infus,ss,addl                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |ADVAN4 same as ODE - GUT,bolus,ss,addl                                    |  2|      0|FALSE   |FALSE |       0|      2|
|test-pkmodel.R        |Compare PKMODEL with equivalent ODEs |Incorrect number of compartments causes error                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-plugin.R         |PLUGIN: Rcpp                         |Rcpp all distributions                                                    |  3|      0|FALSE   |FALSE |       0|      3|
|test-plugin.R         |PLUGIN: simeta                       |resimulate ETAs                                                           |  3|      0|FALSE   |FALSE |       0|      3|
|test-pred.R           |test-pred                            |with no data set                                                          |  3|      0|FALSE   |FALSE |       0|      3|
|test-pred.R           |test-pred                            |data_set with no time                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-pred.R           |test-pred                            |data_set with negative times                                              |  4|      0|FALSE   |FALSE |       0|      4|
|test-pred.R           |test-pred                            |time/TIME required when neq > 0                                           |  1|      0|FALSE   |FALSE |       0|      1|
|test-pred.R           |test-pred                            |time/TIME not required when neq > 0                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-pred.R           |test-pred                            |cmt with pred is zero                                                     |  1|      0|FALSE   |FALSE |       0|      1|
|test-pred.R           |test-pred                            |rate with pred is zero                                                    |  1|      0|FALSE   |FALSE |       0|      1|
|test-pred.R           |test-pred                            |ss with pred is zero                                                      |  1|      0|FALSE   |FALSE |       0|      1|
|test-pred.R           |test-pred                            |amt is ok                                                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-recsort.R        |test-recsort                         |recsort 1 and 2, data                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-recsort.R        |test-recsort                         |recsort 3 and 4, data                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-recsort.R        |test-recsort                         |recsort 2 and 4, addl                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-recsort.R        |test-recsort                         |recsort 1 and 3, addl                                                     |  3|      0|FALSE   |FALSE |       0|      3|
|test-request.R        |test-request                         |Req gets the right variables                                              |  4|      0|FALSE   |FALSE |       0|      4|
|test-request.R        |test-request                         |Req gets the right variables, with request                                |  4|      0|FALSE   |FALSE |       0|      4|
|test-request.R        |Testing various request settings     |Testing request setting                                                   |  2|      0|FALSE   |FALSE |       0|      2|
|test-request.R        |Testing various request settings     |Testing that request is properly set in $SET                              |  4|      0|FALSE   |FALSE |       0|      4|
|test-request.R        |Testing various request settings     |Testing that request is (all) by default                                  |  1|      0|FALSE   |FALSE |       0|      1|
|test-request.R        |Testing various request settings     |Typedef capture                                                           |  5|      0|FALSE   |FALSE |       0|      5|
|test-update.R         |test-update                          |Update parameter - via param                                              |  7|      0|FALSE   |FALSE |       0|      7|
|test-update.R         |test-update                          |Update parameter - via idata                                              |  2|      0|FALSE   |FALSE |       0|      2|
|test-update.R         |test-update                          |Update parameter - via data, not-time-varying                             |  1|      0|FALSE   |FALSE |       0|      1|
|test-update.R         |test-update                          |Update parameter - via data, time-varying                                 |  1|      0|FALSE   |FALSE |       0|      1|
|test-z-alag-f.R       |Set F via F_CMT                      |F is set for compartment 1 and 2                                          |  6|      0|FALSE   |FALSE |       0|      6|
|test-z-alag-f.R       |Set ALAG via ALAG_CMT                |ALAG is set for compartment 1 and 2                                       |  6|      0|FALSE   |FALSE |       0|      6|
|test-z-alag-f.R       |Set ALAG via ALAG_CMT                |F is set for multiple doses                                               |  2|      0|FALSE   |FALSE |       0|      2|
|test-z-alag-f.R       |Set ALAG via ALAG_CMT                |F and ALAG are set from idata                                             |  2|      0|FALSE   |FALSE |       0|      2|
|test-z-alag-f.R       |Set ALAG via ALAG_CMT                |F  is set from data                                                       |  1|      0|FALSE   |FALSE |       0|      1|
|test-z-alag-f.R       |Set ALAG via ALAG_CMT                |ALAG is set from data                                                     |  1|      0|FALSE   |FALSE |       0|      1|
