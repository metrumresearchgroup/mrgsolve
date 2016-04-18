


library(mrgsolve)

code <- '

$THETA name="foo"
1 2 3 5 6

'



mod <- mread("multimod", tempdir(), code)



param(mod)






