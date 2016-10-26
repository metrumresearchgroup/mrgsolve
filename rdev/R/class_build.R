
new_build <- function(model,project,soloc,code=NULL,udll=FALSE) {
  
  ## Check for spaces in the model name
  if(charthere(model," ")) {
    stop("model name cannot contain spaces.")
  }
  
  if(any(charthere(project,"\n"))) {
    stop("project argument contains newline(s); did you mean to call mcode?",call.=FALSE) 
  }

  env <- new.env()
  
  ## Both project and soloc get normalized

  if(!file_exists(soloc)) {
    stop("soloc directory does not exist.",call.=FALSE) 
  }
  soloc <-   normalizePath(soloc, mustWork=TRUE, winslash="/")
  env$soloc <-   as.character(setup_soloc(soloc,model))
  
  if(!file_exists(project)) {
    stop("project directory does not exist.",call.=FALSE) 
  }
  env$project <- normalizePath(project, mustWork=TRUE, winslash="/")
  
  ## The model file is <stem>.cpp in the <project> directory
  env$modfile <- file.path(project,paste0(model, ".cpp"))
  
  ## If code is passed in as character:
  if(is.character(code)) {
    mod.con <- file(env$modfile, open="w")
    cat(code, "\n", file=mod.con)
    close(mod.con)
  }
  
  if(!file_exists(env$modfile)) {
    stop("The model file ", basename(env$modfile), " does not exist.",call.=FALSE) 
  }
  
  env$md5 <- tools::md5sum(env$modfile)
  
  env$package <- ifelse(udll,rfile(model),model)
  
  return(env)

}




