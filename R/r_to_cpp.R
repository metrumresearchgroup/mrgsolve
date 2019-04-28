# 
# 
# cpp_vector <- function(name, x, type="double", init=0) {
#   n <- length(x)
#   decl <- paste0("std::vector<",type,"> ", name, ";")
#   assig <- paste0(name, ".assign(", n, ",", init, ");")
#   init <- paste0(name, "[", seq_along(x)-1, "] = ", x, ";")
#   list(decl, c(assig,init) ,name)
# }
# 
# cpp_scalar <- function(name, x, type = "double", init=0) {
#   decl <- paste0(type, " ", name, ";")
#   assign <- paste0(name, " = ", x, ";")
#   list(decl, assign, name)
# }
# 
# cpp_map <- function(name, x, type1="std::string", type2="double") {
#   keys <- names(x)
#   if(is.null(keys)) stop("x must be a named vector")
#   if(!all(nchar(keys) > 0)) stop("x must be a named vector")
#   decl <- paste0("std::map<", type1, ",", type2,"> ", name, ";")
#   assig <- paste0(name, ".insert(std::make_pair(\"", keys,"\",", x,"));")
#   assig <- paste0(name, "[\"", keys,"\"] = ", x,";")
#   list(decl, assig, name)
# }
# 
# r_to_cpp <- function(class_name, ..., output=NULL, quiet = FALSE) {
#   vars <- list(...)
#   decl <- sapply(vars, "[[",1)
#   assig <- sapply(vars, "[[", 2)
#   var_names <- sapply(vars, "[[", 3)
#   preamble <- c("#include <vector>", "#include <map>", "#include <string>")
#   constr <- paste0(class_name, "();")
#   start_constr <- paste0(class_name, "::", class_name, "() {")
#   end_block <- "};"
#   start_class <- paste0("class ", class_name, " {")
#   public <- "public: "
#   body <- c(start_class, public, constr, decl, end_block)
#   constructor <- c(start_constr, unlist(assig), end_block)
#   class <- c(preamble, "", body, "", constructor)
# 
#   if(!quiet) {
#     message("Class: ", class_name)
#     message("Variables: ", paste0(var_names, collapse = ", "))
#   }
#   if(!is.null(output)) {
#     if(!quiet) message("Writing file: ", output)
#     cat(class, sep = "\n", file = output)
#   }
#   if(!quiet) message("------")
#   return(class)
# }
# 
# 
# 
# 
# 
# 
