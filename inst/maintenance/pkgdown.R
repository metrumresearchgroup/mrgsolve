library(pkgdown)
library(methods)
override = list()
build_site(".", override = list(destination = "DOCS"))
# init_site(".", path = "DOCS")
# build_articles(".", path = "DOCS")
# build_home(".", path = "DOCS")
# build_reference(".", path = "DOCS")

