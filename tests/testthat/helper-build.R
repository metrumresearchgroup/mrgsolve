new_test_build <- function(model = "pk1", project = tempdir()) {
  file.copy(file.path(modlib(), paste0(model, ".cpp")), project, overwrite = TRUE)
  mrgsolve:::new_build(model = model, project = project)
}

