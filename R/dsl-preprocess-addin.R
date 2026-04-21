addin_run <- function(ctx, apply_fn) {
  sel <- ctx$selection[[1]]
  text <- sel$text
  if(!nchar(text)) { 
    # error and warning don't work quite the way I want
    msg <- c(x = "No text selected; please select the code to process.")
    inform(msg); 
    return(invisible(NULL))
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  result <- apply_fn(text, lines)
  # Don't eat blank line at end of selection
  if(endsWith(text, "\n")) result <- paste0(result, "\n")
  rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  invisible(NULL)
}

# Convert NM addin -------------------------------------------------
addin_apply_convert_nm <- function(text, lines) {
  if(has_block_markers(lines)) {
    result <- modelsplit(lines)
    result <- convert_fort_if_spec(result)
    result <- convert_semicolons_spec(result)
    result <- modelunsplit(result)
  } else {
    result <- convert_fort_if(lines)
    result <- convert_semicolons(result)
  }
  paste(result, collapse = "\n")
}

addin_convert_nm <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  addin_run(ctx, addin_apply_convert_nm)
}

# Add semicolons addin ---------------------------------------------
addin_apply_semicolons <- function(text, lines) {
  if(has_block_markers(lines)) {
    result <- modelsplit(lines)
    result <- convert_semicolons_spec(result)
    result <- modelunsplit(result)
  } else {
    result <- convert_semicolons(lines)
  }
  paste(result, collapse = "\n")
}

addin_add_semicolons <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  addin_run(ctx, \(text, lines) addin_apply_semicolons(text, lines))
}

# Convert pow addin ---------------------------------------------
addin_apply_convert_pow <- function(text, lines) {
  if(has_block_markers(lines)) {
    result <- modelsplit(lines)
    result <- convert_pow_spec(result)
    result <- modelunsplit(result)
  } else {
    result <- convert_pow(lines)
  }
  paste(result, collapse = "\n")
}

addin_convert_pow <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  addin_run(ctx, \(text, lines) addin_apply_convert_pow(text, lines))
}
