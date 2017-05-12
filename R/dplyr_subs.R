

arrange__ <- function(x,.dots) {
  if (utils::packageVersion("dplyr") > "0.5.0") {
      dplyr::arrange_at(x,.dots)    
  } else {
      dplyr::arrange_(x,.dots=.dots)
  }
}

select__ <- function(x,.dots) {
  if (utils::packageVersion("dplyr") > "0.5.0") {
    dplyr::select_at(x,.dots)    
  } else {
    dplyr::select_(x,.dots=.dots)
  }
}

# distinct_ <- function(x,.dots) {
#   if (utils::packageVersion("dplyr") > "0.5.0") {
#     dplyr::select_at(x,.dots)    
#   } else {
#     dplyr::distinct_(x,.dots=.dots)
#   }
# }
