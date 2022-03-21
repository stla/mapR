dllunload <- function(){
  dyn.unload(
    system.file("libs", "x64", "mapR.dll", package = "mapR")
  )
}

makedoc <- function(){
  roxygen2::roxygenise(load_code = roxygen2::load_installed)
}
