.onUnload <- function (libpath) {
  library.dynam.unload("mapR", libpath)
}