load_and_install <- function(package_list) {
  for (package in package_list) {
    if (
      !(package %in% installed.packages())
    ) {
      install.packages(package)
    }
    lapply(package, library, character.only = T)
  }
}