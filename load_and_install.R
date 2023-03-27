load_and_install <- function(package_list) {
  for (i in 1:length(package_list)) {
    if (
      !(package_list[i] %in% installed.packages())
    ) {
      install.packages(packate_list[i])
    }
    lapply(package_list[i], library, character.only = T)
  }
}