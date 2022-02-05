# required packages

packages <- c("dplyr","forcats","ggplot2","ggrepel","prob","cowplot","stringr","tidyr")
versions <- lapply(packages, packageVersion)

for(i in seq_len(length(packages))) {

  print(
    paste0(
      packages[i]," (>= ", versions[[i]][1], "),")
        )


}
