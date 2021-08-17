#!bin/bash

nohup /
  Rscript -e "targets::tar_make_future(workers = future::availableCores())" /
  > targets.out 2>&1 &
