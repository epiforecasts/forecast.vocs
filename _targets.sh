#!bin/bash

nohup \
  Rscript -e "targets::tar_make_future(workers = future::availableCores())" \
  > targets.log 2>&1 &
