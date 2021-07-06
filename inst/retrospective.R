library(bp.delta)
library(purrr)

# set number of cores
options(mc.cores = 4)

fits <- map(germany_cases$date[-c(1:3)],
            ~ forecast(germany_cases, target_date = .),
            horizon = 4,
            save_path = "inst/output",
            max_treedepth = 15,
            adapt_delta = 0.99)