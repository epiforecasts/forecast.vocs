library(bp.delta)
library(purrr)

# set number of cores
options(mc.cores = 4)

dates <- germany_cases$date[-c(1:3)],
fits <- map(dates,
            ~ forecast(germany_cases, target_date = .),
            horizon = 4,
            save_path = "inst/output",
            max_treedepth = 15,
            adapt_delta = 0.99)
names(fits) <- dates