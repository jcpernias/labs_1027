## load R libraries
library(tidyverse)
library(readxl)
library(zoo)

theme_set(theme_bw())

theme_update(panel.border = element_blank(),
             axis.line.x = element_line(size = 0.2),
             axis.line.y = element_line(size=0.2),
             text = element_text(size = 9))

ipc_db <- read_xlsx("data/50902.xlsx", range = "a9:c251",
                    col_names = c("tidx", "ipc", "infl"))
infl <- zooreg(ipc_db$infl, start = 2002, frequency = 12)

autoplot(window(infl, start = "2016-1")) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_x_yearmon(breaks = seq(2016, 2022, 1), format = "%Y-%m",
                  minor_breaks = NULL) +
  labs(x = NULL, y = NULL)
