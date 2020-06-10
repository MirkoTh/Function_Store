# preferred plotting theme in ggplot
library(tidyverse)
library(bestNormalize)

showcase_qt <- function(){
  tbl <- tibble(id = 1:1000, 
                y = rgamma(1000, 1, .01)
  )
  m_qt <- orderNorm(tbl$y)
  tbl$y_normal <- predict(m_qt)
  tbl %>%
    gather(variable, value, -id) %>%
    ggplot(aes(value, fill = variable)) +
    geom_histogram() +
    facet_wrap(~ variable, scales = "free")
}
