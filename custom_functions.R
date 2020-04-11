# preferred plotting theme in ggplot
library(tidyverse)
library(bestNormalize)
plotTheme <- function (plot){
  plot +
    theme(panel.background = element_rect(fill = "black")) +
    theme(panel.grid=element_blank(),panel.background=element_blank()) + theme_bw() +
    theme(plot.title = element_text(size = rel(1.75), hjust = 0)) +
    theme(legend.text = element_text(size = 11), legend.title = element_text(size = 11), 
          legend.key = element_blank(), legend.box = 'horizontal') +
    theme(legend.background = element_rect(colour = "grey50")) +
    theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=14)) +
    theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=14)) +
    theme(legend.position = "bottom")+ theme(strip.text.x = element_text(size = 14))+
    labs(title = "\n") + 
    theme(plot.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# extract coefficients from fitted glmnet object into matrix
extractcoefs_glmnet = function(fit){
  which <- nonzeroCoef(fit$beta)
  nwhich <- length(which)
  df <- as.data.frame(as.matrix(fit$beta[which, , drop = FALSE]))
  names(df) <- prettyNum(log(fit[["lambda"]]), digits = 4)
  df[,ncol(df)+1] <- rownames(df)
  df.r <- melt(df)
  names(df.r) <- c("iv", "lambda_log", "beta")
  df.r$lambda_log <- as.numeric(as.character(df.r$lambda_log))
  df.r
}

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
