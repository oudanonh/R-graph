library(tidyverse)
library(ggplot2)
library(extrafont)
setwd("C:/Users/artid/Desktop/MGMT 190/project")

df <- read.csv('quarterly.csv')
date <- as.Date(df$X,'%m/%d/%Y')
model <- glm(btc_quart_avg ~ cpi + gdp + investment, data = df)
summary(model)

predicted_df <- data.frame(btc_pred = predict(model, df), X=df$X)

ggplot(data = df,aes(x = date, y = btc_quart_avg)) +
    geom_point(size=3,aes(color='#ed7d31')) +
    geom_line(data = predicted_df,aes(x=date, y=btc_pred,color='black'),size=0.9, alpha=0.6) +
    xlab('Year') + ylab('BTC \n% return') +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    scale_color_identity(name = "Model Fit",
                         labels = c('Actual Values','Fitted Line'),
                         guide = "legend")
ggsave('fitted_model.png',dpi=300)