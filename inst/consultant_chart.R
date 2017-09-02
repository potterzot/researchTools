library(ggplot2)
set.seed(9876)
df <- data.frame(variable = 1:10, value = sample(c(1:10), replace = TRUE))
ggplot(df, aes(factor(variable), value, fill = factor(variable))) + 
  geom_bar(width = 1)

last_plot() + scale_y_continuous(breaks = 0:10) +
  coord_polar() + labs(x = "", y = "") + opts(legend.position = "none",
  axis.text.x = theme_blank(), axis.text.y = theme_blank(),
  axis.ticks = theme_blank())

df <- ddply(df, .(variable), transform, border = rep(1, value))

ggplot(df, aes(factor(variable))) +
  geom_bar(aes(y = border, width = 1, fill = factor(variable)), position = "stack", stat = "identity", colour = "white") +
  coord_polar() +
  scale_y_continuous(breaks = 0:10) + labs(x = "", y = "") +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks = element_blank())
