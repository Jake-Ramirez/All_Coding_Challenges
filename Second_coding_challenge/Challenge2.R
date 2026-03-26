library(ggplot2)

dataset <- read.csv("MycotoxinData.csv")

#there is big trouble if I didn' do this
dataset$DON <- as.numeric(as.character(dataset$DON))

#boxplot representation
ggplot(dataset, aes(x = Treatment, y = DON, fill = Cultivar)) + #aes es aesthetics
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "blue")) +
  geom_point(fill = "black", position = position_jitterdodge(), shape = 21, alpha = 0.2) +
  facet_wrap(~ Cultivar) +
  xlab ("") +
  ylab ("DON (ppm)")

#bar chart with transparency
ggplot(dataset, aes(x= Treatment, y = DON, fill = Cultivar,)) +
  scale_fill_manual(values = c("orange", "blue")) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") +
  geom_point(position = position_jitterdodge(), shape = 21, fill = "black", alpha = 0.2) +
  facet_wrap(~ Cultivar) +
  xlab ("")


  
