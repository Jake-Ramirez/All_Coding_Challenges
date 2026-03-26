library(ggplot2)

dataset <- read.csv("MycotoxinData.csv")

#there is big trouble if I didn' do this
dataset$DON <- as.numeric(as.character(dataset$DON))

#Define palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#boxplot representation question 1)
Plot1 <- ggplot(dataset, aes(x = Treatment, y = DON, fill = Cultivar)) + 
          geom_boxplot() +
          scale_fill_manual(values = c("#56B4E9", "#009E73")) +
          geom_point(aes(fill = Cultivar), position = position_jitterdodge(), shape = 21, alpha = 0.6) +
          facet_wrap(~ Cultivar) +
          xlab ("") +
          ylab ("DON (ppm)") +
          theme_classic()
print(Plot1)

#Question number 2)
# 1. change the level of the factor Treatment
dataset$Treatment <- factor(dataset$Treatment, 
                            levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

ggplot(dataset, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#56B4E9", "#009E73")) +
  geom_point(aes(fill = Cultivar), position = position_jitterdodge(), shape = 21, alpha = 0.6) +
  facet_wrap(~ Cultivar) +
  xlab ("") +
  ylab ("DON (ppm)") +
  theme_classic()

#Change y axis question 3)
#y as X15ADON
dataset$X15ADON <- as.numeric(as.character(dataset$X15ADON)) #gotta makes sure they are floats
Plot2 <- ggplot(dataset, aes(x = Treatment, y = X15ADON, fill = Cultivar)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#56B4E9", "#009E73")) +
  geom_point(aes(fill = Cultivar), position = position_jitterdodge(), shape = 21, alpha = 0.6) +
  facet_wrap(~ Cultivar) +
  xlab ("") +
  ylab ("X15ADON") +
  theme_classic()
print(Plot2)
#y as Mass per seed
dataset$MassperSeed_mg <- as.numeric(as.character(dataset$MassperSeed_mg))
Plot3 <- ggplot(dataset, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#56B4E9", "#009E73")) +
  geom_point(aes(fill = Cultivar), position = position_jitterdodge(), shape = 21, alpha = 0.6) +
  facet_wrap(~ Cultivar) +
  xlab ("") +
  ylab ("Seed Mass (mg)") +
  theme_classic()
print(Plot3)

#Question 4)
install.packages("ggpubr")
library(ggpubr)

# Combine the three plots into a single figure
# ncol = 3 and nrow = 1 creates a horizontal layout with three columns
# labels = c("A", "B", "C") adds the subplot identifiers
# common.legend = TRUE merges the legends into one
combined_plot <- ggarrange(Plot1, Plot2, Plot3, 
                           ncol = 3, nrow = 1, 
                           labels = c("A", "B", "C"), 
                           common.legend = TRUE, 
                           legend = "right")

# Display the combined figure
combined_plot
#By default, since we have three separate plot objects
#(plot1, plot2, and plot3), each one carries its own legend for "Cultivar" 
#on the right side. This often looks redundant and takes up valuable plotting space.

# Load necessary library
library(ggpubr)

#QUESTION 5: Adding t-test pairwise comparisons

Plot1.0 <- Plot1 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.signif")
Plot2.0 <- Plot2 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.signif")
Plot3.0 <- Plot3 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.signif")
#All in one plot
fina_plot <- ggarrange(plotlist = Plot1.0, Plot2.0, Plot3.0, labels = "auto", nrow = 1, ncol = 3, common.legend = T)
fina_plot
  
