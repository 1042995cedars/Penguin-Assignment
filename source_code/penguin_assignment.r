setwd

##A t-test on culmen depth length in Adelie vs Gentoo penguins, plot two overlapping histograms.

##Loading libraries
source("functions/libraries.r")

##Loading raw data
write.csv(penguins_raw, "data_raw/penguins_raw.csv")
penguins_raw <- read.csv("data_raw/penguins_raw.csv")
head(penguins_raw)
names(penguins_raw)

##Cleaning data

source("functions/cleaning.r")

penguins_clean <- cleaning(penguins_raw)
names(penguins_clean)
write.csv(penguins_clean, "data_clean/penguins_clean.csv")


##Work out mean of bill depth length in Adelie penguins and Gentoo penguins

adelie = penguins_clean %>%
  filter(species=="Adelie Penguin (Pygoscelis adeliae)")
mean(adelie$culmen_length_mm, na.rm=TRUE)
sd(adelie$culmen_length_mm, na.rm=TRUE)

gentoo = penguins_clean %>% 
  filter(species=="Gentoo penguin (Pygoscelis papua)") 
mean(gentoo$culmen_length_mm, na.rm=TRUE)
sd(gentoo$culmen_length_mm, na.rm=TRUE)

##Checking if data is normally distributed

ggplot(data=adelie) +
  geom_histogram(aes(x=culmen_length_mm)) +
  facet_wrap(gentoo)

ggplot(data=gentoo) +
  geom_histogram(aes(x=culmen_length_mm))

##Shows the histograms stacked on top of eachother (also includes chinstrap penguin histogram which can be ignored)

culmen_histogram <- ggplot(penguins_clean, aes(x = culmen_length_mm, fill=species, color=species)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ species, nrow = 2) +
  scale_color_manual(values=c("#009999", "#999999", "#FF99FF"))+
  scale_fill_manual(values=c("#009999", "#999999", "#FF99FF"))+
  labs(x = "Culmen Length (mm)",
       y = "Count") +
  theme_bw()

culmen_histogram

##Saving the figure as a vector

svglite("figures/fig01_vector.svg", 
        width = 5.9, height = 5.9)
culmen_histogram
dev.off()

##Saving the figure as a png

agg_png("figures/fig01_25x20.png", 
        width = 25, height = 20, units = "cm", res = 600, scaling = 1.0)
culmen_histogram
dev.off()



##Carry out two-sample t-test on just the Adelie penguins and Gentoo penguins

t.test(adelie$culmen_length_mm, gentoo$culmen_length_mm)
