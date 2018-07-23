#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                 USSR - Test d'export de graphiques                                                                                    #
#-------------------------------------------------------------------------------------------------------------------------------------------------------#

# Nicolas Kempf

# MAJ : 23.07.2018

# I. Export en vectoriel
#--------------------------------------------------------------------------------------------------------------------------------------------------------

# Plus d'informations : http://larmarange.github.io/analyse-R/export-de-graphiques.html

# I.1. R de base
#---------------

boxplot(rnorm(100))
# Png
dev.print(device = png, file = "Documentation/7 Export en excel/boxplot.png", width = 600)

# svg
dev.print(device = svg, file = "Documentation/7 Export en excel/boxplot.svg")

# I.2. ggoplot2
#--------------

# install 'ggalt' pkg
# devtools::install_github("hrbrmstr/ggalt")
options(scipen = 999)
library(ggplot2)
library(ggalt)
midwest_select <- midwest[midwest$poptotal > 350000 & 
                            midwest$poptotal <= 500000 & 
                            midwest$area > 0.01 & 
                            midwest$area < 0.1, ]

# Plot
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) +   # draw smoothing line
  geom_encircle(aes(x=area, y=poptotal), 
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   # encircle
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")

# Png
dev.print(device = png, file = "Documentation/7 Export en excel/ggplot2Complex.png", width = 600)

# svg
dev.print(device = svg, file = "Documentation/7 Export en excel/ggplot2Complex.svg",pointsize = 10)

# pdf
dev.print(device = pdf, file = "Documentation/7 Export en excel/ggplot2Complex.pdf")

# postscript
dev.print(device = postscript, file = "Documentation/7 Export en excel/ggplot2Complex.postscript")


# load package and data
options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Scatterplot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)


# II. Exemple d'export d'un classeur pour la PAO
#--------------------------------------------------------------------------------------------------------------------------------------------------------

# II.1. Graphique
#----------------
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")

# Scatterplot
figure_1 <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")
plot(figure_1)

# II.2. Export en vectoriel
#--------------------------
plot(figure_1)
dev.print(device = pdf, file = "figure_1.pdf")

# II.3. Export vers un tableur
#-----------------------------
library(openxlsx)

# Creation du classeur excel
wb <- createWorkbook()

# Creation des styles de mise en forme pour les titres et les sources
titre.style <- createStyle(fontSize=14, textDecoration=c("bold", "italic"))
source.style <- createStyle(fontSize=9, textDecoration=c("italic"))

# Ajout d'une feuille au classeur
sheet <- "Fig1"
addWorksheet(wb, sheet)

# Ajout du titre
writeData(wb,sheet = sheet, x = "Densité de population dans le midwest", startRow = 1)
addStyle(wb, sheet, style = titre.style, rows= 1,cols = 1)

# Ajout du tableau de données
writeData(wb,sheet = sheet, x = midwest[1:10,c("state","area","poptotal","popdensity")], startRow = 3, borders = "all")

# Ajout de la source
writeData(wb,sheet = sheet, x = "Source : midwest", startRow = 15)
addStyle(wb, sheet, style = source.style, rows= 15,cols = 1)

# Ajout du graphique 
plot(figure_1)
insertPlot(wb, sheet = "Fig1", xy = c("F", 3))

# Ajout d'une feuille au classeur
sheet <- "Fig2"
addWorksheet(wb, sheet)

# Creation d'un second graphique
library(ggcorrplot)
data(mtcars)
corr <- round(cor(mtcars), 1)

figure_2 <- ggcorrplot(corr, hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, 
                       lab_size = 3, 
                       method="circle", 
                       colors = c("tomato2", "white", "springgreen3"), 
                       title="Correlogram of mtcars", 
                       ggtheme=theme_bw)



# Ajout du titre
writeData(wb,sheet = sheet, x = "Corrélogramme entre les variables de la table mtcars", startRow = 1)
addStyle(wb, sheet, style = titre.style, rows= 1,cols = 1)

# Ajout du tableau de données
writeData(wb,sheet = sheet, x = corr, startRow = 3, borders = "all",rowNames = TRUE)

# Ajout de la source
writeData(wb,sheet = sheet, x = "Source : mtcars.", startRow = 16)
addStyle(wb, sheet, style = source.style, rows= 16,cols = 1)

# Ajout du graphique 
plot(figure_2)
insertPlot(wb, sheet = "Fig2", xy = c("A", 18))

# Enregistrement du graphique en vectoriel
dev.print(device = pdf, file = "figure_2.pdf")

# Enregistrement du tableur
saveWorkbook(wb, "exportPao_exemple.xlsx", overwrite = TRUE)













