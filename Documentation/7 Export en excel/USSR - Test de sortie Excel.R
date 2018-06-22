#------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                              USSR - Test de sortie Excel                                                                             #
#------------------------------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 22/06/2018

# version de R : 3.5.0

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Table d'entrainement
df <- data.frame("Date" = Sys.Date()-0:4,
                 "Logical" = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                 "Currency" = paste("$",-2:2),
                 "Accounting" = -2:2,
                 "hLink" = "https://CRAN.R-project.org/",
                 "Percentage" = seq(-1, 1, length.out=5),
                 "TinyNumber" = runif(5) / 1E9, stringsAsFactors = FALSE)

# I. Package XLConnect 
#-------------------------------------------------------------------------------------------------------------------------------------------------------
library(XLConnect)

# I.1. Import des données
#------------------------

# En Xlsx (ok)
wb <- loadWorkbook("testXlsx.xlsx", create = TRUE)
data <- readWorksheet(wb,sheet = 1,  startRow = 0, endRow = 10,
                      startCol = 0, endCol = 0)
data

# En Xls (ok)
wb <- loadWorkbook("testXls.xls", create = TRUE)
data <- readWorksheet(wb,sheet = 1,  startRow = 0, endRow = 10,
                      startCol = 0, endCol = 0)
data


# I.2. Export des données
#------------------------

# En xlsx (ok)
wb <- loadWorkbook("ExportXLConnect.xlsx",create=T)
createSheet(wb,"data")
writeWorksheet(wb,df,"data")
saveWorkbook(wb)

# En Xls (ok)
wb <- loadWorkbook("ExportXLConnect.xls",create=T)
createSheet(wb,"data")
writeWorksheet(wb,df,"data")
saveWorkbook(wb)

# I.3. Ecrire plusieurs data.frame dans une seule feuille
#--------------------------------------------------------
wb <- loadWorkbook("ExportXLConnect2.xls",create=T)
createSheet(wb,"input")
writeWorksheet(wb,df,"input",startRow = 1)
writeWorksheet(wb,mtcars,"input",startRow = 7)
saveWorkbook(wb)


# II. Package Xlsx 
#-------------------------------------------------------------------------------------------------------------------------------------------------------


# III. Package openxlsx 
#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Principal avantage : ne necessite pas d'installation java. Rapide. Haut niveau de personnalisation. Gère beaucoup de format de données. Fonctionne sous shiny !!!
# Dispose de deux vignettes très complètes
# Possibilité d'ouvrir un version temporaire du classeur. Export des graphiques en png (réglage possible du dpi)

# Principal inconvenient : ne gère pas l'import et l'export de fichiers xls.


library(openxlsx)

# III.1. Import des données
#--------------------------

# En xlsx (ok) ne detecte pas le format date
test <- read.xlsx(xlsxFile = "testXlsx.xlsx",detectDates = T)

# En xls (nok)
test <- read.xlsx(xlsxFile = "testXls.xls",detectDates = T)

# III.2. Export des données
#--------------------------
# En xlsx (ok)
openxlsx::write.xlsx(df, file = "writeXLSX1.xlsx")

# En xls (nok)
write.xlsx(iris, file = "writeXLSX1.xls")

# III.3. Ecrire plusieurs data.frame dans une seule feuille
#----------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "writeData auto-formatting")
writeData(wb, 1, df, startRow = 2, startCol = 2)
writeData(wb, 1, df, startRow = 9, startCol = 2, borders = "surrounding")
writeData(wb, 1, df, startRow = 16, startCol = 2, borders = "rows")
writeData(wb, 1, df, startRow = 23, startCol = 2, borders ="columns")
writeData(wb, 1, df, startRow = 30, startCol = 2, borders ="all")

hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")
writeData(wb, 1, df, startRow = 16, startCol = 10, headerStyle = hs1,
          borders = "rows", borderStyle = "medium")

writeData(wb, sheet = 1, x = paste("Hyperlink", 1:5), startRow = 17, startCol = 14)

addWorksheet(wb, "writeDataTable")
writeDataTable(wb, 2, df, startRow = 2, startCol = 2)
writeDataTable(wb, 2, df, startRow = 9, startCol = 2, tableStyle = "TableStyleLight9")
writeDataTable(wb, 2, df, startRow = 16, startCol = 2, tableStyle = "TableStyleLight2")
writeDataTable(wb, 2, df, startRow = 23, startCol = 2, tableStyle = "TableStyleMedium21")
openXL(wb) ## opens a temp version


require(ggplot2)
## set default border Colour and style
wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")
addWorksheet(wb, sheetName = "Motor Trend Car Road Tests", gridLines = FALSE)
addWorksheet(wb, sheetName = "Iris", gridLines = FALSE)
## sheet 1
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, sheet = 1, x = mtcars,
               colNames = TRUE, rowNames = TRUE,
               tableStyle = "TableStyleLight9")
setColWidths(wb, sheet = 1, cols = "A", widths = 18)
## write iris data.frame as excel table
writeDataTable(wb, sheet = 2, iris, startCol = "K", startRow = 2)
qplot(data=iris, x = Sepal.Length, y= Sepal.Width, colour = Species)
insertPlot(wb, 2, xy=c("B", 16)) ## insert plot at cell B16
means <- aggregate(x = iris[,-5], by = list(iris$Species), FUN = mean)
vars <- aggregate(x = iris[,-5], by = list(iris$Species), FUN = var)
## write group means
headSty <- createStyle(fgFill="#DCE6F1", halign="center", border = "TopBottomLeftRight")
writeData(wb, 2, x = "Iris dataset group means", startCol = 2, startRow = 2)
writeData(wb, 2, x = means, startCol = "B", startRow=3, borders="rows", headerStyle = headSty)
## write group variances
writeData(wb, 2, x = "Iris dataset group variances", startCol = 2, startRow = 9)
writeData(wb, 2, x= vars, startCol = "B", startRow=10, borders="columns",
          headerStyle = headSty)
setColWidths(wb, 2, cols=2:6, widths = 12) ## width is recycled for each col
setColWidths(wb, 2, cols=11:15, widths = 15)
# style mean & variance table headers
s1 <- createStyle(fontSize=14, textDecoration=c("bold", "italic"))
addStyle(wb, 2, style = s1, rows=c(2,9), cols=c(2,2))
saveWorkbook(wb, "basics.xlsx", overwrite = TRUE) ## save to working directory



## Create a new workbook
wb <- createWorkbook()
data(tli, package = "xtable")
## data.frame
test.n <- "data.frame"
my.df <- tli[1:10, ]
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.df, borders = "n")
## matrix
test.n <- "matrix"
design.matrix <- model.matrix(~ sex * grade, data = my.df)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = design.matrix)
## aov
test.n <- "aov"
fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = fm1)
## lm
test.n <- "lm"
fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = fm2)
## anova 1
test.n <- "anova"
my.anova <- anova(fm2)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.anova)
## anova 2
test.n <- "anova2"
fm2b <- lm(tlimth ~ ethnicty, data = tli)
my.anova2 <- anova(fm2b, fm2)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.anova2)
## glm
test.n <- "glm"
fm3 <- glm(disadvg ~ ethnicty*grade, data = tli, family = binomial())
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = fm3)
## prcomp
test.n <- "prcomp"
5
pr1 <- prcomp(USArrests)
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = pr1)
## summary.prcomp
test.n <- "summary.prcomp"
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = summary(pr1))
## simple table
test.n <- "table"
data(airquality)
airquality$OzoneG80 <- factor(airquality$Ozone > 80,
                              levels = c(FALSE, TRUE),
                              labels = c("Oz <= 80", "Oz > 80"))
airquality$Month <- factor(airquality$Month,
                           levels = 5:9,
                           labels = month.abb[5:9])
my.table <- with(airquality, table(OzoneG80,Month) )
addWorksheet(wb = wb, sheetName = test.n)
writeData(wb = wb, sheet = test.n, x = my.table)
## survdiff 1
library(survival)
test.n <- "survdiff1"
addWorksheet(wb = wb, sheetName = test.n)
x <- survdiff(Surv(futime, fustat) ~ rx, data = ovarian)
writeData(wb = wb, sheet = test.n, x = x)
## survdiff 2
test.n <- "survdiff2"
addWorksheet(wb = wb, sheetName = test.n)
expect <- survexp(futime ~ ratetable(age=(accept.dt - birth.dt),
                                     sex=1,year=accept.dt,race="white"), jasa, cohort=FALSE,
                  ratetable=survexp.usr)
x <- survdiff(Surv(jasa$futime, jasa$fustat) ~ offset(expect))
writeData(wb = wb, sheet = test.n, x = x)
## coxph 1
test.n <- "coxph1"
addWorksheet(wb = wb, sheetName = test.n)
bladder$rx <- factor(bladder$rx, labels = c("Pla","Thi"))
x <- coxph(Surv(stop,event) ~ rx, data = bladder)
writeData(wb = wb, sheet = test.n, x = x)
## coxph 2
test.n <- "coxph2"
addWorksheet(wb = wb, sheetName = test.n)
x <- coxph(Surv(stop,event) ~ rx + cluster(id), data = bladder)
writeData(wb = wb, sheet = test.n, x = x)
## cox.zph
test.n <- "cox.zph"
6
addWorksheet(wb = wb, sheetName = test.n)
x <- cox.zph(coxph(Surv(futime, fustat) ~ age + ecog.ps, data=ovarian))
writeData(wb = wb, sheet = test.n, x = x)
## summary.coxph 1
test.n <- "summary.coxph1"
addWorksheet(wb = wb, sheetName = test.n)
x <- summary(coxph(Surv(stop,event) ~ rx, data = bladder))
writeData(wb = wb, sheet = test.n, x = x)
## summary.coxph 2
test.n <- "summary.coxph2"
addWorksheet(wb = wb, sheetName = test.n)
x <- summary(coxph(Surv(stop,event) ~ rx + cluster(id), data = bladder))
writeData(wb = wb, sheet = test.n, x = x)
## view without saving
openXL(wb)


















