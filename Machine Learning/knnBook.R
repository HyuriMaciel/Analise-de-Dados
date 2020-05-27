
wbcd <- read.csv("/home/hyuri/Dropbox/study/Machine-Learning-with-R-datasets-master/wisc_bc_data.csv",
                 stringsAsFactors = FALSE)

str(wbcd)

wbcd<- wbcd[-1]

table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                       labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)
  