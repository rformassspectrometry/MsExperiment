library("testthat")
library("MsExperiment")
library("msdata")
library("S4Vectors")
library("Spectra")

fls <- dir(system.file("sciex", package = "msdata"), pattern = "mzML",
           full.names = TRUE)

mse <- MsExperiment()
df <- data.frame(sample = c("QC1", "QC2"),
                 time = c(1, 2),
                 mzML_file = basename(fls))
sampleData(mse) <- DataFrame(df)
experimentFiles(mse) <- MsExperimentFiles(mzML_file = fls,
                                          other_file = "other_file.txt")
spectra(mse) <- Spectra(fls)

test_check("MsExperiment")
