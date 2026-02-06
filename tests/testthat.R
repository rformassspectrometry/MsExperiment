library("testthat")
library("MsExperiment")
library("S4Vectors")
library("Spectra")
library("MsDataHub")

fls <- unname(c(MsDataHub::X20171016_POOL_POS_1_105.134.mzML(),
                MsDataHub::X20171016_POOL_POS_3_105.134.mzML()))

mse <- MsExperiment()
df <- data.frame(sample = c("QC1", "QC2"),
                 time = c(1, 2),
                 mzML_file = basename(fls))
sampleData(mse) <- DataFrame(df)
experimentFiles(mse) <- MsExperimentFiles(mzML_file = fls,
                                          other_file = "other_file.txt")
spectra(mse) <- Spectra(fls)

test_check("MsExperiment")
