setClass("MsExperiment",
         slots = c(Spectra = "Spectra",
                   Features = "Features",
                   colData = "DataFrame",
                   metadata = "list"))

setMethod("show", "MsExperiment",
          function(object) cat("Object of class", class(object), "\n"))
