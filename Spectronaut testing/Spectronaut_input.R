# Short script to test SpectronautToMSstatsFormat

spectronaut_raw = system.file("tinytest/raw_data/Spectronaut/spectronaut_input.csv",
                              package = "MSstatsConvert")
spectronaut_raw = data.table::fread(spectronaut_raw)
spectronaut_imported = SpectronauttoMSstatsFormat(spectronaut_raw, use_log_file = FALSE)




dat <- vroom("input/Spectronaut/spectronaut_input.csv")
