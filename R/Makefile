
default: zzz_distros.R

distros.csv:
	Rscript -e 'ls("package:stats") |> grep("^r", x = _, value = TRUE) |> grep("^(re[^x]|runmed)", x = _, invert = TRUE, value = TRUE) |> writeLines("$@")'

zzz_distros.R: builder.notR distros.csv rrejig.R
	Rscript $(filter-out %.R,$^) > $@
