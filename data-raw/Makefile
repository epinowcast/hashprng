
default: $(patsubst %,../inst/%_vignette.rda,basic intermediate)

R = $(strip Rscript $^ $(1) $@)

../inst ../man/figures:
	mkdir -p $@

../inst/%.rda: %.R config.R | ../inst
	$(call R)

../man/figures/logo.png: make_hexsticker.R ../inst/basic_vignette.rda | ../man/figures
	$(call R)