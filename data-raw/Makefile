
default: $(patsubst %,../inst/%_vignette.rda,basic intermediate)

R = $(strip Rscript $^ $(1) $@)

../inst:
	mkdir -p $@

../inst/%.rda: %.R config.R
	$(call R)