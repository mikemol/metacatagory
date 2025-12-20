regen-makefile:
	agda -i src/agda --compile src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile
	cp Makefile.generated Makefile
.PHONY: all check md-fix regen-makefile
md-fix: node-deps
	npx prettier --write '**/*.md'
	npx remark . --output