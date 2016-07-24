analysis: clean
	mkdir graphs
	Rscript runner.R

clean:
	rm -rf graphs
