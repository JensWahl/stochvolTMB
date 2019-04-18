all: stochvolTMB.so 
	# Comment here preserves the prior tab
stochvolTMB.so: stochvolTMB.cpp
	Rscript --vanilla -e "TMB::compile('stochvolTMB.cpp')"
clean:
	rm -rf *o
