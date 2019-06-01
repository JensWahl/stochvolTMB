all: stochvolTMB.so 
	# Comment here preserves the prior tab
stochvolTMB.so: stochvolTMB.cpp
	$(R_HOME)/bin/Rscript --vanilla -e "TMB::compile('stochvolTMB.cpp', '-O0 -g', safebounds=FALSE, safeunload=FALSE)"
clean:
	rm -rf *o
