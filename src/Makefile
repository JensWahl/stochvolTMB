all: sv_likelihood.so 
	# Comment here preserves the prior tab
sv_likelihood.so: sv_likelihood.cpp
	Rscript --vanilla -e "TMB::compile('sv_likelihood.cpp')"
clean:
	rm -rf *o
