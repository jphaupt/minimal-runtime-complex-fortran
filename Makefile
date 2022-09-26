
FC=gfortran

all: main

SOURCES=do_stuff.f90 real_or_complex.f90

# main: $(subst .f90,.o,$(SOURCES))
#     $(FC) -o $@ $+


real_or_complex: real_or_complex.f90
	$(FC) -c real_or_complex.f90

main: real_or_complex
	$(FC) -c do_stuff.f90
	$(FC) -o main do_stuff.o real_or_complex.o
