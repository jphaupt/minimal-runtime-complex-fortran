
FC=gfortran

all: main

# SOURCES=do_stuff.f90 real_or_complex.f90

# main: $(subst .f90,.o,$(SOURCES))
#     $(FC) -o $@ $+

speed_test: speed_test.f90 real_or_complex
	$(FC) -c speed_test.f90
	$(FC) -o speed_test speed_test.o real_or_complex_mod.o

real_or_complex: real_or_complex_mod.f90
	$(FC) -c real_or_complex_mod.f90

main: real_or_complex
	$(FC) -c main.f90
	$(FC) -o main main.o real_or_complex_mod.o

clean:
	rm -f *.o *.mod main speed_test
