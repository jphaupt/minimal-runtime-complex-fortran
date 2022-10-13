
FC=gfortran
# mcmodel=large allows to compile with large arrays, basically
FFLAGS=-O2 -fPIC -mcmodel=large


all: speed_test prim_speed

# SOURCES=do_stuff.f90 real_or_complex.f90

# main: $(subst .f90,.o,$(SOURCES))
#     $(FC) -o $@ $+

speed_test: speed_test.f90 real_or_complex
	$(FC) $(FFLAGS) -c speed_test.f90
	$(FC) $(FFLAGS) -o speed_test speed_test.o real_or_complex_mod.o

real_or_complex: real_or_complex_mod.f90
	$(FC) $(FFLAGS) -c real_or_complex_mod.f90

prim_speed: prim_speed.f90
	$(FC) $(FFLAGS) -o prim_speed prim_speed.f90

# main: real_or_complex
# 	$(FC) -c main.f90
# 	$(FC) -o main main.o real_or_complex_mod.o

clean:
	rm -f *.o *.mod main speed_test
