###################################
# Makefile for stone_soup package #
###################################

# Fortran compiler and flags
FC=gfortran
#FC=mpiifort
FFLAGS=-Wall -Wextra -O3 -std=f2003 $(shell nf-config --fflags)

# C compiler and flags
CC=gcc
CFLAGS=-Wall -Wextra -O3
#CC=icc

# Command to use for linking and executable
LD=$(FC)
EXE=exec
#TEST=tests
LDFLAGS=-lgcc $(shell nf-config --flibs)

BASICS=kinds.o dictionary.o

OBJECTS=my_read_write.o analysis.o write_netcdf.o\
#	comms.o write_netcdf.o integrators.o \
#	simulation.o 

# Default build target
main : $(BASICS) $(OBJECTS) main.o
	$(LD) $(FFLAGS) $(DEBUGFLAGS) -o $(EXE) $(BASICS) \
	$(OBJECTS) main.o $(LDFLAGS)

# Purge build files and executable
clean :
	rm -f *.o *.mod $(EXE) 

# Rules for building object files
%.o: %.f90
	$(FC) $(FFLAGS) $(DEBUGFLAGS) -c -o $@ $<

# Rules for building object files
%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# Dependencies of various files
# Tells Make how to work in parallel
#shared_data.o: kinds.o


