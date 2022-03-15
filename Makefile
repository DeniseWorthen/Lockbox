#CDF=/apps/netcdf/4.6.1/intel/16.1.150/
#CDF=/apps/netcdf/4.7.0/intel/18.0.5.274
CDF=/glade/u/apps/ch/modulefiles/default/intel/19.1.1/netcdf/4.8.1

#####################################################################
# compiler options
#####################################################################
#FOPT = -O2
FOPT = -C
#FOPT = -convert big_endian
#FOPT = -p

F90 = ifort
#F90 = ifort -warn

#####################################################################
#
#####################################################################

optall = $(opt1) $(opt2) $(opt3) $(opt4)

OBJS = vartypedefs.o testvars.o

istat: $(OBJS)
		$(F90) $(FOPT) -o istat $(OBJS) -L$(CDF)/lib -lnetcdff -lnetcdf

%.o: %.F90
		$(F90) $(FOPT) $(optall) -c -I$(CDF)/include $<
		cpp $(optall) -I$(CDF)/include $*.F90>$*.i

clean:
		/bin/rm -f istat *.o *.i *.mod
