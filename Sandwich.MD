## 

############################################################
#####	 	    Argon sandwich	   	   #########
############################################################
variable		temperature equal 85.0

units			real	
atom_style		atomic
dimension 		3	
boundary		p p p	

read_data		atomdata
#read_restart 		water.restart.mpiio

############################################################	
#############INTERACTION PARAMETERS#########################
group                   bread     type 2
group                   ham       type 1

mass                    2       195.084  ## atomic mass of platinum
mass                    1       39.95  ## atomic mass of platinum

neighbor        	2.0 bin
neigh_modify		every 1 delay 1 check yes

################# hybrid potential #######################
#pair_style 		hybrid/overlay lj/cut 18.7 sw

#pair_coeff              * * sw mW.sw mW NULL
#pair_coeff	    	2 2  lj/cut 15.999 2.47 7.0
#pair_coeff		1 2  lj/cut 0.9 2.8140 7.0
################### L-J potential #####################
pair_style             lj/cut  5.5

pair_coeff             1 1  0.24 3.4  18.7
pair_coeff             1 2  0.168 2.72  18.7
pair_coeff             2 2  15.999 2.47  7.0
######################################################

fix	    		f1 bread setforce 0.0 0.0 0.0
thermo			5
min_style 		fire
minimize    		1.0e-5 1.0e-7 200 10000	
	

############################################################	
#############EQUILIBRATION PARAMETERS######################
velocity 		ham create ${temperature} 12345
compute 		myTemp ham temp
fix			1 ham nvt temp ${temperature} ${temperature} 100.0
fix_modify   		1 temp myTemp

fix	    		f2 bread setforce 0.0 0.0 0.0
velocity		bread set 0.0 0.0 0.0

###########################################################
############### Post parameters############################
thermo_style    	custom step temp pe etotal 
thermo_modify	        temp myTemp
thermo			100
fix            		extra ham print 2000 "$(step) $(temp) $(pe)" append iteration_info.dat &
			screen no

timestep		5.0
reset_timestep 		0
restart     		100000 water.restart.mpiio
dump			1 all custom 500 sandwich.dat id type x y z
run			200000


