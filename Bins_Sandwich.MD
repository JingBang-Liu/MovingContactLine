## binning x:130-500, y:0.0-121.83, z:11.79-98.0 

############################################################
#####	 	    Argon sandwich	   	   #########
############################################################
variable		T equal 33.0
variable 		V equal vol
variable 		dt equal 5.0

units			real	
atom_style		atomic
dimension 		3	
boundary		p p f	

#read_data  		bond_atomdata
#read_data 		bond_atomdata_small
read_data		atomdata
#read_data         	atomdata_small
#read_restart 		water.restart.mpiio

############################################################	
#############INTERACTION PARAMETERS#########################
group                   bread     type 2
group                   ham       type 1

mass                    2       12  ## atomic mass of platinum
mass                    1       12  ## atomic mass of argon

neighbor        	2.0 bin
neigh_modify		every 1 delay 1 check yes

################# hybrid potential #######################
#pair_style 		hybrid/overlay lj/cut 18.7 sw

#pair_coeff              * * sw mW.sw mW NULL
#pair_coeff	    	2 2  lj/cut 15.999 2.47 7.0
#pair_coeff		1 2  lj/cut 0.9 2.8140 7.0
################### L-J potential #####################
pair_style              lj/cut/opt  5.5

pair_coeff              1 1  0.0656 3.5  8.75
pair_coeff              1 2  0.0656 3.5  8.75
pair_coeff              2 2  0.05248 3.5  8.75
################### Harmonic potential ###################
fix  			wall bread spring/self 5.355101

thermo			10
min_style 		fire
#minimize    		1.0e-5 1.0e-7 200 10000	
min_modify 		dmax 0.01
minimize 		1.0e-5 1.0e-7 200 10000	

############################################################	
#############EQUILIBRATION PARAMETERS######################
velocity 		ham create ${T} 12345
compute 		myTemp ham temp
fix			1 ham nvt temp ${T} ${T} 100.0
fix_modify   		1 temp myTemp

velocity		bread set 0.0 0.0 0.0
timestep 		${dt}

run 			100

###########################################################
############### Post parameters############################
reset_timestep  	0
compute  		bins ham chunk/atom bin/3d x 130.0 4 y 0.0 4.08 z 11.79 4.312 bound x 130.0 500.0 bound y 0.0 121.83 bound z 11.79 98.0
fix  			d ham ave/chunk 1 1 10 bins density/number norm sample file bins_dens.dat
thermo_style    	custom step temp pe etotal 
thermo_modify	        temp myTemp
thermo			100
fix            		extra ham print 2000 "$(step) $(temp) $(pe)" append iteration_info.dat &
			screen no

#restart     		100000 water.restart.mpiio
dump			1 all custom 200 sandwich.dat id type x y z
run			50000


