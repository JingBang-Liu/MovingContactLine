## binning x:60-160, y:0.0-34.4, z:2.8-29.0 
## binning x:60-90 x:120-150

############################################################
#####	 	    Argon sandwich	   	   #########
############################################################
variable		T equal 1.0
variable 		V equal vol
variable 		dt equal 0.002

units			lj	
atom_style		bond
special_bonds 		fene

dimension 		3	
boundary		f p f	

read_data         	tank_atomdata
#read_restart 		water.restart.mpiio

############################################################	
#############INTERACTION PARAMETERS#########################
group                   bread     type 2
group                   ham       type 1

mass                    2       1.0  ## atomic mass of platinum
mass                    1       1.0  ## atomic mass of platinum

neighbor        	0.3 bin
neigh_modify		every 1 delay 10 check yes
################### L-J potential #####################
pair_style              lj/cut  2.5

pair_coeff              1 1  1.0 1.0 2.5
pair_coeff              1 2  0.8 1.0 2.5
pair_coeff              2 2  1.0 1.0 2.5
#################### FENE bond #########################
bond_style 		fene
bond_coeff 		1 10.2478 1.4 1.0 1.0
#################### Harmonic potential ##################
fix 			wall bread spring/self 10000.0

thermo 			10
min_style 		fire
min_modify 		dmax 0.005
minimize 		1.0e-5 1.0e-7 500 10000

############################################################	
#############EQUILIBRATION PARAMETERS######################
velocity 		all create ${T} 12345
compute 		myTemp all temp
fix			1 all nvt temp ${T} ${T} 100.0
fix 			2 all langevin 1 1 10 100000
fix 			30 all wall/reflect zhi EDGE
fix 			300 all wall/reflect zlo EDGE
fix 			40 all wall/reflect xhi EDGE
fix 			400 all wall/reflect xlo EDGE
timestep 		${dt}
thermo 			100

dump 			1 all custom 200 equ_tank.dat id type x y z
run 			50000

unfix 			1
unfix 			2
unfix 			30
unfix 			300
unfix 			40
unfix 			400
undump 			1
###########################################################
############### Post parameters############################
reset_timestep  	0
velocity 		all create $T 293849 mom yes rot yes dist gaussian
fix 			1 all nvt temp $T $T 0.3
fix_modify 		1 temp myTemp
fix 			30 all wall/reflect zhi EDGE
fix 			300 all wall/reflect zlo EDGE
fix 			40 all wall/reflect xhi EDGE
fix 			400 all wall/reflect xlo EDGE

#compute 		bins ham chunk/atom bin/3d x 60.0 1.149 y 0.0 1.143 z 2.8 1.1391 bound &
#			x 60.0 160.0 bound y 0.0 34.4 bound z 2.8 29.0
#fix  			d ham ave/chunk 2 5 10 bins density/number norm sample &
#			file small_bond_bins_dens.dat

#compute 		bins1 ham chunk/atom bin/3d x 60.0 1 y 0.0 1.1467 z 2.8 1.1392 bound &
#			x 60.0 90.0 bound y 0.0 34.4 bound z 2.8 29.0
#fix  			d1 ham ave/chunk 2 5 40 bins1 density/number norm sample &
#			file left_small_bond_bins_dens.dat

#compute 		bins2 ham chunk/atom bin/3d x 120.0 1 y 0.0 1.1467 z 2.8 1.1392 bound &
#			x 120.0 150.0 bound y 0.0 34.4 bound z 2.8 29.0
#fix  			d2 ham ave/chunk 2 5 40 bins2 density/number norm sample &
#			file right_small_bond_bins_dens.dat

thermo_style    	custom step temp pe etotal 
thermo_modify	        temp myTemp
thermo			100
#fix            		extra ham print 1000 "$(step) $(temp) $(pe)" append iteration_info.dat &
#			screen no

#restart     		100000 water.restart.mpiio
dump			1 all custom 200 tank.dat id type x y z
run			100000


