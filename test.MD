## binning x:130-500, y:0.0-121.83, z:11.79-98.0 

############################################################
#####	 	    Argon sandwich	   	   #########
############################################################
variable		T equal 1.0
variable 		V equal vol
variable 		dt equal 0.002
variable 		p equal 400
variable 		s equal 5
variable 		d equal $p*$s

variable 		kB equal 1.3806504e-23
variable 		atm2Pa equal 101325.0
variable 		A2m equal 1.0e-10
variable 		fs2s equal 1.0e-15
variable 		epsilon equal ${kB}*$T
variable 		sigma equal 3.5e-10
variable 		tau equal 2.318e-12
variable 		convert equal ${epsilon}*${tau}/${sigma}/${sigma}/${sigma}

units			lj	
atom_style		bond
special_bonds 		fene

dimension 		3	
boundary		p p p	

read_data 		test_atomdata

############################################################
#####            Interaction Parameters               ######

group  			ham    type 1
mass  			1   1.0
neighbor 		0.3 bin
neigh_modify 		every 1 delay 10 check yes

############################################################
####                  L-J Potential                    #####

pair_style 		lj/cut   2.5
pair_coeff 		1 1 1.0 1.0 2.5
############################################################
####                  FENE Bond                        #####

bond_style 		fene
bond_coeff 		1 10.2478 1.4 1.0 1.0

############################################################
####                  Minimization                     #####

thermo 			$d
min_style 		fire
min_modify 		dmax 0.02
minimize 		1.0e-5 1.0e-7 200 10000

############################################################
####                  Equilibriation                   #####

velocity 		all create ${T} 12345 mom yes rot yes dist gaussian
fix 			NVT all nvt temp $T $T 10
run 			150000

############################################################
####              Shear viscosity calculation          #####

reset_timestep 		0
variable 		pxy equal pxy
variable 		pxz equal pxz
variable 		pyz equal pyz
fix 			SS all ave/correlate $s $p $d &
			v_pxy v_pxz v_pyz type auto ave running
variable 		v11 equal trap(f_SS[3])*${convert}
variable 		v22 equal trap(f_SS[4])*${convert}
variable 		v33 equal trap(f_SS[5])*${convert}
thermo_style 		custom step temp press v_pxy v_pxz v_pyz v_v11 v_v22 v_v33
dump 			1 all custom 500 test_bond_sandwich.dat id type x y z

run 			200000
variable 		v equal (v_v11+v_v22+v_v33)/3.0
variable 		ndens equal count(all)/vol
print 			"average viscosity: $v [Pa.s] @ $T K, ${ndens} /A^3"








