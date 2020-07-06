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
boundary		f f f	

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
thermo_style 		custom step temp ke pe etotal press
#min_style 		fire
#min_modify 		dmax 0.02
#minimize 		1.0e-5 1.0e-7 200 10000

############################################################
####                  Equilibriation                   #####

velocity 		all create ${T} 12345 mom yes rot yes dist gaussian
fix 			NVE all nve/limit 0.05
fix 			2 all langevin 1 1 10 100000
fix 			30 all wall/reflect xhi EDGE
fix 			300 all wall/reflect xlo EDGE
fix 			40 all wall/reflect yhi EDGE
fix 			400 all wall/reflect ylo EDGE
fix 			50 all wall/reflect zhi EDGE
fix 			500 all wall/reflect zlo EDGE

timestep 		0.002
dump 			1 all custom 100 test_equ_equ.dat id type x y z
run 			2000

unfix 			NVE
unfix 			2
unfix 			30
unfix 			300
unfix 			40
unfix 			400
unfix 			50
unfix 			500
undump 			1

############################################################
####              Shear viscosity calculation          #####

reset_timestep 		0
velocity 		all create $T 293847 mom yes rot yes dist gaussian
fix 			NVT all nvt temp $T $T 0.3
#fix_modify 		NVT temp mobile
fix 			30 all wall/reflect xhi EDGE
fix 			300 all wall/reflect xlo EDGE
fix 			40 all wall/reflect yhi EDGE
fix 			400 all wall/reflect ylo EDGE
fix 			50 all wall/reflect zhi EDGE
fix 			500 all wall/reflect zlo EDGE

variable 		pxy equal pxy
variable 		pxz equal pxz
variable 		pyz equal pyz
fix 			SS all ave/correlate $s $p $d &
			v_pxy v_pxz v_pyz type auto ave running
variable 		v11 equal trap(f_SS[3])*${convert}
variable 		v22 equal trap(f_SS[4])*${convert}
variable 		v33 equal trap(f_SS[5])*${convert}
thermo_style 		custom step temp press v_pxy v_pxz v_pyz v_v11 v_v22 v_v33
dump 			1 all custom 500 1test_bond_sandwich.dat id type x y z

run 			50000
variable 		v equal (v_v11+v_v22+v_v33)/3.0
variable 		ndens equal count(all)/vol
print 			"average viscosity: $v [Pa.s] @ $T K, ${ndens} /A^3"








