## binning x:130-500, y:0.0-121.83, z:11.79-98.0 

############################################################
#####	 	    Argon sandwich	   	   #########
############################################################
variable		T equal 33.0
variable 		V equal vol
variable 		dt equal 5.0
variable 		epsilon equal 0.0656
variable 		sigma equal 3.5

units			real	
atom_style		bond
special_bonds 		fene

dimension 		3	
boundary		p p f	

read_data 		test_atomdata