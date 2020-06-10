MODULE dictionary

  USE kinds

  IMPLICIT NONE

  SAVE

  !!!!!!!!!!!!!!!!!! read data variables !!!!!!!!!!!!!!!!!
  CHARACTER(len=100) :: filename
  CHARACTER(len=100) :: data_type
  REAL(REAL64), DIMENSION(:,:,:), ALLOCATABLE :: pos
  INTEGER :: n_header ! number of lines within the header
  INTEGER :: n_particle ! number of particles
  INTEGER :: n_lines ! number of lines the data file have
  INTEGER :: time_marks ! number of iterations

  !!!!!!!!!!!!!!!!!!! analysis variables !!!!!!!!!!!!!!!!
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: dens
  REAL(REAL64) :: radius_tol ! radius cutoff for density 

END MODULE

