MODULE dictionary

  USE kinds

  IMPLICIT NONE

  SAVE

  !!!!!!!!!!!!!!!!!! read data variables !!!!!!!!!!!!!!!!!
  CHARACTER(len=100) :: filename
  CHARACTER(len=100) :: data_type
  REAL(REAL64), DIMENSION(:,:,:), ALLOCATABLE :: pos
  REAL(REAL64), DIMENSION(:,:,:), ALLOCATABLE :: pos_1
  REAL(REAL64), DIMENSION(:,:,:), ALLOCATABLE :: pos_2
  REAL(REAL64), DIMENSION(:,:,:), ALLOCATABLE :: pos_3
  INTEGER :: n_header ! number of lines within the header
  INTEGER :: n_particle ! number of particles
  INTEGER :: n_lines ! number of lines the data file have
  INTEGER :: time_marks ! number of iterations
  INTEGER :: n_type_1, n_type_2, n_type_3
  REAL(REAL64), DIMENSION(2) :: XD, YD, ZD  ! size of simulation box
  INTEGER :: output_gap  ! time gap between outputs
  INTEGER :: time_marks_dens

  !!!!!!!!!!!!!!!!!!! analysis variables !!!!!!!!!!!!!!!!
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: dens, dens_1, dens_1_all
  REAL(REAL64) :: radius_tol ! radius cutoff for density 
  INTEGER :: nbin_x, nbin_y, nbin_z
  REAL(REAL64), DIMENSION(:,:,:,:), ALLOCATABLE :: bins_1, bins_2, bins
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: contact_line_index_1
  REAL(REAL64), DIMENSION(2) :: tol_1, tol_1_all
  REAL(REAL64), DIMENSION(2) :: XB, YB, ZB ! size of bins

END MODULE

