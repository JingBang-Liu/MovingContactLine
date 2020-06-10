PROGRAM main

  USE kinds
  USE dictionary
  USE my_read_write
  USE analysis

  IMPLICIT NONE
  
  CALL read_input
  CALL read_data

  CALL calc_dens(radius_tol)
  PRINT*, n_lines
  PRINT*, n_header
  PRINT*, n_particle
END PROGRAM
