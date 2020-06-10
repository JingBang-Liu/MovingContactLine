PROGRAM main

  USE kinds
  USE dictionary
  USE my_read_write
  USE analysis

  IMPLICIT NONE
  
  CALL read_input
  CALL read_data

  CALL write_pos_1_Duncan
  !CALL calc_dens(radius_tol)
  PRINT*, n_lines
  PRINT*, n_header
  PRINT*, n_particle
  !PRINT*, pos(1,:,:)
  !PRINT*, pos_1(1,:,:)
  !PRINT*, pos_2(1,:,:)
END PROGRAM
