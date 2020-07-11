PROGRAM main

  USE kinds
  USE dictionary
  USE my_read_write
  USE analysis
  USE write_netcdf

  IMPLICIT NONE
  
  REAL :: T1, T2

  CALL read_input
  CALL cpu_time(T1)
  CALL read_data
  CALL cpu_time(T2)
  PRINT*, "read_data takes ", T2-T1

  CALL write_bins("bins.nc")

  !CALL cpu_time(T1)
  !CALL trim_data
  !CALL cpu_time(T2)
  !PRINT*, "trim_data takes ", T2-T1

  !CALL write_pos_1_Duncan

  !CALL cpu_time(T1)
  !CALL binning_1
  !CALL cpu_time(T2)
  !PRINT*, "binning_1 takes ", T2-T1
  !CALL write_one_contactline
  
  !CALL cpu_time(T1)
  !CALL calc_dens_1(radius_tol)
  !CALL cpu_time(T2)
  !PRINT*, "calc_dens_1 takes ", T2-T1
  
  !CALL write_dens_1
  !CALL write_dens_1_all

  !CALL read_dens_1
  !CALL read_dens_1_all
  
  !PRINT*, "the maximum of dens_1 is ", maxval(dens_1(1,:))
  !PRINT*, "the minimum of dens_1 is ", minval(dens_1(1,:))

  !PRINT*, "the maximum of dens_1_all is ", maxval(dens_1_all(1,:))
  !PRINT*, "the minimum of dens_1_all is ", minval(dens_1_all(1,:))

  !CALL cpu_time(T1)
  !CALL pick_contact_line
  !CALL cpu_time(T2)
  !PRINT*, "pick_contact_line takes ", T2-T1

  !CALL cpu_time(T1)
  !CALL write_xyz_ovito
  !CALL cpu_time(T2)
  !PRINT*, "write_xyz_ovito takes ", T2-T1

  !CALL cpu_time(T1)
  !CALL write_xyz_ovito_1
  !CALL cpu_time(T2)
  !PRINT*, "write_xyz_ovito_1 takes ", T2-T1
  
  !PRINT*, n_lines
  !PRINT*, n_header
  !PRINT*, n_particle
  !PRINT*, pos(1,:,:)
  !PRINT*, pos_1(1,:,:)
  !PRINT*, pos_2(1,:,:)
  PRINT*, time_marks

END PROGRAM
