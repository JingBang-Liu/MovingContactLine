MODULE my_read_write

  USE kinds
  USE dictionary

  IMPLICIT NONE
  SAVE

  CONTAINS

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Subroutine that reads Input file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE read_input
    REAL(REAL32) :: temp, test_1, test_2
    
    OPEN(1, FILE = 'Input')
    !/*********** file variables ***********
    READ(1,*)
    READ(1,*) filename
    READ(1,*) data_type
    READ(1,*) n_header
    READ(1,*) n_particle
    !/********** analysis params**********
    READ(1,*)
    READ(1,*) radius_tol
    !/************** test *****************
    READ(1,*)
    READ(1,*) test_1, test_2
    !/************** end ******************
    READ(1,*)
    READ(1,*) temp
    IF (temp/= -10001) THEN
      WRITE(*,*) "Input files wrong!!! Stop!!!"
      STOP
    ENDIF
    CLOSE(1)
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Count how many line the data file have
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE count_lines(filename)
  CHARACTER(LEN=*), INTENT(IN) :: filename
  INTEGER :: io

  OPEN(1,file=filename, iostat=io, status='old')
  IF (io/=0) STOP 'Cannot open file!'

  n_lines = 0
  DO
    READ(1,*,iostat=io)
    IF (io/=0) EXIT
    n_lines = n_lines + 1
  END DO
  CLOSE(1)
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that reads data from data file according to data type
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE read_data

  CALL count_lines(filename)
  IF (data_type == 'xyz') THEN
    CALL read_xyz(filename)
  ENDIF

  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that reads data with only position
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE read_xyz(filename)
    ! Input variables
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER :: i, j
    REAL(REAL64) :: x_temp, y_temp, z_temp
    INTEGER :: particle_type_temp, particle_temp

    time_marks = int(n_lines/(n_header+n_particle))
    
    ALLOCATE(pos(time_marks,n_particle,3))

    OPEN (1, file = filename)
    DO i = 1,time_marks
      DO j = 1,n_header
        READ(1,*)
      END DO
      DO j = 1,n_particle
        READ(1,*) particle_temp, particle_type_temp, x_temp, y_temp, z_temp
        pos(i,j,1) = x_temp
        pos(i,j,2) = y_temp
        pos(i,j,3) = z_temp
      END DO
    END DO
    CLOSE(1)
        

  END SUBROUTINE
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that output density
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE write_dens
    INTEGER :: i

    OPEN(1, file = 'dens.dat')
  END SUBROUTINE

END MODULE
