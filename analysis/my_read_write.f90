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
  CALL count_types(filename)
  IF (data_type == 'xyz') THEN
    CALL read_xyz(filename)
  ENDIF
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that reads particle types
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE count_types(filename)
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER :: i
    INTEGER :: particle_type_temp, particle_temp

    n_type_1 = 0
    n_type_2 = 0
    n_type_3 = 0

    OPEN(1, file = filename)
    DO i = 1,n_header
      READ(1,*)
    END DO
    DO i = 1,n_particle
      READ(1,*) particle_temp, particle_type_temp
      SELECT CASE (particle_type_temp)
        CASE(1)
          n_type_1 = n_type_1 + 1
        CASE(2)
          n_type_2 = n_type_2 + 1
        CASE(3)
          n_type_3 = n_type_3 + 1
      END SELECT
    END DO
    CLOSE(1)
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
    INTEGER :: type_1_count, type_2_count, type_3_count

    time_marks = int(n_lines/(n_header+n_particle))
    
    ALLOCATE(pos(time_marks,n_particle,3))
    ALLOCATE(pos_1(time_marks,n_type_1,3))
    ALLOCATE(pos_2(time_marks,n_type_2,3))
    ALLOCATE(pos_3(time_marks,n_type_3,3))

    OPEN (1, file = filename)
    DO i = 1,time_marks
      type_1_count = 0
      type_2_count = 0
      type_3_count = 0
      DO j = 1,n_header
        READ(1,*)
      END DO
      DO j = 1,n_particle
        READ(1,*) particle_temp, particle_type_temp, x_temp, y_temp, z_temp
        SELECT CASE (particle_type_temp)
          CASE(1)
            type_1_count = type_1_count + 1
            pos_1(i,type_1_count,1) = x_temp
            pos_1(i,type_1_count,2) = y_temp
            pos_1(i,type_1_count,3) = z_temp
            pos(i,particle_temp,1) = x_temp
            pos(i,particle_temp,2) = y_temp
            pos(i,particle_temp,3) = z_temp
          CASE(2)
            type_2_count = type_2_count + 1
            pos_2(i,type_2_count,1) = x_temp
            pos_2(i,type_2_count,2) = y_temp
            pos_2(i,type_2_count,3) = z_temp
            pos(i,particle_temp,1) = x_temp
            pos(i,particle_temp,2) = y_temp
            pos(i,particle_temp,3) = z_temp
          CASE(3)
            type_3_count = type_3_count + 1
            pos_3(i,type_3_count,1) = x_temp
            pos_3(i,type_3_count,2) = y_temp
            pos_3(i,type_3_count,3) = z_temp
            pos(i,particle_temp,1) = x_temp
            pos(i,particle_temp,2) = y_temp
            pos(i,particle_temp,3) = z_temp
        END SELECT
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

    CLOSE(1)
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that writes position data of liquid particles
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE write_pos_1_Duncan
    INTEGER :: i

    OPEN(1, file = 'pos1.dat')
    DO i=1,n_type_1
      WRITE(1,"( 3(E16.9, 2X) )") pos_1(1,i,1),pos_1(1,i,2),pos_1(1,i,3)
    END DO
    CLOSE(1)
  END SUBROUTINE

END MODULE
