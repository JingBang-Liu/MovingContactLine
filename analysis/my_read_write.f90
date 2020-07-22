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
    
    PRINT*, "READ INPUT STARTED"
    OPEN(1, FILE = 'Input')
    !/*********** file variables ***********
    READ(1,*)
    READ(1,*) filename
    READ(1,*) filename_bins
    READ(1,*) data_type
    READ(1,*) n_header
    READ(1,*) n_particle
    READ(1,*) output_gap
    !/********** analysis params**********
    READ(1,*)
    READ(1,*) radius_tol
    READ(1,*) nbin_x
    READ(1,*) nbin_y
    READ(1,*) nbin_z
    READ(1,*) tol_1(1)
    READ(1,*) tol_1(2)
    READ(1,*) tol_1_all(1)
    READ(1,*) tol_1_all(2)
    READ(1,*) XB(1)
    READ(1,*) XB(2)
    READ(1,*) YB(1)
    READ(1,*) YB(2)
    READ(1,*) ZB(1)
    READ(1,*) ZB(2)
    READ(1,*) n_lines
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
    PRINT*, "READ INPUT FINISHED"
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Count how many line the data file have
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE count_lines(filename)
  CHARACTER(LEN=*), INTENT(IN) :: filename
  INTEGER :: io

  PRINT*, "COUNT LINES STARTED"
  OPEN(1,file=filename, iostat=io, status='old')
  IF (io/=0) STOP 'Cannot open file!'

  n_lines = 0
  DO
    READ(1,*,iostat=io)
    IF (io/=0) EXIT
    n_lines = n_lines + 1
  END DO
  CLOSE(1)
  PRINT*, "FILE HAS ", n_lines, " LINES "
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that reads data from data file according to data type
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE read_data

  PRINT*, "STARTED READING DATA from file ", filename
  !CALL count_lines(filename)
  !n_lines = 1035050003
  !n_lines = (30*30*23+1)*500 + 3
  IF (data_type == 'xyz') THEN
    CALL count_types(filename)
    CALL read_xyz(filename)
  ELSE IF (data_type == 'bins') THEN
    CALL read_bins(filename)
  ENDIF
  PRINT*, "FINISHED READING DATA from file ", filename
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
    
    ALLOCATE(pos(time_marks,n_particle,5))
    ALLOCATE(pos_1(time_marks,n_type_1,3))
    ALLOCATE(pos_2(time_marks,n_type_2,3))
    ALLOCATE(pos_3(time_marks,n_type_3,3))

    OPEN (1, file = filename)
    DO i=1,5
      READ(1,*)
    END DO
    READ(1,*) XD(1), XD(2)
    READ(1,*) YD(1), YD(2)
    READ(1,*) ZD(1), ZD(2)
    CLOSE(1)

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
            pos(i,particle_temp,1) = particle_temp
            pos(i,particle_temp,2) = 1
            pos(i,particle_temp,3) = x_temp
            pos(i,particle_temp,4) = y_temp
            pos(i,particle_temp,5) = z_temp
          CASE(2)
            type_2_count = type_2_count + 1
            pos_2(i,type_2_count,1) = x_temp
            pos_2(i,type_2_count,2) = y_temp
            pos_2(i,type_2_count,3) = z_temp
            pos(i,particle_temp,1) = particle_temp
            pos(i,particle_temp,2) = 2
            pos(i,particle_temp,3) = x_temp
            pos(i,particle_temp,4) = y_temp
            pos(i,particle_temp,5) = z_temp
          CASE(3)
            type_3_count = type_3_count + 1
            pos_3(i,type_3_count,1) = x_temp
            pos_3(i,type_3_count,2) = y_temp
            pos_3(i,type_3_count,3) = z_temp
            pos(i,particle_temp,1) = particle_temp
            pos(i,particle_temp,2) = 3
            pos(i,particle_temp,3) = x_temp
            pos(i,particle_temp,4) = y_temp
            pos(i,particle_temp,5) = z_temp
        END SELECT
      END DO
    END DO
    CLOSE(1)
  END SUBROUTINE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!! Subroutine to read bins data !!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE read_bins(filename)
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER :: i, k, block, tempZ, tempY, tempX
    INTEGER :: first
    REAL(REAL32) :: second, third, fourth, fifth, sixth

    block = nbin_x*nbin_y*nbin_z + 1
    time_marks = int((n_lines-3)/(block))
    block = block - 1
    ALLOCATE(bins(time_marks,nbin_x,nbin_y,nbin_z))
    OPEN(1, file = filename)
    DO i=1,3
      READ(1,*)
    END DO
    DO k = 1, time_marks
      READ(1,*)
      DO i = 1, block
        tempZ = INT(MOD(i-1,nbin_z) + 1)
        tempY = INT(MOD(INT((i-tempZ)/nbin_z),nbin_y) + 1)
        tempX = INT((i-tempZ-nbin_y*(tempY-1))/nbin_z/nbin_y + 1)
        READ(1,*) first, second, third, fourth, fifth, sixth
        bins(k,tempX,tempY,tempZ) = fifth
      END DO
    END DO

    CLOSE(1)

  END SUBROUTINE
  
  SUBROUTINE read_dens_1
    INTEGER :: i,j

    ALLOCATE(dens_1(time_marks,n_type_1))
    OPEN(1, file = 'dens_1.dat')
    DO i=1,time_marks
      DO j=1,n_type_1
        READ(1,*) dens_1(i,j)
      END DO
    END DO
    CLOSE(1)

  END SUBROUTINE

  SUBROUTINE read_dens_1_all
    INTEGER :: i,j

    ALLOCATE(dens_1_all(time_marks,n_type_1))
    OPEN(1, file = 'dens_1_all.dat')
    DO i=1,time_marks
      DO j=1,n_type_1
        READ(1,*) dens_1_all(i,j)
      END DO
    END DO
    CLOSE(1)

  END SUBROUTINE

  !SUBROUTINE read_bins_dens
  !  
  !
  !END SUBROUTINE
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine that output density
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE write_dens_1
    INTEGER :: i,j

    OPEN(1, file = 'dens_1.dat')
    DO i=1,time_marks
      DO j=1,n_type_1
        WRITE(1,"( (E16.9, 2X) )") dens_1(i,j)
      END DO
    END DO
    CLOSE(1)
  END SUBROUTINE

  SUBROUTINE write_dens_1_all
    INTEGER :: i,j

    OPEN(1, file = 'dens_1_all.dat')
    DO i=1,time_marks
      DO j=1,n_type_1
        WRITE(1,"( (E16.9, 2X) )") dens_1_all(i,j)
      END DO
    END DO
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

  SUBROUTINE write_xyz_ovito
    INTEGER :: i,j

    OPEN(1, file = 'xyz_ovito.dat')
    DO i = 1,time_marks
      WRITE(1,"(A14)") "ITEM: TIMESTEP"
      !WRITE(1,*) int((i-1)*output_gap)
      WRITE(1,"(I7)") int((i-1)*output_gap)
      WRITE(1,"(A21)") "ITEM: NUMBER OF ATOMS"
      WRITE(1,"(I8)") n_particle
      WRITE(1,"(A25)") "ITEM: BOX BOUNDS pp pp pp"
      WRITE(1,"( 2(E16.9, 2X) )") XD(1), XD(2)
      WRITE(1,"( 2(E16.9, 2X) )") YD(1), YD(2)
      WRITE(1,"( 2(E16.9, 2X) )") ZD(1), ZD(2)
      WRITE(1,"(A25)") "ITEM: ATOMS id type x y z"
      DO j = 1,n_particle
        WRITE(1,"( (I7, 2X), (I1, 2X), 3(E16.9, 2X) )") int(pos(i,j,1)),int(pos(i,j,2)),pos(i,j,3),pos(i,j,4),pos(i,j,5) 
      END DO
    END DO
    CLOSE(1)

  END SUBROUTINE

  SUBROUTINE write_xyz_ovito_1
    INTEGER :: i,j
    INTEGER :: line_num

    OPEN(1, file='xyz_ovito_1.dat')
    DO i = 1,time_marks
      line_num = 0
      WRITE(1,"(A14)") "ITEM: TIMESTEP"
      !WRITE(1,*) int((i-1)*output_gap)
      WRITE(1,"(I7)") int((i-1)*output_gap)
      WRITE(1,"(A21)") "ITEM: NUMBER OF ATOMS"
      WRITE(1,"(I8)") n_type_1
      WRITE(1,"(A25)") "ITEM: BOX BOUNDS pp pp pp"
      WRITE(1,"( 2(E16.9, 2X) )") XD(1), XD(2)
      WRITE(1,"( 2(E16.9, 2X) )") YD(1), YD(2)
      WRITE(1,"( 2(E16.9, 2X) )") ZD(1), ZD(2)
      WRITE(1,"(A25)") "ITEM: ATOMS id type x y z"
      DO j = 1,n_type_1
        line_num = line_num + 1
        WRITE(1,"( (I7, 2X), (I1, 2X), 3(E16.9, 2X) )") int(line_num),int(1),pos_1(i,j,1),pos_1(i,j,2),pos_1(i,j,3) 
      END DO
    END DO
    CLOSE(1)

  END SUBROUTINE

  SUBROUTINE write_one_contactline
  INTEGER :: i, j

  OPEN(1, file='contactline_matrix.dat')
  DO i=1,time_marks
    DO j=1,nbin_x
      WRITE(1,*) bins_1(i,j,:,1)
    END DO
  END DO
  CLOSE(1)
  END SUBROUTINE

END MODULE
