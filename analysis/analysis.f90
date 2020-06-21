MODULE analysis

  USE kinds
  USE dictionary

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE trim_data
    INTEGER :: i,j
    
    DO i=1,time_marks
      DO j=1,n_particle
        IF (pos(i,j,3)<XD(1)) THEN
          pos(i,j,3) = pos(i,j,3) + XD(2)
        ELSE IF (pos(i,j,3)>XD(2)) THEN
          pos(i,j,3) = pos(i,j,3) - XD(2)
        END IF
        IF (pos(i,j,4)<YD(1)) THEN
          pos(i,j,4) = pos(i,j,4) + YD(2)
        ELSE IF (pos(i,j,4)>YD(2)) THEN
          pos(i,j,4) = pos(i,j,4) - YD(2)
        END IF
        IF (pos(i,j,5)<ZD(1)) THEN
          pos(i,j,5) = pos(i,j,5) + ZD(2)
        ELSE IF (pos(i,j,5)>ZD(2)) THEN
          pos(i,j,5) = pos(i,j,5) - ZD(2)
        END IF
      END DO
    END DO
    DO i=1,time_marks
      DO j=1,n_type_1
        IF (pos_1(i,j,1)<XD(1)) THEN
          pos_1(i,j,1) = pos_1(i,j,1) + XD(2)
        ELSE IF (pos_1(i,j,1)>XD(2)) THEN
          pos_1(i,j,1) = pos_1(i,j,1) - XD(2)
        END IF
        IF (pos_1(i,j,2)<YD(1)) THEN
          pos_1(i,j,2) = pos_1(i,j,2) + YD(2)
        ELSE IF (pos_1(i,j,2)>YD(2)) THEN
          pos_1(i,j,2) = pos_1(i,j,2) - YD(2)
        END IF
        IF (pos_1(i,j,3)<ZD(1)) THEN
          pos_1(i,j,3) = pos_1(i,j,3) + ZD(2)
        ELSE IF (pos_1(i,j,3)>ZD(2)) THEN
          pos_1(i,j,3) = pos_1(i,j,3) - ZD(2)
        END IF
      END DO
    END DO

  END SUBROUTINE

  SUBROUTINE calc_dens(radius_tol)
    REAL(REAL64), INTENT(IN) :: radius_tol
    INTEGER :: i, j, k
    REAL(REAL64) :: distance_temp

    ALLOCATE(dens(time_marks,n_particle))
    dens = 0.0_REAL64
    DO i = 1,time_marks
      DO j= 1,n_particle
        DO k = 1, j
          distance_temp = sqrt((pos(i,j,3)-pos(i,k,3))**2+(pos(i,j,4)-pos(i,k,4))**2&
                                +(pos(i,j,5)-pos(i,k,5))**2)
          IF (distance_temp < radius_tol) THEN
            dens(i,j) = dens(i,j) + 1
            dens(i,k) = dens(i,k) + 1
          ENDIF 
        END DO
      END DO
    END DO
  END SUBROUTINE

  SUBROUTINE calc_dens_1(radius_tol)
    REAL(REAL64), INTENT(IN) :: radius_tol
    INTEGER :: i, j, k
    REAL(REAL64) :: distance_temp

    ALLOCATE(dens_1(time_marks,n_type_1))
    ALLOCATE(dens_1_all(time_marks,n_type_1))
    dens_1 = 0.0_REAL64
    dens_1_all = 0.0_REAL64
    DO i = 1,time_marks
      DO j= 1,n_type_1
        DO k = 1, n_type_2
          distance_temp = sqrt((pos_1(i,j,1)-pos_2(i,k,1))**2+(pos_1(i,j,2)-pos_2(i,k,2))**2&
                                +(pos_1(i,j,3)-pos_2(i,k,3))**2)
          IF (distance_temp < radius_tol) THEN
            dens_1_all(i,j) = dens_1_all(i,j) + 1
          END IF
        END DO
        DO k = 1, j
          distance_temp = sqrt((pos_1(i,j,1)-pos_1(i,k,1))**2+(pos_1(i,j,2)-pos_1(i,k,2))**2&
                                +(pos_1(i,j,3)-pos_1(i,k,3))**2)
          IF (distance_temp < radius_tol) THEN
            dens_1(i,j) = dens_1(i,j) + 1
            dens_1(i,k) = dens_1(i,k) + 1
            dens_1_all(i,j) = dens_1_all(i,j) + 1
            dens_1_all(i,k) = dens_1_all(i,k) + 1
          ENDIF 
        END DO
      END DO
    END DO
    dens_1 = dens_1 / radius_tol / radius_tol
    dens_1_all = dens_1_all / radius_tol / radius_tol
  END SUBROUTINE

  SUBROUTINE pick_contact_line
    INTEGER :: i,j, n_particles_temp
    CHARACTER(LEN=300) :: line

    ALLOCATE(contact_line_index_1(time_marks,n_type_1))
    contact_line_index_1 = 0
    DO i=1,time_marks
      DO j=1,n_type_1
        IF ((dens_1(i,j)<tol_1(2)) .AND. (dens_1(i,j)>tol_1(1))) THEN
          contact_line_index_1(i,j) = contact_line_index_1(i,j) + 1
        END IF
        IF ((dens_1_all(i,j)<tol_1_all(2)) .AND. (dens_1_all(i,j)>tol_1_all(1))) THEN
          contact_line_index_1(i,j) = contact_line_index_1(i,j) + 1
        END IF
      END DO
    END DO

    OPEN(1, FILE='contact_line_ovito_1.dat', ACTION='WRITE')
    DO i=1,time_marks
      OPEN(2, STATUS='SCRATCH', ACTION='READWRITE')
      n_particles_temp = 0
      WRITE(2,"(A14)") "ITEM: TIMESTEP"
      WRITE(2,"(I7)") int((i-1)*output_gap)
      WRITE(2,"(A21)") "ITEM: NUMBER OF ATOMS"
      !WRITE(2,"(I10)") 0
      WRITE(2,"(A25)") "ITEM: BOX BOUNDS pp pp pp"
      WRITE(2,"( 2(E16.9, 2X) )") XD(1), XD(2)
      WRITE(2,"( 2(E16.9, 2X) )") YD(1), YD(2)
      WRITE(2,"( 2(E16.9, 2X) )") ZD(1), ZD(2)
      WRITE(2,"(A25)") "ITEM: ATOMS id type x y z"
      DO j=1,n_type_1
        SELECT CASE (contact_line_index_1(i,j))
          CASE(2)
            n_particles_temp = n_particles_temp + 1
            WRITE(2,"( (I7, 2X), (I1, 2X), 3(E16.9, 2X) )") int(n_particles_temp),int(1),pos_1(i,j,1), pos_1(i,j,2), pos_1(i,j,3)
        END SELECT
      END DO
      REWIND(2)
      DO j=1,3
        READ(2,'(A)') line
        WRITE(1,'(A)') trim(line)
      END DO
      WRITE(1,"(I10)") n_particles_temp
      DO j=1,5
        READ(2,'(A)') line
        WRITE(1,"(A)") trim(line)
      END DO
      DO j=1,n_particles_temp
        READ(2,'(A)') line
        WRITE(1,'(A)') trim(line)
      END DO
      CLOSE(2)
    END DO
    CLOSE(1)
  END SUBROUTINE

  SUBROUTINE binning_1
    INTEGER :: binI, binJ, binK
    INTEGER :: i,j
  

    ALLOCATE(bins_1(time_marks,nbin_x,nbin_y,nbin_z))
    bins_1 = 0.0_REAL64
    DO i=1,time_marks
      DO j=1,n_type_1
        binI = INT(CEILING(nbin_x*(pos_1(i,j,1)-XD(1))/(XD(2)-XD(1))))
        binJ = INT(CEILING(nbin_y*(pos_1(i,j,2)-YD(1))/(YD(2)-YD(1))))
        binK = INT(CEILING(nbin_z*(pos_1(i,j,3)-ZD(1))/(ZD(2)-ZD(1))))
        bins_1(i,binI,binJ,binK) = bins_1(i,binI,binJ,binK) + 1
      END DO
    END DO

  END SUBROUTINE

END MODULE
