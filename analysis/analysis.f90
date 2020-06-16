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
