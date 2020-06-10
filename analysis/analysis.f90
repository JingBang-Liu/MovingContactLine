MODULE analysis

  USE kinds
  USE dictionary

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE calc_dens(radius_tol)
    REAL(REAL64), INTENT(IN) :: radius_tol
    INTEGER :: i, j, k
    REAL(REAL64) :: distance_temp

    ALLOCATE(dens(time_marks,n_particle))
    dens = 0.0_REAL64
    DO i = 1,time_marks
      DO j= 1,n_particle
        DO k = 1, j
          distance_temp = sqrt((pos(i,j,1)-pos(i,k,1))**2+(pos(i,j,2)-pos(i,k,2))**2&
                                +(pos(i,j,3)-pos(i,k,3))**2)
          IF (distance_temp < radius_tol) THEN
            dens(i,j) = dens(i,j) + 1
            dens(i,k) = dens(i,k) + 1
          ENDIF 
        END DO
      END DO
    END DO
  END SUBROUTINE


END MODULE
