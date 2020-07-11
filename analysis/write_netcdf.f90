MODULE write_netcdf

  USE netcdf
  USE dictionary
  USE kinds

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE write_bins(filename_bins)
    ! output file name
    CHARACTER(LEN=*), INTENT(IN) :: filename_bins
    ! number of dimensions to write
    INTEGER, PARAMETER :: ndims = 4
    ! name for dimensions
    CHARACTER(len=2), DIMENSION(ndims) :: dims_bins = (/"TS", "NX", "NY", "NZ"/)
    ! size ids for output variable
    INTEGER, DIMENSION(ndims) :: sizes_bins
    ! dimension ids for output variable
    INTEGER, DIMENSION(ndims) :: dim_ids_bins
    ! variable ids for output variable
    INTEGER :: var_id_bins
    ! file id
    INTEGER :: file_id
    ! for loop
    INTEGER :: i, ierr
    
    ! shape of bins
    sizes_bins = SHAPE(bins)

    ! create netCDF file
    !CALL check(nf90_create(filename_bins, nf90_clobber, file_id))
    ! add information about global runtime data
    !CALL ckeck(nf90_put_att(file_id, NF90_GLOBAL, 'time_marks', time_marks))

    ! define dimension ids
    !DO i=1, ndims
    !  CALL check(nf90_def_dim(file_id, dims_bins(i), sizes_bins(i), dim_ids_bins(i)))
    !END DO

    ! define variable ids
    !CALL check(nf90_def_var(file_id, "bins", NF90_DOUBLE, dim_ids_bins, var_id_bins))

    ! end define
    !CALL check(nf90_enddef(file_id))

    ! write data
    !CALL check(nf90_put_var(file_id, var_id_bins, bins))

    ! close netCDF file
    !CALL check(nf90_close(file_id))

    ierr = nf90_create(filename_bins, NF90_CLOBBER, file_id)

    IF (ierr /= nf90_noerr) THEN
      PRINT*, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    DO i=1,ndims
      ierr = nf90_def_dim(file_id, dims_bins(i), sizes_bins(i), dim_ids_bins(i))
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
    END DO

    ierr = nf90_def_var(file_id, "bins", NF90_DOUBLE, dim_ids_bins, var_id_bins)
    IF (ierr /= nf90_noerr) THEN
      PRINT*, TRIM(nf90_strerror(ierr))
      RETURN
    END IF
    
    ierr = nf90_enddef(file_id)
    IF (ierr /= nf90_noerr) THEN
      PRINT*, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, var_id_bins, bins)
    IF (ierr /= nf90_noerr) THEN
      PRINT*, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_close(file_id)
    IF (ierr /= nf90_noerr) THEN
      PRINT*, TRIM(nf90_strerror(ierr))
      RETURN
    END IF
  END SUBROUTINE

  SUBROUTINE check(stat)
    INTEGER, INTENT(IN) :: stat

    ! Check for error
    IF(stat /= nf90_noerr) THEN
      PRINT *, trim(nf90_strerror(stat))
      STOP "Stopped"
    END IF
  END SUBROUTINE

END MODULE

