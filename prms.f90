      PROGRAM PRMS_FORTRAN
!***********************************************************************
! PRMS main that controls time loop
!***********************************************************************
      USE PRMS_CONTROL_FILE, ONLY: Julian_day_absolute
      USE PRMS_MODULE, ONLY: Endtime, Starttime
      USE PRMS_SET_TIME, ONLY: Nowtime
      USE PRMS_LISAPI
      USE PRMS_DATA_FILE
      IMPLICIT NONE
! Functions
      INTRINSIC TRANSFER
      EXTERNAL read_control_file, read_parameter_file_dimens, read_prms_data_file, read_data_line
      EXTERNAL read_parameter_file_params, check_parameters_declared, read_error, dattim
      INTEGER, EXTERNAL :: call_modules, setdims, compute_julday
! Declared Parameters
! Local Variables
      INTEGER :: i, dmy, startday, endday, num_ts
!***********************************************************************
      CALL read_control_file
      !CALL read_var_name_file
      CALL read_parameter_file_dimens
      CALL read_prms_data_file
      CALL read_parameter_file_params
      dmy = setdims()
      dmy = call_modules('decl')
      CALL check_parameters_declared()
      !print *, 'after check'
      dmy = call_modules('init')
      !print *, 'after init'
      startday = compute_julday(Starttime(1), Starttime(2), Starttime(3))
      Julian_day_absolute = startday
      endday = compute_julday(Endtime(1), Endtime(2), Endtime(3))
      num_ts = endday - startday + 1
      !print *, num_ts, startday, endday, julian_day_absolute

      DO i = 1, num_ts
        CALL dattim('now', Nowtime)
        CALL read_data_line()
        dmy = call_modules('run')
        Julian_day_absolute = Julian_day_absolute + 1
      ENDDO
      dmy = call_modules('clean')
      END PROGRAM PRMS_FORTRAN
