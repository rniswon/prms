      PROGRAM PRMS_FORTRAN
      !SUBROUTINE GSFLOW_FORTRAN BIND(C,NAME="GSFLOW_FORTRAN")
      !DEC$ ATTRIBUTES DLLEXPORT :: GSFLOW_FORTRAN
!***********************************************************************
! PRMS main that controls time loop
!***********************************************************************
      USE PRMS_CONTROL_FILE, ONLY: Julian_day_absolute
      USE PRMS_MODULE, ONLY: Endtime, Starttime, Model, Logunt, Print_debug, PRMS_output_unit, Model_output_file
      USE PRMS_SET_TIME, ONLY: Nowtime
      USE PRMS_MMFAPI
      USE PRMS_DATA_FILE
      IMPLICIT NONE
! Functions
      INTRINSIC TRANSFER
      EXTERNAL read_control_file, read_parameter_file_dimens, read_prms_data_file, read_data_line
      EXTERNAL read_parameter_file_params, check_parameters_declared, read_error, dattim, gsflow_prms
      INTEGER, EXTERNAL :: setdims, compute_julday
! Declared Parameters
! Local Variables
      INTEGER :: i, dmy, startday, endday, num_ts, iret
      LOGICAL :: AFR
!***********************************************************************
      CALL PRMS_open_module_file(Logunt, 'gsflow.log')

      PRINT 3
      WRITE ( Logunt, 3 )
3     FORMAT (//, 26X, 'U.S. Geological Survey', /, 8X, &
     &        'Coupled Groundwater and Surface-water FLOW model (GSFLOW)', /, &
     &        22X, 'Version 1.2 MODSIM 03/14/2017', //, &
     &        '    An integration of the Precipitation-Runoff Modeling System (PRMS)', /, &
     &        '    and the Modular Groundwater Model (MODFLOW-NWT and MODFLOW-2005)', /)

      CALL read_control_file
      ! Open PRMS module output file
      IF ( Print_debug>-2 ) THEN
        CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
        IF ( iret/=0 ) STOP
      ENDIF

      dmy = setdims()
      ! over write start time with MODSIM start time, check to make sure it's valid with data
      ! start date and modflow_time_zero
      !CALL read_var_name_file
      CALL read_parameter_file_dimens
      CALL read_prms_data_file
      CALL read_parameter_file_params

      CALL gsflow_prms('decl')
      CALL check_parameters_declared()
      !print *, 'after check'
      CALL gsflow_prms('init')
      !print *, 'after init'
      startday = compute_julday(Starttime(1), Starttime(2), Starttime(3))
      Julian_day_absolute = startday
      endday = compute_julday(Endtime(1), Endtime(2), Endtime(3))
      num_ts = endday - startday + 1
      !print *, num_ts, startday, endday, julian_day_absolute

      IF ( Model<2 ) THEN
        DO i = 1, num_ts
          CALL dattim('now', Nowtime)
          CALL read_data_line()
          CALL gsflow_prms('run') !add AFR and exchange vectors and model mode
          Julian_day_absolute = Julian_day_absolute + 1
        ENDDO
        CALL gsflow_prms('clean')
      ELSE
        IF ( AFR ) CALL read_data_line()
      ENDIF

      END PROGRAM PRMS_FORTRAN
      ! END SUBROUTINE GSFLOW_FORTRAN
