      PROGRAM PRMS_FORTRAN
      !SUBROUTINE GSFLOW_FORTRAN BIND(C,NAME="GSFLOW_FORTRAN")
      !DEC$ ATTRIBUTES DLLEXPORT :: GSFLOW_FORTRAN
!***********************************************************************
! PRMS main that controls time loop
!***********************************************************************
      USE PRMS_CONTROL_FILE, ONLY: Julian_day_absolute
      USE PRMS_MODULE, ONLY: Endtime, Starttime, Model, Logunt, Model, PRMS_output_unit
      USE PRMS_SET_TIME, ONLY: Nowtime
      USE PRMS_MMFAPI
      USE PRMS_DATA_FILE
      IMPLICIT NONE
! Functions
      INTRINSIC TRANSFER
      EXTERNAL read_control_file, read_parameter_file_dimens, read_data_line, setup_dimens
      EXTERNAL read_parameter_file_params, read_error, dattim, gsflow_prms, setup_params, read_prms_data_file
      INTEGER, EXTERNAL :: setdims, compute_julday
! Declared Parameters
! Local Variables
      INTEGER :: i, dmy, startday, endday, num_ts
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

      CALL read_control_file()

      CALL setup_dimens()

      dmy = setdims()

      ! over write start time with MODSIM start time, check to make sure it's valid with data
      ! start date and modflow_time_zero
      !CALL read_var_name_file
      IF ( Model<2 ) THEN ! add conditions for PRMS-MODSIM and GSFLOW-MODSIM
        WRITE ( PRMS_output_unit, 3 )
        CALL setup_params()
        CALL read_parameter_file_dimens()
      ENDIF

      CALL gsflow_prms('decl')

      IF ( Model<2) CALL read_parameter_file_params() ! add conditions for PRMS-MODSIM and GSFLOW-MODSIM

      !print *, 'after check'
      CALL gsflow_prms('init')
      IF ( Model<2) CALL read_prms_data_file() ! add conditions for PRMS-MODSIM and GSFLOW-MODSIM
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
