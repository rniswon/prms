!***********************************************************************
! Read and makes available dynamic parameters hru_percent_imperv,
! Wrain_intcp, Srain_intcp, Snow_intcp by HRU from pre-processed files.
! These parameters can be input for any date within the simulation time
! period. Associated states with each parameter are adjusted.
!***********************************************************************
      MODULE PRMS_DYNAMIC_PARAM_READ
        IMPLICIT NONE
        ! Local Variables
        !CHARACTER(LEN=18), SAVE :: MODNAME
        INTEGER, SAVE :: Imperv_frac_unit, Imperv_next_yr, Imperv_next_mo, Imperv_next_day, Imperv_frac_flag
        INTEGER, SAVE :: Wrain_intcp_unit, Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day
        INTEGER, SAVE :: Srain_intcp_unit, Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day
        INTEGER, SAVE :: Snow_intcp_unit, Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day
        INTEGER, SAVE :: Transp_event_unit, Transp_event_next_yr, Transp_event_next_mo, Transp_event_next_day
        INTEGER, SAVE :: Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day, Imperv_stor_unit
        INTEGER, SAVE :: Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day, Soil_rechr_unit
        INTEGER, SAVE :: Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day, Soil_moist_unit
        INTEGER, SAVE :: Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day, Dprst_depth_unit, Dprst_depth_flag
        INTEGER, SAVE :: Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day, Dprst_frac_unit, Dprst_frac_flag
        INTEGER, SAVE :: Covtype_unit, Covtype_next_yr, Covtype_next_mo, Covtype_next_day
        INTEGER, SAVE :: Covden_sum_unit, Covden_sum_next_yr, Covden_sum_next_mo, Covden_sum_next_day, Covden_sum_flag
        INTEGER, SAVE :: Covden_win_unit, Covden_win_next_yr, Covden_win_next_mo, Covden_win_next_day, Covden_win_flag
        INTEGER, SAVE :: Potetcoef_unit, Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day
        INTEGER, SAVE :: Transpbeg_unit, Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day, Transpbeg_flag
        INTEGER, SAVE :: Transpend_unit, Transpend_next_yr, Transpend_next_mo, Transpend_next_day, Transpend_flag
        INTEGER, SAVE :: Fallfrost_unit, Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day, Fallfrost_flag
        INTEGER, SAVE :: Springfrost_unit, Springfrost_next_yr, Springfrost_next_mo, Springfrost_next_day, Springfrost_flag
        INTEGER, SAVE :: Rad_trncf_unit, Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day
        INTEGER, SAVE :: Sro_to_dprst_unit, Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day
        INTEGER, SAVE :: Sro_to_imperv_unit, Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day
        INTEGER, SAVE :: Check_imperv, Wrainintcp_flag, Srainintcp_flag, Snowintcp_flag, Check_dprst_frac
        INTEGER, SAVE :: Soilmoist_flag, Soilrechr_flag, Output_unit
        INTEGER, SAVE :: Snarea_thresh_unit, Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day
        INTEGER, SAVE, ALLOCATABLE :: Itemp(:), Updated_hrus(:)
        REAL, SAVE, ALLOCATABLE :: Temp(:), Temp3(:), Potet_coef(:, :), Soil_rechr_max_frac(:)
      END MODULE PRMS_DYNAMIC_PARAM_READ

!***********************************************************************
!     Main dynamic parameter routine
!***********************************************************************
      INTEGER FUNCTION dynamic_param_read()
      USE PRMS_MODULE, ONLY: Process
      !USE PRMS_DYNAMIC_PARAM_READ, ONLY: MODNAME
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: dynparamrun, dynparaminit
      EXTERNAL print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_dynamic_param_read
!***********************************************************************
      dynamic_param_read = 0

      IF ( Process(:3)=='run' ) THEN
        dynamic_param_read = dynparamrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_dynamic_param_read = 'dynamic_param_read.f90 2018-04-18 11:21:00Z'
        CALL print_module(Version_dynamic_param_read, 'Time Series Data            ', 90)
        !MODNAME = 'dynamic_param_read'
      ELSEIF ( Process(:4)=='init' ) THEN
        dynamic_param_read = dynparaminit()
      ENDIF

      END FUNCTION dynamic_param_read

!***********************************************************************
!     dynparaminit - open files, read to start time, initialize flags and arrays
!***********************************************************************
      INTEGER FUNCTION dynparaminit()
      USE PRMS_DYNAMIC_PARAM_READ
      USE PRMS_MODULE, ONLY: Nhru, Starttime, Dyn_imperv_flag, Dyn_dprst_flag, Dyn_intcp_flag, Dyn_covden_flag, &
     &    Dyn_covtype_flag, Dyn_potet_flag, Dyn_transp_flag, Dyn_soil_flag, Dyn_radtrncf_flag, Dyn_transp_on_flag, &
     &    Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Transp_flag, Dprst_flag, Dyn_fallfrost_flag, &
     &    Dyn_springfrost_flag, Dyn_snareathresh_flag, MAXFILE_LENGTH, Print_debug, PRMS4_flag
      IMPLICIT NONE
      INTEGER, EXTERNAL :: control_string, get_ftnunit
      EXTERNAL read_error, find_header_end, find_current_file_time
      INTRINSIC ABS
! Local Variables
      INTEGER :: year, month, day, istop, ierr
! Control Parameters
      CHARACTER(LEN=MAXFILE_LENGTH) :: imperv_frac_dynamic, imperv_stor_dynamic, dprst_depth_dynamic, dprst_frac_dynamic
      CHARACTER(LEN=MAXFILE_LENGTH) :: wrain_intcp_dynamic, srain_intcp_dynamic, snow_intcp_dynamic, covtype_dynamic
      CHARACTER(LEN=MAXFILE_LENGTH) :: potetcoef_dynamic, transpbeg_dynamic, transpend_dynamic
      CHARACTER(LEN=MAXFILE_LENGTH) :: soilmoist_dynamic, soilrechr_dynamic, radtrncf_dynamic
      CHARACTER(LEN=MAXFILE_LENGTH) :: fallfrost_dynamic, springfrost_dynamic, transp_on_dynamic, snareathresh_dynamic
      CHARACTER(LEN=MAXFILE_LENGTH) :: covden_sum_dynamic, covden_win_dynamic, sro2dprst_perv_dyn, sro2dprst_imperv_dyn
!***********************************************************************
      dynparaminit = 0

      year = Starttime(1)
      month = Starttime(2)
      day = Starttime(3)

      ALLOCATE ( Temp(Nhru), Itemp(Nhru), Updated_hrus(Nhru) )

      Imperv_frac_flag = 0
      istop = 0
      ierr = 0
      IF ( Dyn_imperv_flag==1 .OR. Dyn_imperv_flag==3 ) THEN
        IF ( control_string(imperv_frac_dynamic, 'imperv_frac_dynamic')/=0 ) CALL read_error(5, 'imperv_frac_dynamic')
        CALL find_header_end(Imperv_frac_unit, imperv_frac_dynamic, 'imperv_frac_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Imperv_frac_unit, year, month, day, Imperv_next_yr, Imperv_next_mo, Imperv_next_day)
          Imperv_frac_flag = 1
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_imperv_flag>1 ) THEN
        IF ( control_string(imperv_stor_dynamic, 'imperv_stor_dynamic')/=0 ) CALL read_error(5, 'imperv_stor_dynamic')
        CALL find_header_end(Imperv_stor_unit, imperv_stor_dynamic, 'imperv_stor_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Imperv_stor_unit, year, month, day, &
     &                                Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Dprst_frac_flag = 0
      Dprst_depth_flag = 0
      IF ( Dprst_flag==1 ) THEN
        IF ( Dyn_dprst_flag==1 .OR. Dyn_dprst_flag==3 ) THEN
          IF ( control_string(dprst_frac_dynamic, 'dprst_frac_dynamic')/=0 ) CALL read_error(5, 'dprst_frac_dynamic')
          CALL find_header_end(Dprst_frac_unit, dprst_frac_dynamic, 'dprst_frac_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Dprst_frac_unit, year, month, day, &
     &                                  Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day)
            ALLOCATE ( Temp3(Nhru) )
            Dprst_frac_flag = 1
          ELSE
            istop = 1
          ENDIF
        ENDIF

        IF ( Dyn_dprst_flag==2 .OR. Dyn_dprst_flag==3 ) THEN
          IF ( control_string(dprst_depth_dynamic, 'dprst_depth_dynamic')/=0 ) CALL read_error(5, 'dprst_depth_dynamic')
          CALL find_header_end(Dprst_depth_unit, dprst_depth_dynamic, 'dprst_depth_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Dprst_depth_unit, year, month, day, &
     &                                  Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day)
            Dprst_depth_flag = 1
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Wrainintcp_flag = 0
      Srainintcp_flag = 0
      Snowintcp_flag = 0
      IF ( Dyn_intcp_flag>0 ) THEN
        IF ( Dyn_intcp_flag==1 .OR. Dyn_intcp_flag==3 .OR. Dyn_intcp_flag==5 .OR. Dyn_intcp_flag==7 ) THEN
          Wrainintcp_flag = 1
          IF ( control_string(wrain_intcp_dynamic, 'wrain_intcp_dynamic')/=0 ) CALL read_error(5, 'wrain_intcp_dynamic')
          CALL find_header_end(Wrain_intcp_unit, wrain_intcp_dynamic, 'wrain_intcp_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Wrain_intcp_unit, year, month, day, &
     &                                  Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day)
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Dyn_intcp_flag==2 .OR. Dyn_intcp_flag==3 .OR. Dyn_intcp_flag==6 .OR. Dyn_intcp_flag==7 ) THEN
          Srainintcp_flag = 1
          IF ( control_string(srain_intcp_dynamic, 'srain_intcp_dynamic')/=0 ) CALL read_error(5, 'srain_intcp_dynamic')
          CALL find_header_end(Srain_intcp_unit, srain_intcp_dynamic, 'srain_intcp_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Srain_intcp_unit, year, month, day, &
     &                                  Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day)
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Dyn_intcp_flag>3 ) THEN
          Snowintcp_flag = 1
          IF ( control_string(snow_intcp_dynamic, 'snow_intcp_dynamic')/=0 ) CALL read_error(5, 'snown_intcp_dynamic')
          CALL find_header_end(Snow_intcp_unit, snow_intcp_dynamic, 'snow_intcp_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Snow_intcp_unit, year, month, day, &
     &                                  Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day)
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Covden_win_flag = 0
      Covden_sum_flag = 0
      IF ( Dyn_covden_flag==1 .OR. Dyn_covden_flag==3 ) THEN
        IF ( control_string(covden_sum_dynamic, 'covden_sum_dynamic')/=0 ) CALL read_error(5, 'covden_sum_dynamic')
        CALL find_header_end(Covden_sum_unit, covden_sum_dynamic, 'covden_sum_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Covden_sum_unit, year, month, day, Covden_sum_next_yr, Covden_sum_next_mo,Covden_sum_next_day)
          Covden_sum_flag = 1
        ELSE
          istop = 1
        ENDIF
      ENDIF
      IF ( Dyn_covden_flag==2 .OR. Dyn_covden_flag==3 ) THEN
        IF ( control_string(covden_win_dynamic, 'covden_win_dynamic')/=0 ) CALL read_error(5, 'covden_win_dynamic')
        CALL find_header_end(Covden_win_unit, covden_win_dynamic, 'covden_win_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Covden_win_unit, year, month, day, Covden_win_next_yr, Covden_win_next_mo,Covden_win_next_day)
          Covden_win_flag = 1
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_covtype_flag==1 ) THEN
        IF ( control_string(covtype_dynamic, 'covtype_dynamic')/=0 ) CALL read_error(5, 'covtype_dynamic')
        CALL find_header_end(Covtype_unit, covtype_dynamic, 'covtype_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Covtype_unit, year, month, day, Covtype_next_yr, Covtype_next_mo, Covtype_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_potet_flag>0 ) THEN
        ALLOCATE ( Potet_coef(Nhru,12) )
        IF ( control_string(potetcoef_dynamic, 'potetcoef_dynamic')/=0 ) CALL read_error(5, 'potetcoef_dynamic')
        CALL find_header_end(Potetcoef_unit, potetcoef_dynamic, 'potetcoef_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Potetcoef_unit, year, month, day, Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Transpbeg_flag = 0
      IF ( Dyn_transp_flag==1 .OR. Dyn_transp_flag==3 ) THEN
        IF ( Transp_flag/=1 ) THEN
          PRINT *, 'ERROR, transp_beg input as dynamic parameter but transp_module not transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(transpbeg_dynamic, 'transpbeg_dynamic')/=0 ) CALL read_error(5, 'transpbeg_dynamic')
          CALL find_header_end(Transpbeg_unit, transpbeg_dynamic, 'transpbeg_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Transpbeg_unit, year, month, day, Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day)
            Transpbeg_flag = 1
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Transpend_flag = 0
      IF ( Dyn_transp_flag>1 ) THEN
        IF ( Transp_flag/=1 ) THEN
          PRINT *, 'ERROR, transp_end input as dynamic parameter but transp_module not transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(transpend_dynamic, 'transpend_dynamic')/=0 ) CALL read_error(5, 'transpend_dynamic')
          CALL find_header_end(Transpend_unit, transpend_dynamic, 'transpend_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Transpend_unit, year, month, day, Transpend_next_yr, Transpend_next_mo, Transpend_next_day)
            Transpend_flag = 1
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Fallfrost_flag = 0
      IF ( Dyn_fallfrost_flag==1 ) THEN
        IF ( Transp_flag==1 ) THEN
          PRINT *, 'ERROR, fall_frost input as dynamic parameter but transp_module set to transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(fallfrost_dynamic, 'fallfrost_dynamic')/=0 ) CALL read_error(5, 'fallfrost_dynamic')
          CALL find_header_end(Fallfrost_unit, fallfrost_dynamic, 'fallfrost_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Fallfrost_unit, year, month, day, Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day)
            Fallfrost_flag = 1
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Springfrost_flag = 0
      IF ( Dyn_springfrost_flag==1 ) THEN
        IF ( Transp_flag==1 ) THEN
          PRINT *, 'ERROR, spring_frost input as dynamic parameter but transp_module set to transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(springfrost_dynamic, 'springfrost_dynamic')/=0 ) CALL read_error(5, 'springfrost_dynamic')
          CALL find_header_end(Springfrost_unit, springfrost_dynamic, 'springfrost_dynamic', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Springfrost_unit, year, month, day, Springfrost_next_yr, Springfrost_next_mo, &
     &                                  Springfrost_next_day)
            Springfrost_flag = 1
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_soil_flag>1 ) THEN
        IF ( PRMS4_flag==0 ) ALLOCATE ( Soil_rechr_max_frac(Nhru) )
        IF ( control_string(soilrechr_dynamic, 'soilrechr_dynamic')/=0 ) CALL read_error(5, 'soilrechr_dynamic')
        CALL find_header_end(Soil_rechr_unit, soilrechr_dynamic, 'soilrechr_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Soil_rechr_unit, year, month, day, &
     &                                Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_soil_flag==1 .OR. Dyn_soil_flag==3 ) THEN
        IF ( control_string(soilmoist_dynamic, 'soilmoist_dynamic')/=0 ) CALL read_error(5, 'soilmoist_dynamic')
        CALL find_header_end(Soil_moist_unit, soilmoist_dynamic, 'soilmoist_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Soil_moist_unit, year, month, day, &
     &                                Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_radtrncf_flag==1 ) THEN
        IF ( control_string(radtrncf_dynamic, 'radtrncf_dynamic')/=0 ) CALL read_error(5, 'radtrncf_dynamic')
        CALL find_header_end(Rad_trncf_unit, radtrncf_dynamic, 'radtrncf_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Rad_trncf_unit, year, month, day, &
     &                                Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_snareathresh_flag==1 ) THEN
        IF ( control_string(snareathresh_dynamic, 'snareathresh_dynamic')/=0 ) CALL read_error(5, 'snareathresh_dynamic')
        CALL find_header_end(Snarea_thresh_unit, snareathresh_dynamic, 'snareathresh_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Snarea_thresh_unit, year, month, day, &
     &                                Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_perv_flag==1 ) THEN
        IF ( control_string(sro2dprst_perv_dyn, 'sro2dprst_perv_dynamic')/=0 ) CALL read_error(5, 'sro2dprst_perv_dynamic')
        CALL find_header_end(Sro_to_dprst_unit, sro2dprst_perv_dyn, 'sro2dprst_perv_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Sro_to_dprst_unit, year, month, day, &
     &                                Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_imperv_flag==1 ) THEN
        IF ( control_string(sro2dprst_imperv_dyn, 'sro2dprst_imperv_dynamic')/=0 ) CALL read_error(5, 'sro2dprst_imperv_dynamic')
        CALL find_header_end(Sro_to_imperv_unit, sro2dprst_imperv_dyn, 'sro2dprst_imperv_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Sro_to_imperv_unit, year, month, day, &
     &                                Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_transp_on_flag==1 ) THEN
        IF ( control_string(transp_on_dynamic, 'transp_on_dynamic')/=0 ) CALL read_error(5, 'transp_on_dynamic')
        CALL find_header_end(Transp_event_unit, transp_on_dynamic, 'transp_on_dynamic', ierr, 0, 0)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Transp_event_unit, year, month, day, Transp_event_next_yr, Transp_event_next_mo, &
     &                                Transp_event_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( istop==1 ) STOP 'ERROR in dynamic_param_read initialize procedure'

      IF ( Print_debug>-2 ) THEN
        Output_unit = get_ftnunit(520)
        OPEN ( Output_unit, FILE='dynamic_parameter.out' )
        PRINT '(/,A,//)', 'A summary of dynamic parameter events are written to file: dynamic_parameter.out'
      ENDIF

      END FUNCTION dynparaminit

!***********************************************************************
!     dynparamrun - Read and set dynamic parameters
!***********************************************************************
      INTEGER FUNCTION dynparamrun()
      USE PRMS_DYNAMIC_PARAM_READ
      USE PRMS_MODULE, ONLY: Nhru, Dprst_flag, Dyn_imperv_flag, &
     &    Dyn_covtype_flag, Dyn_radtrncf_flag, Dyn_transp_on_flag, Dyn_snareathresh_flag, &
     &    Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Et_flag, Dyn_potet_flag, PRMS4_flag
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Hru_area, Dprst_clos_flag, &
     &    Hru_percent_imperv, Hru_frac_perv, Hru_imperv, Hru_perv, Dprst_frac, Dprst_open_flag, &
     &    Dprst_area_max, Dprst_area_open_max, Dprst_area_clos_max, Dprst_frac_open, &
     &    Cov_type, Basin_area_inv, NEARZERO, Covden_win, Covden_sum, DNEARZERO
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Epan_coef
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Soil_moist, Soil_rechr, Imperv_stor, Sat_threshold, &
     &    Soil_rechr_max, Soil_moist_max, Imperv_stor_max, Dprst_vol_open, Dprst_vol_clos, Ssres_stor
      USE PRMS_POTET_JH, ONLY: Jh_coef, Jh_coef_hru
      USE PRMS_POTET_PM, ONLY: Pm_n_coef, Pm_d_coef
      USE PRMS_POTET_PT, ONLY: Pt_alpha
      USE PRMS_POTET_HS, ONLY: Hs_krs
      !USE PRMS_POTET_JH_HRU, ONLY: Jh_coef_hru2
      USE PRMS_POTET_HAMON, ONLY: Hamon_coef
      USE PRMS_CLIMATE_HRU, ONLY: Potet_cbh_adj
      USE PRMS_TRANSP_TINDEX, ONLY: Transp_beg, Transp_end
      USE PRMS_TRANSP_FROST, ONLY: Fall_frost, Spring_frost
      USE PRMS_INTCP, ONLY: Wrain_intcp, Srain_intcp, Snow_intcp
      USE PRMS_SNOW, ONLY: Rad_trncf, Snarea_thresh
      USE PRMS_SRUNOFF, ONLY: Sro_to_dprst_perv, Sro_to_dprst_imperv, Dprst_depth_avg, &
     &    Op_flow_thres, Dprst_vol_open_max, Dprst_vol_clos_max, Dprst_vol_thres_open, &
     &    Dprst_vol_open_frac, Dprst_vol_clos_frac, Dprst_vol_frac
      USE PRMS_SOILZONE, ONLY: Basin_soil_rechr, Soil_zone_max, Soil_moist_tot, &
     &    Soil_lower_stor_max, Replenish_frac, Soil_lower_ratio, Soil_lower
!     &    Soil_moist_frac, Cpr_stor_frac, Soil_lower_stor_max, Replenish_frac, &
!     &    Soil_lower_ratio, Soil_lower, Soil_rechr_ratio
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL, DBLE
      EXTERNAL write_dynoutput, is_eof, write_dynparam, write_dynparam_int
      EXTERNAL write_dynparam_potet !, write_dynparam_dble
! Local Variables
      INTEGER :: i, jj, istop
      REAL :: soilmoist, soilrechr, harea, frac_imperv, tmp, hruperv, dprst_areamax
!***********************************************************************
      dynparamrun = 0
      istop = 0

      IF ( Imperv_frac_flag==1 .OR. Dprst_frac_flag==1 ) THEN
        IF ( Imperv_frac_flag==1 ) THEN
          Check_imperv = 0
          IF ( Imperv_next_mo/=0 ) THEN
            IF ( Imperv_next_yr==Nowyear .AND. Imperv_next_mo==Nowmonth .AND. Imperv_next_day==Nowday ) THEN
              READ ( Imperv_frac_unit, * ) Imperv_next_yr, Imperv_next_mo, Imperv_next_day, Temp
              ! Temp has new values, Hru_percent_imperv has old values
              CALL write_dynoutput(Output_unit, Nhru, Updated_hrus, Temp, Hru_percent_imperv, 'hru_percent_imperv')
              ! Temp has new values with negative values set to the old value
              CALL is_eof(Imperv_frac_unit, Imperv_next_yr, Imperv_next_mo, Imperv_next_day)
              Check_imperv = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Dprst_frac_flag==1 ) THEN
          Check_dprst_frac = 0
          Dprst_clos_flag = 0
          Dprst_open_flag = 0
          IF ( Dprst_frac_next_mo/=0 ) THEN
            IF ( Dprst_frac_next_yr==Nowyear .AND. Dprst_frac_next_mo==Nowmonth .AND. Dprst_frac_next_day==Nowday ) THEN
              ! Temp3 has new values, Dprst_frac has old values
              READ ( Dprst_frac_unit, * ) Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day, Temp3
              ! Temp3 has new values with negative values set to the old value
              CALL write_dynoutput(Output_unit, Nhru, Updated_hrus, Temp3, Dprst_frac, 'dprst_frac')
              CALL is_eof(Dprst_frac_unit, Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day)
              Check_dprst_frac = 1
            ENDIF
          ENDIF
        ENDIF

        Basin_soil_moist = 0.0D0
        DO jj = 1, Active_hrus
          i = Hru_route_order(jj)
          IF ( Hru_type(i)==2 ) CYCLE ! skip lake HRUs
          soilmoist = Soil_moist(i)
          soilrechr = Soil_rechr(i)
          harea = Hru_area(i)
          IF ( Dprst_frac_flag==1 ) Dprst_area_max(i) = Dprst_frac(i)*harea
          IF ( Check_imperv==1 ) THEN
            frac_imperv = Temp(i) ! updated value
            IF ( frac_imperv>0.999 ) THEN
              PRINT *, 'ERROR, dynamic value of hru_percent_imperv > 0.999'
              PRINT *, '       Impervious fraction:', frac_imperv, ' HRU:', i
              istop = 1
            ELSEIF ( Imperv_stor(i)>0.0 ) THEN
              IF ( frac_imperv>0.0 ) THEN
                Imperv_stor(i) = Imperv_stor(i)*Hru_percent_imperv(i)/frac_imperv
              ELSE
                PRINT *, 'WARNING, dynamic impervious changed to 0 when impervious storage > 0'
                PRINT *, '         storage added to soil_moist and soil_rechr'
                tmp = Imperv_stor(i)*Hru_percent_imperv(i)/Hru_frac_perv(i)
                soilmoist = soilmoist + tmp
                soilrechr = soilrechr + tmp
                Imperv_stor(i) = 0.0
              ENDIF
            ENDIF
            Hru_percent_imperv(i) = frac_imperv
            Hru_imperv(i) = harea*frac_imperv
          ENDIF

          ! CAUTION: other DPRST parameters need to have valid values as related to any dynamic parameter updates
          IF ( Dprst_frac_flag==1 ) THEN
            dprst_areamax = Dprst_area_max(i)
            IF ( Check_dprst_frac==1 ) dprst_areamax = Temp3(i)*harea ! updated value
            IF ( dprst_areamax>0.0 ) THEN
              IF ( Hru_percent_imperv(i)+dprst_areamax>0.999 ) THEN
                tmp = 0.999 - Hru_percent_imperv(i)
                IF ( tmp<0.001) THEN
                  PRINT *, 'ERROR, fraction impervious + fraction dprst > 0.999 for HRU:', i
                  PRINT *, '   hru_area:', harea, ' hru_imperv:', Hru_imperv(i), ' dprst_area:', dprst_areamax
                  PRINT *, '   (hru_imperv+dprst_area)/hru_area =', (Hru_imperv(i)+dprst_areamax)/harea
                  istop = 1
                ENDIF
              ENDIF
            ENDIF
            tmp = SNGL( Dprst_vol_open(i) + Dprst_vol_clos(i) )
            IF ( dprst_areamax==0.0 .AND. tmp>0.0 ) THEN
              PRINT *, 'Warning, dprst_area reduced to 0 with storage > 0'
              PRINT *, '         storage added to soil_moist and soil_rechr'
              tmp = tmp/dprst_areamax/Hru_frac_perv(i) ! not sure this is correct???
              soilmoist = soilmoist + tmp
              soilrechr = soilrechr + tmp
              Dprst_vol_open(i) = 0.0D0
              Dprst_vol_clos(i) = 0.0D0
            ENDIF
            ! adjust dprst_area, volume stays the same
            Dprst_area_open_max(i) = dprst_areamax*Dprst_frac_open(i)
            Dprst_area_clos_max(i) = dprst_areamax - Dprst_area_open_max(i)
            Dprst_area_max(i) = Dprst_area_open_max(i) + Dprst_area_clos_max(i)
            IF ( Dprst_area_clos_max(i)>0.0 ) Dprst_clos_flag = 0
            IF ( Dprst_area_open_max(i)>0.0 ) Dprst_open_flag = 0
            Dprst_frac(i) = Dprst_area_max(i)/harea
            Dprst_vol_clos_max(i) = DBLE( Dprst_area_clos_max(i)*Dprst_depth_avg(i) )
            Dprst_vol_open_max(i) = DBLE( Dprst_area_open_max(i)*Dprst_depth_avg(i) )
            Dprst_vol_thres_open(i) = Dprst_vol_open_max(i)*DBLE(Op_flow_thres(i))
          ENDIF

          IF ( Check_imperv==1 .OR. Dprst_frac_flag==1 ) THEN
            hruperv = harea - Hru_imperv(i)
            IF ( Dprst_flag==1 ) THEN
              hruperv = hruperv - Dprst_area_max(i)
              IF ( Dprst_area_max(i)+Hru_imperv(i)>0.999*harea ) THEN
                PRINT *, 'ERROR, Impervious + depression area > 0.999*hru_area, HRU:', i
                PRINT *, '       Pervious area:', hruperv, ' Impervious area:', Hru_imperv(i), &
     &                   '       Depression area:', Dprst_area_max(i), &
     &                   '       Impervious + Depression area:', Dprst_area_max(i)+Hru_imperv(i)
                istop = 1
                CYCLE
              ENDIF
            ENDIF
            IF ( Hru_perv(i)/=hruperv ) THEN
                !print *, hru_perv(i), hruperv, hru_perv(i)-hruperv, Dprst_area_max(i), hru_imperv(i),i
              IF ( hruperv<NEARZERO ) &
     &             PRINT *, 'pervious area error for dynamic parameter, HRU:', i, hruperv, harea
              tmp = Hru_perv(i)/hruperv
              Soil_moist(i) = soilmoist*tmp
              Soil_rechr(i) = soilrechr*tmp
              Hru_perv(i) = hruperv
              Hru_frac_perv(i) = Hru_perv(i)/harea
            ENDIF
          ENDIF
          Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(i)*Hru_perv(i) )
          Basin_soil_rechr = Basin_soil_rechr + DBLE( Soil_rechr(i)*Hru_perv(i) )
        ENDDO
        Basin_soil_moist = Basin_soil_moist*Basin_area_inv
        Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      ENDIF

      ! need to update maximum volumes after DPRST area is updated
      IF ( Dprst_depth_flag==1 ) THEN
        IF ( Dprst_depth_next_mo/=0 ) THEN
          IF ( Dprst_depth_next_yr==Nowyear .AND. Dprst_depth_next_mo==Nowmonth .AND. Dprst_depth_next_day==Nowday ) THEN
            READ ( Dprst_depth_unit, * ) Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Dprst_depth_avg, 'dprst_depth_avg')
            DO jj = 1, Active_hrus
              i = Hru_route_order(jj)
              IF ( Hru_type(i)==2 ) CYCLE ! skip lake HRUs
              IF ( Dprst_area_max(i)>NEARZERO ) THEN
                Dprst_vol_clos_max(i) = DBLE( Dprst_area_clos_max(i)*Dprst_depth_avg(i) )
                Dprst_vol_open_max(i) = DBLE( Dprst_area_open_max(i)*Dprst_depth_avg(i) )
                IF ( Dprst_vol_open_max(i)>0.0 ) Dprst_vol_open_frac(i) = SNGL( Dprst_vol_open(i)/Dprst_vol_open_max(i) )
                IF ( Dprst_vol_clos_max(i)>0.0 ) Dprst_vol_clos_frac(i) = SNGL( Dprst_vol_clos(i)/Dprst_vol_clos_max(i) )
                Dprst_vol_frac(i) = SNGL( (Dprst_vol_open(i)+Dprst_vol_clos(i))/(Dprst_vol_open_max(i)+Dprst_vol_clos_max(i)) )
              ENDIF
            ENDDO
            CALL is_eof(Dprst_depth_unit, Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day)
          ENDIF
        ENDIF
      ENDIF

      ! leave current impervious storage amount alone as it will be taking care of later in current timestep
      IF ( Dyn_imperv_flag>1 ) THEN
        IF ( Imperv_stor_next_mo/=0 ) THEN
          IF ( Imperv_stor_next_yr==Nowyear .AND. Imperv_stor_next_mo==Nowmonth .AND. Imperv_stor_next_day==Nowday ) THEN
            READ ( Imperv_stor_unit, * ) Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Imperv_stor_max, 'imperv_stor_max')
            CALL is_eof(Imperv_stor_unit, Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day)
          ENDIF
        ENDIF
      ENDIF

      ! leave any interception storage unchanged, it will be evaporated based on new values in intcp module
      IF ( Wrainintcp_flag==1 ) THEN
        IF ( Wrain_intcp_next_mo/=0 ) THEN
          IF ( Wrain_intcp_next_yr==Nowyear .AND. Wrain_intcp_next_mo==Nowmonth .AND. Wrain_intcp_next_day==Nowday ) THEN
            READ ( Wrain_intcp_unit, * ) Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Wrain_intcp, 'wrain_intcp')
            CALL is_eof(Wrain_intcp_unit, Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day)
          ENDIF
        ENDIF
      ENDIF
      IF ( Srainintcp_flag==1 ) THEN
        IF ( Srain_intcp_next_mo/=0 ) THEN
          IF ( Srain_intcp_next_yr==Nowyear .AND. Srain_intcp_next_mo==Nowmonth .AND. Srain_intcp_next_day==Nowday ) THEN
            READ ( Srain_intcp_unit, * ) Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Srain_intcp, 'srain_intcp')
            CALL is_eof(Srain_intcp_unit, Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day)
          ENDIF
        ENDIF
      ENDIF
      IF ( Snowintcp_flag==1 ) THEN
        IF ( Snow_intcp_next_mo/=0 ) THEN
          IF ( Snow_intcp_next_yr==Nowyear .AND. Snow_intcp_next_mo==Nowmonth .AND. Snow_intcp_next_day==Nowday ) THEN
            READ ( Snow_intcp_unit, * ) Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Snow_intcp, 'snow_intcp')
            CALL is_eof(Snow_intcp_unit, Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day)
          ENDIF
        ENDIF
      ENDIF        

      IF ( Covden_sum_flag==1 ) THEN
        IF ( Covden_sum_next_mo/=0 ) THEN
          IF ( Covden_sum_next_yr==Nowyear .AND. Covden_sum_next_mo==Nowmonth .AND. Covden_sum_next_day==Nowday ) THEN
            READ ( Covden_sum_unit, * ) Covden_sum_next_yr, Covden_sum_next_mo, Covden_sum_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Covden_sum, 'covden_sum')
            CALL is_eof(Covden_sum_unit, Covden_sum_next_yr, Covden_sum_next_mo, Covden_sum_next_day)
          ENDIF
        ENDIF
      ENDIF
      IF ( Covden_win_flag==1 ) THEN
        IF ( Covden_win_next_mo/=0 ) THEN
          IF ( Covden_win_next_yr==Nowyear .AND. Covden_win_next_mo==Nowmonth .AND. Covden_win_next_day==Nowday ) THEN
            READ ( Covden_win_unit, * ) Covden_win_next_yr, Covden_win_next_mo, Covden_win_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Covden_win, 'covden_win')
            CALL is_eof(Covden_win_unit, Covden_win_next_yr, Covden_win_next_mo, Covden_win_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_covtype_flag==1 ) THEN
        IF ( Covtype_next_mo/=0 ) THEN
          IF ( Covtype_next_yr==Nowyear .AND. Covtype_next_mo==Nowmonth .AND. Covtype_next_day==Nowday ) THEN
            READ ( Covtype_unit, * ) Covtype_next_yr, Covtype_next_mo, Covtype_next_day, Itemp
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Cov_type, 'cov_type')
            Cov_type = Itemp
            CALL is_eof(Covtype_unit, Covtype_next_yr, Covtype_next_mo, Covtype_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_potet_flag>0 ) THEN  ! fix so only current month is updated
        IF ( Potetcoef_next_mo/=0 ) THEN
          IF ( Potetcoef_next_yr==Nowyear .AND. Potetcoef_next_mo==Nowmonth .AND. Potetcoef_next_day==Nowday ) THEN
            READ ( Potetcoef_unit, * ) Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day, Temp
            IF ( Et_flag==1 .AND. Dyn_potet_flag/=1 ) THEN ! allow values to be < 0.0 for potet_jh_hru parameter
              CALL write_dynparam_potet(Output_unit, Nhru, Updated_hrus, Temp, Potet_coef(1,Nowmonth), 'potet_coef')
            ELSE
              CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Potet_coef(1,Nowmonth), 'potet_coef')
            ENDIF
            CALL is_eof(Potetcoef_unit, Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day)
          ENDIF
          IF ( Et_flag==1 ) THEN ! potet_jh
            IF ( Dyn_potet_flag==1 ) THEN
              Jh_coef = Potet_coef
            ELSE
              DO i = 1, Nhru
                Jh_coef_hru(i) = Potet_coef(i,Nowmonth)
              ENDDO
            ENDIF
          ELSEIF ( Et_flag==7 ) THEN ! climate_hru
            Potet_cbh_adj = Potet_coef
          ELSEIF ( Et_flag==11 ) THEN ! potet_pm
            IF ( Dyn_potet_flag==1 ) THEN
              Pm_n_coef = Potet_coef
            ELSE
              Pm_d_coef = Potet_coef
            ENDIF
          ELSEIF ( Et_flag==5 ) THEN ! potet_pt
            Pt_alpha = Potet_coef
          ELSEIF ( Et_flag==10 ) THEN ! potet_hs
            Hs_krs = Potet_coef
          ELSEIF ( Et_flag==2 ) THEN ! potet_hamon
            Hamon_coef = Potet_coef
          !ELSEIF ( Et_flag==6 ) THEN ! potet_jh_hru
            !Jh_coef_hru2 = Potet_coef
          ELSEIF ( Et_flag==4 ) THEN ! potet_pan
            Epan_coef = Potet_coef
          ENDIF
        ENDIF
      ENDIF

      IF ( Transpbeg_flag==1 ) THEN
        IF ( Transpbeg_next_mo/=0 ) THEN
          IF ( Transpbeg_next_yr==Nowyear .AND. Transpbeg_next_mo==Nowmonth .AND. Transpbeg_next_day==Nowday ) THEN
            READ ( Transpbeg_unit, * ) Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day, Itemp
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Transp_beg, 'transp_beg')
            CALL is_eof(Transpbeg_unit, Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Transpend_flag==1 ) THEN
        IF ( Transpend_next_mo/=0 ) THEN
          IF ( Transpend_next_yr==Nowyear .AND. Transpend_next_mo==Nowmonth .AND. Transpend_next_day==Nowday ) THEN
            READ ( Transpend_unit, * ) Transpend_next_yr, Transpend_next_mo, Transpend_next_day, Itemp
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Transp_end, 'transp_end')
            CALL is_eof(Transpend_unit, Transpend_next_yr, Transpend_next_mo, Transpend_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Fallfrost_flag==1 ) THEN
        IF ( Fallfrost_next_mo/=0 ) THEN
          IF ( Fallfrost_next_yr==Nowyear .AND. Fallfrost_next_mo==Nowmonth .AND. Fallfrost_next_day==Nowday ) THEN
            READ ( Fallfrost_unit, * ) Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day, Itemp
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Fall_frost, 'fall_frost')
            CALL is_eof(Fallfrost_unit, Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Springfrost_flag==1 ) THEN
        IF ( Springfrost_next_mo/=0 ) THEN
          IF ( Springfrost_next_yr==Nowyear .AND. Springfrost_next_mo==Nowmonth .AND. Springfrost_next_day==Nowday ) THEN
            READ ( Springfrost_unit, * ) Springfrost_next_yr, Springfrost_next_mo, Springfrost_next_day, Itemp
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Spring_frost, 'spring_frost')
            CALL is_eof(Springfrost_unit, Springfrost_next_yr, Springfrost_next_mo, Springfrost_next_day)
          ENDIF
        ENDIF
      ENDIF

! leave current soil_rechr storage amount alone as it will be taking care of later in current timestep
      IF ( Soilrechr_flag==1 ) THEN
        IF ( Soil_rechr_next_mo/=0 ) THEN
          IF ( Soil_rechr_next_yr==Nowyear .AND. Soil_rechr_next_mo==Nowmonth .AND. Soil_rechr_next_day==Nowday ) THEN
            READ ( Soil_rechr_unit, * ) Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day, Temp
            IF ( PRMS4_flag==1 ) THEN
              CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Soil_rechr_max, 'soil_rechr_max')
            ELSE
              CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Soil_rechr_max_frac, 'soil_rechr_max_frac')
            ENDIF
            CALL is_eof(Soil_rechr_unit, Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day)
          ENDIF
        ENDIF
      ENDIF

! leave current soil_moist storage amount alone as it will be taking care of later in current timestep
      IF ( Soilmoist_flag==1 ) THEN
        IF ( Soil_moist_next_mo/=0 ) THEN
          IF ( Soil_moist_next_yr==Nowyear .AND. Soil_moist_next_mo==Nowmonth .AND. Soil_moist_next_day==Nowday ) THEN
            READ ( Soil_moist_unit, * ) Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Soil_moist_max, 'soil_moist_max')
            CALL is_eof(Soil_moist_unit, Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Soilmoist_flag==1 .OR. Soilrechr_flag==1 ) THEN
        DO jj = 1, Active_hrus
          i = Hru_route_order(jj)
          IF ( Hru_type(i)==2 ) CYCLE ! skip lake HRUs

          IF ( Soil_moist_max(i)<NEARZERO ) THEN
            istop = 1
            PRINT 9001, 'soil_moist_max', NEARZERO, i, Soil_moist_max(i), NEARZERO
          ENDIF
          IF ( PRMS4_flag==0 ) Soil_rechr_max(i) = Soil_moist_max(i)*Soil_rechr_max_frac(i)
          IF ( Soil_rechr_max(i)<NEARZERO ) THEN
            istop = 1
            PRINT 9001, 'soil_rechr_max', NEARZERO, i, Soil_rechr_max(i), NEARZERO
          ENDIF
          IF ( istop==1 ) CYCLE

          Soil_zone_max(i) = Sat_threshold(i) + Soil_moist_max(i)*Hru_frac_perv(i)
          Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*Hru_frac_perv(i)
!          Soil_moist_frac(i) = Soil_moist_tot(i)/Soil_zone_max(i)
!          Cpr_stor_frac(i) = Soil_moist(i)/Soil_moist_max(i)
          Soil_lower_stor_max(i) = Soil_moist_max(i) - Soil_rechr_max(i)
          Replenish_frac(i) = Soil_rechr_max(i)/Soil_moist_max(i)
          IF ( Soil_lower_stor_max(i)>0.0 ) Soil_lower_ratio(i) = Soil_lower(i)/Soil_lower_stor_max(i)
!          Soil_rechr_ratio(i) = Soil_rechr(i)/Soil_rechr_max(i)
        ENDDO
      ENDIF
      IF ( istop==1 ) STOP

      IF ( Dyn_radtrncf_flag==1 ) THEN
        IF ( Rad_trncf_next_mo/=0 ) THEN
          IF ( Rad_trncf_next_yr==Nowyear .AND. Rad_trncf_next_mo==Nowmonth .AND. Rad_trncf_next_day==Nowday ) THEN
            READ ( Rad_trncf_unit, * ) Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Rad_trncf, 'rad_trncf')
            CALL is_eof(Rad_trncf_unit, Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_snareathresh_flag==1 ) THEN
        IF ( Snarea_thresh_next_mo/=0 ) THEN
          IF ( Snarea_thresh_next_yr==Nowyear .AND. Snarea_thresh_next_mo==Nowmonth .AND. Snarea_thresh_next_day==Nowday ) THEN
            READ ( Snarea_thresh_unit, * ) Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Snarea_thresh, 'snarea_thresh')
            CALL is_eof(Snarea_thresh_unit, Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_perv_flag==1 ) THEN
        IF ( Sro_to_dprst_next_mo/=0 ) THEN
          IF ( Sro_to_dprst_next_yr==Nowyear .AND. Sro_to_dprst_next_mo==Nowmonth .AND. Sro_to_dprst_next_day==Nowday ) THEN
            READ ( Sro_to_dprst_unit, * ) Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Sro_to_dprst_perv, 'sro_to_dprst_perv')
            CALL is_eof(Sro_to_dprst_unit, Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_imperv_flag==1 ) THEN
        IF ( Sro_to_imperv_next_mo/=0 ) THEN
          IF ( Sro_to_imperv_next_yr==Nowyear .AND. Sro_to_imperv_next_mo==Nowmonth .AND. Sro_to_imperv_next_day==Nowday ) THEN
            READ ( Sro_to_imperv_unit, * ) Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day, Temp
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Sro_to_dprst_imperv, 'sro_to_dprst_imperv')
            CALL is_eof(Sro_to_imperv_unit, Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_transp_on_flag==1 ) THEN
        IF ( Transp_event_next_mo/=0 ) THEN
          IF ( Transp_event_next_yr==Nowyear .AND. Transp_event_next_mo==Nowmonth .AND. Transp_event_next_day==Nowday ) THEN
            READ ( Transp_event_unit, * ) Transp_event_next_yr, Transp_event_next_mo, Transp_event_next_day, Itemp
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Transp_on, 'transp_on_event')
            CALL is_eof(Transp_event_unit, Transp_event_next_yr, Transp_event_next_mo, Transp_event_next_day)
          ENDIF
        ENDIF
      ENDIF

9001  FORMAT (/, 'ERROR, dynamic parameter', A, ' <', F10.7, ' for HRU:', I7, /, 9X, 'value:', F10.7) !, ' set to', F10.7)

      END FUNCTION dynparamrun

!***********************************************************************
!     Values are read in, Parm are last, Values are updated or old
!***********************************************************************
      SUBROUTINE write_dynoutput(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: NEARZERO, Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      REAL, INTENT(IN) :: Param(Dim)
      REAL, INTENT(INOUT) :: Values(Dim) ! dynamic values with old non-updated
      INTEGER, INTENT(OUT) :: Updated_hrus(Nhru)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, j, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Values(i)<0.0 ) THEN
          Values(i) = Param(i)
        ELSEIF ( ABS(Values(i)-Param(i))>NEARZERO ) THEN
          num = num + 1
          Updated_hrus(num) = i
        ENDIF
      ENDDO
      IF ( Print_debug>-2 ) &
    &      WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
      IF ( Print_debug>-1 ) THEN
        WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
        WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
      ENDIF
      END SUBROUTINE write_dynoutput

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
      SUBROUTINE write_dynparam_int(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: NEARZERO, Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      INTEGER, INTENT(IN) :: Values(Dim)
      INTEGER, INTENT(INOUT) :: Param(Dim)
      INTEGER, INTENT(OUT) :: Updated_hrus(Nhru)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, j, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Values(i)<0.0 ) CYCLE
        IF ( ABS(Values(i)-Param(i))>NEARZERO ) THEN
          num = num + 1
          Updated_hrus(num) = i
          Param(i) = Values(i)
        ENDIF
      ENDDO
      IF ( Print_debug>-2 ) &
     &     WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
      IF ( Print_debug>-1 ) THEN
        WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
        WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
      ENDIF
      END SUBROUTINE write_dynparam_int

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
      SUBROUTINE write_dynparam(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_MODULE, ONLY: Nhru, Print_debug
      USE PRMS_BASIN, ONLY: NEARZERO, Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      REAL, INTENT(IN) :: Values(Dim)
      REAL, INTENT(INOUT) :: Param(Dim)
      INTEGER, INTENT(OUT) :: Updated_hrus(Nhru)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, j, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( Values(i)<0.0 ) CYCLE
        IF ( ABS(Values(i)-Param(i))>NEARZERO ) THEN
          Param(i) = Values(i)
          num = num + 1
          Updated_hrus(num) = i
        ENDIF
      ENDDO
      IF ( Print_debug>-2 ) &
     &     WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
      IF ( Print_debug>-1 ) THEN
        WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
        WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
      ENDIF
      END SUBROUTINE write_dynparam

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
!      SUBROUTINE write_dynparam_dble(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
!      USE PRMS_MODULE, ONLY: Print_debug, Nhru
!      USE PRMS_BASIN, ONLY: NEARZERO, Active_hrus, Hru_route_order
!      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
!      IMPLICIT NONE
! Arguments
!      INTEGER, INTENT(IN) :: Output_unit, Dim
!      REAL, INTENT(IN) :: Values(Dim)
!      DOUBLE PRECISION, INTENT(INOUT) :: Param(Dim)
!      INTEGER, INTENT(OUT) :: Updated_hrus(Nhru)
!      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Functions
!      INTRINSIC DBLE, SNGL
! Local Variables
!      INTEGER i, j, num
!***********************************************************************
!      Updated_hrus = 0
!      num = 0
!      DO j = 1, Active_hrus
!        i = Hru_route_order(j)
!        IF ( Values(i)<0.0 ) CYCLE
!        IF ( ABS(Values(i)-SNGL(Param(i)))>NEARZERO ) THEN
!          Param(i) = DBLE( Values(i) )
!          num = num + 1
!          Updated_hrus(num) = i
!        ENDIF
!      ENDDO
!      IF ( Print_debug>-2 ) &
!     &     WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
!      IF ( Print_debug>-1 ) THEN
!        WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
!        WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
!      ENDIF
!      END SUBROUTINE write_dynparam_dble

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
      SUBROUTINE write_dynparam_potet(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_MODULE, ONLY: Print_debug, Nhru
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      REAL, INTENT(IN) :: Values(Dim)
      REAL, INTENT(INOUT) :: Param(Dim)
      INTEGER, INTENT(OUT) :: Updated_hrus(Nhru)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, j, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        IF ( ABS(Values(i)-Param(i))>0.0 ) THEN
          Param(i) = Values(i)
          num = num + 1
          Updated_hrus(num) = i
        ENDIF
      ENDDO
      IF ( Print_debug>-2 ) &
     &     WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
      IF ( Print_debug>-1 ) THEN
        WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
        WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
      ENDIF
      END SUBROUTINE write_dynparam_potet
