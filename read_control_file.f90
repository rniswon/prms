!***********************************************************************
! Read Control File
!***********************************************************************
      MODULE PRMS_CONTROL_FILE
        USE PRMS_MODULE, ONLY: MAXFILE_LENGTH, Print_debug, EQULS, Prms_output_unit, statsON_OFF, MAXCONTROL_LENGTH, &
     &      Init_vars_from_file, Save_vars_to_file, Parameter_check_flag
        USE PRMS_CLIMATE_HRU, ONLY: Cbh_check_flag
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Model_output_file, Data_file, Var_init_file, Stat_var_file, Ani_out_file
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Executable_desc, Executable_model, Param_file, Var_save_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: Control_file, Modflow_name, Ani_output_file, Gsflow_output_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: Precip_day, Tmax_day, Tmin_day, Potet_day, Transp_day, Swrad_day
        INTEGER, SAVE :: Julian_day_absolute, PlotsON_OFF, Gsf_rpt, Rpt_days
        INTEGER, SAVE :: NstatVars, AniOutON_OFF, NaniOutVars, NdispGraphs, DispGraphsBuffSize
        INTEGER, SAVE :: Num_statvar_elements, Num_statvar_names, NumdispVar_names, NumdispVar_elements
        REAL, SAVE :: Initial_deltat
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: statVar_element(:), statVar_names(:)
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: dispVar_element(:), dispVar_names(:)
        CHARACTER(LEN=80), SAVE :: Version_read_control_file
      END MODULE PRMS_CONTROL_FILE

      SUBROUTINE read_control_file
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      INTEGER, EXTERNAL :: numchars, control_string
      EXTERNAL read_error, set_control_parameter, PRMS_open_input_file, write_outfile, print_module
      ! Local Variables
      CHARACTER(LEN=32) :: paramname
      CHARACTER(LEN=4) :: string
      CHARACTER(LEN=MAXFILE_LENGTH) line
      INTEGER control_unit, nchars, ios, numvalues, param_type, iflag, i, num_control_parameters, nchars_control_file, iret
      INTEGER, ALLOCATABLE :: int_parameter_values(:)
      CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: parameter_values(:)
      REAL, ALLOCATABLE :: real_parameter_values(:)
!***********************************************************************
      Version_read_control_file = 'read_control_file.f90 2012-12-18 19:47:26Z'
      CALL print_module(Version_read_control_file, 'Read Control File       ', 90)

      ! control filename cannot include blanks
      CALL get_control_filename(Control_file, nchars)
      CALL PRMS_open_input_file(control_unit, Control_file, 'control_file', 0, ios)
      nchars_control_file = numchars( Control_file )
      IF ( ios/=0 ) CALL read_error(10, Control_file(:nchars_control_file))
      ! read header
      READ (control_unit, '(A)', IOSTAT=ios ) line
      IF ( ios/=0 ) CALL read_error(12, Control_file(:nchars_control_file))
      IF ( Print_debug>-1 ) THEN
        PRINT *, EQULS
        PRINT *, 'Using Control File: ', Control_file(:nchars_control_file)
        PRINT *, 'Control File Description:'
        PRINT *, TRIM(line)
        PRINT *, EQULS
        PRINT *, 'Messages from your model concerning the Control File'
        PRINT *, 'Please give careful consideration to fixing all ERRORS and WARNINGS'
        PRINT *, EQULS
      ENDIF
      ! Read all Control Parameters
      Num_control_parameters = 0
      DO
        READ (control_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of file
        IF ( ios/=0 ) CALL read_error(12, Control_file(:nchars_control_file))
        ! skip to first parameter delimiter
        IF ( string/='####' ) CYCLE
        READ ( control_unit, '(A)', IOSTAT=ios ) paramname
        IF ( ios/=0 ) CALL read_error(12, 'missing control parameter name')
        nchars = INDEX( paramname, ' ' ) - 1
        READ ( control_unit, *, IOSTAT=ios ) numvalues
        IF ( ios/=0 ) CALL read_error(12, 'invalid number of values for: '//paramname(:nchars))
        READ ( control_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(12, 'invalid parameter type for: '//paramname(:nchars))
        ALLOCATE ( int_parameter_values(Numvalues), parameter_values(Numvalues), real_parameter_values(Numvalues) )
        DO i = 1, Numvalues
          IF ( param_type==1 ) THEN
            READ ( Control_unit, *, IOSTAT=ios ) int_parameter_values(i)
          ELSEIF ( Param_type==4 ) THEN
            READ ( Control_unit, *, IOSTAT=ios ) parameter_values(i)
          ELSEIF ( Param_type==2 ) THEN
            READ ( Control_unit, *, IOSTAT=ios ) real_parameter_values(i)
          ELSE
            CALL read_error(12, 'invalid parameter type for: '//paramname(:nchars)//', must be 1, 2 or 4')
          ENDIF
        ENDDO
        iflag = 0
        CALL set_control_parameter(paramname, numvalues, param_type, int_parameter_values, parameter_values, real_parameter_values, iflag)
        IF ( iflag==-2 ) THEN
          CALL read_error(12, 'invalid number of values for: '//paramname(:nchars))
        ELSEIF ( iflag==-1 ) THEN
          PRINT *, 'Warning, control parameter: ', paramname(:nchars), ' is unused, thus ignored'
        ENDIF
        DEALLOCATE ( int_parameter_values, parameter_values, real_parameter_values )
        Num_control_parameters = Num_control_parameters + 1
      ENDDO

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'prms.out')
      CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
      IF ( iret/=0 ) STOP
      IF ( Print_debug>-1 ) THEN
        PRINT *, 'Model output written to file: ', Model_output_file(:numchars(Model_output_file))
        PRINT *, EQULS
      ENDIF

      CALL write_outfile(EQULS)
      CALL write_outfile('Using PRMS executable: '//Executable_model(:numchars(Executable_model)))
      CALL write_outfile('Description: '//Executable_desc(:numchars(Executable_desc)))
      CALL write_outfile(EQULS)
      CALL write_outfile('Using Control File: '//Control_file(:nchars_control_file))
      CALL write_outfile('Control File Description: '//TRIM(line))
      CALL write_outfile(EQULS)
      CALL write_outfile('Model output written to file: '//Model_output_file(:numchars(Model_output_file)))
      CALL write_outfile(EQULS)
      CALL write_outfile(' ')

      CLOSE ( control_unit )
      END SUBROUTINE read_control_file

!***********************************************************************
! set_control_parameter - Set control parameter values
!***********************************************************************
      SUBROUTINE set_control_parameter(Parmname, Numvalues, Param_type, Int_parameter_values, &
     &           Parameter_values, Real_parameter_values, Iflag)
      USE PRMS_CONTROL_FILE
      USE PRMS_MODULE, ONLY: Precip_module, Temp_module, Et_module, Srunoff_module, Solrad_module, Gwr_swale_flag, &
     &    Strmflow_module, Transp_module, Soilzone_module, Print_debug, Dprst_flag, Subbasin_flag, Frozen_flag, &
     &    CsvON_OFF, MapOutON_OFF, Model_mode, Orad_flag, Endtime, Starttime, Snow_cbh_flag, Stream_temp_flag, &
     &    Cascade_flag, Cascadegw_flag, Csv_output_file, StatsON_OFF, &
     &    Modflow_time_zero, Gwflow_cbh_flag, NhruOutON_OFF, NsubOutON_OFF
      USE PRMS_MAP_RESULTS, ONLY: NmapOutVars, MapOutVar_names
      IMPLICIT NONE
      ! Functions
      INTRINSIC CHAR
      EXTERNAL read_error
      INTEGER, EXTERNAL :: numchars
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Parmname
      INTEGER, INTENT(IN) :: Numvalues, Param_type
      INTEGER, INTENT(INOUT) :: Int_parameter_values(Numvalues)
      INTEGER, INTENT(INOUT) :: Iflag
      CHARACTER(LEN=*), INTENT(INOUT) :: Parameter_values(Numvalues)
      REAL, INTENT(INOUT) :: Real_parameter_values(Numvalues)
      ! Local Variables
      INTEGER nc
      INTEGER, SAVE :: init
      DATA init/0/
!***********************************************************************
      IF ( init==0 ) THEN ! First time set default values for control parameters
        init = 1
        Model_mode = 'PRMS'
        Precip_module = 'precip_1sta'
        Temp_module = 'temp_1sta'
        Et_module = 'potet_jh'
        Srunoff_module = 'srunoff_smidx'
        Solrad_module = 'ddsolrad'
        Soilzone_module = 'soilzone'
        Strmflow_module = 'strmflow'
        Transp_module = 'transp_tindex'
        Print_debug = 0
        Cascade_flag = 1
        Cascadegw_flag = 1
        Subbasin_flag = 1
        Dprst_flag = 0
        Parameter_check_flag = 1
        Cbh_check_flag = 1
        Snow_cbh_flag = 0
        Gwflow_cbh_flag = 0
        Stream_temp_flag = 0
        Frozen_flag = 0
        Gwr_swale_flag = 0
        NhruOutON_OFF = 0
        NsubOutON_OFF = 0
        Rpt_days = 7
        Gsf_rpt = 1
        CsvON_OFF = 0
        Orad_flag = 0
        MapOutON_OFF = 0
        Starttime = 0
        Endtime = 0
        Modflow_time_zero = 0
        Init_vars_from_file = 0
        Save_vars_to_file = 0
        Initial_deltat = 24.0
        StatsON_OFF = 0
        NstatVars = 0
        AniOutON_OFF = 0
        NaniOutVars = 0
        NdispGraphs = 0
        DispGraphsBuffSize = 50
        Executable_desc = 'MOWS executable'
        Executable_model = 'PRMS_IV'
        Data_file = 'prms.data'
        Param_file = 'prms.params'
        Var_init_file = 'prms_ic.in'
        Var_save_file = 'prms_ic.out'
        Stat_var_file = 'statvar.out'
        Ani_out_file = 'animation.out'
        Model_output_file = 'prms.out'
        Tmax_day = 'tmax.day'
        Tmin_day = 'tmin.day'
        Precip_day = 'precip.day'
        Swrad_day = 'swrad.day'
        Potet_day = 'potet.day'
        Transp_day = 'transp.day'
        Csv_output_file = 'prms_summary.csv'
        ! mapOutVar_names = CHAR(0) - need to allocate, so don't initialize
      ENDIF
      nc = numchars(Parmname)
      IF ( Param_type==1 ) THEN
        IF ( Parmname(:7)=='gsf_rpt' ) THEN
          IF ( Iflag==0 ) THEN
            Gsf_rpt = Int_parameter_values(1)
          ELSE
            Int_parameter_values(1) = Gsf_rpt
          ENDIF
        ELSEIF ( nc==8 ) THEN
          IF ( Parmname(:8)=='end_time' ) THEN
            IF ( Iflag==0 ) THEN
              Endtime = Int_parameter_values
            ELSE
              Int_parameter_values = Endtime
            ENDIF
          ELSEIF ( Parmname(:8)=='rpt_days' ) THEN
            IF ( Iflag==0 ) THEN
              Rpt_days = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Rpt_days
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==9 ) THEN
          IF ( Parmname(:9)=='csvON_OFF' ) THEN
            IF ( Iflag==0 ) THEN
              CsvON_OFF = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = CsvON_OFF
            ENDIF
          ELSEIF ( Parmname(:9)=='nstatVars' ) THEN
            IF ( Iflag==0 ) THEN
              NstatVars = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = NstatVars
            ENDIF
          ELSEIF ( Parmname(:9)=='orad_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Orad_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Orad_flag
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==10 ) THEN
          IF ( Parmname(:10)=='dprst_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Dprst_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Dprst_flag
            ENDIF
          ELSEIF ( Parmname(:10)=='start_time' ) THEN
            IF ( Iflag==0 ) THEN
              Starttime = Int_parameter_values
            ELSE
              Int_parameter_values = Starttime
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==11 ) THEN
          IF ( Parmname(:11)=='nmapOutVars' ) THEN
            IF ( Iflag==0 ) THEN
              NmapOutVars = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = NmapOutVars
            ENDIF
          ELSEIF ( Parmname(:11)=='print_debug' ) THEN
            IF ( Iflag==0 ) THEN
              Print_debug = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Print_debug
            ENDIF
          ELSEIF ( Parmname(:11)=='statsON_OFF' ) THEN
            IF ( Iflag==0 ) THEN
              StatsON_OFF = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = StatsON_OFF
            ENDIF
          ELSEIF ( Parmname(:11)=='naniOutVars' ) THEN
            IF ( Iflag==0 ) THEN
              NaniOutVars = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = NaniOutVars
            ENDIF
          ELSEIF ( Parmname(:11)=='ndispGraphs' ) THEN
            IF ( Iflag==0 ) THEN
              NdispGraphs = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = NdispGraphs
            ENDIF
          ELSEIF ( Parmname(:11)=='frozen_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Frozen_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Frozen_flag
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==12 ) THEN
          IF ( Parmname(:12)=='aniOutON_OFF' ) THEN
            IF ( Iflag==0 ) THEN
              AniOutON_OFF = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = AniOutON_OFF
            ENDIF
          ELSEIF ( Parmname(:12)=='mapOutON_OFF' ) THEN
            IF ( Iflag==0 ) THEN
              MapOutON_OFF = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = MapOutON_OFF
            ENDIF
          ELSEIF ( Parmname(:12)=='cascade_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Cascade_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Cascade_flag
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==13 ) THEN
          IF ( Parmname(:13)=='subbasin_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Subbasin_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Subbasin_flag
            ENDIF
          ELSEIF ( Parmname(:13)=='snow_cbh_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Snow_cbh_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Snow_cbh_flag
            ENDIF
          ELSEIF ( Parmname(:13)=='nhruOutON_OFF' ) THEN
            IF ( Iflag==0 ) THEN
              NhruOutON_OFF = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = NhruOutON_OFF
            ENDIF
          ELSEIF ( Parmname(:13)=='nsubOutON_OFF' ) THEN
            IF ( Iflag==0 ) THEN
              NsubOutON_OFF = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = NsubOutON_OFF
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==14 ) THEN
          IF ( Parmname(:14)=='cascadegw_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Cascadegw_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Cascadegw_flag
            ENDIF
          ELSEIF ( Parmname(:14)=='cbh_check_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Cbh_check_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Cbh_check_flag
            ENDIF
          ELSEIF ( Parmname(:14)=='gwr_swale_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Gwr_swale_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Gwr_swale_flag
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==15 ) THEN
          IF ( Parmname(:15)=='gwflow_cbh_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Gwflow_cbh_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Gwflow_cbh_flag
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==16 ) THEN
          IF ( Parmname(:16)=='stream_temp_flag' ) THEN
            IF ( Iflag==0 ) THEN
              Stream_temp_flag = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Stream_temp_flag
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==17 ) THEN
          IF ( Parmname(:17)=='save_vars_to_file' ) THEN
            IF ( Iflag==0 ) THEN
              Save_vars_to_file = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Save_vars_to_file
            ENDIF
          ELSEIF ( Parmname(:17)=='modflow_time_zero' ) THEN
            IF ( Iflag==0 ) THEN
              Modflow_time_zero = Int_parameter_values
            ELSE
              Int_parameter_values = Modflow_time_zero
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==18 ) THEN
          IF ( Parmname(:18)=='DispGraphsBuffSize' ) THEN
            IF ( Iflag==0 ) THEN
              DispGraphsBuffSize = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = DispGraphsBuffSize
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==19 ) THEN
          IF ( Parmname(:19)=='init_vars_from_file' ) THEN
            IF ( Iflag==0 ) THEN
              Init_vars_from_file = Int_parameter_values(1)
            ELSE
              Int_parameter_values(1) = Init_vars_from_file
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( Parmname(:20)=='parameter_check_flag' ) THEN
          IF ( Iflag==0 ) THEN
            Parameter_check_flag = Int_parameter_values(1)
          ELSE
            Int_parameter_values(1) = Parameter_check_flag
          ENDIF
        ELSE
          Iflag = -1
        ENDIF
        IF ( Numvalues/=1 ) THEN
          IF ( Iflag/=-1 .AND. Parmname(:8)/='end_time' .AND. Parmname(:10)/='start_time' &
     &         .AND. Parmname(:17)/='modflow_time_zero' ) Iflag = -2
        ENDIF
      ELSEIF ( Param_type==4 ) THEN ! character control parameters
        IF ( nc==8 ) THEN
          IF ( Parmname(:8)=='tmax_day' ) THEN
            IF ( Iflag==0 ) THEN
              Tmax_day = Parameter_values(1)
            ELSE
              Parameter_values(1) = Tmax_day
            ENDIF
          ELSEIF ( Parmname(:8)=='tmin_day' ) THEN
            IF ( Iflag==0 ) THEN
              Tmin_day = Parameter_values(1)
            ELSE
              Parameter_values(1) = Tmin_day
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==9 ) THEN
          IF ( Parmname(:9)=='potet_day' ) THEN
            IF ( Iflag==0 ) THEN
              Potet_day = Parameter_values(1)
            ELSE
              Parameter_values(1) = Potet_day
            ENDIF
          ELSEIF ( Parmname(:9)=='swrad_day' ) THEN
            IF ( Iflag==0 ) THEN
              Swrad_day = Parameter_values(1)
            ELSE
              Parameter_values(1) = Swrad_day
            ENDIF
          ELSEIF ( Parmname(:9)=='data_file' ) THEN
            IF ( Iflag==0 ) THEN
              Data_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Data_file
            ENDIF
          ELSEIF ( Parmname(:9)=='et_module' ) THEN
            IF ( Iflag==0 ) THEN
              Et_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Et_module
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==10 ) THEN
          IF ( Parmname(:10)=='model_mode' ) THEN
            IF ( Iflag==0 ) THEN
              Model_mode = Parameter_values(1)
            ELSE
              Parameter_values(1) = Model_mode
            ENDIF
          ELSEIF ( Parmname(:10)=='param_file' ) THEN
            IF ( Iflag==0 ) THEN
              Param_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Param_file
            ENDIF
          ELSEIF ( Parmname(:10)=='precip_day' ) THEN
            IF ( Iflag==0 ) THEN
              Precip_day = Parameter_values(1)
            ELSE
              Parameter_values(1) = Precip_day
            ENDIF
          ELSEIF ( Parmname(:10)=='transp_day' ) THEN
            IF ( Iflag==0 ) THEN
              Transp_day = Parameter_values(1)
            ELSE
              Parameter_values(1) = Transp_day
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==11 ) THEN
          IF ( Parmname(:11)=='temp_module' ) THEN
            IF ( Iflag==0 ) THEN
              Temp_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Temp_module
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==13 ) THEN
          IF ( Parmname(:13)=='precip_module' ) THEN
            IF ( Iflag==0 ) THEN
              Precip_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Precip_module
            ENDIF
          ELSEIF ( Parmname(:13)=='transp_module' ) THEN
            IF ( Iflag==0 ) THEN
              Transp_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Transp_module
            ENDIF
          ELSEIF ( Parmname(:13)=='solrad_module' ) THEN
            IF ( Iflag==0 ) THEN
              Solrad_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Solrad_module
            ENDIF
          ELSEIF ( Parmname(:13)=='stat_var_file' ) THEN
            IF ( Iflag==0 ) THEN
              Stat_var_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Stat_var_file
            ENDIF
          ELSEIF ( Parmname(:13)=='var_init_file' ) THEN
            IF ( Iflag==0 ) THEN
              Var_init_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Var_init_file
            ENDIF
          ELSEIF ( Parmname(:13)=='dispVar_names' ) THEN ! might not have nmapOut_vars yet, use Numvalues
            ALLOCATE ( dispVar_names(Numvalues) )
            IF ( Iflag==0 ) THEN
              dispVar_names = Parameter_values
              NumdispVar_names = Numvalues
            ELSE
              Parameter_values = dispVar_names
            ENDIF
          ELSEIF ( Parmname(:13)=='var_save_file' ) THEN
            IF ( Iflag==0 ) THEN
              Var_save_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Var_save_file
            ENDIF
          ELSEIF ( Parmname(:13)=='statVar_names' ) THEN ! might not have nmapOut_vars yet, use Numvalues
            ALLOCATE ( statVar_names(Numvalues) )
            IF ( Iflag==0 ) THEN
              statVar_names = Parameter_values
              Num_statvar_names = Numvalues
            ELSE
              Parameter_values = statVar_names
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==14 ) THEN
          IF ( Parmname(:14)=='srunoff_module' ) THEN
            IF ( Iflag==0 ) THEN
              Srunoff_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Srunoff_module
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==15 ) THEN
          IF ( Parmname(:15)=='strmflow_module' ) THEN
            IF ( Iflag==0 ) THEN
              Strmflow_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Strmflow_module
            ENDIF
          ELSEIF ( Parmname(:15)=='soilzone_module' ) THEN
            IF ( Iflag==0 ) THEN
              Soilzone_module = Parameter_values(1)
            ELSE
              Parameter_values(1) = Soilzone_module
            ENDIF
          ELSEIF ( Parmname(:15)=='csv_output_file' ) THEN
            IF ( Iflag==0 ) THEN
              Csv_output_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Csv_output_file
            ENDIF
          ELSEIF ( Parmname(:15)=='mapOutVar_names' ) THEN ! might not have nmapOut_vars yet, use Numvalues
            ALLOCATE ( MapOutVar_names(Numvalues) )
            IF ( Iflag==0 ) THEN
              MapOutVar_names = Parameter_values
            ELSE
              Parameter_values = MapOutVar_names
            ENDIF
          ELSEIF ( Parmname(:15)=='statVar_element' ) THEN ! might not have nmapOut_vars yet, use Numvalues
            ALLOCATE ( statVar_element(Numvalues) )
            IF ( Iflag==0 ) THEN
              statVar_element = Parameter_values
              Num_statvar_elements = Numvalues
            ELSE
              Parameter_values = statVar_element
            ENDIF
          ELSEIF ( Parmname(:15)=='executable_desc' ) THEN
            IF ( Iflag==0 ) THEN
              Executable_desc = Parameter_values(1)
            ELSE
              Parameter_values(1) = Executable_desc
            ENDIF
          ELSEIF ( Parmname(:15)=='dispVar_element' ) THEN ! might not have nmapOut_vars yet, use Numvalues
            ALLOCATE ( dispVar_element(Numvalues) )
            IF ( Iflag==0 ) THEN
              dispVar_element = Parameter_values
              Numdispvar_elements = Numvalues
            ELSE
              Parameter_values = dispVar_element
            ENDIF
          ELSEIF ( Parmname(:15)=='ani_output_file' ) THEN
            IF ( Iflag==0 ) THEN
              Ani_output_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Ani_output_file
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==16 ) THEN
          IF ( Parmname(:16)=='executable_model' ) THEN
            IF ( Iflag==0 ) THEN
              Executable_model = Parameter_values(1)
            ELSE
              Parameter_values(1) = Executable_model
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( nc==17 ) THEN
          IF ( Parmname(:17)=='model_output_file' ) THEN
            IF ( Iflag==0 ) THEN
              Model_output_file = Parameter_values(1)
            ELSE
              Parameter_values(1) = Model_output_file
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSEIF ( Parmname(:12)=='modflow_name' ) THEN
          IF ( Iflag==0 ) THEN
            Modflow_name = Parameter_values(1)
          ELSE
            Parameter_values(1) = Modflow_name
          ENDIF
        ELSEIF ( Parmname(:18)=='gsflow_output_file' ) THEN
          IF ( Iflag==0 ) THEN
            Gsflow_output_file = Parameter_values(1)
          ELSE
            Parameter_values(1) = Gsflow_output_file
          ENDIF
        ELSE
          Iflag = -1
        ENDIF
        IF ( Numvalues/=1 ) THEN
          IF ( Iflag/=-1 .AND. Parmname(:15)/='mapOutVar_names' .AND. Parmname(:15)/='statVar_element' .AND. &
       &       Parmname(:13)/='statVar_names' .AND. Parmname(:13)/='dispVar_names' .AND. Parmname(:15)/='dispVar_element' ) Iflag = -2
        ENDIF
         ! ignore these for now
         ! aniOutVar_names, dispVar_element, dispVar_names, dispVar_plot
      ELSEIF ( Param_type==2 ) THEN
        IF ( nc==14 ) THEN
          IF ( Parmname(:14)=='initial_deltat' ) THEN
            IF ( Numvalues/=1 ) Iflag = -2
            IF ( Iflag==0 ) THEN
              Initial_deltat = Real_parameter_values(1)
            ELSE
              Real_parameter_values = Initial_deltat
            ENDIF
          ELSE
            Iflag = -1
          ENDIF
        ELSE
          Iflag = -1
        ENDIF
      ENDIF

      END SUBROUTINE set_control_parameter

      ! fix so it works in standalone PRMS
!***********************************************************************
! Get Control File from command line or user interaction.
!***********************************************************************
      SUBROUTINE get_control_filename(Control_file, Nchars)
      USE PRMS_CONTROL_FILE, ONLY: PlotsON_OFF
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH, Print_debug, EQULS
      IMPLICIT NONE
      ! Functions
      INTEGER, EXTERNAL :: numchars
      INTRINSIC :: GET_COMMAND_ARGUMENT
      ! Arguments
      CHARACTER(LEN=*), INTENT(OUT) :: Control_file
      INTEGER, INTENT(OUT) :: Nchars
      ! Local Variables
      CHARACTER(LEN=MAXFILE_LENGTH) command_line_arg, command_line_arg2
      LOGICAL exists
      INTEGER status, first, nchars_rtg, status2
!***********************************************************************
! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
! This routine expects the Control File name to be the first argument, if present
      PRINT *, ' '
      Nchars = 0
      CALL GET_COMMAND_ARGUMENT(1, command_line_arg, Nchars, status)
      nchars_rtg = 0
      CALL GET_COMMAND_ARGUMENT(2, command_line_arg2, nchars_rtg, status2)
      IF ( status2==0 ) THEN
        IF ( Print_debug>-1 ) THEN
          PRINT *, EQULS
          PRINT *, 'PRMS command line argument 2: ', command_line_arg2(:nchars_rtg), nchars_rtg, status2
        ENDIF
        PlotsON_OFF = 0
        IF ( command_line_arg2(:nchars_rtg)=='-rtg' ) PlotsON_OFF = 1
      ENDIF
      IF ( status==0 ) THEN
        IF ( Print_debug>-1 ) THEN
          PRINT *, EQULS
          PRINT *, 'PRMS command line argument 1: ', command_line_arg(:nchars), nchars, status
        ENDIF
        Control_file = command_line_arg(:Nchars)
        first = 1
        IF ( Control_file(:2)=='-C' ) first = 3
        INQUIRE ( FILE=Control_file(first:Nchars), EXIST=exists )
        IF ( first==3 ) Control_file = command_line_arg(3:Nchars)
        IF ( exists ) RETURN
        PRINT *, 'Control File does not exist based on command-line argument: '//Control_file(first:Nchars)
      ENDIF
      DO
        WRITE ( *,'(/,A)' ) 'Enter the name of the PRMS Control File or quit:'
        READ ( *, '(A)' ) Control_file
        IF ( Control_file(:4)=='quit' .OR. Control_file(:4)=='QUIT' ) STOP
        Nchars = numchars(Control_file)
        INQUIRE ( FILE=Control_file(first:Nchars), EXIST=exists )
        IF ( exists ) RETURN
        WRITE ( *,'(/,A)' ) 'Control File does not exist based on response: '//Control_file(:Nchars)
        PRINT *, 'Note: Control File names cannot include spaces'
      ENDDO
      Nchars = 0
      CALL GET_COMMAND_ARGUMENT(2, command_line_arg, Nchars, status)
      IF ( status==0 ) THEN
        IF ( Print_debug>-1 ) THEN
          PRINT *, EQULS
          PRINT *, 'PRMS command line argument 2: ', command_line_arg(:nchars), nchars, status
        ENDIF
        IF ( command_line_arg(:nchars)=='-rtg' ) PlotsON_OFF = 1
      ENDIF

      END SUBROUTINE get_control_filename
