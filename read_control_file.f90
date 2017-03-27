!***********************************************************************
! Read Control File
!***********************************************************************
      MODULE PRMS_CONTROL_FILE

        USE PRMS_MODULE, ONLY: Print_debug, EQULS, statsON_OFF, MAXCONTROL_LENGTH, MAXFILE_LENGTH, &
     &      Init_vars_from_file, Save_vars_to_file, Parameter_check_flag, Param_file, Model_output_file, &
     &      Precip_module, Temp_module, Et_module, Srunoff_module, Solrad_module, Gwr_swale_flag, &
     &      Strmflow_module, Transp_module, Soilzone_module, Print_debug, Dprst_flag, Subbasin_flag, Frozen_flag, &
     &      CsvON_OFF, MapOutON_OFF, Model_mode, Orad_flag, Endtime, Starttime, Snow_cbh_flag, Stream_temp_flag, &
     &      Cascade_flag, Cascadegw_flag, Prms_warmup, Humidity_cbh_flag, Windspeed_cbh_flag, &
     &      Gwflow_cbh_flag, NhruOutON_OFF, NsubOutON_OFF, Dyn_imperv_flag, Dyn_dprst_flag, Dyn_intcp_flag, &
     &      Dyn_covtype_flag, Dyn_potet_flag, Dyn_transp_flag, Dyn_soil_flag, Dyn_radtrncf_flag, Dyn_transp_on_flag, &
     &      Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Dyn_fallfrost_flag, &
     &      Dyn_springfrost_flag, Dyn_snareathresh_flag, Dyn_covden_flag, Segment_transferON_OFF, Gwr_transferON_OFF, &
     &      Lake_transferON_OFF, External_transferON_OFF, Dprst_transferON_OFF
        USE GSFMODFLOW, ONLY: Modflow_name, Modflow_time_zero
        USE PRMS_CLIMATE_HRU, ONLY: Precip_day, Tmax_day, Tmin_day, Potet_day, Transp_day, Swrad_day, &
     &      Cbh_check_flag, Cbh_binary_flag, Windspeed_day, Humidity_day
        USE GSFSUM, ONLY: Gsf_rpt, Rpt_days, Gsflow_output_file, Csv_output_file
        USE PRMS_MAP_RESULTS, ONLY: NmapOutVars, MapOutVar_names
        USE PRMS_NHRU_SUMMARY, ONLY: NhruOutVars, NhruOut_freq, NhruOutBaseFileName, NhruOutVar_names
        USE PRMS_NSUB_SUMMARY, ONLY: NsubOutVars, NsubOut_freq, NsubOutBaseFileName, NsubOutVar_names
        USE PRMS_DYNAMIC_PARAM_READ, ONLY: imperv_frac_dynamic, imperv_stor_dynamic, dprst_depth_dynamic, dprst_frac_dynamic, &
     &      wrain_intcp_dynamic, srain_intcp_dynamic, snow_intcp_dynamic, covtype_dynamic, &
     &      potetcoef_dynamic, transpbeg_dynamic, transpend_dynamic, &
     &      soilmoist_dynamic, soilrechr_dynamic, radtrncf_dynamic, &
     &      fallfrost_dynamic, springfrost_dynamic, transp_on_dynamic, snareathresh_dynamic, &
     &      covden_sum_dynamic, covden_win_dynamic, sro2dprst_perv_dyn, sro2dprst_imperv_dyn

        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Data_file, Var_init_file, Stat_var_file, Ani_out_file
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Executable_desc, Executable_model, Var_save_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: Control_file, Ani_output_file, Control_description
        INTEGER, SAVE :: PlotsON_OFF, Num_control_parameters, Glacier_flag, Stream_temp_shade_flag
        INTEGER, SAVE :: NstatVars, AniOutON_OFF, NaniOutVars, NdispGraphs, DispGraphsBuffSize, Param_file_control_parameter_id
        INTEGER, SAVE :: Num_statvar_elements, Num_statvar_names, NumdispVar_names, NumdispVar_elements
        REAL, SAVE :: Initial_deltat
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: statVar_element(:), statVar_names(:), param_file_names(:)
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: dispVar_element(:), dispVar_names(:)
        ! read_flag: 0 = not set, 1 = set from control file, 2 = set to default, 3 = means variably dimension, 4 = variably dimension set from Control File
        TYPE PRMS_control_parameter
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: name
             INTEGER :: numvals, read_flag, data_type, index
             INTEGER, ALLOCATABLE :: values_int(:)
             REAL, ALLOCATABLE :: values_real(:)
             CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: values_character(:)
        END TYPE PRMS_control_parameter
        TYPE ( PRMS_control_parameter ), SAVE, ALLOCATABLE :: Control_parameter_data(:)
      END MODULE PRMS_CONTROL_FILE

      SUBROUTINE read_control_file
      USE PRMS_CONTROL_FILE
      USE PRMS_MODULE, ONLY: Version_read_control_file, Print_debug, Model, PRMS_output_unit, Model_output_file
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      INTEGER, EXTERNAL :: numchars, control_string
      EXTERNAL read_error, set_control_parameter, PRMS_open_input_file, write_outfile, PRMS_open_output_file !, print_module
      ! Local Variables
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramname
      CHARACTER(LEN=4) :: string
      INTEGER nchars, ios, numvalues, param_type, control_unit, j, iret
      INTEGER, ALLOCATABLE :: int_parameter_values(:)
      CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: parameter_values(:)
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramstring
      REAL, ALLOCATABLE :: real_parameter_values(:)
!***********************************************************************
      Version_read_control_file = 'read_control_file.f90 2017-03-27 13:28:00Z'

      ! control filename cannot include blanks
      CALL get_control_filename(Control_file, nchars)
      CALL PRMS_open_input_file(control_unit, Control_file, 'control_file', 0, ios)
      IF ( ios/=0 ) CALL read_error(10, TRIM(Control_file))
      ! read header
      READ (control_unit, '(A)', IOSTAT=ios ) Control_description
      IF ( ios/=0 ) CALL read_error(12, TRIM(Control_file))

      CALL setup_cont() ! set default control parameter values

      ! Read all Control Parameters
      DO
        READ ( control_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of Control File
        IF ( ios/=0 ) CALL read_error(12, 'missing #### delimiter')
        IF ( string(:4)/='####' ) CYCLE ! skip until delimiter found, such as blank of // comment lines
        READ ( control_unit, '(A)', IOSTAT=ios ) paramname ! parameter name
        IF ( ios/=0 ) CALL read_error(5, 'missing parameter name')
        READ ( control_unit, *, IOSTAT=ios ) numvalues
        IF ( ios/=0 ) CALL read_error(5, 'invalid number of values: '//TRIM(paramname) )
        READ ( control_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(5, 'invalid parameter type: '//TRIM(paramstring) )
        IF ( param_type<1 .OR. param_type>4 .OR. param_type==3 ) CALL read_error(5, 'invalid parameter type: '//TRIM(paramstring) )
        ALLOCATE ( int_parameter_values(numvalues), real_parameter_values(numvalues), parameter_values(numvalues) )
        IF ( param_type==1 ) THEN
          READ ( Control_unit, *, IOSTAT=ios ) (int_parameter_values(j),j=1,numvalues)
          IF ( ios/=0 ) CALL read_error(5, 'invalid integer value: '//TRIM(paramname) )
        ELSEIF ( param_type==4 ) THEN
          DO j = 1, numvalues
            READ ( Control_unit, '(A)', IOSTAT=ios ) parameter_values(j)
            IF ( ios/=0 ) CALL read_error(5, 'invalid character value: '//TRIM(paramname) )
          ENDDO
        ELSE
          READ ( Control_unit, *, IOSTAT=ios ) (real_parameter_values(j),j=1,numvalues)
          IF ( ios/=0 ) CALL read_error(5, 'invalid real value: '//TRIM(paramname) )
        ENDIF
        CALL set_control_parameter(paramname, numvalues, int_parameter_values, real_parameter_values, parameter_values)
        DEALLOCATE ( int_parameter_values, real_parameter_values, parameter_values )
      ENDDO
      IF ( Print_debug>-1 ) PRINT *, EQULS
      CLOSE ( control_unit )

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'prms.out')
      IF ( Print_debug>-2 ) THEN
        IF ( Model/=2 .OR. Model/=3 .OR. Model/=5 .OR. Model/=6 ) THEN
          CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
          IF ( iret/=0 ) STOP
        ENDIF
      ENDIF

      END SUBROUTINE read_control_file

!***********************************************************************
! setup_cont - Set control parameter values
!***********************************************************************
      SUBROUTINE setup_cont()
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Local Variables
      INTEGER i, numvalues
!***********************************************************************
      Num_control_parameters = 121 ! WARNING, hard coded, DANGER, DANGER
      ! allocate and store parameter data
      ALLOCATE ( Control_parameter_data(Num_control_parameters+20) ) ! allow for extra parameters being expected
      DO i = 1, Num_control_parameters
        Control_parameter_data(i)%read_flag = 2 ! set to default
        Control_parameter_data(i)%data_type = 1 ! 1 = integer, 2 = real, 4 = string
        Control_parameter_data(i)%numvals = 1
        Control_parameter_data(i)%name = ' '
        ! WARNING, parameter index is set based on order defaults defined
        Control_parameter_data(i)%index = i
      ENDDO

      DO i = Num_control_parameters+1, Num_control_parameters+20
        Control_parameter_data(i)%read_flag = 0 ! 0 means not set
        Control_parameter_data(i)%data_type = 0 ! 1 = integer, 2 = real, 4 = string
        Control_parameter_data(i)%numvals = 0
        Control_parameter_data(i)%name = ' '
        ! WARNING, parameter index is set based on order defaults defined
        Control_parameter_data(i)%index = i
      ENDDO

      ! assign default value for integer flags
      numvalues = 1
      i = 1
      Control_parameter_data(i)%name = 'print_debug'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Print_debug = 0
      Control_parameter_data(i)%values_int(numvalues) = Print_debug
      i = i + 1
      Control_parameter_data(i)%name = 'parameter_check_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Parameter_check_flag = 1
      Control_parameter_data(i)%values_int(numvalues) = Parameter_check_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dprst_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dprst_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cascade_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Cascade_flag = 1
      Control_parameter_data(i)%values_int(numvalues) = Cascade_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cascadegw_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Cascadegw_flag = 1
      Control_parameter_data(i)%values_int(numvalues) = Cascadegw_flag
      i = i + 1
      Control_parameter_data(i)%name = 'save_vars_to_file'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Save_vars_to_file = 0
      Control_parameter_data(i)%values_int(numvalues) = Save_vars_to_file
      i = i + 1
      Control_parameter_data(i)%name = 'init_vars_from_file'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Init_vars_from_file = 0
      Control_parameter_data(i)%values_int(numvalues) = Init_vars_from_file
      i = i + 1
      Control_parameter_data(i)%name = 'frozen_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Frozen_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Frozen_flag
      i = i + 1
      Control_parameter_data(i)%name = 'glacier_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Glacier_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Glacier_flag
      i = i + 1
      Control_parameter_data(i)%name = 'stream_temp_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Stream_temp_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Stream_temp_flag
      i = i + 1
      Control_parameter_data(i)%name = 'stream_temp_shade_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Stream_temp_shade_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Stream_temp_shade_flag
      i = i + 1
      Control_parameter_data(i)%name = 'orad_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Orad_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Orad_flag
      i = i + 1
      Control_parameter_data(i)%name = 'subbasin_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Subbasin_flag = 1
      Control_parameter_data(i)%values_int(numvalues) = Subbasin_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cbh_check_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Cbh_check_flag = 1
      Control_parameter_data(i)%values_int(numvalues) = Cbh_check_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cbh_binary_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Cbh_binary_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Cbh_binary_flag
      i = i + 1
      Control_parameter_data(i)%name = 'gwr_swale_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Gwr_swale_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Gwr_swale_flag
      i = i + 1
      Control_parameter_data(i)%name = 'snow_cbh_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Snow_cbh_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Snow_cbh_flag
      i = i + 1
      Control_parameter_data(i)%name = 'gwflow_cbh_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Gwflow_cbh_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Gwflow_cbh_flag
      i = i + 1
      Control_parameter_data(i)%name = 'humidity_cbh_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Humidity_cbh_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Humidity_cbh_flag
      i = i + 1
      Control_parameter_data(i)%name = 'windspeed_cbh_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Windspeed_cbh_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Windspeed_cbh_flag
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NhruOutON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = NhruOutON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOut_freq'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NhruOut_freq = 1
      Control_parameter_data(i)%values_int(numvalues) = NhruOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutVars'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NhruOutVars = 0
      Control_parameter_data(i)%values_int(numvalues) = NhruOutVars
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NsubOutON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = NsubOutON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutVars'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NsubOutVars = 0
      Control_parameter_data(i)%values_int(numvalues) = NsubOutVars
      i = i + 1
      Control_parameter_data(i)%name = 'statsON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      StatsON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = StatsON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'csvON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      CsvON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = CsvON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'aniOutON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      AniOutON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = AniOutON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'mapOutON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      MapOutON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = MapOutON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'nstatVars'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NstatVars = 0
      Control_parameter_data(i)%values_int(numvalues) = NstatVars
      i = i + 1
      Control_parameter_data(i)%name = 'nmapOutVars'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NmapOutVars = 0
      Control_parameter_data(i)%values_int(numvalues) = NmapOutVars
      i = i + 1
      Control_parameter_data(i)%name = 'naniOutVars'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NaniOutVars = 0
      Control_parameter_data(i)%values_int(numvalues) = NaniOutVars
      i = i + 1
      Control_parameter_data(i)%name = 'ndispGraphs'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NdispGraphs = 0
      Control_parameter_data(i)%values_int(numvalues) = NdispGraphs
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutVars'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NsubOutVars = 0
      Control_parameter_data(i)%values_int(numvalues) = NsubOutVars
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOut_freq'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      NsubOut_freq = 1
      Control_parameter_data(i)%values_int(numvalues) = NsubOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'prms_warmup'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Prms_warmup = 1
      Control_parameter_data(i)%values_int(numvalues) = Prms_warmup
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_imperv_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_imperv_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_imperv_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_intcp_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_intcp_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_intcp_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_covden_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_covden_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_covden_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dispGraphsBuffSize'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      DispGraphsBuffSize = 50
      Control_parameter_data(i)%values_int(numvalues) = DispGraphsBuffSize
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_transp_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_transp_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_transp_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_transp_on_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_transp_on_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_transp_on_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_sro2dprst_perv_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_sro2dprst_perv_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_sro2dprst_perv_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_sro2dprst_imperv_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_sro2dprst_imperv_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_sro2dprst_imperv_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_covtype_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_covtype_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_covtype_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_fallfrost_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_fallfrost_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_fallfrost_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_springfrost_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_springfrost_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_springfrost_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_potet_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_potet_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_potet_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_soil_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_soil_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_soil_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_radtrncf_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_radtrncf_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_radtrncf_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_snareathresh_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_snareathresh_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_snareathresh_flag
      i = i + 1
!      Control_parameter_data(i)%name = 'dyn_sro_to_dprst_flag'
!      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
!      Dyn_sro_to_dprst_flag = 0
!      Control_parameter_data(i)%values_int(numvalues) = Dyn_sro_to_dprst_flag
!      i = i + 1
!      Control_parameter_data(i)%name = 'dyn_sro_to_imperv_flag'
!      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
!      Dyn_sro_to_imperv_flag = 0
!      Control_parameter_data(i)%values_int(numvalues) = Dyn_sro_to_imperv_flag
!      i = i + 1
      Control_parameter_data(i)%name = 'dyn_dprst_flag'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dyn_dprst_flag = 0
      Control_parameter_data(i)%values_int(numvalues) = Dyn_dprst_flag
      i = i + 1
      Control_parameter_data(i)%name = 'segment_transferON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Segment_transferON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = Segment_transferON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'gwr_transferON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Gwr_transferON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = Gwr_transferON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'external_transferON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      External_transferON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = External_transferON_OFF
      i = i + 1
!      Control_parameter_data(i)%name = 'consumed_transferON_OFF'
!      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
!      Consumed_transferON_OFF = 0
!      Control_parameter_data(i)%values_int(numvalues) = Consumed_transferON_OFF
!      i = i + 1
      Control_parameter_data(i)%name = 'lake_transferON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Lake_transferON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = Lake_transferON_OFF
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_transferON_OFF'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Dprst_transferON_OFF = 0
      Control_parameter_data(i)%values_int(numvalues) = Dprst_transferON_OFF
      i = i + 1
!      Control_parameter_data(i)%name = 'soilzone_transferON_OFF'
!      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
!      Soilzone_transferON_OFF = 0
!      Control_parameter_data(i)%values_int(numvalues) = Soilzone_transferON_OFF
!      i = i + 1      
!      Control_parameter_data(i)%name = 'canopy_transferON_OFF'
!      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
!      Canopy_transferON_OFF = 0
!      Control_parameter_data(i)%values_int(numvalues) = Canopy_transferON_OFF
!      i = i + 1 

      ! parameters that get allocated if in Control File
      Control_parameter_data(i)%name = 'mapOutVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'statVar_element'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'statVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
!      Control_parameter_data(i)%name = 'aniOutVar_names'
!      Control_parameter_data(i)%data_type = 4
!      Control_parameter_data(i)%read_flag = 3 ! need to allocate
!      i = i + 1
      Control_parameter_data(i)%name = 'dispVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
!      Control_parameter_data(i)%name = 'dispVar_plot'
!      Control_parameter_data(i)%data_type = 4
!      Control_parameter_data(i)%read_flag = 3 ! need to allocate
!      i = i + 1
      Control_parameter_data(i)%name = 'dispVar_element'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutVar_names'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      i = i + 1

      Control_parameter_data(i)%name = 'gsf_rpt'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Gsf_rpt = 1
      Control_parameter_data(i)%values_int(numvalues) = Gsf_rpt
      i = i + 1
      Control_parameter_data(i)%name = 'rpt_days'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Rpt_days = 7
      Control_parameter_data(i)%values_int(numvalues) = Rpt_days
      i = i + 1

      !!!! add dynamic and water use

! floating point parameters
      Control_parameter_data(i)%name = 'initial_deltat'
      ALLOCATE ( Control_parameter_data(i)%values_real(numvalues) )
      Initial_deltat = 24.0
      Control_parameter_data(i)%values_real(numvalues) = Initial_deltat
      Control_parameter_data(i)%data_type = 2
      i = i + 1

      ! assign default value for character parameters
      numvalues = 1
      Control_parameter_data(i)%name = 'model_mode'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Model_mode = 'PRMS'
      Control_parameter_data(i)%values_character(numvalues) = Model_mode
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'executable_desc'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Executable_desc = 'PRMS_5'
      Control_parameter_data(i)%values_character(numvalues) = Executable_desc
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'executable_model'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Executable_model = 'prms'
      Control_parameter_data(i)%values_character(numvalues) = Executable_model
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'precip_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Precip_module = 'precip_1sta'
      Control_parameter_data(i)%values_character(numvalues) = Precip_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'temp_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Temp_module = 'temp_1sta'
      Control_parameter_data(i)%values_character(numvalues) = Temp_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'solrad_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Solrad_module = 'ddsolrad'
      Control_parameter_data(i)%values_character(numvalues) = Solrad_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'et_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Et_module = 'potet_jh'
      Control_parameter_data(i)%values_character(numvalues) = Et_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'srunoff_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Srunoff_module = 'srunoff_smidx'
      Control_parameter_data(i)%values_character(numvalues) = Srunoff_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'strmflow_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Strmflow_module = 'strmflow'
      Control_parameter_data(i)%values_character(numvalues) = Strmflow_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'transp_module'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Transp_module = 'transp_tindex'
      Control_parameter_data(i)%values_character(numvalues) = Transp_module
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'data_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Data_file = 'prms.data'
      Control_parameter_data(i)%values_character(numvalues) = Data_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'param_file'         !!!! make multiple
      Param_file = 'prms.params'
      Control_parameter_data(i)%data_type = 4
      Control_parameter_data(i)%read_flag = 3 ! need to allocate
      Param_file_control_parameter_id = i
      i = i + 1
      Control_parameter_data(i)%name = 'model_output_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Model_output_file = 'prms.out'
      Control_parameter_data(i)%values_character(numvalues) = Model_output_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'csv_output_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Csv_output_file = 'prms_summary.csv'
      Control_parameter_data(i)%values_character(numvalues) = Csv_output_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'var_save_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Var_save_file = 'prms_ic.out'
      Control_parameter_data(i)%values_character(numvalues) = Var_save_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'var_init_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Var_init_file = 'prms_ic.in'
      Control_parameter_data(i)%values_character(numvalues) = Var_init_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'stat_var_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Stat_var_file = 'statvar.out'
      Control_parameter_data(i)%values_character(numvalues) = Stat_var_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'ani_output_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Ani_output_file = 'animation.out'
      Control_parameter_data(i)%values_character(numvalues) = Ani_output_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutBaseFileName'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      NhruOutBaseFileName = 'nhruout_path'
      Control_parameter_data(i)%values_character(numvalues) = NhruOutBaseFileName
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutBaseFileName'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      NsubOutBaseFileName = 'nsubout_path'
      Control_parameter_data(i)%values_character(numvalues) = NsubOutBaseFileName
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'tmax_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Tmax_day = 'tmax_day'
      Control_parameter_data(i)%values_character(numvalues) = Tmax_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'tmin_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Tmin_day = 'tmin_day'
      Control_parameter_data(i)%values_character(numvalues) = Tmin_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'precip_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Precip_day = 'precip_day'
      Control_parameter_data(i)%values_character(numvalues) = Precip_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'swrad_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Swrad_day = 'swrad_day'
      Control_parameter_data(i)%values_character(numvalues) = Swrad_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'potet_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Potet_day = 'potet_day'
      Control_parameter_data(i)%values_character(numvalues) = Potet_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'transp_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Transp_day = 'transp_day'
      Control_parameter_data(i)%values_character(numvalues) = Transp_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'windspeed_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Windspeed_day = 'windspeed_day'
      Control_parameter_data(i)%values_character(numvalues) = Windspeed_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'humidity_day'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Humidity_day = 'humidity_day'
      Control_parameter_data(i)%values_character(numvalues) = Humidity_day
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      ! GSFLOW parameters
      Control_parameter_data(i)%name = 'modflow_name'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Modflow_name = 'modflow.nam'
      Control_parameter_data(i)%values_character(numvalues) = Modflow_name
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'gsflow_output_file'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Gsflow_output_file = 'gsflow.out'
      Control_parameter_data(i)%values_character(numvalues) = Gsflow_output_file
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_depth_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Dprst_depth_dynamic = 'dyndprst_depth'
      Control_parameter_data(i)%values_character(numvalues) = Dprst_depth_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_frac_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Dprst_frac_dynamic = 'dyndprst_frac'
      Control_parameter_data(i)%values_character(numvalues) = Dprst_frac_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'snow_intcp_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Snow_intcp_dynamic = 'dynsnowintcp'
      Control_parameter_data(i)%values_character(numvalues) = Snow_intcp_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'srain_intcp_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Srain_intcp_dynamic = 'dynsrainintcp'
      Control_parameter_data(i)%values_character(numvalues) = Srain_intcp_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'wrain_intcp_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Wrain_intcp_dynamic = 'dynwrainintcp'
      Control_parameter_data(i)%values_character(numvalues) = Wrain_intcp_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'imperv_frac_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Imperv_frac_dynamic = 'dynimperv_frac'
      Control_parameter_data(i)%values_character(numvalues) = Imperv_frac_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'imperv_stor_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Imperv_stor_dynamic = 'dynimperv_stor'
      Control_parameter_data(i)%values_character(numvalues) = Imperv_stor_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'covtype_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Covtype_dynamic = 'dyncovtype'
      Control_parameter_data(i)%values_character(numvalues) = Covtype_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'covden_sum_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Covden_sum_dynamic = 'dyncovden_sum'
      Control_parameter_data(i)%values_character(numvalues) = Covden_sum_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'covden_win_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Covden_win_dynamic = 'dyncovden_win'
      Control_parameter_data(i)%values_character(numvalues) = Covden_win_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'potetcoef_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Potetcoef_dynamic = 'dynpotetcoef'
      Control_parameter_data(i)%values_character(numvalues) = Potetcoef_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'transpbeg_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Transpbeg_dynamic = 'dyntranspbeg'
      Control_parameter_data(i)%values_character(numvalues) = Transpbeg_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'transpend_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Transpend_dynamic = 'dyntranspend'
      Control_parameter_data(i)%values_character(numvalues) = Transpend_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'fallfrost_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Fallfrost_dynamic = 'dynfallfrost'
      Control_parameter_data(i)%values_character(numvalues) = Fallfrost_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'springfrost_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Springfrost_dynamic = 'dynspringfrost'
      Control_parameter_data(i)%values_character(numvalues) = Springfrost_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'soilrechr_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Soilrechr_dynamic = 'dynsoilrechr'
      Control_parameter_data(i)%values_character(numvalues) = Soilrechr_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'soilmoist_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Soilmoist_dynamic = 'dynsoilmoist'
      Control_parameter_data(i)%values_character(numvalues) = Soilmoist_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'radtrncf_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Radtrncf_dynamic = 'dynradtrnch'
      Control_parameter_data(i)%values_character(numvalues) = Radtrncf_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'sro2dprst_perv_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Sro2dprst_perv_dyn = 'dynsro2dprst_perv'
      Control_parameter_data(i)%values_character(numvalues) = Sro2dprst_perv_dyn
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'sro2dprst_imperv_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Sro2dprst_imperv_dyn = 'dynsro2dprst_imperv'
      Control_parameter_data(i)%values_character(numvalues) = Sro2dprst_imperv_dyn
      Control_parameter_data(i)%data_type = 4
      i = i + 1
      Control_parameter_data(i)%name = 'transp_on_dynamic'
      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
      Transp_on_dynamic = 'dyntranspon'
      Control_parameter_data(i)%values_character(numvalues) = Transp_on_dynamic
      Control_parameter_data(i)%data_type = 4
      i = i + 1
!      Control_parameter_data(i)%name = 'stats_output_file'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Stats_output_file = 'stats.out'
!      Control_parameter_data(i)%values_character(numvalues) = Stats_output_file
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
!      Control_parameter_data(i)%name = 'pkwater_equiv_day'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Pkwater_equiv_day = 'pkwater_equiv.day'
!      Control_parameter_data(i)%values_character(numvalues) = Pkwater_equiv_day
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
!      Control_parameter_data(i)%name = 'pk_depth_day'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Pk_depth_day = 'pk_depth.day'
!      Control_parameter_data(i)%values_character(numvalues) = Pk_depth_day
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
!      Control_parameter_data(i)%name = 'snow_evap_day'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Snow_evap_day = 'snow_evap.day'
!      Control_parameter_data(i)%values_character(numvalues) = Snow_evap_day
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
!      Control_parameter_data(i)%name = 'snowcov_area_day'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Snowcov_area_day = 'snowcov_area.day'
!      Control_parameter_data(i)%values_character(numvalues) = Snowcov_area_day
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
!      Control_parameter_data(i)%name = 'snowmelt_day'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Snowmelt_day = 'snowmelt.day'
!      Control_parameter_data(i)%values_character(numvalues) = Snowmelt_day
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
!      Control_parameter_data(i)%name = 'gwres_flow_day'
!      ALLOCATE ( Control_parameter_data(i)%values_character(numvalues) )
!      Gwres_flow_day = 'gwres_flow.day'
!      Control_parameter_data(i)%values_character(numvalues) = Gwres_flow_day
!      Control_parameter_data(i)%data_type = 4
!      i = i + 1
          
      ! time arrays
      numvalues = 6
      Control_parameter_data(i)%name = 'start_time'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Starttime(1) = 2000
      Starttime(2) = 10
      Starttime(3) = 1
      Starttime(4) = 0
      Starttime(5) = 0
      Starttime(6) = 0
      Control_parameter_data(i)%values_int(1) = Starttime(1)
      Control_parameter_data(i)%values_int(2) = Starttime(2)
      Control_parameter_data(i)%values_int(3) = Starttime(3)
      Control_parameter_data(i)%values_int(4) = Starttime(4)
      Control_parameter_data(i)%values_int(5) = Starttime(5)
      Control_parameter_data(i)%values_int(6) = Starttime(6)
      Control_parameter_data(i)%numvals = 6
      i = i + 1
      Control_parameter_data(i)%name = 'end_time'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Endtime(1) = 2001
      Endtime(2) = 9
      Endtime(3) = 30
      Endtime(4) = 0
      Endtime(5) = 0
      Endtime(6) = 0
      Control_parameter_data(i)%values_int(1) = Endtime(1)
      Control_parameter_data(i)%values_int(2) = Endtime(2)
      Control_parameter_data(i)%values_int(3) = Endtime(3)
      Control_parameter_data(i)%values_int(4) = Endtime(4)
      Control_parameter_data(i)%values_int(5) = Endtime(5)
      Control_parameter_data(i)%values_int(6) = Endtime(6)
      Control_parameter_data(i)%numvals = 6
      i = i + 1

      ! GSFLOW parameters
      Control_parameter_data(i)%name = 'modflow_time_zero'
      ALLOCATE ( Control_parameter_data(i)%values_int(numvalues) )
      Control_parameter_data(i)%values_int(1) = -999 
      Modflow_time_zero(1) = -999 ! set to negative, so default can be set to start_time in code
      Modflow_time_zero(2) = 10
      Modflow_time_zero(3) = 1
      Modflow_time_zero(4) = 0
      Modflow_time_zero(5) = 0
      Modflow_time_zero(6) = 0
      Control_parameter_data(i)%values_int(1) = Modflow_time_zero(1)
      Control_parameter_data(i)%values_int(2) = Modflow_time_zero(2)
      Control_parameter_data(i)%values_int(3) = Modflow_time_zero(3)
      Control_parameter_data(i)%values_int(4) = Modflow_time_zero(4)
      Control_parameter_data(i)%values_int(5) = Modflow_time_zero(5)
      Control_parameter_data(i)%values_int(6) = Modflow_time_zero(6)
      Control_parameter_data(i)%numvals = 6

      END SUBROUTINE setup_cont

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
      CHARACTER(LEN=MAXFILE_LENGTH), INTENT(OUT) :: Control_file
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
      IF ( Print_debug>-1 ) PRINT *, EQULS
      IF ( status2==0 ) THEN
        IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument 2: ', command_line_arg2(:nchars_rtg) !, nchars_rtg, status2
        PlotsON_OFF = 0
        IF ( command_line_arg2(:nchars_rtg)=='-rtg' ) PlotsON_OFF = 1
      ENDIF
      IF ( status==0 ) THEN
        IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument 1: ', command_line_arg(:nchars) !, nchars, status
        Control_file = command_line_arg(:Nchars)
        first = 1
        IF ( Control_file(:2)=='-C' ) first = 3
        INQUIRE ( FILE=Control_file(first:Nchars), EXIST=exists )
        IF ( first==3 ) Control_file = command_line_arg(3:Nchars)
        IF ( Print_debug>-1 ) PRINT *, EQULS
        IF ( exists ) RETURN
        PRINT *, 'Control File does not exist based on command-line argument: '//Control_file(first:Nchars)
      ENDIF
      DO
        WRITE ( *,'(/,A)' ) 'Enter the name of the PRMS Control File or quit:'
        READ ( *, '(A)' ) Control_file
        IF ( Control_file(:4)=='quit' .OR. Control_file(:4)=='QUIT' ) STOP
        Nchars = numchars(Control_file)
        INQUIRE ( FILE=Control_file(first:Nchars), EXIST=exists )
        IF ( Print_debug>-1 ) PRINT *, EQULS
        IF ( exists ) RETURN
        WRITE ( *,'(/,A)' ) 'Control File does not exist based on response: '//Control_file(:Nchars)
        PRINT *, 'Note: Control File names cannot include spaces'
      ENDDO
      Nchars = 0
      CALL GET_COMMAND_ARGUMENT(2, command_line_arg, Nchars, status)
      IF ( status==0 ) THEN
        IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument 2: ', command_line_arg(:nchars), nchars, status
        IF ( command_line_arg(:nchars)=='-rtg' ) PlotsON_OFF = 1
      ENDIF
      IF ( Print_debug>-1 ) PRINT *, EQULS
      END SUBROUTINE get_control_filename

!***********************************************************************
! check control parameter if in Control File
!***********************************************************************
      SUBROUTINE set_control_parameter(Paramname, Numvalues, Paramval_int, Paramval_real, Paramval_char) ! allow arrays
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=MAXCONTROL_LENGTH), INTENT(IN) :: Paramname
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, INTENT(IN) :: Paramval_int(Numvalues)
      REAL, INTENT(IN) :: Paramval_real(Numvalues)
      CHARACTER(LEN=MAXFILE_LENGTH), INTENT(IN) :: Paramval_char(Numvalues)
      ! Functions
      INTRINSIC :: TRIM
      ! Local Variables
      INTEGER :: i, j, found, dtype
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          found = i
          dtype = Control_parameter_data(i)%data_type
          IF ( Control_parameter_data(i)%read_flag > 2 ) THEN ! one of variably sized parameters
            IF ( Control_parameter_data(i)%read_flag == 4 ) THEN
              PRINT *, 'ERROR, allocatable control parameter in Control File multiple times'
              PRINT *, '*** ', TRIM(Paramname), ' ***'
              STOP
            ENDIF
!!!!! DANGER, if control parameter in twice, this is a problem as could try to allocate again
            IF ( dtype==1 ) THEN
              ALLOCATE ( Control_parameter_data(i)%values_int(Numvalues) )
              Control_parameter_data(i)%read_flag = 4
            ELSEIF ( dtype==4 ) THEN
              ALLOCATE ( Control_parameter_data(i)%values_character(Numvalues) )
              Control_parameter_data(i)%read_flag = 4
            ELSE
              STOP 'ERROR, allocatable control parameter that is real'
            ENDIF
          ELSE
            Control_parameter_data(i)%read_flag = 1
          ENDIF
          Control_parameter_data(i)%numvals = Numvalues
          IF ( dtype==1 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_int(j) = Paramval_int(j)
            ENDDO
          ELSEIF ( dtype==4 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_character(j) = Paramval_char(j)
            ENDDO
          ELSE !IF ( dtype==2 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_real(j) = Paramval_real(j)
            ENDDO
          ENDIF
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'WARNING, control parameter not used: ', TRIM(Paramname), ', ignored'
        RETURN
      ENDIF

      END SUBROUTINE set_control_parameter
