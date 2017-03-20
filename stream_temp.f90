!***********************************************************************
! stream temperature module
!***********************************************************************
      MODULE PRMS_STRMTEMP
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=11), SAVE :: MODNAME
      INTEGER, SAVE, ALLOCATABLE :: Seg_hru_num(:), Seg_close(:)
      REAL, SAVE, ALLOCATABLE ::  Tyear(:,:), T30(:,:), T_ss(:), Seg_carea_inv(:)
      REAL, SAVE, ALLOCATABLE :: T_gw(:), T_ground(:), Tyr_avg(:), T30_avg(:) !, Flowsum(:)
      ! next variables only needed of strm_temp_shade_flag = 0
      REAL, SAVE, ALLOCATABLE :: Seg_lat(:), Shade_jday(:, :), Svi_jday(:, :)
      ! rsr, at some point it would be good to have option to input seg_lat and seg_elev
      REAL, SAVE, ALLOCATABLE :: Seg_elev(:), Press(:)
      REAL, SAVE, ALLOCATABLE :: Cos_seg_lat(:), Sin_seg_lat(:), Horizontal_hour_angle(:, :), Total_shade(:, :)
      REAL, SAVE, ALLOCATABLE :: Sin_declination(:, :), Sin_lat_decl(:, :), Cos_lat_decl(:, :), Sin_alrs(:, :)
      REAL, SAVE, ALLOCATABLE :: Max_solar_altitude(:, :), Level_sunset_azimuth(:, :)
      REAL, SAVE, ALLOCATABLE :: Local_sunset_hour_angle(:, :), Local_sunrise_hour_angle(:, :)
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Temp_avg(:), Upstrm_temp(:), Dlit(:)
      REAL, SAVE, ALLOCATABLE :: Seg_humid(:), Seg_maxtemp(:), Seg_width(:), Seg_ccov(:)
      REAL, SAVE, ALLOCATABLE :: T_roff(:), Seg_melt(:), Seg_rain(:)
      DOUBLE PRECISION, ALLOCATABLE :: Seg_potet(:)
!   Segment Parameters
      REAL, SAVE, ALLOCATABLE :: Seg_length(:) !, Mann_n(:)
      REAL, SAVE, ALLOCATABLE :: Seg_slope(:), Width_values(:, :)
      REAL, SAVE:: Width_flow(3)
      REAL, SAVE, ALLOCATABLE :: Gw_init(:), Ss_init(:)
      INTEGER, SAVE:: Width_dim, Maxiter_sntemp
      REAL, SAVE, ALLOCATABLE :: Seg_winter_humidity(:), Seg_summer_humidity(:)
      INTEGER, SAVE, ALLOCATABLE :: Seg_humidity_sta(:)
!   Shade Parameters needed if stream_temp_shade_flag = 0
      REAL, SAVE, ALLOCATABLE :: Azrh(:), Alte(:), Altw(:), Vce(:)
      REAL, SAVE, ALLOCATABLE :: Vdemx(:), Vhe(:), Voe(:), Vcw(:), Vdwmx(:), Vhw(:), Vow(:)
      REAL, SAVE, ALLOCATABLE :: Vdemn(:), Vdwmn(:)
  !??    DATA SPR, SUM, AUT, WIN / 75., 135., 290., 325./ !add parameters
      INTEGER, SAVE :: Spring_jday, Summer_jday, Autumn_jday, Winter_jday
!   Shade Parameters needed if stream_temp_shade_flag = 2
      REAL, SAVE, ALLOCATABLE :: Segshade_sum(:), Segshade_win(:)
      REAL, SAVE:: Albedo, Melt_temp
      ! INTEGER, SAVE :: Shadeflg, now using stream_temp_shade_flag
      INTEGER, SAVE :: Ss_tau, Gw_tau
!   Cloud Parameters
      REAL, SAVE, ALLOCATABLE :: Ccov_slope(:, :), Ccov_intcp(:, :)
!   Control parameters
      INTEGER, SAVE :: Strmtemp_humidity_flag, Stream_temp_shade_flag
!   Conversions
      INTRINSIC :: ACOS
      REAL, PARAMETER :: HALF_PI = ACOS(0.0), ZERO_C = 273.16
      REAL, PARAMETER :: PI = ACOS(-1.0)
      REAL, PARAMETER :: DEG_TO_RAD = PI / 180.0, DAYSYR = 365.242
      DOUBLE PRECISION :: MPS_CONVERT = 2.93981481D-07
      END MODULE PRMS_STRMTEMP

!***********************************************************************
!     Main stream temperature routine
!***********************************************************************
      INTEGER FUNCTION stream_temp()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: stream_temp_decl, stream_temp_init, stream_temp_run, stream_temp_setdims
      EXTERNAL :: stream_temp_restart
!***********************************************************************
      stream_temp = 0

      IF ( Process(:3)=='run' ) THEN
        stream_temp  = stream_temp_run()
      ELSEIF ( Process(:7)=='setdims' ) THEN
        stream_temp  = stream_temp_setdims()
      ELSEIF ( Process(:4)=='decl' ) THEN
        stream_temp  = stream_temp_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL stream_temp_restart(1)
        stream_temp  = stream_temp_init()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL stream_temp_restart(0)
      ENDIF

      END FUNCTION stream_temp
!***********************************************************************
!     stream_temp_setdims - declares module specific dimensions
!***********************************************************************
      INTEGER FUNCTION stream_temp_setdims()
      USE PRMS_MODULE, ONLY: MAXDIM
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix
      EXTERNAL read_error
!***********************************************************************
      stream_temp_setdims = 0

      IF ( decldim('width_dim', 1, MAXDIM, 'Number of stream-channel width intervals of segment outflow')/=0 ) &
     &     CALL read_error(7, 'width_dim')
      IF ( declfix('three', 3, 3, 'The constant 3')/=0 ) CALL read_error(7, 'three')

      END FUNCTION stream_temp_setdims

!***********************************************************************
!     stream_temp_decl - set up parameters and storage
!   Declared Parameters
!***********************************************************************
      INTEGER FUNCTION stream_temp_decl()
      USE PRMS_STRMTEMP
      USE PRMS_MODULE, ONLY: Nsegment, Init_vars_from_file, Nhru
      IMPLICIT NONE
! Functions
      INTRINSIC INDEX
      INTEGER, EXTERNAL :: declparam, declvar, getdim, control_integer
      EXTERNAL :: read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_stream_temp
!***********************************************************************
      stream_temp_decl = 0

      Version_stream_temp = 'stream_temp.f90 2016-09-21 14:02:00Z'
      CALL print_module(Version_stream_temp, 'Stream Temperature          ', 90)
      MODNAME = 'stream_temp'

      ! 0 = CBH File; 1 = specified constant; 2 = Stations
      IF ( control_integer(Strmtemp_humidity_flag, 'strmtemp_humidity_flag')/=0 ) Strmtemp_humidity_flag = 0
      ! 0 = compute shade; 1 = specified constant
      IF ( control_integer(Stream_temp_shade_flag, 'stream_temp_shade_flag')/=0 ) Stream_temp_shade_flag = 0

! Width_values holds flow-dependent data with dimension and interval in Q specified by Width_dim and Width_flow
      Width_dim = getdim('width_dim')
      IF ( Width_dim==-1 ) CALL read_error(6, 'width_dim')

! Declared Variables
      ALLOCATE ( Seg_width(Nsegment) )
      IF ( declvar( MODNAME, 'seg_width', 'nsegment', Nsegment, 'real', &
     &     'Width of each segment', &
     &     'meters', Seg_width)/=0 )  CALL read_error(3, 'seg_width')

      ALLOCATE ( Temp_avg(Nsegment) ) ! previous ??
      IF ( declvar( MODNAME, 'temp_avg', 'nsegment', Nsegment, 'real', &
     &     'Average stream temperature based on all inflows and previous temperature for each segment', &
     &     'degrees Celsius', Temp_avg)/=0 ) CALL read_error(3, 'temp_avg')

      ALLOCATE ( Upstrm_temp(Nsegment) )
      IF ( declvar( MODNAME, 'upstrm_temp', 'nsegment', Nsegment, 'real', &
     &     'Temperature of streamflow entering each segment', &
     &     'degrees Celsius', Upstrm_temp)/=0 )   CALL read_error(3,'upstrm_temp')

      ALLOCATE ( Seg_humid(Nsegment) )
      IF ( declvar( MODNAME, 'seg_humid', 'nsegment', Nsegment, 'real', &
     &     'Area-weighted average relative humidity for each segment from HRUs contributing flow to the segment', &
     &     'decimal fraction', Seg_humid)/=0 ) CALL read_error(3,'seg_humid')

      ALLOCATE ( Seg_maxtemp(Nsegment) )
      IF ( declvar( MODNAME, 'seg_maxtemp', 'nsegment', Nsegment, 'real', &
     &     'Area-weighted average maximum air temperature for each segment from HRUs contributing flow to the segment', &
     &     'degrees Celsius', Seg_maxtemp)/=0 ) CALL read_error(3, 'seg_maxtemp')

      ALLOCATE ( Seg_melt(Nsegment) )
      IF ( declvar( MODNAME, 'seg_melt', 'nsegment', Nsegment, 'real', &
     &     'Area-weighted average snowmelt for each segment from HRUs contributing flow to the segment', &
     &     'inches', Seg_melt)/=0 ) CALL read_error(3, 'seg_melt')

      ALLOCATE ( Seg_rain(Nsegment) )
      IF ( declvar( MODNAME, 'seg_rain', 'nsegment', Nsegment, 'real', &
     &     'Area-weighted average rainfall for each segment from HRUs contributing flow to the segment', &
     &     'inches', Seg_rain)/=0 ) CALL read_error(3, 'seg_rain')

      ALLOCATE ( T_roff(Nsegment) )
      IF ( declvar( MODNAME, 't_roff', 'nsegment', Nsegment, 'real', &
     &     'Area-weighted average air temperature for each segment from HRUs contributing flow to the segment', &
     &     'degrees Celsius', T_roff)/=0 ) CALL read_error(3, 't_roff')

      ALLOCATE ( Seg_potet(Nsegment) )
      IF ( declvar( MODNAME, 'seg_potet', 'nsegment', Nsegment, 'double', &
     &     'Hru area-weighted average potential ET for each segment', &
     &     'inches', Seg_potet)/=0 ) CALL read_error(3, 'seg_potet')

      ALLOCATE ( Seg_ccov(Nsegment) )
      IF ( declvar( MODNAME, 'seg_ccov', 'nsegment', Nsegment, 'real', &
     &     'Area-weighted average cloud cover fraction for each segment from HRUs contributing flow to the segment', &
     &     'decimal fraction', Seg_ccov )/=0 ) CALL read_error(3, 'seg_ccov')

      ALLOCATE ( Dlit(Nsegment) )
      IF ( declvar( MODNAME, 'dlit', 'nsegment', Nsegment, 'real', &
     &     'Hours of daylight', &
     &     'hours', Dlit)/=0 )   CALL read_error(3,'dlit')

      ALLOCATE ( Seg_elev(Nsegment), Press(Nsegment) ) !, Flowsum(Nsegment) )
      ALLOCATE ( T_ss(Nsegment), T_gw(Nsegment), T_ground(Nsegment) )
      ALLOCATE ( Tyr_avg(Nsegment), T30_avg(Nsegment), Seg_hru_num(Nsegment) )
      ALLOCATE ( Tyear(Nsegment, 365), T30(Nsegment, 300), Seg_carea_inv(Nsegment) )
      ALLOCATE ( Seg_close(Nsegment) )

      IF ( declparam( MODNAME, 'albedo', 'one', 'real', &
     &     '0.10', '0.0', '1.0', &
     &     'Short-wave solar radiation reflected by streams', &
     &     'Short-wave solar radiation reflected by streams', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'albedo')

      IF ( declparam( MODNAME, 'width_flow', 'three', 'real', &
     &     '50.0', '0.0', '220000.0', &
     &     'Minimum flow, maximum flow and flow interval for width values array', &
     &     'Minimum flow, maximum flow and flow interval for width values array', &
     &     'cms')/=0 ) CALL read_error(1, 'width_flow')

      ALLOCATE ( Width_values(Nsegment,Width_dim) )
      IF ( declparam( MODNAME, 'width_values', 'nsegment,width_dim', 'real', &
     &     '10.0', '0.0', '200.0', &
     &     'Width at each specified outflow interval', &
     &     'Width at each specified outflow interval for each segment', &
     &     'meters')/=0 ) CALL read_error(1, 'width_values')

      ALLOCATE ( Seg_length(Nsegment) )
      IF ( declparam( MODNAME, 'seg_length', 'nsegment', 'real', &
     &     '1000.0', '1.0', '100000.0', &
     &     'Length of each segment', &
     &     'Length of each segment', &
     &     'meters')/=0 ) CALL read_error(1, 'seg_length')

!      ALLOCATE ( Mann_n(Nsegment) )
!      IF ( declparam( MODNAME, 'Mann_n', 'nsegment', 'real', &
!     &     '0.04', '0.01', '0.15', &
!     &     'Mannings roughness coefficient', &
!     &     'Mannings roughness coefficient for each segment', &
!     &     'dimensionless')/=0 ) CALL read_error(1, 'Mann_n')

      ALLOCATE ( Seg_slope(Nsegment) )
      IF ( declparam( MODNAME, 'seg_slope', 'nsegment', 'real', &
     &     '0.015', '0.0001', '2.0', &
     &     'Bed slope of each segment', &
     &     'Bed slope of each segment', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'seg_slope')

      IF ( Stream_temp_shade_flag==0 ) THEN
        ALLOCATE ( Azrh(Nsegment) )
        IF ( declparam( MODNAME, 'azrh', 'nsegment', 'real', &
     &       '0.0', '-1.5708', '1.5708', &
     &       'Azimuth angle of each segment', &
     &       'Azimuth angle of each segment', &
     &       'radians')/=0 ) CALL read_error(1, 'azrh')

        ALLOCATE ( Alte(Nsegment) )
        IF ( declparam( MODNAME, 'alte', 'nsegment', 'real', &
     &       '0.0', '0.0','1.57079633', &
     &       'East bank topographic altitude', &
     &       'East bank topographic altitude of each segment', &
     &       'radians')/=0 ) CALL read_error(1, 'alte')

        ALLOCATE ( Altw(Nsegment) )
        IF ( declparam( MODNAME, 'altw', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.57079633', &
     &       'West bank topographic altitude', &
     &       'West bank topographic altitude of each segment', &
     &       'radians')/=0 ) CALL read_error(1, 'altw')

        ALLOCATE ( Vce(Nsegment) )
        IF ( declparam( MODNAME, 'vce', 'nsegment', 'real', &
     &       '0.0', '0.0', '15.0', &
     &       'East bank average vegetation crown width', &
     &       'East bank average vegetation crown width for each segment', &
     &       'meters')/=0 ) CALL read_error(1, 'vce')

        ALLOCATE ( Vdemx(Nsegment) )
        IF ( declparam( MODNAME, 'vdemx', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Maximum east bank vegetation density', &
     &       'Maximum east bank vegetation density for each segment', & 
     &       'decimal fraction')/=0 )  CALL read_error(1, 'vdemx')

        ALLOCATE ( Vdemn(Nsegment) )
        IF ( declparam( MODNAME, 'vdemn', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Minimum east bank vegetation density', &
     &       'Minimum east bank vegetation density for each segment', & 
     &       'decimal fraction')/=0 )  CALL read_error(1, 'vdemn')

        ALLOCATE ( Vhe(Nsegment) )
        IF ( declparam( MODNAME, 'vhe', 'nsegment', 'real', &
     &       '0.0', '0.0', '30.0', &
     &       'East bank vegetation height', &
     &       'East bank average vegetation height for each segment', &
     &       'meters')/=0 ) CALL read_error(1, 'vhe')

        ALLOCATE ( Voe(Nsegment) )
        IF ( declparam( MODNAME, 'voe', 'nsegment', 'real', &
     &       '0.0', '0.0', '100.0',&
     &       'East bank vegetation offset', &
     &       'East bank vegetation offset for each segment', &
     &       'meters')/=0 ) CALL read_error(1, 'voe')

        ALLOCATE ( Vcw(Nsegment) )
        IF ( declparam( MODNAME, 'vcw', 'nsegment', 'real', &
     &       '0.0', '0.0', '15.0', &
     &       'West bank vegetation crown width', &
     &       'West bank average vegetation crown width for each segment', &
     &       'meters')/=0 ) CALL read_error(1, 'vcw')

        ALLOCATE ( Vdwmx(Nsegment) )
        IF ( declparam( MODNAME, 'vdwmx', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Maximum west bank vegetation density', &
     &       'Maximum west bank vegetation density for each segment', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'vdwmx')

        ALLOCATE ( Vdwmn(Nsegment) )
        IF ( declparam( MODNAME, 'vdwmn', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Minimum west bank vegetation density', &
     &       'Minimum west bank vegetation density for each segment', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'vdwmn')

        ALLOCATE ( Vhw(Nsegment) )
        IF ( declparam( MODNAME, 'vhw', 'nsegment', 'real', &
     &       '0.0', '0.0', '30.0', &
     &       'West bank vegetation height', &
     &       'West bank average vegetation height for each segment', &
     &       'meters')/=0 ) CALL read_error(1, 'vhw')

        ALLOCATE ( Vow(Nsegment) )
        IF ( declparam( MODNAME, 'vow', 'nsegment', 'real', &
     &       '0.0', '0.0', '100.0', &
     &       'West bank vegetation offset', &
     &       'West bank vegetation offset for each segment', &
     &       'meters')/=0 ) CALL read_error(1, 'vow')
      ENDIF

      IF ( Init_vars_from_file==0 ) THEN
        ALLOCATE ( Gw_init(Nsegment) )
        IF ( declparam( MODNAME, 'gw_init', 'nsegment', 'real', &
     &       '1.0', '0.0', '45.', &
     &       'Initial groundwater temperature', &
     &       'Initial temperature of groundwater entering each segment;'// &
     &       ' recommended value equal to long-term average air temperature', &
     &       'degrees Celsius')/=0 ) CALL read_error(1, 'gw_init')

        ALLOCATE ( Ss_init(Nsegment) )
        IF ( declparam( MODNAME, 'ss_init', 'nsegment', 'real', &
     &       '15.0', '0.0', '45.0', &
     &       'Initial interflow temperature', &
     &       'Initial temperature of interflow entering each segment;'// &
     &       ' recommended value equal to long-term average air temperature', &
     &       'degrees Celsius')/=0 ) CALL read_error(1, 'ss_init')
      ENDIF

      IF ( Stream_temp_shade_flag==1 ) THEN
        ALLOCATE ( Segshade_sum(Nsegment) )
        IF ( declparam( MODNAME, 'segshade_sum', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0.', &
     &       'Total shade fraction for summer vegetation', &
     &       'Total shade fraction for summer vegetation; required when stream_temp_flag=1', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'segshade_sum')

        ALLOCATE ( Segshade_win(Nsegment) )
        IF ( declparam( MODNAME, 'segshade_win', 'nsegment', 'real', &
     &       '0.0', '0.0', '1.0.', &
     &       'Total shade fraction for winter vegetation', &
     &       'Total shade fraction for winter vegetation; required when stream_temp_flag=1', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'segshade_win')
      ENDIF

      IF ( declparam( MODNAME, 'ss_tau', 'one', 'integer', &
     &     '30', '1', '365', &
     &     'Average residence time of subsurface interflow', &
     &     'Average residence time of subsurface interflow', &
     &     'days')/=0 ) CALL read_error(1, 'ss_tau')

      IF ( declparam( MODNAME, 'gw_tau', 'one', 'integer', &
     &     '365', '1', '365', &
     &     'Average residence time in groundwater flow', &
     &     'Average residence time in subsurface flow', &
     &     'days')/=0 ) CALL read_error(1, 'gw_tau')
      
      IF ( declparam( MODNAME, 'melt_temp', 'one', 'real', &
     &     '1.5', '0.0', '10.0', &
     &     'Temperature at which snowmelt enters a stream', &
     &     'Temperature at which snowmelt enters a stream', &
     &     'degrees Celsius')/=0 ) CALL read_error(1, 'melt_temp')

      IF ( declparam( MODNAME, 'maxiter_sntemp', 'one', 'integer', &
     &     '1000', '10', '2000', &
     &     'Maximum number of Newton-Raphson iterations to compute stream temperature', &
     &     'Maximum number of Newton-Raphson iterations to compute stream temperature', &
     &     'none')/=0 ) CALL read_error(1, 'maxiter_sntemp')

      IF ( Strmtemp_humidity_flag==1 ) THEN  ! specified constant
        ALLOCATE ( Seg_winter_humidity(Nsegment) )
        IF ( declparam( MODNAME, 'seg_winter_humidity', 'nsegment', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Winter humidity for each segment', &
     &       'Winter humidity for each segment, used when values not input in CBH File', &
     &       'decimal fraction')/=0 )  CALL read_error(1, 'seg_winter_humidity')
        ALLOCATE ( Seg_summer_humidity(Nsegment) )
        IF ( declparam( MODNAME, 'seg_summer_humidity', 'nsegment', 'real', &
     &       '0.7', '0.0', '1.0', &
     &       'Summer humidity for each segment', &
     &       'Summer humidity for each segment, used when values not input in CBH File', &
     &       'decimal fraction')/=0 )  CALL read_error(1, 'seg_summer_humidity')
      ELSEIF ( Strmtemp_humidity_flag==2 ) THEN  ! use station data
        ALLOCATE ( Seg_humidity_sta(Nsegment) )
        IF ( declparam(MODNAME, 'seg_humidity_sta', 'nsegment', 'integer', &
     &       '0', 'bounded', 'nhumid', &
     &       'Index of humidity measurement station for each stream segment', &
     &       'Index of humidity measurement station for each stream segment', &
     &       'none')/=0 ) CALL read_error(1, 'seg_humidity_sta')
      ENDIF

      ALLOCATE ( Ccov_slope(Nhru,12) )
      IF ( declparam(MODNAME, 'ccov_slope', 'nhru,nmonths', 'real', &
     &     '-0.13', '-0.5', '-0.01', &
     &     'Slope in temperature cloud cover relationship', &
     &     'Monthly (January to December) coefficient in cloud-cover relationship', &
     &     'none')/=0 ) CALL read_error(1, 'ccov_slope')

      ALLOCATE ( Ccov_intcp(Nhru,12) )
      IF ( declparam(MODNAME, 'ccov_intcp', 'nhru,nmonths', 'real', &
     &     '1.83', '0.0', '5.0', &
     &     'Intercept in temperature cloud cover relationship', &
     &     'Monthly (January to December) intercept in cloud-cover relationship', &
     &     'none')/=0 ) CALL read_error(1, 'ccov_intcp')

      END FUNCTION stream_temp_decl

!***********************************************************************
!    stream_temp_init - Initialize module - get parameter values
!***********************************************************************
      INTEGER FUNCTION stream_temp_init()
      USE PRMS_STRMTEMP
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Init_vars_from_file, Inputerror_flag
      USE PRMS_BASIN, ONLY: Hru_elev_meters, Hru_lat, Hru_area, Active_hrus, Hru_route_order, NEARZERO
      USE PRMS_OBS, ONLY: Nhumid
      USE PRMS_ROUTING, ONLY: Hru_segment, Tosegment, Segment_hruarea, Segment_order, Segment_up
      IMPLICIT NONE
! Functions
      INTRINSIC :: COS, SIN, ABS, SIGN, ASIN
      INTEGER, EXTERNAL :: getparam
      REAL, EXTERNAL :: solalt
      EXTERNAL :: read_error, checkdim_param_limits
! Local Variables
      INTEGER :: i, j, k, iseg, ierr, ii
      REAL :: tan_d, tano, sinhro, temp, decl, cos_d, tanod, alrs
!***********************************************************************
      stream_temp_init = 0

      IF ( getparam( MODNAME, 'width_flow', 3, 'real', Width_flow)/=0 ) CALL read_error(2, 'width_flow')
      IF ( Width_flow(3)==0.0 ) THEN
        Width_flow(3) = 1.0
        PRINT *, 'WARRNING, third value of width_flow specified = 0, set to 1.0'
      ENDIF ! rsr, width_flow(3) can't be 0, or divide by 0

      IF ( getparam( MODNAME, 'width_values', Nsegment*width_dim, 'real', Width_values)/= 0 ) CALL read_error(2, 'width_values')
      IF ( getparam( MODNAME, 'albedo', 1, 'real', Albedo)/=0 ) CALL read_error(2, 'albedo')
      IF ( getparam( MODNAME, 'seg_length', Nsegment, 'real', Seg_length)/=0 ) CALL read_error(2, 'seg_length')
!      IF ( getparam( MODNAME, 'Mann_n', Nsegment, 'real', Mann_n)/=0 ) CALL read_error(2, 'Mann_n')
      IF ( getparam( MODNAME, 'seg_slope', Nsegment, 'real', Seg_slope)/=0 ) CALL read_error(2, 'seg_slope')
      IF ( getparam( MODNAME, 'azrh', Nsegment, 'real', Azrh)/=0 ) CALL read_error(2, 'azrh')
      IF ( getparam( MODNAME, 'alte', Nsegment, 'real', Alte)/=0 ) CALL read_error(2, 'alte')
      IF ( getparam( MODNAME, 'altw', Nsegment, 'real', Altw)/=0 ) CALL read_error(2, 'altw')
      IF ( getparam( MODNAME, 'vce', Nsegment, 'real', Vce)/=0 ) CALL read_error(2, 'vce')
      IF ( getparam( MODNAME, 'vdemx', Nsegment, 'real', Vdemx)/=0 ) CALL read_error(2, 'vdemx')
      IF ( getparam( MODNAME, 'vdemn', Nsegment, 'real', Vdemn)/=0 ) CALL read_error(2, 'vdemn')
      IF ( getparam( MODNAME, 'vhe', Nsegment, 'real', Vhe)/=0 ) CALL read_error(2, 'vhe')
      IF ( getparam( MODNAME, 'voe', Nsegment, 'real', Voe)/=0 ) CALL read_error(2, 'voe')
      IF ( getparam( MODNAME, 'vcw', Nsegment, 'real', Vcw)/=0 ) CALL read_error(2, 'vcw')
      IF ( getparam( MODNAME, 'vdwmx', Nsegment, 'real', Vdwmx)/=0 ) CALL read_error(2, 'vdwmx')
      IF ( getparam( MODNAME, 'vdwmn', Nsegment, 'real', Vdwmn)/=0 ) CALL read_error(2, 'vdwmn')
      IF ( getparam( MODNAME, 'vhw', Nsegment, 'real', Vhw)/=0 ) CALL read_error(2, 'vhw')
      IF ( getparam( MODNAME, 'vow', Nsegment, 'real', Vow)/=0 ) CALL read_error(2, 'vow')
      IF ( getparam( MODNAME, 'gw_init', Nsegment, 'real', Gw_init)/=0 ) CALL read_error(2, 'gw_init')
      IF ( getparam( MODNAME, 'ss_init', Nsegment, 'real', Ss_init)/=0 ) CALL read_error(2, 'ss_init')
      IF ( Stream_temp_shade_flag==1 ) THEN
        IF ( getparam( MODNAME, 'segshade_sum', Nsegment, 'real', Segshade_sum)/=0 ) CALL read_error(2, 'segshade_sum')
        IF ( getparam( MODNAME, 'segshade_win', Nsegment, 'real', Segshade_win)/=0 ) CALL read_error(2, 'segshade_win')
      ENDIF
      IF ( getparam( MODNAME, 'ss_tau', 1, 'integer', Ss_tau)/=0 ) CALL read_error(2, 'ss_tau')
      IF ( getparam( MODNAME, 'gw_tau', 1, 'integer', Gw_tau)/=0 ) CALL read_error(2, 'Gw_tau')
      ierr = 0
      IF ( getparam( MODNAME, 'melt_temp', 1, 'real', Melt_temp)/=0 ) CALL read_error(2, 'melt_temp')
      IF ( getparam( MODNAME, 'maxiter_sntemp', 1, 'real', Maxiter_sntemp)/=0 ) CALL read_error(2, 'maxiter_sntemp')
      IF ( Strmtemp_humidity_flag==1 ) THEN
        IF ( getparam( MODNAME, 'seg_summer_humidity', Nsegment, 'real', Seg_summer_humidity)/=0 ) &
     &       CALL read_error(2, 'seg_summer_humidity')
        IF ( getparam( MODNAME, 'seg_winter_humidity', Nsegment, 'real', Seg_winter_humidity)/=0 ) &
     &       CALL read_error(2, 'seg_winter_humidity')
      ELSEIF ( Strmtemp_humidity_flag==2 ) THEN ! use station data
        IF ( getparam(MODNAME, 'seg_humidity_sta', Nsegment, 'integer', Seg_humidity_sta)/=0 ) &
     &       CALL read_error(2, 'seg_humidity_sta')
        DO i = 1, Nsegment
          CALL checkdim_param_limits(i, 'seg_humidity_sta', 'nhumid', Seg_humidity_sta(i), 1, Nhumid, ierr)
        ENDDO
      ENDIF
      IF ( getparam(MODNAME, 'ccov_slope', Nhru*12, 'real', Ccov_slope)/=0 ) CALL read_error(2, 'ccov_slope')
      IF ( getparam(MODNAME, 'ccov_intcp', Nhru*12, 'real', Ccov_intcp)/=0 ) CALL read_error(2, 'ccov_intcp')

      IF ( Init_vars_from_file == 0 ) THEN
! Initialize declared variables
        Temp_avg = 0.0
        Upstrm_temp = 0.0
        Seg_potet = 0.0D0
        Seg_maxtemp = 0.0
        Seg_humid = 0.0
        Seg_width = 0.0
        Seg_ccov = 0.0
        DO i = 1, Nsegment
          DO j = 1, 365
            Tyear(i,j) =  Gw_init(i)
          ENDDO
          DO k = 1, 30
            T30(i,k) = Ss_init(i)
          ENDDO
        ENDDO
        T_roff = 0.0
        T_ground = 0.0
      ENDIF

      IF ( Stream_temp_shade_flag==0 ) THEN
        ALLOCATE ( Cos_seg_lat(Nsegment), Sin_seg_lat(Nsegment), Horizontal_hour_angle(366,Nsegment) )
        ALLOCATE ( Total_shade(366,Nsegment), Sin_declination(366,Nsegment), Sin_alrs(366,Nsegment) )
        ALLOCATE ( Sin_lat_decl(366,Nsegment), Cos_lat_decl(366,Nsegment) )
        ALLOCATE ( Max_solar_altitude(366,Nsegment), Level_sunset_azimuth(366,Nsegment) )
        ALLOCATE ( Local_sunset_hour_angle(366,Nsegment), Local_sunrise_hour_angle(366,Nsegment) )
        ALLOCATE ( Shade_jday(Nsegment, 366), Svi_jday(Nsegment, 366), Seg_lat(Nsegment) )
        Shade_jday = 0.0
        Svi_jday = 0.0
        Seg_lat = 0.0
        Dlit = 12.0
      ENDIF

      Seg_elev = 30000.0
      Seg_carea_inv = 0.0
      Seg_hru_num = 0
      DO k = 1, Active_hrus
        j = Hru_route_order(k)
        i = Hru_segment(j)
        IF ( i==0 ) CYCLE
        Seg_hru_num(i) = Seg_hru_num(i) + 1
        IF ( Stream_temp_shade_flag==0 ) Seg_lat(i) = Seg_lat(i) + Hru_lat(j)*Hru_area(j)
        IF ( Seg_elev(i)>Hru_elev_meters(j) ) Seg_elev(i) = Hru_elev_meters(j)
      ENDDO
      DO i = 1, Nsegment ! set values for all segments with HRUs so next loop has maximum possible values
        IF ( Seg_hru_num(i)>0 ) THEN
          Seg_carea_inv(i) = 1.0/Segment_hruarea(i)
          IF ( Stream_temp_shade_flag==0 ) Seg_lat(i) = Seg_lat(i)*Seg_carea_inv(i) * DEG_TO_RAD
        ENDIF
        IF ( Seg_length(i)<NEARZERO ) THEN
          PRINT *, 'ERROR, seg_length too small for segment:', i, ', value:', Seg_length(i)
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) THEN
        Inputerror_flag = ierr
        RETURN
      ENDIF
      Seg_close = Segment_up ! assign upstream values
      DO j = 1, Nsegment ! set values based on routing order for segments without associated HRUs
        i = Segment_order(j)
        IF ( Seg_hru_num(i)==0 ) THEN
          IF ( Segment_up(i)==0 ) THEN 
            IF ( Tosegment(i)>0 ) THEN ! assign downstream values
              Seg_close(i) = Tosegment(i) ! don't have a value yet, need to fix
            ELSE ! no upstream or downstream segment
              IF ( j>1 ) THEN
                Seg_close(i) = Segment_order(j-1) ! set to previous segment id
              ELSE
                Seg_close(i) = Segment_order(j+1) ! assume at least 2 segments
              ENDIF
            ENDIF
          ENDIF
          IF ( Seg_elev(Seg_close(i))==30000.0 ) THEN ! need different segment
            iseg = -1
            DO k = j+1, Nsegment ! find first segment with valid values
              ii = Segment_order(k)
              IF ( Seg_hru_num(ii)>0 ) THEN
                Seg_close(i) = ii
                EXIT
              ENDIF
            ENDDO
            IF ( iseg==-1 ) THEN
              IF ( j>1 ) THEN
                Seg_close(i) = Segment_order(j-1) ! set to previous segment id
              ELSE ! this is a problem, shouldn't happen
                STOP 'ERROR, segments do not have associated HRUs'
                ! Seg_close(i) = Segment_order(1) ! set to first segment id
              ENDIF
            ENDIF
          ENDIF
          IF ( Stream_temp_shade_flag==0 ) Seg_lat(i) = Seg_lat(Seg_close(i))
          Seg_elev(i) = Seg_elev(Seg_close(i))
        ENDIF
        Press(i) = 1013.0 - (0.1055 * Seg_elev(i))
        IF ( Stream_temp_shade_flag==0 ) THEN
!  LATITUDE TRIGONOMETRIC PARAMETERS 
          Cos_seg_lat(i) = COS(Seg_lat(i)) ! coso
          IF ( Cos_seg_lat(i) < NEARZERO ) Cos_Seg_lat(i) = NEARZERO
          Sin_seg_lat(i) = SIN(Seg_lat(i)) ! sino
          tano = Sin_seg_lat(i) / Cos_seg_lat(i)
          DO k = 1, 366
!  DECLINATION TRIGONOMETRIC PARAMETERS
            decl = 0.40928 * COS(((2.0 * PI) / 365.25) * (172.0 - k))
            cos_d = COS(decl)
            Sin_declination(k, i) = SIN(decl) ! sin_d
            IF ( cos_d < NEARZERO ) cos_d = NEARZERO
            tan_d = Sin_declination(k, i) / cos_d
!
!  JOINT LATITUDE & DECLINATION TRIGONOMETRIC PARAMETERS
            Cos_lat_decl(k, i) = Cos_seg_lat(i) * cos_d ! cosod
            Sin_lat_decl(k, i) = Sin_seg_lat(i) * Sin_declination(k, i) ! sinod
            tanod = tano * tan_d
            IF ( ABS(tanod) > 1.0 ) tanod = SIGN(1.0,tanod)

!  LEVEL-PLAIN SUNRISE/SET HOUR ANGLE
            Horizontal_hour_angle(k, i) = ACOS(-tanod) ! hrso
            sinhro = SIN(Horizontal_hour_angle(k, i))
!
!  LEVEL-PLAIN SOLAR AZIMUTH
            temp = -Sin_declination(k, i)/Cos_Seg_lat(i)
            IF ( ABS(temp) > 1.0 ) temp = SIGN(1.0,temp)
            Level_sunset_azimuth(k, i) = ACOS(temp) ! azso
!
!  MAXIMUM POSSIBLE SOLAR ALTITUDE
            Max_solar_altitude(k, i) = ASIN( Sin_lat_decl(k,i) + Cos_lat_decl(k,i) ) ! alsmx
!
!  TOTAL POTENTIAL SHADE ON LEVEL-PLAIN ! totsh
            Total_shade(k, i) = 2.0 * ((Horizontal_hour_angle(k, i) * Sin_lat_decl(k, i)) + (sinhro * Cos_lat_decl(k, i)))
            IF ( Total_shade(k, i) < NEARZERO ) Total_shade(k, i) = NEARZERO
!
!  CHECK FOR REACH AZIMUTH LESS THAN SUNRISE
            IF ( Azrh(i) <= (-Level_sunset_azimuth(k, i)) ) THEN
              alrs = 0.0
!
!  CHECK FOR REACH AZIMUTH GREATER THAN SUNSET
            ELSEIF ( Azrh(i) >= Level_sunset_azimuth(k, i) ) THEN
              alrs = 0.0
!
!  REACH AZIMUTH IS BETWEEN SUNRISE & SUNSET
            ELSEIF ( Azrh(i) == 0.0 ) THEN
              alrs = Max_Solar_altitude(k, i)
            ELSE
              alrs = solalt(Cos_seg_lat(i), Sin_seg_lat(i), Sin_declination(k,i), Azrh(i), 0.0, Max_Solar_altitude(k,i))
              Sin_alrs(k, i) = SIN(alrs)
!
!  END REACH & SOLAR AZIMUTH CHECK
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!      IF ( Stream_temp_shade_flag==0 ) CALL shday()

      END FUNCTION stream_temp_init

!***********************************************************************
!     stream_temp_run - Computes stream temperatures
!***********************************************************************
      INTEGER FUNCTION stream_temp_run()
      USE PRMS_STRMTEMP
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, NEARZERO
      USE PRMS_SET_TIME, ONLY: Summer_flag, Nowmonth
      USE PRMS_CLIMATEVARS, ONLY: Tavgc, Tmaxc, Potet, Hru_rain, Basin_cloud_cover, Cloud_cover_hru, Tmax_hru, Tmin_hru
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      USE PRMS_CLIMATE_HRU, ONLY: Humidity_hru
      USE PRMS_SNOW, ONLY: Snowmelt
      USE PRMS_ROUTING, ONLY: Hru_segment, Tosegment, Segment_order
      USE PRMS_OBS, ONLY: Humidity
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE
      REAL, EXTERNAL :: twavg, twmax, get_segwidth
      EXTERNAL :: equilb, lat_inflow, shday
! Local Variables
      REAL :: harea, carea, svi, flowsum
      INTEGER :: i, j, k, yday, mday, xday, gday, toseg, iseg
      REAL :: tlat, te, ak1, ak2, shade, ccov
      DOUBLE PRECISION :: qlat
!***********************************************************************
      stream_temp_run = 0
      T_roff = 0.0
      Seg_maxtemp = 0.0
      T_ground = 0.0
      IF ( Strmtemp_humidity_flag==1 ) THEN
        IF ( Summer_flag==0 ) THEN
          Seg_humid = Seg_winter_humidity
        ELSE
          Seg_humid = Seg_summer_humidity
        ENDIF
      ELSEIF ( Strmtemp_humidity_flag==2 ) THEN ! use station data
        DO i = 1, Nsegment
          Seg_humid(i) = Humidity(Seg_humidity_sta(i))
        ENDDO
      ELSE
        Seg_humid = 0.0
      ENDIF
      Seg_potet = 0.0D0
      Seg_ccov = 0.0
      Seg_melt = 0.0
      Seg_rain = 0.0
      Basin_cloud_cover = 0.0D0
      Flowsum = 0.0
      ! Compute segment lateral inflow temperatures and segment meteorological values
      DO k = 1, Active_hrus
        j = Hru_route_order(k)
        ccov = Ccov_slope(j, Nowmonth)*(Tmax_hru(j)-Tmin_hru(j)) + Ccov_intcp(j, Nowmonth)
        IF ( ccov<NEARZERO ) THEN
          ccov = 0.0
        ELSEIF ( ccov>1.0 ) THEN
          ccov = 1.0
        ENDIF
        Cloud_cover_hru(j) = ccov
        harea = Hru_area(j)
        Basin_cloud_cover = Basin_cloud_cover + DBLE( ccov*harea )
        i = Hru_segment(j)
        IF ( i==0 ) CYCLE
        T_roff(i) = T_roff(i) + Tavgc(j)*harea
        Seg_maxtemp(i) = Seg_maxtemp(i) + Tmaxc(j)*harea
        IF ( Strmtemp_humidity_flag==0 ) Seg_humid(i) = Seg_humid(i) + Humidity_hru(j)*harea
        Seg_ccov(i) = Seg_ccov(i) + Cloud_cover_hru(j)*harea
        Seg_potet(i) = Seg_potet(i) + DBLE( Potet(j)*harea )
        Seg_melt(i) = Seg_melt(i) + Snowmelt(j)*harea
        Seg_rain(i) = Seg_rain(i) + Hru_rain(j)*harea 
      ENDDO

! Segment loop to find and assign temperatures.
      Upstrm_temp = 0.0
      DO j = 1, Nsegment
        i = Segment_order(j)
        toseg = Tosegment(i)
        IF ( Seg_hru_num(i)>0 ) THEN
          carea = Seg_carea_inv(i)
          Seg_maxtemp(i) = Seg_maxtemp(i)*carea
          Seg_ccov(i) = Seg_ccov(i)*carea
          Seg_potet(i) = Seg_potet(i)*DBLE(carea)
          T_roff(i) = T_roff(i)*carea 
          Seg_melt(i) = Seg_melt(i)*carea
          Seg_rain(i) = Seg_rain(i)*carea
          IF ( Strmtemp_humidity_flag==0 ) Seg_humid(i) = Seg_humid(i)*carea 
        ELSE
          iseg = Seg_close(i) ! doesn't work if no and upstream segment
          T_roff(i) = T_roff(iseg)
          Seg_maxtemp(i) = Seg_maxtemp(iseg)
          Seg_ccov(i) = Seg_ccov(iseg)
          Seg_potet(i) = Seg_potet(iseg)
          Seg_melt(i) = Seg_melt(iseg)
          Seg_rain(i) = Seg_rain(iseg)
          IF ( Strmtemp_humidity_flag==0 ) Seg_humid(i) = Seg_humid(iseg)*Seg_carea_inv(iseg) ! ??
        ENDIF
! Store temps in arrays over residence times
        DO yday = 1, (Gw_tau - 1)
          Tyear(i, yday) = Tyear(i, yday + 1)
        ENDDO
        Tyear(i, Gw_tau) = T_roff(i)
        IF (Gw_tau < 365) THEN
          DO  gday = (Gw_tau + 1), 365
            Tyear (i, gday) = 0.0
          ENDDO
        ENDIF
        DO mday = 1, (Ss_tau - 1)
          T30 (i, mday) = T30 (i, mday + 1)
        ENDDO
        T30(i, Ss_tau) = T_roff(i)
        IF (Ss_tau < 300) THEN
          DO  xday = (Ss_tau + 1), 300
            T30 (i, xday) = 0.0
          ENDDO
        ENDIF
        Tyr_avg = (SUM ( Tyear, DIM = 2 ))/Gw_tau
        T30_avg = (SUM ( T30 , DIM = 2 ))/Ss_tau
        T_gw(i) = Tyr_avg(i)
        T_ss(i) = T30_avg(i)
        IF ( T_gw(i) < NEARZERO ) T_gw = NEARZERO
        IF ( T_ss(i) < NEARZERO ) T_ss = NEARZERO
! Find upstream intitial inflow temperature for segment i
        Upstrm_temp(i) = 0.0
        flowsum = 0
        DO k = 1, Nsegment
          IF ( Tosegment(k)==i ) THEN
            Upstrm_temp(i) = Upstrm_temp(i) + (Temp_avg(k) * Seg_outflow(i))
            flowsum = flowsum + Seg_outflow(i)
          ENDIF
        ENDDO
        IF ( Upstrm_temp(i) == 0.0 ) THEN
          Upstrm_temp(i) = Temp_avg(i)
          IF ( Upstrm_temp(i) <= 0 ) Upstrm_temp(i) = T_roff(i)
        ELSEIF ( flowsum > 0 ) THEN
          Upstrm_temp(i) = Upstrm_temp(i) / flowsum
        ELSE
          Upstrm_temp(i) = T_roff(i)
        ENDIF
        IF ( Stream_temp_shade_flag==1 ) THEN
          IF ( Summer_flag==0 ) THEN
            shade = Segshade_win(i)
          ELSE
            shade = Segshade_sum(i)
          ENDIF
!          svi = shade
           svi = 0.0
        ELSE
!          shade = Shade_jday(i, Jday)
!          svi = Svi_jday(i, Jday)
!          Seg_width(i) = get_segwidth(i)
          CALL shday(i, shade, svi)
        ENDIF
        qlat = 0.0D0
        tlat = 0.0
        te = 0.0
        ak1 = 0.0 
        ak2 = 0.0
        !rsr, ?? why if = 0?, need to verify this whole section
        te = Upstrm_temp(i)
!        IF ( te == 0.0 ) THEN
!          te = Temp_avg(i)
!        ELSEIF ( Seg_upstream_inflow(i) > 0.0 ) THEN
!          te = te/Seg_upstream_inflow(i)
!        ELSE
!          te = T_roff(i)
!        ENDIF
!        IF ( te < 0.0 ) te = 0.0
!        Upstrm_temp(i) = te
        CALL lat_inflow(qlat, tlat, i, T_ground(i), T_gw(i), T_roff(i), T_ss(i), Upstrm_temp(i), &
     &                  Seg_melt(i), Seg_rain(i))
        CALL equilb(te, ak1, ak2, shade, svi, i)
        Temp_avg(i) = twavg(Upstrm_temp(i), qlat, tlat, te, ak1, ak2, i, Seg_width(i), Seg_length(i))

! need temp_avg and t_roff ??
!        IF ( Upstrm_temp(i) == 0.0 ) THEN
!          Upstrm_temp(i) = Temp_avg(i)
!          IF ( Upstrm_temp(i) <= 0 ) Upstrm_temp(i) = T_roff(i)
!        ELSEIF ( Flowsum(i) > 0.0 ) THEN
!          Upstrm_temp(i) = Upstrm_temp(i) / Flowsum(i)
!        ELSE
!          Upstrm_temp(i) = T_roff(i)
!        ENDIF

!        IF ( toseg>0 ) Upstrm_temp(toseg) = Upstrm_temp(toseg) + Temp_avg(i)*Seg_outflow(i)
! Find upstream intitial inflow temperature for segment i
!        flowsum = 0
!        DO k = 1, Nsegment
!          IF ( Tosegment(k)==i ) THEN
!            Upstrm_temp(i) = Upstrm_temp(i) + (Temp_avg(k) * Seg_outflow(i))
!            flowsum = flowsum + Seg_outflow(i)
!          ENDIF
!        ENDDO

      ENDDO

      END FUNCTION stream_temp_run
!
!*********************************************************************************
! Compute the flow-weighted average temperature and a total sum of lateral inflows
!*********************************************************************************
      SUBROUTINE lat_inflow(Qlat, Tl_avg, Seg_id, T_ground, T_gw, T_roff, T_ss, Upstrm_temp, Seg_melt, Seg_rain)
      USE PRMS_STRMTEMP, ONLY: Melt_temp
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV
      USE PRMS_FLOWVARS, ONLY: Seg_lateral_inflow
      USE PRMS_ROUTING, ONLY: Seginc_sroff, Seginc_ssflow, Seginc_gwflow
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL
! Arguments
      INTEGER, INTENT(IN) :: Seg_id
      REAL, INTENT(IN) :: T_gw, T_roff, T_ss, Upstrm_temp, Seg_melt, Seg_rain
      REAL, INTENT(OUT) :: Tl_avg, T_ground
      DOUBLE PRECISION, INTENT(OUT) :: Qlat
! Local Variables
      REAL :: weight_roff, weight_ss, weight_gw, melt_wt, rain_wt, troff, tss
!*****************************************************************************
      IF ( Seginc_gwflow(Seg_id)>0.0 ) THEN
        T_ground = T_gw
      ELSE
        T_ground = Upstrm_temp
      ENDIF
      Qlat = Seg_lateral_inflow(Seg_id) * CFS2CMS_CONV
      Tl_avg = 0.0
      IF ( Qlat>0.0D0 ) THEN ! weights do not include water-use if active, not sure it works for cascades
        weight_roff = SNGL( (Seginc_sroff(Seg_id) / Qlat) * CFS2CMS_CONV )
        weight_ss = SNGL( (Seginc_ssflow(Seg_id) / Qlat) * CFS2CMS_CONV )
        weight_gw = SNGL( (Seginc_gwflow(Seg_id) / Qlat) * CFS2CMS_CONV )
      ELSE
        weight_roff = 0.0
        weight_ss = 0.0
        weight_gw = 0.0
      ENDIF
      IF (Seg_melt > 0.0) THEN
        melt_wt = Seg_melt/(Seg_melt + Seg_rain)
        IF (melt_wt < 0.0) melt_wt = 0.0
        IF (melt_wt > 1.0) melt_wt = 1.0
        rain_wt = 1.0 - melt_wt
        IF (Seg_rain == 0.0) THEN
          troff = Melt_temp
          tss = Melt_temp
        ELSE
          troff = Melt_temp * melt_wt + T_roff * rain_wt
          tss = Melt_temp * melt_wt + T_ss * rain_wt
        ENDIF
      ELSE
        troff = T_roff
        tss = T_ss
      ENDIF
          
      Tl_avg = weight_roff * troff + weight_ss * tss + weight_gw * T_ground

      END SUBROUTINE lat_inflow

!***********************************************************************************************
      REAL FUNCTION twavg(T0, Qlat, Tl_avg, Te, Ak1, Ak2, Seg_id, Seg_width, Seg_length)
!
!     PURPOSE:
!        1. TO PREDICT THE AVERAGE DAILY WATER TEMPERATURE USING A SECOND-ORDER
!           CLOSED-FORM SOLUTION TO THE STEADY-STATE HEAT TRANSPORT EQUATION.
      USE PRMS_BASIN, ONLY: NEARZERO, CFS2CMS_CONV
      USE PRMS_FLOWVARS, ONLY: Seg_upstream_inflow
      IMPLICIT NONE
! Functions
      INTRINSIC ABS, EXP, ALOG, SNGL, SIGN
! Arguments
      INTEGER, INTENT(IN) :: Seg_id
      REAL, INTENT(IN) :: T0, Tl_avg, Te, Ak1, Ak2, Seg_width, Seg_length
      DOUBLE PRECISION, INTENT(IN) :: Qlat
! Local Variables
      REAL :: tep, b, r, rexp, tw, delt, q_init, denom, Ql
!***************************************************************************************************
! DETERMINE EQUATION PARAMETERS
      q_init = SNGL( Seg_upstream_inflow(Seg_id) * CFS2CMS_CONV )
      IF ( q_init < NEARZERO ) q_init = NEARZERO ! rsr, don't know what value this should be to avoid divide by 0
      Ql = SNGL( Qlat )
      b = (Ql / Seg_length) + ((Ak1 * Seg_width) / 4182.0E03)
      IF ( b < NEARZERO ) b = NEARZERO ! rsr, don't know what value this should be to avoid divide by 0
      r = 1.0 + (Ql / q_init)
      IF ( r < NEARZERO ) r = NEARZERO
!
! CHECK FOR SIGN OF LATERAL FLOW TERM
      IF ( ABS(1.0-r) < NEARZERO ) THEN
!
! ZERO LATERAL FLOW
        tep = Te
        rexp = -1.0*(b * Seg_length) / q_init
        r = EXP(rexp)
!
! LOSING STREAM
      ELSEIF ( Ql < 0.0 ) THEN
        tep  = Te
        rexp = (Ql - (b * Seg_length)) / Ql
        r = r**rexp
!
! GAINING STREAM
      ELSE
        tep = (((Ql / Seg_length) * Tl_avg) + (((Ak1 * Seg_width) / (4182.0E03)) * Te)) / b
        IF ( Ql > 0.0 ) THEN
          rexp = -b / (Ql / Seg_length)
        ELSE
          rexp = 0.0
        ENDIF
        r = r**rexp
!
! END LATERAL FLOW TERM LOGIC
      ENDIF
!
! DETERMINE WATER TEMPERATURE
      delt  = tep - T0
      denom = (1.0 + (Ak2 / Ak1) * delt * (1.0 - r))
      IF ( ABS(denom) < NEARZERO ) denom = SIGN(NEARZERO, denom)
      tw    = tep - (delt * r / denom)
      IF ( tw < 0.0 ) tw = 0.0
      twavg = tw
!
      END FUNCTION twavg
!
!*******************************************************************************
!    "equilb"
!*******************************************************************************
      SUBROUTINE equilb (Ted, Ak1d, Ak2d, Sh, Svi, Seg_id)
!
!     PURPOSE:
!        1. DETERMINE THE AVERAGE DAILY EQUILIBRIUM WATER TEMPERATURE PARAMETERS
!        2. DETERMINE THE MAXIMUM DAILY EQUILIBRIUM WATER TEMPERATURE PARAMETERS
      USE PRMS_STRMTEMP, ONLY: ZERO_C, Seg_width, T_roff, Seg_humid, Press, T_ground, MPS_CONVERT, &
     &    Seg_ccov, Seg_slope, Seg_potet, Albedo, Upstrm_temp
      USE PRMS_BASIN, ONLY: NEARZERO, CFS2CMS_CONV
      USE PRMS_FLOWVARS, ONLY: Seg_inflow
      USE PRMS_ROUTING, ONLY: Seginc_swrad
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, SQRT, ABS, SNGL, DBLE
      EXTERNAL :: teak1
      REAL, EXTERNAL :: sat_vapor_press_poly
! Arguments:
      REAL, INTENT(INOUT) :: Ted
      REAL, INTENT(OUT) :: Ak1d, Ak2d
      REAL, INTENT(IN) :: Sh, Svi
      INTEGER, INTENT(IN) :: Seg_id
! Local Variables:  !RSR, maybe declare enegry balance fluxes
      DOUBLE PRECISION :: ha, hv, taabs
      REAL :: hf, hs, b, c, d, delt, del_ht, ltnt_ht, bow_coeff
      REAL :: hnet, vp_sat, sw_power, evap, q_init
      REAL, PARAMETER :: AKZ = 1.65, A = 5.40E-8, RAD_CONVERT = 41840.0/86400.0
! *******************************************************************************
! AVERAGE DAILY CONDITIONS
!     BAVG = AW * (q_init**BW)
      taabs = DBLE( T_roff(Seg_id) + ZERO_C )
!      vp_sat = 6.108 * EXP(17.26939 * T_roff(i)/(T_roff(i) + 237.3))
      vp_sat = sat_vapor_press_poly(T_roff(Seg_id))
! 
!  Convert units and set up parameters
      q_init = SNGL( Seg_inflow(Seg_id) * CFS2CMS_CONV )
      IF ( q_init < NEARZERO ) q_init = NEARZERO
      sw_power = SNGL( Seginc_swrad(Seg_id) ) * RAD_CONVERT
      del_ht = 2.36E06   ! could multiple by 10E6 for this and other terms later to reduce round-off
      ltnt_ht = 2495.0E06 
      bow_coeff = (0.00061 * Press(Seg_id))/(vp_sat * (1.0 - Seg_humid(Seg_id)))
      evap = SNGL( Seg_potet(Seg_id) * MPS_CONVERT )
!
! HEAT FLUX COMPONENTS
      ! document - ha = (1-rl)(1-sh)(1+0.17Cl**2)(0.61+0.05*SQRT(vp_sat)*stefan(Ta+273.16)**4
      ha = ( (3.354939D-8 + 2.74995D-9 * DBLE(SQRT(Seg_humid(Seg_id) * vp_sat))) * DBLE((1.0 - Sh) &
     &     * (1.0 + (0.17*(Seg_ccov(Seg_id)**2)))) ) * (taabs**4)
      hf = 9805.0 * (q_init/Seg_width(Seg_id)) * Seg_slope(Seg_id)
      hs = (1.0 - sh) * sw_power * (1.0 - Albedo)
      hv = 5.24D-8 * DBLE(Svi) * (taabs**4)
! Stefan-Boltzmann constant = 5.670373D-08; emissivity of water = 0.9526, times each other: 5.4016D-08
! hw = water-emitted longwave radiation
! hw = 5.4016D-08 * (taabs**4)  hw is include in other computations
!
! DETERMINE EQUILIBIRIUM COEFFICIENTS
      b = bow_coeff * evap * (ltnt_ht + (del_ht * T_roff(Seg_id))) + AKZ - (del_ht * evap)
      c = bow_coeff * del_ht * evap
      d = (SNGL(ha + hv) + hf + hs) + (ltnt_ht * evap * ((bow_coeff * T_roff(Seg_id)) - 1.0) + (T_ground(Seg_id) * AKZ))
!
! DETERMINE EQUILIBRIUM TEMPERATURE & 1ST ORDER THERMAL EXCHANGE COEF.
      Ted = T_roff(Seg_id)
      CALL teak1(A, b, c, d, Ted, Ak1d)
!
! DETERMINE 2ND ORDER THERMAL EXCHANGE COEFFICIENT
      hnet = (A * ((Upstrm_temp(Seg_id) + ZERO_C)**4)) + (b * Upstrm_temp(Seg_id)) - (c * (Upstrm_temp(Seg_id)**2.0)) - d
      delt = Upstrm_temp(Seg_id) - Ted
      IF ( ABS(delt) < NEARZERO) THEN
        Ak2d = 0.0
      ELSE
        Ak2d = ((delt * Ak1d) - hnet) / (delt**2)
      ENDIF
!
! RETURN TO STREAMTEMP FUNCTION
      END SUBROUTINE equilb

!**********************************************************************************
!    "teak1"
!**********************************************************************************
      SUBROUTINE teak1(A, B, C, D, Teq, Ak1c)
!     PURPOSE:  
!        1. TO DETERMINE THE EQUILIBRIUM WATER TEMPERATURE FROM THE ENERGY BALANCE
!           EQUATION BY ITERATING NEWTON'S METHOD
!        2. TO DETERMINE THE 1ST THERMAL EXCHANGE COEFFICIENT.
      USE PRMS_STRMTEMP, ONLY: ZERO_C, Maxiter_sntemp
      IMPLICIT NONE
      INTRINSIC ABS
! Arguments
      REAL, INTENT(IN) ::  A, B, C, D
      REAL, INTENT(INOUT) :: Teq
      REAL, INTENT(OUT) :: Ak1c
! Local variables
      REAL :: teabs, fte, fpte, delte
      INTEGER :: kount
! Parameters
      ! SOLUTION CONVERGENCE TOLERANCE
      REAL, PARAMETER :: TOLRN = 1.0E-4
!**********************************************************************************
      fte = 99999.0 ! rsr, fte was not set
      delte = 99999.0 ! rsr, delte was not set
      kount = 0
! BEGIN NEWTON ITERATION SOLUTION FOR TE
      DO kount = 1, Maxiter_sntemp
        IF ( ABS(fte) < TOLRN ) EXIT
        IF ( ABS(delte) < TOLRN ) EXIT
        teabs  = Teq + ZERO_C
        fte    = (A * (teabs**4.0)) + (B * Teq) - (C * (Teq**2.0)) - D
        fpte   = (4.0 * A * (teabs**3.0)) + B - (2.0 * C * Teq)
        delte  = fte / fpte
        Teq    = Teq - delte
      ENDDO
!
! DETERMINE 1ST THERMAL EXCHANGE COEFFICIENT
      Ak1c = (4.0 * A * ((Teq + ZERO_C)**3.0)) + B - (2.0 * C * Teq)
!
! RETURN TO 'EQUILB' SUBROUTINE
      END SUBROUTINE teak1

!     "shday" ***********************************************************
      SUBROUTINE shday(Seg_id, Shade, Svi)
!
!      THIS SUBPROGRAM IS TO CALCULATE THE TOTAL DAILY SHADE FOR A
!  GIVEN REACH. BOTH TOPOGRAPHIC AND RIPARIAN VEGETATION SHADE
!  IS INCLUDED.
!
!  VARIABLE NAME LIST
!
!      Als    = CURRENT SOLAR ALTITUDE
!      Alrs   = SOLAR ALTITUDE WHEN SOLAR & REACH AZIMUTHS ARE EQUAL
!      Alsmx  = MAXIMUM POSSIBLE SOLAR ALTITUDE
!      Alsr   = LOCAL SUNRISE SOLAR ALTITUDE ! rsr, not used
!      Alss   = LOCAL SUNSET SOLAR ALTITUDE ! rsr, not used
!      Alt    = CURRENT TOPOGRAPHIC ALTITUDE
!      Alte   = EAST SIDE MAXIMUM TOPOGRAPHIC ALTITUDE
!      Altmx  = CURRENT MAXIMUM TOPOGRAPHIC ALTITUDE LIMIT
!      Altop  = CURRENT TOPOGRAPHIC ALTITUDE
!      Altw   = WEST SIDE MAXIMUM TOPOGRAPHIC ALTITUDE
!      Azrh   = STREAM REACH AZIMUTH
!      Azs    = CURRENT SOLAR AZIMUTH
!      Azsr   = LOCAL SUNRISE SOLAR AZIMUTH ! rsr, not used
!      Azss   = LOCAL SUNSET SOLAR AZIMUTH ! rsr, not used
!      Azso   = LEVEL-PLAIN SUNSET AZIMUTH
!      Bavg   = AVERAGE STREAM WIDTH
!      Bs     = SHADED PART OF STREAM WIDTH
!      Cosas  = COS(AS)
!      Cosd   = COS(DECL)
!      Coshs  = COS(HS)
!      Coso   = COS(XLAT)
!      Cosod  = COS(XLAT)*COS(DECL)
!      Dayrad = CONVERSION RATIO FOR JULIAN DAYS TO RADIANS
!      Decl   = CURRENT SOLAR DECLINATION
!      Delhsr = SUNRISE SIDE HOUR ANGLE INCREMENT
!      Delhss = SUNSET SIDE HOUR ANGLE INCREMENT
!      Hrrs   = REACH HOUR ANGLE WHEN SOLAR & REACH AZIMUTHS ARE EQUAL
!      Hrs    = CURRENT SOLAR HOUR ANGLE
!      Hrsr   = LOCAL SUNRISE SOLAR HOUR ANGLE
!      Hrss   = LOCAL SUNSET SOLAR HOUR ANGLE
!      Hrso   = LEVEL-PLAIN SUNRISE/SET SOLAR HOUR ANGLE
!      Nbhs   = NUMBER OF SUNRISE/SET HOUR ANGLE INCREMENTS
!      Shday  = TOTAL DAILY SHADE
!      Sinal  = SIN(Al)
!      Sinar  = SIN(Ar)
!      Sin_d   = SIN(DECL)
!      Sinhsr = SIN(Hrsr)
!      Sinhss = SIN(Hrss)
!      Sinhro = SIN(Hrso)
!      Sino   = SIN(XLAT)
!      Sinod  = SIN(XLAT)*SIN(DECL)
!      Snflag = SOLAR NOON LIMIT FLAG
!      Sti    = TOPOGRAPHIC SHADE
!      Svi    = RIPARIAN VEGETATION SHADE
!      Svri   = SUNRISE VEGETATIVE SHADE
!      Svsi   = SUNSET VEGETATIVE SHADE
!      Tanasr = TAN(Alsr)
!      Tanass = TAN(Alss)
!      Tanalt = TAN(Alt)
!      Tano   = TAN(XLAT)
!      Tanod  = TAN(XLAT)*TAN(DECL)
!      Totsh  = LEVEL-PLAIN TOTAL SHADE POTENTIAL
!      Tolrn  = CONVERGENCE TOLERANCE CRITERIA
!      Flgrs  = SUNRISE FLAG; TRUE IF SUNRISE, FALSE IF SUNSET
!      Flgst  = SUNSET FLAG; TRUE IF SUNSET, FALSE IF SUNRISE
!      Vc = CROWN DIAMETER, CURRENT VEGETATION
!      Vce    = CROWN DIAMETER, EAST SIDE VEGETATION
!      Vco    = CURRENT VEGETATION OVERHANG
!      Vcw    = CROWN DIAMETER, WEST SIDE VEGETATION
!      Vd     = DENSITY, CURRENT VEGETATION
!      Vde    = DENSITY, EAST SIDE VEGETATION
!      Vdw    = DENSITY, WEST SIDE VEGETATION
!      Vh     = HEIGHT, CURRENT VEGETATION
!      Vhe    = HEIGHT, EAST SIDE VEGETATION
!      Vhw    = HEIGHT, WEST SIDE VEGETATION
!      Vo     = OFFSET, CURRENT VEGETATION
!      Voe    = OFFSET, EAST SIDE VEGETATION
!      Vow    = OFFSET, WEST SIDE VEGETATION
!
      USE PRMS_SET_TIME, ONLY: Jday
      USE PRMS_STRMTEMP, ONLY: Azrh, Alte, Altw, Dlit, Seg_width, Width_values, Width_dim, Width_flow, &
     &    PI, HALF_PI, Cos_seg_lat, Sin_seg_lat, Cos_lat_decl, Horizontal_hour_angle, &
     &    Level_sunset_azimuth, Max_solar_altitude, Sin_alrs, Sin_declination, Sin_lat_decl, Total_shade
      USE PRMS_FLOWVARS, ONLY: Seg_outflow
      USE PRMS_BASIN, ONLY: CFS2CMS_CONV
      IMPLICIT NONE
! Functions
      INTRINSIC COS, SIN, TAN, ACOS, ASIN, ATAN, ABS, MAX, SNGL
      REAL, EXTERNAL:: solalt, rprnvg
      EXTERNAL snr_sst
! Arguments
      INTEGER, INTENT(IN) :: Seg_id
      REAL, INTENT(OUT):: Shade, Svi
! Local Variables
      REAL :: outflow, coso, cosod, sin_d, sino, sinod
      REAL :: altmx, alsmx, als, almn, almx
      REAL :: azso, azmn, azmx, azs, hrrs, hrsr, hrss, hrso, hrs, hrrh
      REAL :: temp, totsh, sti
      REAL :: altop(3), aztop(3)
!      REAL :: alsr, alss, azsr, azss, altr, alts
      INTEGER :: index
! PARAMETER
      REAL, PARAMETER :: RADTOSECOND = 86400.0/(2.0 * PI)
!*********************************************************************************
! Assign flow-dependent segment width value
      IF ( Width_dim == 1 ) THEN
        Seg_width(Seg_id) = Width_values(Seg_id, 1)
      ELSE
        outflow = SNGL( Seg_outflow(Seg_id) * CFS2CMS_CONV )
        IF ( (outflow > Width_flow(1)) .AND. (outflow < Width_flow(2)) ) THEN
          index = (outflow - Width_flow(1))/Width_flow(3)
          index = index + 1
          Seg_width(Seg_id) = Width_values(Seg_id, index)
        ELSEIF ( outflow >= Width_flow(2) ) THEN
          Seg_width(Seg_id) = Width_values(Seg_id, Width_dim)
        ELSE
          Seg_width(Seg_id) = Width_values(Seg_id, 1)
        ENDIF
      ENDIF
!
!  LATITUDE TRIGONOMETRIC PARAMETERS 
      coso = Cos_seg_lat(Seg_id)
      sino = Sin_seg_lat(Seg_id)
      sin_d = Sin_declination(Jday, Seg_id)
      sinod = Sin_lat_decl(Jday, Seg_id)
      cosod = Cos_lat_decl(Jday, Seg_id)
!
!  INITIALIZE LOCAL SUNRISE/SET SOLAR PARAMETERS
!      altr = 0.0
!      alts = 0.0
!      alrs = 0.0
!      hrrs = 0.0
!      azsr = 0.0
!      azss = 0.0
!      alsr = 0.0
!      alss = 0.0
      hrsr = 0.0
      hrss = 0.0
!
!  MAXIMUM POSSIBLE SOLAR ALTITUDE
      alsmx = Max_solar_altitude(Jday, Seg_id)
!
!  LEVEL-PLAIN SUNRISE/SET HOUR ANGLE
      hrso = Horizontal_hour_angle(Jday, Seg_id)
!
!  LEVEL-PLAIN SOLAR AZIMUTH
      azso = Level_sunset_azimuth(Jday, Seg_id)
!
!  TOTAL POTENTIAL SHADE ON LEVEL-PLAIN
      totsh = Total_shade(Jday, Seg_id)
!
!  CHECK FOR REACH AZIMUTH LESS THAN SUNRISE
      IF ( Azrh(Seg_id) <= (-azso) ) THEN
        hrrs = -hrso
!
!  CHECK FOR REACH AZIMUTH GREATER THAN SUNSET
      ELSEIF ( Azrh(Seg_id) >= azso ) THEN
        hrrs = hrso
!
!  REACH AZIMUTH IS BETWEEN SUNRISE & SUNSET
      ELSEIF ( Azrh(Seg_id) == 0.0 ) THEN
        hrrs = 0.0
      ELSE
        temp = (Sin_alrs(Jday, Seg_id) - sinod) / cosod
        IF ( ABS(temp) > 1.0 ) temp = SIGN(1.0,temp)
        hrrs = SIGN(ACOS(temp), Azrh(Seg_id))
!
!  END REACH & SOLAR AZIMUTH CHECK
      ENDIF
!
!  CHECK IF LEVEL-PLAIN
      IF ( (Alte(Seg_id) == 0.0 ) .AND. (Altw(Seg_id) == 0.0) ) THEN
!        azsr = -azso
        hrsr = -hrso
!        azss = azso
        hrss = hrso
        sti = 0.0
        Svi = (rprnvg(hrsr, hrrs, hrss, sino, coso, sin_d, cosod, sinod, Seg_id)) / (Seg_width(Seg_id) * totsh)
!
      ELSE
!  INITIALIZE SHADE VALUES
!  
!  INSERT STARTING TOPOGRAPHIC AZIMUTH VALUES BETWEEN LEVEL PLAIN SUNRISE AND SUNSET
        aztop = 0.0
!
!  DETERMINE SUNRISE HOUR ANGLE.
        altop = 0.0
        IF ( -azso <= Azrh(Seg_id) ) THEN
          altop(1) = Alte(Seg_id)
          aztop(1) = azso*(Alte(Seg_id)/HALF_PI) - azso
        ELSE
          altop(1) = Altw(Seg_id)
          aztop(1) = azso*(Altw(Seg_id)/HALF_PI) - azso
        ENDIF
! LEVEL PLAIN
        IF (altop(1) == 0.0) THEN
          hrsr = -hrso
! NOT
        ELSE
! LOOK FOR SOLUTION BETWEEN LIMITS OF LEVEL PLAIN SUNRISE AND NOON
          azmn = -azso
          azmx = 0.0
          azs = aztop(1)
          altmx = altop(1)
          almn = 0.0
          almx = 1.5708
          als = solalt(coso, sino, sin_d, azs, almn, almx)
          CALL snr_sst(coso, sino, sin_d, altmx, almn, almx, azmn, azmx, azs, als, hrs, Seg_id)
!          azsr = azs
!          alsr = als
          hrsr = hrs
!          altr = altmx
        ENDIF
!
!  DETERMINE SUNSET HOUR ANGLE.
        IF ( azso <= Azrh(Seg_id) )THEN
          altop(2) = Alte(Seg_id)
          aztop(2) = azso - azso*(Alte(Seg_id)/HALF_PI)
        ELSE
          altop(2) = Altw(Seg_id)
          aztop(2) = azso - azso*(Altw(Seg_id)/HALF_PI)
        ENDIF
! LEVEL PLAIN
        IF (altop(2) == 0.0) THEN
          hrss = hrso
! NOT
        ELSE
! LOOK FOR SOLUTION BETWEEN LIMITS OF NOON AND LEVEL PLAIN SUNSET
          azmn = 0.0
          azmx = azso
          azs = aztop(2)
          altmx = altop(2)
          almn = 0.0
          almx = 1.5708
          als = solalt(coso, sino, sin_d, azs, almn, almx)
          CALL snr_sst(coso, sino, sin_d, altmx, almn, almx, azmn, azmx, azs, als, hrs, Seg_id)
!          azss = azs
!          alss = als
          hrss = hrs
!          alts = altmx
        ENDIF
!
!  SOLVE FOR SHADE INCREMENTS THIS SEGMENT
        IF ( hrrs < hrsr ) THEN
          hrrh = hrsr
        ELSEIF ( hrrs > hrss ) THEN
          hrrh = hrss
        ELSE
          hrrh = hrrs
        ENDIF
        Dlit(Seg_id) = (hrss - hrsr) * RADTOSECOND
        sti = 1.0 - ((((hrss - hrsr) * sinod) + ((SIN(hrss) - SIN(hrsr)) * cosod)) / (totsh))
! ??? seg_width changes with flow, so need to do each timestep ???????
        Svi = ((rprnvg(hrsr, hrrh, hrss, sino, coso, sin_d, cosod, sinod, Seg_id)) / (Seg_width(Seg_id)*totsh))
!
!  END SUNRISE/SUNSET CALCULATION 
      ENDIF
!
!  CHECK FOR ROUNDOFF ERRORS 
      IF ( sti < 0.0 ) sti = 0.0
      IF ( sti > 1.0 ) sti = 1.0
      IF ( Svi < 0.0 ) Svi = 0.0
      IF ( Svi > 1.0 )  Svi = 1.0
!
!  RECORD TOTAL SHADE
      Shade = sti + Svi

      END SUBROUTINE shday
!
!**********************************************************************************************************
!    "snr_sst"
      SUBROUTINE snr_sst (Coso, Sino, Sin_d, Alt, Almn, Almx, Azmn, Azmx, Azs, Als, Hrs, Seg_id)
!
!     THIS SUBPROGRAM DETERMINES THE LOCAL SOLAR SUNRISE/SET
! AZIMUTH, ALTITUDE, AND HOUR ANGLE
!
      USE PRMS_STRMTEMP, ONLY: Azrh, PI, Maxiter_sntemp
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Functions
      INTRINSIC TAN, SIN, COS, ACOS, ASIN, ABS
!  Arguments
      INTEGER, INTENT(IN):: Seg_id
      REAL, INTENT(IN):: Coso, Sino, Sin_d, Alt, Almn, Almx, Azmn, Azmx
      REAL, INTENT(INOUT):: Azs, Als
      REAL, INTENT(OUT):: Hrs
!  Local Variables
      REAL :: cosazs, sinazs, sinazr, cosazr, cosals, f, g, fazs, fals, gazs, gals, xjacob
      REAL :: sinals, tanalt, tano, tanals, temp, delazs, delals
      INTEGER :: count
!***********************************************************************************************************
! TRIG FUNCTION FOR LOCAL ALTITUDE
      tanalt = TAN(Alt)
      tano   = Sino / Coso
      f = 999999.0 !rsr, these need values
      delazs = 9999999.0
      g = 99999999.0
      delals = 99999999.0
! BEGIN NEWTON-RAPHSON SOLUTION
!
      DO count = 1, Maxiter_sntemp
        IF ( ABS(delazs) < NEARZERO ) EXIT
        IF ( ABS(delals) < NEARZERO ) EXIT
        IF ( ABS(f) < NEARZERO ) EXIT
        IF ( ABS(g) < NEARZERO ) EXIT
!
        cosazs = COS(Azs)
        sinazs = SIN(Azs)
!
        sinazr = ABS(SIN(Azs - Azrh(Seg_id)))
        IF ( (((Azs-Azrh(Seg_id)) <= 0.0 ) .AND. ((Azs-Azrh(Seg_id)) <= (-PI))) .OR. &
     &       (((Azs-Azrh(Seg_id)) > 0.0 ) .AND. ((Azs-Azrh(Seg_id)) <= PI)) ) THEN
          cosazr = COS(Azs-Azrh(Seg_id))
        ELSE
          cosazr = -COS(Azs-Azrh(Seg_id))
        ENDIF
! 
        cosals = COS(Als)
        IF ( cosals < NEARZERO ) cosals = NEARZERO
        sinals = SIN(Als)
        tanals = sinals / cosals
! FUNCTIONS OF AZS & ALS
        f = cosazs- (((Sino * sinals) - Sin_d) / (Coso * cosals))
        g = tanals - (tanalt * sinazr)
! FIRST PARTIALS DERIVATIVES OF F & G
        fazs = -sinazs
        fals = ((tanals * (Sin_d / Coso)) - (tano / cosals)) / cosals
        gazs = -tanalt * cosazr
        gals = 1.0 / (cosals * cosals)
! JACOBIAN
        xjacob = (fals * gazs) - (fazs * gals)
! DELTA CORRECTIONS
        delazs = ((f * gals) - (g * fals)) / xjacob
        delals = ((g * fazs) - (f * gazs)) / xjacob
! NEW VALUES OF AZS & ALS
        Azs = Azs + delazs
        Als = Als + delals
! CHECK FOR LIMITS
        IF ( Azs < (Azmn + NEARZERO) ) Azs = (Azmn + NEARZERO)
        IF ( Azs > (Azmx - NEARZERO) ) Azs = (Azmx - NEARZERO)
        IF ( Als < (Almn + NEARZERO) ) Als = (Almn + NEARZERO)
        IF ( Als > (Almx - NEARZERO) ) Als = (Almx - NEARZERO)
!
      ENDDO
! 
! ENSURE AZIMUTH REMAINS BETWEEN -PI & PI
      IF ( Azs < (-PI) ) THEN
        Azs = Azs + PI
      ELSEIF ( Azs > PI) THEN
        Azs = Azs - PI
      ENDIF
!
! DETERMINE LOCAL SUNRISE/SET HOUR ANGLE
      sinals = SIN(Als)
      temp = (sinals - (Sino * Sin_d)) / (Coso * COS(ASIN(Sin_d)))
      IF ( ABS(temp) > 1.0 ) temp = SIGN(1.0,temp)
      Hrs = SIGN(ACOS(temp), Azs)

      END SUBROUTINE snr_sst

!*****************************************************************************
!     "solalt"
      REAL FUNCTION solalt (Coso, Sino, Sin_d, Az, Almn, Almx)
!
!      THIS SUBPROGRAM IS TO DETERMINE THE SOLAR ALTITUDE WHEN THE
!  TRIGONOMETRIC PARAMETERS FOR LATITUDE, DECLINATION, AND AZIMUTH
!  ARE GIVEN.
!
!  VARIABLE NAME LIST
!
!      Al     = TRIAL SOLAR ALTITUDE
!      AZ     = SOLAR AZIMUTH
!      COSAL  = COS(AL)
!      COSAZ  = COS(AZ)
!      Coso   = COS(XLAT)
!      DELAL  = INCREMENTAL CORRECTION TO AL
!      FAL    = FUNCTION OF AL
!      FPAL   = FIRST DERIVATIVE OF FAL
!      FPPAL  = SECOND DERIVATIVE OF FAL
!      Sin_d   = SIN(DECL)
!      Sino   = SIN(XLAT)
      USE PRMS_STRMTEMP, ONLY: HALF_PI, Maxiter_sntemp
      USE PRMS_BASIN, ONLY: NEARZERO
      IMPLICIT NONE
! Functions
      INTRINSIC ASIN, ABS, COS, SIN
! Arguments
      REAL, INTENT(IN):: Coso, Sino, Sin_d, Az, Almn, Almx
! Local Variables
      REAL :: cosal, sinal, fal, fpal, fppal, al, alold, delal, a, b, cosaz, temp
      INTEGER :: kount
!*************************************************************************************
!
!  CHECK COS(AZ) EQUAL TO 0
      IF ( ABS(ABS(Az) - HALF_PI) < NEARZERO ) THEN
        temp = ABS(Sin_d / Sino)
        IF ( temp > 1.0 ) temp = 1.0
        Al = ASIN(temp)
      ELSE
!
!  DETERMINE SOLAR ALTITUDE FUNCTION COEFFICIENTS
        cosaz  = COS(Az)
        a      = Sino / (cosaz * Coso)
        b      = Sin_d / (cosaz * Coso)
!
!  INITIALIZE
        al     = (Almn + Almx) / 2.0
        kount = 0
        fal = COS(al) - (a * SIN(al)) + b
        delal = fal/(-SIN(al) - (a * COS(al)))
!
!  BEGIN NEWTON SECOND-ORDER SOLUTION
        DO kount = 1, Maxiter_sntemp
          IF ( ABS(fal) < NEARZERO ) EXIT
          IF ( ABS(delal) < NEARZERO ) EXIT
          alold  = al
          cosal  = COS(al)
          sinal  = SIN(al)
          fal    =  cosal - (a * sinal) + b
          fpal   = -sinal - (a * cosal)
          IF ( kount <= 3 ) THEN
            delal = fal / fpal
          ELSE
            fppal = b - fal
            delal = (2.0 * fal * fpal) / ((2.0 * fpal * fpal) - (fal * fppal))
          ENDIF
          al = al - delal 
          IF (al < Almn) al = (alold + Almn) / 2.0
          IF (al > Almx) al = (alold + Almx) / 2.0
        ENDDO
      ENDIF
!
!  SOLUTION OBTAINED
      solalt = al

      END FUNCTION solalt

!***********************************************************************
      REAL FUNCTION rprnvg (Hrsr, Hrrs, Hrss, Sino, Coso, Sin_d, Cosod, Sinod, Seg_id)
!
!      THIS SUBPROGRAM IS TO COMPUTE THE RIPARIAN VEGETATION SHADE
!  SEGMENT BETWEEN THE TWO HOUR ANGLES HRSR & HRSS.
!
      USE PRMS_STRMTEMP, ONLY: Azrh, Vce, Vdemx, Vhe, Voe, Vcw, Vdwmx, Vhw, Vow, Seg_width, &
     &    Vdemn, Vdwmn, HALF_PI
      USE PRMS_BASIN, ONLY: NEARZERO
      USE PRMS_SET_TIME, ONLY: Summer_flag
      IMPLICIT NONE
! Functions
      INTRINSIC COS, SIN, ASIN, ACOS, ABS
! Arguments
      REAL, INTENT(IN) :: Hrsr, Hrrs, Hrss, Sino, Coso, Sin_d, Cosod, Sinod
      INTEGER, INTENT(IN):: Seg_id
! Local Variables
      REAL :: svri, svsi, hrs, vco, delhsr, coshrs
      REAL :: sinhrs, temp, als, cosals, sinals, azs, bs, delhss
      INTEGER :: n
! Parameters
      INTEGER, PARAMETER :: NBHS = 15
      DOUBLE PRECISION, SAVE :: Epslon(15), Weight(15)
      DATA Epslon / .006003741, .031363304, .075896109, .137791135, .214513914, &
     &              .302924330, .399402954, .500000000, .600597047, .697075674, &
     &              .785486087, .862208866, .924103292, .968636696, .993996259 /
      DATA Weight / .015376621, .035183024, .053579610, .069785339, .083134603, &
     &              .093080500, .099215743, .101289120, .099215743, .093080500, &
     &              .083134603, .069785339, .053579610, .035183024, .015376621 /
!******************************************************************************
!  ****************** Determine seasonal shade
!
!  CKECK FOR NO SUNRISE
      IF ( Hrsr == Hrss ) THEN
        svri = 0.0
        svsi = 0.0

      ELSE
!
!  VEGETATIVE SHADE BETWEEN SUNRISE & REACH HOUR ANGLES
        svri = 0.0
        IF ( Hrsr < Hrrs ) THEN
          vco = ( Vce(Seg_id)/2.0 ) - Voe(Seg_id)
!
!  DETERMINE SUNRISE SIDE HOUR ANGLE INCREMENT PARAMETERS
          delhsr = Hrrs - Hrsr
!
!  PERFORM NUMERICAL INTEGRATION
          DO n = 1, NBHS
!  CURRENT SOLAR HOUR ANGLE
            hrs = Hrsr + (Epslon(n) * delhsr)
            coshrs = COS(hrs)
            sinhrs = SIN(hrs)
!  CURRENT SOLAR ALTITUDE
            temp = Sinod + (Cosod * coshrs)
            IF ( temp > 1.0 ) temp = 1.0
            als = ASIN(temp)
            cosals = COS(als)
            sinals = SIN(als)
            IF ( sinals == 0.0 ) sinals = NEARZERO
!  CURRENT SOLAR AZIMUTH
            temp = ((Sino * sinals) - Sin_d) / (Coso * cosals)
            IF ( ABS(temp) > 1.0 ) temp = SIGN(1.0, temp)
            azs = ACOS(temp)
            IF ( azs < 0.0 ) azs = HALF_PI - azs
            IF ( hrs < 0.0 ) azs = -azs
!  DETERMINE AMOUNT OF STREAM WIDTH SHADED 
            bs = ((Vhe(Seg_id) * (cosals/sinals)) * ABS(SIN(azs-Azrh(Seg_id)))) + vco
            IF ( bs < 0.0 ) bs = 0.0
            IF ( bs > Seg_width(Seg_id) ) bs = Seg_width(Seg_id)
!  INCREMENT SUNRISE SIDE VEGETATIVE SHADE
            IF ( Summer_flag == 1 ) THEN ! put back spring and autumn
               svri = svri + (Vdemx(Seg_id) * bs * sinals * Weight(n))
            ELSE
               svri = svri + (Vdemn(Seg_id) * bs * sinals * Weight(n))
            ENDIF
          ENDDO
!
          svri = svri * delhsr
        ENDIF
!
!  VEGETATIVE SHADE BETWEEN REACH & SUNSET HOUR ANGLES
        svsi = 0.0
        IF ( Hrss > Hrrs ) THEN
          vco = (Vcw(Seg_id)/2.0 ) - Vow(Seg_id)
!
!  DETERMINE SUNSET SIDE HOUR ANGLE INCREMENT PARAMETERS
          delhss = Hrss - Hrrs
!
!  PERFORM NUMERICAL INTEGRATION
          DO n = 1, Nbhs
!  CURRENT SOLAR HOUR ANGLE
            hrs = Hrrs + (Epslon(n) * delhss)
            coshrs = COS(hrs)
            sinhrs = SIN(hrs)
!  CURRENT SOLAR ALTITUDE
            temp = Sinod + (Cosod * coshrs)
            IF ( temp > 1.0 ) temp = 1.0
            als = ASIN(temp)
            cosals = COS(als)
            sinals = SIN(als)
            IF ( sinals == 0.0 ) sinals = NEARZERO
!  CURRENT SOLAR AZIMUTH
            temp = ((Sino * sinals) - Sin_d) / (Coso * cosals)
            IF ( ABS(temp) > 1.0 ) temp = SIGN(1.0, temp)
            azs = ACOS(temp)
            IF ( azs < 0.0 ) azs = HALF_PI - azs
            IF ( hrs < 0.0 ) azs = -azs
!  DETERMINE AMOUNT OF STREAM WIDTH SHADED
            bs = ((Vhw(Seg_id) * (cosals/sinals)) * ABS(SIN(azs-Azrh(Seg_id)))) + vco
            IF ( bs < 0.0 ) bs = 0.0
            IF ( bs > Seg_width(Seg_id) ) bs = Seg_width(Seg_id)
!  INCREMENT SUNSET SIDE VEGETATIVE SHADE
            IF ( Summer_flag == 1 ) THEN ! fix for seasons
               svsi = svsi + (Vdwmx(Seg_id) * bs * sinals * Weight(n))
            ELSE
               svsi = svsi + (Vdwmn(Seg_id) * bs * sinals * Weight(n))
            ENDIF
!
          ENDDO
!
          svsi = svsi * delhss
        ENDIF
      ENDIF
!
!  COMBINE SUNRISE/SET VEGETATIVE SHADE VALUES
      rprnvg = svri + svsi

      END FUNCTION rprnvg

!***********************************************************************
!     stream_temp_restart - write or read stream_temp restart file
!***********************************************************************
      SUBROUTINE stream_temp_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_STRMTEMP
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) T_gw
        WRITE ( Restart_outunit ) Dlit
        WRITE ( Restart_outunit ) T_roff
        WRITE ( Restart_outunit ) Temp_avg
        WRITE ( Restart_outunit ) Upstrm_temp
        WRITE ( Restart_outunit ) Seg_humid
        WRITE ( Restart_outunit ) Seg_maxtemp
        WRITE ( Restart_outunit ) Seg_width
        WRITE ( Restart_outunit ) Seg_ccov
        WRITE ( Restart_outunit ) Seg_rain
        WRITE ( Restart_outunit ) Seg_potet
        WRITE ( Restart_outunit ) Seg_melt
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Dlit
        READ ( Restart_inunit ) T_roff
        READ ( Restart_inunit ) Temp_avg
        READ ( Restart_inunit ) Upstrm_temp
        READ ( Restart_inunit ) Seg_humid
        READ ( Restart_inunit ) Seg_maxtemp
        READ ( Restart_inunit ) Seg_width
        READ ( Restart_inunit ) Seg_ccov
        READ ( Restart_inunit ) Seg_rain
        READ ( Restart_inunit ) Seg_potet
        READ ( Restart_inunit ) Seg_melt
      ENDIF
      END SUBROUTINE stream_temp_restart
