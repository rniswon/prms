!***********************************************************************
! Reads and stores observed data from all specified measurement stations
!***********************************************************************
      MODULE PRMS_OBS
      USE PRMS_CONSTANTS, ONLY: DOCUMENTATION, ACTIVE, OFF, xyz_dist_module, &
     &    MONTHS_PER_YEAR, CMS, CFS, CFS2CMS_CONV
      USE PRMS_MODULE, ONLY: Model, Nratetbl, Ntemp, Nrain, Nsol, Nobs, Nevap, Nsnow, &
     &    Precip_flag
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Time Series Data'
      character(len=*), parameter :: MODNAME = 'obs'
      character(len=*), parameter :: Version_obs = '2020-12-02'
      INTEGER, SAVE :: Nlakeelev, Nwind, Nhumid, Rain_flag
!   Declared Variables
      INTEGER, SAVE :: Rain_day
      REAL, SAVE, ALLOCATABLE :: Pan_evap(:), Runoff(:), Precip(:)
      REAL, SAVE, ALLOCATABLE :: Humidity(:), Wind_speed(:)
      REAL, SAVE, ALLOCATABLE :: Tmax(:), Tmin(:), Solrad(:), Snowdepth(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Streamflow_cfs(:), Streamflow_cms(:)
      ! Lake Module Variables
      REAL, SAVE, ALLOCATABLE :: Gate_ht(:), Lake_elev(:)
!   Declared Parameters
      INTEGER, SAVE :: Runoff_units, Rain_code(MONTHS_PER_YEAR)
      END MODULE PRMS_OBS

!***********************************************************************
!     main obs routine
!***********************************************************************
      INTEGER FUNCTION obs()
      USE PRMS_CONSTANTS, ONLY: RUN, SETDIMENS, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: obsdecl, obsinit, obsrun, obssetdims
!***********************************************************************
      obs = 0

! obsrun not needed as data file is read in read_data_file and variables are set there
      IF ( Process_flag==SETDIMENS ) THEN
        obs = obssetdims()
      ELSEIF ( Process_flag==DECL ) THEN
        obs = obsdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        obs = obsinit()
      ENDIF

      END FUNCTION obs

!***********************************************************************
!     obssetdims - declares obs module specific dimensions
!***********************************************************************
      INTEGER FUNCTION obssetdims()
      USE PRMS_CONSTANTS, ONLY: MAXDIM
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL read_error
!***********************************************************************
      obssetdims = 0

      IF ( decldim('nlakeelev', 0, MAXDIM, &
     &     'Maximum number of lake elevations for any rating table data set')/=0 ) CALL read_error(7, 'nlakeelev')
      IF ( decldim('nwind', 0, MAXDIM, 'Number of wind-speed measurement stations')/=0 ) CALL read_error(7, 'nwind')
      IF ( decldim('nhumid', 0, MAXDIM, 'Number of relative humidity measurement stations')/=0 ) CALL read_error(7, 'nhumid')

      END FUNCTION obssetdims

!***********************************************************************
!     obsdecl - makes public variable declarations for the obs module
!   Declared Parameters
!     rain_code
!***********************************************************************
      INTEGER FUNCTION obsdecl()
      USE PRMS_OBS
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getdim, declparam
      EXTERNAL :: read_error, print_module, declvar_real, declvar_dble
!***********************************************************************
      obsdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_obs)

!   Declared Variables
      IF ( Nobs>0 ) THEN
        ALLOCATE ( Runoff(Nobs) )
        CALL declvar_real(MODNAME, 'runoff', 'nobs', Nobs, 'real', &
     &       'Streamflow at each measurement station', &
     &       'runoff_units', Runoff)
        ALLOCATE ( Streamflow_cfs(Nobs) )
        CALL declvar_dble(MODNAME, 'streamflow_cfs', 'nobs', Nobs, 'double', &
     &       'Streamflow at each measurement station', &
     &       'cfs', Streamflow_cfs)
        ALLOCATE ( Streamflow_cms(Nobs) )
        CALL declvar_dble(MODNAME, 'streamflow_cms', 'nobs', Nobs, 'double', &
     &       'Streamflow at each measurement station', &
     &       'cms', Streamflow_cms)
        IF ( declparam(MODNAME, 'runoff_units', 'one', 'integer', &
     &       '0', '0', '1', &
     &       'Measured streamflow units', 'Measured streamflow units (0=cfs; 1=cms)', &
     &       'none')/=0 ) CALL read_error(1, 'runoff_units')
      ENDIF

      IF ( Nrain>0 ) THEN
        ALLOCATE ( Precip(Nrain) )
        CALL declvar_real(MODNAME, 'precip', 'nrain', Nrain, 'real', &
     &       'Precipitation at each measurement station', &
     &       'precip_units', Precip)
      ENDIF

      IF ( Ntemp>0 ) THEN
        ALLOCATE ( Tmin(Ntemp) )
        CALL declvar_real(MODNAME, 'tmin', 'ntemp', Ntemp, 'real', &
     &       'Minimum air temperature at each measurement station', &
     &       'temp_units', Tmin)
        ALLOCATE ( Tmax(Ntemp) )
        CALL declvar_real(MODNAME, 'tmax', 'ntemp', Ntemp, 'real', &
     &       'Maximum air temperature at each measurement station', &
     &       'temp_units', Tmax)
      ENDIF

      IF ( Nsol>0 ) THEN
        ALLOCATE ( Solrad(Nsol) )
        CALL declvar_real(MODNAME, 'solrad', 'nsol', Nsol, 'real', &
     &       'Solar radiation at each measurement station', &
     &       'Langleys', Solrad)
      ENDIF

      Nhumid = getdim('nhumid')
      IF ( Nhumid==-1 ) CALL read_error(6, 'nhumid')
      Nwind = getdim('nwind')
      IF ( Nwind==-1 ) CALL read_error(6, 'nwind')
      Nlakeelev = getdim('nlakeelev')
      IF ( Nlakeelev==-1 ) CALL read_error(6, 'nlakeelev')

      IF ( Model==DOCUMENTATION ) THEN
        IF ( Nsnow==0 ) Nsnow = 1
        IF ( Nhumid==0 ) Nhumid = 1
        IF ( Nwind==0 ) Nwind = 1
        IF ( Nlakeelev==0 ) Nlakeelev = 1
      ENDIF

      IF ( Nsnow>0 ) THEN
        ALLOCATE ( Snowdepth(Nsnow) )
        CALL declvar_real(MODNAME, 'snowdepth', 'nsnow', Nsnow, 'real', &
     &       'Snow depth at each measurement station', &
     &       'inches', Snowdepth)
      ENDIF

      IF ( Nevap>0 ) THEN
        ALLOCATE ( Pan_evap(Nevap) )
        CALL declvar_real(MODNAME, 'pan_evap', 'nevap', Nevap, 'real', &
     &       'Pan evaporation at each measurement station', &
     &       'inches', Pan_evap)
      ENDIF

      IF ( Nhumid>0 ) THEN
        ALLOCATE ( Humidity(Nhumid) )
        CALL declvar_real(MODNAME, 'humidity', 'nhumid', Nhumid, 'real', &
     &       'Relative humidity at each measurement station', &
     &       'percentage', Humidity)
      ENDIF

      IF ( Nwind>0 ) THEN
        ALLOCATE ( Wind_speed(Nwind) )
        CALL declvar_real(MODNAME, 'wind_speed', 'nwind', Nwind, 'real', &
     &       'Wind speed at each measurement station', &
     &       'mph', Wind_speed)
      ENDIF

!   Declared Parameters
      Rain_flag = OFF
      IF ( Precip_flag==xyz_dist_module ) Rain_flag = ACTIVE
      IF ( Rain_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        CALL declvar_int(MODNAME, 'rain_day', 'one', 1, 'integer', &
     &       'Flag to set the form of any precipitation to rain (0=determine form; 1=rain)', &
     &       'none', Rain_day)
        IF ( declparam(MODNAME, 'rain_code', 'nmonths', 'integer', &
     &       '2', '1', '5', &
     &       'Flag indicating rule for precipitation station use', &
     &       'Monthly (January to December) flag indicating rule for'// &
     &       ' precipitation measurement station use (1=only'// &
     &       ' precipitation if the regression stations have'// &
     &       ' precipitation; 2=only precipitation'// &
     &       ' if any station in the basin has precipitation;'// &
     &       ' 3=precipitation if xyz says so; 4=only'// &
     &       ' precipitation if rain_day variable is set to 1; 5=only'// &
     &       ' precipitation if psta_freq_nuse stations have precipitation)', &
     &       'none')/=0 ) CALL read_error(1, 'rain_code')
      ENDIF

! Lake Variables
      IF ( Nratetbl>0 ) THEN
        ALLOCATE ( Gate_ht(Nratetbl) )
        CALL declvar_real(MODNAME, 'gate_ht', 'nratetbl', Nratetbl, 'real', &
     &       'Height of the gate opening at each dam with a gate', &
     &       'inches', Gate_ht)
      ENDIF

      IF ( Nlakeelev>0 ) THEN
        ALLOCATE ( Lake_elev(Nlakeelev) )
        CALL declvar_real(MODNAME, 'lake_elev', 'nlakeelev', Nlakeelev, 'real', &
     &       'Elevation of each simulated lake surface', &
     &       'feet', Lake_elev)
      ENDIF

      END FUNCTION obsdecl

!***********************************************************************
!     obsinit - initializes obs module
!***********************************************************************
      INTEGER FUNCTION obsinit()
      USE PRMS_OBS
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error
!***********************************************************************
      obsinit = 0

      Runoff_units = CFS
      IF ( Nobs>0 ) THEN
        IF ( getparam(MODNAME, 'runoff_units', 1, 'integer', Runoff_units)/=0 ) CALL read_error(2, 'runoff_units')
      ENDIF

      IF ( Rain_flag==ACTIVE ) THEN
        IF ( getparam(MODNAME, 'rain_code', MONTHS_PER_YEAR, 'integer', Rain_code)/=0 ) CALL read_error(2, 'rain_code')
      ENDIF

      IF ( Nobs>0 ) THEN
        Runoff = 0.0
        Streamflow_cfs = 0.0D0
        Streamflow_cms = 0.0D0
      ENDIF
      IF ( Nrain>0 ) Precip = 0.0
      Rain_day = OFF
      IF ( Ntemp>0 ) THEN
        Tmax = 0.0
        Tmin = 0.0
      ENDIF
      IF ( Nsol>0 ) Solrad = 0.0
      IF ( Nevap>0 ) Pan_evap = 0.0
      IF ( Nsnow>0 ) Snowdepth = 0.0
      IF ( Nlakeelev>0 ) Lake_elev = 0.0
      IF ( Nratetbl>0 ) Gate_ht = 0.0
      IF ( Nhumid>0 ) Humidity = 0.0
      IF ( Nwind>0 ) Wind_speed = 0.0

      END FUNCTION obsinit
