!***********************************************************************
! Computes the potential evapotranspiration using the Penman-Monteith
! formulation according to Murray (1967), shown equation 13 in Irmak and others (2012)
! Irmak, Suat, Kabenge, Isa, Skaggs, K.E., and Mutiibwa, Denis, 2012
!   Trend and magnitude of changes in climate variables and reference
!   evapotranspiration over 116-yr period in the Platte River Basin,
!   central Nebraska-USA: Journal of Hydrology, V. 420-421, p. 228-244
!***********************************************************************
      MODULE PRMS_POTET_PM_STA
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=12), SAVE :: MODNAME
        !REAL, SAVE, ALLOCATABLE :: Tavgc_ante(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Pm_n_coef(:, :), Pm_d_coef(:, :), Crop_coef(:, :)
        INTEGER, SAVE, ALLOCATABLE :: Hru_windspeed_sta(:), Hru_humidity_sta(:)
      END MODULE PRMS_POTET_PM_STA

!***********************************************************************
      INTEGER FUNCTION potet_pm_sta()
      USE PRMS_POTET_PM_STA
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file, Init_vars_from_file, Parameter_check_flag, Inputerror_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, Hru_elev_meters
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Swrad, Tminc, Tmaxc, &
     &    Tempc_dewpt, Vp_actual, Lwrad_net, Vp_slope, Vp_sat
      USE PRMS_OBS, ONLY: Humidity, Wind_speed, Nwind, Nhumid
      USE PRMS_SOLTAB, ONLY: Soltab_potsw
      USE PRMS_SET_TIME, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC SQRT, DBLE, LOG, SNGL
      INTEGER, EXTERNAL :: declparam, getparam
      REAL, EXTERNAL :: sat_vapor_press
      EXTERNAL read_error, print_module, potet_pm_restart, checkdim_param_limits
! Local Variables
      INTEGER :: i, j
      REAL :: elh, prsr, psycnst, heat_flux, net_rad, vp_deficit, a, b, c 
      REAL :: A1, B1, t1, num, den, stab, sw
      CHARACTER(LEN=80), SAVE :: Version_potet
!***********************************************************************
      potet_pm_sta = 0

      IF ( Process(:3)=='run' ) THEN
        Basin_potet = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!...ATMOSPHERIC PRESSURE FOR ALTITUDE, KPA:
          !prsr = 101.3 - 0.003215*Hru_elev_feet(i)
          prsr = 101.3 * (((293.0-0.0065*Hru_elev_meters(i))/293.0)**5.26)

!...LATENT HEAT OF VAPORIZATION AT AVG TEMPERATURE, CAL/GRAM:
          ! elh = 597.3 - 0.5653*Tavgc(i) ! same as potet_jh
!...LATENT HEAT OF VAPORIZATION AT AVG TEMPERATURE, JOULES/GRAM:
          elh = (597.3 - 0.5653*Tavgc(i)) * 4.184 
          ! elh = 2501.0 - 2.361*Tavgc(i)
          ! elh = 2500.8 - 2.36*Tavgc(i) + 0.0016*Tavgc(i)**2 - 0.00006*Tavgc(i)**3

!...PSCHOMETRIC CONSTANT AT AVG TEMPERATURE FOR ALTITUDE, KPA:
          ! psycnst = 1.6286*prsr/(elh*4.184) ! 4.184 converts CAL to JOULES
          ! psychrometric constant, kilopascals per degrees C
          ! atmospheric pressure for altitude, kPa
          ! Cp = 1.005 approximate specific heat capacity of air at 20 degrees C, increases with temp
          ! MW = 0.622 = molecular weight of water
          ! 1.615755627 = Cp / MW
          psycnst = 1.615755627*prsr/elh

!   heat flux density to the ground,  MJ / m2 / day
!          heat_flux = -4.2 * (Tavgc_ante(i)-Tavgc(i)) ! could use solrad_tmax or running avg instead of Tavgc_ante
          heat_flux = 0.0 ! Irmak and others (2012) says equal to zero for daily time step ! G

! Dew point temperature (Lawrence(2005) eqn. 8), degrees C
! Hru_humidity_sta is input as percent so divided by 100 to be in units of decimal fraction
          A1 = 17.625
          B1 = 243.04
          t1 = A1 * Tavgc(i) / (B1 + Tavgc(i))
          num = B1 * (LOG(Humidity(Hru_humidity_sta(i))/100.0) + t1)
          den = A1 - LOG(Humidity(Hru_humidity_sta(i))/100.0) - t1
          Tempc_dewpt(i) = num / den

! Actual vapor pressure (Irmak eqn. 12), KPA
! divide by 10 to convert millibar to kpa
          Vp_actual(i) = sat_vapor_press(Tempc_dewpt(i)) / 10.0 ! ea

!...SATURATION VAPOR PRESSURE AT AVG TEMP, KILOPASCALS (KPA):
! divide by 10 to convert millibar to kpa
          Vp_sat(i) = sat_vapor_press(Tavgc(i)) / 10.0

          ! saturation vapor pressure deficit
          vp_deficit = Vp_sat(i) - Vp_actual(i)

!...SLOPE OF SATURATION VAPOR PRESSURE CURVE AT AVG TEMP, KPA/DEG(CELSIUS), Irmak 2012, eqn 18
          Vp_slope(i) = 4098.0*Vp_sat(i)/((Tavgc(i)+237.3)**2) ! delta

! The long wave equation uses the soltab values in the denominator. There
! are cases when soltab is zero for certain HRUs (depending on slope/aspect)
! for certain months. If this value is zero, reset it to a small value so
! there is no divide by zero.
          IF (Soltab_potsw(Jday,i) <= 10.0) THEN
            stab = 10.0
          ELSE
            stab = Soltab_potsw(Jday,i)
          ENDIF

          IF (Swrad(i) <= 10.0) THEN
            sw = 10.5
          ELSE
            sw = Swrad(i)
          ENDIF

!
! Net long wave rediation (Irmak eqn. 10) MJ / m2/ day
! 4.903E-09 = Stefan-Boltzmann constant

           Lwrad_net(i) = 4.903E-09 * (((Tmaxc(i) + 273.16)**4 + (Tminc(i) + 273.16)**4)/2.0 ) &
      &                  * (0.34 - 0.14*(Vp_actual(i)**0.5)) * (((1.35*sw) / stab) - 0.35)

! Net radiation (Irmak eqn. 8) MJ / m2 / day
! 1 Langley = 0.04184 MJ/m2
          net_rad = Swrad(i)*0.04184 - Lwrad_net(i)

          a = Vp_slope(i) * (net_rad - heat_flux) / elh * 1000.0
          b = psycnst * Pm_n_coef(i,Nowmonth) * Wind_speed(Hru_windspeed_sta(i)) * vp_deficit / (Tavgc(i) + 273.0)
          c = (Vp_slope(i) + psycnst * (1.0 + Pm_d_coef(i,Nowmonth) * Wind_speed(Hru_windspeed_sta(i))))

!  PM equation with crop_coef in mm/day
!          Potet(i) = (a + b)/c
          Potet(i) = Crop_coef(i, Nowmonth) * (a + b)/c
          Potet(i) = Potet(i) / 25.4

! may be able to use intrinsic ISNAN
!          if (potet(i) .ne. potet(i)) then
!             print *, "potet NaN", potet(i)
!          end if

          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
!          Tavgc_ante(i) = Tavgc(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet = 'potet_pm_sta.f90 2017-01-23 11:27:00Z'
        CALL print_module(Version_potet, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_pm_sta'

        ! Declare Parameters
        ALLOCATE ( Pm_n_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'pm_n_coef', 'nhru,nmonths', 'real', &
     &       '900.0', '850.0', '950.0', &
     &       'Penman-Monteith coefficient', &
     &       'Monthly (January to December) Penman-Monteith potential ET N temperauture coefficient for each HRU', &
     &       'degrees Celsius per day')/=0 ) CALL read_error(1, 'pm_n_coef')

        ALLOCATE ( Pm_d_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'pm_d_coef', 'nhru,nmonths', 'real', &
     &       '0.34', '0.25', '0.45', &
     &       'Penman-Monteith coefficient', &
     &       'Monthly (January to December) Penman-Monteith potential ET D wind-speed coefficient for each HRU', &
     &       'seconds/meters')/=0 ) CALL read_error(1, 'pm_d_coef')

        ALLOCATE ( Crop_coef(Nhru,12) )
        IF ( declparam(MODNAME, 'crop_coef', 'nhru,nmonths', 'real', &
     &       '1.0', '0.0', '2.0', &
     &       'Crop coefficient for each HRU', &
     &       'Monthly (January to December) crop coefficient for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'crop_coef')

        ALLOCATE ( Hru_windspeed_sta(Nhru) )
        IF ( declparam(MODNAME, 'hru_windspeed_sta', 'nhru', 'integer', &
     &       '0', 'bounded', 'nwind', &
     &       'Index of wind speed measurement station for each HRU', &
     &       'Index of wind speed measurement station for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'hru_windspeed_sta')

        ALLOCATE ( Hru_humidity_sta(Nhru) )
        IF ( declparam(MODNAME, 'hru_humidity_sta', 'nhru', 'integer', &
     &       '0', 'bounded', 'nhumid', &
     &       'Index of humidity measurement station for each HRU', &
     &       'Index of humidity measurement station for each HRU', &
     &       'none')/=0 ) CALL read_error(1, 'hru_humidity_sta')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) THEN
          CALL potet_pm_restart(1)
        ELSE
          Vp_sat = 0.0
!          Tavgc_ante = Tavgc
        ENDIF
        IF ( getparam(MODNAME, 'pm_n_coef', Nhru*12, 'real', Pm_n_coef)/=0 ) CALL read_error(2, 'pm_n_coef')
        IF ( getparam(MODNAME, 'pm_d_coef', Nhru*12, 'real', Pm_d_coef)/=0 ) CALL read_error(2, 'pm_d_coef')
        IF ( getparam(MODNAME, 'crop_coef', Nhru*12, 'real', Crop_coef)/=0 ) CALL read_error(2, 'crop_coef')
        IF ( getparam(MODNAME, 'hru_windspeed_sta', Nhru, 'integer', Hru_windspeed_sta)/=0 ) CALL read_error(2,'hru_windspeed_sta')
        IF ( getparam(MODNAME, 'hru_humidity_sta', Nhru, 'integer', Hru_humidity_sta)/=0 ) CALL read_error(2, 'hru_humidity_sta')
        IF ( Parameter_check_flag>0 ) THEN
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            CALL checkdim_param_limits(i, 'hru_windspeed_sta', 'nhru', Hru_windspeed_sta(i), 1, Nwind, Inputerror_flag)
            CALL checkdim_param_limits(i, 'hru_humidity_sta', 'nhru', Hru_humidity_sta(i), 1, Nhumid, Inputerror_flag)
          ENDDO
        ENDIF

        !ALLOCATE ( Tavgc_ante(Nhru) )
        !Tavgc_ante = Tavgc

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_pm_restart(0)
      ENDIF

      END FUNCTION potet_pm_sta

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_pm_sta_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_PM_STA
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=12) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
!        WRITE ( Restart_outunit ) Tavgc_ante
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
!        READ ( Restart_inunit ) Tavgc_ante
      ENDIF
      END SUBROUTINE potet_pm_sta_restart
