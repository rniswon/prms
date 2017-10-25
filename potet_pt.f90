!***********************************************************************
! Computes the potential evapotranspiration using the Priestly-Taylor
! formulation (Jensen, 1990), modification of potet_dpm.f - Mastin
! Irmak, Suat, Kabenge, Isa, Skaggs, K.E., and Mutiibwa, Denis, 2012
!   Trend and magnitude of changes in climate variables and reference
!   evapotranspiration over 116-yr period in the Platte River Basin,
!   central Nebraska-USA: Journal of Hydrology, V. 420-421, p. 228-244
!***********************************************************************
      MODULE PRMS_POTET_PT
        IMPLICIT NONE
        ! Local Variables
        CHARACTER(LEN=8), SAVE :: MODNAME
!        REAL, SAVE, ALLOCATABLE :: Tavgc_ante(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Pt_alpha(:, :)
      END MODULE PRMS_POTET_PT

!***********************************************************************
      INTEGER FUNCTION potet_pt()
      USE PRMS_POTET_PT
      USE PRMS_MODULE, ONLY: Process, Nhru, Save_vars_to_file, Init_vars_from_file, Humidity_cbh_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv, Hru_elev_meters
      USE PRMS_CLIMATEVARS, ONLY: Basin_potet, Potet, Tavgc, Swrad, Tminc, Tmaxc, &
     &    Tempc_dewpt, Vp_actual, Lwrad_net, Vp_slope, Basin_humidity, Humidity_percent
      USE PRMS_CLIMATE_HRU, ONLY: Humidity_hru
      USE PRMS_SOLTAB, ONLY: Soltab_potsw
      USE PRMS_SET_TIME, ONLY: Nowmonth, Jday
      IMPLICIT NONE
! Functions
      INTRINSIC SQRT, DBLE, LOG, SNGL
      INTEGER, EXTERNAL :: declparam, getparam
      REAL, EXTERNAL :: sat_vapor_press
      EXTERNAL read_error, print_module, potet_pt_restart
! Local Variables
      INTEGER :: i, j
      REAL :: elh, satvapor, prsr, psycnst, ratio, eeq, heat_flux, net_rad
      REAL :: stab, A1, B1, t1, num, den, sw
      CHARACTER(LEN=80), SAVE :: Version_potet
!***********************************************************************
      potet_pt = 0

      IF ( Process(:3)=='run' ) THEN
!***********************************************************************
!******Compute "EQUIVALENT" EVAPOTRANSPIRATION, EEQ (IN./DAY),
!...USING PRIESTLY-TAYLOR METHOD. THE VARIBLES ARE CALCULATED
!...USING FORMULAS GIVEN IN JENSEN, 1990.
        IF ( Humidity_cbh_flag==0 ) Humidity_hru = Humidity_percent(1, Nowmonth) 
        Basin_potet = 0.0D0
        Basin_humidity = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)

!...SATURATION VAPOR PRESSURE AT AVG TEMP, KILOPASCALS (KPA):
! divide by 10 to convert millibar to kpa
          satvapor = sat_vapor_press(Tavgc(i)) / 10.0

!...SLOPE OF SATURATION VAPOR PRESSURE CURVE AT AVG TEMP, KPA/DEG(CELSIUS), Irmak 2012, eqn 18
          Vp_slope(i) = 4098.0*satvapor/((Tavgc(i)+237.3)**2) ! delta

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

!...RATIO USED IN PRIESTLY-TAYLOR FORMULA:
          ratio = Vp_slope(i)/(Vp_slope(i)+psycnst)

!   heat flux density to the ground,  MJ / m2 / day
!          heat_flux = -4.2 * (Tavgc_ante(i)-Tavgc(i)) ! could use solrad_tmax or running avg instead of Tavgc_ante
          heat_flux = 0.0 ! Irmak and others (2012) says equal to zero for daily time step ! G

! Dew point temperature (Irmak eqn. 13), degrees C
! Humidity_hru is input as percent so divided by 100 to be in units of decimal fraction
!          Tempc_dewpt(i) = 237.3 / (1.0/(LOG(Humidity_hru(i)/100.0)/17.26939) + (Tavgc(i)/237.3+Tavgc(i)))

! Dew point temperature (Lawrence(2005) eqn. 8), degrees C
          A1 = 17.625
          B1 = 243.04
          t1 = A1 * Tavgc(i) / (B1 + Tavgc(i))
          num = B1 * (LOG(Humidity_hru(i)/100.0) + t1) 
          den = A1 - LOG(Humidity_hru(i)/100.0) - t1 
          Tempc_dewpt(i) = num / den

! Actual vapor pressure (Irmak eqn. 12), KPA
! divide by 10 to convert millibar to kpa
!          actual_vapor_press=0.6108*EXP(17.27*dew_point_temp/(dew_point_temp+237.3))
          Vp_actual(i) = sat_vapor_press(Tempc_dewpt(i)) / 10.0 ! ea

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
!          net_rad = ((Swrad(i)*0.04184 - Lwrad_net(i)) / 0.2389) - heat_flux
          net_rad = Swrad(i)*0.04184 - Lwrad_net(i) - heat_flux
          
!...COMPUTE EEQ, CM/DAY
!...net_rad in units of MJ/m2
!...elh (LATENT HEAT OF VAPORIZATION) in units of JOULES/GRAM:
!  conversion: multiply by 10^6 to convert MJ to J
!  conversion: one gram of water is 1 cm3
!  conversion: 1 m2 is 10^4 cm2
!  eeq is in cm
          eeq = ratio*net_rad/elh * 100.0 ! if eeq<0, the Potet will be set to 0.0 below
!...CONVERT TO INCHES/DAY
          eeq = eeq/2.54

          Potet(i) = Pt_alpha(i, Nowmonth)*eeq
          IF ( Potet(i)<0.0 ) Potet(i) = 0.0
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
          Basin_humidity = Basin_humidity + DBLE( Humidity_hru(i)*Hru_area(i) )
!          Tavgc_ante(i) = Tavgc(i)
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
        Basin_humidity = Basin_humidity*Basin_area_inv

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_potet = 'potet_pt.f90 2017-10-24 12:24:00Z'
        CALL print_module(Version_potet, 'Potential Evapotranspiration', 90)
        MODNAME = 'potet_pt'

        ! Declare Parameters
        ALLOCATE ( Pt_alpha(Nhru,12) )
        IF ( declparam(MODNAME, 'pt_alpha', 'nhru,nmonths', 'real', &
     &       '1.26', '1.0', '2.0', &
     &       'Potential ET adjustment factor - Priestly-Taylor', &
     &       'Monthly (January to December) adjustment factor used in Priestly-Taylor potential ET computations for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'pt_alpha')

!******Get parameters
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( getparam(MODNAME, 'pt_alpha', Nhru*12, 'real', Pt_alpha)/=0 ) CALL read_error(2, 'pt_alpha')
        IF ( Init_vars_from_file==1 ) CALL potet_pt_restart(1)

        !ALLOCATE ( Tavgc_ante(Nhru) )
        !Tavgc_ante = Tavgc

      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL potet_pt_restart(0)
      ENDIF

      END FUNCTION potet_pt

!***********************************************************************
!     Write to or read from restart file
!***********************************************************************
      SUBROUTINE potet_pt_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_POTET_PT
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=8) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
!        WRITE ( Restart_outunit ) Tavgc_ante
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
!        READ ( Restart_inunit ) Tavgc_ante
      ENDIF
      END SUBROUTINE potet_pt_restart
