!***********************************************************************
! Defines shared watershed and HRU physical parameters and variables
!***********************************************************************
      MODULE PRMS_BASIN
      IMPLICIT NONE
!   Local Variables
      REAL, PARAMETER :: NEARZERO = 1.0E-6, INCH2CM = 2.54
      REAL, PARAMETER :: CLOSEZERO = 1.0E-09
      DOUBLE PRECISION, PARAMETER :: DNEARZERO = 1.0D-15, FT2_PER_ACRE = 43560.0D0
      DOUBLE PRECISION, PARAMETER :: CFS2CMS_CONV = 0.028316847D0
      REAL, PARAMETER :: INCH2MM = 25.4, INCH2M = 0.0254
      REAL, PARAMETER :: MM2INCH = 1.0/INCH2MM
      REAL, PARAMETER :: FEET2METERS = 0.3048
      REAL, PARAMETER :: METERS2FEET = 1.0/FEET2METERS
      CHARACTER(LEN=5), SAVE :: MODNAME
      INTEGER, SAVE :: Numlake_hrus, Active_hrus, Active_gwrs, Numlakes_check
      INTEGER, SAVE :: Hemisphere, Dprst_clos_flag
      DOUBLE PRECISION, SAVE :: Land_area, Water_area
      DOUBLE PRECISION, SAVE :: Basin_area_inv, Basin_lat, Totarea, Active_area
      REAL, SAVE, ALLOCATABLE :: Hru_elev_feet(:), Hru_elev_meters(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_clos(:)
      INTEGER, SAVE, ALLOCATABLE :: Gwr_type(:), Hru_route_order(:), Gwr_route_order(:)
      INTEGER, SAVE :: Weir_gate_flag
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_area_dble(:), Lake_area(:)
      CHARACTER(LEN=80), SAVE :: Version_basin
!   Declared Variables
      REAL, SAVE, ALLOCATABLE :: Hru_frac_perv(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_max(:)
      REAL, SAVE, ALLOCATABLE :: Hru_perv(:), Hru_imperv(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open_max(:), Dprst_area_clos_max(:)
!   Declared Parameters
      INTEGER, SAVE :: Elev_units
      INTEGER, SAVE, ALLOCATABLE :: Hru_type(:), Cov_type(:)
      INTEGER, SAVE, ALLOCATABLE :: Lake_hru_id(:), Lake_type(:) !not needed if no lakes
      REAL, SAVE, ALLOCATABLE :: Hru_area(:), Hru_percent_imperv(:), Hru_elev(:), Hru_lat(:)
      REAL, SAVE, ALLOCATABLE :: Covden_sum(:), Covden_win(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_frac_open(:), Dprst_frac(:), Dprst_area(:)
      END MODULE PRMS_BASIN

!***********************************************************************
!     Main basin routine
!***********************************************************************
      INTEGER FUNCTION basin()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basdecl, basinit
!***********************************************************************
      basin = 0

      IF ( Process(:4)=='decl' ) THEN
        basin = basdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        basin = basinit()
      ENDIF

      END FUNCTION basin

!***********************************************************************
!     basdecl - set up parameters
!   Declared Parameters
!     print_debug, hru_area, hru_percent_imperv, hru_type, hru_elev,
!     cov_type, hru_lat, dprst_frac_open, dprst_frac_hru, dprst_area, basin_area
!     lake_hru_id
!***********************************************************************
      INTEGER FUNCTION basdecl()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Model, Nhru, Nlake, Dprst_flag, Lake_route_flag, Numlakes, &
     &    Et_flag, Precip_flag, Cascadegw_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
!***********************************************************************
      basdecl = 0

      Version_basin = 'basin.f90 2016-11-15 14:56:00Z'
      CALL print_module(Version_basin, 'Basin Definition            ', 90)
      MODNAME = 'basin'

! Declared Variables
      ALLOCATE ( Hru_imperv(Nhru) )
      IF ( declvar(MODNAME, 'hru_imperv', 'nhru', Nhru, 'real', &
     &     'Area of HRU that is impervious', &
     &     'acres', Hru_imperv)/=0 ) CALL read_error(3, 'hru_imperv')

      ALLOCATE ( Hru_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_perv', 'nhru', Nhru, 'real', &
     &     'Area of HRU that is pervious', &
     &     'acres', Hru_perv)/=0 ) CALL read_error(3, 'hru_perv')

      ALLOCATE ( Hru_frac_perv(Nhru) )
      IF ( declvar(MODNAME, 'hru_frac_perv', 'nhru', Nhru, 'real', &
     &     'Fraction of HRU that is pervious', &
     &     'decimal fraction', Hru_frac_perv)/=0 ) CALL read_error(3, 'hru_frac_perv')

      IF ( Dprst_flag==1 .OR. Model==99 ) THEN
        ALLOCATE ( Dprst_area_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_max', 'nhru', Nhru, 'real', &
     &       'Aggregate sum of surface-depression storage areas of each HRU', &
     &       'acres', Dprst_area_max)/=0 ) CALL read_error(1, 'dprst_area_max')

        ALLOCATE ( Dprst_area_open_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_open_max', 'nhru', Nhru, 'real', &
     &       'Aggregate sum of open surface-depression storage areas of each HRU', &
     &       'acres', Dprst_area_open_max)/=0 ) CALL read_error(1, 'dprst_area_open_max')

        ALLOCATE ( Dprst_area_clos_max(Nhru) )
        IF ( declvar(MODNAME, 'dprst_area_clos_max', 'nhru', Nhru, 'real', &
     &       'Aggregate sum of closed surface-depression storage areas of each HRU', &
     &       'acres', Dprst_area_clos_max)/=0 ) CALL read_error(1, 'dprst_area_clos_max')

        ALLOCATE ( Dprst_area(Nhru) )
        IF ( declparam(MODNAME, 'dprst_area', 'nhru', 'real', &
     &       '0.0', '0.0', '1.0E9', &
     &       'Aggregate sum of surface-depression storage areas of each HRU', &
     &       'Aggregate sum of surface-depression storage areas of each HRU', &
     &       'acres')/=0 ) CALL read_error(1, 'dprst_area')

        ALLOCATE ( Dprst_frac(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_hru', 'nhru', 'real', &
     &       '-1.0', '-1.0', '0.999', &
     &       'Fraction of each HRU area that has surface depressions', &
     &       'Fraction of each HRU area that has surface depressions', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_hru')

        ALLOCATE ( Dprst_frac_open(Nhru), Dprst_frac_clos(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_open', 'nhru', 'real', &
     &       '1.0', '0.0', '1.0', &
     &       'Fraction of open surface-depression storage area within'// &
     &       ' an HRU that can generate surface runoff as a function of storage volume', &
     &       'Fraction of open surface-depression storage area within'// &
     &       ' an HRU that can generate surface runoff as a function of storage volume', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_open')
      ENDIF

      ! local arrays
      ALLOCATE ( Hru_route_order(Nhru) )
      IF ( Model/=0 .OR. Cascadegw_flag>0 ) ALLOCATE ( Gwr_route_order(Nhru), Gwr_type(Nhru) )
      IF ( Et_flag==5 .OR. Et_flag==11 .OR. Et_flag==6 ) ALLOCATE ( Hru_elev_feet(Nhru) )
      IF ( Precip_flag==5 ) ALLOCATE ( Hru_elev_meters(Nhru) )

      ! Declared Parameters
      ALLOCATE ( Hru_area(Nhru), Hru_area_dble(Nhru) )
      IF ( declparam(MODNAME, 'hru_area', 'nhru', 'real', &
     &     '1.0', '0.0001', '1.0E9', &
     &     'HRU area', 'Area of each HRU', &
     &     'acres')/=0 ) CALL read_error(1, 'hru_area')

      IF ( declparam(MODNAME, 'elev_units', 'one', 'integer', &
     &     '0', '0', '1', &
     &     'Elevation units flag', &
     &     'Flag to indicate the units of the elevation values (0=feet; 1=meters)', &
     &     'none')/=0 ) CALL read_error(1, 'elev_units')

      ALLOCATE ( Hru_elev(Nhru) )
      IF ( declparam(MODNAME, 'hru_elev', 'nhru', 'real', &
     &     '0.0', '-1000.0', '30000.0', &
     &     'HRU mean elevation', 'Mean elevation for each HRU', &
     &     'elev_units')/=0 ) CALL read_error(1, 'hru_elev')

      ALLOCATE ( Hru_lat(Nhru) )
      IF ( declparam(MODNAME, 'hru_lat', 'nhru', 'real', &
     &     '40.0', '-90.0', '90.0', &
     &     'HRU latitude', 'Latitude of each HRU', &
     &     'angular degrees')/=0 ) CALL read_error(1, 'hru_lat')

      ALLOCATE ( Hru_percent_imperv(Nhru) )
      IF ( declparam(MODNAME, 'hru_percent_imperv', 'nhru', 'real', &
     &     '0.0', '0.0', '0.999', &
     &     'HRU percent impervious', 'Fraction of each HRU area that is impervious', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'hru_percent_imperv')

      ALLOCATE ( Hru_type(Nhru) )
      IF ( declparam(MODNAME, 'hru_type', 'nhru', 'integer', &
     &     '1', '0', '3', &
     &     'HRU type', 'Type of each HRU (0=inactive; 1=land; 2=lake; 3=swale)', &
     &     'none')/=0 ) CALL read_error(1, 'hru_type')

      ALLOCATE ( Cov_type(Nhru) )
      IF ( declparam(MODNAME, 'cov_type', 'nhru', 'integer', &
     &     '3', '0', '4', &
     &     'Cover type designation for HRU', &
     &     'Vegetation cover type for each HRU (0=bare soil;'// &
     &     ' 1=grasses; 2=shrubs; 3=trees; 4=coniferous)', &
     &     'none')/=0 ) CALL read_error(1, 'cov_type')

      ALLOCATE ( Covden_sum(Nhru) )
      IF ( declparam(MODNAME, 'covden_sum', 'nhru', 'real', &
     &     '0.5', '0.0', '1.0', &
     &     'Summer vegetation cover density for major vegetation type', &
     &     'Summer vegetation cover density for the major vegetation type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'covden_sum')

      ALLOCATE ( Covden_win(Nhru) )
      IF ( declparam(MODNAME, 'covden_win', 'nhru', 'real', &
     &     '0.5', '0.0', '1.0', &
     &     'Winter vegetation cover density for major vegetation type', &
     &     'Winter vegetation cover density for the major vegetation type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'covden_win')

      IF ( Nlake>0 .OR. Model==99 ) THEN
        ! Local array
        ALLOCATE ( Lake_area(Numlakes) ) ! lake area is for each lake, but, nlake is number of lake HRUs
        ! parameters
        ALLOCATE ( Lake_hru_id(Nhru) )
        IF ( declparam(MODNAME, 'lake_hru_id', 'nhru', 'integer', &
     &       '0', 'bounded', 'numlakes', & ! nlake is number of lake HRUs, numlakes the number of lakes
     &       'Identification number of the lake associated with an HRU', &
     &       'Identification number of the lake associated with an HRU;'// &
     &       ' more than one HRU can be associated with each lake', &
     &       'none')/=0 ) CALL read_error(1, 'lake_hru_id')
        IF ( (Lake_route_flag==1 .AND. Model/=0) .OR. Model==99 ) THEN
          ALLOCATE ( Lake_type(Nlake) )
          IF ( declparam(MODNAME, 'lake_type', 'nlake', 'integer', &
     &         '1', '1', '6', &
     &         'Type of lake routing method', &
     &         'Type of lake routing method (1=Puls routing; 2=linear routing; 3=flow through;'// &
     &         ' 4=broad crested weir; 5=gate opening; 6=measured flow)', &
     &         'none')/=0 ) CALL read_error(1, 'lake_type')
        ENDIF
      ENDIF

      END FUNCTION basdecl

!**********************************************************************
!     basinit - check for validity of basin parameters
!               and compute reservoir areas
!**********************************************************************
      INTEGER FUNCTION basinit()
      USE PRMS_BASIN
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Dprst_flag, &
     &    Print_debug, Inputerror_flag, Model, PRMS_VERSION, Starttime, Endtime, &
     &    Lake_route_flag, Et_flag, Precip_flag, Cascadegw_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL write_outfile
      INTRINSIC ABS, DBLE, SNGL
! Local Variables
      CHARACTER(LEN=68) :: buffer
      INTEGER :: i, j, lakeid, dprst_frac_flag
      REAL :: harea
      DOUBLE PRECISION :: basin_imperv, basin_perv, basin_dprst, harea_dble
!**********************************************************************
      basinit = 0

      IF ( getparam(MODNAME, 'hru_area', Nhru, 'real', Hru_area)/=0 ) CALL read_error(2, 'hru_area')
      IF ( getparam(MODNAME, 'hru_elev', Nhru, 'real', Hru_elev)/=0 ) CALL read_error(2, 'hru_elev')
      IF ( getparam(MODNAME, 'hru_lat', Nhru, 'real', Hru_lat)/=0 ) CALL read_error(2, 'hru_lat')
      IF ( getparam(MODNAME, 'hru_type', Nhru, 'integer', Hru_type)/=0 ) CALL read_error(2, 'hru_type')
      IF ( getparam(MODNAME, 'cov_type', Nhru, 'integer', Cov_type)/=0 ) CALL read_error(2, 'cov_type')
      IF ( getparam(MODNAME, 'covden_sum', Nhru, 'real', Covden_sum)/=0 ) CALL read_error(2, 'covden_sum')
      IF ( getparam(MODNAME, 'covden_win', Nhru, 'real', Covden_win)/=0 ) CALL read_error(2, 'covden_win')
      IF ( getparam(MODNAME, 'elev_units', 1, 'integer', Elev_units)/=0 ) CALL read_error(2, 'elev_units')
      IF ( getparam(MODNAME, 'hru_percent_imperv', Nhru, 'real', Hru_percent_imperv)/=0 ) CALL read_error(2, 'hru_percent_imperv')

      dprst_frac_flag = 0
      IF ( Dprst_flag==1 ) THEN
        IF ( getparam(MODNAME, 'dprst_frac_open', Nhru, 'real', Dprst_frac_open)/=0 ) CALL read_error(2, 'dprst_frac_open')
        IF ( getparam(MODNAME, 'dprst_area', Nhru, 'real', Dprst_area)/=0 ) CALL read_error(2, 'dprst_area')
        IF ( getparam(MODNAME, 'dprst_frac_hru', Nhru, 'real', Dprst_frac)/=0 ) CALL read_error(2, 'dprst_frac_hru')
        IF ( Dprst_frac(1)>-1.0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Using dprst_frac_hru instead of dprst_area'
          dprst_frac_flag = 1
        ENDIF
      ENDIF

      IF ( Nlake>0 ) THEN
        IF ( getparam(MODNAME, 'lake_hru_id', Nhru, 'integer', Lake_hru_id)/=0 ) CALL read_error(1, 'lake_hru_id')
        Lake_area = 0.0D0
        IF ( Lake_route_flag==1 .AND. Model/=0 ) THEN
          IF ( getparam(MODNAME, 'lake_type', Nlake, 'integer', Lake_type)/=0 ) CALL read_error(2, 'lake_type')
        ENDIF
      ENDIF

      IF ( Dprst_flag==1 ) THEN
        Dprst_frac_clos = 0.0
        Dprst_area_open_max = 0.0
        Dprst_area_clos_max = 0.0
        Dprst_area_max = 0.0
      ENDIF
      Dprst_clos_flag = 0
      basin_perv = 0.0D0
      basin_imperv = 0.0D0
      basin_dprst = 0.0D0
      Numlakes_check = 0
      Numlake_hrus = 0
      Totarea = 0.0D0
      Land_area = 0.0D0
      Water_area = 0.0D0
      Active_area = 0.0D0
      Basin_lat = 0.0D0
      Hru_route_order = 0
      j = 0
      DO i = 1, Nhru
        harea = Hru_area(i)
        harea_dble = DBLE( harea )
        Hru_area_dble(i) = harea_dble
        Totarea = Totarea + harea_dble

        IF ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) THEN ! inactive or lakes
          Hru_frac_perv(i) = 1.0
          Hru_imperv(i) = 0.0
          Hru_perv(i) = harea
          IF ( Hru_type(i)==0 ) CYCLE
          Water_area = Water_area + harea_dble
          Numlake_hrus = Numlake_hrus + 1
          lakeid = Lake_hru_id(i)
          IF ( lakeid>Numlakes_check ) Numlakes_check = lakeid
          IF ( lakeid>0 ) THEN
             Lake_area(lakeid) = Lake_area(lakeid) + harea_dble
          ELSE
            PRINT *, 'ERROR, hru_type = 2 for HRU:', i, ' and lake_hru_id = 0'
            Inputerror_flag = 1
          ENDIF
          IF ( Nlake==0 ) THEN
            PRINT *, 'ERROR, hru_type = 2 for HRU:', i, ' and dimension nlake = 0'
            Inputerror_flag = 1
          ENDIF
        ELSE
          Land_area = Land_area + harea_dble ! swale or land
          IF ( Nlake>0 ) THEN
            IF ( Lake_hru_id(i)>0 .AND. Hru_type(i)/=2 ) THEN
              PRINT *, 'ERROR, HRU:', i, ' specifed to be a lake by lake_hru_id but hru_type not equal 2'
              Inputerror_flag = 1
            ENDIF
          ENDIF
        ENDIF

        Basin_lat = Basin_lat + DBLE( Hru_lat(i)*harea )
        IF ( Elev_units==0 ) THEN
          IF ( Et_flag==5 .OR. Et_flag==11 .OR. Et_flag==6 ) Hru_elev_feet(i) = Hru_elev(i)
          IF ( Precip_flag==5 ) Hru_elev_meters(i) = Hru_elev(i)*FEET2METERS
        ELSE
          IF ( Precip_flag==5 ) Hru_elev_meters(i) = Hru_elev(i)
          IF ( Et_flag==5 .OR. Et_flag==11 .OR. Et_flag==6 ) Hru_elev_feet(i) = Hru_elev(i)*METERS2FEET
        ENDIF
        j = j + 1
        Hru_route_order(j) = i

        IF ( Hru_type(i)==2 ) CYCLE ! lake

        Hru_imperv(i) = Hru_percent_imperv(i)*harea
        Hru_perv(i) = harea - Hru_imperv(i)

        IF ( Dprst_flag==1 ) THEN
          IF ( dprst_frac_flag==1 ) THEN
            Dprst_area_max(i) = Dprst_frac(i)*harea
          ELSE
            IF ( Dprst_area(i)>harea ) THEN
              PRINT *, 'ERROR, dprst_area > hru_area for HRU:', i, ', value:', Dprst_area(i)
              PRINT *, '       hru_area:', harea
              Inputerror_flag = 1
            ELSEIF ( Dprst_area(i)>0.999*harea ) THEN
              PRINT *, 'ERROR, dprst_area > 0.999*hru_area for HRU:', i, ', value:', Dprst_area(i)
              PRINT *, '       hru_area:', harea, '; fraction', 0.999*harea
              Inputerror_flag = 1
            ENDIF
            Dprst_area_max(i) = Dprst_area(i)
            Dprst_frac(i) = Dprst_area_max(i)/harea
          ENDIF
          IF ( Dprst_area_max(i)>0.0 ) THEN
            Dprst_area_open_max(i) = Dprst_area_max(i)*Dprst_frac_open(i)
            Dprst_frac_clos(i) = 1.0 - Dprst_frac_open(i)
            Dprst_area_clos_max(i) = Dprst_area_max(i) - Dprst_area_open_max(i)
            IF ( Hru_percent_imperv(i)+Dprst_frac(i)>0.999 ) THEN
              PRINT *, 'ERROR, impervious plus depression fraction > 0.999 for HRU:', i
              PRINT *, '       value:', Hru_percent_imperv(i) + Dprst_frac(i)
              Inputerror_flag = 1
            ENDIF
            IF ( Dprst_area_open_max(i)>0.0 ) Dprst_frac_open(i) = Dprst_area_max(i)/Dprst_area_open_max(i)
            Hru_perv(i) = harea - Hru_imperv(i) - Dprst_area_max(i)
          ENDIF
        ENDIF

        Hru_frac_perv(i) = Hru_perv(i)/harea
        basin_perv = basin_perv + DBLE( Hru_perv(i) )
        basin_imperv = basin_imperv + DBLE( Hru_imperv(i) )
        IF ( Dprst_flag==1 ) THEN
          basin_dprst = basin_dprst + DBLE( Dprst_area_max(i) )
          IF ( Dprst_area_clos_max(i)>0.0 ) Dprst_clos_flag = 1
        ENDIF
      ENDDO

      Active_hrus = j
      Active_area = Land_area + Water_area

      IF ( Model/=0 .OR. Cascadegw_flag>0 ) THEN
        Active_gwrs = Active_hrus
        Gwr_type = Hru_type
        Gwr_route_order = Hru_route_order
      ENDIF

      Weir_gate_flag = 0
! ????????? need to fix for lakes with multiple HRUs and PRMS lake routing ????????
      IF ( Model==99 ) Numlake_hrus = Nlake
      IF ( Nlake<1 .AND. Numlake_hrus>0 ) THEN
        PRINT *, 'ERROR, dimension nlake=0 and number of specified lake HRUs equals', Numlake_hrus
        Inputerror_flag = 1
      ELSEIF ( Numlake_hrus/=Nlake ) THEN
        PRINT *, 'ERROR, number of lake HRUs specified in hru_type'
        PRINT *, 'does not equal dimension nlake:', Nlake, ', number of lake HRUs:', Numlake_hrus
        Inputerror_flag = 1
      ENDIF
      IF ( Nlake>0 ) THEN
        DO i = 1, Numlakes_check
          IF ( Lake_area(i)<DNEARZERO ) THEN
            PRINT *, 'ERROR, Lake:', i, ' has 0 area, thus no value of lake_hru_id is associated with the lake'
            Inputerror_flag = 1
          ENDIF
        ENDDO
        IF ( Model/=0 ) THEN
          IF ( Numlakes_check/=Nlake ) THEN
            PRINT *, 'WARNING, number of lakes specified in lake_hru_id'
            PRINT *, 'does not equal dimension nlake:', Nlake, ', number of lakes:', Numlakes_check
!            Inputerror_flag = 1 ! make warning for now to allow PRMS-only with multiple HRU lakes
          ENDIF
          DO i = 1, Nlake ! maybe lake_type is per lake instead of each lake ???
            IF ( Lake_route_flag==1 ) THEN
              IF ( Lake_type(i)==4 .OR. Lake_type(i)==5 ) Weir_gate_flag = 1
            ENDIF
          ENDDO
        ENDIF
      ENDIF

      Basin_area_inv = 1.0D0/Active_area
      Basin_lat = Basin_lat*Basin_area_inv
      ! used in solrad modules to winter/summer radiation adjustment
      IF ( Basin_lat>0.0D0 ) THEN
        Hemisphere = 0 ! Northern
      ELSE
        Hemisphere = 1 ! Southern
      ENDIF

      basin_perv = basin_perv*Basin_area_inv
      basin_imperv = basin_imperv*Basin_area_inv
      IF ( Dprst_flag==1 ) basin_dprst = basin_dprst*Basin_area_inv

      IF ( Print_debug==2 ) THEN
        PRINT *, ' HRU     Area'
        PRINT ('(I7, F14.5)'), (i, Hru_area(i), i=1, Nhru)
        PRINT *, 'Sum of HRU areas      = ', Totarea
        PRINT *, 'Active basin area     = ', Active_area
        PRINT *, 'Fraction impervious   = ', basin_imperv
        PRINT *, 'Fraction pervious     = ', basin_perv
        IF ( Dprst_flag==1 ) PRINT *, 'Fraction depression storage =', basin_dprst
        PRINT *, ' '
      ENDIF

!     print out start and end times
      IF ( Print_debug>-2 ) THEN
        CALL write_outfile('  ')
        !CALL write_outfile(' Surface Water and Energy Budgets Simulated by '//PRMS_VERSION)
        WRITE (buffer, 9002) ' Start time: ', Starttime
        CALL write_outfile(buffer(:32))
        WRITE (buffer, 9002) ' End time:   ', Endtime
        CALL write_outfile(buffer(:32))
        WRITE (buffer, 9003) ' Sum of HRU areas:', Totarea, '; Active basin area:', Active_area
        CALL write_outfile(buffer(:62))
        WRITE (buffer, 9004) ' Fraction impervious:', basin_imperv, '; Fraction pervious:   ', basin_perv
        CALL write_outfile(buffer(:62))
        IF ( Dprst_flag==1 ) THEN
          WRITE (buffer, 9004) ' Fraction depression storage:', basin_dprst
          CALL write_outfile(buffer(:45))
        ENDIF
        CALL write_outfile(' ')
      ENDIF

 9002 FORMAT (A, I4.2, 2('/', I2.2), I3.2, 2(':', I2.2))
 9003 FORMAT (2(A,F12.2))
 9004 FORMAT (2(A,F9.4))

      END FUNCTION basinit
