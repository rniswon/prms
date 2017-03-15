    MODULE PRMS_READ_PARAM_FILE
        USE PRMS_MODULE, ONLY: Print_debug, MAXFILE_LENGTH, EQULS
        INTEGER, SAVE :: Num_parameters, Num_dimensions, Param_unit, Read_parameters
        TYPE PRMS_parameter
             CHARACTER(LEN=32) :: param_name
             CHARACTER(LEN=312) :: short_description, long_description
             INTEGER :: numvals, data_flag, decl_flag, read_flag, param_name_nchars, id_num
             DOUBLE PRECISION :: default_value
             CHARACTER(LEN=12) :: max_value, min_value, def_value, data_type
             CHARACTER(LEN=12) :: dimen_names, module_name, units
             DOUBLE PRECISION, POINTER :: values(:)
             REAL :: maxval, minval
        END TYPE PRMS_parameter
        TYPE ( PRMS_parameter ), SAVE, ALLOCATABLE :: Parameter_data(:)
        TYPE PRMS_dimension
             CHARACTER(LEN=16), POINTER :: name(:)
             INTEGER, POINTER :: numvals(:)
             CHARACTER(LEN=MAXFILE_LENGTH) :: short_description
        END TYPE PRMS_dimension
      END MODULE PRMS_READ_PARAM_FILE

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
      SUBROUTINE read_parameter_file_dimens
      USE PRMS_READ_PARAM_FILE
      USE PRMS_MODULE, ONLY: Version_read_parameter_file, Param_file 
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      EXTERNAL read_error, PRMS_open_input_file, write_outfile, set_dimension, print_module
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      CHARACTER(LEN=16) :: string, dimname
      CHARACTER(LEN=80) :: line
      CHARACTER(LEN=24) :: dimstring
      INTEGER nchars, ios, dimen_value
!***********************************************************************
      Version_read_parameter_file = 'read_parameter_file.f90 2017-03-11 12:12:00Z'

      CALL PRMS_open_input_file(Param_unit, param_file, 'param_file', 0, ios)
      IF ( ios/=0 ) STOP
      CALL write_outfile(EQULS)
      CALL write_outfile('Using PRMS Parameter File: '//param_file)

! Echo Parmeter File Header and comment lines
      READ ( Param_unit, FMT='(A)', IOSTAT=ios ) line
      IF ( ios/=0 ) CALL read_error(13, 'description')
      CALL write_outfile('Description: '//TRIM(line))
      CALL write_outfile(EQULS)
      CALL write_outfile('Comment lines:')

! Find start of dimensions section
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) line
        IF ( ios==-1 ) CALL read_error(13, 'end of file found before dimensions')
        IF ( ios/=0 ) CALL read_error(13, 'comment')
        IF ( line(:16)=='** Dimensions **' ) EXIT
        CALL write_outfile(TRIM(line))
      ENDDO
      IF ( line(:16)/='** Dimensions **' ) CALL read_error(11, 'missing dimension section: '//TRIM(line))
      CALL write_outfile(EQULS)
      CALL write_outfile('Using dimensions    number')

! Read all dimensions
      Num_dimensions = 0
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) CALL read_error(13, 'end of file found before parameter section')
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension #### delimiter')
        IF ( string(:4)=='    ' ) CYCLE
        IF ( string(:2)=='//' ) CYCLE
        IF ( string=='** Parameters **' ) EXIT ! stop reading if end of dimensions section
        !IF ( string(:4)/='####' ) CALL read_error(11, 'missing dimension #### delimiter '//string)
        IF ( string(:4)/='####' ) THEN
          PRINT *, 'Warning, ignoring dimension line: ', string
          CYCLE
        ENDIF
        READ ( Param_unit, *, IOSTAT=ios ) dimname
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension name: '//dimname)
        READ ( Param_unit, *, IOSTAT=ios ) dimen_value
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension value')
        CALL set_dimension(dimname, dimen_value)
        nchars = numchars(dimname)
        IF ( dimen_value==0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, dimension: ', dimname(:nchars), ' is not needed as value specified as 0'
        ELSEIF ( dimen_value==-1 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, dimension: ', dimname(:nchars), ' is not needed in this PRMS version'
        ELSE
          WRITE ( dimstring, '(A,I8)' ) dimname, dimen_value
          CALL write_outfile(dimstring)
          Num_dimensions = Num_dimensions + 1
        ENDIF
      ENDDO
      CALL write_outfile(EQULS)
      END SUBROUTINE read_parameter_file_dimens

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
      SUBROUTINE read_parameter_file_params
      USE PRMS_READ_PARAM_FILE
      IMPLICIT NONE
      ! Functions
      EXTERNAL read_error, PRMS_open_input_file, set_dimension
      INTEGER, EXTERNAL :: control_string, numchars
      ! Local Variables
      CHARACTER(LEN=16) :: string
      CHARACTER(LEN=32) :: paramstring
      CHARACTER(LEN=12) :: dim_string(2)
      INTEGER nchars, ios, num_dims, num_param_values, i, j, param_type, num, inum
      DOUBLE PRECISION, ALLOCATABLE :: dmy(:)
      !***********************************************************************
! Find parameter section
      REWIND ( Param_unit )
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) CALL read_error(11, 'end of file found before parameter section') ! found end of Parameter File
        IF ( string(:16)=='** Parameters **' ) EXIT ! stop reading if end of dimensions section
      ENDDO
! Read all parameters and verify
      Read_parameters = 0
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of Parameter File
        IF ( ios/=0 ) CALL read_error(11, 'missing parameter #### delimiter')
        IF ( string(:4)=='    ' ) CYCLE ! skip blank lines
        IF ( string(:2)=='//' ) CYCLE ! skip comment lines
        !IF ( string(:4)/='####' ) CALL read_error(11, 'missing parameter #### delimiter')
        IF ( string(:4)/='####' ) CYCLE
        READ ( Param_unit, '(A)', IOSTAT=ios ) paramstring ! parameter name
        IF ( ios/=0 ) CALL read_error(11, 'missing parameter name')
        nchars = numchars(paramstring)
        READ ( Param_unit, *, IOSTAT=ios ) num_dims
        IF ( ios/=0 ) CALL read_error(11, 'invalid number of dimensions: '//paramstring(:nchars))
        IF ( num_dims>2 ) CALL read_error(11, 'number of dimensions > 3: '//paramstring(:nchars))
        num = 1
        DO i = 1, num_dims
          READ ( Param_unit, '(A)', IOSTAT=ios ) dim_string(i)
          IF ( ios/=0 ) CALL read_error(11, 'invalid dimension for parameter: '//paramstring(:nchars))
          inum = -2 ! get dimension value
          CALL set_dimension(dim_string(i), inum)
          num = num*inum
        ENDDO
        READ ( Param_unit, *, IOSTAT=ios ) num_param_values
        IF ( num/=num_param_values ) CALL read_error(11, 'invalid number of parameter values based on specified dimensions '//paramstring(:nchars))
        IF ( ios/=0 ) CALL read_error(11, 'invalid number of parameter values: '//paramstring(:nchars))
        READ ( Param_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(11, 'invalid parameter type '//paramstring(:nchars))
        IF ( param_type<1 .OR. param_type>3 ) CALL read_error(11, 'invalid parameter type: '//paramstring(:nchars))
        ALLOCATE ( dmy(num_param_values) )
        READ ( Param_unit, *, IOSTAT=ios ) (dmy(j),j=1,num_param_values)
        IF ( ios/=0 ) CALL read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
        DEALLOCATE ( dmy )
        Read_parameters = Read_parameters + 1
      ENDDO
      Num_parameters = Read_parameters ! allow for 25 extra parameters not declared, must check in declare
      ALLOCATE ( Parameter_data(Num_parameters+25) )
      DO i = 1, Num_parameters + 25
        Parameter_data(i)%id_num = 0
        Parameter_data(i)%read_flag = 0
        Parameter_data(i)%decl_flag = 0
        Parameter_data(i)%numvals = 0
        Parameter_data(i)%data_flag = 0
        Parameter_data(i)%param_name_nchars = 0
      ENDDO
      
! allocate and store parameter data
      REWIND ( Param_unit )
      DO
        READ ( Param_unit, '(A)' ) string
        IF ( string(:16)=='** Parameters **' ) EXIT ! stop reading if end of dimensions section
      ENDDO

      DO i = 1, Read_parameters
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of Parameter File
        IF ( ios/=0 ) CALL read_error(11, 'missing parameter #### delimiter')
        IF ( string(:4)=='    ' ) CYCLE ! skip blank lines
        IF ( string(:2)=='//' ) CYCLE ! skip comment lines
        IF ( string(:4)/='####' ) CALL read_error(11, 'missing parameter #### delimiter')
        READ ( Param_unit, '(A)' ) paramstring ! parameter name
        nchars = numchars(paramstring)
        READ ( Param_unit, * ) num_dims
        IF ( num_dims>3 ) CALL read_error(11, 'number of dimensions > 3'//paramstring)
        DO j = 1, num_dims
          READ ( Param_unit, '(A)' ) dim_string(j)
        ENDDO
        READ ( Param_unit, * ) num_param_values
        READ ( Param_unit, * ) param_type
        ALLOCATE ( Parameter_data(i)%values(num_param_values) )
        READ ( Param_unit, * ) (Parameter_data(i)%values(j),j=1,num_param_values)
        Parameter_data(i)%read_flag = 1
        Parameter_data(i)%param_name = paramstring(:nchars)
        Parameter_data(i)%param_name_nchars = nchars
        Parameter_data(i)%data_flag = param_type
        Parameter_data(i)%numvals = num_param_values
        Parameter_data(i)%id_num = i
      ENDDO
      CLOSE ( param_unit )
      END SUBROUTINE read_parameter_file_params

!***********************************************************************
! set_dimension - Set dimension value based on name and number
!***********************************************************************
      SUBROUTINE set_dimension(Dimname, Number)
      USE PRMS_MODULE, ONLY: Nhru, Nssr, Ngw, Nsub, Npoigages, Ndepl, Nlake, Ncascade, Ncascdgw, &
     &    Nsegment, Nlake, Ndepl, Nratetbl, Numlakes, Nobs, Nevap, Nwateruse, Nexternal, Nconsumed, &
     &    One, Nmonths, Ndays, Nlapse, Npoigages, Ndeplval, Nhrucell, Ntemp, Nrain, Nsol, Ngwcell
      USE PRMS_OBS, ONLY: Nsnow, Nlakeelev, Nwind, Nhumid
      USE PRMS_MUSKINGUM_LAKE, ONLY: Ngate, Ngate2, Ngate3, Ngate4, Nstage, Nstage2, Nstage3, Nstage4, Mxnsos
      USE GSFBUDGET, ONLY: Nreach
      IMPLICIT NONE
      ! Functions
      INTRINSIC LEN_TRIM
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Dimname
      INTEGER, INTENT(INOUT) :: Number
      ! Local Variables
      INTEGER return_value, string_length, found
!***********************************************************************
      return_value = 0
      IF ( Number==-2 ) return_value = 1 ! return dimension value, either default or value in Paramter File, getdim
      ! else set dimension value to value in Parameter File
      One = 1
      Nmonths = 12
      Ndays = 366

      found = 0
      string_length = LEN_TRIM(Dimname)

      IF ( string_length==3 ) THEN
        IF ( Dimname(:3)=='ngw' ) THEN ! number of groundwater reservoirs
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ngw
          ELSE
            Ngw = Number
          ENDIF
        ELSEIF ( Dimname(:3)=='one' ) THEN
          found = 1
          IF ( return_value==1 ) THEN
            Number = 1
          ELSE
            One = 1
          ENDIF
        ENDIF
      ELSEIF ( string_length==4 ) THEN
        IF ( Dimname(:4)=='nhru' ) THEN ! number of HRUs
          found = 1
          IF ( return_value==1 ) THEN ! getdim
            Number = Nhru
          ELSE ! set to value in Parameter File
            Nhru = Number
          ENDIF
        ELSEIF ( Dimname(:4)=='nssr' ) THEN ! number of gravity reservoirs
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nssr
          ELSE
            Nssr = Number
          ENDIF
        ELSEIF ( Dimname(:4)=='nobs' ) THEN ! number of streamflow columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nobs
          ELSE
            Nobs = Number
          ENDIF
        ELSEIF ( Dimname(:4)=='nsol' ) THEN ! number of solar radiation columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nsol
          ELSE
            Nsol = Number
          ENDIF
        ELSEIF ( Dimname(:4)=='nsub' ) THEN ! number of subbasins
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nsub
          ELSE
            Nsub = Number
          ENDIF
        ENDIF
      ELSEIF ( string_length==5 ) THEN
        IF ( Dimname(:5)=='nwind' ) THEN ! number of wind speed columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nwind
          ELSE
            Nwind = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='ndays' ) THEN
          found = 1
         IF ( return_value==1 ) THEN
            Number = 366
          ELSE
            Ndays = 366
          ENDIF
        ELSEIF ( Dimname(:5)=='ntemp' ) THEN ! number of maximum and minimum air temperature columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ntemp
          ELSE
            Ntemp = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='nrain' ) THEN ! number of precipitation columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nrain
          ELSE
            Nrain = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='nsnow' ) THEN ! number of snow pack columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nsnow
          ELSE
            Nsnow = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='nevap' ) THEN ! number of pan evaporation columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nevap
          ELSE
            Nevap = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='ndepl' ) THEN
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ndepl
          ELSE
            Ndepl = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='ngate' ) THEN ! number of rating tables for lake 1 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ngate
          ELSE
            Ngate = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='ngate2' ) THEN ! number of rating tables for lake 2 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ngate2
          ELSE
            Ngate2 = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='ngate3' ) THEN ! number of rating tables for lake 3 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ngate3
          ELSE
            Ngate3 = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='ngate4' ) THEN ! number of rating tables for lake 4 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ngate4
          ELSE
            Ngate4 = Number
          ENDIF
        ELSEIF ( Dimname(:5)=='nlake' ) THEN ! number of lake HRUs
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nlake
          ELSE
            Nlake = Number
          ENDIF
        ENDIF
      ELSEIF ( string_length==6 ) THEN
        IF ( Dimname(:6)=='mxnsos' ) THEN ! number of rating tables for lakes using Puls routing
          found = 1
          IF ( return_value==1 ) THEN
            Number = Mxnsos
          ELSE
            Mxnsos = Number
          ENDIF
        ELSEIF ( Dimname(:6)=='nstage4' ) THEN ! number of rating tables for lake 4 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nstage4
          ELSE
            Nstage4 = Number
          ENDIF
        ELSEIF ( Dimname(:6)=='nstage3' ) THEN ! number of rating tables for lake 3 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nstage3
          ELSE
            Nstage3 = Number
          ENDIF
        ELSEIF ( Dimname(:6)=='nstage2' ) THEN ! number of rating tables for lake 2 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nstage2
          ELSE
            Nstage2 = Number
          ENDIF
        ELSEIF ( Dimname(:6)=='nstage' ) THEN ! number of rating tables for lake 1 with gates
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nstage
          ELSE
            Nstage = Number
          ENDIF
        ELSEIF ( Dimname(:6)=='nhumid' ) THEN ! number of humidity columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nhumid
          ELSE
            Nhumid = Number
          ENDIF
        ELSEIF ( Dimname(:6)=='nlapse' ) THEN ! number of lapse rates for xyz, declfix
          found = 1
          IF ( return_value==1 ) THEN
            Number = 3
          ELSE
            Nlapse = 3
          ENDIF
        ELSEIF ( Dimname(:6)=='nreach' ) THEN
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nreach
          ELSE
            Nreach = Number
          ENDIF
        ENDIF
      ELSEIF ( string_length==7 ) THEN
        IF ( Dimname(:7)=='nmonths' ) THEN
          found = 1
          IF ( return_value==1 ) THEN
            Number = 12
          ELSE
            Nmonths = 12
          ENDIF
        ELSEIF ( Dimname(:7)=='ngwcell' ) THEN ! number of spatial elements in target map for map_results
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ngwcell
          ELSE
            Ngwcell = Number
          ENDIF
        ENDIF
      ELSEIF ( string_length==8 ) THEN
        IF ( Dimname(:8)=='nhrucell' ) THEN ! number of intersections betweern HRU and spatial elements in target map for map_results
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nhrucell
          ELSE
            Nhrucell = Number
          ENDIF
        ELSEIF ( Dimname(:8)=='ndeplval' ) THEN
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ndeplval
          ELSE
            Ndeplval = Number
          ENDIF
        ELSEIF ( Dimname(:8)=='nsegment' ) THEN ! number of stream segments
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nsegment
          ELSE
            Nsegment = Number
          ENDIF
        ELSEIF ( Dimname(:8)=='nratetbl' ) THEN ! number of gate opening columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nratetbl
          ELSE
            Nratetbl = Number
          ENDIF
        ELSEIF ( Dimname(:8)=='ncascade' ) THEN ! number of HRU cascades
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ncascade
          ELSE
            Ncascade = Number
          ENDIF
        ELSEIF ( Dimname(:8)=='ncascdgw' ) THEN ! number of GWR cascades
          found = 1
          IF ( return_value==1 ) THEN
            Number = Ncascdgw
          ELSE
            Ncascdgw = Number
          ENDIF
        ELSEIF ( Dimname(:8)=='numlakes' ) THEN
          found = 1
          IF ( return_value==1 ) THEN
            Number = Numlakes
          ELSE
            Numlakes = Number
          ENDIF
        ENDIF
      ELSEIF ( string_length==9 ) THEN
        IF ( Dimname(:9)=='npoigages' ) THEN ! number of gages in Lumen
          found = 1
          IF ( return_value==1 ) THEN
            Number = Npoigages
          ELSE
            Npoigages = Number
          ENDIF
        ELSEIF ( Dimname(:9)=='nlakeelev' ) THEN ! number of lake elevation columns columns in Data File
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nlakeelev
          ELSE
            Nlakeelev = Number
          ENDIF
        ELSEIF ( Dimname(:9)=='npoigages' ) THEN ! number of points-of-interest gages, based on NHDPlus
          found = 1
          IF ( return_value==1 ) THEN
            Number = Npoigages
          ELSE
            Npoigages = Number
          ENDIF
        ELSEIF ( Dimname(:9)=='nwateruse' ) THEN ! number of water use transfers
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nwateruse
          ELSE
            Nwateruse = Number
          ENDIF
        ELSEIF ( Dimname(:9)=='nconsumed' ) THEN ! number of water use consumption locations
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nconsumed
          ELSE
            Nconsumed = Number
          ENDIF
        ELSEIF ( Dimname(:9)=='nexternal' ) THEN ! number of water use external locations
          found = 1
          IF ( return_value==1 ) THEN
            Number = Nexternal
          ELSE
            Nexternal = Number
          ENDIF
        ENDIF
      ENDIF
      IF ( found==0 ) Number = -1 ! dimension not included in PRMS version
      END SUBROUTINE set_dimension

!***********************************************************************
! Set data type flag
!***********************************************************************
      SUBROUTINE set_data_type(Data_type, Type_flag)
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Data_type
      INTEGER, INTENT(OUT) :: Type_flag
      ! Functions
      INTRINSIC LEN
      ! Local Variables
      INTEGER string_length
!***********************************************************************
      string_length = LEN ( Data_type )
      IF ( string_length>3 .AND. Data_type(:4)=='real' ) THEN
        Type_flag = 2
      ELSEIF ( string_length>5 .AND. Data_type(:6)=='double' ) THEN
        Type_flag = 3
      ELSEIF ( string_length>5 .AND. Data_type(:6)=='string' ) THEN
        Type_flag = 4
      ELSEIF ( string_length>6 .AND. Data_type(:7)=='integer' ) THEN
        Type_flag = 1
      ELSE
        PRINT *, 'ERROR, invalid data type: ', Data_type
        PRINT *, '       valid values are real, double, string, integer'
        STOP
      ENDIF
      END SUBROUTINE set_data_type
