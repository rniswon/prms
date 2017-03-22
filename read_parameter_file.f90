      MODULE PRMS_READ_PARAM_FILE
        USE PRMS_MODULE, ONLY: Print_debug, EQULS, Version_read_parameter_file, Param_file
        INTEGER, SAVE :: Param_unit, Read_parameters
      END MODULE PRMS_READ_PARAM_FILE

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
      SUBROUTINE read_parameter_file_dimens
      USE PRMS_READ_PARAM_FILE
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      EXTERNAL read_error, PRMS_open_input_file, write_outfile, setdimension
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      CHARACTER(LEN=16) :: string, dimname
      CHARACTER(LEN=150) :: line
      CHARACTER(LEN=24) :: dimstring
      INTEGER nchars, ios, dimen_value
!***********************************************************************
      Version_read_parameter_file = 'read_parameter_file.f90 2017-03-11 12:12:00Z'

      CALL PRMS_open_input_file(Param_unit, Param_file, 'param_file', 0, ios)
      IF ( ios/=0 ) STOP
      IF ( Print_debug>-1 ) THEN
        CALL write_outfile(EQULS)
        CALL write_outfile('Using PRMS Parameter File: '//Param_file)
      ENDIF

! Echo Parmeter File Header and comment lines
      READ ( Param_unit, FMT='(A)', IOSTAT=ios ) line
      IF ( ios/=0 ) CALL read_error(13, 'description')
      IF ( Print_debug>-1 ) THEN
        CALL write_outfile('Description: '//TRIM(line))
        CALL write_outfile(EQULS)
        CALL write_outfile('Comment lines:')
      ENDIF

      ! Find start of dimensions section
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) line
        IF ( ios==-1 ) CALL read_error(13, 'end of file found before dimensions')
        IF ( ios/=0 ) CALL read_error(13, 'comment')
        IF ( line(:16)=='** Dimensions **' ) EXIT
        IF ( Print_debug>-1 ) CALL write_outfile(TRIM(line))
      ENDDO
      IF ( line(:16)/='** Dimensions **' ) CALL read_error(11, 'missing dimension section: '//TRIM(line))
      IF ( Print_debug>-1 ) THEN
        CALL write_outfile(EQULS)
        CALL write_outfile('Using dimensions    number')
      ENDIF

! Read all dimensions

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
        nchars = numchars(dimname)
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension name: '//dimname(:nchars))
        READ ( Param_unit, *, IOSTAT=ios ) dimen_value
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension value')

        CALL setdimension(dimname, dimen_value)

        IF ( dimen_value==0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, dimension: ', dimname(:nchars), ' is not needed as value specified as 0'
        ENDIF
        IF ( Print_debug>-1 ) THEN
          WRITE ( dimstring, '(A,I8)' ) dimname, dimen_value
          CALL write_outfile(dimstring)
        ENDIF
      ENDDO
      IF ( Print_debug>-1 ) CALL write_outfile(EQULS)
      END SUBROUTINE read_parameter_file_dimens

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
      SUBROUTINE read_parameter_file_params
      USE PRMS_READ_PARAM_FILE
      USE PRMS_MODULE, ONLY: Version_read_parameter_file
      USE PRMS_MMFAPI, ONLY: Num_parameters
      IMPLICIT NONE
      ! Functions
      EXTERNAL read_error, PRMS_open_input_file, print_module, setparam
      INTEGER, EXTERNAL :: control_string, numchars, getdim
      ! Local Variables
      CHARACTER(LEN=16) :: string
      CHARACTER(LEN=32) :: paramstring
      CHARACTER(LEN=12) :: dim_string(2)
      INTEGER nchars, ios, num_dims, num_param_values, i, j, param_type, num, inum
      INTEGER, ALLOCATABLE :: idmy(:)
      REAL, ALLOCATABLE :: dmy(:)
      !***********************************************************************
      CALL print_module(Version_read_parameter_file, 'Read Parameter File         ', 90)
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
          inum = getdim(dim_string(i))
          IF ( inum==-1 ) CALL read_error(11, TRIM(dim_string(i)))
          num = num*inum
        ENDDO
        READ ( Param_unit, *, IOSTAT=ios ) num_param_values
        IF ( ios/=0 ) CALL read_error(11, 'invalid number of parameter values: '//paramstring(:nchars))
!        IF ( num/=num_param_values ) CALL read_error(11, 'invalid number of parameter values based on specified dimensions '//paramstring(:nchars))
        READ ( Param_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(11, 'invalid parameter type '//paramstring(:nchars))
        IF ( param_type<1 .OR. param_type>3 ) CALL read_error(11, 'invalid parameter type: '//paramstring(:nchars))
        IF ( param_type==1 ) THEN
          ALLOCATE ( idmy(num_param_values), dmy(1) )
          READ ( Param_unit, *, IOSTAT=ios ) (idmy(j),j=1,num_param_values)
          IF ( ios/=0 ) CALL read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
        ELSE
          ALLOCATE ( dmy(num_param_values), idmy(1) )
          READ ( Param_unit, *, IOSTAT=ios ) (dmy(j),j=1,num_param_values)
          IF ( ios/=0 ) CALL read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
        ENDIF
        CALL setparam(paramstring(:nchars), num_param_values, param_type, num_dims, dim_string, dmy, idmy)
        Read_parameters = Read_parameters + 1
        DEALLOCATE ( dmy, idmy )
      ENDDO
      
      CLOSE ( param_unit )
      END SUBROUTINE read_parameter_file_params
