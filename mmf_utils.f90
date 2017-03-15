!***********************************************************************
! DONE:
!  getdim, declfix, declmodule, decldim
!  control_string, control_integer, control_string_array
!  declpri - removed
!  read Control File
!  read Parameter File dimension section
!
! Place holders:
!  getstep - need (current time step, initially 0, restart last)
!  deltim - need (need time step increment in hours, hard-coded to 24)
!  declparam - need parameter data structure
!  declvar - need variable data structure, need cast type
!  getparam - need parameter data structure, need cast type
!  getparamstring - need parameter data structure
!  dattim - need function, or just compute the current date and time
!  Read Parameter File - put parameters in data structure
!
! TO DO:
! need to read Data File, check dimensions, verify, start and end time
! getvartype
! get rid of getvar
!***********************************************************************
      MODULE PRMS_MMFAPI
        IMPLICIT NONE
        INTEGER, SAVE :: Num_variables, Total_parameters
        TYPE PRMS_variable
             CHARACTER(LEN=32) :: variable_name
             CHARACTER(LEN=256) :: description
             INTEGER :: numvals, data_flag, decl_flag, get_flag, var_name_nchars, id_num
             CHARACTER(LEN=12) :: data_type, dimen_names, module_name, units
             DOUBLE PRECISION, POINTER :: values_dble(:)
        END TYPE PRMS_variable
        TYPE ( PRMS_variable ), SAVE, ALLOCATABLE :: Variable_data(:)
      END MODULE PRMS_MMFAPI

!***********************************************************************
! declparam - set up memory for parameters
!***********************************************************************
      INTEGER FUNCTION declparam(Modname, Paramname, Dimenname, Data_type, &
     &                           Defvalue, Minvalue, Maxvalue, Descshort, Desclong, Units)
      USE PRMS_MMFAPI
      USE PRMS_READ_PARAM_FILE
      USE PRMS_MODULE, ONLY: EQULS
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Paramname, Dimenname, Data_type
      CHARACTER(LEN=*), INTENT(IN) :: Defvalue, Minvalue, Maxvalue, Descshort, Desclong, Units
      ! Functions
      INTRINSIC TRIM, LEN_TRIM
      EXTERNAL set_data_type, read_error
      INTEGER, EXTERNAL :: numchars
      ! LIS function
      ! Local Variables
      INTEGER type_flag, found, i, j, num, inum, number, start, iset, id
      INTEGER, SAVE :: numpar
      CHARACTER(LEN=12) :: dim_string(3)
      DATA numpar/0/
!***********************************************************************
      IF ( numpar==0 ) THEN
        numpar = Read_parameters
        Total_parameters = Read_parameters
      ENDIF

      iset = 0
      num = LEN(Minvalue)
      IF ( num>6 ) THEN
        IF ( Minvalue(:3)/='bounded' ) iset = 1
      ENDIF

      found = 0
      DO i = 1, Read_parameters
        IF ( TRIM(Parameter_data(i)%param_name)==Paramname ) THEN
          Parameter_data(i)%decl_flag = 1
          Parameter_data(i)%short_description = Descshort
          Parameter_data(i)%long_description = Desclong
          Parameter_data(i)%units = Units
          Parameter_data(i)%dimen_names = Dimenname
          Parameter_data(i)%module_name = Modname
          Parameter_data(i)%max_value = Maxvalue
          Parameter_data(i)%min_value = Minvalue
          Parameter_data(i)%def_value = Defvalue
          Parameter_data(i)%data_type = Data_type      
          CALL set_data_type(Data_type, type_flag)
          IF ( type_flag<1 .OR. type_flag>3 ) CALL read_error(16, Paramname//' data type not implemented: '//Data_type)
          IF ( Parameter_data(i)%data_flag/=type_flag ) CALL read_error(16, Paramname// &
     &         ' data type does not match type in Parameter File')
          found = 1
          id = i
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        numpar = numpar + 1
        IF ( numpar>Num_parameters ) PRINT *, 'numpar', numpar, Num_parameters
        IF ( numpar>Num_parameters ) CALL read_error(16, Paramname//' PRMS error maximum number of parameters exceeded')
        Parameter_data(numpar)%decl_flag = 1
        Parameter_data(numpar)%short_description = Descshort
        Parameter_data(numpar)%long_description = Desclong
        Parameter_data(numpar)%units = Units
        Parameter_data(numpar)%dimen_names = Dimenname
        Parameter_data(numpar)%module_name = Modname
        Parameter_data(numpar)%max_value = Maxvalue
        Parameter_data(numpar)%min_value = Minvalue
        Parameter_data(numpar)%def_value = Defvalue
        Parameter_data(numpar)%data_type = Data_type
        Parameter_data(numpar)%param_name_nchars = LEN_TRIM(Paramname)
        CALL set_data_type(Data_type, type_flag)
        IF ( type_flag<1 .OR. type_flag>3 ) CALL read_error(16, Paramname//' data type not implemented: '//Data_type)
        Parameter_data(numpar)%data_flag = type_flag
        Parameter_data(numpar)%param_name = Paramname
        num = INDEX(Dimenname, ',')
        IF ( num==0 ) THEN
          inum = -2 ! get dimension value
          CALL set_dimension(Dimenname, inum)
          number = inum
        ELSE
          start = 1
          number = 1
          DO i = 1, 2
            num = num - 1
            READ ( Dimenname(start:num), '(A)' ) dim_string(i)
            start = num + 2
            num = LEN(Dimenname) + 1
            inum = -2 ! get dimension value
            CALL set_dimension(dim_string(i)(:numchars(dim_string(i))), inum)
            number = number*inum
          ENDDO
        ENDIF
        Parameter_data(numpar)%numvals = number
        IF ( Print_debug>-1 ) THEN
          PRINT *, 'Parameter: ', Paramname, ' not specified in Parameter File, set to default'
          PRINT *, EQULS
        ENDIF
        CALL write_outfile('Parameter: '//Paramname//' not specified in Parameter File, set to default')
        CALL write_outfile(EQULS)
        READ ( Defvalue, * ) Parameter_data(numpar)%default_value
        ALLOCATE ( Parameter_data(numpar)%values(number) )
        DO j = 1, number
          Parameter_data(numpar)%values(j) = Parameter_data(numpar)%default_value
        ENDDO
        Total_parameters = numpar
        id = numpar
      ENDIF

      READ ( Defvalue, * ) Parameter_data(id)%default_value
      IF ( iset==0 ) THEN
        READ ( Maxvalue, * ) Parameter_data(id)%maxval
        READ ( Minvalue, * ) Parameter_data(id)%minval
      ELSE
        Parameter_data(id)%maxval = 99999999
        Parameter_data(id)%minval = 0
      ENDIF

      !print *, 'total_parameters', Total_parameters, ' num_parameters', Num_parameters
      declparam = 0
      END FUNCTION declparam

!***********************************************************************
! check_parameters_declared - check for unnecessary parameters
!***********************************************************************
      SUBROUTINE check_parameters_declared()
      USE PRMS_READ_PARAM_FILE
      USE PRMS_MODULE, ONLY: Print_debug
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      EXTERNAL read_error
      ! Local Variables
      INTEGER i
!***********************************************************************
      DO i = 1, Read_parameters
        IF ( Parameter_data(i)%decl_flag/=1 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Parameter: ', TRIM(Parameter_data(i)%param_name), ' specified but not needed, ignored'
        ENDIF
      ENDDO
      END SUBROUTINE check_parameters_declared
     
!***********************************************************************
! declvar - set up memory for variables
!***********************************************************************
      INTEGER FUNCTION declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Data_type, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      DOUBLE PRECISION, TARGET :: Values(*)
      ! Functions
      INTEGER, EXTERNAL :: numchars
      EXTERNAL set_data_type
      ! LIS function
      ! Local Variables
      INTEGER type_flag
      INTEGER, SAVE :: init
      DATA init/0/
!***********************************************************************

!***********************************************************************
      IF ( init==0 ) THEN
        init = 1
        Num_variables = 0
        ALLOCATE ( Variable_data(400) ) ! don't know how many, need to read var_name file
      ENDIF
      ! need to declare parameters first, but don't know how many, know how many in Parameter File
      Num_variables = Num_variables + 1
      IF ( Num_variables>400 ) STOP 'PRMS ERROR, maximum number of declared variables (400) exceeded'
      Variable_data(Num_variables)%get_flag = 0
      Variable_data(Num_variables)%decl_flag = 1
      Variable_data(Num_variables)%variable_name = Varname
      Variable_data(Num_variables)%var_name_nchars = numchars(Varname)
      Variable_data(Num_variables)%description = Desc
      Variable_data(Num_variables)%units = Units
      Variable_data(Num_variables)%dimen_names = Dimenname
      Variable_data(Num_variables)%module_name = Modname
      Variable_data(Num_variables)%numvals = Numvalues
      Variable_data(Num_variables)%data_type = Data_type
      Variable_data(Num_variables)%id_num = Num_variables
      CALL set_data_type(Data_type, type_flag)
      IF ( type_flag<1 .OR. type_flag>3 ) THEN
        PRINT *, 'ERROR, data type not implemented: ', Data_type, ' Variable: ', &
     &           Varname(:Variable_data(Num_variables)%var_name_nchars)
        STOP
      ENDIF
      Variable_data(Num_variables)%data_flag = type_flag

      ALLOCATE ( Variable_data(Num_variables)%values_dble(Numvalues) )
      Variable_data(Num_variables)%values_dble => Values(:Numvalues)

      declvar = 0
      END FUNCTION declvar

!***********************************************************************
! getvar - get variable values
!***********************************************************************
      INTEGER FUNCTION getvar(Modname, Varname, Numvalues, Data_type, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      REAL, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      INTRINSIC TRANSFER
      INTEGER, EXTERNAL :: find_variable
      ! LIS function
      ! Local Variables
      INTEGER :: var_id
      REAL :: temp(Numvalues)
!***********************************************************************
      !need LIS data structure
      var_id = find_variable(Modname, Varname, Numvalues, Data_type)

       temp = transfer(Variable_data(var_id)%values_dble, temp)
       !temp = Variable_data(var_id)%values_dble
       !print *, Numvalues, varname, temp

      !Values = TRANSFER(Variable_data(var_id)%values_dble,Values)
      !do i = 1, Numvalues
      !    !Values(i) = SNGL(Variable_data(var_id)%values_dble(i))
      !    Values(i) = temp(i)
      !    IF ( values(i)<0.0 ) values(i) = 0.0
      !    !print *, Variable_data(var_id)%values_dble(i)
      !ENDDO
      values = temp
      !print *, 'values', values
      !print *, 'temp', temp
       !stop
      !print *, 'pointer', Variable_data(var_id)%values_dble
      getvar = 0
      END FUNCTION getvar

!***********************************************************************
! getvar_int - get variable values
!***********************************************************************
      INTEGER FUNCTION getvar_int(Modname, Varname, Numvalues, Data_type, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      INTEGER, INTENT(INOUT) :: Values(Numvalues)
      ! Functions
      INTRINSIC TRANSFER
      INTEGER, EXTERNAL :: find_variable
      ! LIS function
      ! Local Variables
      INTEGER :: var_id
!***********************************************************************
      !need LIS data structure
      var_id = find_variable(Modname, Varname, Numvalues, Data_type)

      Values = TRANSFER(Variable_data(var_id)%values_dble,Values)

      getvar_int = 0
      END FUNCTION getvar_int

!***********************************************************************
! getvar_dble - get variable values
!***********************************************************************
      INTEGER FUNCTION getvar_dble(Modname, Varname, Numvalues, Data_type, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      DOUBLE PRECISION, TARGET, INTENT(INOUT) :: Values(Numvalues)
      ! Functions
      INTEGER, EXTERNAL :: find_variable
      ! LIS function
      ! Local Variables
      INTEGER :: var_id
!***********************************************************************
      var_id = find_variable(Modname, Varname, Numvalues, Data_type)
      !need LIS data structure

      !Values = Variable_data(var_id)%values_dble
      Values = TRANSFER(Variable_data(var_id)%values_dble,Values)

      getvar_dble = 0
      END FUNCTION getvar_dble

!***********************************************************************
! find_variable - find variable in data structure
!***********************************************************************
      INTEGER FUNCTION find_variable(Modname, Varname, Numvalues, Data_type)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! Functions
      INTRINSIC TRIM
      ! LIS function
      ! Local Variables
      INTEGER :: found, i, ierr
!***********************************************************************
      ierr = 0
      found = 0
      find_variable = 1
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          found = 1
          IF ( Variable_data(i)%numvals/=Numvalues ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Variable: ', Varname, &
     &               ' number of values in getvar does not match declared number of values'
          ENDIF
          IF ( TRIM(Variable_data(i)%data_type)/=Data_type ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Variable: ', Varname, ' data type does in getvar not match declared data type'
          ENDIF
          find_variable = i
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'ERROR in: ', Modname, ', Variable: ', Varname, ' not declared'
        ierr = 1
      ENDIF
      IF ( ierr==1 ) STOP

      END FUNCTION find_variable

!***********************************************************************
! getvar_id - get variable index
!***********************************************************************
      INTEGER FUNCTION getvar_id(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! LIS function
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvar_id = 1
      !need LIS data structure
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvar_id = Variable_data(i)%id_num
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR control parameter: ', Varname, ' not available, remove from Control File'
      STOP
      END FUNCTION getvar_id

!***********************************************************************
! getvartype - get variable type needed to be compatible with MMF function
!***********************************************************************
      INTEGER FUNCTION getvartype(Varname, Var_type)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      INTEGER, INTENT(OUT) :: Var_type
      ! Functions
      INTRINSIC TRIM
      ! LIS function
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvartype = 1
      !need LIS data structure
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvartype = Variable_data(i)%data_flag
          Var_type = getvartype
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR control parameter: ', Varname, ' not available, remove from Control File'
      STOP
      END FUNCTION getvartype

!***********************************************************************
! getvar_type - get variable type
!***********************************************************************
      INTEGER FUNCTION getvar_type(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! LIS function
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvar_type = 1
      !need LIS data structure
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvar_type = Variable_data(i)%data_flag
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR control parameter: ', Varname, ' not available, remove from Control File'
      STOP
      END FUNCTION getvar_type

!***********************************************************************
! getvarnvals - get variable number of values
!***********************************************************************
      INTEGER FUNCTION getvarnvals(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! LIS function
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvarnvals = 1
      !need LIS data structure
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvarnvals = Variable_data(i)%numvals
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR in: getvarnvals, Variable: ', Varname, ' not declared'
      STOP
      END FUNCTION getvarnvals

!***********************************************************************
! getparam - get parameter values
!***********************************************************************
      INTEGER FUNCTION getparam(Modname, Paramname, Numvalues, Data_type, Values)
      USE PRMS_MMFAPI, ONLY: Total_parameters
      USE PRMS_READ_PARAM_FILE
      USE PRMS_MODULE, ONLY: Parameter_check_flag
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Paramname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      REAL, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      INTRINSIC TRIM
      ! LIS function
      ! Local Variables
      INTEGER :: type_flag, found, param_id, i, ierr
!***********************************************************************
      Values = 0.0
      !need LIS data structure
      ierr = 0
      found = 0
      DO i = 1, Total_parameters
        IF ( Paramname==TRIM(Parameter_data(i)%param_name) ) THEN
          found = 1
          IF ( Parameter_data(i)%numvals/=Numvalues ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &               ' number of values in getparam does not match declared number of values'
          ENDIF
          IF ( TRIM(Parameter_data(i)%data_type)/=Data_type ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' data type does in getparam not match declared data type'
          ENDIF
          param_id = i
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
        ierr = 1
      ENDIF
      IF ( ierr==1 ) STOP

      type_flag = Parameter_data(param_id)%data_flag

      IF ( type_flag==3 ) THEN
        CALL getvalues_dbl(param_id, Numvalues, Values)
      ELSEIF ( type_flag==2 ) THEN
        IF ( Parameter_check_flag==1 ) THEN
          DO i = 1, Numvalues
            IF ( Parameter_data(param_id)%values(i) > Parameter_data(param_id)%maxval ) THEN
              PRINT *, 'WARNING, value > maximum value for parameter: ', Paramname, '; index:', param_id
              PRINT *, '         value:', Parameter_data(param_id)%values(i), '; maximum value:', Parameter_data(param_id)%maxval
            ENDIF
            IF ( Parameter_data(param_id)%values(i) < Parameter_data(param_id)%minval ) THEN
              PRINT *, 'WARNING, value < minimum value for parameter: ', Paramname, '; index:', param_id
              PRINT *, '         value:', Parameter_data(param_id)%values(i), '; minimum value:', Parameter_data(param_id)%maxval
            ENDIF
          ENDDO
        ENDIF
        Values = Parameter_data(param_id)%values
      ELSEIF ( type_flag==1 ) THEN
        CALL getvalues_int(param_id, Numvalues, Values)
      ELSE
        PRINT *, 'Paramname: ', Paramname, ' type: ', type_flag
        STOP 'Parameter type not implemented'
      ENDIF

      getparam = 0
      END FUNCTION getparam

!***********************************************************************
! getvar_values_int - get values from variable data structure
!***********************************************************************
!      SUBROUTINE getvar_values_int(param_id, Numvalues, Values)
!      USE PRMS_MMFAPI
!      IMPLICIT NONE
!      ! Arguments
!      INTEGER, INTENT(IN) :: param_id, Numvalues
!      INTEGER, INTENT(OUT) :: Values(Numvalues)
!!***********************************************************************
!      values = Variable_data(param_id)%values_int
!      END SUBROUTINE getvar_values_int

!***********************************************************************
! getvar_values_real - get values from variable data structure
!***********************************************************************
!      SUBROUTINE getvar_values_real(param_id, Numvalues, Values)
!      USE PRMS_MMFAPI
!      IMPLICIT NONE
!      ! Arguments
!      INTEGER, INTENT(IN) :: param_id, Numvalues
!      REAL, INTENT(OUT) :: Values(Numvalues)
!!***********************************************************************
!      values = Variable_data(param_id)%values_real
!      END SUBROUTINE getvar_values_real

!***********************************************************************
! getvalues_int - get values from parameter data structure
!***********************************************************************
      SUBROUTINE getvalues_int(param_id, Numvalues, Values)
      USE PRMS_READ_PARAM_FILE
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: param_id, Numvalues
      INTEGER, INTENT(OUT) :: Values(Numvalues)
!***********************************************************************
      values = Parameter_data(param_id)%values
      END SUBROUTINE getvalues_int

!***********************************************************************
! getvalues_dbl - get values from parameter data structure
!***********************************************************************
      SUBROUTINE getvalues_dbl(param_id, Numvalues, Values)
      USE PRMS_READ_PARAM_FILE
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: param_id, Numvalues
      DOUBLE PRECISION, INTENT(OUT) :: Values(Numvalues)
!***********************************************************************
      values = Parameter_data(param_id)%values
      END SUBROUTINE getvalues_dbl

!***********************************************************************
! readvar - get variable from Data File
!***********************************************************************
      INTEGER FUNCTION readvar(Modname, Varname)
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname
!***********************************************************************
      readvar = 0
      END FUNCTION readvar

!***********************************************************************
! timestep_hours - time step increment in hours
!***********************************************************************
      DOUBLE PRECISION FUNCTION deltim()
      IMPLICIT NONE
      ! Functions
      ! LIS function
!***********************************************************************
      !deltim = lisfunction() ! need to make routine to get time step increment
      deltim = 24.0D0
      END FUNCTION deltim

!***********************************************************************
! getstep - current time step
! declare and init, time step is 0
! however, for a restart execution this value needs to be the last timestep from the previous simulation
!***********************************************************************
      INTEGER FUNCTION getstep()
      IMPLICIT NONE
      ! Local Variable
      INTEGER, SAVE :: time_step
      DATA time_step/-1/ ! set to -1 so declare and init start with 0
      !need to save the value in restart file
!***********************************************************************
      !getstep = ! need to get LIS time step number and/or restart value
      time_step = time_step + 1
      getstep = time_step
      END FUNCTION getstep

!***********************************************************************
! dattim - get start, end, or current date and time
!***********************************************************************
      SUBROUTINE dattim(String, Datetime)
      USE PRMS_CONTROL_FILE, ONLY: Julian_day_absolute
      USE PRMS_MODULE, ONLY: Timestep, Endtime, Starttime
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: String
      INTEGER, INTENT(OUT) :: Datetime(6)
      EXTERNAL compute_gregorian
      ! Local variable
      INTEGER string_length
!***********************************************************************
      Datetime = 0
      string_length = LEN(String)
      IF ( String(:3)=='end' ) THEN
        Datetime = Endtime
      ELSEIF ( String(:3)=='now' ) THEN
        CALL compute_gregorian(Julian_day_absolute, Nowyear, Nowmonth, Nowday)
        Datetime(1) = Nowyear
        Datetime(2) = Nowmonth
        Datetime(3) = Nowday
        ! Datetime = LIS function
      ELSEIF ( string_length>4 ) THEN
        IF ( String(:5)=='start' ) THEN
          Datetime = Starttime
        ELSE
          STOP 'ERROR, invalid call to dattim'
        ENDIF
      ENDIF
      END SUBROUTINE dattim

!***********************************************************************
! declmodule
! print module version information to user's screen
!***********************************************************************
      INTEGER FUNCTION declmodule(Module_name, Description, Versn)
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Module_name, Description, Versn
!***********************************************************************
      WRITE (*, '(A)' ) Description//Module_name//', version: '//Versn
      declmodule = 0
      END FUNCTION declmodule

!***********************************************************************
! decldim
! certain dimensions names with definitions are needed by MMF data structure
! for a LIS executable calling this function is for compatibility only
!***********************************************************************
      INTEGER FUNCTION decldim(Dimname, Defval, Maxval, Description)
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Defval, Maxval
      CHARACTER(LEN=*), INTENT(IN) :: Dimname, Description
      ! Functions
      EXTERNAL set_dimension, read_error
      ! Local Variables
      INTEGER def_value
!***********************************************************************
      !def_value = Defval
      def_value = -2
      CALL set_dimension(Dimname, def_value)
      IF ( def_value==-1 ) CALL read_error(15, 'dimension: '//Dimname)
      decldim = 0
      END FUNCTION decldim

!***********************************************************************
! getdim - get dimension number
!***********************************************************************
      INTEGER FUNCTION getdim(Dimname)
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Dimname
      ! Functions
      EXTERNAL set_dimension, read_error
      ! Local Variable
      INTEGER ndimen
!***********************************************************************
      ndimen = -2
      CALL set_dimension(Dimname, ndimen)
      IF ( ndimen==-1 ) CALL read_error(15,'dimension: '//Dimname)
      getdim = ndimen
      END FUNCTION getdim

!***********************************************************************
! declfix
! certain dimensions names with definitions are needed by MMF data structure
! for a LIS executable calling this function is for compatibility only
!***********************************************************************
      INTEGER FUNCTION declfix(Dimname, Defval, Maxval, Description)
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Defval, Maxval
      CHARACTER(LEN=*), INTENT(IN) :: Dimname, Description
      ! Functions
      EXTERNAL set_dimension, read_error
      ! Local Variables
      INTEGER def_value
!***********************************************************************
      def_value = Defval
      CALL set_dimension(Dimname, def_value)
      IF ( def_value==-1 ) CALL read_error(15, 'dimension: '//Dimname)
      declfix = 0
      END FUNCTION declfix

!***********************************************************************
! control_integer
! control parameters are read this sets integer values stored in the
! data base and checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_integer(Parmval, Paramname)
      USE PRMS_CONTROL_FILE, ONLY: Num_control_parameters, Control_parameter_data
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      INTEGER, INTENT(OUT) :: Parmval
      ! Functions
      INTRINSIC :: TRIM
      ! Local Variables
      INTEGER :: i, found
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          Parmval = Control_parameter_data(i)%values_int(1)
          found = i
          EXIT
        ENDIF
      ENDDO
      IF ( found==0 ) THEN
        Num_control_parameters = Num_control_parameters + 1
        PRINT *, 'WARNING, control parameter not in Control File: ', TRIM(Paramname), ', set to 0'
        Control_parameter_data(Num_control_parameters)%read_flag = 2 ! set to default
        Control_parameter_data(Num_control_parameters)%data_type = 1
        Control_parameter_data(Num_control_parameters)%numvals = 1
        Control_parameter_data(Num_control_parameters)%name = paramname
        Control_parameter_data(Num_control_parameters)%values_int(1) = 0 !???
      ENDIF

      control_integer = 0
      END FUNCTION control_integer

!***********************************************************************
! control_string
! control parameters are read in PRMS with MMF.
! For a LIS executable control parameters are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_string(Parmval, Paramname)
      USE PRMS_CONTROL_FILE, ONLY: Num_control_parameters, Control_parameter_data
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: TRIM
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      CHARACTER(LEN=*), INTENT(OUT) :: Parmval
      ! Local Variables
      INTEGER :: found, i
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          Parmval = Control_parameter_data(i)%values_character(1)
          found = i
          EXIT
        ENDIF
      ENDDO
      IF ( found==0 ) THEN
        Num_control_parameters = Num_control_parameters + 1
        PRINT *, 'WARNING, control parameter not in Control File: ', TRIM(Paramname), ', set to blank'
        Control_parameter_data(Num_control_parameters)%read_flag = 2 ! set to default
        Control_parameter_data(Num_control_parameters)%data_type = 4
        Control_parameter_data(Num_control_parameters)%numvals = 1
        Control_parameter_data(Num_control_parameters)%name = paramname
        Control_parameter_data(Num_control_parameters)%values_int(1) = ' '
      ENDIF

      control_string = 0
      END FUNCTION control_string

!***********************************************************************
! control_string_array
! control parameters are read in PRMS with MMF.
! For a LIS executable control parameters are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_string_array(Parmval, Paramname, Array_index)
      IMPLICIT NONE
      ! Arguments
      ! Array_index and Parmval not used, only used with MMF
      INTEGER, INTENT(IN) :: Array_index
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      INTEGER, INTENT(OUT) :: Parmval
      ! Functions
      INTEGER, EXTERNAL :: control_integer
!***********************************************************************
      control_string_array = control_integer(Parmval, Paramname)
      END FUNCTION control_string_array

!***********************************************************************
! getparamstring
! control parameters are read in PRMS with MMF.
! For a LIS executable control parameters are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION getparamstring(Module_name, Paramname, Numvalues, Data_type, Array_index, String)
      USE PRMS_READ_PARAM_FILE, ONLY: Num_parameters
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Module_name, Paramname, Data_type
      INTEGER, INTENT(IN) :: Numvalues, Array_index
      CHARACTER(LEN=*), INTENT(OUT) :: String
      ! Functions
      INTRINSIC INDEX
      !EXTERNAL set_dimension
      ! LIS function
      ! Local Variables
      INTEGER nchars, nchars_param, type_flag, num_values, i, j
      CHARACTER(LEN=16) :: dimenname
!***********************************************************************
      String = ' '
      !need LIS data structure
      ! Modname
      nchars_param = INDEX( Paramname, ' ') - 1
      ! Paramname(:nchars_param)
      nchars = INDEX( Dimenname, ' ') - 1
      num_values = -2
      IF ( num_values/=Numvalues ) THEN
        PRINT *, 'ERROR, number of values does not equal values for the dimension'
        PRINT *, '       parameter: ', Dimenname(:nchars), ' dimension value:', num_values
        PRINT *, '       dimension: ', Paramname(:nchars_param), ' number of values:', Numvalues
        STOP
      ENDIF
      nchars = INDEX( Data_type, ' ') - 1
      ! Data_type(:nchars)
      CALL set_data_type(Data_type, type_flag)

      DO j = 1, Num_parameters
          DO i = 1, Numvalues
            IF ( type_flag==1 ) THEN
            ELSEIF ( type_flag==2 ) THEN
            ELSEIF ( type_flag==3 ) THEN
            ELSEIF ( type_flag==4 ) THEN
            ENDIF
          ENDDO
        EXIT
      ENDDO

      getparamstring = 0
      END FUNCTION getparamstring
