!***********************************************************************
!     Output statvar file
!***********************************************************************
      MODULE PRMS_STATVAR_OUT
        IMPLICIT NONE
        INTEGER, SAVE :: Statvar_unit
        INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:), Stat_var_type(:), Statvar_id(:), Statvar_nvals(:)
        REAL, SAVE, ALLOCATABLE :: Stat_var_values(:)
        CHARACTER(LEN=11), PARAMETER :: MODNAME = 'statvar_out'
        CHARACTER(LEN=80) :: Version_statvar_out
      END MODULE PRMS_STATVAR_OUT

!     ******************************************************************
!     Statistics Variables module
!     ******************************************************************
      INTEGER FUNCTION statvar_out()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: statvar_outdecl, statvar_outinit
      INTEGER, EXTERNAL :: statvar_outclean, statvar_outrun
!***********************************************************************
      statvar_out = 0

      IF ( Process(:3)=='run' ) THEN
        statvar_out = statvar_outrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        statvar_out = statvar_outdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        statvar_out = statvar_outinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        statvar_out = statvar_outclean()
      ENDIF

      END FUNCTION statvar_out

!***********************************************************************
!     statvar_outdecl - declare parameters and variables
!***********************************************************************
      INTEGER FUNCTION statvar_outdecl()
      USE PRMS_STATVAR_OUT, ONLY: Version_statvar_out
      USE PRMS_CONTROL_FILE, ONLY: NstatVars
      USE PRMS_MODULE, ONLY: StatsON_OFF
      IMPLICIT NONE
! Functions
      EXTERNAL :: print_module
!***********************************************************************
      Version_statvar_out = 'statvar_out.f90 2017-09-27 15:42:00Z'
      CALL print_module(Version_statvar_out, 'Summary                 ', 90)

      IF ( NstatVars==0 ) THEN
        PRINT *, 'Warning, statvar output requested with nstatVars equal 0, no output is produced'
        StatsON_OFF = 0
        RETURN
      ENDIF

      statvar_outdecl = 0
      END FUNCTION statvar_outdecl

!***********************************************************************
!     statvar_outinit - Initialize statvar_out module
!***********************************************************************
      INTEGER FUNCTION statvar_outinit()
      USE PRMS_STATVAR_OUT
      USE PRMS_CONTROL_FILE, ONLY: NstatVars, statVar_names, Stat_var_file, statVar_element, Num_statvar_names, Num_statvar_elements
      USE PRMS_MODULE, ONLY: Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: numchars, getvartype, getvarnvals
      EXTERNAL PRMS_open_output_file
! Local Variables
      INTEGER :: jj, ios, ierr, itype
!***********************************************************************
      statvar_outinit = 1

      ierr = 0
      IF ( Num_statvar_names/=NstatVars ) THEN
        PRINT *, 'ERROR, number of names specified for statVar_names not equal to nstatVars'
        PRINT *, '       nstatVars =', NstatVars, ' number of names = ', Num_statvar_names
        ierr = 1
      ENDIF
      IF ( Num_statvar_elements/=NstatVars ) THEN
        PRINT *, 'ERROR, number of elements specified for statVar_elements not equal to nstatVars'
        PRINT *, '       nstatVars =', NstatVars, ' number of names = ', Num_statvar_elements
        ierr = 1
      ENDIF
      IF ( ierr==1 ) STOP

      ALLOCATE ( Stat_var_type(NstatVars), Nc_vars(NstatVars), Statvar_id(NstatVars) )
      ALLOCATE ( Statvar_nvals(NstatVars), Stat_var_values(NstatVars) )

      CALL PRMS_open_output_file(Statvar_unit, Stat_var_file, 'xxx', 0, ios)
      IF ( ios/=0 ) STOP 'ERROR opening statvar file'
      WRITE ( Statvar_unit, '(I2)' ) NstatVars 

      ierr = 0
      DO jj = 1, NstatVars
        Nc_vars(jj) = numchars(statVar_names(jj))
        itype = getvartype(statVar_names(jj)(:Nc_vars(jj)))
        IF ( itype<1 .AND. itype>3 ) THEN
          ierr = 1
          PRINT *, 'ERROR, invalid statvar type: 1, 2, and 3 allowed', itype
          CYCLE
        ENDIF
        Stat_var_type(jj) = itype
        Statvar_nvals(jj) = getvarnvals(statVar_names(jj)(:Nc_vars(jj)))
        READ ( statVar_element(jj), * ) Statvar_id(jj)
        WRITE ( Statvar_unit, '(A,I7)' ) statVar_names(jj)(:Nc_vars(jj)), Statvar_id(jj)
      ENDDO
      IF ( ierr==1 ) STOP

      IF ( Timestep==0 ) Stat_var_values = 0.0

      statvar_outinit = 0
      END FUNCTION statvar_outinit

!***********************************************************************
!     statvar_outrun - Output to a file the statvar variables
!***********************************************************************
      INTEGER FUNCTION statvar_outrun()
      USE PRMS_STATVAR_OUT
      USE PRMS_CONTROL_FILE, ONLY: NstatVars, statVar_names
      USE PRMS_MODULE, ONLY: Timestep
      USE PRMS_SET_TIME, ONLY: Nowtime
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTRINSIC SNGL, FLOAT
      INTEGER, EXTERNAL :: getvarnvals
      EXTERNAL read_error, getvar_real, getvar_dble, getvar_int
! Local Variables
      INTEGER :: jj, nvals, nc, nvalues
      INTEGER, ALLOCATABLE, TARGET :: values_int(:)
      REAL, ALLOCATABLE, TARGET :: values_real(:)
      DOUBLE PRECISION, ALLOCATABLE, TARGET :: values_dble(:)
!***********************************************************************
      DO jj = 1, NstatVars
        nc = Nc_vars(jj)
        nvals = Statvar_nvals(jj)
        nvalues = getvarnvals(statVar_names(jj)(:nc))
        IF ( Statvar_id(jj)>nvalues ) THEN
          PRINT *, 'ERROR, element id number exceeds dimension for variable: ', statVar_names(jj)(:nc)
          STOP
        ENDIF
        IF ( Stat_var_type(jj)==3 ) THEN
          ALLOCATE ( values_dble(nvals) )
          CALL getvar_dble(MODNAME, statVar_names(jj)(:nc), nvals, values_dble)
          Stat_var_values(jj) = SNGL(values_dble(Statvar_id(jj)))
          !print *, statVar_names(jj)(:nc), nvals, Stat_var_values(jj), 'double'
          DEALLOCATE ( values_dble )
        ELSEIF ( Stat_var_type(jj)==2 ) THEN
          ALLOCATE ( values_real(nvals) )
          CALL getvar_real(MODNAME, statVar_names(jj)(:nc), nvals, values_real)
          Stat_var_values(jj) = values_real(Statvar_id(jj))
          !print *, statVar_names(jj)(:nc), nvals,Stat_var_values(jj), 'real'
          DEALLOCATE ( values_real )
        ELSEIF ( Stat_var_type(jj)==1 ) THEN
          ALLOCATE ( values_int(nvals) )
          CALL getvar_int(MODNAME, statVar_names(jj)(:nc), nvals, values_int)
          Stat_var_values(jj) = FLOAT(values_int(Statvar_id(jj)))
          !print *, statVar_names(jj)(:nc), nvals,Stat_var_values(jj), 'integer'
          DEALLOCATE ( values_int )
        ELSE
          STOP 'ERROR in statvar_out'
        ENDIF
      ENDDO

      WRITE ( Statvar_unit, * ) Timestep, Nowtime, (Stat_var_values(jj), jj=1, Nstatvars)
 
      statvar_outrun = 0
      END FUNCTION statvar_outrun

!***********************************************************************
!     statvar_outclean - close files
!***********************************************************************
      INTEGER FUNCTION statvar_outclean()
      USE PRMS_STATVAR_OUT, ONLY: Statvar_unit
      IMPLICIT NONE
!***********************************************************************
      CLOSE ( Statvar_unit )
      statvar_outclean = 0
      END FUNCTION statvar_outclean
