!***********************************************************************
!     Output runtime plot information
!***********************************************************************
      MODULE PRMS_RUNTIME_OUT
        IMPLICIT NONE
        INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:), Runtime_type(:), Runtime_id(:), Runtime_nvals(:)
        REAL, SAVE, ALLOCATABLE :: Runtime_values(:), Runtime_values2(:)
        CHARACTER(LEN=11), PARAMETER :: MODNAME = 'runtime_out'
        CHARACTER(LEN=26), PARAMETER :: PROCNAME = 'Summary                   '
        INTEGER, ALLOCATABLE :: values_int(:)
        REAL, ALLOCATABLE :: values_real(:)
        DOUBLE PRECISION, ALLOCATABLE :: values_dble(:)
      double precision, pointer :: zero

      END MODULE PRMS_RUNTIME_OUT

!     ******************************************************************
!     Runtime plots module
!     ******************************************************************
      INTEGER FUNCTION runtime_out()
      USE PRMS_MODULE, ONLY: Process
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: runtime_outinit, runtime_outrun
      EXTERNAL :: print_module
! Local Variables
      CHARACTER(LEN=80) :: Version_runtime_out
!***********************************************************************
      runtime_out = 0

      IF ( Process(:3)=='run' ) THEN
        runtime_out = runtime_outrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_runtime_out = '$Id: runtime_out.f90 5142 2012-12-18 19:47:26Z rsregan $'
        CALL print_module(Version_runtime_out, 'Summary                 ', 90)
      ELSEIF ( Process(:4)=='init' ) THEN
        runtime_out = runtime_outinit()
      ENDIF

      END FUNCTION runtime_out

!***********************************************************************
!     runtime_outinit - Initialize runtime_out module
!***********************************************************************
      INTEGER FUNCTION runtime_outinit()
      USE PRMS_RUNTIME_OUT
      USE PRMS_CONTROL_FILE, ONLY: dispVar_names, dispVar_element, NumdispVar_names, NumdispVar_elements
      USE PRMS_MODULE, ONLY: Nhru, Timestep
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: numchars, getvartype, getvarnvals
! Local Variables
      INTEGER :: jj, ierr, itype
!***********************************************************************
      runtime_outinit = 1

      IF ( NumdispVar_names/=NumdispVar_elements ) THEN
        PRINT *, 'ERROR, number of names specified for dispVar_names not equal to number of elements'
        PRINT *, '       number of names =', NumdispVar_names, ' number of elements = ', NumdispVar_elements
        STOP
      ENDIF

      ALLOCATE ( Runtime_type(NumdispVar_names), Nc_vars(NumdispVar_names), Runtime_id(NumdispVar_names) )
      ALLOCATE ( Runtime_nvals(NumdispVar_names), Runtime_values(NumdispVar_names), Runtime_values2(NumdispVar_names) )
      allocate (zero)
      zero = 0.0D0

      ierr = 0
      DO jj = 1, NumdispVar_names
        Nc_vars(jj) = numchars(dispVar_names(jj))
        itype = getvartype(dispVar_names(jj)(:Nc_vars(jj)))
        IF ( itype<1 .OR. itype>3 ) THEN
          ierr = 1
          PRINT *, 'ERROR, invalid runtime variable type: 1, 2, and 3 allowed', itype
          CYCLE
        ENDIF
        Runtime_type(jj) = itype
        Runtime_nvals(jj) = getvarnvals(dispVar_names(jj)(:Nc_vars(jj)))
        READ ( dispVar_element(jj), * ) Runtime_id(jj)
        IF ( jj<10 ) THEN
          PRINT '(A,I1,A)', 'control_array: key = dispVar_names ind = ', jj-1, ' val = '// &
     &                      dispVar_names(jj)(:Nc_vars(jj))
          PRINT '(A,I1,A,A)', 'control_array: key = dispVar_element ind = ', jj-1, ' val = ', &
     &                        dispVar_element(jj)(:numchars(dispVar_element(jj)))
        ELSE
          PRINT '(A,I2,A)', 'control_array: key = dispVar_names ind = ', jj-1, ' val = '// &
     &                      dispVar_names(jj)(:Nc_vars(jj))
          PRINT '(A,I2,A,A)', 'control_array: key = dispVar_element ind = ', jj-1, ' val = ', &
     &                        dispVar_element(jj)(:numchars(dispVar_element(jj)))
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP

      IF ( Timestep==0 ) Runtime_values = 0.0
      ALLOCATE ( values_int(Nhru), values_real(Nhru), values_dble(Nhru) )
      runtime_outinit = 0
      END FUNCTION runtime_outinit

!***********************************************************************
!     runtime_outrun - Output to the screen runtime variables
!***********************************************************************
      INTEGER FUNCTION runtime_outrun()
      USE PRMS_RUNTIME_OUT
      USE PRMS_CONTROL_FILE, ONLY: NumdispVar_names, dispVar_names
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTRINSIC SNGL, FLOAT
      INTEGER, EXTERNAL :: getvar, getvar_dble, getvar_int, getvarnvals
      EXTERNAL read_error
! Local Variables
      INTEGER :: jj, nvals, nc, nvalues, i
      CHARACTER(LEN=26) :: fmt, fmt2
!***********************************************************************
      fmt = ' '
      WRITE ( fmt2, '(A,I4,A)' ) "(A,", NumdispVar_names, "(E12.5,A),E12.5)"
      WRITE ( fmt, '(A,I3.2,A)' ) "(A,", NumdispVar_names, "E12.5)"
      DO jj = 1, NumdispVar_names
        nc = Nc_vars(jj)
        nvals = Runtime_nvals(jj)
        nvalues = getvarnvals(dispVar_names(jj)(:nc))
        IF ( Runtime_id(jj)>nvalues ) THEN
          PRINT *, 'ERROR, element id number exceeds dimension for variable: ', dispVar_names(jj)(:nc)
          STOP
        ENDIF
        IF ( nvalues>Nhru ) THEN
          PRINT *, 'ERROR, in PRMS, number of values (', nvals, ') exceeds array limit of nhru (', Nhru, ')'
          STOP
        ENDIF
        IF ( Runtime_type(jj)==3 ) THEN
          !ALLOCATE ( values_dble(nvals) )
          IF ( getvar_dble(MODNAME, dispVar_names(jj)(:nc), nvals, 'double', values_dble)/=0 ) &
     &         CALL read_error(4, dispVar_names(jj)(:nc))
          Runtime_values(jj) = SNGL(values_dble(Runtime_id(jj)))
          !Runtime_values(jj) = TRANSFER(values_dble(Runtime_id(jj)),Runtime_values(jj))
          !print *, dispVar_names(jj)(:nc), nvals, Runtime_values(jj), 'double'
          !DEALLOCATE ( values_dble )
        ELSEIF ( Runtime_type(jj)==2 ) THEN
          !ALLOCATE ( values_real(nvals) )
          IF ( getvar(MODNAME, dispVar_names(jj)(:nc), nvals, 'real', values_real)/=0 ) &
     &         CALL read_error(4, dispVar_names(jj)(:nc))
          Runtime_values(jj) = values_real(Runtime_id(jj))
          !print *, dispVar_names(jj)(:nc), nvals, Runtime_values(jj), 'real'
          !DEALLOCATE ( values_real )
        ELSEIF ( Runtime_type(jj)==1 ) THEN
          !ALLOCATE ( values_int(nvals) )
          IF ( getvar_int(MODNAME, dispVar_names(jj)(:nc), nvals, 'integer', values_int)/=0 ) &
     &         CALL read_error(4, dispVar_names(jj)(:nc))
          Runtime_values(jj) = FLOAT(values_int(Runtime_id(jj)))
          !print *, dispVar_names(jj)(:nc), nvals, Runtime_values(jj), 'integer'
          !DEALLOCATE ( values_int )
        ELSE
          STOP 'ERROR in runtime_out'
        ENDIF
      ENDDO

      i = 0
      DO jj = 1, NumdispVar_names
         Runtime_values2(jj) = Runtime_values(jj)
     !                 print *, Runtime_values2(jj), Runtime_values(jj), jj, values_real(Runtime_id(jj)), &
     !&                values_dble(Runtime_id(jj)), Runtime_type(jj), zero
         !IF ( Runtime_values2(jj)<0.0 ) THEN
         !    Runtime_values2(jj) = 0.0
         !    i = 1
         !endif
     !                 print *, Runtime_values2(jj), Runtime_values(jj), jj, values_real(Runtime_id(jj)), &
     !&                values_dble(Runtime_id(jj)), Runtime_type(jj), zero
         
         ENDDO


      WRITE (77, fmt2) 'plotRuntimeGraphValue: xval =', (Runtime_values2(jj), ',', jj=1, NumdispVar_names-1), &
     &                 Runtime_values2(NumdispVar_names)
      WRITE (*, fmt) 'plotRuntimeGraphValue: xval =', (Runtime_values2(jj), jj=1, NumdispVar_names)
      !stop
      
      runtime_outrun = 0
      END FUNCTION runtime_outrun

