      PROGRAM PRMS_FORTRAN
      !SUBROUTINE GSFLOW_FORTRAN BIND(C,NAME="GSFLOW_FORTRAN")
      !DEC$ ATTRIBUTES DLLEXPORT :: GSFLOW_FORTRAN
!***********************************************************************
! PRMS main that controls time loop
!***********************************************************************
      USE PRMS_MODULE, ONLY: Model, Number_timesteps
      USE PRMS_MMFAPI
      USE PRMS_DATA_FILE
      IMPLICIT NONE
! Functions
      EXTERNAL gsflow_prms
! Local Variables
      INTEGER :: i
      LOGICAL :: AFR, MODSIM_ON_OFF
!***********************************************************************
      AFR = .TRUE.
      MODSIM_ON_OFF = .FALSE.

      CALL gsflow_prms('setdims', AFR, Number_timesteps, MODSIM_ON_OFF)

      CALL gsflow_prms('decl', AFR, Number_timesteps, MODSIM_ON_OFF)

      CALL gsflow_prms('init', AFR, Number_timesteps, MODSIM_ON_OFF)

      IF ( Model<2 ) THEN
        DO i = 1, Number_timesteps
          CALL gsflow_prms('run', AFR, Number_timesteps, MODSIM_ON_OFF) !add exchange vectors and model mode
        ENDDO
        CALL gsflow_prms('clean', AFR, Number_timesteps, MODSIM_ON_OFF)
      ENDIF

      END PROGRAM PRMS_FORTRAN
      ! END SUBROUTINE GSFLOW_FORTRAN
