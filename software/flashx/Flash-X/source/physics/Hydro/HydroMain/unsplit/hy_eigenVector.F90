!!****if* source/physics/Hydro/HydroMain/unsplit/hy_eigenVector
!! NOTICE
!!  Copyright 2022 UChicago Argonne, LLC and contributors
!!
!!  Licensed under the Apache License, Version 2.0 (the "License");
!!  you may not use this file except in compliance with the License.
!!
!!  Unless required by applicable law or agreed to in writing, software
!!  distributed under the License is distributed on an "AS IS" BASIS,
!!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!  See the License for the specific language governing permissions and
!!  limitations under the License.
!!
!! NAME
!!
!!  hy_eigenVector
!!
!! SYNOPSIS
!!
!!  hy_eigenVector( real (OUT)         :: LeftEigvec(HY_WAVENUM,HY_VARINUM),
!!                      real (OUT)         :: RightEigvec(HY_VARINUM,HY_WAVENUM)
!!                      real (IN)          :: V(HY_VARINUM2),
!!                      integer(IN)        :: dir,
!!                      logical(IN)        :: cons,
!!                      real (IN)          :: C_fast,
!!                      real (IN),optional :: C_alfn,
!!                      real (IN),optional :: C_slow,
!!                      real (IN),optional :: A_f,
!!                      real (IN),optional :: A_s,
!!                      real (IN),optional :: B_beta(MDIM) )
!!
!! DESCRIPTION
!!
!!  This routine calculates MHD/Hydro eigenvectors in either primitive form (used in
!!  Riemann solver) or conservative form (used in conservative updates).
!!
!!  This implementation is a stub implementation at this level.
!!
!! ARGUMENTS
!!
!!  LeftEigvec  - Left eigenvectors
!!  RightEigvec - Right eigenvectors
!!  V           - Primitive variables + gammas:
!!                (dens,velx,vely,velz,pres,(magx,magy,magz),gamc,game)
!!  dir         - x,y,z direction
!!  cons        - A logical switch to choose either primitive or conservative eigenvector
!!  C_fast      - Fast magnetoacoustic speed for MHD/Sound speed for Hydro
!!  C_alfn      - Alfven speed (needed for MHD only)
!!  C_slow      - Slow magnetoacoustic speed (needed for MHD only)
!!  A_f         - Normalization coefficient (needed for MHD only)
!!  A_s         - Normalization coefficient (needed for MHD only)
!!  B_beta      - Alfven velcoities in transversal direction (needed for MHD only)
!!
!!***

Subroutine hy_eigenVector&
     (LeftEigvec,RightEigvec,V,dir,cons,C_fast,C_alfn,C_slow,A_f,A_s,B_beta)

  implicit none

#include "Simulation.h"
#include "constants.h"
#include "UHD.h"

  !! Arguments type declaration ---------------------------------------
  real, dimension(HY_VARINUM,HY_WAVENUM), intent(OUT) :: LeftEigvec
  real, dimension(HY_VARINUM,HY_WAVENUM), intent(OUT) :: RightEigvec
  real, dimension(HY_VARINUM2), intent(IN) :: V
  integer, intent(IN) :: dir
  logical, intent(IN) :: cons
  real,    intent(IN) :: C_fast
  real,    intent(IN), optional :: C_alfn,C_slow,A_f,A_s
  real, dimension(MDIM), intent(IN), optional  :: B_beta
  !! ------------------------------------------------------------------

End Subroutine hy_eigenVector
