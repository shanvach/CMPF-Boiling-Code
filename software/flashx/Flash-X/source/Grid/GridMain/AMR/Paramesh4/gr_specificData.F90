!!****if* source/Grid/GridMain/paramesh/Grid_data
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
!!  Grid_data
!!
!! SYNOPSIS
!!
!!  use Grid_data
!!
!! DESCRIPTION 
!!  
!!  This includes the global integer identifier for a block, the grid geometry information
!!  
!!  
!! 
!! CREATE AD:04/12/04
!!
!! 
!!   Defining data structures for storing paramesh related infomation.  
!!   including function for updating the grid information
!!
!! MODIFIED AD:05/19/04
!!   
!!***

!!REORDER(5):scratch, scratch_ctr, scratch_facevar[xyz], gr_[xyz]flx
!!REORDER(5): gr_flx[xyz]
!!REORDER(5):gr_xflx_[yz]face, gr_yflx_[xz]face, gr_zflx_[xy]face

Module gr_specificData

  implicit none

#include "constants.h"
#include "Simulation.h"

  integer, save :: gr_iloGc = GRID_ILO_GC
  integer, save :: gr_ihiGc = GRID_IHI_GC
  integer, save :: gr_jloGc = GRID_JLO_GC
  integer, save :: gr_jhiGc = GRID_JHI_GC
  integer, save :: gr_kloGc = GRID_KLO_GC
  integer, save :: gr_khiGc = GRID_KHI_GC

  integer, save :: gr_ilo = GRID_ILO
  integer, save :: gr_ihi = GRID_IHI
  integer, save :: gr_jlo = GRID_JLO
  integer, save :: gr_jhi = GRID_JHI
  integer, save :: gr_klo = GRID_KLO
  integer, save :: gr_khi = GRID_KHI

  integer, parameter :: gr_iloFl = GRID_ILO   ! -2  for staggered mesh MHD
  integer, parameter :: gr_ihiFl = GRID_IHI   ! +2  for staggered mesh MHD
  integer, parameter :: gr_jloFl = GRID_JLO   ! -2*K2D  for staggered mesh MHD
  integer, parameter :: gr_jhiFl = GRID_JHI   ! +2*K2D  for staggered mesh MHD
  integer, parameter :: gr_kloFl = GRID_KLO   ! -2*K3D  for staggered mesh MHD
  integer, parameter :: gr_khiFl = GRID_KHI   ! +2*K3D  for staggered mesh MHD

#ifdef USE_LEVELWIDE_FLUXES
  integer, parameter, dimension(3) :: gr_loFl = (/gr_iloFl,gr_jloFl,gr_kloFl/)
#endif

!! Define the block information

!  Type define

  type gridBlock
     !!cornerID is integer coordinates of the lower left cornor
     !! (ie the smallest point) of a block
     integer,dimension(MDIM) :: cornerID
     !! atmost 2 neighbors, 2faces along
     !! each dimension, hence.
     real,dimension(3,GRID_IHI_GC) :: firstAxisCoords
     real,dimension(3,GRID_JHI_GC) :: secondAxisCoords
     real,dimension(3,GRID_KHI_GC) :: thirdAxisCoords
  end type gridBlock

  type(gridBlock),save,dimension(MAXBLOCKS),target :: gr_oneBlock

#ifdef BSS_GRID_ARRAYS
# if NSCRATCH_GRID_VARS > 0
  real,target,dimension(SCRATCH_GRID_VARS_BEGIN:SCRATCH_GRID_VARS_END,&
                        GRID_IHI_GC+1,&
                        GRID_JHI_GC+1,&
                        GRID_KHI_GC+1,&
                        MAXBLOCKS) :: scratch
# else
  real,target,dimension(1,1,1,1,1) :: scratch
# endif

# if NSCRATCH_CENTER_VARS > 0
  real,target,dimension(SCRATCH_CENTER_VARS_BEGIN:SCRATCH_CENTER_VARS_END,&
                        GRID_IHI_GC,&
                        GRID_JHI_GC,&
                        GRID_KHI_GC,&
                        MAXBLOCKS) :: scratch_ctr
# else
  real,target,dimension(1,1,1,1,1):: scratch_ctr
# endif

# if NSCRATCH_FACEX_VARS > 0
  real,target,dimension(SCRATCH_FACEX_VARS_BEGIN:SCRATCH_FACEX_VARS_END,&
                        GRID_IHI_GC+1,&
                        GRID_JHI_GC,  &
                        GRID_KHI_GC,  &
                        MAXBLOCKS) :: scratch_facevarx
# else
  real, target,dimension(1,1,1,1,1):: scratch_facevarx
# endif

# if NSCRATCH_FACEY_VARS > 0
  real,target,dimension(SCRATCH_FACEY_VARS_BEGIN:SCRATCH_FACEY_VARS_END,&
                        GRID_IHI_GC,  &
                        GRID_JHI_GC+1,&
                        GRID_KHI_GC,  &
                        MAXBLOCKS) :: scratch_facevary
# else
  real, target,dimension(1,1,1,1,1):: scratch_facevary
# endif

# if NSCRATCH_FACEZ_VARS > 0
  real,target,dimension(SCRATCH_FACEZ_VARS_BEGIN:SCRATCH_FACEZ_VARS_END,&
                        GRID_IHI_GC,  &
                        GRID_JHI_GC,  &
                        GRID_KHI_GC+1,&
                        MAXBLOCKS) :: scratch_facevarz
# else
  real, target,dimension(1,1,1,1,1):: scratch_facevarz
# endif
# if (NFLUXES>0) && defined(USE_LEVELWIDE_FLUXES)
  real,target,dimension(NFLUXES,&
                        gr_iloFl:gr_ihiFl+1,  &
                        gr_jloFl:gr_jhiFl,  &
                        gr_kloFl:gr_khiFl,&
                        MAXBLOCKS) :: gr_flxx
  real,target,dimension(NFLUXES,&
                        gr_iloFl:gr_ihiFl,  &
                        gr_jloFl:gr_jhiFl+K2D,  &
                        gr_kloFl:gr_khiFl,&
                        MAXBLOCKS) :: gr_flxy
  real,target,dimension(NFLUXES,&
                        gr_iloFl:gr_ihiFl,  &
                        gr_jloFl:gr_jhiFl,  &
                        gr_kloFl:gr_khiFl+K3D,&
                        MAXBLOCKS) :: gr_flxz
# else
  real, target,dimension(1,1,1,1,1):: gr_flxx, gr_flxy, gr_flxz
# endif

#else
  real, save, target, allocatable :: scratch(:,:,:,:,:)
  real, save, target, allocatable :: scratch_ctr(:,:,:,:,:)
  real, save, target, allocatable :: scratch_facevarx(:,:,:,:,:)
  real, save, target, allocatable :: scratch_facevary(:,:,:,:,:)
  real, save, target, allocatable :: scratch_facevarz(:,:,:,:,:)
  real, save, target, allocatable :: gr_flxx(:,:,:,:,:)
  real, save, target, allocatable :: gr_flxy(:,:,:,:,:)
  real, save, target, allocatable :: gr_flxz(:,:,:,:,:)
#endif

  integer ,save :: gr_nblockX, gr_nblockY, gr_nblockZ
  integer ,dimension(MAXBLOCKS), save :: gr_blkList
  integer ,save :: gr_finestExistingLevel = 0

  !below values needed to make data structures for IO output
  integer,save,allocatable,target,dimension(:,:) :: gr_gid  !holds neigh, child, parent info for checkpoint files
  integer, save, allocatable :: gr_nToLeft(:) !holds 

#ifdef BSS_GRID_ARRAYS
  !For flux conservation
  real,save, dimension(NFLUXES,2,NYB,NZB,MAXBLOCKS) :: gr_xflx
  real,save, dimension(NFLUXES,NXB,2,NZB,MAXBLOCKS) :: gr_yflx
  real,save, dimension(NFLUXES,NXB,NYB,2,MAXBLOCKS) :: gr_zflx

  !For unsplit hydro/MHD to store transverse fluxes on AMR
# ifdef FLASH_HYDRO_UNSPLIT
#  if NDIM >= 2
  real,save, dimension(NFLUXES,2:NXB, 2   ,NZB  ,MAXBLOCKS) :: gr_xflx_yface
  real,save, dimension(NFLUXES,2    ,2:NYB,NZB  ,MAXBLOCKS) :: gr_yflx_xface
#   if NDIM == 3
  real,save, dimension(NFLUXES,2:NXB,NYB  , 2   ,MAXBLOCKS) :: gr_xflx_zface
  real,save, dimension(NFLUXES,NXB,  2:NYB, 2   ,MAXBLOCKS) :: gr_yflx_zface
  real,save, dimension(NFLUXES, 2 ,NYB    ,2:NZB,MAXBLOCKS) :: gr_zflx_xface
  real,save, dimension(NFLUXES,NXB, 2     ,2:NZB,MAXBLOCKS) :: gr_zflx_yface
#   endif
#  endif
# endif
#else
  !For flux conservation
  real, save, allocatable :: gr_xflx(:,:,:,:,:)
  real, save, allocatable :: gr_yflx(:,:,:,:,:)
  real, save, allocatable :: gr_zflx(:,:,:,:,:)

  !For unsplit hydro/MHD to store transverse fluxes on AMR
# ifdef FLASH_HYDRO_UNSPLIT
#  if NDIM >= 2
  real,save, allocatable :: gr_xflx_yface(:,:,:,:,:)
  real,save, allocatable :: gr_yflx_xface(:,:,:,:,:)
#   if NDIM == 3
  real,save, allocatable :: gr_xflx_zface(:,:,:,:,:)
  real,save, allocatable :: gr_yflx_zface(:,:,:,:,:)
  real,save, allocatable :: gr_zflx_xface(:,:,:,:,:)
  real,save, allocatable :: gr_zflx_yface(:,:,:,:,:)
#   endif
#  endif
# endif
#endif

  logical, save :: gr_enableMaskedGCFill
  integer, save :: gr_sanitizeDataMode, gr_sanitizeVerbosity
  integer,save :: gr_globalOffset !stores beginning blk offset for a proc

  !A global surr_blks array (gsurr_blks) only makes sense for Paramesh 3 and 4
  !because surr_blks does not exist in Paramesh 2.  We write gsurr_blks
  !to file for Paramesh 3 and 4 and read gsurr_blks from file for Paramesh 4dev
  !with FLASH optimizations.
  integer,save,allocatable,target,dimension(:,:,:,:,:) :: gr_gsurr_blks
  logical,save :: gr_is_gsurr_blks_initialized = .false.

  logical,save :: gr_gidIsValid = .FALSE.

  logical,save :: gr_haveInternalBoundaryBlocks ! see gr_createDomain
end Module gr_specificData
