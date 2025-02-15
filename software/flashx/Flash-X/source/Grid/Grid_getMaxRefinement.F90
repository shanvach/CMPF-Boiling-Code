!!****f* source/Grid/Grid_getMaxRefinement
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
!!  Grid_getMaxRefinement
!!
!! SYNOPSIS
!!
!!  call Grid_getMaxRefinement(integer(OUT) :: maxRefinement,
!!                    OPTIONAL,integer(IN)  :: mode,
!!                    OPTIONAL,integer(IN)  :: scope,
!!                    OPTIONAL,integer(IN)  :: inputComm
!!                              )
!!
!! DESCRIPTION
!!
!!  This routine returns the maximum block refinement level in the grid.  
!!  Depending on the mode used (and modified by other optional arguments),
!!  the returned value represents the maximum refinement level that
!!  is either allowed, or is currently realized anywhere on the grid
!!  (or a subset of the grid).
!!
!!  We may have an AMR grid in which one portion of the grid is highly
!!  refined.  Here, it may be useful to determine globally the highest block
!!  refinement level that occurs, or can potentially occur during the
!!  simulation.
!!
!! ARGUMENTS
!!
!!  maxRefinement - Maximum refinement level returned.
!!
!!  inputComm - Input MPI communicator, only used if mode=4 and scope=2.
!!              Default - none.
!!  mode      - 1 for lrefine_max,
!!              2 for gr_maxRefine (if used) or lrefine_max,
!!              3 for gr_maxRefine,
!!              4 for existing blocks.
!!              Default is 3.
!!  scope     - scope only used if mode=4;
!!              1 for local to this MPI task,
!!              2 for tasks in inputComm,
!!             [3 for mesh communicator,]
!!             [4 for MPI_COMM_WORLD].
!!              Default is 3.
!!
!! NOTES
!! 
!!  Communicator argument allows us to compare a subset of processes.
!!  It also makes it explicit to the user that this routine must be 
!!  called by all processes in the passed communicator ... otherwise
!!  deadlock. This applies only for modes that require communication.
!!
!!  For a uniform grid implementation, the returned level will always
!!  be 1.
!!
!!  This routine differs from Grid_getMaxCommonRefinement is several ways:
!!   1. Grid_getMaxCommonRefinement looks for existing LEAF blocks with the
!!      smallest refinement level (actual), while
!!      Grid_getMaxRefinement looks for blocks with the highest
!!      refinement level (either actual or potential).
!!   2. Grid_getMaxRefinement has additional optional arguments to select
!!      modes and task subsets.
!!
!!   For the Amrex implementation, not all values for scope are supported.
!!   In the case mode=4 it is up to the AMReX function amrex_get_finest_level()
!!   whether actual communication takes place.
!!
!!   In the Paramesh4 implementation, for the common case mode=4,scope=3
!!   a value from the module variable gr_finestExistingLevel may be
!!   returned rather than performing actual MPI communication; this
!!   value is assumed to be a valid cached value set to the finest
!!   existing level if it is > 0. However, this subroutine does not
!!   modify gr_finestExistingLevel; such modification should be done
!!   outside of this routine whenever the refinement pattern changes.
!!***

subroutine Grid_getMaxRefinement(maxRefinement, mode, scope, inputComm)

  implicit none
  integer, intent(IN), OPTIONAL :: mode, scope
  integer, intent(IN), OPTIONAL :: inputComm
  integer, intent(OUT) :: maxRefinement

  maxRefinement = 0

end subroutine Grid_getMaxRefinement
