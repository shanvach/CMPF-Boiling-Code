!!****if* source/Grid/GridMain/paramesh/gr_sanitizeDataAfterInterp
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
!!  gr_sanitizeDataAfterInterp
!!
!!
!! SYNOPSIS
!!
!!  call gr_sanitizeDataAfterInterp(integer(in)          :: ntype,
!!                                  character(len=*)(in) :: info,
!!                                  integer(in)          :: layers(MDIM))
!!
!!
!!
!! DESCRIPTION
!!
!!  Given a block node type specification, loop over all of
!!  the matching blocks and check whether matching solution data in certain
!!  variableslie in a reasonable range of values.
!!
!!  Energies (ENER_VAR and EINT_VAR) are expected to be .ge. gr_smalle,
!!  and the density (DENS_VAR) is expected to be .ge. gr_smallrho,
!!  where gr_smalle and gr_smallrho are lower bounds coming from the
!!  runtime parameters smalle and smlrho, respectively.
!!
!!  For data that do satisfy these expectations, warning messages are
!!  generated, but the offending data is not modified.
!!
!! ARGUMENTS
!!
!!   ntype - the node type of blocks to iterate over (e.g. LEAF, ACTIVE_BLKS)
!!
!!   layers - number of guardcell layers to be included in the check 
!!
!! NOTES
!!
!!  The checks are based on gr_conserveToPrimitive, which is called to
!!  convert solution data back from conserved form when using the old
!!  (convertToConsvdForMeshCalls) way of ensuring that the mesh
!!  handles data interpolation in conserved form.
!!
!!  This is meant to be called where gr_conserveToPrimitive used to be
!!  called when using the new (convertToConsvdInMeshInterp) way of
!!  ensuring that the mesh handles data interpolation in conserved
!!  form.
!!
!! SEE ALSO
!!
!!  gr_conserveToPrimitive
!!
!!
!! BUGS
!!
!!  This routine accesses the global variable storage 
!!  array unk directly.  It won't work for data stored
!!  in the paramesh workspace array WORK. It won't work
!!  for the Uniform Grid (its functionality is currently
!!  not needed there). 
!!
!!***

!!REORDER(5): unk

#define DEBUG_CONSCONV

subroutine gr_sanitizeDataAfterInterp(ntype, info, layers)

  use Grid_interface, ONLY : Grid_getTileIterator, &
                             Grid_releaseTileIterator
  use Grid_data, ONLY : gr_smallrho,gr_smalle, gr_meshMe
  use gr_specificData, ONLY : gr_sanitizeDataMode, gr_sanitizeVerbosity
  use Logfile_interface, ONLY : Logfile_stamp
  use physicaldata, ONLY:unk, gcell_on_cc
  use tree, ONLY:nodetype
  use paramesh_dimensions, ONLY: il_bnd,iu_bnd,jl_bnd,ju_bnd,kl_bnd,ku_bnd, kl_bndi, ndim
  use Grid_iterator, ONLY : Grid_iterator_t
  use Grid_tile,     ONLY : Grid_tile_t

  implicit none
#include "constants.h"
#undef REAL_FORMAT
#define REAL_FORMAT "(1PG23.16)"
#include "Simulation.h"

  integer, intent(IN) :: ntype
  character(len=*), intent(IN) :: info
  integer,dimension(MDIM), intent(IN):: layers
  integer :: n, blockID

  integer ::  i,j
  integer :: iskip, jskip, kskip
  integer :: il,iu,jl,ju,kl,ku
  integer :: kwrite,locs(3),kReorder(1:ku_bnd-kl_bnd+1),nReorder
  character(len=32), dimension(4,2) :: block_buff
  character(len=32)                 :: number_to_str
  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc

111 format (a,a,a1,(1x,a18,'=',a),(1x,a2,'=',a5),(1x,a5,'=',a),(1x,a4,'=',a))
112 format (i3,1x,24(1x,1G8.2))
113 format (' :,',i2,',',i2,1x,24(1x,1G8.2))

  if (gr_sanitizeDataMode == 0) return ! IMMEDIATE RETURN

  iskip = NGUARD - layers(IAXIS)
  jskip = (NGUARD - layers(JAXIS)) * K2D
  kskip = (NGUARD - layers(KAXIS)) * K3D
  il = il_bnd + iskip
  iu = iu_bnd - iskip
  jl = jl_bnd + jskip
  ju = ju_bnd - jskip
  kl = kl_bnd + kskip
  ku = ku_bnd - kskip

  nReorder = 0

  call Grid_getTileIterator(itor, ntype, tiling=.FALSE.)
  do while (itor%isValid())
     call itor%currentTile(tileDesc)
     blockID = tileDesc%id

#ifdef DENS_VAR
     if (gcell_on_cc(DENS_VAR) .OR. gr_sanitizeDataMode == 2) then
        ! small limits -- in case the interpolants are not monotonic
        if (any(unk(DENS_VAR,il:iu,jl:ju,kl:ku,blockID) .LT. gr_smallrho)) then
           kwrite = kl_bndi
           if (gr_sanitizeVerbosity .GE. 5 .AND. ndim==3) then
              call set_kReorder
!              print*,'kReorder(1:nReorder)',kReorder(1:nReorder)
              locs = minloc(unk(DENS_VAR,il:iu,jl:ju,kReorder(1:nReorder),blockID))
!              print*,'LOCS:',locs
              kwrite = kReorder(locs(3))
           end if
           write (block_buff(1,1), '(a18)') 'min. unk(DENS_VAR)'
           !        write (number_to_str, '('//REAL_FORMAT//',a1)') minval(unk(DENS_VAR,il:iu,jl:ju,kl:ku,blockID)), ','
           write (number_to_str, '(G30.22)') minval(unk(DENS_VAR,il:iu,jl:ju,kl:ku,blockID))
           write (block_buff(1,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(2,1), '(a)') 'PE'
           write (number_to_str, '(i7)') gr_meshMe
           write (block_buff(2,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(3,1), '(a)') 'block'
           write (number_to_str, '(i7)') blockID
           write (block_buff(3,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(4,1), '(a)') 'type'
           write (number_to_str, '(i7)') nodetype(blockID)
           write (block_buff(4,2), '(a)') trim(adjustl(number_to_str))

           if (gr_sanitizeVerbosity .GE. 1) call Logfile_stamp( block_buff, 4, 2, 'WARNING '//info)
           if (gr_sanitizeVerbosity .GE. 4) print 111, 'WARNING ',info,':', ((block_buff(i,j),j=1,2),i=1,4)

           if (gr_sanitizeVerbosity .GE. 5) then
              do j=ju_bnd,jl_bnd,-1
                 if (kwrite==kl_bndi) then
                 ! For 3D, this prints a slice at the lowest k index that is interior - KW
                    print 112, j, (unk(DENS_VAR,i,j,kwrite,blockID), i=il_bnd,iu_bnd)
                 else
                    print 113, j,kwrite, (unk(DENS_VAR,i,j,kwrite,blockID), i=il_bnd,iu_bnd)
                 end if
              end do
           end if

           if (gr_sanitizeDataMode == 3) then
              unk(DENS_VAR,il:iu,jl:ju,kl:ku,blockID) = max(gr_smallrho,unk(DENS_VAR,il:iu,jl:ju,kl:ku,blockID))
           end if
           if (gr_sanitizeDataMode == 4) call Driver_abort("DENS var below acceptable minimum")
        end if
     end if
     
#endif

#ifdef ENER_VAR               
     if (gcell_on_cc(ENER_VAR) .OR. gr_sanitizeDataMode == 2) then
        ! energy
        if (any(unk(ENER_VAR,il:iu,jl:ju,kl:ku,blockID) .LT. gr_smalle*0.999999999)) then
           kwrite = kl_bndi
           if (gr_sanitizeVerbosity .GE. 5 .AND. ndim==3) then
              call set_kReorder
              locs = minloc(unk(ENER_VAR,il:iu,jl:ju,kReorder(1:nReorder),blockID))
              kwrite = kReorder(locs(3))
           end if
           write (block_buff(1,1), '(a)') 'min. unk(ENER_VAR)'
           write (number_to_str, '('//REAL_FORMAT//')') minval(unk(ENER_VAR,il:iu,jl:ju,kl:ku,blockID))
           write (block_buff(1,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(2,1), '(a)') 'PE'
           write (number_to_str, '(i7)') gr_meshMe
           write (block_buff(2,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(3,1), '(a)') 'block'
           write (number_to_str, '(i7)') blockID
           write (block_buff(3,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(4,1), '(a)') 'type'
           write (number_to_str, '(i7)') nodetype(blockID)
           write (block_buff(4,2), '(a)') trim(adjustl(number_to_str))

           if (gr_sanitizeVerbosity .GE. 1) call Logfile_stamp( block_buff, 4, 2, 'WARNING '//info)
           if (gr_sanitizeVerbosity .GE. 4) print 111, 'WARNING ',info,':', ((block_buff(i,j),j=1,2),i=1,4)

           if (gr_sanitizeVerbosity .GE. 5) then
              do j=ju_bnd,jl_bnd,-1
                 if (kwrite==kl_bndi) then
                    print 112, j, (unk(ENER_VAR,i,j,kwrite,blockID), i=il_bnd,iu_bnd) 
                 else
                    print 113, j,kwrite, (unk(ENER_VAR,i,j,kwrite,blockID), i=il_bnd,iu_bnd)
                 end if
              end do
           end if

           if (gr_sanitizeDataMode == 3) then
              unk(ENER_VAR,il:iu,jl:ju,kl:ku,blockID) = max(gr_smalle,unk(ENER_VAR,il:iu,jl:ju,kl:ku,blockID))
           end if
           if (gr_sanitizeDataMode == 4) call Driver_abort("ENER var below acceptable minimum")
        end if
     end if
#endif
#ifdef EINT_VAR
     if (gcell_on_cc(EINT_VAR) .OR. gr_sanitizeDataMode == 2) then
        if (any(unk(EINT_VAR,il:iu,jl:ju,kl:ku,blockID) .LT. gr_smalle*0.999999999)) then
           kwrite = kl_bndi
           if (gr_sanitizeVerbosity .GE. 5 .AND. ndim==3) then
              call set_kReorder
              locs = minloc(unk(EINT_VAR,il:iu,jl:ju,kReorder(1:nReorder),blockID))
              kwrite = kReorder(locs(3))
           end if
           write (block_buff(1,1), '(a)') 'min. unk(EINT_VAR)'
           write (number_to_str, '('//REAL_FORMAT//')') minval(unk(EINT_VAR,il:iu,jl:ju,kl:ku,blockID))
           write (block_buff(1,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(2,1), '(a)') 'PE'
           write (number_to_str, '(i7)') gr_meshMe
           write (block_buff(2,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(3,1), '(a)') 'block'
           write (number_to_str, '(i7)') blockID
           write (block_buff(3,2), '(a)') trim(adjustl(number_to_str))

           write (block_buff(4,1), '(a)') 'type'
           write (number_to_str, '(i7)') nodetype(blockID)
           write (block_buff(4,2), '(a)') trim(adjustl(number_to_str))

           if (gr_sanitizeVerbosity .GE. 1) call Logfile_stamp( block_buff, 4, 2, 'WARNING '//info)
           if (gr_sanitizeVerbosity .GE. 4) print 111, 'WARNING ',info,':', ((block_buff(i,j),j=1,2),i=1,4)

           if (gr_sanitizeVerbosity .GE. 5) then
              do j=ju_bnd,jl_bnd,-1
                 if (kwrite==kl_bndi) then
                    print 112, j, (unk(EINT_VAR,i,j,kwrite,blockID), i=il_bnd,iu_bnd) 
                 else
                    print 113, j,kwrite, (unk(EINT_VAR,i,j,kwrite,blockID), i=il_bnd,iu_bnd)
                 end if
              end do
           end if

           if (gr_sanitizeDataMode == 3) then
              unk(EINT_VAR,il:iu,jl:ju,kl:ku,blockID) = max(gr_smalle,unk(EINT_VAR,il:iu,jl:ju,kl:ku,blockID))
           end if
           if (gr_sanitizeDataMode == 4) call Driver_abort("EINT var below acceptable minimum")
        end if
     end if
#endif

     call itor%next()
  end do

  call Grid_releaseTileIterator(itor)

  return 

contains
  subroutine set_kReorder
    integer :: i,j,k
    if (nReorder==0) then       !We need to do this only once per gr_sanitize... call.

       !  The following code sets
       !  kReorder = (/5,6,7,8,9,10,11,12,4,13,3,14,2,15,1,16/) ! for kl:ku = 1:16

       i = 0
       do k = kl, ku
          if (k>NGUARD .AND. k .LE. ku_bnd-NGUARD) then
             i = i+1
             kReorder(i) = k
          end if
       end do
       do j = 1, layers(KAXIS)
          k = NGUARD+1-j
          if (k .LE. NGUARD) then
             i = i+1
             kReorder(i) = k
          end if
          k = ku_bnd-NGUARD+j
          if (k > ku_bnd-NGUARD) then
             i = i+1
             kReorder(i) = k
          end if
       end do
       nReorder = i
    end if
  end subroutine set_kReorder

end subroutine gr_sanitizeDataAfterInterp
        
