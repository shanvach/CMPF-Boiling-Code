!!****if* source/Grid/GridMain/UG/gr_getBndBox
!!
!! NAME
!!  gr_getBndBox
!!
!! SYNOPSIS
!!
!! 
!!  gr_getBndBox(integer(IN)  :: real(OUT) :: boundBox(2, MDIM))
!!  
!! DESCRIPTION 
!!
!!  Gets the physical domain bounding box of the block identified 
!!  by blockId.  For each dimension the left (lower or forward) 
!!  physical coordinate of the block edge and the right (upper or back) 
!!  physical coordinate of the block edge is returned.  See arguments
!!  below for more detail.
!!
!! ARGUMENTS
!!
!!
!!  boundBox - returned array holding the boundBox coordinates in
!!             each dimension
!!
!!            for readability, in constants.h we define IAXIS = 1, JAXIS = 2, KAXIS = 3
!!
!!            boundBox(1,IAXIS) = left edge coordinate of block in x direction
!!            boundBox(2,IAXIS) = right edge coordinate of block in x direction
!!            boundBox(1,JAXIS) = top edge coordinate of block in y direction
!!            boundBox(2,JAXIS) = bottom edge coordinate of block in y direction
!!            boundBox(1,KAXIS) = front edge coordinate of block in z direction
!!            boundBox(2,KAXIS) = back edge coordinate of block in z direction
!!
!! EXAMPLE
!!  
!!   In 2 dimensions, if physical coordinates are ...
!!    
!!     ________________(0.5 1.0)
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |_______________ |
!!  (-0.5, 0.0)
!!
!!
!!
!!     boundBox(1, IAXIS) = -0.5
!!     boundBox(2, IAXIS) = 0.5
!!     boundBox(1, JAXIS) = 0.0
!!     boundBox(2, JAXIS) = 1.0
!!     boundBox(1, KAXIS) = 1 !returned as 1 because only 2 dims
!!     boundBox(1, KAXIS) = 1 !returned as 1 because only 2 dims
!!
!!
!!***


#ifdef DEBUG_ALL
#define DEBUG_GRID
#endif

subroutine gr_getBndBox(boundBox)

  implicit none


  real,dimension(2,3),intent(out) :: boundBox


  boundBox = 0.0

  return
end subroutine gr_getBndBox














