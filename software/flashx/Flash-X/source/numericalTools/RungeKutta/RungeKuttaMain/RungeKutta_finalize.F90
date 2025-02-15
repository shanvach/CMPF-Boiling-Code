!!****if* source/numericalTools/RungeKutta/RungeKuttaMain/RungeKutta_finalize
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
!!  RungeKutta_finalize
!!
!! SYNOPSIS
!! 
!!  call RungeKutta_finalize ()
!!
!! DESCRIPTION
!!
!!  Finalizes the RungeKutta unit.
!!
!! ARGUMENTS
!!
!!***

subroutine RungeKutta_finalize ()

  use RungeKutta_data, ONLY: rk_a, rk_b, rk_c

  implicit none
!
!
!   ...Deassociate all pointers.
!
!
  if (associated (rk_a)) nullify (rk_a)
  if (associated (rk_b)) nullify (rk_b)
  if (associated (rk_c)) nullify (rk_c)
!
!
!    ...Ready!
!
!
  return
end subroutine RungeKutta_finalize
