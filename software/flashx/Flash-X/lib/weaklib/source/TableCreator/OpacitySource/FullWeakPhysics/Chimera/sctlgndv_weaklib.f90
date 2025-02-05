SUBROUTINE sctlgndv_weaklib &
           ( e_in, e_out, eta, hot0i, hot0ii, hot1i, hot1ii )
!-----------------------------------------------------------------------
!
!    Author:   R. Chu, Dept. Phys. & Astronomy
!              U. Tennessee, Knoxville
!
!    Created:  12/04/2018
!
!    Purpose:
!      To integrate the quantities
!
!          f(e)*(1.-f(e+w-wp))*hl  (l = i,ii) (out)
!
!  e                :     (electron energy)/kt    (integration variable)
!  w                :     (in beam neutrino energy)/kt
!  wp               :     (out beam neutrino energy)/kt
!  eta              :     (electron chemical potential - mc2)/kt
!
!    Subprograms called:
!  w_gt_wp_e_lt_wp  : to integrate from a to b for the case w > wp, e < wp
!  w_gt_wp_e_gt_wp  : to integrate from b to c for the case w > wp, e > wp
!  w_gt_wp_e_to_inf : to integrate from c to infinity for the case w > wp
!  w_lt_wp_e_gt_wp  : to integrate from a to b for the case w < wp, e < wp
!  w_lt_wp_e_lt_wp  : to integrate from b to c for the case w < wp e > wp
!  w_lt_wp_e_to_inf : to integrate from c to infinity for the case w < wp
!
!    Input arguments:
!  w                :     (in beam neutrino energy)/kt
!  wp               :     (out beam neutrino energy)/kt
!  eta              :     (electron chemical potential - mc2)/kt
!
!    Output arguments:
!  hot0i            : zero moment of the "i" out neutrino scattering function
!  hot0ii           : zero moment of the "ii" out neutrino scattering function
!  hot1i            : first moment of the "i" out neutrino scattering function
!  hot1ii           : first moment of the "ii" out neutrino scattering function
!
!-----------------------------------------------------------------------

USE kind_module
USE numerical_module, ONLY: zero, half, one

USE nes_module

IMPLICIT none

!-----------------------------------------------------------------------
!        Input variables.
!-----------------------------------------------------------------------

REAL(double), INTENT(in)    :: e_in          !(in beam neutrino energy)/kt
REAL(double), INTENT(in)    :: e_out         !(out beam neutrino energy)/kt

!-----------------------------------------------------------------------
!        Output variables.
!-----------------------------------------------------------------------

REAL(double), INTENT(out)   :: hot0i         ! zero moment of outgoing scattering function type i
REAL(double), INTENT(out)   :: hot0ii        ! zero moment of outgoing scattering function type ii
REAL(double), INTENT(out)   :: hot1i         ! first moment of outgoing scattering function type i
REAL(double), INTENT(out)   :: hot1ii        ! first moment of outgoing scattering function type ii

!-----------------------------------------------------------------------
!        Local variables
!-----------------------------------------------------------------------

REAL(double)                :: h0i           ! partial integral
REAL(double)                :: h0ii          ! partial integral
REAL(double)                :: h1i           ! partial integral
REAL(double)                :: h1ii          ! partial integral

REAL(double)                :: eta           ! (electron chemical potential)/kT
REAL(double)                :: etap          ! eta - w + wp
REAL(double)                :: exd           ! in-scattering out-scattering ratio
REAL(double), EXTERNAL      :: fexp          ! exponential

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  Initialize
!-----------------------------------------------------------------------

w                  = e_in
wp                 = e_out
etap               = eta - w + wp

w_wp               = w - wp
wp_w               = - w_wp
w_wp2              = w_wp * w_wp
w_wp3              = w_wp * w_wp2
w_wp4              = w_wp * w_wp3
w_wp5              = w_wp * w_wp4

w2                 = w * w
w3                 = w * w2
w4                 = w * w3
w5                 = w * w4
w6                 = w * w5

wp2                = wp * wp
wp3                = wp * wp2
wp4                = wp * wp3
wp5                = wp * wp4
wp6                = wp * wp5

wwp                = w * wp
w2pwp2             = half * ( w2 + wp2 )

hot0i              = zero
hot0ii             = zero
hot1i              = zero
hot1ii             = zero
h0i                = zero
h0ii               = zero
h1i                = zero
h1ii               = zero

!-----------------------------------------------------------------------
!  w > wp
!-----------------------------------------------------------------------

IF ( w >= wp ) THEN

  IF ( eta <= zero ) THEN ! 0 < wp < infty
  
    CALL w_gt_wp_e_lt_wp( zero, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_to_inf( wp, eta, h0i, h0ii, h1i, h1ii )
  
  ELSE IF ( etap <= zero  .and.  eta <= wp ) THEN ! 0 < eta < wp < infty
  
    CALL w_gt_wp_e_lt_wp( zero, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_lt_wp( eta, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_to_inf( wp, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( etap <= zero  .and.  eta > wp ) THEN ! 0 < wp < eta < infty
  
    CALL w_gt_wp_e_lt_wp( zero, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_gt_wp( wp, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_to_inf( eta, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( eta <= wp ) THEN ! 0 < etap < eta < wp < infty
  
    CALL w_gt_wp_e_lt_wp( zero, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_lt_wp( etap, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_lt_wp( eta, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_to_inf( wp, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( etap <= wp ) THEN ! 0 < etap < wp < eta < infty
  
    CALL w_gt_wp_e_lt_wp( zero, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_lt_wp( etap, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_gt_wp( wp, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_to_inf( eta, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( wp < etap ) THEN ! 0 < wp < etap < eta < infty
  
    CALL w_gt_wp_e_lt_wp( zero, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_gt_wp( wp, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_gt_wp( etap, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_gt_wp_e_to_inf( eta, eta, h0i, h0ii, h1i, h1ii )
    
  END IF

  exd              = fexp(-w_wp)
  hot0i            = h0i
  hot0ii           = h0ii
  hot1i            = h1i
  hot1ii           = h1ii
  
  RETURN

!-----------------------------------------------------------------------
!  w < wp
!-----------------------------------------------------------------------
  
ELSE

  IF ( eta <= wp_w ) THEN ! wp_w < wp < infty
  
    CALL w_lt_wp_e_lt_wp( wp_w, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_to_inf( wp, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( eta <= wp_w  .and.  etap <= wp ) THEN ! wp_w < etap < wp < infty
  
    CALL w_lt_wp_e_lt_wp( wp_w, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_lt_wp( etap, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_to_inf( wp, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( eta <= wp_w .and.  etap > wp ) THEN ! wp_w < wp < etap < infty
  
    CALL w_lt_wp_e_lt_wp( wp_w, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_gt_wp( wp, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_to_inf( etap, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( etap <= wp ) THEN ! wp_w < eta < etap < wp < infty
  
    CALL w_lt_wp_e_lt_wp( wp_w, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_lt_wp( eta, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_lt_wp( etap, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_to_inf( wp, eta, h0i, h0ii, h1i, h1ii )
    
  ELSE IF ( eta <= wp ) THEN ! wp_w < eta < wp < etap < infty
  
    CALL w_lt_wp_e_lt_wp( wp_w, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_lt_wp( eta, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_gt_wp( wp, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_to_inf( etap, eta, h0i, h0ii, h1i, h1ii )
   
  ELSE IF ( wp < eta ) THEN ! wp_w < wp < eta < etap < infty
  
    CALL w_lt_wp_e_lt_wp( wp_w, wp, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_gt_wp( wp, eta, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_gt_wp( eta, etap, eta, h0i, h0ii, h1i, h1ii )
    CALL w_lt_wp_e_to_inf( etap, eta, h0i, h0ii, h1i, h1ii )
    
  END IF

  exd              = fexp(w_wp)
  hot0i            = h0i  * exd
  hot0ii           = h0ii * exd
  hot1i            = h1i  * exd
  hot1ii           = h1ii * exd
  
END IF
 
RETURN
END SUBROUTINE sctlgndv_weaklib
