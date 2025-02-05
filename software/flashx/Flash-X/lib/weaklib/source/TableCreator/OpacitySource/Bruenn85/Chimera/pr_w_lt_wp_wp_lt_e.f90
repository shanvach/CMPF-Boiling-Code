SUBROUTINE pr_w_lt_wp_wp_lt_e( xl, xu, eta, beta, j0i, j0ii, j1i, j1ii )
!-----------------------------------------------------------------------
!
!    File:         pr_w_lt_wp_wp_lt_e
!    Module:       pr_w_lt_wp_wp_lt_e
!    Type:         Subprogram
!    Author:       S. W. Bruenn, Dept of Physics, FAU,
!                  Boca Raton, FL 33431-0991
!
!    Date:         7/05/03
!
!    Purpose:
!      To integrate for the case w < w', w' < e < w + w', the quantities
!
!          Fe(Ee)*Febar(Enu + Enubar - Ee)*Phi(Enu,Enubar,Ee)
!
!      and
!
!          [1-Fe(Ee)]*[1-Febar(Enu + Enubar - Ee)]*Phi(Enu,Enubar,Ee)
!
!  e       : (electron energy)/kt    (integration variable)
!  w       : (in beam neutrino energy)/kt
!  wp      : (out beam neutrino energy)/kt
!  eta     : (electron chemical potential - mc2)/kt
!
!    Subprograms called:
!      none
!
!    Input arguments:
!  xl      : lower limit of integration
!  eta     : (electron chemical potential - mc2)/kt
!
!    Output arguments:
!  j0i     : zero moment of the "i" in neutrino scattering function
!  j0ii    : zero moment of the "ii" in neutrino scattering function
!  j1i     : first moment of the "i" in neutrino scattering function
!  j1ii    : first moment of the "ii" in neutrino scattering function
!
!    Variables that must be passed through common:
!      none
!
!    Modules:
!  kind_module, numerical_module
!  nes_module
!
!-----------------------------------------------------------------------

USE kind_module, ONLY: double
USE numerical_module, ONLY: zero, half, one

USE nes_module
USE pair_module, ONLY: nleg, xe, wte

IMPLICIT none

!-----------------------------------------------------------------------
!        Input variables.
!-----------------------------------------------------------------------

REAL(KIND=double), INTENT(in)      :: xl            ! lower limit of integration
REAL(KIND=double), INTENT(in)      :: xu            ! upper limit of integration
REAL(KIND=double), INTENT(in)      :: eta           ! (electron chemical potential - mc2)/kt
REAL(KIND=double), INTENT(in)      :: beta          ! 1/kt

!-----------------------------------------------------------------------
!        Input-Output variables.
!-----------------------------------------------------------------------

REAL(KIND=double), INTENT(inout)   :: j0i           ! partial integral contributed by this subroutine
REAL(KIND=double), INTENT(inout)   :: j0ii          ! partial integral contributed by this subroutine
REAL(KIND=double), INTENT(inout)   :: j1i           ! partial integral contributed by this subroutine
REAL(KIND=double), INTENT(inout)   :: j1ii          ! partial integral contributed by this subroutine

!-----------------------------------------------------------------------
!        Local variables
!-----------------------------------------------------------------------

INTEGER                            :: i             ! summation index

REAL(KIND=double)                  :: h0i           ! integrand of zero moment of pair function i
REAL(KIND=double)                  :: h0ii          ! integrand of zero moment of pair function ii

REAL(KIND=double)                  :: su0i          ! partial integral
REAL(KIND=double)                  :: su0ii         ! partial integral
REAL(KIND=double)                  :: su1i          ! partial integral
REAL(KIND=double)                  :: su1ii         ! partial integral

REAL(KIND=double)                  :: e_mid         ! midpoint energy
REAL(KIND=double)                  :: e_del         ! half the energy width
REAL(KIND=double)                  :: e_var         ! scaled integration point

REAL(KIND=double)                  :: arg1          ! argument of an exponential
REAL(KIND=double)                  :: arg2          ! argument of an exponential
REAL(KIND=double)                  :: exp1          ! value of an exponential
REAL(KIND=double)                  :: exp2          ! value of an exponential
REAL(KIND=double)                  :: eblock        ! blocking factor for neutrino-antineutrino annihilation

REAL(KIND=double)                  :: fexp          ! exponential

EXTERNAL fexp

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Return if xl = xu
!-----------------------------------------------------------------------

IF ( xl == xu ) RETURN

!-----------------------------------------------------------------------
!  Initialize for integration
!-----------------------------------------------------------------------

su0i               = zero
su0ii              = zero
su1i               = zero
su1ii              = zero

e_mid              = half * ( xu + xl )
e_del              = half * ( xu - xl )

CALL gquad( nleg, xe, wte, nleg )

!-----------------------------------------------------------------------
!  Integrate
!-----------------------------------------------------------------------

DO i = 1,nleg

  e_var            = xe(i) * e_del
  e                = e_mid + e_var

  arg1             = beta * e - eta
  exp1             = fexp(arg1)
  arg2             = beta * ( w_p_wp - e ) + eta
  exp2             = fexp(arg2)
  eblock           = ( exp1/( exp1 + one ) ) * ( exp2/( exp2 + one ) )

  h0i              = - r4_15 * e**5 + r4_3 * e**4 * wp - r8_3 * e**3 * wp2 + r8_3 * e**2 * ( w3 + wp3 ) &
&                  - r4_3 * e * w_p_wp2 * ( wp2 - 2.d0 * wwp + 3.d0 * w2 ) + r4_15 * w_p_wp3 * ( wp2 - 3.d0 * wwp + 6.d0 * w2 )
  h0ii             = - r4_15 * e**5 + r4_3 * e**4 * w - r8_3 * e**3 * w2 + r8_3 * e**2 * ( w3 + wp3 ) &
&                  - r4_3 * e * w_p_wp2 * ( w2 - 2.d0 * wwp + 3.d0 * wp2 ) + r4_15 * w_p_wp3 * ( w2 - 3.d0 * wwp + 6.d0 * wp2 )

  su0i             = su0i  + eblock * h0i  * wte(i)
  su0ii            = su0ii + eblock * h0ii * wte(i)

END DO

su0i               = e_del * su0i
su0ii              = e_del * su0ii
su1i               = e_del * su1i
su1ii              = e_del * su1ii


j0i                = j0i  + su0i
j0ii               = j0ii + su0ii
j1i                = j1i  + su1i
j1ii               = j1ii + su1ii

RETURN
END SUBROUTINE pr_w_lt_wp_wp_lt_e
