module test_mocsy
  use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
  use, intrinsic :: iso_fortran_env, only: error_unit
     USE mocsy_singledouble
   USE mocsy_constants
   USE mocsy_vars
   USE mocsy_derivauto
   use pic_test_helpers, only: is_equal
  implicit none 

  public :: collect_mocsy_suite


contains 

subroutine collect_mocsy_suite(testsuite)
  type(unittest_type), allocatable, intent(out) :: testsuite(:)

  testsuite = [ & 
    new_unittest("constants", test_constants), & 
    new_unittest("kzero", test_kzero), &
    new_unittest("derivauto", test_derivauto), &
    new_unittest("derivnum", test_derivnum) &
  ]

end subroutine collect_mocsy_suite

subroutine test_constants(error) 
type(error_type), allocatable, intent(out) :: error
!  For vars routine (called below)
!  "vars" Output variables:
   REAL(kind=rx), DIMENSION(100) :: ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD, rhoSW, p, tempis
!  "vars" Input variables
   INTEGER :: N
   REAL(kind=rx), DIMENSION(100) :: temp, sal, alk, dic, sil, phos, Patm, depth, lat
   REAL(kind=rx), DIMENSION(6,100) :: ph_deriv, pco2_deriv, fco2_deriv, co2_deriv, hco3_deriv, co3_deriv, OmegaA_deriv, OmegaC_deriv
   REAL(kind=rx) ::  gamma_DIC, gamma_Alk, beta_DIC, beta_Alk, omega_DIC, omega_Alk
!  "vars" Input options
   CHARACTER(10) :: optCON, optT, optP, optB, optKf, optK1K2,optGAS

  !> solubility of CO2 in seawater (Weiss, 1974), also known as K0
  REAL(kind=r8), DIMENSION(6) :: K0
  !> K1 for the dissociation of carbonic acid from Lueker et al. (2000) or Millero (2010), depending on optK1K2
  REAL(kind=r8), DIMENSION(6) :: K1
  !> K2 for the dissociation of carbonic acid from Lueker et al. (2000) or Millero (2010), depending on optK1K2
  REAL(kind=r8), DIMENSION(6) :: K2
  !> equilibrium constant for dissociation of boric acid 
  REAL(kind=r8), DIMENSION(6) :: Kb
  !> equilibrium constant for the dissociation of water (Millero, 1995)
  REAL(kind=r8), DIMENSION(6) :: Kw
  !> equilibrium constant for the dissociation of bisulfate (Dickson, 1990)
  REAL(kind=r8), DIMENSION(6) :: Ks
  !> equilibrium constant for the dissociation of hydrogen fluoride 
  !! either from Dickson and Riley (1979) or from Perez and Fraga (1987), depending on optKf
  REAL(kind=r8), DIMENSION(6) :: Kf
  !> solubility product for calcite (Mucci, 1983)
  REAL(kind=r8), DIMENSION(6) :: Kspc
  !> solubility product for aragonite (Mucci, 1983)
  REAL(kind=r8), DIMENSION(6) :: Kspa
  !> 1st dissociation constant for phosphoric acid (Millero, 1995)
  REAL(kind=r8), DIMENSION(6) :: K1p
  !> 2nd dissociation constant for phosphoric acid (Millero, 1995)
  REAL(kind=r8), DIMENSION(6) :: K2p
  !> 3rd dissociation constant for phosphoric acid (Millero, 1995)
  REAL(kind=r8), DIMENSION(6) :: K3p
  !> equilibrium constant for the dissociation of silicic acid (Millero, 1995)
  REAL(kind=r8), DIMENSION(6) :: Ksi
  !> total sulfate (Morris & Riley, 1966)
  REAL(kind=r8), DIMENSION(6) :: St
  !> total fluoride  (Riley, 1965)
  REAL(kind=r8), DIMENSION(6) :: Ft
  !> total boron
  !! from either Uppstrom (1974) or Lee et al. (2010), depending on optB
  REAL(kind=r8), DIMENSION(6) :: Bt

! Parameter arrays for correctness checking
real(kind=r8), parameter :: K0_ref(6) = [ &
    0.058223497769_r8, 0.050556962375_r8, 0.043899912278_r8, &
    0.038119424259_r8, 0.033100077664_r8, 0.028741649767_r8 ]

real(kind=r8), parameter :: K1_ref(6) = [ &
    0.000000814647_r8, 0.000000909626_r8, 0.000001014354_r8, &
    0.000001129668_r8, 0.000001256455_r8, 0.000001395656_r8 ]

real(kind=r8), parameter :: K2_ref(6) = [ &
    0.000000000444_r8, 0.000000000476_r8, 0.000000000511_r8, &
    0.000000000548_r8, 0.000000000589_r8, 0.000000000632_r8 ]

real(kind=r8), parameter :: Kb_ref(6) = [ &
    0.000000001303_r8, 0.000000001480_r8, 0.000000001679_r8, &
    0.000000001902_r8, 0.000000002152_r8, 0.000000002432_r8 ]

real(kind=r8), parameter :: Kw_ref(6) = [ &
    0.000000000000_r8, 0.000000000000_r8, 0.000000000000_r8, &
    0.000000000000_r8, 0.000000000000_r8, 0.000000000000_r8 ]

real(kind=r8), parameter :: Ks_ref(6) = [ &
    0.260528321264_r8, 0.281507695490_r8, 0.303598633484_r8, &
    0.326801140739_r8, 0.351108647315_r8, 0.376507557419_r8 ]

real(kind=r8), parameter :: Kf_ref(6) = [ &
    0.004094821803_r8, 0.004239407350_r8, 0.004384697209_r8, &
    0.004530132806_r8, 0.004675137446_r8, 0.004819119223_r8 ]

real(kind=r8), parameter :: Kspc_ref(6) = [ &
    0.000000429916_r8, 0.000000528309_r8, 0.000000646101_r8, &
    0.000000786357_r8, 0.000000952462_r8, 0.000001148108_r8 ]

real(kind=r8), parameter :: Kspa_ref(6) = [ &
    0.000000683037_r8, 0.000000829150_r8, 0.000001001682_r8, &
    0.000001204298_r8, 0.000001440941_r8, 0.000001715797_r8 ]

real(kind=r8), parameter :: K1p_ref(6) = [ &
    0.024768086450_r8, 0.026361992774_r8, 0.028025832471_r8, &
    0.029760061616_r8, 0.031564919496_r8, 0.033440413999_r8 ]

real(kind=r8), parameter :: K2p_ref(6) = [ &
    0.000000666884_r8, 0.000000736317_r8, 0.000000811186_r8, &
    0.000000891700_r8, 0.000000978047_r8, 0.000001070394_r8 ]

real(kind=r8), parameter :: K3p_ref(6) = [ &
    0.000000000454_r8, 0.000000000509_r8, 0.000000000569_r8, &
    0.000000000636_r8, 0.000000000709_r8, 0.000000000789_r8 ]

real(kind=r8), parameter :: Ksi_ref(6) = [ &
    0.000000000149_r8, 0.000000000169_r8, 0.000000000192_r8, &
    0.000000000218_r8, 0.000000000247_r8, 0.000000000279_r8 ]

real(kind=r8), parameter :: St_ref(6) = [ &
    0.028235434133_r8, 0.028235434133_r8, 0.028235434133_r8, &
    0.028235434133_r8, 0.028235434133_r8, 0.028235434133_r8 ]

real(kind=r8), parameter :: Ft_ref(6) = [ &
    0.000068324401_r8, 0.000068324401_r8, 0.000068324401_r8, &
    0.000068324401_r8, 0.000068324401_r8, 0.000068324401_r8 ]

real(kind=r8), parameter :: Bt_ref(6) = [ &
    0.000432602930_r8, 0.000432602930_r8, 0.000432602930_r8, &
    0.000432602930_r8, 0.000432602930_r8, 0.000432602930_r8 ]


!  Local variables:
   INTEGER :: i


!> Typical options for observations
   optCON  = 'mol/kg'  ! input concentrations are in MOL/KG
   optT    = 'Tinsitu' ! input temperature, variable 'temp' is actually IN SITU temp [°C]
   optP    = 'db'      ! input variable 'depth' is in 'DECIBARS'
   optB    = 'l10'
   optK1K2 = 'l'
   optKf   = 'dg'
   optGAS  = 'Pzero'

   DO i = 1,6
     temp(i)   = 2.0            !Can be "Potential temperature" or "In situ temperature" (see optT below)
     sal(i)    = 35.0           !Salinity (practical scale)
     alk(i)    = 2295.*1.e-6      ! Convert obs. S. Ocean ave surf ALK (umol/kg) to mocsy data units (mol/kg)
     dic(i)    = 2154.*1.e-6      ! Convert obs. S. Ocean ave surf DIC (umol/kg) to mocsy data units (mol/kg)
     sil(i)    = 0.
     phos(i)   = 0.
     depth(i) = real(i-1) * 1000. ! Vary depth from 0 to 5000 db by 1000 db
     Patm(i)   = 1.0            !Atmospheric pressure (atm)
     N = i
   END DO


!  Need to change to call latest version of constants & derivnum (to get constand
  call constants (K0, K1, K2, Kb, Kw, Ks, Kf, Kspc, Kspa,                   &
                     K1p, K2p, K3p, Ksi,                                    &
                     St, Ft, Bt,                                            &
                     temp, sal, Patm,                                       &
                     depth, lat, 6,                                         &
                     optT='Tinsitu', optP='db', optB='l10', optK1K2=optK1K2, optKf='dg',  &
                     optGAS='Pinsitu')

  call check(error, all(is_equal(K0, K0_ref)), .true., "K0 does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(K1, K1_ref)), .true., "K1 does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(K2, K2_ref)), .true., "K2 does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Kb, Kb_ref)), .true., "Kb does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Kw, Kw_ref)), .true., "Kw does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Ks, Ks_ref)), .true., "Ks does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Kf, Kf_ref)), .true., "Kf does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Kspc, Kspc_ref)), .true., "Kspc does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Kspa, Kspa_ref)), .true., "Kspa does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(K1p, K1p_ref)), .true., "K1p does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(K2p, K2p_ref)), .true., "K2p does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(K3p, K3p_ref)), .true., "K3p does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Ksi, Ksi_ref)), .true., "Ksi does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(St, St_ref)), .true., "St does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Ft, Ft_ref)), .true., "Ft does not match!")
  if(allocated(error)) return

  call check(error, all(is_equal(Bt, Bt_ref)), .true., "Bt does not match!")
  if(allocated(error)) return


end subroutine test_constants 


subroutine test_kzero(error)
  use mocsy_gasx
type(error_type), allocatable, intent(out) :: error
   INTEGER, PARAMETER :: n = 1
   real(r8), parameter :: k0_co2_ref   =    5.5850871408260290E-002_r8
   real(r8), parameter :: k0_n2o_ref   =    4.1320896614892258E-002_r8

!  Computed variables:
   REAL(kind=r8), DIMENSION(1) :: k0_co2, k0_n2o

!  Input variables
   REAL(kind=rx), DIMENSION(1) :: temp, sal

!  Input at standard T and S
!  temp(1)   = 25.0
!  sal(1)    = 35.0

   temp(1)   = 4.0
   sal(1)    = 34.0

   call kzero('co2',   temp, sal, n, k0_co2)
   call kzero('n2o',   temp, sal, n, k0_n2o)

   call check(error, is_equal(k0_co2_ref, k0_co2(1)), .true., "K0 is not equal!")
   if(allocated(error)) return
   call check(error, is_equal(k0_n2o_ref, k0_n2o(1)), .true., "K0 is not equal!")
   if(allocated(error)) return



end subroutine test_kzero

subroutine test_derivauto(error)
    type(error_type), allocatable, intent(out) :: error
    ! Parameter arrays for derivative correctness checking
real(r8), parameter :: H_deriv_ref(6) = [ &
    -0.000026065471_r8, 0.000028597342_r8, 0.000029356219_r8, &
    0.000001069695_r8, 0.000000000257_r8, 0.000000000211_r8 ]

real(r8), parameter :: pco2_deriv_ref(6) = [ &
    -1214835.375000000000_r8, 1487033.375000000000_r8, 1368207.500000000000_r8, &
    49855.355468750000_r8, 12.826700210571_r8, 8.356157302856_r8 ]

real(r8), parameter :: fco2_deriv_ref(6) = [ &
    -1210608.250000000000_r8, 1481859.125000000000_r8, 1363446.750000000000_r8, &
    49681.878906250000_r8, 12.795490264893_r8, 8.327081680298_r8 ]

real(r8), parameter :: co2_deriv_ref(6) = [ &
    -0.041508860886_r8, 0.050809405744_r8, 0.046749327332_r8, &
    0.001703472808_r8, 0.000000136219_r8, 0.000000227711_r8 ]

real(r8), parameter :: hco3_deriv_ref(6) = [ &
    -0.622426390648_r8, 1.575213670731_r8, 0.701007306576_r8, &
    0.025543617085_r8, -0.000000574614_r8, 0.000000934866_r8 ]

real(r8), parameter :: co3_deriv_ref(6) = [ &
    0.663934290409_r8, -0.626021921635_r8, -0.747755467892_r8, &
    -0.027247048914_r8, 0.000000438429_r8, -0.000001162563_r8 ]

real(r8), parameter :: omegaa_deriv_ref(6) = [ &
    10253.260742187500_r8, -9667.773437500000_r8, -11547.727539062500_r8, &
    -420.781280517578_r8, 0.016808891669_r8, -0.035806208849_r8 ]

real(r8), parameter :: omegac_deriv_ref(6) = [ &
    15855.789062500000_r8, -14950.383789062500_r8, -17857.570312500000_r8, &
    -650.702148437500_r8, 0.014105012640_r8, -0.062941417098_r8 ]


!  For vars routine (called below)
!  "vars" Output variables:
   REAL(kind=rx), DIMENSION(1) :: ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD, rhoSW, p, tempis
!  "vars" Input variables
   INTEGER :: N
   REAL(kind=rx), DIMENSION(1) :: temp, sal, alk, dic, sil, phos, Patm, depth, lat
   REAL(kind=rx), DIMENSION(6,1) :: ph_deriv, pco2_deriv, fco2_deriv, co2_deriv, &
                     hco3_deriv, co3_deriv, omegaa_deriv, omegac_deriv
!  "vars" Input options
   CHARACTER(10) :: optCON, optT, optP, optB, optKf, optK1K2


!  Local variables:
   INTEGER :: i
   REAL(kind=r8) :: H
   REAL(kind=r8), DIMENSION(6) :: H_deriv
   CHARACTER*4 :: invar(6)
   
   invar(1) = 'Alk '
   invar(2) = 'DIC '
   invar(3) = 'Phos'
   invar(4) = 'Sil '
   invar(5) = 'T   '
   invar(6) = 'S   '

!> Typical options for observations
   optCON  = 'mol/kg'  ! input concentrations are in MOL/KG
   optT    = 'Tinsitu' ! input temperature, variable 'temp' is actually IN SITU temp [°C]
   optP    = 'm'      ! input variable 'depth' is in 'DECIBARS'
   optB    = 'l10'
   optK1K2 = 'l'
   optKf   = 'dg'
!> Simple input data (with CONCENTRATION units typical for DATA)
!> (based on observed average surface concentrations from S. Ocean (south of 60°S)--GLODAP and WOA2009)
   DO i = 1,1
     temp(i)   = 18.0            !Can be "Potential temperature" or "In situ temperature" (see optT below)
     sal(i)    = 35.0           !Salinity (practical scale)
     alk(i)    = 2300.*1.e-6      ! Convert obs. S. Ocean ave surf ALK (umol/kg) to mocsy data units (mol/kg)
     dic(i)    = 2000.*1.e-6      ! Convert obs. S. Ocean ave surf DIC (umol/kg) to mocsy data units (mol/kg)
     sil(i)    = 60.*1.e-6
     phos(i)   = 2.*1.e-6
     depth(i)  = 0.
     Patm(i)   = 1.0            !Atmospheric pressure (atm)
     lat(i)    = 0.
     N = i
   END DO

   call vars(ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD, rhoSW, p, tempis,         &  ! OUTPUT
             temp, sal, alk, dic, sil, phos, Patm, depth, lat, 1,                             &  ! INPUT
             optCON, optT, optP, optB=optB, optK1K2=optK1K2, optKf=optKf    )

   call derivauto(ph_deriv, pco2_deriv, fco2_deriv, co2_deriv, hco3_deriv, co3_deriv,   &
                omegaa_deriv, omegac_deriv,                                             &
                temp, sal, alk, dic, sil, phos, Patm, depth, lat, 1,                    &
                optCON, optT, optP, optB=optB, optK1K2=optK1K2, optKf=optKf )

   !call set_precision(20)

    ! [H+] concentration
    H = 10.0**(-ph(1))
    ! derivative of [H+] deduced from that of pH
    H_deriv = - H * ph_deriv(:,1) * log(10.0)
    block 
        real(r8), dimension(6) :: buffer
        buffer = 0.0_r8
    call check(error, all(is_equal(H_deriv, H_deriv_ref)), .true., " H deriv does not match!")
    if(allocated(error)) return

    buffer = pco2_deriv(:,1)
    call check(error, all(is_equal(buffer, pco2_deriv_ref)), .true., " pCO2 deriv does not match!")
    if(allocated(error)) return

    buffer = fco2_deriv(:,1)
    call check(error, all(is_equal(buffer, fco2_deriv_ref)), .true., " fCO2 deriv does not match!")
    if(allocated(error)) return

    buffer = co2_deriv(:,1)
    call check(error, all(is_equal(buffer, co2_deriv_ref)), .true., " CO2 deriv does not match!")
    if(allocated(error)) return

    buffer = hco3_deriv(:,1)
    call check(error, all(is_equal(buffer, hco3_deriv_ref)), .true., " HCO3 deriv does not match!")
    if(allocated(error)) return

    buffer = co3_deriv(:,1)
    call check(error, all(is_equal(buffer, co3_deriv_ref)), .true., " CO3 deriv does not match!")
    if(allocated(error)) return

    buffer = omegaa_deriv(:,1)
    call check(error, all(is_equal(buffer, omegaa_deriv_ref)), .true., " OmegaA deriv does not match!")
    if(allocated(error)) return

    buffer = omegac_deriv(:,1)
    call check(error, all(is_equal(buffer, omegac_deriv_ref)), .true., " OmegaC deriv does not match!")
    if(allocated(error)) return
    end block

end subroutine test_derivauto

subroutine test_derivnum(error)
   USE mocsy_derivnum
    type(error_type), allocatable, intent(out) :: error 



!  Output variables:
   REAL(kind=rx), DIMENSION(1) :: h, ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD, rhoSW, p, tempis
!  derivative of "vars" Output variables:
   REAL(kind=rx), DIMENSION(1) :: dh_dx, dpco2_dx, dfco2_dx, dco2_dx, dhco3_dx, dco3_dx, dOmegaA_dx, dOmegaC_dx
!  Input variables
   REAL(kind=rx), DIMENSION(1) :: temp, sal, alk, dic, sil, phos, Patm, depth, lat
!  Input options
   CHARACTER(10) :: optCON, optT, optP, optB, optKf, optK1K2
   real(r8), dimension(13) :: h_deriv, pco2_deriv, fco2_deriv, co2_deriv, &
                     hco3_deriv, co3_deriv, omegaa_deriv, omegac_deriv
! Parameter arrays for numerical derivative correctness checking
real(r8), parameter :: H_deriv_num_ref(13) = [ &
    -0.000027370454_r8, 0.000030411615_r8, 0.000031863739_r8, &
    0.000001062092_r8, 0.000000000257_r8, 0.000000000212_r8, &
    0.000000000000_r8, 0.000245914416_r8, 5.781185150146_r8, &
    0.937451243401_r8, 3658.276123046875_r8, 0.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: pco2_deriv_num_ref(13) = [ &
    -1218969.625000000000_r8, 1485482.625000000000_r8, 1373305.500000000000_r8, &
    49844.394531250000_r8, 12.830508232117_r8, 8.357298851013_r8, &
    -8994.158203125000_r8, -238075264.000000000000_r8, 231731363840.000000000000_r8, &
    43654672384.000000000000_r8, 167346404589568.000000000000_r8, 0.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: fco2_deriv_num_ref(13) = [ &
    -1212416.000000000000_r8, 1478200.875000000000_r8, 1365676.125000000000_r8, &
    49590.085937500000_r8, 12.796609878540_r8, 8.326797485352_r8, &
    -8962.743164062500_r8, -237248736.000000000000_r8, 230940852224.000000000000_r8, &
    43502084096.000000000000_r8, 166846108008448.000000000000_r8, 0.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: co2_deriv_num_ref(13) = [ &
    -0.041406251490_r8, 0.050781250000_r8, 0.046839471906_r8, &
    0.001697689877_r8, 0.000000136384_r8, 0.000000227770_r8, &
    0.000000000000_r8, -8.134672164917_r8, 7918.082519531250_r8, &
    1491.787963867188_r8, 5710434.000000000000_r8, 0.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: hco3_deriv_num_ref(13) = [ &
    -0.625000000000_r8, 1.583333373070_r8, 0.698499321938_r8, &
    0.026192929596_r8, -0.000000549575_r8, 0.000000947441_r8, &
    0.000000000000_r8, 13.533281326294_r8, -81210.203125000000_r8, &
    22368.375000000000_r8, 85880160.000000000000_r8, 0.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: co3_deriv_num_ref(13) = [ &
    0.665624976158_r8, -0.628472208977_r8, -0.745793521404_r8, &
    -0.027284301817_r8, 0.000000440468_r8, -0.000001161446_r8, &
    0.000000000000_r8, -5.366018772125_r8, 73303.078125000000_r8, &
    -23858.212890625000_r8, -91486224.000000000000_r8, 0.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: omegaa_deriv_num_ref(13) = [ &
    10240.000000000000_r8, -9671.111328125000_r8, -11563.423828125000_r8, &
    -421.197845458984_r8, 0.016816737130_r8, -0.035811547190_r8, &
    0.000000000000_r8, -82850.460937500000_r8, 1131913728.000000000000_r8, &
    -368413504.000000000000_r8, -1412923260928.000000000000_r8, -4748266.000000000000_r8, &
    0.000000000000_r8 ]

real(r8), parameter :: omegac_deriv_num_ref(13) = [ &
    15872.000000000000_r8, -15018.666992187500_r8, -17881.582031250000_r8, &
    -651.664611816406_r8, 0.014036016539_r8, -0.062908500433_r8, &
    0.000000000000_r8, -128149.984375000000_r8, 1750222080.000000000000_r8, &
    -569706880.000000000000_r8, -2184852275200.000000000000_r8, 0.000000000000_r8, &
    -11354823.000000000000_r8 ]

!  Local variables:
   CHARACTER*3, DIMENSION(13) ::  devar = (/'alk','dic','pho','sil','tem','sal','k0 ','k1 ','k2 ','kb ','kw ','ka ','kc '/)
   INTEGER ::  i
   
  !     derivar = 3-character identifier of input variable with respect to which derivative is requested
  !               possibilities are 'alk', 'dic', 'pho', 'sil', 'tem', or 'sal'
  !

!> Typical options for observations
   optCON  = 'mol/kg'  ! input concentrations are in MOL/KG
   optT    = 'Tinsitu' ! input temperature, variable 'temp' is actually IN SITU temp [°C]
   optP    = 'db'       ! input variable 'depth' is in meters
   optB    = 'l10'
   optK1K2 = 'l'
   optKf   = 'dg'
!> Simple input data (with CONCENTRATION units typical for DATA)
!> (based on observed average surface concentrations from S. Ocean (south of 60°S)--GLODAP and WOA2009)
    temp(1)   = 18.0d0            !Can be "Potential temperature" or "In situ temperature" (see optT below)
    sal(1)    = 35.0d0           !Salinity (practical scale)
    alk(1)    = 2300.0d-6      ! Convert obs. S. Ocean ave surf ALK (umol/kg) to mocsy data units (mol/kg)
    dic(1)    = 2000.0d-6      ! Convert obs. S. Ocean ave surf DIC (umol/kg) to mocsy data units (mol/kg)
    sil(1)    = 0.0d0   ! 60.d-06
    phos(1)   = 0.0d0   !  2.d-06
    sil(1)    = 60.0d-6   ! 60.d-06
    phos(1)   =  2.0d-6   !  2.d-06
    depth(1)  = 0.d0
    Patm(1)   = 1.0d0            !Atmospheric pressure (atm)
    lat(1)    = 0.d0

!  Select input var 'x', choosing set of dy_i/dx to be computed, where y_i are the diff output vars
   call vars(ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD, rhoSW, p, tempis,         &  ! OUTPUT
             temp, sal, alk, dic, sil, phos, Patm, depth, lat, 1,                             &  ! INPUT
             optCON, optT, optP, optB=optB, optK1K2=optK1K2, optKf=optKf,                     &
             optGAS='Ppot'    )
   h(1) = 10**(- ph(1))

!    write (*,*) "Variables:"
!    write (*,*) "          h,           ph,         pco2,         fco2,           co2",&
!               "           hco3,           co3,         OmegaA,         OmegaC"
!    write (*,"(9ES15.6)")  h, ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC

!    write (*,*) "Absolute derivatives" 
!    write (*,*) "             dh_dx         dpco2_dx       dfco2_dx         dco2_dx      dhco3_dx       dco3_dx", &
!         "       dOmegaA_dx     dOmegaC_dx"

   do i = 1,13
      call derivnum (dh_dx, dpco2_dx, dfco2_dx, dco2_dx, dhco3_dx,                      &
                      dco3_dx, dOmegaA_dx, dOmegaC_dx,                                   &
                      temp, sal, alk, dic, sil, phos, Patm, depth, lat, 1, devar(i),     &
                      optCON, optT, optP, optB=optB, optK1K2=optK1K2, optKf=optKf          )
      H_deriv(i) = dh_dx(1)
      pco2_deriv(i) = dpco2_dx(1)
      fco2_deriv(i) = dfco2_dx(1)
      co2_deriv(i) = dco2_dx(1)
      hco3_deriv(i) = dhco3_dx(1)
      co3_deriv(i) = dco3_dx(1)
      omegaa_deriv(i) = dOmegaA_dx(1)
      omegac_deriv(i) = dOmegaC_dx(1)
    !   write (*,"(A3,A5,8ES15.6)")  devar(i), "  :  ", dh_dx(1), dpco2_dx(1), dfco2_dx(1), dco2_dx(1), dhco3_dx(1), dco3_dx(1), &
    !       dOmegaA_dx(1), dOmegaC_dx(1)
   end do


   call check(error, all(is_equal(H_deriv, H_deriv_num_ref)), .true., " H deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(pco2_deriv, pco2_deriv_num_ref)), .true., " pCO2 deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(fco2_deriv, fco2_deriv_num_ref)), .true., " fCO2 deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(co2_deriv, co2_deriv_num_ref)), .true., " CO2 deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(hco3_deriv, hco3_deriv_num_ref)), .true., " HCO3 deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(co3_deriv, co3_deriv_num_ref)), .true., " CO3 deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(omegaa_deriv, omegaa_deriv_num_ref)), .true., " OmegaA deriv does not match!")
   if(allocated(error)) return

   call check(error, all(is_equal(omegac_deriv, omegac_deriv_num_ref)), .true., " OmegaC deriv does not match!")
   if(allocated(error)) return

end subroutine test_derivnum
end module test_mocsy
