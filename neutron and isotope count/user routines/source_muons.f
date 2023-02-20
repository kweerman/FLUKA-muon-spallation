*$ CREATE SOURCE.FOR
*COPY SOURCE
* source file for muons in XeLS: beam of muons through middle of detector
* with energies read from the file choosen in the input file
* In the file there are 10^5 muon energies that are distributed according 
* to the muon distribution at KamLAND
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2021      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                Infn - Milan          *
*                                                                      *
*     Last change on  03-Apr-21    by             Paola Sala           *
*                                                Infn - Milan          *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'
*
      LOGICAL LFIRST, LISNUT
* This is the amount of muon energies avaible in the muon energy file
      PARAMETER (NMAX=1000000)
*
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
*  Statement function:
      LISNUT (IJ) = INDEX ( PRNAME (IJ), 'NEUTRI' ) .GT. 0
*
* THIS IS NEW ---
      CHARACTER*250 LINE
      INTEGER NNN
      INTEGER IJ_(NMAX)
      
      INTEGER RANDOMNUM
      REAL*8 THETAVAR(NMAX), PHIVAR(NMAX), EVAR(NMAX) 
      SAVE THETAVAR, PHIVAR, EVAR
* ---
*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*     << return message from first call >>
         WRITE (LUNERR,*) 'Version 1 of Routine source called'
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***
         LUNRD = NINT(WHASOU(1))
         WTOT=ZERZER
         NNN=0
 10      CONTINUE 
         READ (LUNRD,'(A)', END=20) LINE
         READ (LINE,*,ERR=10) COSTHETA,PHI,E
         NNN=NNN+1
* There are 1000000 muon energies available in the file
         IF(NNN.GT.NMAX) CALL FLABRT('SOURCE','Increase NMAX')
         IJ_(NNN)=NNN
* There are 3 columns in the file: | cos(theta) | phi | Energy of muon
         THETAVAR(NNN)=COSTHETA
         PHIVAR(NNN)=PHI
         EVAR(NNN)=E
         WRITE (LUNERR,*) 'File read', COSTHETA, PHI, E
         GOTO 10

 20      CONTINUE
         IF (NNN.EQ.0) CALL FLABRT('SOURCE','Error reading file')
         WRITE (LUNOUT,*)
         WRITE (LUNOUT,*) '*** rdsource: ',NNN,' particles loaded'
         WRITE (LUNOUT,*)
      END IF
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
*     
*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROM  * 100000 + MOD ( IPROZ, 100 ) * 1000 + IPROA
         IJHION = IJHION * 100    + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         EEXSTK (NPFLKA) = EXENRG (IONID)
         TMNSTK (NPFLKA) = TMNLF  (IONID)
         ILVSTK (NPFLKA) = IEXLVL (IONID)
         LFRPHN (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROM  * 100000 + MOD ( IPROZ, 100 ) * 1000 + IPROA
         IJHION = IJHION * 100    + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         EEXSTK (NPFLKA) = EXENRG (IONID)
         TMNSTK (NPFLKA) = TMNLF  (IONID)
         ILVSTK (NPFLKA) = IEXLVL (IONID)
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |  Parent radioactive isotope:
         IRDAZM (NPFLKA) = 0
*  |  Particle age (s)
         AGESTK (NPFLKA) = +ZERZER
*  |  Kinetic energy of the particle (GeV)
         TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 )
     &                   - AM (IONID)
*  |  Particle momentum
         PMOFLK (NPFLKA) = PBEAM
*        PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                          + TWOTWO * AM (IONID) ) )
         LFRPHN (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron or lepton:
      ELSE
         IONID = IJBEAM
         EEXSTK (NPFLKA) = EXENRG (IJBEAM)
         TMNSTK (NPFLKA) = TMNLF  (IJBEAM)
         ILVSTK (NPFLKA) = IEXLVL (IJBEAM)
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |  Parent radioactive isotope:
         IRDAZM (NPFLKA) = 0
*  |  Particle age (s)
         AGESTK (NPFLKA) = +ZERZER
*  |  Kinetic energy of the particle (GeV)
         TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 )
     &                   - AM (IONID)
*  |  Particle momentum
         PMOFLK (NPFLKA) = PBEAM
*        PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                          + TWOTWO * AM (IONID) ) )
*  |  +----------------------------------------------------------------*
*  |  |  Check if it is a neutrino, if so force the interaction
*  |  |  (unless the relevant flag has been disabled)
         IF ( LISNUT (IJBEAM) .AND. LNUFIN ) THEN
            LFRPHN (NPFLKA) = .TRUE.
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  Not a neutrino
         ELSE
            LFRPHN (NPFLKA) = .FALSE.
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....
*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      KCHFLK (NPFLKA) = 0
      ECRFLK (NPFLKA) = ZERZER
*  Extra infos:
      INFSTK (NPFLKA) = 0
      LNFSTK (NPFLKA) = 0
      ANFSTK (NPFLKA) = ZERZER
*  Parent variables:
      IPRSTK (NPFLKA) = 0
      EKPSTK (NPFLKA) = ZERZER
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything
      AKNSHR (NPFLKA) = -TWOTWO
* -------------
* We change the kinetic energy input to the file
*  Kinetic energy of the particle (GeV)
      RANDOMNUM = FLRNDM(DUMMY)*NMAX + 1
* with this line the random numbers are printed on .log
      PRINT *, "randomm", RANDOMNUM
      N = RANDOMNUM
      TKEFLK (NPFLKA) = EVAR(N)
*      WRITE (LUNERR,*) 'KinE', TKEFLK (NPFLKA)
*  Particle momentum calculated from the input kinetic energy in GeV/c
      PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     &                       + TWOTWO * AM (IONID) ) )
* ---    
*  Cosines (tx,ty,tz)
* NORMALLY THIS IS THE ASKED DATA <-->
*      TXFLK  (NPFLKA) = UBEAM
*      TYFLK  (NPFLKA) = VBEAM
*      TZFLK  (NPFLKA) = WBEAM
* <-->
* Particle coordinates, from BEAMPOS variable in input file
      XFLK(NPFLKA) = XBEAM
      YFLK(NPFLKA) = YBEAM
      ZFLK(NPFLKA) = ZBEAM
* Cosines (tx,ty,tz) a line moving in the +z direction
      TXFLK(NPFLKA) =  0.0
      TYFLK(NPFLKA) =  0.0
      TZFLK(NPFLKA) = 1.0
*
* in the output file we see that we have the beam along the z-axis
* everytime with a random new energy
      write(LUNOUT,*) N,',',TKEFLK(NPFLKA),',',PMOFLK(NPFLKA),' ',
     &  XFLK(NPFLKA),YFLK(NPFLKA),ZFLK(NPFLKA)
*      CALL sourcemu(XFLK(NPFLKA), YFLK(NPFLKA), ZFLK(NPFLKA),
*     & TKEFLK(NPFLKA), TXFLK(NPFLKA), TYFLK(NPFLKA), TZFLK(NPFLKA), 
*     & MCOST(N), MPHI(N), MENE(N))
* ---
* ---------------------------------------------------------ieki            

*  Calculate the total kinetic energy of the primaries: don't change
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( ILOFLK (NPFLKA) .EQ. -2 .OR.
     &          ABS (ILOFLK (NPFLKA)) .GE. KPIOMN ) THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |  Standard particle:
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
*  |
*  +-------------------------------------------------------------------*
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
      END
