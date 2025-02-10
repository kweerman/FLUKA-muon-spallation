* Version 7 2022 Kelly Weerman: mgdraw routine where usdraw is used to 
* construct the isotope creation the information is dumped in a file
* which can be read with python vers7_eventscreator.py
* only the creation coordinates of isotopes are returned: not the complete paths
* usdraw and sodraw are called: the last returns the initial muon energy
* also the muon energy difference is saved and inital and final muon coordinates
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2021      by   Alfredo Ferrari & Paola Sala   *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     MaGnetic field trajectory DRAWing: actually this entry manages   *
*                                        all trajectory dumping for    *
*                                        drawing                       *
*                                                                      *
*     Created on   01 March 1990   by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*                                                                      *
*     Last change   03-Apr-21      by   Alfredo Ferrari &  Paola Sala  *
*                                          Private        INFN - Milan *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
* heavy secondaries created in the nuclear evaporation:
      INCLUDE '(FHEAVY)'
* radioactive decay particles and such!
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(QUEMGD)'
* also something about decay
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'
* extra Included
      INCLUDE '(RESNUC)'
      INTEGER EVENTNUM
      REAL*8 ZEROO, MUONE
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG ), AMHELP ( MFSTCK )
*
*     |   First particle run is indicated by this
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /
*     |  First call initializations:
      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST /.TRUE./
*
* The following are initialized at the start and after every event (EEDRAW)
* Initialize every start of an event with the following
      LOGICAL EVENTI
      SAVE EVENTI
      DATA EVENTI /.TRUE./
* Initialze the second event (first muon entry in geometry)
      LOGICAL MUONENTRY
      SAVE MUONENTRY
      DATA MUONENTRY /.FALSE./
* 
* Check mgdraw is called correctly
      IF ( LFIRST ) THEN
*     << return message from first call >>
         WRITE (LUNERR,*) 'MGDRAW called for this FLUKA routine'
         LFIRST = .FALSE.
         EVENTNUM = 1
      ENDIF
*
*
*----------------------------------------------------------------------*
*                                                                      *
*     Icode = 1: call from Kaskad                                      *
*     Icode = 2: call from Emfsco                                      *
*     Icode = 3: call from Kasneu                                      *
*     Icode = 4: call from Kashea                                      *
*     Icode = 5: call from Kasoph                                      *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                 
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
* the header of the file is written only the first time
         WRITE (IODRAW) 'Version 7 2022 Kelly Weerman'
      END IF
* first time the event is started we save the muon energy
* we save the muon entrance when it enters the geometry
      IF ( MUONENTRY ) THEN
        WRITE(IODRAW) EVENTNUM, XTRACK(0), YTRACK(0), ZTRACK(0),
     &                 MUONE, ZEROO
* this line returns the directional cosines, if it is necessary
*     &              TXFLK(NPFLKA), TYFLK(NPFLKA), TZFLK(NPFLKA)
        MUONENTRY = .FALSE.
      END IF
      IF ( EVENTI ) THEN
*        WRITE (IODRAW) ETRACK, ZEROO
        MUONE = ETRACK
        EVENTI = .FALSE.
        MUONENTRY = .TRUE.
      END IF
*      WRITE (IODRAW)  JTRACK, ( SNGL (XTRACK (I)),
*     &      SNGL (YTRACK (I)), SNGL (ZTRACK (I)), I = 0, NTRACK )
*  +-------------------------------------------------------------------*
* check if the particle code is -7 or lower, and then add the charge and mass
*      IF ( JTRACK .LE. -7) THEN
*         WRITE (IODRAW) IBHEAV(ABS(JTRACK)), ICHEAV(ABS(JTRACK)), 
*     &                0., 0., 0., 0., 0.
* check if the particle code is -2, heavyion, then add the charge and mass
*      ELSE IF (JTRACK .EQ. -2) THEN
*         WRITE(IODRAW) IBARCH(JTRACK), ICHRGE(JTRACK), 
*     &                0., 0., 0., 0., 0.  
*      END IF
*  +-------------------------------------------------------------------*
      RETURN 
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             19: boundary crossing                                    *
*     Icode = 2x: call from Emfsco                                     *
*             29: boundary crossing                                    *
*     Icode = 3x: call from Kasneu                                     *
*             39: boundary crossing                                    *
*     Icode = 4x: call from Kashea                                     *
*             49: boundary crossing                                    *
*     Icode = 5x: call from Kasoph                                     *
*             59: boundary crossing                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )
*
      RETURN
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE )
* This is the end of the event, the next event will start afterwards
* We already change the eventnumber for binary file reading
* Note: input file USERDUMP to call this function use third option 4.0
* safe the muon exit x,y,z
      EVENTNUM = EVENTNUM + 1
      WRITE(IODRAW) EVENTNUM, XFLK(NPFLKA), YFLK(NPFLKA), ZFLK(NPFLKA),
     &                ETRACK, MUONE - ETRACK
* and directional cosine tx,ty,tz
*     &              TXFLK(NPFLKA), TYFLK(NPFLKA), TZFLK(NPFLKA)
* safe the last muon energy to calculate total muon energy loss
* we are at the next event so EVENTI is true
*      WRITE (IODRAW) ETRACK, MUONE - ETRACK
      EVENTI = .TRUE.
      MUONENTRY = .FALSE.
*
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             10: elastic interaction recoil                           *
*             11: inelastic interaction recoil                         *
*             12: stopping particle                                    *
*             13: pseudo-neutron deposition                            *
*             14: escape                                               *
*             15: time kill                                            *
*             16: recoil from (heavy) bremsstrahlung                   *
*     Icode = 2x: call from Emfsco                                     *
*             20: local energy deposition (i.e. photoelectric)         *
*             21: below threshold, iarg=1                              *
*             22: below threshold, iarg=2                              *
*             23: escape                                               *
*             24: time kill                                            *
*     Icode = 3x: call from Kasneu                                     *
*             30: target recoil                                        *
*             31: below threshold                                      *
*             32: escape                                               *
*             33: time kill                                            *
*     Icode = 4x: call from Kashea                                     *
*             40: escape                                               *
*             41: time kill                                            *
*             42: delta ray stack overflow                             *
*     Icode = 5x: call from Kasoph                                     *
*             50: optical photon absorption                            *
*             51: escape                                               *
*             52: time kill                                            *
*                                                                      *
*======================================================================*
*                                                                      *
* This function is not activated
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
*      WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
*      WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
* This function is called every time a muon is dumped
* into the geometry, so we are at the end of the track
* save x,y,z and directional cosine tx,ty,tz
* Note: this function called after EEDRAW
      ENTRY SODRAW
*         WRITE(IODRAW) XFLK(0), YFLK(0), ZFLK(0),
*     &                    TXFLK(0), TYFLK(0), TZFLK(0)
*  |
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*     Icode =  99: call from Doiosp, ion splitting secondaries         *
*     Icode = 10x: call from Kaskad                                    *
*             100: elastic   interaction secondaries                   *
*             101: inelastic interaction secondaries                   *
*             102: particle decay  secondaries                         *
*             103: delta ray  generation secondaries                   *
*             104: pair production secondaries                         *
*             105: bremsstrahlung  secondaries                         *
*             106: de-excitation in flight secondaries                 *
*             110: radioactive decay products                          *
*     Icode = 20x: call from Emfsco                                    *
*             208: bremsstrahlung secondaries                          *
*             210: Moller secondaries                                  *
*             212: Bhabha secondaries                                  *
*             214: in-flight annihilation secondaries                  *
*             215: annihilation at rest   secondaries                  *
*             217: pair production        secondaries                  *
*             219: Compton scattering     secondaries                  *
*             221: photoelectric          secondaries                  *
*             225: Rayleigh scattering    secondaries                  *
*             237: mu pair production     secondaries                  *
*     Icode = 30x: call from Kasneu                                    *
*             300: interaction secondaries                             *
*     Icode = 40x: call from Kashea                                    *
*             400: delta ray  generation secondaries                   *
*                                                                      *
*  For all interactions secondaries are put on GENSTK common (kp=1,np) *
*  but for KASHEA delta ray generation where only the secondary elec-  *
*  tron is present and stacked on FLKSTK common for kp=npflka          *
*                                                                      *
*  !!! For optical photon production events, please refer to the  !!!  *
*  !!! pshckp, ustckv (Cerenkov), pshscp, ustscn (Scintillation)  !!!  *
*  !!! user routines                                              !!!  *
*                                                                      *
*======================================================================*
*
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
* -----
      DO 2001 I = 1, NP
      IF (KPART(I) .EQ. -2) THEN
         WRITE(IODRAW) EVENTNUM, ICODE, KPART(I), JTRACK,
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IBARCH(KPART(I)), ICHRGE(KPART(I)) 
*     &            NP, (KPART(J), J = 1, NP), 
*     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
*     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
*
      ELSE IF (KPART(I) .LE. -10) THEN
      CALL USRDCI(KPART(I), IONA, IONZ, IONM)
      WRITE (IODRAW) EVENTNUM, ICODE, KPART(I), JTRACK,
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IONA, IONZ 
*     &            NP, (KPART(J), J = 1, NP), 
*     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
*     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
      END IF
*
 2001 CONTINUE
* -----
* now what is normally the negative particle codes 
* JTRACK < 0 = KHEAVY > 0
      DO 2002 I = 1, NPHEAV
      WRITE (IODRAW) EVENTNUM, ICODE, KHEAVY(I), JTRACK, 
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IBHEAV(KHEAVY(I)), ICHEAV(KHEAVY(I)) 
*     &            NP, (KPART(J), J = 1, NP), 
*     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
*     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
 2002 CONTINUE
* ----
* the residual nuclei are scored here
* IBRES = A and ICRES = Z
      IF (ICODE .EQ. 101 .OR. ICODE .EQ. 300) THEN
         IF (IBRES .NE. 0 .AND. ICRES .GE. 2) THEN
            WRITE (IODRAW) EVENTNUM, ICODE, 0, JTRACK,
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IBRES, ICRES
*     &            NP, (KPART(J), J = 1, NP), 
*     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
*     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
         END IF
      END IF
* ----
* Lastly we keep track of neutron hydrogen capture
      DO 2003 I = 1, NP
      IF (JTRACK .EQ. 8 .AND. KPART(I) .EQ. 7) THEN
         IF (TKI(I) .GE. 2.220E-003 .AND. TKI(I) 
     &                             .LE. 2.229E-003 ) THEN
               NEUTRON_COUNT = NEUTRON_COUNT + 1
               WRITE (IODRAW) EVENTNUM, 1, 1, NEUTRON_COUNT, 
     &            XTRACK(0), YTRACK(0), ZTRACK(0), 
     &                                     TKI(I)
         END IF
      END IF
 2003 CONTINUE
      RETURN
*=== End of subrutine Mgdraw ==========================================*
      END

