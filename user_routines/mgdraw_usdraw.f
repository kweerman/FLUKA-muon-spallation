* Version 5 2022 Kelly Weerman: mgdraw routine where usdraw is used to 
* construct the isotope creation the information is dumped in a file
* which can be read with python vers5_eventscreator.py
* only the creation coordinates of isotopes are returned: not the complete paths
* usdraw is called
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
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG ), AMHELP ( MFSTCK )
*
*     |   First particle run is indicated by this
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /
*
*     |  First call initializations:
      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST /.TRUE./
      IF ( LFIRST ) THEN
*     << return message from first call >>
         WRITE (LUNERR,*) 'MGDRAW called for this FLUKA routine'
         LFIRST = .FALSE.
         jobcount = 0
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
         WRITE (IODRAW) 'Version 5 2022 Kelly Weerman'
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
* Create a line with 0 that can be used in the python script
* to count the end of an event.
      WRITE (IODRAW) 0, 0, 0
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
      WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
      WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
* This function is not activated
      ENTRY SODRAW
*  |  Loop on main stack particle(s):
      DO 2000 I = 1, NPFLKA
 2000 CONTINUE
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
         WRITE(IODRAW) ICODE, KPART(I), JTRACK,
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IBARCH(KPART(I)), ICHRGE(KPART(I)), 
     &            NP, (KPART(J), J = 1, NP), 
     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
*
      ELSE IF (KPART(I) .LE. -10) THEN
      CALL USRDCI(KPART(I), IONA, IONZ, IONM)
      WRITE (IODRAW) ICODE, KPART(I), JTRACK,
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IONA, IONZ, 
     &            NP, (KPART(J), J = 1, NP), 
     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
      END IF
*
 2001 CONTINUE
* -----
* now what is normally the negative particle codes 
* JTRACK < 0 = KHEAVY > 0
      DO 2002 I = 1, NPHEAV
      WRITE (IODRAW) ICODE, KHEAVY(I), JTRACK, 
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IBHEAV(KHEAVY(I)), ICHEAV(KHEAVY(I)), 
     &            NP, (KPART(J), J = 1, NP), 
     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
 2002 CONTINUE
* ----
* the residual nuclei are scored here
* IBRES = A and ICRES = Z
      IF (ICODE .EQ. 101 .OR. ICODE .EQ. 300) THEN
         IF (IBRES .NE. 0 .AND. ICRES .GE. 2) THEN
            WRITE (IODRAW) ICODE, 0, JTRACK,
     &            XTRACK(0), YTRACK(0), ZTRACK(0),
     &            IBRES, ICRES,
     &            NP, (KPART(J), J = 1, NP), 
     &            NPHEAV, (IBHEAV(KHEAVY(L)),  
     &                     ICHEAV(KHEAVY(L)), L = 1, NPHEAV)
         END IF
      END IF
* ----
* Lastly we keep track of neutron hydrogen capture
      DO 2003 I = 1, NP
      IF (JTRACK .EQ. 8 .AND. KPART(I) .EQ. 7) THEN
         IF (TKI(I) .GE. 2.220E-003 .AND. TKI(I) 
     &                             .LE. 2.229E-003 ) THEN
               NEUTRON_COUNT = NEUTRON_COUNT + 1
               WRITE (IODRAW) 1, 1, NEUTRON_COUNT, 
     &            XTRACK(0), YTRACK(0), ZTRACK(0), 
     &                                     TKI(I)
         END IF
      END IF
 2003 CONTINUE
      RETURN
*=== End of subrutine Mgdraw ==========================================*
      END

