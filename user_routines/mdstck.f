*$ CREATE MDSTCK.FOR
* Version 1 Kelly Weerman
* score the neutron capture count by looking at interactions where a gamma
* is send out after a low-energy neutron interaction
*
*=== Mdstck ===========================================================*
*
      SUBROUTINE MDSTCK ( IFLAG, NPSECN )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1998-2013      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Created on    19 May 1998    by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  14-Nov-13    by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*        Iflag = 1: standard Kaskad call                               *
*              = 2: Kaskad call after elastic                          *
*              = 3: Kasneu call                                        *
*              = 4: Emfsco call                                        *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(EMFSTK)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(GENSTK)'
      INCLUDE '(TRACKR)'
* extra
      INCLUDE '(FLKSTK)'
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
         WRITE (LUNERR,*) 'MDSTCK called for this FLUKA routine'
         count = 0
         LLOUSE = 0
         LFIRST = .FALSE.
      ENDIF
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         OPEN ( UNIT = 22, FILE = 'NEUTRO', STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
* the header of the file is written only the first time
         WRITE (22) 'Version 1 2022 Kelly Weerman'
      END IF
*
*      check if a neutron is created, particle code 8
      DO 2000 I = 1, NPSECN
      IF (KPART(I) .EQ. 7 .AND. IFLAG .EQ. 3) THEN
* note TKI(I) is the energy of the gamma and ETRACK of the particle followed: 
* in this case that would be the neutron
         WRITE(22) TKI(I), ETRACK,
     &               XTRACK(0), YTRACK(0), ZTRACK(0),
     &               NPSECN, (KPART(J), J = 1, NPSECN)
      END IF
 2000 CONTINUE
*
      RETURN
*=== End of subroutine Mdstck =========================================*
      END

