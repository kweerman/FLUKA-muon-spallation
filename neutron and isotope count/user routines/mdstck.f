*$ CREATE MDSTCK.FOR
*COPY MDSTCK
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
         LFIRST = .FALSE.
      ENDIF
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         OPEN ( UNIT = 22, FILE = 'NEUTRO', STATUS = 'NEW', FORM =
     &          'FORMATTED' )
* the header of the file is written only the first time
         WRITE (22,*) 'Neutron dump from mdstck.f'
      END IF
*
*      WRITE(22,*) JTRACK, NPSECN, XTRACK(0), YTRACK(0), ZTRACK(0)
*      DO 2001 I = 1, NPSECN
*         WRITE (22,*) 'Secondaries', KPART(I), IFLAG, NPSECN,
*     &                 XTRACK(0), YTRACK(0), ZTRACK(0)
* 2001 CONTINUE
*      check if a neutron is created, particle code 8
      DO 2000 I = 1, NPSECN
      IF ( KPART(I) .EQ. 8) THEN
         WRITE (22,*) 'Neutron created', KPART(I), IFLAG, 
     &               ISPUSR (MKBMX2),
     &               ( SNGL (XTRACK (J)),
     &      SNGL (YTRACK (J)), SNGL (ZTRACK (J)), J = 0, NTRACK ) 
         count = count + 1
         WRITE (22,*) JTRACK
         WRITE (22,*) count
      END IF
 2000 CONTINUE
*
      RETURN
*=== End of subroutine Mdstck =========================================*
      END

