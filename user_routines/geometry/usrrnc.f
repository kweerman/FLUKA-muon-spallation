*$ CREATE USRRNC.FOR
* Version 1 Kelly Weerman
* save residual nuclei information: Z A isomeric state and X,Y,Z coordinates
* note: the nuclei is scored at the end of its path, so not when it is created
*
*=== Usrrnc ===========================================================*
*
      SUBROUTINE USRRNC ( IZ, IA, IS, X, Y, Z, MREG, WEE, ICALL )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2005-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR Residual NuClei:                                            *
*                                                                      *
*     Created on   06 april 2005   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 06-apr-05     by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*
* Add trackr to keep track of the coordinates
      INCLUDE '(TRACKR)'
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
         WRITE (LUNERR,*) 'USRRNC called for this FLUKA routine'
         LFIRST = .FALSE.
      ENDIF
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         OPEN ( UNIT = 21, FILE = 'RESIDNUC', STATUS = 'NEW',
     &           FORM = 'UNFORMATTED' )
* the header of the file is written only the first time
         WRITE (21) 'Version 1 2022 Kelly Weerman'
      END IF
*
      IF (IZ .GE. 3) THEN
         WRITE (21) IZ, IA, IS, XTRACK(0), YTRACK(0), ZTRACK(0)
      ELSE IF (IZ .EQ. 2 .AND. IA .GE. 5) THEN
         WRITE (21) IZ, IA, IS, XTRACK(0), YTRACK(0), ZTRACK(0)
      ENDIF
*
      RETURN
*=== End of subroutine Usrrnc =========================================*
      END
