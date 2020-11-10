       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9048.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.10.2016                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9080".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  VAR1                          PIC 9(1) VALUE ZEROES.
       01  VAR2                          PIC 9(1) VALUE ZEROES.
       01  VAR3                          PIC 9(1) VALUE ZEROES.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9080"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           IF       VAR1 = 1
                    CONTINUE
           ELSE IF  VAR2 = 2
                    CONTINUE
           ELSE IF  VAR3 = 3
            AND     VAR2 = 2
                    CONTINUE
           END-IF.

           IF       VAR1 = 1
                    CONTINUE
           ELSE
                    IF   VAR2= 2
                         CONTINUE
                    ELSE
                         IF   VAR3 = 3
                          AND VAR2 = 2
                              CONTINUE
                         END-IF
                    END-IF
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
