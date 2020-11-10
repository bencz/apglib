       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9056.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.12.2012                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9056".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  MERKER                        PIC 9(2).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9056"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE MERKER.
           PERFORM  REK-SEC-1.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTE REKURSIONS-SECTION
     ‚*--------------------------------------------------------------
       REK-SEC-1 SECTION.
       REC-SE1-00.

           ADD      1 TO MERKER.
           IF       MERKER < 10
                    PERFORM  REK-SEC-2
           END-IF.

       REC-SE1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* 2. REKURSIONS-SECTION
     ‚*--------------------------------------------------------------
       REK-SEC-2 SECTION.
       REC-SE2-00.

           PERFORM  REK-SEC-1.

       REC-SE2-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* 3. REKURSIONS-SECTION
     ‚*--------------------------------------------------------------
       REK-SEC-3 SECTION.
       REC-SE3-00.

           INITIALIZE RET-CODE.

       REC-SE3-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
