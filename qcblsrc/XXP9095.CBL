       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9095.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.08.2017                                   *
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
         SOURCE-COMPUTER.   IBM-AS400.
         OBJECT-COMPUTER.   IBM-AS400,
                            PROGRAM COLLATING SEQUENCE IS IBM-ASCII.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                          ALPHABET IBM-ASCII IS NLSSORT.

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

       01  PGM-WRK                        PIC X(10) VALUE "XXP9095".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".
       01  MOD-VAR                        PIC S9(9)V9(3).
       01  SIN-VAR                        PIC S9(9)V9(6).
       01  SQR-VAR                        PIC S9(9)V9(6).
       01  NUM-VAR PIC 9(1).
       01  NUM-ORD                        PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9095"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     17 TO RETURN-CODE.
           COMPUTE  MOD-VAR = FUNCTION MOD(5 2).
           COMPUTE  SIN-VAR = FUNCTION SIN(30).
           COMPUTE  SQR-VAR = FUNCTION SQRT(9).
           MOVE     SPACES TO NUM-VAR.

           COMPUTE  NUM-ORD = FUNCTION ORD("A").

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
