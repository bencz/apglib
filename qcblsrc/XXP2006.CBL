       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP2006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EIN. API "BASIC RANDOM NUMBER GENERATION (CEERAN0) API"*
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/INFOCENTER/ISERIES/V5R4/TOPIC/  *
     ‚* APIS/CEERAN0.HTM                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  CEERAN0 ALS API                              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP2006".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

       01  INT4-WRK                      PIC S9(9) BINARY.
     ‚* 1   BIT: VORZEICHEN
     ‚* 2-7 BIT: EXPONENT
     ‚* REST   : DEN WERT (MANTISSA)
       01  FLOAT8-WRK                    COMP-2.
       01  FC-WRK.
        02 SEV                           PIC 9(4) BINARY.
        02 MSGNO                         PIC 9(4) BINARY.
        02 FLGS                          PIC X(1).
        02 FACID                         PIC X(3).
        02 ISI                           PIC 9(9) BINARY.

       01  NUM-WRK                       PIC S9(18).

       01  DEBUG-WRK                     PIC 9(1).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP2006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE DEBUG-WRK.
           INITIALIZE INT4-WRK, FLOAT8-WRK, FC-WRK.

       ANF020.
           CALL     PROCEDURE "CEERAN0" USING INT4-WRK
                                              FLOAT8-WRK
                                              FC-WRK
           END-CALL.
           IF       DEBUG-WRK = 1
                    GO TO ANF020
           END-IF.

           MOVE     FLOAT8-WRK TO NUM-WRK.
     ‚* Z = -1^VZ * (1,0 + M/2^52) * 2^E-1023

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
