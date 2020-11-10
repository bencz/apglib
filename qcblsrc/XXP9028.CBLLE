       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9028.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.11.2010                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9028".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  TEXT-INPUT                    PIC X(500).
       01  MAXLEN-RECORD                 PIC 9(4).
       01  OUTPUT-TAB.
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC                       OCCURS 100 TIMES.
         06 ZEILE                        PIC X(100).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9028"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE "DIES IST EIN SEHR SEHR LANGER, EIN WIRKLICH LANGER TEXT
      -         " IN COBOL ÜBER MEHR ALS NUR EINE ZEILE, JA SOGAR MEHR A
      -         "LS ÜBER ZWEI!!" TO TEXT-INPUT.
           MOVE     30 TO MAXLEN-RECORD.

           CALL     "HLP0021" USING TEXT-INPUT
                                    MAXLEN-RECORD
                                    OUTPUT-TAB
                                    RET-CODE
           END-CALL.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
