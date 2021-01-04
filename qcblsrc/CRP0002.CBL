       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERSTELLEN CR-VORGANG                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERSTELLEN EINES CR-VORGANGS                  *
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

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI CODEREVIEW
     ‚*-------------------------------------------------------------
           SELECT PCHKSRC-DP
                  ASSIGN       TO  DATABASE-PCHKSRC
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- CODEREVIEW
       FD  PCHKSRC-DP
           LABEL RECORDS ARE STANDARD.
       01  PCHKSRC-P.
           COPY DDS-ALL-FORMATS OF PCHKSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "CHKSRC".

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CRLIB                       LIKE CRLIB  OF PCHKSRC-P.
       01  X-CRFILE                      LIKE CRFILE OF PCHKSRC-P.
       01  X-CRMBR                       LIKE CRMBR  OF PCHKSRC-P.
       01  X-CRLFDN                      LIKE CRLFDN OF PCHKSRC-P.
       01  X-CRVORG                      LIKE CRVORG OF PCHKSRC-P.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CRLIB
                                X-CRFILE
                                X-CRMBR
                                X-CRLFDN
                                X-CRVORG.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-CRVORG, X-CRLFDN.
     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCHKSRC-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DEN KOPFSATZ ERSTELLEN
           PERFORM  SCHREIBE-VORGANG.

       ENDE.
           CLOSE                 PCHKSRC-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN KOPFSATZ DES VORGANGS
     ‚*--------------------------------------------------------------
       SCHREIBE-VORGANG SECTION.
       SCR-VOR-00.

           INITIALIZE PCHKSRCF OF PCHKSRC-P.
     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "PCHKSRC" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.

     ‚* SCHLÜSSEL FÜLLEN
           MOVE     LFDN-WRK               TO CRLFDN OF PCHKSRC-P.
           MOVE     LFDN-WRK               TO X-CRLFDN.
     ‚* DATEN AUS DEM FORMAT KOPIEREN
     ‚*
     ‚* SOURCE
           MOVE     X-CRLIB                TO CRLIB  OF PCHKSRC-P.
           MOVE     X-CRFILE               TO CRFILE OF PCHKSRC-P.
           MOVE     X-CRMBR                TO CRMBR  OF PCHKSRC-P.
     ‚* KOPFSATZ HAT KEINE FEHLERMELDUNG
           MOVE     ZEROES                 TO CRSECN OF PCHKSRC-P.
           MOVE     ZEROES                 TO CRSDTA OF PCHKSRC-P.
           MOVE     SPACES                 TO CRFID  OF PCHKSRC-P.
           MOVE     ZEROES                 TO CRFTYP OF PCHKSRC-P.
           MOVE     SPACES                 TO CRFTXT OF PCHKSRC-P.
     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "CHK-VORG" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK               TO CRVORG OF PCHKSRC-P.
           MOVE     LFDN-WRK               TO X-CRVORG.
     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                 TO CRAUSR OF PCHKSRC-P.
           MOVE     T-TERM                 TO CRABS  OF PCHKSRC-P.
           MOVE     PGM-WRK                TO CRAPGM OF PCHKSRC-P.
           MOVE     DATE-8                 TO CRADTA OF PCHKSRC-P.
           MOVE     TIME-6                 TO CRATIA OF PCHKSRC-P.

           WRITE    PCHKSRC-P.

       SCR-VOR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
