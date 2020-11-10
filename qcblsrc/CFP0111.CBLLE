       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0111.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERSTELLEN EINES SCHLÜSSEL-KOPFSATZ                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERSTELLEN SCHLÜSSEL-KOPFSATZ                 *
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
     ‚* PHYSISCHE DATEI KONFIGURATION
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE  DATEI KONFIGURATION (KOPFSÄTZE)
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DL
                  ASSIGN       TO  DATABASE-PCONFIGL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
     ‚*--- KONFIGURATION
       FD  PCONFIG-DL
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-L.
           COPY DDS-ALL-FORMATS OF PCONFIGL1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0111".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CFID                        LIKE CFID   OF PCONFIG-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CFID.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0111" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCONFIG-DP.
           OPEN     INPUT PCONFIG-DL.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CFID  = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB EINTRAG SCHON VORHANDEN
           INITIALIZE PCONFIGF OF PCONFIG-L.
           MOVE     X-CFID                TO CFID   OF PCONFIG-L.
           MOVE     SPACES                TO CFKEY  OF PCONFIG-L.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-L.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-L.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-L.
           READ     PCONFIG-DL.
           IF       FILE-STATUS = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* KEIN DATENSATZ VORHANDEN, ALSO ANLEGEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     X-CFID                  TO CFID   OF PCONFIG-P.
           MOVE     1                       TO CFKOPF OF PCONFIG-P.
     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                  TO CFAUSR OF PCONFIG-P.
           MOVE     T-TERM                  TO CFABS  OF PCONFIG-P.
           MOVE     PGM-WRK                 TO CFAPGM OF PCONFIG-P.
     ‚* DATUM/UHRZEIT
           MOVE     DATE-8                  TO CFADTA OF PCONFIG-P.
           MOVE     TIME-6                  TO CFATIA OF PCONFIG-P.
           WRITE    PCONFIG-P.

       ENDE.
           CLOSE                 PCONFIG-DP
                                 PCONFIG-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
