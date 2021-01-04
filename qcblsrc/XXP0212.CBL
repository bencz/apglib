       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0212.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AKTUALISIEREN EINER KAFFEEMASCHINE                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STATUS FÜR KAFFEEMASCHINE AKTUALISIEREN      *
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
     ‚* PHYSISCHE DATEI BESTELLUNGEN
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCONFIG: KBLFDN
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0212".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CFKEY                       LIKE CFKEY  OF PCONFIG-P.
       01  X-CFFK01                      LIKE CFFK01 OF PCONFIG-P.
       01  X-EUP-TXT                     LIKE CFIN21 OF PCONFIG-P.
       01  X-IAW-TXT                     LIKE CFIN21 OF PCONFIG-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CFKEY
                                X-CFFK01
                                X-EUP-TXT
                                X-IAW-TXT.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0212" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCONFIG-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CFKEY = SPACES
                    PERFORM  UPDATE-TEXT
           ELSE
                    PERFORM  UPDATE-RECORD
           END-IF.


       ENDE.
           CLOSE                 PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN DES DATENSATZES
     ‚*--------------------------------------------------------------
       UPDATE-RECORD SECTION.
       UPD-REC-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* DATENSATZ AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "K111"              TO CFID   OF PCONFIG-P.
           MOVE     X-CFKEY             TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             STATUS AKTUALISIEREN
                    MOVE    X-CFFK01    TO CFFK01 OF PCONFIG-P
     ‚*             MODIFIKATIONSDATUM
                    MOVE    T-USER      TO CFMSER OF PCONFIG-P
                    MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                    MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                    MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                    MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    REWRITE PCONFIG-P
           END-IF.

       UPD-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN DER TEXTE
     ‚*--------------------------------------------------------------
       UPDATE-TEXT SECTION.
       UPD-TXT-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* EUP AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "K111"              TO CFID   OF PCONFIG-P.
           MOVE     "EUP"               TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    IF   CFIN21 OF PCONFIG-P NOT = X-EUP-TXT
     ‚*                  TEXT AKTUALISIEREN
                         MOVE    X-EUP-TXT   TO CFIN21 OF PCONFIG-P
     ‚*                  MODIFIKATIONSDATUM
                         MOVE    T-USER      TO CFMSER OF PCONFIG-P
                         MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                         MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                         MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                         MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    END-IF
                    REWRITE PCONFIG-P
           END-IF.

     ‚* IAW AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "K111"              TO CFID   OF PCONFIG-P.
           MOVE     "IAW"               TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    IF   CFIN21 OF PCONFIG-P NOT = X-IAW-TXT
     ‚*                  TEXT AKTUALISIEREN
                         MOVE    X-IAW-TXT   TO CFIN21 OF PCONFIG-P
     ‚*                  MODIFIKATIONSDATUM
                         MOVE    T-USER      TO CFMSER OF PCONFIG-P
                         MOVE    T-TERM      TO CFMBS  OF PCONFIG-P
                         MOVE    PGM-WRK     TO CFMPGM OF PCONFIG-P
                         MOVE    DATE-8      TO CFMDTA OF PCONFIG-P
                         MOVE    TIME-6      TO CFMTIA OF PCONFIG-P
                    END-IF
                    REWRITE PCONFIG-P
           END-IF.

       UPD-TXT-90.
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
