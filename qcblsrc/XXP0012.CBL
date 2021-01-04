     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0012.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE KW-DATEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE VON KW-DATEN                         *
     ‚*                  BZW. ERSTELLEN DER BERICHTSHEFTDATEN         *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* PAZUBER: ABKW, ABJAHR, ABUSER
     ‚*-------------------------------------------------------------
           SELECT PAZUBER-DP
                  ASSIGN       TO  DATABASE-PAZUBER
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PAZUBER: ABKW, ABJAHR, ABUSER
       FD  PAZUBER-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUBER-P.
           COPY DDS-ALL-FORMATS OF PAZUBER.
      /
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0012".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST  ".
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-GRUPPE                      LIKE CFKEY  OF PCONFIG-P.
       01  X-KW                          PIC 9(2).
       01  X-JAHR                        PIC 9(4).
       01  X-VERANTW                     LIKE CFDAT1 OF PCONFIG-P.
       01  X-JAHRGANG                    LIKE CFNM04 OF PCONFIG-P.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-GRUPPE
                                         X-KW
                                         X-JAHR
                                         X-VERANTW
                                         X-JAHRGANG.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0012" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEIEN ÖFFNEN
           OPEN     INPUT PCONFIG-DP.
           OPEN     I-O   PAZUBER-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PARAMETER ÜBERGEBEN?
           IF       X-KW     = ZEROES
            OR      X-JAHR   = ZEROES
            OR      X-GRUPPE = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H051"              TO CFID   OF PCONFIG-P.
           MOVE     X-GRUPPE            TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

       ANF020.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "H051"
            OR      CFKEY  OF PCONFIG-P NOT = X-GRUPPE
                    GO TO ENDE
           END-IF.

     ‚* KOPFSATZ ÜBERLESEN
           IF       CFKEY2 OF PCONFIG-P     = SPACES
            OR      CFKOPF OF PCONFIG-P NOT = ZEROES
                    GO TO ANF020
           END-IF.

     ‚* IST VERANTWORTLICHER RICHTIG?
           IF       X-VERANTW  NOT = SPACES
            AND     X-VERANTW  NOT = CFDAT1 OF PCONFIG-P
                    GO TO ANF020
           END-IF.

     ‚* IST JAHRGANG DER AZUBIS RICHTIG?
           IF       X-JAHRGANG NOT = SPACES
            AND     X-JAHRGANG NOT = CFNM04 OF PCONFIG-P
                    GO TO ANF020
           END-IF.

     ‚* DATENSATZ SCHREIBEN
           INITIALIZE PAZUBERF OF PAZUBER-P.
           MOVE     X-KW                      TO ABKW   OF PAZUBER-P.
           MOVE     X-JAHR                    TO ABJAHR OF PAZUBER-P.
           MOVE     CFKEY2 OF PCONFIG-P       TO ABUSER OF PAZUBER-P.
           MOVE     SPACES                    TO ABSTAT OF PAZUBER-P.

     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                    TO ABAUSR OF PAZUBER-P.
           MOVE     T-TERM                    TO ABABS  OF PAZUBER-P.
           MOVE     PGM-WRK                   TO ABAPGM OF PAZUBER-P.
           MOVE     DATE-8                    TO ABADTA OF PAZUBER-P.
           MOVE     TIME-6                    TO ABATIA OF PAZUBER-P.
           WRITE    PAZUBER-P.

           GO TO    ANF020.

       ENDE.
           CLOSE    PAZUBER-DP
                    PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
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
