       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AZP0305.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCEN EINER ÜBUNG KOPIEREN                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  27.07.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCEN KOPIEREN                             *
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
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
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
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0305".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

       01  PAZUSRC-LIKE.
           COPY DDS-ALL-FORMATS OF PAZUSRC.

       01  USER-WRK                      LIKE ASUSER OF PAZUSRC-LIKE.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TO-GROUP                    PIC X(20).
       01  X-ASLFDN                      LIKE ASLFDN OF PAZUSRC-LIKE.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TO-GROUP
                                X-ASLFDN.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0305" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PCONFIG-DP
           END-IF.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-TO-GROUP NOT = SPACES
            AND     X-ASLFDN   NOT = ZEROES
                    PERFORM START-PROCESS
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE SOURCEN KOPIEREN
     ‚*--------------------------------------------------------------
       START-PROCESS SECTION.
       STR-PRC-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H051"                   TO CFID   OF PCONFIG-P.
           MOVE     X-TO-GROUP               TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                   TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                   TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                   TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO STR-PRC-90
           END-IF.

       STR-PRC-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO STR-PRC-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       CFID   OF PCONFIG-P    NOT = "H051"
            OR      CFKEY  OF PCONFIG-P    NOT = X-TO-GROUP
                    GO TO STR-PRC-90
           END-IF.

     ‚* SOURCEN KOPIEREN
           INITIALIZE RET-CODE.
           MOVE     CFKEY2 OF PCONFIG-P TO USER-WRK.
           CALL     "AZP0303" USING USER-WRK
                                    X-ASLFDN
                                    RET-CODE
           END-CALL.

           GO TO    STR-PRC-20.

       STR-PRC-90.
           EXIT.
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
