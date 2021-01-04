       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AZP0306.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCEN EINER ÜBUNG KOPIEREN                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  *LIBL SETZEN                                 *
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

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0306".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.

       01  LIB-WRK                       PIC X(10).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TO-GROUP                    PIC X(20).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TO-GROUP.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0306" TO PGM-WRK.
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

     ‚* JETZT DIE CFG-CPY LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"               TO CFID   OF CFG-CPY.
           MOVE     CFKEY2  OF PCONFIG-P TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

     ‚* BIBLIOTHEK MAPPEN
           IF       CFDAT1 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT1 OF CFG-CPY   TO LIB-WRK
           ELSE
                    MOVE CFKEY2 OF PCONFIG-P TO LIB-WRK
           END-IF.

           INITIALIZE ADDLIBLE-REC.
           MOVE     LIB-WRK      TO LIB    OF ADDLIBLE-REC.
           MOVE     "*LAST"      TO LIBPOS OF ADDLIBLE-REC.
           CALL     "CLP0007" USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

           GO TO    STR-PRC-20.

       STR-PRC-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
