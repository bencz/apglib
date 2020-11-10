       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LESEN CFG-CPY                                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LESEN CFG-CPY                                *
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
     ‚* PHYSISCHE DATEI CONFIG
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
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY   OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8002".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  CFG-SAV                       TYPE CFG-CPY-TYPE.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CFG-CPY                     TYPE CFG-CPY-TYPE.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CFG-CPY.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP8002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI NUR EINMAL ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN INPUT PCONFIG-DP
     ‚*             BEIM ERSTEN MAL INITIALISIEREN
                    INITIALIZE PCONFIGF OF CFG-SAV
           END-IF.

           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     X-CFG-CPY TO CFG-CPY.

     ‚* GLEICHE NOCHMAL?
           IF       CFG-DA OF CFG-SAV     = 1
            AND     CFID   OF CFG-CPY     = CFID   OF CFG-SAV
            AND     CFKEY  OF CFG-CPY     = CFKEY  OF CFG-SAV
            AND     CFKEY2 OF CFG-CPY     = CFKEY2 OF CFG-SAV
            AND     CFKEY3 OF CFG-CPY     = CFKEY3 OF CFG-SAV
            AND     CFKEY4 OF CFG-CPY     = CFKEY4 OF CFG-SAV
            AND     CFTXT1 OF CFG-CPY NOT = "**RESET**"
                    MOVE CFG-SAV TO CFG-CPY
           ELSE
                    MOVE ZEROES TO CFG-DA OF CFG-CPY
     ‚*             ALLE PARAMETER VORHANDEN?
                    IF   CFID   OF CFG-CPY NOT = SPACES
                     AND CFKEY  OF CFG-CPY NOT = SPACES
     ‚*                  LESEN DER KONFIGURATION
                         PERFORM  LESEN-CONFIG
                    END-IF
           END-IF.

     ‚* WERT ÜBERNEHMEN
           MOVE     CFG-CPY TO CFG-SAV.
           MOVE     CFG-CPY TO X-CFG-CPY.

       ENDE.
     ‚*€   CLOSE                 PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DER KONFIGURATION
     ‚*--------------------------------------------------------------
       LESEN-CONFIG SECTION.
       LES-DAT-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF CFG-CPY TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF CFG-CPY TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF CFG-CPY TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF CFG-CPY TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF CFG-CPY TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    IF   CFKOPF OF PCONFIG-P = ZEROES
                         MOVE 1    TO CFG-DA OF CFG-CPY
                         MOVE CORR PCONFIGF OF PCONFIG-P TO
                                   PCONFIGF OF CFG-CPY
                    END-IF
           END-IF.

       LES-DAT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
