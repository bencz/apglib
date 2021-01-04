       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              AZP0303.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCEN EINER ÜBUNG KOPIEREN                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  03.08.2010                                   *
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
     ‚* PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
     ‚*-------------------------------------------------------------
           SELECT PAZUSRC-DP
                  ASSIGN       TO  DATABASE-PAZUSRC
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
     ‚*-------------------------------------------------------------
           SELECT PAZUSRC-DP1
                  ASSIGN       TO  DATABASE-PAZUSRC
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
       FD  PAZUSRC-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUSRC-P.
           COPY DDS-ALL-FORMATS OF PAZUSRC.
      /
     ‚*--- PAZUSRC: ASUSER, ASLFDN, ASSLIB, ASSRCN, ASSMBR
       FD  PAZUSRC-DP1
           LABEL RECORDS ARE STANDARD.
       01  PAZUSRC-P1.
           COPY DDS-ALL-FORMATS OF PAZUSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0303".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CPYF-REC                      TYPE CPYF-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.

       01  TO-LIB                        PIC X(10).
       01  TO-RCN                        PIC X(10).
       01  TO-MBR                        PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TO-USER                     LIKE ASUSER OF PAZUSRC-P.
       01  X-ASLFDN                      LIKE ASLFDN OF PAZUSRC-P.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TO-USER
                                X-ASLFDN
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0303" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT PAZUSRC-DP.
           OPEN     I-O   PAZUSRC-DP1.

           INITIALIZE X-RETCODE.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-TO-USER = SPACES
            OR      X-ASLFDN  = ZEROES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* JETZT CFG-CPY LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"              TO CFID   OF CFG-CPY.
           MOVE     X-TO-USER           TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

     ‚* ALLE SOURCEN PRÜFEN
           PERFORM  CHECK-ALL-SOURCES.
           IF       RET-CODE NOT = SPACES
                    MOVE "3" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* ALLE SOURCEN KOPIEREN
           PERFORM  COPY-ALL-SOURCES.

       ENDE.
           CLOSE                 PAZUSRC-DP
                                 PAZUSRC-DP1.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE SOURCEN KOPIEREN
     ‚*--------------------------------------------------------------
       COPY-ALL-SOURCES SECTION.
       CPY-ASR-00.

     ‚* DIE GENERELLEN SOURCEN SIND MIT *DEFAULT GEKENNZEICHNET
           INITIALIZE PAZUSRCF OF PAZUSRC-P.
           MOVE     "*DEFAULT"               TO ASUSER OF PAZUSRC-P.
           MOVE     X-ASLFDN                 TO ASLFDN OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSLIB OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSRCN OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSMBR OF PAZUSRC-P.
           START    PAZUSRC-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CPY-ASR-90
           END-IF.

        CPY-ASR-20.
           READ     PAZUSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CPY-ASR-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       ASUSER OF PAZUSRC-P    NOT = "*DEFAULT"
            OR      ASLFDN OF PAZUSRC-P    NOT = X-ASLFDN
                    GO TO CPY-ASR-90
           END-IF.

     ‚* BESTIMMTE SOURCE-DATEIEN KÖNNEN GEMAPPT WERDEN
           PERFORM  MAP-FILES.

           INITIALIZE PAZUSRCF OF PAZUSRC-P1.
           MOVE     CORR PAZUSRCF OF PAZUSRC-P TO
                         PAZUSRCF OF PAZUSRC-P1.
     ‚* KEYS NOCHMAL EXPLIZIT FÜLLEN
           MOVE     X-TO-USER           TO ASUSER OF PAZUSRC-P1.
           MOVE     ASLFDN OF PAZUSRC-P TO ASLFDN OF PAZUSRC-P1.
           MOVE     TO-LIB              TO ASSLIB OF PAZUSRC-P1.
           MOVE     TO-RCN              TO ASSRCN OF PAZUSRC-P1.
           MOVE     TO-MBR              TO ASSMBR OF PAZUSRC-P1.

     ‚* DATEN FÜLLEN
           MOVE     T-USER              TO ASAUSR OF PAZUSRC-P1.
           MOVE     T-TERM              TO ASABS  OF PAZUSRC-P1.
           MOVE     PGM-WRK             TO ASAPGM OF PAZUSRC-P1.
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8              TO ASADTA OF PAZUSRC-P1.
           MOVE     TIME-6              TO ASATIA OF PAZUSRC-P1.

           WRITE    PAZUSRC-P1.

     ‚* JETZT SOURCE KOPIEREN
           INITIALIZE CPYF-REC
           MOVE     ASSLIB OF PAZUSRC-P TO FROMLIB  OF CPYF-REC.
           MOVE     ASSRCN OF PAZUSRC-P TO FROMFILE OF CPYF-REC.
           MOVE     ASSMBR OF PAZUSRC-P TO FROMMBR  OF CPYF-REC.
           MOVE     TO-LIB              TO TOLIB    OF CPYF-REC.
           MOVE     TO-RCN              TO TOFILE   OF CPYF-REC.
           MOVE     TO-MBR              TO TOMBR    OF CPYF-REC.
           MOVE     "*REPLACE"          TO SRCOPT   OF CPYF-REC.
           MOVE     "*NOCHK"            TO FMTOPT   OF CPYF-REC.
           MOVE     "*NO"               TO CRTFIL   OF CPYF-REC.
           CALL     "CLP0053" USING CPYF-REC
                                    RET-CODE
           END-CALL.

           GO TO    CPY-ASR-20.

       CPY-ASR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEINAMEN MAPPEN
     ‚*--------------------------------------------------------------
       MAP-FILES SECTION.
       MAP-FIL-00.

           INITIALIZE TO-LIB.
           INITIALIZE TO-RCN.
           INITIALIZE TO-MBR.
     ‚* ERSTMAL SO ÜBERNEHMEN
           MOVE     ASSLIB OF PAZUSRC-P TO TO-LIB.
           MOVE     ASSRCN OF PAZUSRC-P TO TO-RCN.
           MOVE     ASSMBR OF PAZUSRC-P TO TO-MBR.

     ‚* BIBLIOTHEK MAPPEN
           IF       CFDAT1 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT1 OF CFG-CPY TO TO-LIB
           ELSE
                    MOVE X-TO-USER         TO TO-LIB
           END-IF.
     ‚* QCBLSRC MAPPEN
           IF       ASSRCN OF PAZUSRC-P     = "QCBLSRC"
            AND     CFDAT2 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT2 OF CFG-CPY TO TO-RCN
           END-IF.
     ‚* QDDSSRC MAPPEN
           IF       ASSRCN OF PAZUSRC-P     = "QDDSSRC"
            AND     CFDAT3 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT3 OF CFG-CPY TO TO-RCN
           END-IF.
     ‚* QCPYSRC MAPPEN
           IF       ASSRCN OF PAZUSRC-P     = "QCPYSRC"
            AND     CFDAT4 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT4 OF CFG-CPY TO TO-RCN
           END-IF.
     ‚* QCLSRC MAPPEN
           IF       ASSRCN OF PAZUSRC-P     = "QCLSRC"
            AND     CFDAT5 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT5 OF CFG-CPY TO TO-RCN
           END-IF.
     ‚* TEMPSRC MAPPEN
           IF       ASSRCN OF PAZUSRC-P     = "TEMPSRC"
            AND     CFDAT6 OF CFG-CPY   NOT = SPACES
                    MOVE CFDAT6 OF CFG-CPY TO TO-RCN
           END-IF.

       MAP-FIL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB AUCH ALLE SOURCEN EXISTIEREN
     ‚*--------------------------------------------------------------
       CHECK-ALL-SOURCES SECTION.
       CHK-ASR-00.

           INITIALIZE RET-CODE.
     ‚* DIE GENERELLEN SOURCEN SIND MIT *DEFAULT GEKENNZEICHNET
           INITIALIZE PAZUSRCF OF PAZUSRC-P.
           MOVE     "*DEFAULT"               TO ASUSER OF PAZUSRC-P.
           MOVE     X-ASLFDN                 TO ASLFDN OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSLIB OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSRCN OF PAZUSRC-P.
           MOVE     SPACES                   TO ASSMBR OF PAZUSRC-P.
           START    PAZUSRC-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-ASR-90
           END-IF.

        CHK-ASR-20.
           READ     PAZUSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-ASR-90
           END-IF.

     ‚* STIMMT DER KEY?
           IF       ASUSER OF PAZUSRC-P    NOT = "*DEFAULT"
            OR      ASLFDN OF PAZUSRC-P    NOT = X-ASLFDN
                    GO TO CHK-ASR-90
           END-IF.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB SOURCE VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     ASSLIB OF PAZUSRC-P TO LIB     OF CHKOBJ-REC.
           MOVE     ASSRCN OF PAZUSRC-P TO OBJ     OF CHKOBJ-REC.
           MOVE     ASSMBR OF PAZUSRC-P TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"             TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-ASR-90
           END-IF.

           GO TO    CHK-ASR-20.

       CHK-ASR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
