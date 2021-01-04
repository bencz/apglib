       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0024.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRAUM IMPORTIEREN                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.11.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  IMPORTIEREN DER PRAUM-DATEN                  *
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
     ‚* PRAUM: RARAUM
     ‚*-------------------------------------------------------------
           SELECT PRAUM-DP
                  ASSIGN       TO  DATABASE-PRAUM
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
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PRAUM: RARAUM
       FD  PRAUM-DP
           LABEL RECORDS ARE STANDARD.
       01  PRAUM-P.
           COPY DDS-ALL-FORMATS OF MAT-PRAUM.
      /
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF APG-PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* WORKBEREICH LOG-CPY
           COPY WRKLOGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0024".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.

       01  RECORD-OLD                    PIC X(8192).
       01  RECORD-NEW                    PIC X(8192).
       01  POS                           PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0024" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEIEN ÖFFNEN
           OPEN     I-O   PCONFIG-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* MAT-BIBLIOTHEK VORSCHALTEN
           PERFORM  ADD-MAT-LIB.

     ‚* PRÜFEN OB DIE MAT-BIBLIOTHEK GESETZT UND DIE DATEI
     ‚* VORHANDEN IST
           PERFORM  CHECK-OBJECTS.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE PCFGSTSF OF LOG-CPY
                    MOVE ":IMPORT" TO STFILE OF LOG-CPY
                    MOVE "ERROR"   TO STKEY  OF LOG-CPY
                    MOVE "ERR0007" TO STTID  OF LOG-CPY
                    MOVE "Import Failed: Library MAT not found" TO
                         STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.

     ‚* ÖFFNEN DER DATEIEN
           OPEN     INPUT PRAUM-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGSTSF OF LOG-CPY
                    MOVE ":IMPORT" TO STFILE OF LOG-CPY
                    MOVE "ERROR"   TO STKEY  OF LOG-CPY
                    MOVE "ERR0007" TO STTID  OF LOG-CPY
                    MOVE "Import Failed: PRAUM not opened" TO
                         STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.

     ‚* ALLE RÄUME IMPORTIEREN
           PERFORM  IMPORT-PRAUM.

           CLOSE    PRAUM-DP.

     ‚* MAT-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-MAT-LIB.

       ENDE.
           CLOSE    PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* MAT-BIBLIOTHEK VORSCHALTEN
     ‚*--------------------------------------------------------------
       ADD-MAT-LIB SECTION.
       ADD-MAT-00.

     ‚* "MAT" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE ADDLIBLE-REC.
           MOVE     "MAT"        TO LIB     OF ADDLIBLE-REC.
           MOVE     "*LAST"      TO LIBPOS  OF ADDLIBLE-REC.
           CALL     "CLP0007" USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

       ADD-MAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAT-BIBLIOTHEK ENTFERNEN
     ‚*--------------------------------------------------------------
       DEL-MAT-LIB SECTION.
       DEL-MAT-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE RMVLIBLE-REC.
           MOVE     "MAT"        TO LIB     OF RMVLIBLE-REC.
           CALL     "CLP0009" USING RMVLIBLE-REC
           END-CALL.

       DEL-MAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* IMPORTIEREN DES RAUMS
     ‚*--------------------------------------------------------------
       IMPORT-PRAUM SECTION.
       IMP-PRA-00.

     ‚* RAUM LESEN
           INITIALIZE PRAUMF OF PRAUM-P.
           MOVE     SPACES              TO RARAUM OF PRAUM-P.
           START    PRAUM-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO IMP-PRA-90
           END-IF.

       IMP-PRA-20.
           READ     PRAUM-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO IMP-PRA-90
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* RAUM SCHON ANGELEGT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H052"              TO CFID   OF CFG-CPY.
           MOVE     RARAUM OF PRAUM-P   TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
                    PERFORM UPDATE-DATEN
           ELSE
                    PERFORM WRITE-DATEN
           END-IF.

           GO TO    IMP-PRA-20.

       IMP-PRA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES PCONFIG-SATZES
     ‚*--------------------------------------------------------------
       WRITE-DATEN SECTION.
       WRI-DAT-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H052"                TO CFID   OF PCONFIG-P.
           MOVE     RARAUM OF PRAUM-P     TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
     ‚* BEZEICHNUNG 1/2
           MOVE     RABEZ1 OF PRAUM-P     TO CFIN21 OF PCONFIG-P.
           MOVE     RABEZ2 OF PRAUM-P     TO CFIN22 OF PCONFIG-P.
     ‚* ANZEIGENAME = BEZEICHNUNG 1
           MOVE     RABEZ1 OF PRAUM-P     TO CFTXT1 OF PCONFIG-P.

     ‚* BEREICH
           MOVE     RABER  OF PRAUM-P     TO CFIN01 OF PCONFIG-P.

     ‚* USER/DATUM/UHRZEIT FÜLLEN
           MOVE     T-USER                TO CFAUSR OF PCONFIG-P
           MOVE     T-TERM                TO CFABS  OF PCONFIG-P
           MOVE     PGM-WRK               TO CFAPGM OF PCONFIG-P
           MOVE     DATE-8                TO CFADTA OF PCONFIG-P
           MOVE     TIME-6                TO CFATIA OF PCONFIG-P
           WRITE    PCONFIG-P.

       WRI-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN DER DATEN
     ‚*--------------------------------------------------------------
       UPDATE-DATEN SECTION.
       UPD-DAT-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H052"                TO CFID   OF PCONFIG-P.
           MOVE     RARAUM OF PRAUM-P     TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-DAT-90
           END-IF.

     ‚* ALTEN RECORD SICHERN
           MOVE     PCONFIGF OF PCONFIG-P TO RECORD-OLD.

     ‚* BEZEICHNUNG 1/2
           MOVE     RABEZ1 OF PRAUM-P     TO CFIN21 OF PCONFIG-P.
           MOVE     RABEZ2 OF PRAUM-P     TO CFIN22 OF PCONFIG-P.
     ‚* ANZEIGENAME = BEZEICHNUNG 1
           MOVE     RABEZ1 OF PRAUM-P     TO CFTXT1 OF PCONFIG-P.

     ‚* BEREICH
           MOVE     RABER  OF PRAUM-P     TO CFIN01 OF PCONFIG-P.

     ‚* NEUEN RECORD ÜBERNEHMEN
           MOVE     PCONFIGF OF PCONFIG-P TO RECORD-NEW.

     ‚* USER/DATUM/UHRZEIT FÜLLEN
           IF       RECORD-OLD NOT = RECORD-NEW
                    MOVE     T-USER       TO CFMSER OF PCONFIG-P
                    MOVE     T-TERM       TO CFMBS  OF PCONFIG-P
                    MOVE     PGM-WRK      TO CFMPGM OF PCONFIG-P
                    MOVE     DATE-8       TO CFMDTA OF PCONFIG-P
                    MOVE     TIME-6       TO CFMTIA OF PCONFIG-P
           END-IF.
           REWRITE  PCONFIG-P.

       UPD-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEIEN AUCH GIBT
     ‚*--------------------------------------------------------------
       CHECK-OBJECTS SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "PRAUM"      TO OBJ     OF CHKOBJ-REC.
           MOVE     "*LIBL"      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

       CHK-OBJ-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-WRITE-LOG-CPY: SCHREIBEN EINES LOG-SATZES
     ‚*---------------------------------------------------------------

           COPY     LOGCPY     OF APG-QCPYSRC.
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
