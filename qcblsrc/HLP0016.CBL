       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0016.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PVEREIN IMPORTIEREN                                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  24.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  IMPORTIEREN DER PVEREIN-DATEN                *
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
     ‚* PHYSISCHE DATEI VEREINE
     ‚*-------------------------------------------------------------
           SELECT PVEREIN-DP
                  ASSIGN       TO  DATABASE-PVEREIN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
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
     ‚*--- VEREINE
       FD  PVEREIN-DP
           LABEL RECORDS ARE STANDARD.
       01  PVEREIN-P.
           COPY DDS-ALL-FORMATS OF AMU-PVEREIN.
      /
     ‚*--- PROGRAMME
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

       01  PGM-WRK                       PIC X(10) VALUE "HLP0016".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

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
           MOVE     "HLP0016" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEIEN ÖFFNEN
           OPEN     I-O   PCONFIG-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* AMU-BIBLIOTHEK VORSCHALTEN
           PERFORM  ADD-AMU-LIB.

     ‚* PRÜFEN OB DIE AMU-BIBLIOTHEK GESETZT UND DIE DATEI
     ‚* VORHANDEN IST
           PERFORM  CHECK-OBJECTS.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÖFFNEN DER DATEIEN
           OPEN     INPUT PVEREIN-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* ALLE USER IMPORTIEREN
           PERFORM  IMPORT-PVEREIN.

           CLOSE    PVEREIN-DP.

     ‚* AMU-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-AMU-LIB.

       ENDE.
           CLOSE    PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* MAT-BIBLIOTHEK VORSCHALTEN
     ‚*--------------------------------------------------------------
       ADD-AMU-LIB SECTION.
       ADD-AMU-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE ADDLIBLE-REC.
           MOVE     "AMU"        TO LIB     OF ADDLIBLE-REC.
           MOVE     "*LAST"      TO LIBPOS  OF ADDLIBLE-REC.
           CALL     "CLP0007" USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

       ADD-AMU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAT-BIBLIOTHEK ENTFERNEN
     ‚*--------------------------------------------------------------
       DEL-AMU-LIB SECTION.
       DEL-AMU-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE RMVLIBLE-REC.
           MOVE     "AMU"        TO LIB     OF RMVLIBLE-REC.
           CALL     "CLP0009" USING RMVLIBLE-REC
           END-CALL.

       DEL-AMU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE TELEFONNUMMER GIBT
     ‚*--------------------------------------------------------------
       IMPORT-PVEREIN SECTION.
       IMP-PBI-00.

     ‚* PVEREIN LESEN
           INITIALIZE PVEREINF OF PVEREIN-P.
           MOVE     ZEROES              TO FVLFDN OF PVEREIN-P.
           START    PVEREIN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO IMP-PBI-90
           END-IF.

       IMP-PBI-20.
           READ     PVEREIN-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO IMP-PBI-90
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* IST DER USER SCHON ANGELEGT? DANN MÜSSEN EVTL.
     ‚* BESTIMMTE DATEN AKTUALISIERT WERDEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "I101"               TO CFID   OF CFG-CPY.
           MOVE     FVLFDN OF PVEREIN-P  TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
                    PERFORM UPDATE-DATEN
           ELSE
                    PERFORM WRITE-DATEN
           END-IF.

           GO TO    IMP-PBI-20.

       IMP-PBI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES PCONFIG-SATZES
     ‚*--------------------------------------------------------------
       WRITE-DATEN SECTION.
       WRI-DAT-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "I101"                TO CFID   OF PCONFIG-P.
           MOVE     FVLFDN OF PVEREIN-P   TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
     ‚* NAMEN ZUSAMMENBAUEN
           MOVE     FVNAME OF PVEREIN-P   TO CFTXT1 OF PCONFIG-P.

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
           MOVE     "I101"                TO CFID   OF PCONFIG-P.
           MOVE     FVLFDN OF PVEREIN-P   TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-DAT-90
           END-IF.

     ‚* ALTEN RECORD SICHERN
           MOVE     PCONFIGF OF PCONFIG-P TO RECORD-OLD.

     ‚* NAMEN ZUSAMMENBAUEN
           MOVE     FVNAME OF PVEREIN-P   TO CFTXT1 OF PCONFIG-P.

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
           MOVE     "PVEREIN"    TO OBJ     OF CHKOBJ-REC.
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
