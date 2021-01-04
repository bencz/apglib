       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0015.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PBIRTH IMPORTIEREN                                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  24.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  IMPORTIEREN DER PBIRTH-DATEN                 *
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
     ‚* PBIRTH: BILFDN
     ‚*-------------------------------------------------------------
           SELECT PBIRTH-DP
                  ASSIGN       TO  DATABASE-PBIRTH
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
     ‚*--- PBIRTH: BILFDN
       FD  PBIRTH-DP
           LABEL RECORDS ARE STANDARD.
       01  PBIRTH-P.
           COPY DDS-ALL-FORMATS OF MAT-PBIRTH.
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

       01  PGM-WRK                       PIC X(10) VALUE "HLP0015".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.
       01  SNDMSG-REC                    TYPE SNDMSG-TYPE.

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
           MOVE     "HLP0015" TO PGM-WRK.
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
           OPEN     INPUT PBIRTH-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGSTSF OF LOG-CPY
                    MOVE ":IMPORT" TO STFILE OF LOG-CPY
                    MOVE "ERROR"   TO STKEY  OF LOG-CPY
                    MOVE "ERR0007" TO STTID  OF LOG-CPY
                    MOVE "Import Failed: PBIRTH not opened" TO
                         STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.

     ‚* ALLE USER IMPORTIEREN
           PERFORM  IMPORT-PBIRTH.

           CLOSE    PBIRTH-DP.

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
     ‚* PRÜFEN OB ES DIE TELEFONNUMMER GIBT
     ‚*--------------------------------------------------------------
       IMPORT-PBIRTH SECTION.
       IMP-PBI-00.

     ‚* PBIRTH LESEN
           INITIALIZE PBIRTHF OF PBIRTH-P.
           MOVE     ZEROES              TO BILFDN OF PBIRTH-P.
           START    PBIRTH-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO IMP-PBI-90
           END-IF.

       IMP-PBI-20.
           READ     PBIRTH-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO IMP-PBI-90
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* IST DER USER SCHON ANGELEGT? DANN MÜSSEN EVTL.
     ‚* BESTIMMTE DATEN AKTUALISIERT WERDEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"              TO CFID   OF CFG-CPY.
           MOVE     BIMAKU OF PBIRTH-P  TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
     ‚*             DAMIT MAT KEINE PROBLEME BEKOMMT, LÖSCHEN WIR
     ‚*             ALLE INAKTIVEN MITARBEITER
                    IF   BIKZ   OF PBIRTH-P = ZEROES
                         PERFORM DELETE-DATEN
                    ELSE
                         PERFORM UPDATE-DATEN
                    END-IF
           ELSE
     ‚*             DAMIT MAT KEINE PROBLEME BEKOMMT, NUR DIE AKTIVEN
     ‚*             MITARBEITER ÜBERNEHMEN
                    IF   BIKZ   OF PBIRTH-P NOT = ZEROES
                         PERFORM WRITE-DATEN
                    END-IF
           END-IF.

     ‚* SCHON ALS AZUBI ANGELEGT?
           IF       1 = 2
            AND     BIKZ   OF PBIRTH-P   NOT = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     "V100"              TO CFID   OF CFG-CPY
                    MOVE     BIMAKU OF PBIRTH-P  TO CFKEY  OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
                    IF   CFG-DA OF CFG-CPY = ZEROES
                         PERFORM WRITE-DATEN-V100
                    END-IF
           ELSE
                    PERFORM DELETE-DATEN-V100
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
           MOVE     "H050"                TO CFID   OF PCONFIG-P.
           MOVE     BIMAKU OF PBIRTH-P    TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
     ‚* VORNAME, NACHNAME
           MOVE     BIVNAM OF PBIRTH-P    TO CFIN21 OF PCONFIG-P.
           MOVE     BINNAM OF PBIRTH-P    TO CFIN22 OF PCONFIG-P.
     ‚* NAMEN ZUSAMMENBAUEN
           MOVE     BINNAM OF PBIRTH-P    TO CFTXT1 OF PCONFIG-P.
           MOVE     LENGTH OF CFTXT1 OF PCONFIG-P TO POS.
           PERFORM  WITH TEST BEFORE UNTIL POS = ZEROES
                     OR CFTXT1 OF PCONFIG-P(POS:1) NOT = SPACES
                    SUBTRACT 1 FROM POS
           END-PERFORM.
           IF       BIVNAM OF PBIRTH-P NOT = SPACES
                    MOVE ", " TO CFTXT1 OF PCONFIG-P(POS + 1:)
                    MOVE BIVNAM OF PBIRTH-P    TO
                         CFTXT1 OF PCONFIG-P(POS + 3:)
           END-IF.

     ‚* TELEFON
           MOVE     BIHTEL OF PBIRTH-P    TO CFIN23 OF PCONFIG-P.
           MOVE     BIMTEL OF PBIRTH-P    TO CFIN24 OF PCONFIG-P.
           MOVE     BIBTEL OF PBIRTH-P    TO CFIN25 OF PCONFIG-P.
     ‚* RAUM
           MOVE     BIRAUM OF PBIRTH-P    TO CFIN26 OF PCONFIG-P.
     ‚* DURCHWAHLEN
           MOVE     BIDW1  OF PBIRTH-P    TO CFIN01 OF PCONFIG-P.
           MOVE     BIDW2  OF PBIRTH-P    TO CFIN02 OF PCONFIG-P.
     ‚* KFZ-KENNZEICHEN
           STRING   BIKFUZ OF PBIRTH-P DELIMITED BY SIZE
     ‚*€            "-"                DELIMITED BY SIZE
                    BIKFE1 OF PBIRTH-P DELIMITED BY SIZE
     ‚*€            "-"                DELIMITED BY SIZE
                    BIKFE2 OF PBIRTH-P DELIMITED BY SIZE
               INTO CFIN03 OF PCONFIG-P
           END-STRING.
     ‚* GEBURTSTAG ZUSAMMENBAUEN
           MOVE     BIGJH  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(1:2).
           MOVE     BIGJA  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(3:2).
           MOVE     BIGMO  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(5:2).
           MOVE     BIGTA  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(7:2).

     ‚* FUNKTIONSFELDER ÜBERNEHMEN
           IF       BIFKT1 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT1 OF PBIRTH-P    TO CFFK01 OF PCONFIG-P
           END-IF.
           IF       BIFKT2 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT2 OF PBIRTH-P    TO CFFK02 OF PCONFIG-P
           END-IF.
           IF       BIFKT3 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT3 OF PBIRTH-P    TO CFFK03 OF PCONFIG-P
           END-IF.
           IF       BIFKT4 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT4 OF PBIRTH-P    TO CFFK04 OF PCONFIG-P
           END-IF.
           IF       BIFKT5 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT5 OF PBIRTH-P    TO CFFK05 OF PCONFIG-P
           END-IF.
           IF       BIFKT6 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT6 OF PBIRTH-P    TO CFFK06 OF PCONFIG-P
           END-IF.
           IF       BIFKT7 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT7 OF PBIRTH-P    TO CFFK07 OF PCONFIG-P
           END-IF.
           IF       BIFKT8 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT8 OF PBIRTH-P    TO CFFK08 OF PCONFIG-P
           END-IF.
           IF       BIFKT9 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT9 OF PBIRTH-P    TO CFFK09 OF PCONFIG-P
           END-IF.
     ‚* GESCHLECHT (M, W, _)
           EVALUATE BIGES  OF PBIRTH-P
                    WHEN "M"     MOVE 1   TO CFFK10 OF PCONFIG-P
                    WHEN "W"     MOVE 2   TO CFFK10 OF PCONFIG-P
                    WHEN SPACES  MOVE 9   TO CFFK10 OF PCONFIG-P
           END-EVALUATE.
     ‚* MITARBEITER AKTIV?
           MOVE     BIKZ   OF PBIRTH-P    TO CFFK11 OF PCONFIG-P.

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
     ‚* SCHREIBEN EINES PCONFIG-SATZES
     ‚*--------------------------------------------------------------
       WRITE-DATEN-V100 SECTION.
       WRI-DAT-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "V100"                TO CFID   OF PCONFIG-P.
           MOVE     BIMAKU OF PBIRTH-P    TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
     ‚* NAMEN ZUSAMMENBAUEN
           MOVE     BINNAM OF PBIRTH-P    TO CFTXT1 OF PCONFIG-P.
           MOVE     LENGTH OF CFTXT1 OF PCONFIG-P TO POS.
           PERFORM  WITH TEST BEFORE UNTIL POS = ZEROES
                     OR CFTXT1 OF PCONFIG-P(POS:1) NOT = SPACES
                    SUBTRACT 1 FROM POS
           END-PERFORM.
           IF       BIVNAM OF PBIRTH-P NOT = SPACES
                    MOVE ", " TO CFTXT1 OF PCONFIG-P(POS + 1:)
                    MOVE BIVNAM OF PBIRTH-P TO
                         CFTXT1 OF PCONFIG-P(POS + 3:)
           END-IF.

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
           MOVE     "H050"                TO CFID   OF PCONFIG-P.
           MOVE     BIMAKU OF PBIRTH-P    TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-DAT-90
           END-IF.

     ‚* ALTEN RECORD SICHERN
           MOVE     PCONFIGF OF PCONFIG-P TO RECORD-OLD.

     ‚* VORNAME, NACHNAME ÜBERNEHMEN
           MOVE     BIVNAM OF PBIRTH-P    TO CFIN21 OF PCONFIG-P.
           MOVE     BINNAM OF PBIRTH-P    TO CFIN22 OF PCONFIG-P.
     ‚* NAMEN ZUSAMMENBAUEN
           MOVE     BINNAM OF PBIRTH-P    TO CFTXT1 OF PCONFIG-P.
           MOVE     LENGTH OF CFTXT1 OF PCONFIG-P TO POS.
           PERFORM  WITH TEST BEFORE UNTIL POS = ZEROES
                     OR CFTXT1 OF PCONFIG-P(POS:1) NOT = SPACES
                    SUBTRACT 1 FROM POS
           END-PERFORM.
           IF       BIVNAM OF PBIRTH-P NOT = SPACES
                    MOVE ", "   TO CFTXT1 OF PCONFIG-P(POS + 1:)
                    MOVE BIVNAM OF PBIRTH-P TO
                         CFTXT1 OF PCONFIG-P(POS + 3:)
           END-IF.
     ‚* TELEFON ÜBERNEHMEN
           MOVE     BIHTEL OF PBIRTH-P    TO CFIN23 OF PCONFIG-P.
           MOVE     BIMTEL OF PBIRTH-P    TO CFIN24 OF PCONFIG-P.
           MOVE     BIBTEL OF PBIRTH-P    TO CFIN25 OF PCONFIG-P.
     ‚* RAUM ÜBERNEHMEN
           MOVE     BIRAUM OF PBIRTH-P    TO CFIN26 OF PCONFIG-P.
     ‚* DURCHWAHLEN ÜBERNEHMEN
           MOVE     BIDW1  OF PBIRTH-P    TO CFIN01 OF PCONFIG-P.
           MOVE     BIDW2  OF PBIRTH-P    TO CFIN02 OF PCONFIG-P.
     ‚* KFZ-KENNZEICHEN ÜBERNEHMEN
           STRING   BIKFUZ OF PBIRTH-P DELIMITED BY SIZE
     ‚*€            "-"                DELIMITED BY SIZE
                    BIKFE1 OF PBIRTH-P DELIMITED BY SIZE
     ‚*€            "-"                DELIMITED BY SIZE
                    BIKFE2 OF PBIRTH-P DELIMITED BY SIZE
               INTO CFIN03 OF PCONFIG-P
           END-STRING.
     ‚* GEBURTSTAG ZUSAMMENBAUEN UND ÜBERNEHMEN
           MOVE     BIGJH  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(1:2).
           MOVE     BIGJA  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(3:2).
           MOVE     BIGMO  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(5:2).
           MOVE     BIGTA  OF PBIRTH-P    TO CFIN04 OF PCONFIG-P(7:2).

     ‚* FUNKTIONSFELDER ÜBERNEHMEN
           IF       BIFKT1 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT1 OF PBIRTH-P    TO CFFK01 OF PCONFIG-P
           END-IF.
           IF       BIFKT2 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT2 OF PBIRTH-P    TO CFFK02 OF PCONFIG-P
           END-IF.
           IF       BIFKT3 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT3 OF PBIRTH-P    TO CFFK03 OF PCONFIG-P
           END-IF.
           IF       BIFKT4 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT4 OF PBIRTH-P    TO CFFK04 OF PCONFIG-P
           END-IF.
           IF       BIFKT5 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT5 OF PBIRTH-P    TO CFFK05 OF PCONFIG-P
           END-IF.
           IF       BIFKT6 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT6 OF PBIRTH-P    TO CFFK06 OF PCONFIG-P
           END-IF.
           IF       BIFKT7 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT7 OF PBIRTH-P    TO CFFK07 OF PCONFIG-P
           END-IF.
           IF       BIFKT8 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT8 OF PBIRTH-P    TO CFFK08 OF PCONFIG-P
           END-IF.
           IF       BIFKT9 OF PBIRTH-P IS NUMERIC
                    MOVE BIFKT9 OF PBIRTH-P    TO CFFK09 OF PCONFIG-P
           END-IF.
     ‚* GESCHLECHT (M, W, _)
           EVALUATE BIGES  OF PBIRTH-P
                    WHEN "M"     MOVE 1   TO CFFK10 OF PCONFIG-P
                    WHEN "W"     MOVE 2   TO CFFK10 OF PCONFIG-P
                    WHEN SPACES  MOVE 9   TO CFFK10 OF PCONFIG-P
           END-EVALUATE.
     ‚* MITARBEITER AKTIV?
           MOVE     BIKZ   OF PBIRTH-P    TO CFFK11 OF PCONFIG-P.

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

     ‚* MEIN NUMMERNSCHILD GEFÜLLT??
           IF       CFKEY  OF PCONFIG-P    = "APG"
            AND     BIKFUZ OF PBIRTH-P NOT = SPACES
                    INITIALIZE SNDMSG-REC
     ‚*             PGM SOWIE DOPPELPUNKT
                    STRING PGM-WRK ": "
                           "Nummernschild in GB! "
                           DELIMITED BY SIZE
                      INTO MSGTXT OF SNDMSG-REC
                    END-STRING
     ‚*             USER AN DEM DIE NACHRICHT GEHT
                    MOVE "APG"        TO USER OF SNDMSG-REC
                    CALL "CLP0010" USING SNDMSG-REC
                                         RET-CODE
                    END-CALL
           END-IF.

       UPD-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN DER DATEN
     ‚*--------------------------------------------------------------
       DELETE-DATEN SECTION.
       DEL-DAT-00.

           PERFORM  DELETE-DATEN-H050.
           PERFORM  DELETE-DATEN-V100.

       DEL-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUS DEM MITARBEITERSTAMM LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-DATEN-H050 SECTION.
       DLT-H05-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H050"                TO CFID   OF PCONFIG-P.
           MOVE     BIMAKU OF PBIRTH-P    TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCONFIG-DP
           END-IF.

       DLT-H05-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUS DEM AZUBISTAMM LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-DATEN-V100 SECTION.
       DLT-V10-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "V100"                TO CFID   OF PCONFIG-P.
           MOVE     BIMAKU OF PBIRTH-P    TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES                TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCONFIG-DP
           END-IF.

       DLT-V10-90.
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
           MOVE     "PBIRTH"     TO OBJ     OF CHKOBJ-REC.
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
