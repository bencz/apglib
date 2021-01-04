     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0050.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ESSENSMAIL SCHREIBEN                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.12.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SCHREIBEN DER ESSENSMAIL AUF BASIS PFODAW    *
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
     ‚* PFODAWL2: FABJH, FABJA, FABMO, FABTA, FARTRT, FAOZE, FAGERT
     ‚*-------------------------------------------------------------
           SELECT PFODAW-DL
                  ASSIGN       TO  DATABASE-PFODAWL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PFODAWL2: FABJH, FABJA, FABMO, FABTA, FARTRT, FAOZE, FAGERT
       FD  PFODAW-DL
           LABEL RECORDS ARE STANDARD.
       01  PFODAW-L.
           COPY DDS-ALL-FORMATS OF MAT-PFODAWL2.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* WORKBEREICH LOG-CPY
           COPY WRKLOGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0050".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.

       01  MAIL-ANZ                      LIKE MAIL-ANZAHL OF MAIL-EMPF.
       01  MAIL-C-ANZ                    LIKE MAIL-ANZAHL OF MAIL-EMPF.
       01  POS-WRK                       PIC 9(2).
       01  POS-STR                       PIC X(2).

     ‚* PARAMETER FÜR DAS HLP0022
       01  SENDER-KEY                    LIKE CFKEY  OF CFG-CPY.
       01  CHECK-ID                      LIKE CFID   OF CFG-CPY.
       01  CHECK-KEY                     LIKE CFKEY  OF CFG-CPY.
       01  MAIL-AKTION                   PIC X(20).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-DATE                        LIKE DATE-8.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-DATE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0050" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

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
                    MOVE ":ERROR"  TO STFILE OF LOG-CPY
                    MOVE "ERROR"   TO STKEY  OF LOG-CPY
                    MOVE "ERR0009" TO STTID  OF LOG-CPY
                    MOVE "ADDLIBLE failed: Library MAT not found" TO
                         STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.

     ‚* ÖFFNEN DER DATEIEN
           OPEN     INPUT PFODAW-DL.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGSTSF OF LOG-CPY
                    MOVE ":ERROR"  TO STFILE OF LOG-CPY
                    MOVE "ERROR"   TO STKEY  OF LOG-CPY
                    MOVE "ERR0007" TO STTID  OF LOG-CPY
                    MOVE "Open failed: PFODAW not opened" TO
                         STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.

     ‚* MAILEINSTELLUNGEN LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A400"            TO CFID   OF CFG-CPY.
           MOVE     "ESSEN400"        TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
            AND     CFFK01 OF CFG-CPY = 1
                    PERFORM CHECK-FOOD-USER
                    IF   DATEN NOT = ZEROES
                         PERFORM WRITE-MAIL
                    END-IF
           END-IF.

           CLOSE    PFODAW-DL.

     ‚* MAT-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-MAT-LIB.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE USER MIT DIESEM DATUM HERAUSLESEN
     ‚*--------------------------------------------------------------
       CHECK-FOOD-USER SECTION.
       CHK-FDU-00.

     ‚* ERMITTELN DATUM/ZEIT
           IF       X-DATE = ZEROES
                    PERFORM  COPY-GET-TIME
                    MOVE     DATE-8 TO X-DATE
           END-IF.

           INITIALIZE DATEN.
           INITIALIZE POS-WRK.
           INITIALIZE MAIL-TAB.
           MOVE     ZEROES TO MAIL-ANZ, MAIL-C-ANZ.
     ‚* MIT TAG UND MONAT AUFSETZEN
           INITIALIZE PFODAWF OF PFODAW-L.
           MOVE     X-DATE(1:2) TO FABJH  OF PFODAW-L.
           MOVE     X-DATE(3:2) TO FABJA  OF PFODAW-L.
           MOVE     X-DATE(5:2) TO FABMO  OF PFODAW-L.
           MOVE     X-DATE(7:2) TO FABTA  OF PFODAW-L.
           MOVE     SPACES      TO FARTRT OF PFODAW-L.
           MOVE     SPACES      TO FAOZE  OF PFODAW-L.
           MOVE     SPACES      TO FAGERT OF PFODAW-L.
           START    PFODAW-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-FDU-90
           END-IF.

       CHK-FDU-20.
           READ     PFODAW-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-FDU-90
           END-IF.

           IF       FABJH  OF PFODAW-L NOT = X-DATE(1:2)
            OR      FABJA  OF PFODAW-L NOT = X-DATE(3:2)
            OR      FABMO  OF PFODAW-L NOT = X-DATE(5:2)
            OR      FABTA  OF PFODAW-L NOT = X-DATE(7:2)
                    GO TO CHK-FDU-90
           END-IF.

     ‚* NUR VOR ORT-EINTRÄGE
           IF       FAOZE  OF PFODAW-L NOT = "1"
                    GO TO CHK-FDU-20
           END-IF.

     ‚* JETZT DIE PERSON ANHÄNGEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"              TO CFID   OF CFG-CPY.
           MOVE     FAMAKU OF PFODAW-L  TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.

           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE FAMAKU OF PFODAW-L TO CFKEY  OF CFG-CPY
           END-IF.

           PERFORM  ADD-TO-MAIL.

     ‚* JETZT DIE PERSON ANHÄNGEN
           PERFORM  FILL-RECEIVER.

           GO TO    CHK-FDU-20.

       CHK-FDU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL AUFBAUEN
     ‚*--------------------------------------------------------------
       ADD-TO-MAIL SECTION.
       ADD-TOM-00.

           ADD      1 TO POS-WRK.
     ‚* EMAIL BODY ALLGEMEINE INFOS
           IF       DATEN = ZEROES
                    MOVE 1 TO DATEN
     ‚*             EINLEITUNG DER MAIL
                    MOVE     1 TO MAIL-LEN
                    STRING   "Hallo,"                 DELIMITED BY SIZE
                             X"2525"                  DELIMITED BY SIZE
                             "folgende Personen "     DELIMITED BY SIZE
                             "essen heute vor Ort"    DELIMITED BY SIZE
                             ":"                      DELIMITED BY SIZE
                             X"25"                    DELIMITED BY SIZE
                             "======================" DELIMITED BY SIZE
                             "======================" DELIMITED BY SIZE
                             X"2525"                  DELIMITED BY SIZE
                      INTO MAIL-DATA
                      WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

     ‚* POSITION ÜBERNEHMEN
           MOVE     POS-WRK TO POS-STR.
           INSPECT  POS-STR REPLACING LEADING ZEROES BY SPACES.

     ‚* JETZT TEXT FÜR DEN USER
           STRING   POS-STR ". " CFKEY  OF CFG-CPY(1:10) " / "
                    FAGERT OF PFODAW-L " / "
                    FUNCTION TRIM(CFIN21 OF CFG-CPY) " "
                    FUNCTION TRIM(CFIN22 OF CFG-CPY) X"25"
                    DELIMITED BY SIZE
               INTO MAIL-DATA
               WITH POINTER MAIL-LEN
           END-STRING.

       ADD-TOM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL VORBEREITEN UND SENDEN
     ‚*--------------------------------------------------------------
       WRITE-MAIL SECTION.
       WRT-MAI-00.

     ‚* MAIL-TOPIC AUFRUFEN
           MOVE     X-DATE                 TO DATE-DB.
           MOVE     "TO-DSP"               TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.

           STRING   "ESSEN400 "        DELIMITED BY SIZE
                    DATUM-ALPHA-10     DELIMITED BY SIZE
               INTO MAIL-TOPIC OF MAIL-TAB
           END-STRING.

     ‚* MAILPROGRAMM AUFRUFEN
           MOVE     "ESSEN400"     TO SENDER-KEY.
           MOVE     "H051"         TO CHECK-ID.
           MOVE     "ESSEN400"     TO CHECK-KEY.
           CALL     "HLP0022"  USING SENDER-KEY
                                     CHECK-ID
                                     CHECK-KEY
                                     MAIL-AKTION
                                     T-USER
                                     PGM-WRK
                                     MAIL-EMPF
                                     MAIL-KOPIE
                                     MAIL-BKOPIE
                                     MAIL-TOPIC
                                     MAIL-DATA
                                     MAIL-FILE
                                     MAIL-RETTXT
           END-CALL.

       WRT-MAI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EMPFÄNGER FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-RECEIVER SECTION.
       FIL-RCV-00.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           IF       MAIL-ANZ < 10
                    ADD  1        TO MAIL-ANZ
                    MOVE MAIL-ANZ TO MAIL-ANZAHL OF MAIL-EMPF
                    MOVE FAMAKU OF PFODAW-L TO
                         MAIL-ADRESSE OF MAIL-EMPF-REC(MAIL-ANZ)
           ELSE
     ‚*             NUR 8 GEHEN HIER REIN DA GGF. NOCH 2 DAZUKOMMEN
                    IF   MAIL-C-ANZ < 8
                         ADD  1          TO MAIL-C-ANZ
                         MOVE MAIL-C-ANZ TO MAIL-ANZAHL OF MAIL-KOPIE
                         MOVE FAMAKU OF PFODAW-L TO
                              MAIL-ADRESSE OF
                              MAIL-KOPIE-REC(MAIL-C-ANZ)
                    END-IF
           END-IF.

       FIL-RCV-90.
           EXIT.
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
     ‚* PRÜFEN OB ES DIE DATEIEN AUCH GIBT
     ‚*--------------------------------------------------------------
       CHECK-OBJECTS SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "PFODAW"     TO OBJ     OF CHKOBJ-REC.
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
     ‚* COPY-CONVERT-DATE: DATUMSFELDER AUFBEREITEN/UMSETZEN
     ‚*---------------------------------------------------------------

           COPY     CNVDATE    OF APG-QCPYSRC.
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
