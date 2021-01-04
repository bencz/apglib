       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0205.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SENDEN DER DIENSTMAIL                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SENDEN DER DIENSTMAIL                        *
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
     ‚* PHYSISCHE DATEI DIENSTPLAN
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DP
                  ASSIGN       TO  DATABASE-PKDPLAN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- DIENSTPLAN
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0205".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  USER-WRK                      LIKE T-USER.
       01  CFFK03-SAV                    LIKE CFFK03 OF CFG-CPY.

       01  MESSAGE-WRK                   PIC X(256).
       01  FIRST-MAIL                    PIC 9(1).

     ‚* PARAMETER FÜR DAS HLP0006
       01  HOST-WRK                      PIC X(15).
       01  PORT-WRK                      PIC X(5).
       01  SENDER-WRK                    PIC X(30).
       01  EMPFAENGER-WRK                PIC X(30).
       01  BETREFF-WRK                   PIC X(50).
       01  EMAIL-TEXT                    PIC X(500).
       01  EMAIL-FILE                    PIC X(256).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0205" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PKDPLAN-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* ERMITTELN DES HEUTIGEN DIENSTPLANS
           PERFORM  LESE-KDPLAN.

       ENDE.
     ‚* DATEIEN SCHLIEßEN
           CLOSE    PKDPLAN-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER OFFENEN FILME
     ‚*--------------------------------------------------------------
       LESE-KDPLAN SECTION.
       LES-KDP-00.

           INITIALIZE FIRST-MAIL.
           INITIALIZE CFFK03-SAV, USER-WRK.
     ‚* DATEN LESEN
           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     DATE-8              TO KDDATE OF PKDPLAN-P.
           MOVE     SPACES              TO KDTYP  OF PKDPLAN-P.
           MOVE     SPACES              TO KDUSER OF PKDPLAN-P.
           START    PKDPLAN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-KDP-90
           END-IF.

       LES-KDP-20.
           READ     PKDPLAN-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-KDP-90
           END-IF.

     ‚* NUR FÜR HEUTE SENDEN
           IF       KDDATE OF PKDPLAN-P NOT = DATE-8
                    GO TO LES-KDP-90
           END-IF.

     ‚* SOLL HIERFÜR AUCH EINE MAIL GESENDET WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K110"               TO CFID   OF CFG-CPY.
           MOVE     KDTYP  OF PKDPLAN-DP TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    GO TO LES-KDP-20
           END-IF.

     ‚* USER UND EINSTELLUNG ÜBERNEHMEN
           MOVE     CFFK03 OF CFG-CPY        TO CFFK03-SAV.
           IF       KDGUSR OF PKDPLAN-P = SPACES
                    MOVE KDUSER OF PKDPLAN-P TO USER-WRK
           ELSE
                    MOVE KDGUSR OF PKDPLAN-P TO USER-WRK
           END-IF.

     ‚* WILL DENN AUCH DER USER EINE MELDUNG?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "K110"               TO CFID   OF CFG-CPY.
           MOVE     KDTYP  OF PKDPLAN-DP TO CFKEY  OF CFG-CPY.
           MOVE     USER-WRK             TO CFKEY2 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE CFFK03-SAV TO CFFK03 OF CFG-CPY
           END-IF.

     ‚* EINSTELLUNG PRÜFEN
           IF       CFFK03 OF CFG-CPY = ZEROES
                    GO TO LES-KDP-20
           END-IF.

     ‚* JETZT PRÜFEN OB DER USER AUCH ANGEMELDET IST
           PERFORM  CHECK-USER-ANGEMELDET.
           IF       RET-CODE = SPACES
                    GO TO LES-KDP-20
           END-IF.

     ‚* JETZT DIE MAIL ZUSAMMENBAUEN UND SENDEN...
           IF       CFFK03 OF CFG-CPY = 1
            OR      CFFK03 OF CFG-CPY = 9
                    PERFORM  SEND-MAIL
           END-IF.

     ‚* ...ODER EINE BREAK-MESSAGE ABSCHICKEN?
           IF       CFFK03 OF CFG-CPY = 2
            OR      CFFK03 OF CFG-CPY = 9
                    PERFORM  SEND-BREAKMESSAGE
           END-IF.

           GO TO    LES-KDP-20.

       LES-KDP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER MAIL
     ‚*--------------------------------------------------------------
       SEND-MAIL SECTION.
       SND-MAL-00.

     ‚* WERTE INITIALISIEREN
           INITIALIZE EMAIL-FILE.

     ‚* BEIM ERSTEN MAL DIE CONFIG LESEN
           IF       FIRST-MAIL NOT = ZEROES
                    PERFORM GET-EMAIL-CONFIG
           END-IF.

           INITIALIZE EMAIL-TEXT, BETREFF-WRK.
     ‚* NACHRICHT JETZT ZUSAMMENBAUEN
           MOVE     KDTYP  OF PKDPLAN-P TO EMAIL-TEXT(1:10).
           MOVE     ":"                 TO EMAIL-TEXT(11:1).
     ‚* TEST LESEN
           MOVE     "PGM0001"           TO CPY-TXID.
           MOVE     SPRACHE-WRK         TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT          TO EMAIL-TEXT(13:).
           MOVE     CPY-TXTEXT          TO BETREFF-WRK.

           INITIALIZE EMPFAENGER-WRK.
     ‚* USERNAME ZUSAMMENBAUEN
           STRING   USER-WRK            DELIMITED BY SPACES
                    "@EHRHARDT-PARTNER" DELIMITED BY SPACES
                    ".COM"              DELIMITED BY SPACES
           INTO     EMPFAENGER-WRK.

     ‚* JETZT EMAIL-PROGRAMM AURFUFEN
           CALL     "HLP0006" USING HOST-WRK
                                    PORT-WRK
                                    SENDER-WRK
                                    EMPFAENGER-WRK
                                    BETREFF-WRK
                                    EMAIL-TEXT
                                    EMAIL-FILE
           END-CALL.

       SND-MAL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER BREAKMESSAGE
     ‚*--------------------------------------------------------------
       SEND-BREAKMESSAGE SECTION.
       SND-BRK-00.

           INITIALIZE MESSAGE-WRK.
     ‚* NACHRICHT JETZT ZUSAMMENBAUEN
           MOVE     KDTYP  OF PKDPLAN-P TO MESSAGE-WRK(1:10).
           MOVE     ":"                 TO MESSAGE-WRK(11:1).
     ‚* TEST LESEN
           MOVE     "PGM0001"           TO CPY-TXID.
           MOVE     SPRACHE-WRK         TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT          TO MESSAGE-WRK(13:).

     ‚* API AUFRUFEN UND NACHRICHT SENDEN
           CALL     "XXP2003" USING USER-WRK
                                    MESSAGE-WRK
           END-CALL.

       SND-BRK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN DER EMAIL-KONFIGURATION
     ‚*--------------------------------------------------------------
       GET-EMAIL-CONFIG SECTION.
       GET-EMC-00.

     ‚* INITIALISIEREN ALLER VARIABLEN
           INITIALIZE HOST-WRK, PORT-WRK, SENDER-WRK.

     ‚* LESEN DER EMAIL-EINSTELLUNGEN FÜR DIENST-MAILS
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "E100"         TO CFID   OF CFG-CPY.
           MOVE     "DIENST"       TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE "SYSEH03"    TO HOST-WRK
                    MOVE 25           TO PORT-WRK
                    MOVE PGM-WRK      TO SENDER-WRK
                    GO TO GET-EMC-90
           END-IF.

     ‚* ÜBERNEHMEN DER EINSTELLUNGEN
           MOVE     CFDAT1 OF CFG-CPY TO HOST-WRK.
           MOVE     CFNM02 OF CFG-CPY TO PORT-WRK.
           MOVE     CFTXT2 OF CFG-CPY TO SENDER-WRK.

       GET-EMC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DER USER AUCH ANGEMELDET IST
     ‚*--------------------------------------------------------------
       CHECK-USER-ANGEMELDET SECTION.
       CHK-USR-00.

           INITIALIZE RET-CODE.
     ‚* DAS CFP8003 PRÜFT OB DER USER ANGEMELDET IST
           CALL     "CFP8003" USING USER-WRK
                                    RET-CODE
           END-CALL.

       CHK-USR-90.
           EXIT.
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
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
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
