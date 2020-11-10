       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP1000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSFÜHREN MEHRERER ANMELDETÄTIGKEITEN                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  03.06.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN ANMELDETÄTIGKEITEN                 *
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

      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP1000".
       01  PGM-TYP                       PIC X(8)  VALUE "ANMELDEN".

       01  DUMMY-DATE                    LIKE DATE-8.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚* "1" = MANUELLER AUFRUF
     ‚* "2" = AUFRUF AUS WRKJOBSCDE
       01  X-MODE                        PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-MODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP1000" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* SOLL AUTOMATISCH EINE KINO-ANMELDUNG ERFOLGEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "KINO-ANM"    TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM KINO-ANMELDUNG
                    END-IF
           END-IF.

     ‚* SOLL AUTOMATISCH EINE MAIL FÜR KÜCHENDIENST GESENDET WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "MAIL-DIENST" TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
            AND     CFFK01 OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM SENDEN-DIENSTMAIL
                    END-IF
           END-IF.

     ‚* SOLL DIE P3AZUBI ERSTELLT WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "P3AZUBI"     TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM CREATE-P3AZUBI
                    END-IF
           END-IF.

     ‚* SOLL DIE PBIRTH IMPORTIERT WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "PBIRTH"      TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM IMPORT-PBIRTH
                    END-IF
           END-IF.

     ‚* SOLL DIE PRAUM IMPORTIERT WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "PRAUM"       TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM IMPORT-PRAUM
                    END-IF
           END-IF.

     ‚* SOLL DIE PPFOODPUB IMPORTIERT WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "PFOODPUB"    TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM IMPORT-PFOODPUB
                    END-IF
           END-IF.

     ‚* SOLL DIE GEBURTSTAGSMAIL GESCHRIEBEN WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A100"        TO CFID   OF CFG-CPY.
           MOVE     "GEBURTSTAG"  TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
     ‚*             AUSFÜHREN?
                    IF   (    CFFK01 OF CFG-CPY = 1
                          AND X-MODE            = "1")
                     OR  (    CFFK01 OF CFG-CPY = 2
                          AND X-MODE            = "2")
                     OR  CFFK01 OF CFG-CPY      = 9
                         PERFORM SEND-BIRTHDAY
                    END-IF
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUF DES PROGRAMMS UM IN AMU/KINO REINZUGEHEN
     ‚*--------------------------------------------------------------
       KINO-ANMELDUNG SECTION.
       KIN-ANM-00.

           CALL     "XXP1001"
           END-CALL.

       KIN-ANM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER MAIL FÜR LAGERDIENST
     ‚*--------------------------------------------------------------
       SENDEN-DIENSTMAIL SECTION.
       SND-DML-00.

           CALL     "XXP0205"
           END-CALL.

       SND-DML-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOLL DIE DATEI NEU ERSTELLT WERDEN?
     ‚*--------------------------------------------------------------
       CREATE-P3AZUBI SECTION.
       CRT-P3A-00.

           CALL     "CLP0061"
           END-CALL.

       CRT-P3A-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PBIRTH IMPORTIEREN
     ‚*--------------------------------------------------------------
       IMPORT-PBIRTH SECTION.
       IMP-PBI-00.

           CALL     "HLP0015"
           END-CALL.

       IMP-PBI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRAUM IMPORTIEREN
     ‚*--------------------------------------------------------------
       IMPORT-PRAUM SECTION.
       IMP-PRA-00.

           CALL     "HLP0024"
           END-CALL.

       IMP-PRA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PPFOODPUB IMPORTIEREN
     ‚*--------------------------------------------------------------
       IMPORT-PFOODPUB SECTION.
       IMP-PFP-00.

           CALL     "HLP0025"
           END-CALL.

       IMP-PFP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GEBURTSTAGSMAIL SCHREIBEN
     ‚*--------------------------------------------------------------
       SEND-BIRTHDAY SECTION.
       IMP-PBI-00.

           INITIALIZE DUMMY-DATE.
           CALL     "XXP0048" USING DUMMY-DATE
           END-CALL.

       IMP-PBI-90.
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
