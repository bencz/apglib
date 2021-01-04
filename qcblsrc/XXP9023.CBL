       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9023.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  06.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
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
     ‚*
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9023".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  LEN                           PIC 9(4).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9023"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* EMAIL ZUR VERSENDUNG AUFBEREITEN
           PERFORM  BAU-MAIL.

     ‚* EMAIL SENDEN
           PERFORM  SEND-EMAIL.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL AN ÜBERTRAGUNG UND ANLEGER ZUSAMMENSTELLEN
     ‚*--------------------------------------------------------------
       BAU-MAIL SECTION.
       BAU-MAI-00.

           MOVE     1 TO LEN.
           INITIALIZE MAIL-TAB.

           MOVE     "SYSEH03"        TO MAIL-HOST   OF MAIL-TAB.
           MOVE     "25"             TO MAIL-PORT   OF MAIL-TAB.
           MOVE     "SYSEH02@EHRHARDT-PARTNER.COM"
                                     TO MAIL-SENDER OF MAIL-TAB.

     ‚* BETREFF ZUSAMMENBAUEN
           INITIALIZE MAIL-TOPIC   OF MAIL-TAB.
           MOVE     PGM-WRK TO MAIL-TOPIC OF MAIL-TAB.

     ‚* EMAIL BODY ALLGEMEINE INFOS - P3
           MOVE     "TEST"    TO ERRFLD-WRK.
           STRING   FUNCTION TRIM(ERRFLD-WRK) DELIMITED BY SIZE
                    X"25"                     DELIMITED BY SIZE
                    INTO MAIL-DATA
                    WITH POINTER LEN
           END-STRING.

       BAU-MAI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSENDE ZUSAMMENGESTELLTE EMAIL AN ANLEGER
     ‚*--------------------------------------------------------------
       SEND-EMAIL SECTION.
       SND-MAI-00.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           MOVE     1                TO MAIL-ANZAHL OF MAIL-EMPF.
           STRING   "APG"                   DELIMITED BY SPACES
                    "@ehrhardt-partner.com" DELIMITED BY SIZE
           INTO     MAIL-ADRESSE OF MAIL-EMPF-REC(1).

     ‚* KOPIE AN
     ‚*€   IF       CPY-EMPFAENGER1 NOT = SPACES
     ‚*€            ADD  1 TO MAIL-ANZAHL OF MAIL-KOPIE
     ‚*€            STRING "APG"                   DELIMITED BY SPACES
     ‚*€                   "@ehrhardt-partner.com" DELIMITED BY SIZE
     ‚*€            INTO   MAIL-ADRESSE OF MAIL-KOPIE(1)
     ‚*€   END-IF.

     ‚* PRÜFEN OB ERSTES FELD GEFÜLLT WURDE. WENN NICHT, MUSS
     ‚* DIE STRUKTUR ANDERS GEFÜLLT WERDEN
     ‚*€   IF       CPY-EMPFAENGER2 NOT = SPACES
     ‚*€    AND     CPY-EMPFAENGER1 NOT = SPACES
     ‚*€            ADD  1 TO MAIL-ANZAHL OF MAIL-KOPIE
     ‚*€            STRING CPY-EMPFAENGER2        DELIMITED BY SPACES
     ‚*€                   "@ehrhardt-partner.com" DELIMITED BY SIZE
     ‚*€            INTO   MAIL-ADRESSE OF MAIL-KOPIE(2)
     ‚*€   END-IF.
     ‚*€   IF       CPY-EMPFAENGER2 NOT = SPACES
     ‚*€    AND     CPY-EMPFAENGER1     = SPACES
     ‚*€            ADD  1 TO MAIL-ANZAHL OF MAIL-KOPIE
     ‚*€            STRING CPY-EMPFAENGER2         DELIMITED BY SPACES
     ‚*€                   "@ehrhardt-partner.com" DELIMITED BY SIZE
     ‚*€            INTO   MAIL-ADRESSE OF MAIL-KOPIE(1)
     ‚*€   END-IF.

     ‚* MAILPROGRAMM AUFRUFEN
           CALL     "HLP0006"  USING MAIL-HOST
                                     MAIL-PORT
                                     MAIL-SENDER
                                     MAIL-EMPF
                                     MAIL-KOPIE
                                     MAIL-BKOPIE
                                     MAIL-TOPIC
                                     MAIL-DATA
                                     MAIL-FILE
                                     MAIL-RETTXT
           END-CALL.

       SND-MAI-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
