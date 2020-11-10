       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0215.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERINNERUNGSMAIL FÜR KÜCHENDIENST                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  10.11.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  KÜCHENDIENSTMAIL                             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI KÜCHENPLAN
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DP
                  ASSIGN       TO  DATABASE-PKDPLAN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KÜCHENPLAN
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT      OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY   OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0215".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  FILE-OPEN                     PIC 9(1).
       01  KDTYP-WRK                     LIKE KDTYP  OF PKDPLAN-P.
       01  USER-WRK                      LIKE T-USER.

       01  HEADER-TEXT                   LIKE CPY-TXTEXT.
       01  AKTION1-TEXT                  LIKE CPY-TXTEXT.
       01  AKTION2-TEXT                  LIKE CPY-TXTEXT.
       01  AKTION3-TEXT                  LIKE CPY-TXTEXT.
       01  AKTION4-TEXT                  LIKE CPY-TXTEXT.

     ‚* PARAMETER FÜR DAS HLP0022
       01  SENDER-KEY                    LIKE CFKEY  OF CFG-CPY.
       01  CHECK-ID                      LIKE CFID   OF CFG-CPY.
       01  CHECK-KEY                     LIKE CFKEY  OF CFG-CPY.
       01  MAIL-AKTION                   PIC X(20).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-AKTION                      PIC X(10).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-AKTION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE FILE-OPEN.
     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0215" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           OPEN     INPUT PKDPLAN-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE 1 TO FILE-OPEN
           ELSE
                    GO TO ENDE
           END-IF.

     ‚* ERMITTELN DATUM/ZEIT
           PERFORM  COPY-GET-TIME.

     ‚* KOPFEINTRAG MUSS VORHANDEN UND AKTIV SEIN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H051"            TO CFID   OF CFG-CPY.
           MOVE     "KD-MAIL-"        TO CFKEY  OF CFG-CPY(1:8).
           MOVE     X-AKTION          TO CFKEY  OF CFG-CPY(9:).
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
            AND     CFFK01 OF CFG-CPY = ZEROES
                    PERFORM  GET-TEXTE
                    PERFORM  WRITE-EMAIL
           END-IF.

       ENDE.
           IF       FILE-OPEN NOT = ZEROES
                    CLOSE PKDPLAN-DP
           END-IF.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-EMAIL SECTION.
       WRT-MAI-00.

           INITIALIZE MAIL-TAB.
     ‚* MAIL-TOPIC AUFRUFEN
           STRING   HEADER-TEXT        DELIMITED BY SPACES
               INTO MAIL-TOPIC OF MAIL-TAB
           END-STRING.

     ‚* EMAIL BODY ALLGEMEINE INFOS
           MOVE     1 TO MAIL-LEN.
           STRING   "Hallo,"                   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "bitte beim Küchendienst " DELIMITED BY SIZE
                    "auch auf folgende Sachen" DELIMITED BY SIZE
                    " vor eurem Feierabend"    DELIMITED BY SIZE
                    " achten:"                 DELIMITED BY SIZE
               INTO MAIL-DATA
               WITH POINTER MAIL-LEN
           END-STRING.

     ‚* 1. AKTION
           IF       AKTION1-TEXT NOT = SPACES
                    STRING   X"25"             DELIMITED BY SIZE
                             " - "             DELIMITED BY SIZE
                             AKTION1-TEXT      DELIMITED BY SIZE
                        INTO MAIL-DATA
                        WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.
     ‚* 2. AKTION
           IF       AKTION2-TEXT NOT = SPACES
                    STRING   X"25"             DELIMITED BY SIZE
                             " - "             DELIMITED BY SIZE
                             AKTION2-TEXT      DELIMITED BY SIZE
                        INTO MAIL-DATA
                        WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.
     ‚* 3. AKTION
           IF       AKTION3-TEXT NOT = SPACES
                    STRING   X"25"             DELIMITED BY SIZE
                             " - "             DELIMITED BY SIZE
                             AKTION3-TEXT      DELIMITED BY SIZE
                        INTO MAIL-DATA
                        WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.
     ‚* 4. AKTION
           IF       AKTION4-TEXT NOT = SPACES
                    STRING   X"25"             DELIMITED BY SIZE
                             " - "             DELIMITED BY SIZE
                             AKTION4-TEXT      DELIMITED BY SIZE
                        INTO MAIL-DATA
                        WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

           MOVE     ZEROES           TO MAIL-ANZAHL OF MAIL-EMPF.
     ‚* JE DIENST USER EINTRAGEN
           MOVE     "KD-EUP" TO KDTYP-WRK.
           PERFORM  GET-USER-DIENST.

           MOVE     "KD-IAW" TO KDTYP-WRK.
           PERFORM  GET-USER-DIENST.

     ‚* MAILPROGRAMM AUFRUFEN
           MOVE     "KD-DIENST"    TO SENDER-KEY.
           MOVE     "H051"         TO CHECK-ID.
           MOVE     "KD-MAIL-"     TO CHECK-KEY(1:8).
           MOVE     X-AKTION       TO CHECK-KEY(9:).
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
     ‚* USER ERMITTELN UND ALS EMPFÄNGER EINTRAGEN
     ‚*--------------------------------------------------------------
       GET-USER-DIENST SECTION.
       GET-USR-00.

           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     DATE-8              TO KDDATE OF PKDPLAN-P.
           MOVE     KDTYP-WRK           TO KDTYP  OF PKDPLAN-P.
           MOVE     SPACES              TO KDUSER OF PKDPLAN-P.
           START    PKDPLAN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-USR-90
           END-IF.

       GET-USR-20.
           READ     PKDPLAN-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-USR-90
           END-IF.

     ‚* KEY ABFRAGEN
           IF       KDDATE OF PKDPLAN-P NOT = DATE-8
            OR      KDTYP  OF PKDPLAN-P NOT = KDTYP-WRK
                    GO TO GET-USR-90
           END-IF.

           IF       KDGUSR OF PKDPLAN-P = SPACES
                    MOVE KDUSER OF PKDPLAN-P TO USER-WRK
           ELSE
                    MOVE KDGUSR OF PKDPLAN-P TO USER-WRK
           END-IF.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           ADD      1                TO MAIL-ANZAHL OF MAIL-EMPF.
           MOVE     USER-WRK         TO MAIL-ADRESSE OF
                    MAIL-EMPF-REC(MAIL-ANZAHL OF MAIL-EMPF).

           GO TO    GET-USR-20.

       GET-USR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXTE IN PROGRAMMTEXTEN ERMITTELN
     ‚*--------------------------------------------------------------
       GET-TEXTE SECTION.
       GET-TXT-00.

           INITIALIZE HEADER-TEXT.
           INITIALIZE AKTION1-TEXT.
           INITIALIZE AKTION2-TEXT.
           INITIALIZE AKTION3-TEXT.
           INITIALIZE AKTION4-TEXT.
     ‚* PASSENDEN PROGRAMMTEXT FÜR DEN DIENST LESEN
           EVALUATE X-AKTION
                    WHEN "WINDOW" MOVE "PGM0035" TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK   TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT    TO HEADER-TEXT.

     ‚* 1. AKTION
           EVALUATE X-AKTION
                    WHEN "WINDOW" MOVE "PGM0036" TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK   TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT    TO AKTION1-TEXT.

     ‚* 2. AKTION
           EVALUATE X-AKTION
                    WHEN "WINDOW" MOVE "PGM0037" TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK   TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT    TO AKTION2-TEXT.

     ‚* 3. AKTION
           EVALUATE X-AKTION
                    WHEN "WINDOW" MOVE "PGM0038" TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK   TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT    TO AKTION3-TEXT.

     ‚* 4. AKTION
           EVALUATE X-AKTION
                    WHEN "WINDOW" MOVE "PGM0039" TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK   TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT    TO AKTION4-TEXT.

       GET-TXT-90.
           EXIT.
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
