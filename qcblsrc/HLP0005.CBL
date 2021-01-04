       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ALLE KOMMENTARE OHNE DATUMSÄNDERUNG WEIß FÄRBEN (CRTREM)      *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  KOMMENTARE WEIß FÄRBEN                       *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚* DATEI QSETSRC (NUR ZUM AUSLESEN)
     ‚*-------------------------------------------------------------
           SELECT QSETSRC-DP
                  ASSIGN       TO  DATABASE-QSETSRC
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DATEI QSETSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0005 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  SUCH-STRING                   PIC X(2).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-LIB                         PIC X(10).
       01  X-FILE                        PIC X(10).
       01  X-MBR                         PIC X(10).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-LIB
                                X-FILE
                                X-MBR.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0005" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* SETZEN OVRWRT FÜR QSETSRC UND ÖFFNEN DER DATEI
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     I-O   QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* SUCHSTRING DEFINIEREN
           MOVE     X'40' TO SUCH-STRING.
           MOVE     "*"   TO SUCH-STRING(2:1).

           PERFORM  SRC-SATZ-LESEN.
           IF       RET-CODE > SPACES
                    GO TO ENDE
           END-IF.

     ‚* DATEI NUN WIEDER SCHLIEßEN
           CLOSE    QSETSRC-DP.

           PERFORM  DLT-OVR-MBR.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* NÄCHSTEN SATZ AUS SOURCE-CODE LESEN
     ‚*---------------------------------------------------------------
       SRC-SATZ-LESEN SECTION.
       SRC-LES-00.

       SRC-LES-20.
           READ      QSETSRC-DP NEXT RECORD.
           IF        FILE-STATUS NOT = ZEROES
            AND      FILE-STATUS NOT = "04"
                     GO TO SRC-LES-90
           END-IF.

     ‚* WENN DIE BEMERKUNG IN GRÜNER SCHRIFT VORLIEGT, DANN ATTRIBUT
     ‚* SETZEN.
           IF       SRCDTA OF QSETSRC-P(6:2) = SUCH-STRING
                    MOVE X'22' TO SRCDTA OF QSETSRC-P(6:1)
           END-IF.

           REWRITE  QSETSRC-P.
           GO TO    SRC-LES-20.

       SRC-LES-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SETZEN OVRWRT FÜR TEILDATEI
     ‚*---------------------------------------------------------------
       SET-OVR-MBR SECTION.
       SET-OVR-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN DER TEILDATEI
           INITIALIZE OVRDBF-REC.
           MOVE     "QSETSRC"  TO FROMFILE OF OVRDBF-REC.
           MOVE     X-LIB      TO TOLIB    OF OVRDBF-REC.
           MOVE     X-FILE     TO TOFILE   OF OVRDBF-REC.
           MOVE     X-MBR      TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ENTFERNEN OVERWRITE FÜR MEMBER
     ‚*---------------------------------------------------------------
       DLT-OVR-MBR SECTION.
       DEL-OVR-00.

     ‚* OVR ZURÜCKSETZEN
           INITIALIZE DELOVR-REC.
           MOVE     X-FILE TO DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

       DEL-OVR-90.
           EXIT.
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
