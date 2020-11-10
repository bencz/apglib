       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0023.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFBEREITEN TELEFONNUMMER UND VERGLEICH                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TELEFONNUMMER AUFBEREITEN                    *
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
     ‚* PHYSISCHE DATEI MITARBEITER
     ‚*-------------------------------------------------------------
           SELECT PBIRTH-DP
                  ASSIGN       TO  DATABASE-PBIRTH
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- MITARBEITER
       FD  PBIRTH-DP
           LABEL RECORDS ARE STANDARD.
       01  PBIRTH-P.
           COPY DDS-ALL-FORMATS OF MAT-PBIRTH.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0023".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  TELEPHONE-WRK                 LIKE BIHTEL OF PBIRTH-P.
       01  TELEPHONE-NUM                 LIKE BIHTEL OF PBIRTH-P.

       01  PLUS-WRK                      PIC 9(1).
       01  POS                           PIC 9(4).
       01  LEN                           PIC 9(4).

       01  SNDPGMMSG-REC                 TYPE SNDPGMMSG-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TELEPHONE                   PIC X(20).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TELEPHONE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0023" TO PGM-WRK.
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
                    GO TO ENDE
           END-IF.

     ‚* ÖFFNEN DER DATEIEN
           OPEN     INPUT PBIRTH-DP.

     ‚* LESEN OB ES DIE TELEFONNUMMER GIBT
           PERFORM  CHECK-TELEPHONE.

           CLOSE    PBIRTH-DP.

     ‚* MAT-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-MAT-LIB.

       ENDE.

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
       CHECK-TELEPHONE SECTION.
       CHK-TEL-00.

           INITIALIZE TELEPHONE-WRK.
     ‚* PBIRTH LESEN
           INITIALIZE PBIRTHF OF PBIRTH-P.
           MOVE     ZEROES              TO BILFDN OF PBIRTH-P.
           START    PBIRTH-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TEL-90
           END-IF.

       CHK-TEL-20.
           READ     PBIRTH-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TEL-90
           END-IF.

     ‚* TELEFON "ZU HAUSE" PRÜFEN
           MOVE     BIHTEL OF PBIRTH-P TO TELEPHONE-WRK.
           PERFORM  GET-NUMBER-NUMERIC.
           IF       TELEPHONE-NUM = X-TELEPHONE
                    PERFORM SEND-PGMMESSAGE
                    GO TO CHK-TEL-90
           END-IF.

     ‚* MOBILTELEFON PRÜFEN
           MOVE     BIMTEL OF PBIRTH-P TO TELEPHONE-WRK.
           PERFORM  GET-NUMBER-NUMERIC.
           IF       TELEPHONE-NUM = X-TELEPHONE
                    PERFORM SEND-PGMMESSAGE
                    GO TO CHK-TEL-90
           END-IF.

           GO TO    CHK-TEL-20.

       CHK-TEL-90.
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
     ‚*--------------------------------------------------------------
     ‚* NUMMER NUMERISCH AUFBEREITEN
     ‚*--------------------------------------------------------------
       GET-NUMBER-NUMERIC SECTION.
       GET-NBR-00.

           INITIALIZE TELEPHONE-NUM.
     ‚* ALLE ZEICHEN AUFBEREITEN
           MOVE     1      TO POS.
           MOVE     1      TO LEN.
           MOVE     ZEROES TO PLUS-WRK.
           PERFORM  WITH TEST BEFORE UNTIL POS =
                     LENGTH OF TELEPHONE-WRK
     ‚*             NUMERISCHE WERTE WERDEN ÜBERNOMMEN
                    IF   TELEPHONE-WRK(POS:1) IS NUMERIC
                         MOVE TELEPHONE-WRK(POS:1) TO
                              TELEPHONE-NUM(LEN:1)
                         ADD 1 TO LEN
                    END-IF
     ‚*             EIN + WIRD IN 00 GEÄNDERT
                    IF   TELEPHONE-WRK(POS:1) = "+"
                         MOVE "00" TO TELEPHONE-NUM(LEN:2)
                         MOVE 1    TO PLUS-WRK
                         ADD 2 TO LEN
                    END-IF
     ‚*             POSITION HOCHZÄHLEN
                    ADD 1 TO POS
           END-PERFORM.

     ‚* GIBT ES EIN + SO WURDE AUCH +49 EINGEGEBEN, ALSO STEHT DORT
     ‚* JETZT 0049. ABER WENN ES KEIN PLUS GIBT, WURDE EVTL. AUCH
     ‚* KEIN 49 EINGETRAGEN. DIES JETZT GLEICHZIEHEN
           IF       PLUS-WRK               = ZEROES
            AND     TELEPHONE-NUM(1:2) NOT = "00"
     ‚*             NUR DIE ERSTEN 3 WENN SCHON EINE FÜHRENDE NULL
     ‚*             EINGEGEBEN WURDE, DIESE WIRD DANN ÜBERSCHREIBEN
                    IF   TELEPHONE-NUM(1:1) = "0"
                         MOVE TELEPHONE-NUM TO TELEPHONE-NUM(4:)
                    ELSE
                         MOVE TELEPHONE-NUM TO TELEPHONE-NUM(5:)
                    END-IF
                    MOVE "0049"        TO TELEPHONE-NUM(1:4)
           END-IF.

       GET-NBR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER NACHRICHT
     ‚*--------------------------------------------------------------
       SEND-PGMMESSAGE SECTION.
       SND-MSG-00.

           INITIALIZE RET-CODE.
     ‚* NACHRICHT SENDEN
           INITIALIZE SNDPGMMSG-REC.
           STRING   TELEPHONE-NUM      DELIMITED BY SPACES
                    " -> "             DELIMITED BY SIZE
                    BIMAKU OF PBIRTH-P DELIMITED BY SPACES
                    INTO MSG    OF SNDPGMMSG-REC.
           MOVE     "*INFO"            TO MSGTYPE OF SNDPGMMSG-REC.
           CALL     "CLP0016" USING SNDPGMMSG-REC
                                    RET-CODE
           END-CALL.

       SND-MSG-90.
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
