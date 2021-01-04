       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0032.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZÄHLEN DER POINTEREINTRÄGE                                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCE SPEICHERN                             *
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
     ‚*    DATEI QSETSRC (NUR ZUM AUSLESEN)
     ‚*-------------------------------------------------------------
           SELECT QSETSRC-DP
                  ASSIGN       TO  DATABASE-QSETSRC
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DATEI QCBLSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0032".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  OVRDBF-REC                    TYPE OVRDBF-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.
       01  DELOVR-REC                    TYPE DELOVR-TYPE.

     ‚* SONSTIGE VARIABLEN
       01  CHANGE-SRC-AKTIV              PIC 9(1).
       01  START-ERFOLGT                 PIC 9(1).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-CHANGE                      LIKE CHANGE-SRC-AKTIV.
       01  X-POINTER                     USAGE POINTER.
       01  X-RETCODE                     LIKE RET-CODE.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                                         X-CHANGE
                                         X-POINTER
                                         X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0032" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE X-RETCODE.
           MOVE     X-CHANGE TO CHANGE-SRC-AKTIV.
     ‚* PRÜFEN OB DIE TEILDATEI VORHANDEN IST UND OB SIE VOM RICHTIGEN
     ‚* TYP IST
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* NUR SICHERN, WENN WAS GEÄNDERT WURDE
           PERFORM  GET-SETTINGS.
           IF       CHANGE-SRC-AKTIV  NOT = ZEROES
            AND     CFFK15 OF CFG-CPY NOT = ZEROES
                    CALL "CRP0014" USING X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                    END-CALL
           END-IF.

     ‚* METADATEN HINZUFÜGEN
           IF       CHANGE-SRC-AKTIV  NOT = ZEROES
            AND     CFFK18 OF CFG-CPY NOT = ZEROES
                    CALL "CRP0030" USING X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                                         SRCTYPE OF RTVMBRD-REC
                                         X-POINTER
                    END-CALL
           END-IF.

     ‚* SETZEN OVRWRT FÜR QCBLSRC UND ÖFFNEN DER DATEI
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* SETZEN EINES LOCKS
     ‚*€   MOVE     X-SRCLIB TO LCK-LIB.
     ‚*€   MOVE     X-SRCRCN TO LCK-RCN.
     ‚*€   MOVE     X-SRCMBR TO LCK-MBR.
     ‚*€   PERFORM  SET-MBR-LOCK.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     OUTPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* SOURCE JETZT SPEICHERN (SCHREIBEN DER QSETSRC!)
           PERFORM  SAVE-SOURCE.

     ‚* SCHLIEßEN DER TEILDATEI
           CLOSE    QSETSRC-DP.

     ‚* LOCK UND OVERWRITE ENTFERNEN
     ‚*€   IF       LCK-SET NOT = ZEROES
     ‚*€            PERFORM DLT-MBR-LOCK
     ‚*€   END-IF.
           PERFORM  DLT-OVR-MBR.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE JETZT SPEICHERN
     ‚*--------------------------------------------------------------
       SAVE-SOURCE SECTION.
       SAV-SRC-00.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO START-ERFOLGT.

       SAV-SRC-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE   1                  TO START-ERFOLGT
                    MOVE   ZEROES             TO MAP-LFDN OF MAPDTA-REC
                    MOVE   MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO X-POINTER
                    PERFORM  COPY-CALL-MAPDTA
           ELSE
                    MOVE   MAP-READ-NEXT      TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO X-POINTER
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       RET-CODE NOT = SPACES
                    GO TO SAV-SRC-90
           END-IF.

     ‚* PASST DER KEY?
           IF       MAP-KEY OF MAPDTA-REC(1:10)  NOT = X-SRCLIB
            OR      MAP-KEY OF MAPDTA-REC(11:10) NOT = X-SRCRCN
            OR      MAP-KEY OF MAPDTA-REC(21:10) NOT = X-SRCMBR
            OR      MAP-KEY OF MAPDTA-REC(38:6)  NOT = X'FFFFFFFFFFFF'
            OR      MAP-KEY OF MAPDTA-REC(51:)   NOT = SPACES
            OR      MAP-KEY OF MAPDTA-REC(1:1)       = "*"
                    GO TO SAV-SRC-20
           END-IF.

     ‚* JETZT BRAUCHEN WIR NUR NOCH NEU SCHREIBEN
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-P.
           WRITE    QSETSRC-P.

           GO TO    SAV-SRC-20.

       SAV-SRC-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ENTFERNEN OVERWRITE FÜR MEMBER
     ‚*---------------------------------------------------------------
       DLT-OVR-MBR SECTION.
       DEL-OVR-00.

     ‚* OVR ZURÜCKSETZEN
           INITIALIZE DELOVR-REC.
           MOVE     X-SRCRCN TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

       DEL-OVR-90.
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
           MOVE     X-SRCLIB   TO TOLIB    OF OVRDBF-REC.
           MOVE     X-SRCRCN   TO TOFILE   OF OVRDBF-REC.
           MOVE     X-SRCMBR   TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINSTELLUNGEN AUSLESEN
     ‚*--------------------------------------------------------------
       GET-SETTINGS SECTION.
       GET-CFG-00.

     ‚* KONFIGURATION ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

       GET-CFG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-SRCLIB     TO LIB     OF CHKOBJ-REC.
           MOVE     X-SRCRCN     TO OBJ     OF CHKOBJ-REC.
           MOVE     X-SRCMBR     TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

           INITIALIZE RTVMBRD-REC.
           MOVE     X-SRCLIB          TO OBJLIB   OF RTVMBRD-REC.
           MOVE     X-SRCRCN          TO FILENAME OF RTVMBRD-REC.
           MOVE     X-SRCMBR          TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       CHK-TED-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
