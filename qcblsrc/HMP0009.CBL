       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP0009.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* EXPORT IN QIS-FORMAT                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.11.2013                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EXPORTIEREN ALLER TAGS (FÜR STANDARD)        *
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
     ‚* PPGMLIB: PLLIB, PTFILE
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PTAGLIBL2: PTLIB, PTFILE, PTTAG
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PTAGLIBL3: PTTAG
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL1
                  ASSIGN       TO  DATABASE-PTAGLIBL3
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

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
     ‚*--- PPGMLIB: PLLIB, PTFILE
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
      /
     ‚*--- PTAGLIBL2: PTLIB, PTFILE, PTTAG
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
      /
     ‚*--- PTAGLIBL3: PTTAG
       FD  PTAGLIB-DL1
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L1.
           COPY DDS-ALL-FORMATS OF PTAGLIBL3.
      /
     ‚*----DATEI QCBLSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "HMP0009".
       01  PGM-TYP                        PIC X(8)  VALUE "HERMES".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  OVRDBF-REC                TYPE OVRDBF-TYPE.
       01  DELOVR-REC                TYPE DELOVR-TYPE.
       01  CHKOBJ-REC                TYPE CHKOBJ-TYPE.
       01  CRTSRCPF-REC              TYPE CRTSRCPF-TYPE.

       01  MBR-LIB                        PIC X(10) VALUE "QTEMP".
       01  MBR-FILE                       PIC X(10) VALUE "TAGFILE".
       01  MBR-RCN                        PIC X(10) VALUE "A_TAGFILE".

     ‚* VORHANDENE TAGS FÜR SOURCE
       01  TAG-TAB.
        02 TMAX                           PIC 9(5) COMP-3.
        02 TAG-REC OCCURS 0 TO 9999 TIMES DEPENDING ON TMAX
                                          INDEXED   BY TIDX.
         04 TAG-REC2.
          07 TAGNAME                 LIKE PTTAG  OF PTAGLIB-L.

     ‚* LÄNGEN- UND POSITIONSVARIABLEN
       01  POS                            PIC 9(4).

     ‚* TAG-VARIABLEN
       01  TAG-COUNT                      PIC 9(4).
       01  TAG-LINE                  LIKE SRCDTA OF QSETSRC-P.

     ‚* VARIABLEN ZUM SCHREIBEN DER SOURCE
       01  SRCSEQ-WRK                LIKE SRCSEQ OF QSETSRC-P.

     ‚* SICHERUNGSVARIABLEN
       01  LIB-SAV                   LIKE PTLIB  OF PTAGLIB-L.
       01  FILE-SAV                  LIKE PTFILE OF PTAGLIB-L.
       01  RCN-SAV                        PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0009" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT PTAGLIB-DL
                          PTAGLIB-DL1
                          PPGMLIB-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* ERSTELLEN DER QTEMP/TAGFILE
           PERFORM  CREATE-SRC-FILE.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* NUN PTAGLIB AUSLESEN UND DIE TEILDATEI SCHREIBEN
           PERFORM  START-MBR-WRITE.

       ENDE.
           CLOSE                 PTAGLIB-DL
                                 PTAGLIB-DL1
                                 PPGMLIB-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DER QIS-TEILDATEI
     ‚*--------------------------------------------------------------
       WRITE-QIS-SOURCE SECTION.
       WRT-QIS-00.

           INITIALIZE LIB-SAV, FILE-SAV, RCN-SAV.
           PERFORM  CLEAR-ARRAY.
     ‚* PTAGLIB LESEN
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     SPACES              TO PTLIB  OF PTAGLIB-L.
           MOVE     SPACES              TO PTFILE OF PTAGLIB-L.
           MOVE     SPACES              TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WRT-QIS-90
           END-IF.

       WRT-QIS-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WRT-QIS-90
           END-IF.

     ‚* KEINE KOPFSÄTZE EXPORTIEREN
           IF       PTLIB  OF PTAGLIB-L = SPACES
            OR      PTTAG  OF PTAGLIB-L = SPACES
                    GO TO WRT-QIS-20
           END-IF.

     ‚* DEN TAG "LFS" NICHT EXPORTIEREN, ABER DAFÜR NUR STD
     ‚* PROGRAMME EXPORTIEREN
           IF       PTTAG  OF PTAGLIB-L          = SPACES
            OR      PTLIB  OF PTAGLIB-L(1:3) NOT = "LFS"
                    GO TO WRT-QIS-20
           END-IF.

     ‚* DARF DIESER TAG EXPORTIERT WERDEN?
           INITIALIZE PTAGLIBF OF PTAGLIB-L1.
           MOVE     PTTAG  OF PTAGLIB-L TO PTTAG  OF PTAGLIB-L1.
           READ     PTAGLIB-DL1.
           IF       FILE-STATUS = ZEROES
                    IF   PTFK02 OF PTAGLIB-L1 = ZEROES
                         GO TO WRT-QIS-20
                    END-IF
           END-IF.

     ‚* BEIM WECHSEL DES PROGRAMMS ALLES SCHREIBEN
           IF       PTLIB  OF PTAGLIB-L NOT = LIB-SAV
            OR      PTFILE OF PTAGLIB-L NOT = RCN-SAV
                    IF   LIB-SAV NOT = SPACES
                         PERFORM  WRITE-ALL-TAGS
                         PERFORM  CLEAR-ARRAY
                    END-IF
                    MOVE PTLIB  OF PTAGLIB-L TO LIB-SAV
                    MOVE PTFILE OF PTAGLIB-L TO RCN-SAV
                    PERFORM GET-QCBLSRC
           END-IF.

           PERFORM  ADD-TAG-TO-ARRAY.

           GO TO    WRT-QIS-20.

       WRT-QIS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NUN WIRKLICH DIE SOURCE SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-ALL-TAGS SECTION.
       WRT-ATG-00.

     ‚* LIB MAPPEN...
           IF       LIB-SAV(4:3) = "PGM"
            OR      LIB-SAV(4:3) = "DAT"
                    MOVE "SRC" TO LIB-SAV(4:3)
           END-IF.

     ‚* ERSTMAL DIE ZEILE MIT DER SOURCE SCHREIBEN
           INITIALIZE QSETSRC OF QSETSRC-P.
           MOVE     LIB-SAV    TO SRCDTA OF QSETSRC-P(10:10).
           MOVE     FILE-SAV   TO SRCDTA OF QSETSRC-P(21:10).
           MOVE     RCN-SAV    TO SRCDTA OF QSETSRC-P(32:10).
           PERFORM  SET-DEFAULT-VALUES.
           WRITE    QSETSRC-P.

     ‚* JETZT ALLE GESAMMELTEN TAGS IM ARRAY DURCHLESEN
           INITIALIZE TAG-COUNT, TAG-LINE.
           MOVE     1 TO POS.
           SET      TIDX TO 1.
           PERFORM  WITH TEST BEFORE UNTIL TIDX > TMAX OF TAG-TAB
                    IF   TAG-COUNT > 7
                         INITIALIZE QSETSRC OF QSETSRC-P
                         MOVE "::"       TO SRCDTA OF QSETSRC-P(10:2)
                         MOVE TAG-LINE   TO SRCDTA OF QSETSRC-P(13:)
                         PERFORM  SET-DEFAULT-VALUES
                         WRITE QSETSRC-P
                         MOVE ZEROES TO TAG-COUNT
                         MOVE SPACES TO TAG-LINE
                         MOVE 1      TO POS
                    END-IF
     ‚*             TAG NUN VERARBEITEN
                    MOVE TAGNAME OF TAG-TAB(TIDX) TO TAG-LINE(POS:)
                    COMPUTE POS =   POS
                                  + FUNCTION LENGTH(
                                     FUNCTION TRIMR(
                                      TAGNAME OF TAG-TAB(TIDX)))
                                  + 1
                    END-COMPUTE
     ‚*             READ NEXT TAG...
                    ADD  1    TO TAG-COUNT
                    SET  TIDX UP BY 1
           END-PERFORM.
           IF       TAG-COUNT > ZEROES
                    INITIALIZE QSETSRC OF QSETSRC-P
                    MOVE     "::"       TO SRCDTA OF QSETSRC-P(10:2)
                    MOVE     TAG-LINE   TO SRCDTA OF QSETSRC-P(13:)
                    PERFORM  SET-DEFAULT-VALUES
                    WRITE    QSETSRC-P
           END-IF.

       WRT-ATG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN DER SOURCE-DATEI (SRCPF)
     ‚*--------------------------------------------------------------
       CREATE-SRC-FILE SECTION.
       CRT-SRC-00.

           INITIALIZE SRCSEQ-WRK, RET-CODE.
     ‚* GIBT ES DAS OBJEKT SCHON?
           INITIALIZE CHKOBJ-REC.
           MOVE     MBR-LIB       TO LIB     OF CHKOBJ-REC.
           MOVE     MBR-FILE      TO OBJ     OF CHKOBJ-REC.
           MOVE     "*FILE"       TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    INITIALIZE CRTSRCPF-REC
                    MOVE MBR-LIB   TO LIBOBJ  OF CRTSRCPF-REC
                    MOVE MBR-FILE  TO OBJ     OF CRTSRCPF-REC
                    MOVE MBR-RCN   TO MBRNAME OF CRTSRCPF-REC
                    CALL "CLP0070" USING CRTSRCPF-REC
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO CRT-SRC-90
                    END-IF
           END-IF.

       CRT-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* OVERRIDE AUSFÜHREN UND TEILDATEI SCHREIBEN
     ‚*--------------------------------------------------------------
       START-MBR-WRITE SECTION.
       STR-MBR-00.

     ‚* OVERRIDE ZUM SCHREIBEN
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE = SPACES
     ‚*             ÖFFNEN DER TEILDATEI
                    OPEN OUTPUT QSETSRC-DP
                    IF   FILE-STATUS = ZEROES
                         PERFORM  WRITE-QIS-SOURCE
     ‚*                  SCHLIEßEN DER TEILDATEI
                         CLOSE    QSETSRC-DP
                    END-IF
     ‚*             ENTFERNEN DES OVERRIDE
                    PERFORM DLT-OVR-MBR
           END-IF.

       STR-MBR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ARRAY LÖSCHEN
     ‚*--------------------------------------------------------------
       CLEAR-ARRAY SECTION.
       CLR-ARY-00.

           INITIALIZE TMAX OF TAG-TAB.

       CLR-ARY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TAG ZUM ARRAY HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-TAG-TO-ARRAY SECTION.
       ADD-TTA-00.

           SET      TIDX TO 1.
     ‚* JEDEN TAG NUR EINMAL HINZUFÜGEN
           SEARCH   TAG-REC OF TAG-TAB
                    AT END
                         MOVE PTTAG  OF PTAGLIB-L TO
                              TAGNAME OF TAG-REC(TIDX)
                         SET TMAX TO TIDX
                    WHEN TAGNAME OF TAG-REC(TIDX) = PTTAG  OF PTAGLIB-L
                         CONTINUE
           END-SEARCH.

       ADD-TTA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ENTFERNEN OVERWRITE FÜR MEMBER
     ‚*---------------------------------------------------------------
       DLT-OVR-MBR SECTION.
       DEL-OVR-00.

     ‚* OVR ZURÜCKSETZEN
           INITIALIZE DELOVR-REC.
           MOVE     MBR-FILE TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

       DEL-OVR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DA DIE DATEI NICHT GESPEICHERT WIRD, HIER NUN "RATEN"
     ‚*--------------------------------------------------------------
       GET-QCBLSRC SECTION.
       GET-CBL-00.

           INITIALIZE FILE-SAV.
     ‚* ERMITTELN DER DATEN FÜR TEILDATEIART
           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     LIB-SAV    TO PLLIB  OF PPGMLIB-P.
           MOVE     RCN-SAV    TO PLFILE OF PPGMLIB-P.
           READ     PPGMLIB-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CBL-90
           END-IF.

     ‚* DATEI UMSETZEN
           EVALUATE PLSEU2 OF PPGMLIB-P
                    WHEN "CBLLE"
                    WHEN "CBL"
                         MOVE "QCBLSRC" TO FILE-SAV
                    WHEN "CLLE"
                    WHEN "CLP"
                         MOVE "QCLSRC"  TO FILE-SAV
                    WHEN "CLE"
                    WHEN "CPPLE"
                         MOVE "QCSRC"   TO FILE-SAV
                    WHEN "RPG"
                         MOVE "QRPGSRC" TO FILE-SAV
                    WHEN "DSPF"
                    WHEN "PRTF"
                    WHEN "PF"
                    WHEN "LF"
                         MOVE "QDDSSRC" TO FILE-SAV
           END-EVALUATE.

       GET-CBL-90.
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
           MOVE     MBR-LIB    TO TOLIB    OF OVRDBF-REC.
           MOVE     MBR-FILE   TO TOFILE   OF OVRDBF-REC.
           MOVE     MBR-RCN    TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEFAULT-WERTE SCHREIBEN
     ‚*--------------------------------------------------------------
       SET-DEFAULT-VALUES SECTION.
       SET-DEF-00.

           ADD      0,01            TO SRCSEQ-WRK.
           MOVE     SRCSEQ-WRK      TO SRCSEQ OF QSETSRC-P.
           MOVE     DATE-8(3:6)     TO SRCDAT OF QSETSRC-P.

       SET-DEF-90.
           EXIT.
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
