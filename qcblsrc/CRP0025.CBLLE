       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0025.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE EINLESEN UND MAPDTA ZURÜCKGEBEN                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.05.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCE EINLESEN                              *
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

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0025".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* VARIABLEN FÜR DIE SUCHROUTINE
       01  SUCH-TAB.
        05  SUCH-REC OCCURS 10.
         06  SUCH                        LIKE SRCDTA OF QSETSRC-P.
         06  S-POS                       PIC 9(3) COMP.
         06  E-POS                       PIC 9(3) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

       01  STR-TAB.
        05  STR-ANZ                      PIC 9(3) COMP.
        05  STR-ANZ2                     PIC 9(3) COMP.
        05  STR-REC OCCURS 100.
         06  STR                         LIKE SRCDTA OF QSETSRC-P.
         06  S-LEN                       PIC 9(3) COMP.
         06  E-LEN                       PIC 9(3) COMP.

       01  SOURCE-TMP                    LIKE SRCDTA OF QSETSRC-P.
       01  SOURCE-WRK                    LIKE SRCDTA OF QSETSRC-P.
       01  SOURCE-FND                    LIKE SRCDTA OF QSETSRC-P.
       01  GEFUNDEN                      PIC 9(01) COMP.
       01  POS                           PIC 9(3) COMP.
       01  POS2                          PIC 9(3) COMP.
       01  LEN                           PIC 9(3) COMP.
       01  LEN2                          PIC 9(3) COMP.
       01  STR-POS                       PIC 9(3) COMP.
       01  STR-ANF                       PIC 9(3) COMP.
       01  STR-LEN                       PIC 9(3) COMP.
       01  STR-WRK                       LIKE SRCDTA OF QSETSRC-P.

     ‚* LAUFVARIABLEN
       01  IND                           PIC 9(4).

     ‚* VARIABLEN FÜR TEILDATEIPRÜFUNG
       01  CHECK-LIB                     PIC X(10).
       01  CHECK-RCN                     PIC X(10).
       01  CHECK-MBR                     PIC X(10).

     ‚* SONSTIGE VARIABLEN
       01  SRCSEQ-WRK                    PIC X(7).

       01  SRCLIB-WRK                    PIC X(10).
       01  SRCRCN-WRK                    PIC X(10).
       01  SRCMBR-WRK                    PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-COPY-SRCLIB                 PIC X(10).
       01  X-COPY-SRCRCN                 PIC X(10).
       01  X-COPY-SRCMBR                 PIC X(10).
       01  X-COPY-SRCSEQ                 LIKE SRCSEQ OF QSETSRC-P.
       01  X-RET-ANZAHL                  PIC 9(9).
       01  X-RET-FIRST-SEQ               LIKE SRCSEQ OF QSETSRC-P.
       01  X-RET-LAST-SEQ                LIKE SRCSEQ OF QSETSRC-P.
       01  X-SOURCE-PTR                  USAGE POINTER.
       01  X-SECTION-PTR                 USAGE POINTER.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-COPY-SRCLIB
                                X-COPY-SRCRCN
                                X-COPY-SRCMBR
                                X-COPY-SRCSEQ
                                X-RET-ANZAHL
                                X-RET-FIRST-SEQ
                                X-RET-LAST-SEQ
                                X-SOURCE-PTR
                                X-SECTION-PTR
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0025"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

           INITIALIZE X-RET-ANZAHL.
           INITIALIZE X-RET-FIRST-SEQ.
           INITIALIZE X-RET-LAST-SEQ.
     ‚* WENN NICHT ÜBERGEBEN, DANN GLEICHZIEHEN
           IF       X-COPY-SRCLIB = SPACES
            OR      X-COPY-SRCRCN = SPACES
            OR      X-COPY-SRCMBR = SPACES
                    MOVE     X-SRCLIB      TO X-COPY-SRCLIB
                    MOVE     X-SRCRCN      TO X-COPY-SRCRCN
                    MOVE     X-SRCMBR      TO X-COPY-SRCMBR
           END-IF.

     ‚* WENN DIE COPY-SOURCE ÜBERGEBEN WURDE, DANN SOLL DIESE DEM
     ‚* POINTER AN DER PASSENDEN STELLE ANGEHÄNGT WERDEN
           IF       X-COPY-SRCLIB = X-SRCLIB
            AND     X-COPY-SRCRCN = X-SRCRCN
            AND     X-COPY-SRCMBR = X-SRCMBR
                    MOVE     X-SRCLIB      TO SRCLIB-WRK
                    MOVE     X-SRCRCN      TO SRCRCN-WRK
                    MOVE     X-SRCMBR      TO SRCMBR-WRK
           ELSE
                    MOVE     X-COPY-SRCLIB TO SRCLIB-WRK
                    MOVE     X-COPY-SRCRCN TO SRCRCN-WRK
                    MOVE     X-COPY-SRCMBR TO SRCMBR-WRK
           END-IF.

           PERFORM  INIT-VAR.

     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN IN MAPDTA
           PERFORM  LESE-DATEN.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO X-RETCODE
                    GO TO ENDE
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER SOURCZEILEN
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE RET-CODE.
           INITIALIZE STR-ANZ, STR-ANZ2.
     ‚* PRÜFEN OB DIE TEILDATEI VORHANDEN IST UND OB SIE VOM RICHTIGEN
     ‚* TYP IST
           MOVE     SRCLIB-WRK TO CHECK-LIB.
           MOVE     SRCRCN-WRK TO CHECK-RCN.
           MOVE     SRCMBR-WRK TO CHECK-MBR.
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO LES-DAT-90
           END-IF.

     ‚* SETZEN OVRWRT FÜR QCBLSRC UND ÖFFNEN DER DATEI
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO RET-CODE
                    GO TO LES-DAT-90
           END-IF.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     INPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           INITIALIZE QSETSRC-P.
           SET      MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR.

       LES-DAT-20.
           READ     QSETSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
            AND     FILE-STATUS NOT = "04"
                    GO TO LES-DAT-80
           END-IF.

     ‚* ETWAIGE META-DATEN ÜBERLESEN
           IF       (    SRCDTA OF QSETSRC-P(7:3) = "*M*"
                     AND (    SRCTYPE OF RTVMBRD-REC = "CBL"
                          OR  SRCTYPE OF RTVMBRD-REC = "CBLLE"
                          OR  SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
                          OR  SRCTYPE OF RTVMBRD-REC = "DSPF"
                          OR  SRCTYPE OF RTVMBRD-REC = "PRTF"
                          OR  SRCTYPE OF RTVMBRD-REC = "PF"
                          OR  SRCTYPE OF RTVMBRD-REC = "LF"
                         )
                    )
                    GO TO LES-DAT-20
           END-IF.

     ‚* SOURCE SICHERN
           MOVE     SRCDTA OF QSETSRC-P TO SOURCE-WRK.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ OF QSETSRC-P TO MAP-KEY OF MAPDTA-REC(31:6).
     ‚* DAMIT DIESE ZEILEN IN DER SORTIERUNG GANZ HINTEN ERSCHEINEN
           MOVE     X'FFFFFFFFFFFF'     TO MAP-KEY OF MAPDTA-REC(38:6).
     ‚* CPY-MODUL?
           IF       SRCLIB-WRK NOT = X-SRCLIB
            OR      SRCRCN-WRK NOT = X-SRCRCN
            OR      SRCMBR-WRK NOT = X-SRCMBR
     ‚*             FÜR DIE SORTIERUNG ZEILENNUMMER DER ORIGINALZEILE
     ‚*             WIEDER ÜBERSCHREIBEN
                    MOVE X-COPY-SRCSEQ TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE SRCLIB-WRK    TO MAP-KEY OF MAPDTA-REC(51:10)
                    MOVE SRCRCN-WRK    TO MAP-KEY OF MAPDTA-REC(61:10)
                    MOVE SRCMBR-WRK    TO MAP-KEY OF MAPDTA-REC(71:10)
                    MOVE SRCSEQ OF QSETSRC-P TO
                         MAP-KEY OF MAPDTA-REC(81:6)
                    MOVE X'FFFFFFFFFFFF' TO MAP-KEY OF MAPDTA-REC(88:6)
     ‚*             DAMIT DER SEU WEIß, DAS DIESE ZEILE NICHT
     ‚*             GEÄNDERT WERDEN DARF
                    MOVE     "N"         TO MAP-REC OF MAPDTA-REC(3:1)
           END-IF.

           MOVE     QSETSRC-P           TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE           TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-SOURCE-PTR        TO MAP-PTR OF MAPDTA-REC.
           ADD      1                   TO X-RET-ANZAHL.

     ‚* SUCHEN NACH BESTIMMTEN WÖRTERN UND AUSFÜHREN EINER SUCHE
           IF       SRCDTA OF QSETSRC-P(7:1) NOT = "*"
                    PERFORM LIST-SOURCE-ELEMENTS
     ‚*             JETZT POINTER WIEDER ZURÜCKSETZEN
                    SET MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR
           END-IF.
           IF       SRCSEQ OF QSETSRC-P > X-RET-LAST-SEQ
                    MOVE SRCSEQ OF QSETSRC-P TO X-RET-LAST-SEQ
           END-IF.

           GO TO    LES-DAT-20.

     ‚* DATEI NUN WIEDER SCHLIEßEN
       LES-DAT-80.
           CLOSE    QSETSRC-DP.

           PERFORM  DLT-OVR-MBR.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SAMMELN DER PROGRAMMINFORMATIONEN
     ‚*--------------------------------------------------------------
       LIST-SOURCE-ELEMENTS SECTION.
       LST-SRE-00.

     ‚* SICHERN ALLE IM PGM VORKOMMENDER SECTIONS
           IF       SRCTYPE OF RTVMBRD-REC = "CBL"
            OR      SRCTYPE OF RTVMBRD-REC = "CBLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
                    PERFORM LIST-SOURCE-ELEMENTS-SECTION
           END-IF.

     ‚* SICHERN ALLE IM PGM VORKOMMENDER SECTIONS
           IF       SRCTYPE OF RTVMBRD-REC = "DSPF"
            OR      SRCTYPE OF RTVMBRD-REC = "PRTF"
            OR      SRCTYPE OF RTVMBRD-REC = "PF"
            OR      SRCTYPE OF RTVMBRD-REC = "LF"
                    PERFORM LIST-SOURCE-ELEMENTS-FORMATS
           END-IF.

       LST-SRE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN ALLER SECTIONS
     ‚*--------------------------------------------------------------
       LIST-SOURCE-ELEMENTS-SECTION SECTION.
       LST-SSC-00.

     ‚* IST DIES EIN SECTION-ANFANG? DANN DIESEN MERKEN
           INITIALIZE SUCH-TAB.
           MOVE     " SECTION."         TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
     ‚*             JETZT DIE SECTION SICHERN
                    INITIALIZE MAPDTA-REC
                    SET  MAP-PTR OF MAPDTA-REC TO X-SECTION-PTR
                    MOVE SRCLIB-WRK    TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE SRCRCN-WRK    TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE SRCMBR-WRK    TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE SRCSEQ OF QSETSRC-P TO
                         MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE "SECTION"     TO MAP-KEY OF MAPDTA-REC(38:7)
                    MOVE SOURCE-WRK(1:E-POS(1)) TO
                         MAP-REC OF MAPDTA-REC
                    MOVE MAP-WRITE     TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
                    SET X-SECTION-PTR TO MAP-PTR OF MAPDTA-REC
     ‚*             JETZT POINTER WIEDER ZURÜCKSETZEN
                    SET MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR
           END-IF.

       LST-SSC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN ALLER FORMATE
     ‚*--------------------------------------------------------------
       LIST-SOURCE-ELEMENTS-FORMATS SECTION.
       LST-SFO-00.

     ‚* FORMAT MERKEN
           IF       SRCDTA OF QSETSRC-P(17:1)    = "R"
            AND     SRCDTA OF QSETSRC-P(7:1) NOT = "*"
     ‚*             JETZT DIE SECTION SICHERN
                    INITIALIZE MAPDTA-REC
                    SET  MAP-PTR OF MAPDTA-REC TO X-SECTION-PTR
                    MOVE SRCLIB-WRK    TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE SRCRCN-WRK    TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE SRCMBR-WRK    TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE SRCSEQ OF QSETSRC-P TO
                         MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE "FORMAT"      TO MAP-KEY OF MAPDTA-REC(38:7)
                    MOVE SRCDTA OF QSETSRC-P(19:) TO
                         MAP-REC OF MAPDTA-REC
                    MOVE MAP-WRITE     TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
                    SET X-SECTION-PTR TO MAP-PTR OF MAPDTA-REC
     ‚*             JETZT POINTER WIEDER ZURÜCKSETZEN
                    SET MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR
           END-IF.

       LST-SFO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DURCHSUCHEN STRINGTABELLE
     ‚*--------------------------------------------------------------
       SUCHEN SECTION.
       STR-SUC-00.

           INITIALIZE GEFUNDEN.
     ‚* ALLE SUCHERGEBNISSE LÖSCHEN
           MOVE     ZEROES TO IND.
           PERFORM  10 TIMES
                    ADD  1      TO IND
                    MOVE ZEROES TO S-POS(IND)
                    MOVE ZEROES TO E-POS(IND)
           END-PERFORM.

     ‚* SOURCE AUF GROßBUCHSTABEN UMSETZEN
           MOVE     1 TO IND.
           MOVE     FUNCTION UPPER-CASE(SOURCE-WRK) TO SOURCE-FND.

           MOVE     1 TO POS.
           MOVE     SUCH(1) TO STR-WRK.
           PERFORM  ERMITTELN-LAENGE.

           PERFORM  WITH TEST BEFORE UNTIL SOURCE-FND(IND:) = SPACES
                     OR IND >= LENGTH OF SOURCE-FND
                     OR STR-LEN + IND >  LENGTH OF SOURCE-FND
                    IF   SOURCE-FND(IND:STR-LEN) =
                         SUCH(1)(STR-ANF:STR-LEN)
     ‚*                  STARTPOSITION SICHERN
                         COMPUTE S-POS(POS) = IND
     ‚*                  ENDPOSITION SICHERN
                         COMPUTE E-POS(POS) = IND + STR-LEN
                         END-COMPUTE
                         MOVE 1 TO GEFUNDEN
                         ADD  1 TO POS
                    END-IF
                    ADD  1 TO IND
           END-PERFORM.

       STR-SUC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN LÄNGE EINES ÜBERGEBENEN STRINGS
     ‚*--------------------------------------------------------------
       ERMITTELN-LAENGE SECTION.
       ERM-LEN-00.

     ‚* WARSCHEINLICH WIRD DER NÄCHSTES SUCHSTRING IN DER TABELLE
     ‚* GESUCHT. DESHALB VORAB SCHONMAL PRÜFEN OB DIESER DAS IST.
     ‚* ANDERENFALLS WIRD NOCHMAL DIE KOMPLETTE TABELLE DURCHSUCHT.
           IF       STR-ANZ2 + 1 <= STR-ANZ
            AND     STR(STR-ANZ2 + 1) = STR-WRK
                    ADD 1 TO STR-ANZ2
                    MOVE S-LEN(STR-ANZ2) TO STR-ANF
                    MOVE E-LEN(STR-ANZ2) TO STR-LEN
                    GO TO ERM-LEN-90
           END-IF.

     ‚* IST FÜR DIESEN STRING BEREITS DIE SUCHE ERFOLGT, DANN
     ‚* GESICHERTE WERTE AUS DER TABELLE ÜBERNEHMEN (PERFORMANCE)
           INITIALIZE STR-ANZ2.
           PERFORM  UNTIL STR-ANZ2 >= STR-ANZ
                    ADD 1 TO STR-ANZ2
                    IF   STR(STR-ANZ2) = STR-WRK
                         MOVE S-LEN(STR-ANZ2) TO STR-ANF
                         MOVE E-LEN(STR-ANZ2) TO STR-LEN
                         GO TO ERM-LEN-90
                    END-IF
           END-PERFORM.

           INITIALIZE STR-ANF.
           INITIALIZE STR-LEN.
           PERFORM  WITH TEST BEFORE VARYING STR-POS FROM LENGTH OF
                    STR-WRK BY -1 UNTIL STR-POS = ZEROES
                    IF   STR-WRK(STR-POS:1) NOT = SPACE
                         IF   STR-LEN = ZEROES
     ‚*                       ABSCHLUSSZEICHEN
                              IF   STR-WRK(STR-POS:1) = "'"
                                   COMPUTE STR-LEN = STR-POS - 1
                              ELSE
                                   COMPUTE STR-LEN = STR-POS
                              END-IF
                         ELSE
                              IF   STR-WRK(STR-POS:1) = "'"
     ‚*                            ANFANGSZEICHEN
                                   COMPUTE STR-ANF = STR-POS + 1
                              END-IF
                         END-IF
                    END-IF
           END-PERFORM.

           IF       STR-ANF = ZEROES
                    MOVE 1 TO STR-ANF
           END-IF.
           COMPUTE  STR-LEN = STR-LEN - STR-ANF + 1.

     ‚* ERMITTELTE WERTE IN TABELLE ABLEGEN.
           ADD      1 TO STR-ANZ.
           IF       STR-ANZ > 100
                    MOVE 1 TO STR-ANZ
           END-IF.
           MOVE     STR-WRK TO STR-REC(STR-ANZ).
           MOVE     STR-ANF TO S-LEN(STR-ANZ).
           MOVE     STR-LEN TO E-LEN(STR-ANZ).

       ERM-LEN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     SRCLIB-WRK   TO OBJLIB   OF RTVMBRD-REC.
           MOVE     SRCRCN-WRK   TO FILENAME OF RTVMBRD-REC.
           MOVE     SRCMBR-WRK   TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

     ‚* KONFIGURATION ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
     ‚*             SYNTAX-HIGHLIGHTING UND ANDERE DEFAULTS
                    MOVE  1           TO CFFK01 OF CFG-CPY
                    MOVE  1           TO CFFK03 OF CFG-CPY
                    MOVE  ZEROES      TO CFFK04 OF CFG-CPY
                    MOVE  1           TO CFFK13 OF CFG-CPY
                    MOVE  "QTMPSRC"   TO CFDAT2 OF CFG-CPY
                    MOVE  "APG"       TO CFDAT3 OF CFG-CPY
                    MOVE  "QTEMP"     TO CFDAT4 OF CFG-CPY
                    MOVE  "*SAME"     TO CFDAT5 OF CFG-CPY
                    MOVE 0,01         TO CFNK01 OF CFG-CPY
           END-IF.

       INI-VAR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ENTFERNEN OVERWRITE FÜR MEMBER
     ‚*---------------------------------------------------------------
       DLT-OVR-MBR SECTION.
       DEL-OVR-00.

     ‚* OVR ZURÜCKSETZEN
           INITIALIZE DELOVR-REC.
           MOVE     SRCRCN-WRK TO FROMFILE OF DELOVR-REC.
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
           MOVE     SRCLIB-WRK TO TOLIB    OF OVRDBF-REC.
           MOVE     SRCRCN-WRK TO TOFILE   OF OVRDBF-REC.
           MOVE     SRCMBR-WRK TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
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
           MOVE     CHECK-LIB    TO LIB     OF CHKOBJ-REC.
           MOVE     CHECK-RCN    TO OBJ     OF CHKOBJ-REC.
           MOVE     CHECK-MBR    TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
