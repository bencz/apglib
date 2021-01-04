     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0015.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE OPL-/TODO-LIST                                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.01.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE/PFLEGE OPL/TODO-LISTE                *
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
     ‚* POPLLST: OPDATE, OPUSER, OPTYPE
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DP
                  ASSIGN       TO  DATABASE-POPLLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* POPLLSTL1: OPUSER, OPTYPE, OPDATE
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DL
                  ASSIGN       TO  DATABASE-POPLLSTL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PTXTFIT-DP
                  ASSIGN       TO  DATABASE-PTXTFIT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚*-   TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
     ‚*-------------------------------------------------------------
           SELECT BSSORT-DAT
                  ASSIGN       TO  DISK-BSSORT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*- TEMPORÄRE SORTDATEI FÜR BILDSCHIRMAUSGABE
       SD  BSSORT-DAT.
       01  BSSORT-REC.
           COPY  DDS-FMT02-O OF XXD0015DE
           REPLACING FMT02-O BY SORT2-O
                     FMT02-O-INDIC BY SORT2-O-INDIC.
      /
     ‚*--- POPLLST: OPDATE, OPUSER, OPTYPE
       FD  POPLLST-DP
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-P.
           COPY DDS-ALL-FORMATS OF POPLLST.
      /
     ‚*--- POPLLSTL1: OPUSER, OPTYPE, OPDATE
       FD  POPLLST-DL
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-L.
           COPY DDS-ALL-FORMATS OF POPLLSTL1.
      /
     ‚*--- TEXTE
       FD  PTXTFIT-DP
           LABEL RECORDS ARE STANDARD.
       01  PTXTFIT-P.
           COPY DDS-ALL-FORMATS OF PTXTFIT.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN02                          PIC 1.
         02 IN04                          PIC 1.
         02 IN40                          PIC 1.
         02 IN41                          PIC 1.
         02 IN42                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0015DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0015DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0015DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0015DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT4X.
           COPY  DDS-FMT04-O OF  XXD0015DE
           REPLACING FMT04-O BY FORMAT4
                     FMT04-O-INDIC BY FORMAT4-O-INDIC.

       01  FORMAT4Y.
           COPY  DDS-FMT04-I OF  XXD0015DE
           REPLACING FMT04-I BY FORMAT4-I
                     FMT04-I-INDIC BY FORMAT4-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP0015".
       01  PGM-TYP                        PIC X(8)  VALUE "OPL   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CMDEXC-REC                TYPE CMDEXC-TYPE.

       01  UPDATE-KZ                      PIC 9(1).
       01  AUSWAHL                        PIC X(2).
       01  OPTYPE-WRK                LIKE OPTYPE OF POPLLST-P.
       01  NEW-OPTYPE-WRK            LIKE OPTYPE OF POPLLST-P.
     ‚* ANZEIGE-MODUS:
     ‚*  0 = ALLE
     ‚*  1 = NUR WICHTIGE (NICHT FERTIG)
     ‚*  2 = ABGESCHLOSSENE
       01  ANZEIGE-MODUS                  PIC 9(1).
       01  ANZEIGE-SYSTEM                 PIC 9(1).

       01  HEX-COLOR-PR                   PIC X(1).
       01  HEX-COLOR-RI                   PIC X(1).

       01  SORT-KZ                        PIC X(2) VALUE "11".
       01  INP-WRK                   LIKE CFFK24 OF CFG-CPY.

       01  COMMAND-WRK                    PIC X(50).
       01  OPTEXT-WRK                LIKE OPTEXT OF POPLLST-P.
       01  FTKEY-PERFORM             LIKE FTKEY  OF PTXTFIT-P.

     ‚* SONSTIGE SCHALTER
       01  CHG-IMPORTANT                  PIC 9(1).

     ‚* VARIABLEN FÜR DIE DISPLAYÄNDERUNG
       01  NEW-DISPLAY-WRK                PIC 9(1).
       01  DISPLAY-HLP               LIKE DISPLAY-WRK.

     ‚* VARIABLEN FÜR DIE ENTSCHLÜSSELUNG
       01  DECRYPT-TEXT              LIKE OPTEXT OF POPLLST-P.
       01  CRYPT-PASSWORD                 PIC X(30).
       01  AUTO-DECRYPT                   PIC 9(1).
       01  MAN-CALL                       PIC 9(1).

     ‚* FARBSTRUKTUR AUS DEM SCHLÜSSEL (FELD CFIN21)
       01  COLOR-STRUCT.
        05 PRZRD1                         PIC 9(2).
        05 PRZRD2                         PIC 9(2).
        05 PRZYW1                         PIC 9(2).
        05 PRZYW2                         PIC 9(2).
        05 PRZGN1                         PIC 9(2).
        05 PRZGN2                         PIC 9(2).
        05 PRIRD1                         PIC 9(2).
        05 PRIRD2                         PIC 9(2).
        05 PRIYW1                         PIC 9(2).
        05 PRIYW2                         PIC 9(2).
        05 PRIGN1                         PIC 9(2).
        05 PRIGN2                         PIC 9(2).

     ‚* PARAMETER FÜR DAS CFP0008
       01  CFID-WRK                  LIKE CFID   OF CFG-CPY.
       01  CFKEY-WRK                 LIKE CFKEY  OF CFG-CPY.
       01  CFKEY2-WRK                LIKE CFKEY2 OF CFG-CPY.
       01  CFKEY3-WRK                LIKE CFKEY3 OF CFG-CPY.
       01  CFKEY4-WRK                LIKE CFKEY4 OF CFG-CPY.

     ‚* PARAMETER FÜR DAS CFP0012
       01  USER-20A                       PIC X(20).
       01  0012-AKTION                    PIC X(20).

     ‚* PARAMETER FÜR DAS XXP0018
       01  USER-WRK                  LIKE T-USER.

     ‚* PARAMETER FÜR DAS CFP0023
       01  FTFILE-WRK                LIKE FTFILE OF PTXTFIT-P.
       01  FTKEY-WRK                 LIKE FTKEY  OF PTXTFIT-P.
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-SELEKTION                    PIC X(30).
       01  X-KATEGORIE                    PIC X(10).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-SELEKTION
                                X-KATEGORIE.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0015" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN   I-O   POPLLST-DP
                    OPEN   INPUT POPLLST-DL
                                 PTXTFIT-DP
           END-IF.

           PERFORM  GET-CONFIG.

     ‚* DISPLAY ÖFFNEN
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT3.
           INITIALIZE F3, F12.
           INITIALIZE ANZEIGE-MODUS.
           INITIALIZE SELEKTION-WRK.
           MOVE     "*DEFAULT" TO OPTYPE-WRK.
           PERFORM  COPY-GET-TIME.

       ANF010.
           PERFORM  ANZEIGE-SUBFILE.
           IF       F2 = "1"
                    GO TO ENDE
           END-IF.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

           GO TO    ANF010.

       ENDE.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SORTFILE EINLESEN UND AUSGEBEN
     ‚*--------------------------------------------------------------
       LESE-DATEN-SORTFILE SECTION.
       LES-DAS-00.

           INITIALIZE DATEN.
     ‚* AUSGABE SORTIEREN
           EVALUATE SORT-KZ
     ‚*             NACH DATUM AUFSTEIGEND SORTIEREN
                    WHEN "11"
                         SORT     BSSORT-DAT ON ASCENDING KEY
                                                  OPDATE OF BSSORT-REC,
                                                  OPLFDN OF BSSORT-REC
                                   WITH DUPLICATES
                                  INPUT  PROCEDURE LESE-DATEN
                                  OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT
     ‚*             NACH DATUM ABSTEIGEND SORTIEREN
                    WHEN "12"
                         SORT     BSSORT-DAT ON DESCENDING KEY
                                                  OPDATE OF BSSORT-REC,
                                                  OPLFDN OF BSSORT-REC
                                   WITH DUPLICATES
                                  INPUT  PROCEDURE LESE-DATEN
                                  OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT
     ‚*             NACH PRIO + DATUM AUFSTEIGEND SORTIEREN
                    WHEN "21"
                         SORT     BSSORT-DAT ON ASCENDING KEY
                                                  OPPRIO OF BSSORT-REC,
                                                  OPDATE OF BSSORT-REC,
                                                  OPLFDN OF BSSORT-REC
                                   WITH DUPLICATES
                                  INPUT  PROCEDURE LESE-DATEN
                                  OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT
     ‚*             NACH PRIO + DATUM ABSTEIGEND SORTIEREN
                    WHEN "22"
                         SORT     BSSORT-DAT ON DESCENDING KEY
                                                  OPPRIO OF BSSORT-REC,
                                                  OPDATE OF BSSORT-REC,
                                                  OPLFDN OF BSSORT-REC
                                   WITH DUPLICATES
                                  INPUT  PROCEDURE LESE-DATEN
                                  OUTPUT PROCEDURE SUBFILE-SORT-OUTPUT
           END-EVALUATE.

       LES-DAS-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* POPLLST LESEN
           INITIALIZE POPLLSTF OF POPLLST-L.
           MOVE     T-USER              TO OPUSER OF POPLLST-L.
           MOVE     OPTYPE-WRK          TO OPTYPE OF POPLLST-L.
           MOVE     ZEROES              TO OPDATE OF POPLLST-L.
           MOVE     ZEROES              TO OPLFDN OF POPLLST-L.
           START    POPLLST-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     POPLLST-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       OPUSER OF POPLLST-L NOT = T-USER
            OR      OPTYPE OF POPLLST-L NOT = OPTYPE-WRK
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHON ABGESCHLOSSEN?
           EVALUATE ANZEIGE-MODUS
     ‚*        ALLE NORMALEN UND WICHTIGE ANZEIGEN
               WHEN ZEROES
                    IF   OPSTAT OF POPLLST-L = "4"
                     OR  OPSTAT OF POPLLST-L = "9"
                         GO TO LES-DAT-20
                    END-IF
     ‚*        NUR DIE WICHTIGEN (OFFENEN) ANZEIGEN
               WHEN 1
                    IF   OPIMPT OF POPLLST-L = ZEROES
                     OR  OPSTAT OF POPLLST-L = "4"
                     OR  OPSTAT OF POPLLST-L = "9"
                         GO TO LES-DAT-20
                    END-IF
     ‚*        ALLE ANZEIGEN (INKL. ABGESCHLOSSENE)
               WHEN 2
                    CONTINUE
           END-EVALUATE.

     ‚* NACH TEXT SELEKTIEREN?
           IF       SELEKTION-WRK NOT = SPACES
                    MOVE OPTEXT OF POPLLST-L           TO RECORD-WRK
                    MOVE LENGTH OF OPTEXT OF POPLLST-L TO RECORD-LEN
                    CALL PROCEDURE "HLP0001" USING RECORD-WRK
                                                   RECORD-LEN
                                                   SELEKTION-WRK
                                                   RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

           INITIALIZE BSSORT-REC.
           MOVE     CORR POPLLSTF OF POPLLST-L TO
                    SORT2-O OF BSSORT-REC.

           MOVE     OPPRIO OF POPLLST-L TO ANZPRIO  OF BSSORT-REC.
           MOVE     OPDATE OF POPLLST-L TO DT_CRT8  OF BSSORT-REC.
           IF       OPMSER OF POPLLST-L = SPACES
                    MOVE OPAUSR OF POPLLST-L TO LSTUSR OF BSSORT-REC
                    MOVE OPADTA OF POPLLST-L TO DT_LST OF BSSORT-REC
                    MOVE OPATIA OF POPLLST-L TO TI_LST OF BSSORT-REC
           ELSE
                    MOVE OPMSER OF POPLLST-L TO LSTUSR OF BSSORT-REC
                    MOVE OPMDTA OF POPLLST-L TO DT_LST OF BSSORT-REC
                    MOVE OPMTIA OF POPLLST-L TO TI_LST OF BSSORT-REC
           END-IF.

           RELEASE  BSSORT-REC.
           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DES SUBFILES INKL. GRUPPIERUNG
     ‚*--------------------------------------------------------------
       SUBFILE-SORT-OUTPUT SECTION.
       SUB-SOU-00.

           INITIALIZE ANZREC-WRK.

     ‚* SORTFILE AUSLESEN
       SUB-SOU-20.
           RETURN   BSSORT-DAT AT END
                    GO TO SUB-SOU-90
           END-RETURN.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     BSSORT-REC   TO FORMAT2.

     ‚* WENN VERSCHLÜSSELT, TEXT ERSTMAL NICHT ANZEIGEN
           IF       OPCRYP OF FORMAT2 = 1
                    MOVE    ZEROES TO MAN-CALL
                    PERFORM GET-ENCRYPTED-TEXT
                    MOVE    DECRYPT-TEXT TO OPTEXT OF FORMAT2
           END-IF.

           PERFORM  SET-COLOR-FMT02.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           ADD      1            TO ANZREC-WRK.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO  SUB-SOU-20
           END-IF.

       SUB-SOU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBE SETZEN
     ‚*--------------------------------------------------------------
       SET-COLOR-FMT02 SECTION.
       SET-COL-00.

     ‚* EINTRÄGE BLAU FÄRBEN, WENN DIESE EIN SONDERTYP SIND
           IF       OPTTYP OF FORMAT2 NOT = SPACES
                    MOVE      COL-RED     TO A_DT_CRT8 OF FORMAT2
           ELSE
     ‚*             ANSONSTEN WICHTIGE EINTRÄGE HERVORHEBEN
                    IF   OPIMPT OF FORMAT2 NOT = ZEROES
                         MOVE COL-BLU     TO A_DT_CRT8 OF FORMAT2
                    END-IF
           END-IF.

     ‚* SHARE-EINTRÄGE MARKIEREN
           IF       OPSHRN OF FORMAT2 NOT = ZEROES
                    MOVE     HEX-COLOR-PR TO A_OPTEXT  OF FORMAT2
                    MOVE     HEX-COLOR-PR TO A_OPPRZT  OF FORMAT2
           ELSE
     ‚*             ALLES PROTECTEN
                    MOVE     ATR-PR       TO A_OPTEXT  OF FORMAT2
                    MOVE     ATR-PR       TO A_OPPRZT  OF FORMAT2
           END-IF.

     ‚* SPEZIELLE FARBE FÜR PROZENT/PRIO?
     ‚*
     ‚* PRIORITÄT
           IF       OPPRIO OF FORMAT2 NOT = ZEROES
            AND     (     PRIRD1 OF COLOR-STRUCT > ZEROES
                     OR   PRIRD2 OF COLOR-STRUCT > ZEROES
                     OR   PRIYW1 OF COLOR-STRUCT > ZEROES
                     OR   PRIYW2 OF COLOR-STRUCT > ZEROES
                     OR   PRIGN1 OF COLOR-STRUCT > ZEROES
                     OR   PRIGN2 OF COLOR-STRUCT > ZEROES)
     ‚*             ROTE FARBE
                    IF   OPPRIO OF FORMAT2 >= PRIRD1 OF COLOR-STRUCT
                     AND OPPRIO OF FORMAT2 <= PRIRD2 OF COLOR-STRUCT
                         IF   CFFK04 OF CFG-CPY = 1
                              MOVE COL-RED-RI TO A_ANZPRIO OF FORMAT2
                         ELSE
                              MOVE COL-RED    TO A_ANZPRIO OF FORMAT2
                         END-IF
                    END-IF
     ‚*             GELBE FARBE
                    IF   OPPRIO OF FORMAT2 >= PRIYW1 OF COLOR-STRUCT
                     AND OPPRIO OF FORMAT2 <= PRIYW2 OF COLOR-STRUCT
                         IF   CFFK05 OF CFG-CPY = 1
                              MOVE COL-YLW-RI TO A_ANZPRIO OF FORMAT2
                         ELSE
                              MOVE COL-YLW    TO A_ANZPRIO OF FORMAT2
                         END-IF
                    END-IF
     ‚*             GRÜNE FARBE
                    IF   OPPRIO OF FORMAT2 >= PRIGN1 OF COLOR-STRUCT
                     AND OPPRIO OF FORMAT2 <= PRIGN2 OF COLOR-STRUCT
                         IF   CFFK06 OF CFG-CPY = 1
                              MOVE COL-GRN-RI TO A_ANZPRIO OF FORMAT2
                         ELSE
                              MOVE COL-GRN    TO A_ANZPRIO OF FORMAT2
                         END-IF
                    END-IF
           END-IF.
     ‚* PROZENT
           IF       OPPRZT OF FORMAT2 NOT = ZEROES
            AND     (     PRZRD1 OF COLOR-STRUCT > ZEROES
                     OR   PRZRD2 OF COLOR-STRUCT > ZEROES
                     OR   PRZYW1 OF COLOR-STRUCT > ZEROES
                     OR   PRZYW2 OF COLOR-STRUCT > ZEROES
                     OR   PRZGN1 OF COLOR-STRUCT > ZEROES
                     OR   PRZGN2 OF COLOR-STRUCT > ZEROES)
     ‚*             ROTE FARBE
                    IF   OPPRZT OF FORMAT2 >= PRZRD1 OF COLOR-STRUCT
                     AND OPPRZT OF FORMAT2 <= PRZRD2 OF COLOR-STRUCT
                         IF   CFFK01 OF CFG-CPY = 1
                              MOVE ATR-PR-RED-RI TO A_OPPRZT OF FORMAT2
                         ELSE
                              MOVE ATR-PR-RED    TO A_OPPRZT OF FORMAT2
                         END-IF
                    END-IF
     ‚*             GELBE FARBE
                    IF   OPPRZT OF FORMAT2 >= PRZYW1 OF COLOR-STRUCT
                     AND OPPRZT OF FORMAT2 <= PRZYW2 OF COLOR-STRUCT
                         IF   CFFK02 OF CFG-CPY = 1
                              MOVE ATR-PR-YLW-RI TO A_OPPRZT OF FORMAT2
                         ELSE
                              MOVE ATR-PR-YLW    TO A_OPPRZT OF FORMAT2
                         END-IF
                    END-IF
     ‚*             GRÜNE FARBE
                    IF   OPPRZT OF FORMAT2 >= PRZGN1 OF COLOR-STRUCT
                     AND OPPRZT OF FORMAT2 <= PRZGN2 OF COLOR-STRUCT
                         IF   CFFK03 OF CFG-CPY = 1
                              MOVE ATR-PR-RI     TO A_OPPRZT OF FORMAT2
                         ELSE
                              MOVE ATR-PR        TO A_OPPRZT OF FORMAT2
                         END-IF
                    END-IF
           END-IF.

     ‚* ABGESCHLOSSENE INVERS (ÜBERSCHREIBT ALLES)
           IF       OPPRZT OF FORMAT2 >= 100
                    IF   A_OPPRZT OF FORMAT2 = HEX-COLOR-PR
                         MOVE HEX-COLOR-RI  TO A_OPTEXT  OF FORMAT2
                         MOVE HEX-COLOR-RI  TO A_OPPRZT  OF FORMAT2
                    ELSE
                         MOVE ATR-PR-RI     TO A_OPTEXT  OF FORMAT2
                         MOVE ATR-PR-RI     TO A_OPPRZT  OF FORMAT2
                    END-IF
           END-IF.

     ‚* INDIKATOR AKTIVIEREN
           MOVE     AN           TO IN02     OF INXX.

       SET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE F2.
           INITIALIZE FORMAT3.
           INITIALIZE NEW-DISPLAY-WRK.
           MOVE     1          TO SFL-ZEILE.
     ‚* DATEN ÜBERNEHMEN
           MOVE     X-SELEKTION TO SELEKTION-WRK.
           IF       X-KATEGORIE NOT = SPACES
                    MOVE X-KATEGORIE TO OPTYPE-WRK
           END-IF.

       ANZ-SFL-10.
     ‚* SELEKTION ANZEIGEN
           MOVE     AUS    TO INXX.
           PERFORM  LESE-DATEN-SORTFILE.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           INITIALIZE FORMAT4.
           IF       INP-WRK = 1
                    MOVE AUS TO IN40 OF INXX
                    MOVE "TXTLNG" TO DSP-CUR-FLD
           ELSE
                    MOVE AN  TO IN40 OF INXX
                    MOVE "OPTEXT" TO DSP-CUR-FLD
           END-IF.
     ‚* SIND INFORMATIONEN VORHANDEN?
           CALL     "CFP0010" USING PGM-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    MOVE AN  TO IN42 OF INXX
           ELSE
                    MOVE AUS TO IN42 OF INXX
           END-IF.
     ‚* AUS IRGENDEINEM GRUND VERLIEREN WIR DAS "BLAU" ATTRIBUT
           MOVE     COL-BLU         TO OPTYPE OF FORMAT4(1:1).
           MOVE     OPTYPE-WRK      TO OPTYPE OF FORMAT4(2:).
           MOVE     DATE-8          TO DT_CRT OF FORMAT4.
           MOVE     CORR INXX       TO FORMAT4-O-INDIC.
           MOVE     FORMAT4         TO WSREC-WRK.
           MOVE     "FMT04"         TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           EVALUATE SFL-ZEILE
                    WHEN 1
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE 1 TO SUBFILE-RECORD-NUMBER
                    WHEN OTHER
                         IF   SFL-ZEILE <= ANZREC-WRK
                              MOVE SFL-ZEILE  TO SUBFILE-RECORD-NUMBER
                         ELSE
                              MOVE ANZREC-WRK TO SUBFILE-RECORD-NUMBER
                         END-IF
           END-EVALUATE.

     ‚* CURSER SETZEN
     ‚*€   MOVE     "OPTEXT" TO DSP-CUR-FLD.
     ‚*€   MOVE     18 TO DSP-CURY OF DSP-ATTR.
     ‚*€   MOVE     31 TO DSP-CURX OF DSP-ATTR.

           IF       DATEN = ZEROES
     ‚*             AUSGABE FEHLERMELDUNG
                    MOVE     "FMT05"         TO  FMT-WRK
                    PERFORM  COPY-DISPLAY-WRITE
                    MOVE     AUS    TO IN91  IN  INXX
           ELSE
                    MOVE     AN     TO IN91  IN  INXX
           END-IF.
     ‚* ANZEIGE VON DATUM/UHRZEIT ODER SYS_CMP/SYS_CMT?
           IF       ANZEIGE-SYSTEM = ZEROES
                    MOVE AUS TO IN41 OF INXX
           ELSE
                    MOVE AN  TO IN41 OF INXX
           END-IF.

           MOVE     SELEKTION-WRK         TO SELEKT  OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F14 SORTIERUNG ÄNDERN
           IF       IN14 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    PERFORM EXECUTE-COMMAND-SORT
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F15 ANZEIGE ÄNDERN
           IF       IN15 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    PERFORM EXECUTE-COMMAND-INPUT
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* F20 PROGRAMMINFORMATIONEN
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO ANZ-SFL-20
           END-IF.

     ‚* F21 KOMMANDOZEILE ANZEIGEN
           IF       IN21 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    CALL     "XXP2001"
                    END-CALL
                    GO TO ANZ-SFL-20
           END-IF.

           MOVE     "FMT04" TO FMT-WRK.
           MOVE     FORMAT4 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT4-I.
           MOVE     CORR FORMAT4-I TO FORMAT4.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F2 KATEGORIE WECHSELN
           IF       IN02 IN FORMAT3-I-INDIC = AN
                    MOVE "H102" TO CFID-WRK
                    MOVE SPACES TO CFKEY-WRK
                    MOVE T-USER TO CFKEY2-WRK
                    MOVE SPACES TO CFKEY3-WRK
                    MOVE SPACES TO CFKEY4-WRK
                    MOVE SPACES TO F12
                    CALL "CFP0008" USING CFID-WRK
                                         CFKEY-WRK
                                         CFKEY2-WRK
                                         CFKEY3-WRK
                                         CFKEY4-WRK
                                         F12
                    END-CALL
                    IF   F12 = SPACES
                         MOVE CFKEY-WRK TO OPTYPE-WRK
                    END-IF
                    INITIALIZE F12
                    GO TO ANZ-SFL-00
           END-IF.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F13 ALLE ANZEIGEN
           IF       IN13 IN FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    PERFORM EXECUTE-COMMAND-SHOW
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* ÄNDERUNG?
           IF       SELEKT OF FORMAT3 NOT = SELEKTION-WRK
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE SELEKT OF FORMAT3 TO SELEKTION-WRK
                    GO TO ANZ-SFL-10
           END-IF.

     ‚* ÄNDERUNG?
           IF       OPTEXT OF FORMAT4 NOT = SPACES
            OR      TXTLNG OF FORMAT4 NOT = SPACES
                    IF   OPTEXT OF FORMAT4(1:1) = ":"
                     OR  TXTLNG OF FORMAT4(1:1) = ":"
                         IF   OPTEXT OF FORMAT4 NOT = SPACES
                              MOVE OPTEXT OF FORMAT4 TO COMMAND-WRK
                         ELSE
                              MOVE TXTLNG OF FORMAT4 TO COMMAND-WRK
                         END-IF
                         PERFORM EXECUTE-COMMAND
                         IF   NEW-DISPLAY-WRK = 1
                              MOVE    DISPLAY-HLP TO REOPEN-DISPLAY-WRK
                              PERFORM COPY-DISPLAY-REOPEN
                         END-IF
                    ELSE
                         PERFORM CREATE-NEW-RECORD
                    END-IF
                    GO TO ANZ-SFL-00
           END-IF.

           MOVE     SPACES TO SFL-CHG.
           MOVE     SPACES TO NEW-OPTYPE-WRK.
           MOVE     ZEROES TO CHG-IMPORTANT.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    IF   SFL-CHG = "1"
                         GO TO ANZ-SFL-10
                    ELSE
                         GO TO ANZ-SFL-20
                    END-IF
           END-IF.

           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.

     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

     ‚* BERECHTIGUNG FÜR DIESE FUNKTION ÜBERPRÜFEN
           IF       AUSW OF FORMAT2 NOT = SPACES
                    MOVE  SPACES          TO RET-CODE
                    MOVE  AUSW OF FORMAT2 TO AUSWAHL
                    CALL "CFP9002" USING T-USER
                                         AUSWAHL
                                         PGM-WRK
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         MOVE     SPACES TO AUSW OF FORMAT2
                         MOVE     "FMT02" TO FMT-WRK
                         MOVE     FORMAT2 TO WSREC-WRK
                         PERFORM  COPY-DISPLAY-SFLREWRITE
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* AUSWÄHLEN DES EINTRAGS (DETAILANZEIGE)
           IF       AUSW OF FORMAT2 = "1"
                    MOVE "1" TO SFL-CHG
                    CALL "XXP0019" USING OPLFDN OF FORMAT2
                                         F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO ANZ-SFL-90
                    END-IF
           END-IF.

     ‚* ÄNDERN DES STATUS
           IF       AUSW OF FORMAT2 = "2"
                    MOVE  COL-GRN-US   TO A_OPTEXT  OF FORMAT2
                    MOVE  COL-GRN-US   TO A_OPPRZT  OF FORMAT2
                    MOVE  AN           TO IN02 OF INXX
                    MOVE  AN           TO IN04 OF INXX
           ELSE
                    PERFORM  SET-COLOR-FMT02
           END-IF.

     ‚* LÖSCHEN DES TODO-EINTRAGS
           IF       AUSW OF FORMAT2 = "4"
                    MOVE "1" TO SFL-CHG
                    CALL "XXP0016" USING OPLFDN OF FORMAT2
                    END-CALL
                    IF   OPSHRN OF FORMAT2 NOT = ZEROES
                         CALL "XXP0017" USING OPLFDN OF FORMAT2
                                              OPSHRN OF FORMAT2
                         END-CALL
                    END-IF
           END-IF.

     ‚* MEHR TEXT ZU DEM EINTRAG ERFASSEN
           IF       AUSW OF FORMAT2 = "8"
                    MOVE "POPLLST"         TO FTFILE-WRK
                    IF   OPSHRN OF FORMAT2 = ZEROES
                         MOVE OPLFDN OF FORMAT2 TO FTKEY-WRK
                    ELSE
                         MOVE "S"               TO FTKEY-WRK(1:1)
                         MOVE OPSHRN OF FORMAT2 TO FTKEY-WRK(2:)
                    END-IF
                    MOVE "2"               TO AUSWAHL
                    CALL "CFP0023" USING FTFILE-WRK
                                         FTKEY-WRK
                                         AUSWAHL
                                         PGM-WRK
                                         F3
                                         F12
                    END-CALL
                    INITIALIZE F12
           END-IF.

     ‚* "PERFORM"-DATEI ERSTELLEN
           IF       AUSW OF FORMAT2 = "Y"
                    MOVE "POPLLST" TO FTFILE-WRK
                    MOVE "2"       TO AUSWAHL
                    CALL "CFP0023" USING FTFILE-WRK
                                         FTKEY-PERFORM
                                         AUSWAHL
                                         PGM-WRK
                                         F3
                                         F12
                    END-CALL
                    INITIALIZE F12
           END-IF.

     ‚* PRIORITÄT ÄNDERN
           IF       AUSW OF FORMAT2 = "P"
                    CALL "XXP0020" USING OPLFDN OF FORMAT2
                                         F12
                    END-CALL
                    IF   F12 NOT = "1"
                         MOVE "1" TO SFL-CHG
                    END-IF
                    INITIALIZE F12
           END-IF.

     ‚* KATEGORIE WECHSELN
           IF       AUSW OF FORMAT2 = "C"
                    MOVE "H102" TO CFID-WRK
                    MOVE SPACES TO CFKEY-WRK
                    MOVE T-USER TO CFKEY2-WRK
                    MOVE SPACES TO CFKEY3-WRK
                    MOVE SPACES TO CFKEY4-WRK
                    MOVE SPACES TO F12
                    CALL "CFP0008" USING CFID-WRK
                                         CFKEY-WRK
                                         CFKEY2-WRK
                                         CFKEY3-WRK
                                         CFKEY4-WRK
                                         F12
                    END-CALL
                    IF   F12 = SPACES
                         MOVE CFKEY-WRK TO NEW-OPTYPE-WRK
                    END-IF
                    INITIALIZE F12
           END-IF.

     ‚* IMPORTANT-FLAG WECHSELN
           IF       AUSW OF FORMAT2 = "I"
                    MOVE 1 TO CHG-IMPORTANT
           END-IF.

     ‚* EINTRAG ABSCHLIEßEN
           IF       AUSW OF FORMAT2 = "9"
                    MOVE 100 TO OPPRZT OF FORMAT2
           END-IF.

     ‚* MIT USER TEILEN
           IF       AUSW OF FORMAT2 = "S"
                    MOVE SPACES TO USER-20A
                    MOVE "USER" TO 0012-AKTION
                    CALL "CFP0012" USING USER-20A
                                         0012-AKTION
                                         F12
                    END-CALL
                    IF   USER-20A NOT = SPACES
                         MOVE USER-20A TO USER-WRK
                         INITIALIZE LFDN-ID, LFDN-WRK
                         MOVE     "OPL-SHARE" TO LFDN-ID
                         CALL     "CFP0020" USING LFDN-ID
                                                  LFDN-WRK
                         END-CALL
                    END-IF
                    INITIALIZE F12
           END-IF.

     ‚* MIT MAT/WECKER EINE ERINNERUNG ERSTELLEN
           IF       AUSW OF FORMAT2 = "M"
                    PERFORM  MAT-WECKER
           END-IF.

     ‚* TEXT VERSCHLÜSSELN
           IF       AUSW OF FORMAT2 = "E"
                    PERFORM  SET-ENCRYPT-TEXT
           END-IF.

     ‚* TEXT ENTSCHLÜSSELN
           IF       AUSW OF FORMAT2 = "D"
                    PERFORM  SET-DECRYPT-TEXT
           END-IF.

     ‚* ZEILE MIT TEXT EDITIEREN
           IF       AUSW OF FORMAT2 NOT = "1"
            AND     AUSW OF FORMAT2 NOT = "4"
            AND     AUSW OF FORMAT2 NOT = "8"
            AND     AUSW OF FORMAT2 NOT = "M"
            AND     AUSW OF FORMAT2 NOT = "P"
            AND     AUSW OF FORMAT2 NOT = "Y"
            AND     AUSW OF FORMAT2 NOT = "E"
            AND     AUSW OF FORMAT2 NOT = "D"
                    MOVE OPTEXT  OF FORMAT2 TO OPTEXT-WRK
                    PERFORM  UPDATE-OPTEXT
           END-IF.

     ‚* SHARE-NUMBER INITIALISIEREN
           MOVE     ZEROES TO LFDN-WRK.
           MOVE     SPACES TO USER-WRK.
           MOVE     SPACES TO NEW-OPTYPE-WRK.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN VON INFORMATIONSTEXTEN
     ‚*--------------------------------------------------------------
       ANZEIGE-PGM-INFOS SECTION.
       ANZ-PGM-00.

           INITIALIZE F12.
     ‚* INFORMATIONEN ANZEIGEN
           CALL     "CFP0009" USING PGM-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

       ANZ-PGM-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

           MOVE     "XXP0015"         TO MAINUEB OF FORMAT3.
           MOVE     "XXP0015*3"       TO SUBUEB  OF FORMAT3.

     ‚* ANZEIGEMODUS
           EVALUATE ANZEIGE-MODUS
                    WHEN ZEROES MOVE "PGM0040" TO CPY-TXID
                    WHEN 1      MOVE "PGM0042" TO CPY-TXID
                    WHEN 2      MOVE "PGM0041" TO CPY-TXID
           END-EVALUATE.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO ANZMOD OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WECKER STARTEN
     ‚*--------------------------------------------------------------
       MAT-WECKER SECTION.
       MAT-WEC-00.

     ‚* COMMAND AUFRUFEN
           INITIALIZE CMDEXC-REC.
           STRING   "MAT/WECKER "          DELIMITED BY SIZE
                    "??WECKZEIT() "        DELIMITED BY SIZE
                    "??MSG('"              DELIMITED BY SIZE
                    OPTEXT OF FORMAT2      DELIMITED BY SIZE
                    "') "                  DELIMITED BY SIZE
                    "??WECKDAT(*CURRENT) " DELIMITED BY SIZE
                    "??SNDMAIL()"          DELIMITED BY SIZE
               INTO COMMAND OF CMDEXC-REC
           END-STRING.
           CALL     "CLP0057" USING CMDEXC-REC
                                    RET-CODE
           END-CALL.

       MAT-WEC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN EINES NEUEN PUNKTES IN DER OPL
     ‚*--------------------------------------------------------------
       CREATE-NEW-RECORD SECTION.
       CRT-NRC-00.

           INITIALIZE POPLLSTF OF POPLLST-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "POPLLST" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK                  TO OPLFDN OF POPLLST-P.

     ‚* NUTZDATEN FÜLLEN
           MOVE     DT_CRT OF FORMAT4(1:8)    TO OPDATE OF POPLLST-P.
           MOVE     T-USER                    TO OPUSER OF POPLLST-P.
           MOVE     OPTYPE-WRK                TO OPTYPE OF POPLLST-P.
           IF       OPTEXT OF FORMAT4 NOT = SPACES
                    MOVE    OPTEXT OF FORMAT4 TO OPTEXT OF POPLLST-P
           ELSE
                    MOVE    TXTLNG OF FORMAT4 TO OPTEXT OF POPLLST-P
           END-IF.
           MOVE     ZEROES                    TO OPPRZT OF POPLLST-P.
           MOVE     SPACES                    TO OPSTAT OF POPLLST-P.

     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                    TO OPAUSR OF POPLLST-P.
           MOVE     T-TERM                    TO OPABS  OF POPLLST-P.
           MOVE     PGM-WRK                   TO OPAPGM OF POPLLST-P.
           MOVE     DATE-8                    TO OPADTA OF POPLLST-P.
           MOVE     TIME-6                    TO OPATIA OF POPLLST-P.

           WRITE    POPLLST-P.
           INITIALIZE LFDN-ID, LFDN-WRK.

       CRT-NRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHL AUSFÜHREN - ERSTMAL FIX
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND SECTION.
       EXC-CMD-00.

     ‚* BEFEHLE IMMER GROß
           MOVE     FUNCTION UPPER-CASE(COMMAND-WRK) TO COMMAND-WRK.

     ‚* WELCHER BEFEHL?
           EVALUATE COMMAND-WRK(1:8)
                    WHEN ":SYSTEM"  PERFORM EXECUTE-COMMAND-SYSTEM
                    WHEN ":INPUT"   PERFORM EXECUTE-COMMAND-INPUT
                    WHEN ":SORT"    PERFORM EXECUTE-COMMAND-SORT
                    WHEN ":SHOW"    PERFORM EXECUTE-COMMAND-SHOW
                    WHEN ":STYPE"   PERFORM EXECUTE-COMMAND-STYPE
                    WHEN ":DSPSIZE" PERFORM EXECUTE-COMMAND-DSPSIZE
           END-EVALUATE.

       EXC-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHL ":SYSTEM" AUSFÜHREN
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND-SYSTEM SECTION.
       EXC-CSY-00.

           EVALUATE ANZEIGE-SYSTEM
                    WHEN ZEROES MOVE 1      TO ANZEIGE-SYSTEM
                    WHEN 1      MOVE ZEROES TO ANZEIGE-SYSTEM
           END-EVALUATE.

       EXC-CSY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHL ":SYSTEM" AUSFÜHREN
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND-SHOW SECTION.
       EXC-CSH-00.

           EVALUATE ANZEIGE-MODUS
                    WHEN ZEROES MOVE 1      TO ANZEIGE-MODUS
                    WHEN 1      MOVE 2      TO ANZEIGE-MODUS
                    WHEN 2      MOVE ZEROES TO ANZEIGE-MODUS
           END-EVALUATE.

       EXC-CSH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHL ":INPUT" AUSFÜHREN
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND-INPUT SECTION.
       EXC-CIP-00.

           EVALUATE INP-WRK
                    WHEN ZEROES MOVE 1      TO INP-WRK
                    WHEN 1      MOVE ZEROES TO INP-WRK
           END-EVALUATE.

       EXC-CIP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHL ":SORT" AUSFÜHREN
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND-SORT SECTION.
       EXC-CST-00.

           EVALUATE SORT-KZ
     ‚*        INNERHALB DATUM WECHSELN
               WHEN "11" MOVE "12" TO SORT-KZ
               WHEN "12" MOVE "11" TO SORT-KZ
     ‚*        INNERHALB PRIO WECHSELN
               WHEN "21" MOVE "22" TO SORT-KZ
               WHEN "22" MOVE "21" TO SORT-KZ
           END-EVALUATE.

       EXC-CST-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHL ":STYPE" AUSFÜHREN
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND-STYPE SECTION.
       EXC-CST-00.

           EVALUATE SORT-KZ
     ‚*        VON DATUM NACH PRIO WECHSELN
               WHEN "11" MOVE "21" TO SORT-KZ
               WHEN "12" MOVE "22" TO SORT-KZ
     ‚*        VON PRIO NACH DATUM WECHSELN
               WHEN "21" MOVE "11" TO SORT-KZ
               WHEN "22" MOVE "12" TO SORT-KZ
           END-EVALUATE.

       EXC-CST-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZU ÖFFNENDES DISPLAY SETZEN
     ‚*--------------------------------------------------------------
       EXECUTE-COMMAND-DSPSIZE SECTION.
       EXC-CDS-00.

           INITIALIZE NEW-DISPLAY-WRK.

           EVALUATE COMMAND-WRK(1:15)
     ‚*             WINDOW
                    WHEN ":DSPSIZE WDW"
                         MOVE "XXD0015DE " TO DISPLAY-HLP
     ‚*             27X132
                    WHEN ":DSPSIZE 27"
                         MOVE "XXD0015DEB" TO DISPLAY-HLP
     ‚*             24X80
                    WHEN ":DSPSIZE 24"
                         MOVE "XXD0015DES" TO DISPLAY-HLP
           END-EVALUATE.
           IF       DISPLAY-WRK NOT = DISPLAY-HLP
                    MOVE 1 TO NEW-DISPLAY-WRK
           END-IF.

       EXC-CDS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXT AKTUALISIEREN
     ‚*--------------------------------------------------------------
       UPDATE-OPTEXT SECTION.
       UPD-TXT-00.

           MOVE     ZEROES TO UPDATE-KZ.
     ‚* EINTRAG AKTUALISIEREN
           INITIALIZE POPLLSTF OF POPLLST-P.
           MOVE     OPLFDN OF FORMAT2 TO OPLFDN OF POPLLST-P.
           READ     POPLLST-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-TXT-90
           END-IF.

           IF       OPTEXT-WRK        NOT = OPTEXT OF POPLLST-P
            OR      OPPRZT OF FORMAT2 NOT = OPPRZT OF POPLLST-P
     ‚*             TEXT ÜBERNEHMEN
                    MOVE     OPTEXT-WRK          TO OPTEXT OF POPLLST-P
     ‚*             PROZENTZAHL ÜBERNEHMEN
                    MOVE     OPPRZT OF FORMAT2   TO OPPRZT OF POPLLST-P
     ‚*             BEI 100% IST DER PUNKT ABGESCHLOSSEN
                    IF   OPPRZT OF POPLLST-P >= 100
                         MOVE "9"                TO OPSTAT OF POPLLST-P
                    ELSE
                         MOVE SPACES             TO OPSTAT OF POPLLST-P
                    END-IF
     ‚*             USER/TERMINAL ETC NEU EINTRAGEN
                    MOVE     1                   TO UPDATE-KZ
           END-IF.

     ‚* NEUE KATEGORIE?
           IF       NEW-OPTYPE-WRK NOT = SPACES
                    MOVE     NEW-OPTYPE-WRK      TO OPTYPE OF POPLLST-P
     ‚*             USER/TERMINAL ETC NEU EINTRAGEN
                    MOVE     1                   TO UPDATE-KZ
           END-IF.

     ‚* SHARE-NUMBER EINTRAGEN
           IF       LFDN-WRK            NOT = ZEROES
            AND     OPSHRN OF POPLLST-P     = ZEROES
     ‚*             TEXT ÜBERNEHMEN
                    MOVE     LFDN-WRK            TO OPSHRN OF POPLLST-P
     ‚*             SOLLTE DER USER NICHT SCHON VOM KOPIERER ERSTELLT
     ‚*             WORDEN SEIN, DANN JETZT SICH SELBST ALS MANAGER
     ‚*             FÜR DIESE EINTRÄGE EITNRAGEN
                    IF   OPSHRM OF POPLLST-P = SPACES
                         MOVE T-USER             TO OPSHRM OF POPLLST-P
                    END-IF
     ‚*             USER/TERMINAL ETC NEU EINTRAGEN
                    MOVE     1                   TO UPDATE-KZ
           END-IF.

     ‚* IMPORTANT WECHSELN?
           IF       CHG-IMPORTANT = 1
                    EVALUATE OPIMPT OF POPLLST-P
                        WHEN 1      MOVE ZEROES  TO OPIMPT OF POPLLST-P
                        WHEN ZEROES MOVE 1       TO OPIMPT OF POPLLST-P
                    END-EVALUATE
     ‚*             USER/TERMINAL ETC NEU EINTRAGEN
                    MOVE     1                   TO UPDATE-KZ
           END-IF.

     ‚* WURDE WAS GEÄNDERT?
           IF       UPDATE-KZ = 1
     ‚*             USER FÜLLEN
                    PERFORM  COPY-GET-TIME
                    MOVE     T-USER              TO OPMSER OF POPLLST-P
                    MOVE     T-TERM              TO OPMBS  OF POPLLST-P
                    MOVE     PGM-WRK             TO OPMPGM OF POPLLST-P
                    MOVE     DATE-8              TO OPMDTA OF POPLLST-P
                    MOVE     TIME-6              TO OPMTIA OF POPLLST-P
     ‚*             SFL NEU ERSTELLEN
                    MOVE     "1"                 TO SFL-CHG
           END-IF.

     ‚* SATZ AKTUALISIEREN
           REWRITE  POPLLST-P.
           IF       OPSHRN OF POPLLST-P NOT = ZEROES
                    CALL "XXP0017" USING OPLFDN OF POPLLST-P
                                         OPSHRN OF POPLLST-P
                    END-CALL
           END-IF.

     ‚* EINTRAG KOPIEREN?
           IF       USER-WRK NOT = SPACES
                    CALL "XXP0018" USING OPLFDN OF POPLLST-P
                                         USER-WRK
                    END-CALL
           END-IF.

       UPD-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEN TEXT ENTSCHLÜSSELN (SOFERN DAS PW STIMMT)
     ‚*--------------------------------------------------------------
       TRY-DECRYPT-TEXT SECTION.
       TRY-DTX-00.

           INITIALIZE RET-CODE.

       TRY-DTX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINSTELLUNGEN FÜR DAS PGM ERMITTELN
     ‚*--------------------------------------------------------------
       GET-CONFIG SECTION.
       GET-CFG-00.

           INITIALIZE COLOR-STRUCT.
     ‚* WURDE EINE VORBELEGUNG IN DER CONFIG HINTERLEGT?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H102.2"      TO CFID   OF CFG-CPY.
           MOVE     T-USER        TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE ZEROES TO CFFK21 OF CFG-CPY
                    MOVE ZEROES TO CFFK22 OF CFG-CPY
                    MOVE ZEROES TO CFFK23 OF CFG-CPY
                    MOVE ZEROES TO CFFK24 OF CFG-CPY
           END-IF.

     ‚* DISPLAY? 0=WINDOW, 1=27X132 (BIG), 2=24X80 (SMALL)
           EVALUATE CFFK21 OF CFG-CPY
                    WHEN ZEROES MOVE     "XXD0015DE "  TO DISPLAY-WRK
                    WHEN 1      MOVE     "XXD0015DEB"  TO DISPLAY-WRK
                    WHEN 2      MOVE     "XXD0015DES"  TO DISPLAY-WRK
           END-EVALUATE.

     ‚* FARBE FÜR DIE SHARE-BEITRÄGE
           EVALUATE CFFK22 OF CFG-CPY
                    WHEN ZEROES MOVE     ATR-PR-RED    TO HEX-COLOR-PR
                                MOVE     ATR-PR-RED-RI TO HEX-COLOR-RI
                    WHEN 1      MOVE     ATR-PR-TRQ    TO HEX-COLOR-PR
                                MOVE     ATR-PR-TRQ-RI TO HEX-COLOR-RI
                    WHEN 2      MOVE     ATR-PR-PNK    TO HEX-COLOR-PR
                                MOVE     ATR-PR-PNK-RI TO HEX-COLOR-RI
           END-EVALUATE.

     ‚* STARTSORTIERUNG
           EVALUATE CFFK23 OF CFG-CPY
                    WHEN ZEROES MOVE     "11"          TO SORT-KZ
                    WHEN 1      MOVE     "12"          TO SORT-KZ
                    WHEN 2      MOVE     "21"          TO SORT-KZ
                    WHEN 3      MOVE     "22"          TO SORT-KZ
           END-EVALUATE.

     ‚* FARBCODES
           IF       CFIN21 OF CFG-CPY(1:24) IS NUMERIC
                    MOVE CFIN21 OF CFG-CPY(1:24) TO COLOR-STRUCT
           END-IF.

     ‚* ANZEIGEMODUS
           MOVE     CFFK24 OF CFG-CPY TO INP-WRK.
           MOVE     ZEROES            TO ANZEIGE-SYSTEM.

     ‚* EINSTELLUNGEN FÜR DIE VERSCHLÜSSELUNG
           MOVE     ZEROES            TO AUTO-DECRYPT.
           MOVE     SPACES            TO CRYPT-PASSWORD.

     ‚* NAME FÜR DIE PERFORMLISTE (MUSS DAS LETZTE SEIN !!!)
           MOVE     "PERFORM" TO FTKEY-PERFORM(1:10).
           MOVE     T-USER    TO FTKEY-PERFORM(11:).
           PERFORM  GET-CONFIG-PERFORM.

       GET-CFG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KONFIGURATION IN EINER MIRC-ÄHNLICHEN "PERFORM-LISTE"
     ‚*--------------------------------------------------------------
       GET-CONFIG-PERFORM SECTION.
       GET-CFP-00.

     ‚* LESEN ALLER VORHANDENEN TEXTE
           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     "POPLLST"         TO FTFILE OF PTXTFIT-P.
           MOVE     FTKEY-PERFORM     TO FTKEY  OF PTXTFIT-P.
           MOVE     ZEROES            TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CFP-90
           END-IF.

       GET-CFP-20.
           READ     PTXTFIT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CFP-90
           END-IF.

     ‚* SCHLÜSSEL OK?
           IF       FTFILE OF PTXTFIT-P NOT = "POPLLST"
            OR      FTKEY  OF PTXTFIT-P NOT = FTKEY-PERFORM
                    GO TO GET-CFP-90
           END-IF.

           IF       FTTEXT OF PTXTFIT-P(1:1) = ":"
                    MOVE    FTTEXT OF PTXTFIT-P TO COMMAND-WRK
                    PERFORM EXECUTE-COMMAND
           END-IF.

           GO TO    GET-CFP-20.

       GET-CFP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSCHLÜSSELTEN TEXT ERMITTELN
     ‚*--------------------------------------------------------------
       GET-ENCRYPTED-TEXT SECTION.
       GET-ECT-00.

     ‚* DEFAULT, WENN NICHTS GEMACHT WIRD:
           MOVE     "<ENCRYPTED>" TO DECRYPT-TEXT.

     ‚* PASSWORT ABFRAGEN WENN DIES NOCH NICHT BEKANNT IST
           IF       (   AUTO-DECRYPT   = 1
                     OR MAN-CALL       = 1)
            AND     CRYPT-PASSWORD     = SPACES
                    PERFORM GET-CRYPT-PASSWORD
           END-IF.

     ‚* SOLL AUTOMATISCH ENTSCHLÜSSELT WERDEN?
           IF       CRYPT-PASSWORD NOT = SPACES
                    PERFORM TRY-DECRYPT-TEXT
                    IF   DECRYPT-TEXT = SPACES
                         MOVE "<DECRYPT_FAILED>" TO DECRYPT-TEXT
                    END-IF
           END-IF.

       GET-ECT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DAS PASSWORT FÜR DIE ENTSCHLÜSSELUNG HOLEN
     ‚*--------------------------------------------------------------
       GET-CRYPT-PASSWORD SECTION.
       GET-CRP-00.

           INITIALIZE RET-CODE.

       GET-CRP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXT VERSCHLÜSSELN
     ‚*--------------------------------------------------------------
       SET-ENCRYPT-TEXT SECTION.
       SET-ETX-00.

           INITIALIZE RET-CODE.

       SET-ETX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXT ENTSCHLÜSSELN
     ‚*--------------------------------------------------------------
       SET-DECRYPT-TEXT SECTION.
       SET-DTX-00.

           INITIALIZE RET-CODE.

       SET-DTX-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
