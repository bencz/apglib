     ‚*--------------------------------------------------------
     ‚* AUFRUF BILDSCHIRM ÖFFNEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-OPEN SECTION.
       DSP-OPN-00C.

           INITIALIZE DSP-ATTR.
           INITIALIZE REOPEN-DISPLAY-WRK.
     ‚* DAS DISPLAY ERSTMAL ÜBERSCHREIBEN
           PERFORM  COPY-DISPLAY-OVERRIDE.

     ‚* WSREC WIRD HIER NICHT BENÖTIGT
           MOVE     SPACES TO WSREC-WRK.
           INITIALIZE DSP-ATTR.

     ‚* JETZT DIE DSPMAIN AUFRUFEN
           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-OPEN
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.

       DSP-OPN-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF BILDSCHIRM SCHLIESSEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-CLOSE SECTION.
       DSP-CLS-00C.

     ‚* WSREC WIRD HIER NICHT BENÖTIGT
           MOVE     SPACES TO WSREC-WRK.
           INITIALIZE DSP-ATTR.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-CLOSE
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.

     ‚* JETZT EVTL VORHANDENEN OVR WIEDER LÖSCHEN.
     ‚* DIESE KÖNNEN NUR EXISTIEREN, WENN STATT EINEM FREMDSPRACHEN
     ‚* BILDSCHIRM DER DEUTSCHE ANGEZEIGT WURDE.
           IF       1 = 2
                    CALL     "CLP0012" USING DISPLAY-WRK
                    END-CALL
           END-IF.

       DSP-CLS-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DISPLAY SCHLIEßEN UND NEU ÖFFNEN
     ‚*--------------------------------------------------------------
       COPY-DISPLAY-REOPEN SECTION.
       DSP-REO-00C.

           PERFORM  COPY-DISPLAY-CLOSE.
           IF       REOPEN-DISPLAY-WRK NOT = SPACES
                    MOVE REOPEN-DISPLAY-WRK TO DISPLAY-WRK
           END-IF.
           PERFORM  COPY-DISPLAY-OPEN.

       DSP-REO-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF BILDSCHIRM SCHREIBEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-WRITE SECTION.
       DSP-WRT-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-WRITE
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.
           INITIALIZE DSP-ATTR.

       DSP-WRT-00C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF SFL-BILDSCHIRM SCHREIBEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-SFLWRITE SECTION.
       DSP-WRS-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-SFLWRITE
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.
           INITIALIZE DSP-ATTR.

       DSP-WRS-00C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF SFL-BILDSCHIRM LESEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-SFLREWRITE SECTION.
       DSP-SRW-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-SFLREWRITE
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.
           INITIALIZE DSP-ATTR.

       DSP-SRW-00C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF BILDSCHIRM LESEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-READ SECTION.
       DSP-RED-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-READ
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.

       DSP-RED-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF BILDSCHIRM LESEN (OHNE DATEN)
     ‚*--------------------------------------------------------
       COPY-DISPLAY-READ-NDTA SECTION.
       DSP-RND-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-READ-NDTA
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.

       DSP-RND-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF SFL-BILDSCHIRM LESEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-SFLREAD SECTION.
       DSP-SRD-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-SFLREAD
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.
           INITIALIZE DSP-ATTR.

       DSP-SRD-00C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF SFL-BILDSCHIRM LESEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-SFLREADREC SECTION.
       DSP-SRD-00C.

           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-SFLREADREC
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.
           INITIALIZE DSP-ATTR.

       DSP-SRD-00C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* AUFRUF CURSOR-POSITION BESTIMMEN
     ‚*--------------------------------------------------------
       COPY-DISPLAY-CURSER SECTION.
       DSP-CSR-00C.

           INITIALIZE WSREC-WRK.
           CALL     LINKAGE TYPE IS PROCEDURE "CFP0000" USING WSREC-WRK
                                              FMT-WRK
                                              DSP-CURSER
                                              DISPLAY-WRK
                                              PGM-WRK
                                              PGM-TYP
                                              SPRACHE-WRK
                                              SYS-COMPIL
                                              SUBFILE-RECORD-NUMBER
                                              BS-STAT
                                              BS-ATR
                                              BS-IO-AREA
                                              DSP-ATTR
           END-CALL.

     ‚* BYTE ORDER PRÜFEN
           MOVE     1 TO WORK-BIN.
           IF       WORK-BIN-CHAR2 > X"00"
                    MOVE ZEROES TO ENDIAN-MODE
           ELSE
                    MOVE 1      TO ENDIAN-MODE
           END-IF.

     ‚* JE NACH BYTEORDER DIE WERTE ANDERS BEFÜLLEN
           IF       ENDIAN-MODE = ZEROES
                    MOVE IOF-DSP-CURSER-LINE OF BS-IO-AREA TO
                         WORK-BIN-CHAR2
                    MOVE IOF-DSP-CURSER-COL  OF BS-IO-AREA TO
                         WORK-BIN-CHAR2
                    MOVE WORK-BIN TO CURSER-SPALTE
                    MOVE WORK-BIN TO CURSER-ZEILE
           ELSE
                    MOVE IOF-DSP-CURSER-LINE OF BS-IO-AREA TO
                         WORK-BIN-CHAR1
                    MOVE IOF-DSP-CURSER-COL  OF BS-IO-AREA TO
                         WORK-BIN-CHAR1
                    MOVE WORK-BIN TO CURSER-SPALTE
                    MOVE WORK-BIN TO CURSER-ZEILE
           END-IF.

     ‚* BILDSCHIRMZEILE
           MOVE     IOF-SFL-LOW-NBR OF BS-IO-AREA TO SFL-ZEILE.
           IF       SFL-ZEILE = ZEROES
                    MOVE  1 TO SFL-ZEILE
           END-IF.

           MOVE     SFL-ZEILE TO SUBFILE-RECORD-NUMBER.

        DSP-CSR-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGES IN DIE DSP-ATR-TAB
     ‚*--------------------------------------------------------
     ‚*€ADD-ATR-TAB SECTION.
     ‚*€ADD-ATR-00.
     ‚*€
     ‚*€   IF       DSP-ATR-FLD = SPACES
     ‚*€            MOVE "DSP-ATR-FLD IS EMPTY" TO LGPGM541-TXT
     ‚*€            CALL "LGPGM541" USING LGPGM541-TXT
     ‚*€            GO TO ADD-ATR-90
     ‚*€   END-IF.
     ‚*€
     ‚*€   ADD      1 TO DSP-ATR-POS.
     ‚*€
     ‚*€   MOVE     DSP-ATR-FLD TO DSI-ATR-FLD(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-COL TO DSI-ATR-COL(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-RI  TO DSI-ATR-RI(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-HI  TO DSI-ATR-HI(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-PR  TO DSI-ATR-PR(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-ND  TO DSI-ATR-ND(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-UL  TO DSI-ATR-UL(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-CS  TO DSI-ATR-CS(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-BL  TO DSI-ATR-BL(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-VAL TO DSI-ATR-VAL(DSP-ATR-POS).
     ‚*€   MOVE     DSP-ATR-POS TO DSI-ATR-NUM(DSP-ATR-POS).
     ‚*€   IF       DSP-ATR-FLD1 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD1 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD2 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD2 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD3 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD3 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD4 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD4 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD5 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD5 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD6 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD6 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD7 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD7 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD8 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD8 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ATR-FLD9 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ATR-POS
     ‚*€            MOVE DSP-ATR-FLD9 TO DSI-ATR-FLD(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-COL  TO DSI-ATR-COL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-RI   TO DSI-ATR-RI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-HI   TO DSI-ATR-HI(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-PR   TO DSI-ATR-PR(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-ND   TO DSI-ATR-ND(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-UL   TO DSI-ATR-UL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-CS   TO DSI-ATR-CS(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-BL   TO DSI-ATR-BL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-VAL  TO DSI-ATR-VAL(DSP-ATR-POS)
     ‚*€            MOVE DSP-ATR-POS  TO DSI-ATR-NUM(DSP-ATR-POS)
     ‚*€   END-IF.
     ‚*€
     ‚*€   INITIALIZE DSP-ATR-TAB.
     ‚*€
     ‚*€ADD-ATR-90.
     ‚*€   EXIT.
     ‚*€
     ‚*--------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGES IN DIE DSP-VAR-TAB
     ‚*--------------------------------------------------------
     ‚*€ADD-VAR-TAB SECTION.
     ‚*€ADD-VAR-00.
     ‚*€
     ‚*€   ADD      1               TO DSP-VAR-POS.
     ‚*€   MOVE     DSP-VAR-TXT     TO DSI-VAR-TXT(DSP-VAR-POS).
     ‚*€   MOVE     DSP-FILL-SPACES TO DSI-FILL-SPACES(DSP-VAR-POS).
     ‚*€
     ‚*€   INITIALIZE DSP-VAR-TAB.
     ‚*€
     ‚*€ADD-VAR-90.
     ‚*€   EXIT.
     ‚*€
     ‚*--------------------------------------------------------
     ‚* HINZUFÜGEN EINES EINTRAGES IN DIE DSP-ERR-TAB
     ‚*--------------------------------------------------------
     ‚*€ADD-ERR-TAB SECTION.
     ‚*€ADD-ERR-00.
     ‚*€
     ‚*€   IF       DSP-ERR-FLD = SPACES
     ‚*€            MOVE "DSP-ERR-FLD IS EMPTY" TO LGPGM541-TXT
     ‚*€            CALL "LGPGM541" USING LGPGM541-TXT
     ‚*€            GO TO ADD-ERR-90
     ‚*€   END-IF.
     ‚*€
     ‚*€   ADD      1 TO DSP-ERR-POS.
     ‚*€
     ‚*€   MOVE     DSP-ERR-FLD     TO DSI-ERR-FLD(DSP-ERR-POS).
     ‚*€   IF       DSP-ERR-FLD1 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD1 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD2 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD2 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD3 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD3 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD4 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD4 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD5 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD5 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD6 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD6 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD7 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD7 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD8 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD8 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€   IF       DSP-ERR-FLD9 NOT = SPACES
     ‚*€            ADD  1 TO DSP-ERR-POS
     ‚*€            MOVE DSP-ERR-FLD9 TO DSI-ERR-FLD(DSP-ERR-POS)
     ‚*€   END-IF.
     ‚*€
     ‚*€   INITIALIZE DSP-ERR-TAB.
     ‚*€
     ‚*€ADD-ERR-90.
     ‚*€   EXIT.
     ‚*€
     ‚*--------------------------------------------------------
     ‚* ERR-REC STRUKTUR AN DSPMAIN ÜBERGEBEN
     ‚*--------------------------------------------------------
     ‚*€ERR-REC-TO-DSP SECTION.
     ‚*€ERR-TDS-00.
     ‚*€
     ‚*€   IF       ERR-ID OF ERR-REC = SPACES
     ‚*€            GO TO ERR-TDS-90
     ‚*€   END-IF.
     ‚*€
     ‚*€   MOVE     ERR-ID    OF ERR-REC     TO DSP-ERR-ID.
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(01) TO DSI-VAR-TAB(01).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(02) TO DSI-VAR-TAB(02).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(03) TO DSI-VAR-TAB(03).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(04) TO DSI-VAR-TAB(04).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(05) TO DSI-VAR-TAB(05).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(06) TO DSI-VAR-TAB(06).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(07) TO DSI-VAR-TAB(07).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(08) TO DSI-VAR-TAB(08).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(09) TO DSI-VAR-TAB(09).
     ‚*€   MOVE     VAR-TAB-I OF ERR-REC(10) TO DSI-VAR-TAB(10).
     ‚*€   MOVE     VAR-MAX OF ERR-REC TO DSP-VAR-POS.
     ‚*€
     ‚*€   INITIALIZE FLD-MAX OF ERR-REC.
     ‚*€   PERFORM  25 TIMES
     ‚*€            ADD 1 TO FLD-MAX OF ERR-REC
     ‚*€            IF  FLD-I OF ERR-REC(FLD-MAX OF ERR-REC) > SPACES
     ‚*€                MOVE FLD-I OF ERR-REC(FLD-MAX OF ERR-REC) TO
     ‚*€                     DSP-ERR-FLD
     ‚*€                PERFORM ADD-ERR-TAB
     ‚*€            END-IF
     ‚*€   END-PERFORM.
     ‚*€
     ‚*€   INITIALIZE ERR-REC.
     ‚*€
     ‚*€ERR-TDS-90.
     ‚*€   EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DAS DISPLAY ÜBERSCHREIBEN
     ‚*--------------------------------------------------------------
       COPY-DISPLAY-OVERRIDE SECTION.
       DSP-OVR-00C.

           INITIALIZE RET-CODE.
     ‚* DISPLAYFILE CFD0000DE MIT DEM NEUEN DISPLAY ÜBERSCHREIBEN
           CALL     "CFP0005" USING DISPLAY-WRK
                                    CONST-LIB
                                    SPRACHE-WRK
           END-CALL.

       DSP-OVR-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ADD-ERR-TAB AUSFÜHREN
     ‚*--------------------------------------------------------------
       COPY-ADD-ERR-TAB SECTION.
       DSP-ADE-00C.

     ‚* DAS CFP0002 VERWALTET DIE INTERNE TABELLE
           MOVE     "ADD-ERR-TAB" TO DSP-AKTION.
           CALL     PROCEDURE "CFP0002" USING DSP-ATTR
                                              DSP-AKTION
           END-CALL.

       DSP-ADE-90C.
           EXIT.
      /
