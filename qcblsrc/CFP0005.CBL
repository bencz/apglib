       IDENTIFICATION DIVISION.
      *
         PROGRAM-ID.              CFP0005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
      *
      *---------------------------------------------------------------*
      * DSPMAIN                                                       *
      *---------------------------------------------------------------*
      *                                                               *
      * AUTOR         :  A. PIEGER                                    *
      *                                                               *
      * ERSTELLT AM   :  05.02.2010                                   *
      *                                                               *
      * FUNKTION      :  OVERRIDE FÜR DSPMAIN                         *
      *                                                               *
      * ÄNDERUNGEN:                                                   *
      * DATUM      VON   GRUND DER ÄNDERUNG                           *
      *                                                               *
      *****************************************************************
      *
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
      * STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

      * STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

      * WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "CFP0005".
       01  PGM-TYP                        PIC X(8)  VALUE "CONFIG".

      * PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                TYPE CHKOBJ-TYPE.

       01  TRY-COUNTER                    PIC 9(4).
       01  POS                            PIC 9(2).

      * SCHALTER
       01  DISPLAY-EXIST                  PIC 9(1).
      *-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-DISPLAY                 LIKE DISPLAY-WRK.
       01  X-LIB                          PIC X(10).
       01  X-SPRACHE                 LIKE SPRACHE-WRK.
      *-------------------------------------------------------------
       PROCEDURE DIVISION USING X-DISPLAY
                                X-LIB
                                X-SPRACHE.
      *-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           MOVE     X-SPRACHE TO SPRACHE-WRK.
           PERFORM  OVERRIDE-DISPLAY.

       ENDE.
           GOBACK.
      /
      *--------------------------------------------------------------
      * OVERRIDE DES DISPLAYS
      *--------------------------------------------------------------
       OVERRIDE-DISPLAY SECTION.
       OVR-DSP-00.

           INITIALIZE TRY-COUNTER.
           IF       X-DISPLAY(9:2) = "DE"
                    MOVE 9 TO POS
           ELSE
                    MOVE 8 TO POS
           END-IF.

       OVR-DSP-10.
           EVALUATE SPRACHE-WRK
               WHEN "DEU" MOVE "DE" TO X-DISPLAY(POS:2)
               WHEN "ENU" MOVE "EN" TO X-DISPLAY(POS:2)
               WHEN OTHER MOVE "DE" TO X-DISPLAY(POS:2)
           END-EVALUATE.

      * PRÜFEN OB ES DAS DISPLAY GIBT
           PERFORM  CHECK-DISPLAY-EXIST.
           IF       DISPLAY-EXIST = ZEROES
                    ADD 1 TO TRY-COUNTER
                    EVALUATE TRY-COUNTER
      *                 BEIM ERSTEN MAL PRÜFEN OB DAS DEUTSCHE
      *                 DISPLAY GEÖFFNET WERDEN KANN
                        WHEN 1
                             MOVE "DEU" TO SPRACHE-WRK
                             GO TO OVR-DSP-10
      *                 KEIN WEITERER VERSUCH UND MIT FEHLER BEENDEN
                        WHEN OTHER
                             GO TO OVR-DSP-90
                    END-EVALUATE
           END-IF.

           INITIALIZE RET-CODE.
      * DISPLAYFILE CFD0000DE MIT DEM NEUEN DISPLAY ÜBERSCHREIBEN
           INITIALIZE DSPOVRDSPF-REC.
           MOVE     "CFD0000DE"    TO FROMFILE OF DSPOVRDSPF-REC.
           MOVE     X-DISPLAY      TO TOFILE   OF DSPOVRDSPF-REC.
           MOVE     X-LIB          TO TOLIB    OF DSPOVRDSPF-REC.
           CALL     "CLP0055" USING DSPOVRDSPF-REC
                                    RET-CODE
           END-CALL.

       OVR-DSP-90.
           EXIT.
      /
      *--------------------------------------------------------------
      * DISPLAYOBJEKT PRÜFEN
      *--------------------------------------------------------------
       CHECK-DISPLAY-EXIST SECTION.
       CHK-DEX-00.

      * PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-LIB        TO LIB     OF CHKOBJ-REC.
           MOVE     X-DISPLAY    TO OBJ     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE ZEROES TO DISPLAY-EXIST
           ELSE
                    MOVE 1      TO DISPLAY-EXIST
           END-IF.

       CHK-DEX-90.
           EXIT.
      /
