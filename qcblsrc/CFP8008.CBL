       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8008.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZEITUNTERSCHIED ERMITTELN                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  17.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZEITUNTERSCHIED ERMITTELN                    *
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
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8008".

       01  TIME-TYPE IS TYPEDEF.
        05 STD                           PIC 9(2).
        05 MIN                           PIC 9(2).
        05 SEK                           PIC 9(2).

       01  DIFF-DAYS-SEC                 LIKE DIFF-SECS.
       01  STR-TIME-SEC                  LIKE DIFF-SECS.
       01  END-TIME-SEC                  LIKE DIFF-SECS.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-STR-DATE                    LIKE DATE-8.
       01  X-STR-TIME                    TYPE TIME-TYPE.
       01  X-END-DATE                    LIKE DATE-8.
       01  X-END-TIME                    TYPE TIME-TYPE.
       01  X-AKTION                      PIC X(20).
       01  X-RET-VALUE                   LIKE DIFF-SECS.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-STR-DATE
                                X-STR-TIME
                                X-END-DATE
                                X-END-TIME
                                X-AKTION
                                X-RET-VALUE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE DIFF-DAYS-SEC.
     ‚* SOLLTE START- UND ENDDATUM GEFÜLLT SEIN, UNTERSCHIED IN TAGEN
     ‚* ERRECHNEN
           IF       X-STR-DATE NOT = ZEROES
            AND     X-END-DATE NOT = ZEROES
                    PERFORM GET-DIFF-DAYS
           END-IF.

     ‚* JETZT ZEITUNERSCHIED BERECHNEN
           IF       X-STR-TIME NOT = ZEROES
            AND     X-END-TIME NOT = ZEROES
                    PERFORM GET-DIFF-TIME
           END-IF.

     ‚* SOLLTE DIE TAGESGRENZE EINMAL ÜBERSCHRITTEN WORDEN SEIN,
     ‚* DANN MUSS DIESE ZEIT EINMAL VON DER STARTZEIT ABGEZOGEN WERDEN
     ‚* BSP.: START: 235500 = 82800 + 3300 + 0 = 86100 SEKUNDEN
     ‚*       ENDE : 000100 = 0     + 60   + 0 =    60 SEKUNDEN
     ‚* DIFFEREN WÄRE HIER WAS VÖLLIG FALSCHES (WIR MÜSSEN HIER AUF
     ‚* 6 MINUTEN KOMMEN, ALSO 360 SEKUNDEN).
     ‚* EIN TAG HAT 86400 SEKUNDEN. DIESEN ZIEHEN WIR JETZT AB:
     ‚* 86100 - 86400 = -300
           IF       DIFF-DAYS-SEC NOT = ZEROES
                    COMPUTE STR-TIME-SEC =   STR-TIME-SEC
                                           - DIFF-DAYS-SEC
                    END-COMPUTE
           END-IF.

     ‚* JETZT DIE SEKUNDENANZAHL BERECHNEN
           COMPUTE  DIFF-SECS =   (   STR-TIME-SEC
                                    - END-TIME-SEC)
                                * (-1)
           END-COMPUTE.

     ‚* JETZT JE NACH AKTION UMRECHNEN
           EVALUATE X-AKTION
     ‚*             SEKUNDEN
                    WHEN "SEC"
                    WHEN "SEK"
                         MOVE     DIFF-SECS TO X-RET-VALUE
     ‚*             MINUTEN
                    WHEN "MIN"
                         COMPUTE  X-RET-VALUE ROUNDED =   DIFF-SECS
                                                        / 60
                         END-COMPUTE
     ‚*             STUNDEN
                    WHEN "HRS"
                    WHEN "STD"
                         COMPUTE  X-RET-VALUE ROUNDED =   DIFF-SECS
                                                        / 60
                                                        / 60
                         END-COMPUTE
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* UNTERSCHIED IN TAGEN
     ‚*--------------------------------------------------------------
       GET-DIFF-DAYS SECTION.
       GET-DID-00.

           INITIALIZE DIFF-DAYS.
           CALL     "CFP8009" USING X-STR-DATE
                                    X-END-DATE
                                    DIFF-DAYS
           END-CALL.

     ‚* ZEITUNTERSCHIED IN SEKUNDEN BERECHNEN
           COMPUTE  DIFF-DAYS-SEC =   DIFF-DAYS
                                    * 24
                                    * 60
                                    * 60
           END-COMPUTE.

       GET-DID-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEITUNTERSCHIED BERECHNEN
     ‚*--------------------------------------------------------------
       GET-DIFF-TIME SECTION.
       GET-DIT-00.

     ‚* STARTZEIT IN SEKUNDEN
           COMPUTE  STR-TIME-SEC  =   ( STD OF X-STR-TIME * 60 * 60 )
                                    + ( MIN OF X-STR-TIME * 60 )
                                    + ( SEK OF X-STR-TIME )
           END-COMPUTE.

     ‚* ENDZEIT IN SEKUNDEN
           COMPUTE  END-TIME-SEC  =   ( STD OF X-END-TIME * 60 * 60 )
                                    + ( MIN OF X-END-TIME * 60 )
                                    + ( SEK OF X-END-TIME )
           END-COMPUTE.

       GET-DIT-90.
           EXIT.
      /
