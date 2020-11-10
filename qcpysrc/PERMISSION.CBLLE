     ‚*--------------------------------------------------------------
     ‚* BERECHTIGUNGSPRÜFUNG
     ‚*--------------------------------------------------------------
       COPY-PERMISSION SECTION.
       CPY-PER-00C.

     ‚* WIR HATTEN MAL DEN QSECOFR ALS GAST, WENN SICH DAS WIEDERHOLT
     ‚* DANN EIN KLEINES WILLKOMMENSWINDOW ANZEIGEN
           IF       T-USER(1:7) = "QSECOFR"
                    CALL "CFP9112" USING T-USER
                                         T-TERM
                    END-CALL
     ‚*             PROGRAMMLAUF BEENDEN
                    GOBACK
           END-IF.

           MOVE     SPACES TO RET-CODE.
           CALL     "CFP9001" IN LIBRARY "APG"
                              USING T-USER
                                    PGM-WRK
                                    PGM-TYP
                                    RET-CODE
           END-CALL.

     ‚* BÖSE ABER NOTWENDIG ...
           IF       RET-CODE = "4"
                    GO TO ENDE
           END-IF.
           IF       RET-CODE = "5"
                    GOBACK
           END-IF.

       CPY-PER-90C.
           EXIT.
      /
