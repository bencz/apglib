     ‚*--------------------------------------------------------------
     ‚* LESEN EINES LOG-EINTRAGS
     ‚*--------------------------------------------------------------
       COPY-READ-LOG-CPY SECTION.
       REA-LOG-00C.

           CALL     "CFP0018" USING LOG-CPY
           END-CALL.

       REA-LOG-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES LOG-EINTRAGS
     ‚*--------------------------------------------------------------
       COPY-WRITE-LOG-CPY SECTION.
       WRI-LOG-00C.

           CALL     "CFP0019" USING LOG-CPY
                                    PGM-WRK
           END-CALL.

       WRI-LOG-90C.
           EXIT.
      /
