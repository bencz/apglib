     ‚*--------------------------------------------------------------
     ‚* LESEN DER PROGRAMMTEXTE
     ‚*--------------------------------------------------------------
       COPY-PGM-TXT SECTION.
       CPY-TXT-00C.

           CALL     PROCEDURE "CFP8000" USING CPY-TXID
                                              CPY-TXSPR
                                              CPY-GETDEF
                                              CPY-TXTEXT
           END-CALL.

       CPY-TXT-90C.
           EXIT.
      /
