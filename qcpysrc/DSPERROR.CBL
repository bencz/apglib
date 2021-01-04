     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER GÄNGSTIGEN EINSTELLUNGEN ZUR FEHLERMELDUNG
     ‚*--------------------------------------------------------------
       COPY-DSP-ERR SECTION.
       DSP-ERR-00C.

           MOVE     ERRFLD-WRK        TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO ERRMSG-WRK.
           MOVE     COL-GRN-RI        TO ERRMSG-ATR.

           MOVE     AN                TO IN01 OF INXX.
           MOVE     AN                TO IN02 OF INXX.

       DSP-ERR-90C.
           EXIT.
      /
