     ‚*---------------------------------------------------------------
     ‚* ERMITTELN DATUM UND UHRZEIT
     ‚*---------------------------------------------------------------
       COPY-GET-TIME SECTION.
       CPY-GTM-00C.

           ACCEPT    DATE-8 FROM DATE YYYYMMDD.
           ACCEPT    TIME-8 FROM TIME.

           MOVE      DATE-8(1:2) TO JAHRHUNDERT.
           MOVE      DATE-8(3:6) TO DATUM.
           MOVE      TIME-8(1:6) TO TIME-6.
           MOVE      TIME-6      TO UHRZEIT.

       CPY-GTM-90C.
           EXIT.
      /
