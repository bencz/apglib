     ‚*--------------------------------------------------------------
     ‚* UMSETZEN EINES DATUMS
     ‚*--------------------------------------------------------------
       COPY-CONVERT-DATE SECTION.
       CPY-CVD-00C.

           CALL     "CFP9005" USING DATUM-ALPHA-10
                                    DATUM-ALPHA-8
                                    DATE-DB
                                    DATE-DB-6
                                    DATUM-CONVERT
           END-CALL.
           MOVE     SPACES TO DATUM-CONVERT.

       CPY-CVD-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* UMSETZEN EINER UHRZEIT
     ‚*--------------------------------------------------------------
       COPY-CONVERT-TIME SECTION.
       CPY-CVT-00C.

           CALL     "CFP9011" USING TIME-ALPHA-8
                                    TIME-DB
                                    TIME-CONVERT
           END-CALL.
           MOVE     SPACES TO TIME-CONVERT.

       CPY-CVT-90C.
           EXIT.
      /
