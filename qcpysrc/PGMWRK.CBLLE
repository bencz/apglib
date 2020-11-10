     ‚*--------------------------------------------------------------
     ‚* PGM-INIT
     ‚*--------------------------------------------------------------
       COPY-PGM-INIT SECTION.
       CPY-INI-00C.

     ‚* ERMITTELN JOB-INFORMATIONEN
           MOVE     "INIT" TO COPY-AKTION.
           CALL     PROCEDURE "CFP9000" USING COPY-AKTION
                                              JOBINFO
                                              PGM-WRK
           END-CALL.

     ‚* SPRACHE ERSTMAL FIX SETZEN, WENN NICHT ERMITTELT
           IF       T-SPRACHE OF JOBINFO = SPACES
                    MOVE "DEU"                TO SPRACHE-WRK
           ELSE
                    MOVE T-SPRACHE OF JOBINFO TO SPRACHE-WRK
           END-IF.
     ‚* KOMPILIERUNGSDATUM SETZEN
           MOVE     FUNCTION WHEN-COMPILED TO SYS-COMPIL.

     ‚* PROGRAMMAUFRUF LOGGEN
           PERFORM  COPY-PGM-LOG.
           PERFORM  COPY-CHK-LIB.

     ‚* EXTERNALS ÜBERNEHMEN
           MOVE     PGM-WRK   TO EXTERNAL-PGM.
           IF       EXTERNAL-DEPTH IS NUMERIC
                    ADD      1         TO EXTERNAL-DEPTH
           ELSE
                    MOVE     1         TO EXTERNAL-DEPTH
           END-IF.

       CPY-INI-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PGM-EXIT
     ‚*--------------------------------------------------------------
       COPY-PGM-EXIT SECTION.
       CPY-EXT-00C.

           MOVE     "EXIT" TO COPY-AKTION.
           CALL     PROCEDURE "CFP9000" USING COPY-AKTION
                                              JOBINFO
                                              PGM-WRK
           END-CALL.

     ‚* PROGRAMMAUFRUF LOGGEN
           PERFORM  COPY-PGM-LOG.
           IF       EXTERNAL-DEPTH IS NUMERIC
                    SUBTRACT 1       FROM EXTERNAL-DEPTH
           END-IF.

       CPY-EXT-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SETZEN EINES PROGRAMMLOGS
     ‚*--------------------------------------------------------------
       COPY-PGM-LOG SECTION.
       CPY-PLG-00C.

     ‚* SICH SELBST NICHT LOGGEN
           IF       PGM-WRK NOT = "CFP9006"
                    CALL "CFP9006" USING PGM-WRK
                                         T-USER
                                         COPY-AKTION
                    END-CALL
           END-IF.

       CPY-PLG-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PGM-INIT (PERFORMANCE-SPAREND NUR MIT NÖTIGSTEN LOG-AUSGABEN)
     ‚*--------------------------------------------------------------
       COPY-PGM-INIT-SIMPLE SECTION.
       CPY-ISP-00C.

     ‚* EXTERNALS ÜBERNEHMEN
           MOVE     PGM-WRK   TO EXTERNAL-PGM.
           IF       EXTERNAL-DEPTH IS NUMERIC
                    ADD      1         TO EXTERNAL-DEPTH
           END-IF.

       CPY-ISP-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PGM-EXIT (PERFORMANCE-SPAREND NUR MIT NÖTIGSTEN LOG-AUSGABEN)
     ‚*--------------------------------------------------------------
       COPY-PGM-EXIT-SIMPLE SECTION.
       CPY-ESP-00C.

           IF       EXTERNAL-DEPTH IS NUMERIC
                    SUBTRACT 1       FROM EXTERNAL-DEPTH
           END-IF.

       CPY-ESP-90C.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB AKTUELLES OBJEKT IM ZUGRIFF
     ‚*--------------------------------------------------------------
       COPY-CHK-LIB SECTION.
       CPY-CLB-00C.

           MOVE     ZEROES TO ACT-DEPTH.
           MOVE     SPACES TO ACTLIB-WRK, ACTPGM-WRK.
           CALL     "CFP9007" USING PGM-WRK
                                    ACT-DEPTH
                                    ACTLIB-WRK
                                    ACTPGM-WRK
           END-CALL.
           IF       ACTPGM-WRK NOT = PGM-WRK
            OR      ACTLIB-WRK NOT = CONST-LIB
                    CALL "CFP9008" USING PGM-WRK
                                         CONST-LIB
                                         ACTPGM-WRK
                                         ACTLIB-WRK
                    END-CALL
           END-IF.

       CPY-CLB-90C.
           EXIT.
      /
