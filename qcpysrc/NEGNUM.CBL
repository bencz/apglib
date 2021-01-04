     ‚*---------------------------------------------------------------
     ‚* PRÜFEN OB DER ÜBERGEBENE GEZONTE WERT NEGATIV IST
     ‚* UND UMSETZUNG IN EINE "NORMALE" NUMMER
     ‚*---------------------------------------------------------------
       COPY-CHECK-NEGATIVITY SECTION.
       CHK-NEG-00C.

     ‚* IST DAS ÜBERGEBENE ZEICHEN NUMERISCH, SO IST DER WERT IN JEDEM
     ‚* FALL POSITIV
           INITIALIZE NEGATIVE-FLAG.
           IF       NEGATIVE-WRK NUMERIC
                    GO TO CHK-NEG-90C
           END-IF.

     ‚* UMSETZEN DES BUCHSTABEN IN DAS ENTSPRECHENDE ZEICHEN
           EVALUATE NEGATIVE-WRK
                    WHEN "ü"
                    WHEN "p"
                         MOVE "0" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "J"
                    WHEN "q"
                         MOVE "1" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "K"
                    WHEN "r"
                         MOVE "2" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "L"
                    WHEN "s"
                         MOVE "3" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "M"
                    WHEN "t"
                         MOVE "4" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "N"
                    WHEN "u"
                         MOVE "5" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "O"
                    WHEN "v"
                         MOVE "6" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "P"
                    WHEN "w"
                         MOVE "7" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "Q"
                    WHEN "x"
                         MOVE "8" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN "R"
                    WHEN "y"
                         MOVE "9" TO NEGATIVE-WRK
                         MOVE 1   TO NEGATIVE-FLAG
                    WHEN OTHER
                         CONTINUE
           END-EVALUATE.


       CHK-NEG-90C.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* UMSETZEN DER ÜBERGEBENEN ZAHL IN DEN ENTSPRECHENDEN NEGATIVEN
     ‚* WERT
     ‚*---------------------------------------------------------------
       COPY-CONVERT-NEGATIVE SECTION.
       CNV-NEG-00C.

     ‚* IST DAS ÜBERGEBENE ZEICHEN NICHT NUMERISCH, SO KANN NICHTS
     ‚* UMGESETZT WERDEN
           IF       NEGATIVE-WRK NOT NUMERIC
                    GO TO CNV-NEG-90C
           END-IF.

     ‚*€   CALL     PROCEDURE "LGUMF16" USING BETRIEBSSYSTEM.

     ‚* JE NACH BETRIEBSYSTEM EINE ANDERE KONVERTIERUNG DURCHFÜHREN
     ‚* iSERIES
           IF       BETRIEBSSYSTEM = "0"
     ‚*             PARAMETER FÜR KONVERTIERUNGSPROGRAMM FÜLLEN
                    EVALUATE NEGATIVE-WRK
                             WHEN "0"
                                  MOVE "ü" TO NEGATIVE-WRK
                             WHEN "1"
                                  MOVE "J" TO NEGATIVE-WRK
                             WHEN "2"
                                  MOVE "K" TO NEGATIVE-WRK
                             WHEN "3"
                                  MOVE "L" TO NEGATIVE-WRK
                             WHEN "4"
                                  MOVE "M" TO NEGATIVE-WRK
                             WHEN "5"
                                  MOVE "N" TO NEGATIVE-WRK
                             WHEN "6"
                                  MOVE "O" TO NEGATIVE-WRK
                             WHEN "7"
                                  MOVE "P" TO NEGATIVE-WRK
                             WHEN "8"
                                  MOVE "Q" TO NEGATIVE-WRK
                             WHEN "9"
                                  MOVE "R" TO NEGATIVE-WRK
                             WHEN OTHER
                                  CONTINUE
                    END-EVALUATE
           ELSE
     ‚* LINUX/WINDOWS
                    EVALUATE NEGATIVE-WRK
                             WHEN "0"
                                  MOVE "p" TO NEGATIVE-WRK
                             WHEN "1"
                                  MOVE "q" TO NEGATIVE-WRK
                             WHEN "2"
                                  MOVE "r" TO NEGATIVE-WRK
                             WHEN "3"
                                  MOVE "s" TO NEGATIVE-WRK
                             WHEN "4"
                                  MOVE "t" TO NEGATIVE-WRK
                             WHEN "5"
                                  MOVE "u" TO NEGATIVE-WRK
                             WHEN "6"
                                  MOVE "v" TO NEGATIVE-WRK
                             WHEN "7"
                                  MOVE "w" TO NEGATIVE-WRK
                             WHEN "8"
                                  MOVE "x" TO NEGATIVE-WRK
                             WHEN "9"
                                  MOVE "y" TO NEGATIVE-WRK
                             WHEN OTHER
                                  CONTINUE
                    END-EVALUATE
           END-IF.

       CNV-NEG-90C.
           EXIT.
      /
