             CMD        PROMPT('MENÜ AUFRUFEN')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('MENU') RSTD(*NO) MIN(0)
             PARM       KWD(CALLPARM) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Menü')
