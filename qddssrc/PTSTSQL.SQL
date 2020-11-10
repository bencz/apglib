CREATE TABLE apg/ptstsql ( atpaln DECIMAL(9) NOT NULL,
                           atpalp DECIMAL(3) NOT NULL,
                           atatrk CHAR(50) NOT NULL,
                           atatrv CHAR(50) NOT NULL,
                           PRIMARY KEY (atpaln, atpalp, atatrk)
                         )

CREATE INDEX apg/ptstsqll1 on apg/ptstsql
                         ( atatrk, atatrv, atpalp, atpaln )
CREATE INDEX apg/ptstsqll2 on apg/ptstsql
                         ( atpaln, atpalp )
