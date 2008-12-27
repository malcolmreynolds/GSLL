;; Regression test BESSEL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BESSEL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.3971498098638474d0 4.334456411751256d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-J0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.0660433280235491d0 2.1409770694795335d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-J1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3641281458520729d0 3.974061014982464d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-JN 2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.35283402861563773d0 0.12894324947440206d0
                           0.033995719807568436d0 0.007039629755871686d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (CYLINDRICAL-BESSEL-JN-ARRAY 2.0d0 BESARR 2)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.016940739325064968d0 1.8993556609468549d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-Y0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3979257105570999d0 3.1396236150465943d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-Y1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.18202211595348539d0 3.355735727760045d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-YN 3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(-0.6174081041906827d0 -1.127783776840428d0
                           -2.7659432263306014d0 -9.935989128481978d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (CYLINDRICAL-BESSEL-YN-ARRAY 2.0d0 BESARR 2)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 11.301921952136329d0 2.7297681442535893d-14)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-I0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 9.759465153704449d0 1.9210136786427457d-14)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-I1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.3372757784203446d0 8.06056628872663d-15)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-IN 3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.6889484476987382d0 0.21273995923985267d0
                           0.05072856997918024d0 0.009825679323131702d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (CYLINDRICAL-BESSEL-IN-ARRAY 2.0d0 BESARR 2)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.2070019212239867d0 2.241925168997723d-16)
                        (MULTIPLE-VALUE-LIST
                         (CYLINDRICAL-BESSEL-I0-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1787508395024353d0 1.1370197115937822d-16)
                        (MULTIPLE-VALUE-LIST
                         (CYLINDRICAL-BESSEL-I1-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0611243380296663d0 9.334510342661594d-17)
                        (MULTIPLE-VALUE-LIST
                         (CYLINDRICAL-BESSEL-IN-SCALED 3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.09323903330473338d0 0.028791222639470898d0
                           0.006865365386320685d0 0.0013297610941881578d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (CYLINDRICAL-BESSEL-IN-SCALED-ARRAY 2.0d0 BESARR 2)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.011159676085853023d0 2.0424662435034432d-17)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-K0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.01248349888726843d0 1.767412161819488d-17)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-K1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.017401425529487143d0 2.257461693414273d-16)
                        (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-KN 2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6092976692566953d0 3.0340122249326356d-16)
                        (MULTIPLE-VALUE-LIST
                         (CYLINDRICAL-BESSEL-K0-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.681575945185671d0 3.596132979136138d-16)
                        (MULTIPLE-VALUE-LIST
                         (CYLINDRICAL-BESSEL-K1-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9500856418495256d0 1.1481477659153143d-14)
                        (MULTIPLE-VALUE-LIST
                         (CYLINDRICAL-BESSEL-KN-SCALED 2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.2537597545660558d0 0.6473853909486341d0
                           2.1959159274119586d0 9.431049100596468d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (CYLINDRICAL-BESSEL-KN-ARRAY 2.0d0 BESARR 2)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.18920062382698205d0 1.6804391107692678d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-J0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.11611074925915747d0 2.387125482192573d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-J1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.27628368577135015d0 3.680838111259856d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-J2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.22924385795503022d0 7.126330661956055d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-JL 3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(-0.18920062382698202d0 0.11611074925915743d0
                           0.2762836857713501d0 0.22924385795503022d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (SPHERICAL-BESSEL-JL-ARRAY 4.0d0 BESARR)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(-0.18920062382698208d0 0.11611074925915742d0
                           0.27628368577135015d0 0.22924385795503024d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (SPHERICAL-BESSEL-JL-STEED-ARRAY 4.0d0 BESARR)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.16341090521590299d0 1.4513803955642766d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-Y0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.2300533501309578d0 1.5324631572452525d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-Y1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.009129107382315343d0 1.6876604955506113d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-Y2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.009129107382315343d0 1.6876604955506113d-16)
                        (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-YL 2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.16341090521590299d0 0.2300533501309578d0
                           0.009129107382315343d0 -0.21864196590306362d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (SPHERICAL-BESSEL-YL-ARRAY 4.0d0 BESARR)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.12495806717151219d0 5.5492529314587895d-17)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-I0-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.09380241603560975d0 4.165664081928078d-17)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-I1-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.05460625514480487d0 2.425004870012731d-17)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-I2-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.02554459710460367d0 5.842201171222646d-16)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-IL-SCALED 3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.12495806717151212d0 0.09380241603560971d0
                           0.05460625514480483d0 0.02554459710460367d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (SPHERICAL-BESSEL-IL-SCALED-ARRAY 4.0d0 BESARR)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.39269908169872414d0 1.743934249004316d-16)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-K0-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4908738521234052d0 2.1799178112553949d-16)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-K1-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.760854470791278d0 3.378872607445862d-16)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-K2-SCALED 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.02554459710460367d0 5.842201171222646d-16)
                        (MULTIPLE-VALUE-LIST
                         (SPHERICAL-BESSEL-KL-SCALED 3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.39269908169872414d0 0.4908738521234052d0
                           0.760854470791278d0 1.4419419406125027d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (SPHERICAL-BESSEL-KL-SCALED-ARRAY 4.0d0 BESARR)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.43017147387562193d0 7.641380397338472d-16)
                        (MULTIPLE-VALUE-LIST (BESSEL-JNU 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.6713967071418024d0 0.5130161365618323d0
                           0.06500818287738516d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((BESARR
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.0d0 2.0d0 3.0d0))))
                           (SPHERICAL-JNU-ARRAY 0.5d0 BESARR)
                           (CL-ARRAY BESARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.1820221159534852d0 2.020851441225493d-15)
                        (MULTIPLE-VALUE-LIST (BESSEL-YNU 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.3372757784203437d0 1.1856385307923545d-14)
                        (MULTIPLE-VALUE-LIST (BESSEL-INU 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.061124338029666284d0 1.3572329489101316d-16)
                        (MULTIPLE-VALUE-LIST (BESSEL-INU-SCALED 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.029884924416755682d0 1.0617257976532701d-16)
                        (MULTIPLE-VALUE-LIST (BESSEL-KNU 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -3.5104011258456183d0 4.776268519767339d-15)
                        (MULTIPLE-VALUE-LIST (BESSEL-LNKNU 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.6316615870352025d0 5.072223134504136d-15)
                        (MULTIPLE-VALUE-LIST (BESSEL-KNU-SCALED 3.0d0 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 14.930917708487813d0 4.4792753125463437d-14)
                        (MULTIPLE-VALUE-LIST (BESSEL-ZERO-J0 5)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 16.470630050877624d0 3.2941260101755246d-13)
                        (MULTIPLE-VALUE-LIST (BESSEL-ZERO-J1 5)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 17.95981949498783d0 3.591963898997566d-14)
                        (MULTIPLE-VALUE-LIST (BESSEL-ZERO-JNU 2.0d0 5))))

