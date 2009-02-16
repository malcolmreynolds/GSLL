;; Regression test MATHIEU for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATHIEU
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7071067811865475d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 0.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7071067811865475d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 0.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.04480018165188903d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 5.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.334848674698019d0 5.927918932160465d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 5.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.007626517570935777d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 10.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.4686604707128563d0 6.522162679768934d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 10.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0019325083152045943d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 15.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.5501081466866489d0 6.883863020442189d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 15.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.037438292241945d-4 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 20.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.609890857395926d0 7.149351588057966d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 20.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.1586301841465958d-4 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 25.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.657510298323475d0 7.360824387008136d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 0 25.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 1 0.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.25654287932236375d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 1 5.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.053598747747176455d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 1 10.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.015040066453826315d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 1 15.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.005051813764713131d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 1 20.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0019110515066575852d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 1 25.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 1 0.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.3374338870223457d0 5.939399581144514d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 1 5.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.4687556641029376d0 6.522585423342775d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 1 10.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.5501150743575518d0 6.883893785481162d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 1 15.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.6098915926037722d0 7.149354853036681d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 1 20.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.6575103983745163d0 7.360824831324015d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 1 25.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 0.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 0.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7352943084006845d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 5.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.7244881519676682d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 5.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.24588834929131909d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 10.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9267592641263209d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 10.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0787928278463933d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 15.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.019966226030262d0 4.529559953915294d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 15.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.028648943147074366d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 20.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.0752932287796875d0 4.77526120325894d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 20.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.011512866330887451d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 25.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.116278953295253d0 4.957274383411439d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 2 25.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 5 0.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1248072506384799d0 4.995147631696639d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 5 5.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.2580199413082875d0 5.586730817112196d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 5 10.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.193432230413072d0 5.29990376213739d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 5 15.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.936575531422621d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 5 20.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6106943100506989d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 5 25.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 5 0.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.906077930202355d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 5 5.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8460384335355104d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 5 10.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8379493400124841d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 5 15.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8635431218533666d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 5 20.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8992683245108409d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 5 25.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 0.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 0.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0259950270894385d0 4.556333208902423d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 5.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.975347487235964d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 5.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0538159921009345d0 4.679883112594638d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 10.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9516453181789555d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 10.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0841063118392213d0 4.814399154181454d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 15.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9285480638845388d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 15.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1177886312593968d0 4.963978700353685d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 20.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9057107845940974d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 20.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1562399186322392d0 5.134736718624918d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 25.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.8826919105636902d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 10 25.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 15 0.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0112937325295657d0 4.491046346053754d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 15 5.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.022878282438181d0 4.54249208220761d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 15 10.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0347936522368735d0 4.595406953797336d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 15 15.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0470843441628868d0 4.649988590456674d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 15 20.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0598004418139373d0 4.706459408038987d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-CE 15 25.0d0 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.0d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 15 0.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9889607027406359d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 15 5.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.978142347183216d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 15 10.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9675137031854539d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 15 15.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9570452540612818d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 15 20.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.9467086958780897d0 4.440892098500626d-16)
                        (MULTIPLE-VALUE-LIST (MATHIEU-SE 15 25.0d0 (/ PI 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.7071067811865475d0 6.123233995736766d-17 -1.0d0
                           -1.8369701987210297d-16 1.0d0
                           3.061616997868383d-16))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY (MATHIEU-CE-ARRAY 0.0d0 (/ PI 2) 6))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(6.03743829223914d-4 0.005051813764712108d0
                           0.02864894314707411d0 0.12611832163429212d0
                           0.43023077677357696d0 0.9365755314225924d0
                           1.2761355598483817d0 1.3076504092104844d0
                           1.2172162968773408d0 1.154852006191031d0
                           1.117788631259397d0 1.0936613176252825d0
                           1.0767085541408374d0 1.0641859681403019d0
                           1.054606963632144d0 1.0470843441628828d0))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY (MATHIEU-CE-ARRAY 20.0d0 0.0d0 16))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(1.6098915926037731d0 3.974439518389252d-16
                           -1.0759417465195853d0 -4.3771386586551907d-16
                           0.8635431218533666d0 4.514819072643059d-16
                           -0.8254327649109297d0 -5.435816907711848d-16
                           0.8843014096898285d0 6.616729635144726d-16
                           -0.921662441765126d0 -7.791496473385796d-16
                           0.9433025704899831d0 8.979725089944524d-16
                           -0.9570452540612865d0))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY (MATHIEU-SE-ARRAY 20.0d0 (/ PI 2) 15 1)))))

