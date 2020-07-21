       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFPIN ASSIGN TO RFPIN.
           SELECT PROPOSAL ASSIGN TO PROPOSAL.
       DATA DIVISION.
       FILE SECTION.
       FD  RFPIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFPIN-REC.
       01  RFPIN-REC                   PIC X(80).
      *
       FD  PROPOSAL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PROPOSAL-REC.
       01  PROPOSAL-REC                PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01 RFPIN-EOF                    PIC X(01) VALUE SPACE.
           88  END-OF-FILE             VALUE 'Y'.
      *
       01 RFP-REC.
           05  ARTIST-ACCT-NO          PIC X(08).
           05  ARTIST-MUSICAL-GENRE    PIC X(06).
                88 ROCK                VALUE 'ROCK'.
                88 JAZZ                VALUE 'JAZZ'.
                88 FUSION              VALUE 'FUSION'.
           05  MUSICIAN.
                10  MUSICIAN-LNAME     PIC X(15).
                10  MUSICIAN-FNAME     PIC X(15).
           05  INSTRUMENT-TYPE         PIC X(06).
                88  KEYBOARD           VALUE 'KEYS'.
                88  VOCALS             VALUE 'VOCALS'.
                88  GUITAR             VALUE 'GUITAR'.
                88  BASS               VALUE 'BASS'.
                88  DRUMS              VALUE 'DRUMS'.
                88  PERCUSSION         VALUE 'PERC'.
           05  INSTRUMENT-QUALITY      PIC X(01).
                88 USED-FLAG           VALUE 'U'.
                88 NEW-FLAG            VALUE 'N'.
                88 PREMIUM-FLAG        VALUE 'P'.
           05  MAX-MUSICIAN-BUDGET-AMT PIC 9(5)V99.
           05  SHIP-TO                 PIC X(03).
                88 IN-COUNTRY          VALUE 'IN'.
                88 OUT-OF-COUNTRY      VALUE 'OUT'.
           05  FILLER                  PIC X(19).
      *
       01  PROP-OUT-1.
           05  ARTIST-ACCT-NO-OUT      PIC X(08).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  ARTIST-GENRE-OUT        PIC X(06).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  LNAME-OUT               PIC X(15).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FNAME-OUT               PIC X(15).
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  INSTRUMENT-TYPE-OUT     PIC X(06).
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  INSTRUMENT-QUALITY-OUT  PIC X(01).
           05  FILLER                  PIC X(07) VALUE SPACES.
           05  SHIP-TO-OUT             PIC X(03).
           05  FILLER                  PIC X(02).
       01  PROP-OUT-2.
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  MAX-BUDGET-AMT-OUT      PIC $$$,999.99.
           05  FILLER                  PIC X(03) VALUE SPACES.
           05  INSTR-PRICE-OUT         PIC $$$,999.99.
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  QUALITY-ADJ-OUT         PIC $$,999.99-.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  TAX-AMT-OUT             PIC $$99.99.
           05  FILLER                  PIC X(08) VALUE SPACES.
           05  SHIP-COST-OUT           PIC $$99.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  TOTAL-COST-OUT          PIC $$$,999.99.
           05  FILLER                  PIC X(04) VALUE SPACES.
      *
       01  HEADER-1.
           05  FILLER         PIC X(05) VALUE 'DATE '.
           05  HDR-YR         PIC 9(04).
           05  FILLER         PIC X(01) VALUE '/'.
           05  HDR-MO         PIC 9(02).
           05  FILLER         PIC X(01) VALUE '/'.
           05  HDR-DAY        PIC 9(02).
           05  FILLER         PIC X(16) VALUE SPACES.
           05  FILLER         PIC X(18)
                              VALUE 'GLENN''S MUSIC BARN'.
           05  FILLER         PIC X(31) VALUE SPACES.
       01  HEADER-2.
           05  FILLER         PIC X(30) VALUE SPACES.
           05  FILLER         PIC X(20)
                              VALUE 'REQUEST FOR PROPOSAL'.
           05  FILLER         PIC X(30) VALUE SPACES.
       01  HEADER-3.
           05  FILLER         PIC X(10) VALUE 'ACCT NO   '.
           05  FILLER         PIC X(08) VALUE 'GENRE   '.
           05  FILLER         PIC X(17) VALUE 'MUSICIAN LAST    '.
           05  FILLER         PIC X(17) VALUE 'MUSICIAN FIRST   '.
           05  FILLER         PIC X(12) VALUE 'INSTRUMENT  '.
           05  FILLER         PIC X(09) VALUE 'QUALITY  '.
           05  FILLER         PIC X(07) VALUE 'SHIP TO'.
       01  HEADER-4.
           05  FILLER         PIC X(07) VALUE ALL '-'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(15) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(15) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE ALL '-'.
       01  HEADER-5.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(12) VALUE 'MAX BUDGET  '.
           05  FILLER         PIC X(13) VALUE 'INSTR PRICE  '.
           05  FILLER         PIC X(11) VALUE 'QUALITY ADJ'.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE 'TAX'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(15) VALUE 'SHIPPING COST  '.
           05  FILLER         PIC X(14) VALUE 'TOTAL COST    '.
       01  HEADER-6.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE ALL '-'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(13) VALUE ALL '-'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE ALL '-'.
           05  FILLER         PIC X(04) VALUE SPACES.
      *
       01  BLANK-LINE.
           05  FILLER         PIC X(80) VALUE SPACES.
      *
       01  TRAILER-1.
           05  FILLER         PIC X(19) VALUE 'QUALITY ADJUSTMENT:'.
           05  FILLER         PIC X(61) VALUE SPACES.
       01  TRAILER-2.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(33)
               VALUE 'USED - 20% OFF THE STANDARD PRICE'.
           05  FILLER         PIC X(42) VALUE SPACES.
       01  TRAILER-3.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(42)
               VALUE 'PREMIUM - 20% MORE THAN THE STANDARD PRICE'.
           05  FILLER         PIC X(33) VALUE SPACES.
       01  TRAILER-4.
           05  FILLER         PIC X(14) VALUE 'TAX RATE IS 8%'.
           05  FILLER         PIC X(66) VALUE SPACES.
       01  TRAILER-5.
           05  FILLER         PIC X(14) VALUE 'SHIPPING COST:'.
           05  FILLER         PIC X(66) VALUE SPACES.
       01  TRAILER-6.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(16) VALUE 'IN COUNTRY - 10%'.
           05  FILLER         PIC X(59) VALUE SPACES.
       01  TRAILER-7.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(20) VALUE 'OUT OF COUNTRY - 20%'.
           05  FILLER         PIC X(55) VALUE SPACES.
      *
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
           10  WS-CURRENT-SECOND           PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
      *
       01  WS-CALC-FIELDS.
           05  WS-INSTR-PRICE              PIC 9(05)V99.
           05  WS-ADJ-PRICE                PIC 9(05)V99.
           05  WS-TAX                      PIC 9(03)V99.
           05  WS-SHIPPING                 PIC 9(03)V99.
           05  WS-QUAL-ADJ                 PIC S9(03)V99.
           05  WS-TOTAL-COST               PIC 9(05)V99.
      *
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL END-OF-FILE.
           PERFORM 800-WRITE-TRAILERS.
           PERFORM 600-CLOSE-FILES.
           GOBACK.
      *
       000-HOUSEKEEPING.
      * Initialization Routine
           INITIALIZE RFP-REC, PROP-OUT-1, PROP-OUT-2.
           INITIALIZE WS-CALC-FIELDS.
           PERFORM 300-OPEN-FILES.
           PERFORM 700-WRITE-HEADERS.
           PERFORM 400-READ-RFP.
       100-MAIN.
           PERFORM 200-PROCESS-DATA.
           PERFORM 250-MOVE-DATA.
           PERFORM 500-WRITE-DETAIL.
           PERFORM 400-READ-RFP.
       200-PROCESS-DATA.
      * Calculate Instrument Price
           EVALUATE TRUE
           WHEN KEYBOARD
                MOVE 3017.89 TO WS-INSTR-PRICE
           WHEN VOCALS
                MOVE 599.05 TO WS-INSTR-PRICE
           WHEN GUITAR
                MOVE 2648.99 TO WS-INSTR-PRICE
           WHEN BASS
                MOVE 1876.10 TO WS-INSTR-PRICE
           WHEN DRUMS
                MOVE 3087.22 TO WS-INSTR-PRICE
           WHEN PERCUSSION
                MOVE 799.99 TO WS-INSTR-PRICE
           END-EVALUATE.
      * Calculate Instrument Quality Adjusted Price
           EVALUATE TRUE
           WHEN USED-FLAG
                COMPUTE WS-QUAL-ADJ ROUNDED = WS-INSTR-PRICE * -.2
           WHEN NEW-FLAG
                INITIALIZE WS-QUAL-ADJ
           WHEN PREMIUM-FLAG
                COMPUTE WS-QUAL-ADJ ROUNDED = WS-INSTR-PRICE * .2
           END-EVALUATE.
           COMPUTE WS-ADJ-PRICE = WS-INSTR-PRICE + WS-QUAL-ADJ
      * Calculate Tax
           COMPUTE WS-TAX ROUNDED = WS-ADJ-PRICE * .08.
      * Calculate Shipping Cost
           IF IN-COUNTRY
                COMPUTE WS-SHIPPING ROUNDED = WS-ADJ-PRICE * .1
           ELSE
                COMPUTE WS-SHIPPING ROUNDED = WS-ADJ-PRICE * .2
           END-IF.
      * Calculate Total Cost
           COMPUTE WS-TOTAL-COST = WS-ADJ-PRICE + WS-TAX + WS-SHIPPING.
      *
       250-MOVE-DATA.
           MOVE ARTIST-ACCT-NO TO ARTIST-ACCT-NO-OUT.
           MOVE ARTIST-MUSICAL-GENRE TO ARTIST-GENRE-OUT.
           MOVE MUSICIAN-LNAME TO LNAME-OUT.
           MOVE MUSICIAN-FNAME TO FNAME-OUT.
           MOVE INSTRUMENT-TYPE TO INSTRUMENT-TYPE-OUT.
           MOVE INSTRUMENT-QUALITY TO INSTRUMENT-QUALITY-OUT.
           MOVE SHIP-TO TO SHIP-TO-OUT.
           MOVE MAX-MUSICIAN-BUDGET-AMT TO MAX-BUDGET-AMT-OUT.
           MOVE WS-INSTR-PRICE TO INSTR-PRICE-OUT.
           MOVE WS-QUAL-ADJ TO QUALITY-ADJ-OUT.
           MOVE WS-TAX TO TAX-AMT-OUT.
           MOVE WS-SHIPPING TO SHIP-COST-OUT.
           MOVE WS-TOTAL-COST TO TOTAL-COST-OUT.
      *
       300-OPEN-FILES.
           OPEN INPUT RFPIN.
           OPEN OUTPUT PROPOSAL.
       400-READ-RFP.
           READ RFPIN INTO RFP-REC
                AT END MOVE 'Y' TO RFPIN-EOF
           END-READ.
       500-WRITE-DETAIL.
           WRITE PROPOSAL-REC FROM PROP-OUT-1.
           WRITE PROPOSAL-REC FROM PROP-OUT-2.
       600-CLOSE-FILES.
           CLOSE RFPIN, PROPOSAL.
       700-WRITE-HEADERS.
           INITIALIZE PROPOSAL-REC.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR  TO HDR-YR.
           MOVE WS-CURRENT-MONTH TO HDR-MO.
           MOVE WS-CURRENT-DAY   TO HDR-DAY.
                WRITE PROPOSAL-REC FROM HEADER-1.
                WRITE PROPOSAL-REC FROM HEADER-2.
                WRITE PROPOSAL-REC FROM BLANK-LINE.
                WRITE PROPOSAL-REC FROM BLANK-LINE.
                WRITE PROPOSAL-REC FROM HEADER-3.
                WRITE PROPOSAL-REC FROM HEADER-4.
                WRITE PROPOSAL-REC FROM HEADER-5.
                WRITE PROPOSAL-REC FROM HEADER-6.
      *
       800-WRITE-TRAILERS.
           INITIALIZE PROPOSAL-REC.
           WRITE PROPOSAL-REC FROM BLANK-LINE.
           WRITE PROPOSAL-REC FROM BLANK-LINE.
           WRITE PROPOSAL-REC FROM TRAILER-1.
           WRITE PROPOSAL-REC FROM TRAILER-2.
           WRITE PROPOSAL-REC FROM TRAILER-3.
           WRITE PROPOSAL-REC FROM TRAILER-4.
           WRITE PROPOSAL-REC FROM TRAILER-5.
           WRITE PROPOSAL-REC FROM TRAILER-6.
           WRITE PROPOSAL-REC FROM TRAILER-7.