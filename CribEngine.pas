UNIT CribEngine;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/18 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines the engine used to determine discards, score hands,     *)
(* score play and determine comps card to play for the cribbage game.   *)
(* Game engine is based on an engine originally design and written in   *)
(* Amiga A-basic by David Addison (1986?).                              *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2001/04/16 - Fixed a bug in the discard analysis function.  *)
(*          2001/04/20 - Fixed another bug and enhanced the discard     *)
(*                       function.  In addition, add logging to the     *)
(*                       discard analysis (for debugging).              *)
(*          2001/07/07 - Removed last GOTO's from analyze function.     *)
(*                                                                      *)

INTERFACE

{ $DEFINE LOGGINGON}

 USES CribSup;

 VAR UpCard         : LONGWORD; // upcard
     HandTally,      // tally for hand points and play points
     CardsPlayed,    // number of cards played (on the board currently)
     RecDiscardIdx1, // recommended discards
     RecDiscardIdx2 : INTEGER;
     PlayedCards    : ARRAY[1..8] OF LONGWORD; // cards currently on the board

(************************************************************************)

 PROCEDURE Figure_Points(Hnd : TCARDHAND; CribScore : BOOLEAN; VAR P : INTEGER);
     (* procedure gets points for hand (15, flush, pairs, etc.) *)

 PROCEDURE AnalyzeHandForDiscards(Hnd : TCARDHAND);
     (* procedure analyzes hand and recommends discards (computer discards) *)

 PROCEDURE PointsInGame(Who : INTEGER; VAR X31 : BOOLEAN; VAR P : INTEGER);
     (* procedure to determine if any points have been/will be scored *)
     (*  - who = 0 / analyze, 1 = player, 2 = computer                *)

 FUNCTION  GetCompCardToPlay(VAR Hnd : TCARDHAND) : INTEGER;
     (* function to determine card computer will play                *)
     (* - will return -1 for 'GO' (no play) or index in CHnd of card *)
     (* - CHnd2 should be the hand sent in, because the card chosen  *)
     (*   will be zeroed out in the hand                             *)

(************************************************************************)

IMPLEMENTATION

 USES SysUtils, Forms, QCardU, CribMsg;

 TYPE E11X6 = ARRAY[1..11,1..6] OF INTEGER;
      E4X5  = ARRAY[1..4,1..5] OF INTEGER;
      E4X0  = ARRAY[1..4] OF INTEGER;
      E15X7 = ARRAY[1..15,1..7] OF INTEGER;

 CONST ConQ : E11X6 = ((1,1,1,2,3, 9),(1,1,2,2,3,12),(1,1,2,3,3,12),
                       (1,1,2,3,4, 8),(1,2,2,2,3, 9),(1,2,2,3,3,12),
                       (1,2,2,3,4, 8),(1,2,3,3,3, 9),(1,2,3,3,4, 8),
                       (1,2,3,4,4, 8),(1,2,3,4,5, 5));
       ConR : E4X5  = ((1,1,2,3,6),(1,2,2,3,6),(1,2,3,3,6),(1,2,3,4,4));
       ConS : E4X0  = (1,2,3,3);
       ConV : E15X7 = ((1,2,3,4,5,6,0),(1,2,3,5,4,6,0),(1,2,3,6,4,5,0),
                       (1,2,4,5,3,6,0),(1,2,4,6,3,5,0),(1,2,5,6,3,4,0),
                       (1,3,4,5,2,6,0),(1,3,4,6,2,5,0),(1,3,5,6,2,4,0),
                       (1,4,5,6,2,3,0),(2,3,4,5,1,6,0),(2,3,4,6,1,5,0),
                       (2,3,5,6,1,4,0),(2,4,5,6,1,3,0),(3,4,5,6,1,2,0));

 VAR UpSuit, UpValue, UpV : INTEGER;

(************************************************************************)

 PROCEDURE CheckForJack(Hnd : TCARDHAND; VAR P : INTEGER);
     (* procedure to check hand for a jack that matches the suit of the upcard *)

    VAR I, S1, S2 : INTEGER;

  BEGIN (*checkforjack*)
   P := 0; // start points out at zero
   IF UpCard <> 0
    THEN BEGIN {have an upcard to check against}
          S1 := QC_GetCardSuit(UpCard);
          FOR I := 1 TO 4 DO
           BEGIN {check if suit matches and if card is a jack}
            S2 := QC_GetCardSuit(Hnd[I]);
            IF (S1 = S2) AND (QC_GetCardValue(Hnd[I]) = qv_Jack)
             THEN BEGIN {add 1 point, then exit}
                   Inc(P); Break;
                  END;
           END; {i for}
         END; {then}
  END; (*checkforjack*)

(************************************************************************)

 PROCEDURE CheckFlush(Hnd : TCARDHAND; CribScore : BOOLEAN; VAR P : INTEGER);
     (* procedure checks to see if the hand contains a flush *)

    VAR I, L : INTEGER;

  BEGIN (*checkflush*)
   Application.ProcessMessages; // release window messages
   L := 4;
   FOR I := 1 TO 3 DO
    IF (QC_GetCardSuit(Hnd[I]) <> QC_GetCardSuit(Hnd[I+1]))
     THEN Dec(L);
   IF NOT(CribScore) AND (L = 4)
    THEN BEGIN {score the flush}
          P := P + 4;
          IF UpSuit = QC_GetCardSuit(Hnd[1]) THEN Inc(P); // match upcard also
         END {then}
   ELSE IF (L = 4) AND (UpSuit = QC_GetCardSuit(Hnd[1]))
         THEN P := P + 5; // only score flush for crib if all match upcard
  END; (*checkflush*)

(************************************************************************)

 PROCEDURE Check15(Hnd : TCARDHAND; VAR P : INTEGER);
     (* procedure to check to see if hand has cards totalling 15 *)

    VAR I, J, K, L, M : INTEGER;

  BEGIN (*check15*)
   // check for 15 (between 2 cards)
   Application.ProcessMessages;
   FOR I := 1 TO 4 DO
    FOR J := (I+1) TO 5 DO {including upcard}
     BEGIN {check for 15}
      IF J = 5
       THEN L := QC_GetCV_Face10(Hnd[I]) + UpValue
      ELSE L := QC_GetCV_Face10(Hnd[I]) + QC_GetCV_Face10(Hnd[J]);
      IF L = 15 THEN P := P + 2;
     END; {j for}
   // check for 15 (between 3 cards)
   Application.ProcessMessages;
   FOR I := 1 TO 3 DO
    FOR J := (I+1) TO 4 DO
     FOR K := (J+1) TO 5 DO
      BEGIN {check for 15}
       L := QC_GetCV_Face10(Hnd[I]) + QC_GetCV_Face10(Hnd[J]);
       IF K = 5
        THEN L := L + UpValue
       ELSE L := L + QC_GetCV_Face10(Hnd[K]);
       IF L = 15 THEN P := P + 2;
      END; {k for}
   // check for 15 (4 cards)
   Application.ProcessMessages;
   FOR I := 1 TO 2 DO
    FOR J := (I+1) TO 3 DO
     FOR K := (J+1) TO 4 DO
      FOR L := (K+1) TO 5 DO
       BEGIN
        M := QC_GetCV_Face10(Hnd[I]) + QC_GetCV_Face10(Hnd[J]) +
             QC_GetCV_Face10(Hnd[K]);
        IF L = 5
         THEN M := M + UpValue
        ELSE M := M + QC_GetCV_Face10(Hnd[L]);
        IF M = 15 THEN P := P + 2;
       END;
   // check for 15 (hand + upcard)
   L := UpValue;
   FOR I := 1 TO 4 DO
    L := L + QC_GetCV_Face10(Hnd[I]);
   IF L = 15 THEN P := P + 2;
  END; (*check15*)

(************************************************************************)

 PROCEDURE CheckDups(Hnd : TCARDHAND; VAR P : INTEGER);
     (* procedure to check for duplicates (pairs, 3ofkind, etc.) *)

    VAR I  : INTEGER;
        JJ : ARRAY[1..13] OF INTEGER;

  BEGIN (*checkdups*)
   Application.ProcessMessages;
   FOR I := 1 TO 13 DO
    JJ[I] := 0;
   FOR I := 1 TO 4 DO
    Inc(JJ[QC_GetCardValue(Hnd[I])]);
   IF UpCard <> 0 THEN Inc(JJ[UpV]);
   FOR I := 1 TO 13 DO
    CASE JJ[I] OF
     2 : P := P + 2; {pair}
     3 : P := P + 6; {three of a kind}
     4 : P := P + 12; {four of a kind}
    END; {case}
  END; (*checkdups*)

(************************************************************************)

 PROCEDURE CheckRuns(Hnd : TCARDHAND; VAR P : INTEGER);
     (* procedure to check for card runs (7,8,9, etc.) *)

    LABEL L3, L4, L5;

    VAR I, J, K, L, M : INTEGER;
        VV            : ARRAY[1..5] OF LONGWORD;
        T             : LONGWORD;
        Q             : E11X6;
        R             : E4X5;
        S             : E4X0;

  BEGIN (*checkruns*)
   FOR I := 1 TO 4 DO
    VV[I] := QC_GetCardValue(Hnd[I]);
   VV[5] := UpV;
   FOR I := 1 TO 5 DO
    FOR J := I TO 5 DO
     IF VV[I] > VV[J]
      THEN BEGIN {swap}
            T := VV[J]; VV[J] := VV[I]; VV[I] := T;
           END; {then}
   Application.ProcessMessages;
   Q := ConQ;
   L := VV[1] - Q[1,1];
   FOR I := 1 TO 11 DO
    FOR J := 1 TO 5 DO
     Q[I,J] := Q[I,J] + L;
   I := 1;
   WHILE I < 12 DO
    BEGIN {check runs}
     J := 1;
     WHILE J < 6 DO
      IF VV[J] <> Q[I,J] THEN GOTO L3 ELSE Inc(J);
     P := P + Q[I,6];
     Exit;
     L3: Inc(I);
    END; {while}
   Application.ProcessMessages;
   R := ConR;
   L := 1;
   WHILE (L < 3) DO
    BEGIN {check other runs}
     M := VV[L] - R[1,1];
     FOR I := 1 TO 4 DO
      FOR J := 1 TO 4 DO
       R[I,J] := R[I,J] + M;
     I := 1;
     WHILE I < 5 DO
      BEGIN {inner run}
       K := 1;
       WHILE K < 5 DO
        IF VV[K+L-1] <> R[I,K] THEN GOTO L4 ELSE Inc(K);
       P := P + R[I,5];
       Exit;
       L4: Inc(I);
      END; {while i}
     Inc(L);
    END; {while L}
   Application.ProcessMessages;
   S := ConS;
   L := 1;
   WHILE (L < 4) DO
    BEGIN {last run check}
     M := VV[L] - S[1];
     FOR I := 1 TO 3 DO
      S[I] := S[I] + M;
     I := 1;
     WHILE (I < 4) DO
      IF VV[L+I-1] <> S[I] THEN GOTO L5 ELSE Inc(I);
     P := P + S[4];
     Exit;
     L5: Inc(L);
    END; {while L}
  END; (*checkruns*)

(************************************************************************)

 PROCEDURE Figure_Points(Hnd : TCARDHAND; CribScore : BOOLEAN; VAR P : INTEGER);
     (* procedure gets points for hand (15, flush, pairs, etc.) *)

  BEGIN (*figure_points*)
   IF UpCard <> 0
    THEN BEGIN {get suit and value of it then}
          UpSuit := QC_GetCardSuit(UpCard);
          UpValue := QC_GetCV_Face10(UpCard);
          UpV := QC_GetCardValue(UpCard);
         END {then}
   ELSE BEGIN {assign to out of context constants}
         UpSuit := 25; UpValue := 25; UpV := 25;
        END; {else}
   // check for jack points
   CheckForJack(Hnd, P);
   // check for flush
   CheckFlush(Hnd, CribScore, P);
   // check for 15s
   Check15(Hnd, P);
   // count pairs/three of kinds, etc.
   CheckDups(Hnd, P);
   // check for runs now
   CheckRuns(Hnd, P);
  END; (*figure_points*)

(************************************************************************)

{$IFDEF LOGGINGON}
 FUNCTION OpenLogFile(VAR LogFile : TEXTFILE) : BOOLEAN;
     (* function used to open the log file (if analyze logging turned on) *)

    VAR FnAndStr : STRING[160];

  BEGIN (*openlogfile*)
   FnAndStr := Application.EXEName;
   FnAndStr := Copy(FnAndStr,1,Length(FnAndStr)-3) + 'LOG';
   AssignFile(LogFile, FnAndStr);
   IF FileExists(FnAndStr)
    THEN {$I-} Append(LogFile) {$I+}
   ELSE {$I-} Rewrite(LogFile); {$I+}
   Result := IOResult = 0;
  END; (*openlogfile*)

//--------------------------------------------------------------

 PROCEDURE WriteLogStart(VAR LogFile : TEXTFILE; Hnd : TCARDHAND);
     (* procedure to write out start of analyze log (for current hand) *)

    VAR I, iSuit : INTEGER;
        TmpC     : CHAR;

  BEGIN (*writelogstart*)
   Writeln(LogFile, '-----');
   Write(LogFile, 'Hand Analyzing -');
   FOR I := 1 TO 6 DO
    BEGIN {write out cards in hand}
     iSuit := QC_GetCardSuit(Hnd[I]); TmpC := '?';
     CASE iSuit OF
      qs_Clubs    : TmpC := 'C';
      qs_Diamonds : TmpC := 'D';
      qs_Hearts   : TmpC := 'H';
      qs_Spades   : TmpC := 'S';
     END; {case}
     Write(LogFile, ' ', QC_GetCardValue(Hnd[I]), TmpC);
    END; {for i}
   Writeln(LogFile);
  END; (*writelogstart*)
{$ENDIF}

(************************************************************************)

 PROCEDURE AnalyzeHandForDiscards(Hnd : TCARDHAND);
     (* procedure analyzes hand and recommends discards (computer discards) *)
     (*                                                                     *)
     (* NOTE: Some places of this analysis seem incomplete (see N:=1 code). *)
     (* - My feeling, should be checking for 7's if have 8's.               *)
     (* Somehow, I feel some stuff got lost in the conversion from the      *)
     (*  basic code.                                                        *)

    VAR V                  : E15X7;
        I, J, K, L, N, P,
        B9, C9, P9, Z9, ZZ : INTEGER;
        TmpH               : TCARDHAND;
        II, JJ             : ARRAY[1..15] OF INTEGER;
       {$IFDEF LOGGINGON}
        LogFile            : TEXTFILE;
        LogOpen            : BOOLEAN;
       {$ENDIF}

  BEGIN (*analyzehandfordiscards*)
  {$IFDEF LOGGINGON}
   LogOpen := OpenLogFile(LogFile);
   IF LogOpen THEN WriteLogStart(LogFile, Hnd); // set up begining
  {$ENDIF}
   V := ConV;
   P9 := 0;
   FOR Z9 := 1 TO 15 DO
    BEGIN {analyze and get points for each hand}
     FOR I := 1 TO 4 DO
      TmpH[I] := Hnd[V[Z9,I]];
     Figure_Points(TmpH, FALSE, P);
     V[Z9,7] := P;
     IF P > P9 THEN P9 := P; // largest points so far
    END; {z9 for}
  {$IFDEF LOGGINGON}
   IF LogOpen
    THEN BEGIN // write out points
          Writeln(LogFile, 'Points for each combination:');
          FOR I := 1 TO 15 DO
           Write(LogFile, ' ', I, ':', V[I,7]);
          Writeln(LogFile);
         END; {then}
  {$ENDIF}
   // see if more than one hand has the same (biggest) point total
   J := 0;
   FOR I := 1 TO 15 DO
    IF V[I,7] = P9
     THEN BEGIN {found our point total, mark in an array}
           Inc(J); II[J] := I;
          END; {then}
   B9 := II[1]; // select this hand (may be it...)
   IF J <> 1
    THEN BEGIN // more than one hand with the biggest amount of points
         {$IFDEF LOGGINGON}
          IF LogOpen
           THEN BEGIN {indicate more than one hand is high}
                 Writeln(LogFile, '** More than one hand with highest score.');
                 Writeln(LogFile, 'Hand combinations:');
                 FOR I := 1 TO J DO
                  Write(LogFile, ' ', II[I]);
                 Writeln(LogFile);
                END; {then}
         {$ENDIF}
          C9 := 5; ZZ := 1; // start by checking for hand with 5's in it
          REPEAT
           Application.ProcessMessages;
          {$IFDEF LOGGINGON}
           IF LogOpen
            THEN Writeln(LogFile, 'Do we have ', C9, ' in one of the hands?');
          {$ENDIF}
           P9 := 0;
           FOR I := 1 TO 15 DO
            JJ[I] := 0;
           FOR I := 1 TO J DO
            BEGIN {find out if hand has high value card or not}
             FOR K := 1 TO 4 DO
              BEGIN {do we have it or not?}
               L := V[II[I], K];
               IF QC_GetCardValue(Hnd[L]) = C9
                THEN BEGIN {inc card we have in array}
                      Inc(JJ[I]);
                      IF JJ[I] > P9 THEN P9 := JJ[I]; {more than one?}
                     END; {then}
              END; {k for}
            END; {i for}
           // more than one hand with our card in it?
           K := 0;
           FOR I := 1 TO J DO
            IF (JJ[I] <> 0) AND (JJ[I] = P9)
             THEN BEGIN {maybe}
                   Inc(K); B9 := II[I];
                  {$IFDEF LOGGINGON}
                   IF LogOpen
                    THEN Writeln(LogFile, ' - Yes: Hand ', B9, ' (index ', I, ')');
                  {$ENDIF}
                  END; {then}
           // if true, more than one hand...
           IF K <> 1
            THEN BEGIN {rebuild list of hands to choose from and look again}
                  IF K <> 0
                   THEN BEGIN {reset hands to look at list}
                         FOR I := (J-1) DOWNTO 1 DO
                          IF JJ[I] = 0
                           THEN FOR L := I TO J-1 DO
                                 II[L] := II[L+1];
                         J := K;
                        END; {then}
                 {$IFDEF LOGGINGON}
                  IF LogOpen
                   THEN BEGIN {show new hand to look through}
                         Writeln(LogFile,'(...More than one hand left...)');
                         Write(LogFile, ' Hand combinations left =');
                         FOR I := 1 TO J DO
                          Write(LogFile, ' ', II[I]);
                         Writeln(LogFile);
                        END; {then}
                 {$ENDIF}
                  Inc(ZZ);
                  CASE ZZ OF // look for next 'high value' card then
                   2 : BEGIN {eight's?}
                        C9 := 8; N := 1;
                       END; {zz = 2}
                   3 : C9 := 7; {seven's}
                   4 : C9 := 11; {jack's}
                   5 : C9 := 1; {ace's}
                   6 : B9 := II[Random(J)+1]; // all hands are equal, pick one...
                  END; {case}
                 END {then}
           ELSE ZZ := 6;
          UNTIL ZZ >= 6;
         END; {then}
   RecDiscardIdx1 := V[B9,5]; RecDiscardIdx2 := V[B9,6];
  {$IFDEF LOGGINGON}
   IF LogOpen
    THEN BEGIN {close it down}
          Writeln(LogFile, 'Using hand combination: ',B9,'  Points=',V[B9,7]);
          Close(LogFile);
         END; {then}
  {$ENDIF}
  END; (*analyzehandfordiscards*)

(************************************************************************)

 PROCEDURE CheckForRun(I : INTEGER; VAR R9 : INTEGER);
     (* procdure to check for a run played 1,2,3,4... *)

    VAR J, K, L : INTEGER;
        JJ      : ARRAY[1..20] OF LONGWORD;
        X       : LONGWORD;

  BEGIN (*checkforrun*)
   FOR J := 11 TO 20 DO
    JJ[J] := 14;
   FOR J := 1 TO CardsPlayed DO
    JJ[J+10] := QC_GetCardValue(PlayedCards[CardsPlayed-J+1]);
   FOR K := 1 TO (I-1) DO
    BEGIN {sort}
     L := K + 1;
     WHILE L <= I DO
      BEGIN {sort}
       IF JJ[K+10] > JJ[L+10]
        THEN BEGIN {swap}
              X := JJ[K+10]; JJ[K+10] := JJ[L+10]; JJ[L+10] := X;
             END; {then}
       Inc(L);
      END; {while}
    END; {k for}
   FOR K := 1 TO (I-1) DO
    IF JJ[K+10] <> (JJ[K+11]-1) THEN Exit;
   R9 := I; // have at least I card run
  END; (*checkforrun*)

(************************************************************************)

 PROCEDURE PointsInGame(Who : INTEGER; VAR X31 : BOOLEAN; VAR P : INTEGER);
     (* procedure to determine if any points have been/will be scored *)
     (*  - who = 0 / analyze, 1 = player, 2 = computer                *)

    VAR I, J, N : INTEGER;
        sWho    : STRING[10];

  BEGIN (*pointsingame*)
   P := 0; X31 := FALSE; J := 0;
   IF CardsPlayed = 1 THEN Exit; // can't score with only one card
   IF Who = 1 THEN sWho := mYou ELSE sWho := mMe;
   IF HandTally = 15
    THEN BEGIN {got 15 with play}
          P := P + 2;
          IF Who <> 0
           THEN Show_Message(Application.MainForm, sWho + mTwo15);
         END; {then}
   IF HandTally = 31
    THEN BEGIN {got to 31 with play - set to reset everything}
          P := P + 2; X31 := TRUE;
         END; {then}
   IF (CardsPlayed - 2) > 2
    THEN N := CardsPlayed - 2
   ELSE N := 2;
   FOR I := CardsPlayed DOWNTO N DO
    BEGIN {check for pair/3ok/4ok}
     Application.ProcessMessages;
     IF QC_GetCardValue(PlayedCards[I]) <> QC_GetCardValue(PlayedCards[I-1])
      THEN Break;
     CASE CardsPlayed-I+1 OF
      1 : BEGIN {a pair}
           P := P + 2; J := 1;
          END; {pair}
      2 : BEGIN {three of a kind}
           P := P + 4; J := 2; // cumulitive points
          END; {3ok}
      3 : BEGIN {four of a kind}
           P := P + 6; J := 3;
          END; {4ok}
     END; {case}
    END; {i for}
   IF (Who <> 0)
    THEN CASE J OF {message points}
          1 : Show_Message(Application.MainForm, sWho + mPair);
          2 : Show_Message(Application.MainForm, sWho + mThreeKind);
          3 : Show_Message(Application.MainForm, sWho + mFourKind);
         END; {then case}
   IF CardsPlayed = 2 THEN Exit; // can't have runs with 2 cards
   I := 3; N := 0;
   WHILE (I <= CardsPlayed) DO
    BEGIN {check for run}
     Application.ProcessMessages;
     CheckForRun(I, N); Inc(I);
    END; {while}
   P := P + N;
   IF (Who <> 0) AND (N <> 0)
    THEN Show_Message(Application.MainForm, Format(mRun, [sWho, N, N]));
  END; (*pointsingame*)

(************************************************************************)

 FUNCTION GetCompCardToPlay(Hnd : TCARDHAND) : INTEGER;
     (* function to determine card computer will play *)

    VAR I, J, K, P, I9, P9 : INTEGER;
        TT                 : BOOLEAN;
        II, JJ             : ARRAY[1..4] OF INTEGER;

  BEGIN (*getcompcardtoplay*)
   Inc(CardsPlayed); // temporary
   P := 0; P9 := 0; I9 := -1; J := 0; K := 0;
   FOR I := 1 TO 4 DO
    IF Hnd[I] <> 0
     THEN BEGIN {have a card - see if we can get some points for it}
           IF (HandTally + QC_GetCV_Face10(Hnd[I])) <= 31
            THEN BEGIN {we can play this card}
                  PlayedCards[CardsPlayed] := Hnd[I];
                  PointsInGame(0, TT, P);
                  IF P > P9
                   THEN BEGIN {save it}
                         P9 := P; I9 := I;
                        END; {then}
                  // never throw a 5 first (unless you have to)
                  IF (QC_GetCardValue(Hnd[I]) = 5) AND (CardsPlayed = 1)
                   THEN BEGIN {add to list to consider playing}
                         Inc(K); JJ[K] := I;
                        END {then}
                  ELSE BEGIN {add to cards we could play (first)}
                         Inc(J); II[J] := I;
                       END; {else}
                 END; {then}
          END; {then}
   Dec(CardsPlayed); // reset back to what it was
   // select card to play (if points can't be scored with any of them)
   IF (I9 = -1) AND (J <> 0)
    THEN BEGIN {we had some other cards we could play}
          IF J = 1
           THEN I9 := II[J]
          ELSE BEGIN {we have more than one card we can play}
                I9 := II[Random(J)+1];
               END; {else}
         END; {then}
   // if we still don't have a card, check secondary list of cards to play
   // (may only have a 5 we can play...)
   IF (I9 = -1) AND (K <> 0)
    THEN BEGIN {we had some other cards we could play}
          IF K = 1
           THEN I9 := JJ[K]
          ELSE BEGIN {we have more than one card in the list}
                I9 := JJ[Random(K)+1];
               END; {else}
         END; {then}
   // clear out card in temp hand (we've played it)
   IF (I9 <> -1) THEN Hnd[I9] := 0;
   // return index of card to play back
   Result := I9;
  END; (*getcompcardtoplay*)

(************************************************************************)

INITIALIZATION

END. (*of unit*)
