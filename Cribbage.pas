PROGRAM Cribbage;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/05 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Program will allow a user to play a game of cribbage with the        *)
(* computer.                                                            *)
(* Based, a lot, on a game created by David Addison in Amiga A-Basic.   *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2001/04/16 - Fixed a bug in the analyze hand function.      *)
(*          2001/04/20 - More changes to the analyze engine.            *)
(*          2001/05/22 - Fixed a bug in the initial display of the      *)
(*                       current card back image in the options dialog. *)
(*          2001/06/17 - Fixed (I think) the issue with the 'last card' *)
(*                       point not being scored the same between the    *)
(*                       player and the computer.                       *)
(*          2001/06/24 - Added in another fix for 'last card' scoring.  *)
(*                       Computer was not getting a last point after    *)
(*                       playing out hand.                              *)
(*          2001/07/07 - Removed GOTO's from 'analyze' function.        *)
(*          2001/07/08 - Fixed a bug in the enabling of discard buttons.*)
(*          2003-01-17 - Recompiled with 4-bit card images instead of   *)
(*                       24-bit (issues between how Sibyl's TBitmap     *)
(*                       displayed them and SDD/Snap drivers).          *)
(*                                                                      *)

 USES Forms, Graphics, CribWin, CribAbout, CribConf, CribPDlg, CribMsg;

{$r Cribbage.scu}
{$IFDEF OS2}
 {$R Cribbage.srf}
{$ELSE}
 {$R CribbageW.srw}
{$ENDIF}

BEGIN (*main*)
 Application.Create;
 Application.CreateForm(TCribbageWin, CribbageWin);
 Application.Run;
 Application.Destroy;
END. (*main*)
