UNIT CribAbout;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/10 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines the about dialog used by the cribbage program.          *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2001/04/16 - Updated the version/date in dialog.            *)
(*          2001/04/20 - Updated the version/date in dialog.            *)
(*          2001/05/22 - Updated the version/date in dialog.            *)
(*          2001/06/17 - Updated the version/date in dialog.            *)
(*          2001/06/24 - Updated the version/date in dialog.            *)
(*          2001/07/07 - Added new message string constant to update    *)
(*                       version label with.                            *)
(*          2003-01-18 - Updated about box for 2003.                    *)
(*                                                                      *)

INTERFACE

 USES Classes, Forms, Graphics, ExtCtrls, Buttons, StdCtrls, Messages, CribSup;

 TYPE TAboutBox = CLASS(TForm)
                   Panel1      : TPanel;
                   ProgramIcon : TImage;
                   OkButton    : TBitBtn;
                   ProductName : TLabel;
                   Version     : TLabel;
                   Copyright   : TLabel;
                   Procedure AboutBoxOnShow(Sender : TObject);
                  PRIVATE
                   {Insert private declarations here}
                  PROTECTED
                   PROCEDURE WMAboutSUp(VAR Msg : TMESSAGE);
                    MESSAGE wm_AboutSUp;
                  PUBLIC
                   {Insert public declarations here}
                  END;

 VAR AboutBox : TAboutBox;

(************************************************************************)

IMPLEMENTATION

(************************************************************************)

 Procedure TAboutBox.AboutBoxOnShow(Sender : TObject);

  Begin
   Version.Caption := mVersion;
   PostMsg(Handle, wm_AboutSUp, 0, 0);
  End;

(************************************************************************)

 PROCEDURE TAboutBox.WMAboutSUp(VAR Msg : TMESSAGE);
     (* procedure used to do final setup of about box before displaying *)

  BEGIN (*taboutbox.wmaboutsup*)
   PutCardInImage(ProgramIcon, Random(52)+1);
  END; (*taboutbox.wmaboutsup*)

(************************************************************************)

INITIALIZATION
  RegisterClasses([TAboutBox, TPanel, TImage, TBitBtn, TLabel]);
END. (*of unit*)
