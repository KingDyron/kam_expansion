unit uPSI_JvMail;
{
This file has been generated by UnitParser v0.4b, written by M. Knight
and updated by NP. v/d Spek.
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ifps3 are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok''s conv unility
}

{$IFDEF MSWINDOWS}
{$I ..\PascalScript.inc}
{$ELSE}
{$I ../PascalScript.inc}
{$ENDIF}
interface
 
uses
  SysUtils, Classes, uPSComponent, uPSCompiler, uPSRuntime;

type
(*----------------------------------------------------------------------------*)
  TPSImport_JvMail = class(TPSPlugin)
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure CompileImport2(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
    procedure ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;

implementation


uses
   Windows
  ,Controls
  ,Forms
  ,Mapi
  ,JclBase
  ,JclMapi
  ,JvComponent
  ,JvMail
  ;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TJvMail(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TJvComponent', 'TJvMail') do
  with CL.AddClassN(CL.FindClass('TComponent'),'TJvMail') do
  begin
    RegisterMethod('function Address(const Caption: string; EditFields: Integer): Boolean');
    RegisterMethod('procedure Clear');
    RegisterMethod('function ErrorCheck(Res: DWORD): DWORD');
    RegisterMethod('function FindFirstMail: Boolean');
    RegisterMethod('function FindNextMail: Boolean');
    RegisterMethod('procedure FreeSimpleMapi');
    RegisterMethod('procedure LogOff');
    RegisterMethod('procedure LogOn');
    RegisterMethod('procedure ReadMail');
    RegisterMethod('function ResolveName(const Name: string): string');
    RegisterMethod('function SaveMail(const MessageID: string): string');
    RegisterMethod('procedure SendMail(ShowDialog: Boolean)');
    RegisterProperty('ReadedMail', 'TJvMailReadedData', iptr);
    RegisterProperty('SeedMessageID', 'string', iptrw);
    RegisterProperty('SessionHandle', 'THandle', iptr);
    RegisterProperty('SimpleMAPI', 'TJclSimpleMapi', iptr);
    RegisterProperty('UserLogged', 'Boolean', iptr);
    RegisterProperty('Attachment', 'TStrings', iptrw);
    RegisterProperty('BlindCopy', 'TJvMailRecipients', iptrw);
    RegisterProperty('Body', 'TStrings', iptrw);
    RegisterProperty('CarbonCopy', 'TJvMailRecipients', iptrw);
    RegisterProperty('LogonOptions', 'TJvMailLogonOptions', iptrw);
    RegisterProperty('LongMsgId', 'Boolean', iptrw);
    RegisterProperty('Password', 'string', iptrw);
    RegisterProperty('ProfileName', 'string', iptrw);
    RegisterProperty('ReadOptions', 'TJvMailReadOptions', iptrw);
    RegisterProperty('Recipient', 'TJvMailRecipients', iptrw);
    RegisterProperty('Subject', 'string', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TJvMailRecipients(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TCollection', 'TJvMailRecipients') do
  with CL.AddClassN(CL.FindClass('TCollection'),'TJvMailRecipients') do
  begin
    RegisterMethod('constructor Create(AOwner: TJvMail; ARecipientClass: DWORD)');
    RegisterMethod('function Add: TJvMailRecipient');
    RegisterMethod('function AddRecipient(const Address: string; const Name: string): Integer');
    RegisterProperty('Items', 'TJvMailRecipient Integer', iptrw);
    SetDefaultPropery('Items');
    RegisterProperty('RecipientClass', 'DWORD', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TJvMailRecipient(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TCollectionItem', 'TJvMailRecipient') do
  with CL.AddClassN(CL.FindClass('TCollectionItem'),'TJvMailRecipient') do
  begin
    RegisterProperty('AddressAndName', 'string', iptr);
    RegisterProperty('Address', 'string', iptrw);
    RegisterProperty('Name', 'string', iptrw);
    RegisterProperty('Valid', 'Boolean', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_JvMail(CL: TPSPascalCompiler);
begin
  CL.AddClassN(CL.FindClass('TObject'),'TJvMail');
  SIRegister_TJvMailRecipient(CL);
  SIRegister_TJvMailRecipients(CL);
  CL.AddTypeS('TJvMailLogonOption', '(loLogonUI, loNewSession)');
  CL.AddTypeS('TJvMailReadOption', '(roUnreadOnly, roFifo, roPeek, roHeaderOnly, roAttachments)');

  CL.AddTypeS('TJvMailLogonOptions', 'set of TJvMailLogonOption');
  CL.AddTypeS('TJvMailReadOptions', 'set of TJvMailReadOption');
  CL.AddTypeS('TJvMailReadedData', 'record RecipientAddress: string; RecipientName: string; ConversationID: string; DateReceived: TDateTime; end');

  SIRegister_TJvMail(CL);
end;

(* === run-time registration functions === *)
{$IFDEF DELPHI10UP}{$REGION 'TJvMail'}{$ENDIF}
{$IFDEF class_helper_present}
type
  TJvMail_PSHelper = class helper for TJvMail
  public
    procedure Attachment_R(var T: TStrings);
    procedure Attachment_W(const T: TStrings);
    procedure BlindCopy_R(var T: TJvMailRecipients);
    procedure BlindCopy_W(const T: TJvMailRecipients);
    procedure Body_R(var T: TStrings);
    procedure Body_W(const T: TStrings);
    procedure CarbonCopy_R(var T: TJvMailRecipients);
    procedure CarbonCopy_W(const T: TJvMailRecipients);
    procedure LogonOptions_R(var T: TJvMailLogonOptions);
    procedure LogonOptions_W(const T: TJvMailLogonOptions);
    procedure LongMsgId_R(var T: Boolean);
    procedure LongMsgId_W(const T: Boolean);
    procedure Password_R(var T: string);
    procedure Password_W(const T: string);
    procedure ProfileName_R(var T: string);
    procedure ProfileName_W(const T: string);
    procedure ReadedMail_R(var T: TJvMailReadedData);
    procedure ReadOptions_R(var T: TJvMailReadOptions);
    procedure ReadOptions_W(const T: TJvMailReadOptions);
    procedure Recipient_R(var T: TJvMailRecipients);
    procedure Recipient_W(const T: TJvMailRecipients);
    procedure SeedMessageID_R(var T: string);
    procedure SeedMessageID_W(const T: string);
    procedure SessionHandle_R(var T: THandle);
    procedure SimpleMAPI_R(var T: TJclSimpleMapi);
    procedure Subject_R(var T: string);
    procedure Subject_W(const T: string);
    procedure UserLogged_R(var T: Boolean);
  end;

procedure TJvMail_PSHelper.Subject_W(const T: string);
begin Self.Subject := T; end;


procedure TJvMail_PSHelper.Subject_R(var T: string);
begin T := Self.Subject; end;


procedure TJvMail_PSHelper.Recipient_W(const T: TJvMailRecipients);
begin Self.Recipient := T; end;


procedure TJvMail_PSHelper.Recipient_R(var T: TJvMailRecipients);
begin T := Self.Recipient; end;


procedure TJvMail_PSHelper.ReadOptions_W(const T: TJvMailReadOptions);
begin Self.ReadOptions := T; end;


procedure TJvMail_PSHelper.ReadOptions_R(var T: TJvMailReadOptions);
begin T := Self.ReadOptions; end;


procedure TJvMail_PSHelper.ProfileName_W(const T: string);
begin Self.ProfileName := T; end;


procedure TJvMail_PSHelper.ProfileName_R(var T: string);
begin T := Self.ProfileName; end;


procedure TJvMail_PSHelper.Password_W(const T: string);
begin Self.Password := T; end;


procedure TJvMail_PSHelper.Password_R(var T: string);
begin T := Self.Password; end;


procedure TJvMail_PSHelper.LongMsgId_W(const T: Boolean);
begin Self.LongMsgId := T; end;


procedure TJvMail_PSHelper.LongMsgId_R(var T: Boolean);
begin T := Self.LongMsgId; end;


procedure TJvMail_PSHelper.LogonOptions_W(const T: TJvMailLogonOptions);
begin Self.LogonOptions := T; end;


procedure TJvMail_PSHelper.LogonOptions_R(var T: TJvMailLogonOptions);
begin T := Self.LogonOptions; end;


procedure TJvMail_PSHelper.CarbonCopy_W(const T: TJvMailRecipients);
begin Self.CarbonCopy := T; end;


procedure TJvMail_PSHelper.CarbonCopy_R(var T: TJvMailRecipients);
begin T := Self.CarbonCopy; end;


procedure TJvMail_PSHelper.Body_W(const T: TStrings);
begin Self.Body := T; end;


procedure TJvMail_PSHelper.Body_R(var T: TStrings);
begin T := Self.Body; end;


procedure TJvMail_PSHelper.BlindCopy_W(const T: TJvMailRecipients);
begin Self.BlindCopy := T; end;


procedure TJvMail_PSHelper.BlindCopy_R(var T: TJvMailRecipients);
begin T := Self.BlindCopy; end;


procedure TJvMail_PSHelper.Attachment_W(const T: TStrings);
begin Self.Attachment := T; end;


procedure TJvMail_PSHelper.Attachment_R(var T: TStrings);
begin T := Self.Attachment; end;


procedure TJvMail_PSHelper.UserLogged_R(var T: Boolean);
begin T := Self.UserLogged; end;


procedure TJvMail_PSHelper.SimpleMAPI_R(var T: TJclSimpleMapi);
begin T := Self.SimpleMAPI; end;


procedure TJvMail_PSHelper.SessionHandle_R(var T: THandle);
begin T := Self.SessionHandle; end;


procedure TJvMail_PSHelper.SeedMessageID_W(const T: string);
begin Self.SeedMessageID := T; end;


procedure TJvMail_PSHelper.SeedMessageID_R(var T: string);
begin T := Self.SeedMessageID; end;


procedure TJvMail_PSHelper.ReadedMail_R(var T: TJvMailReadedData);
begin T := Self.ReadedMail; end;

procedure RIRegister_TJvMail(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMail) do
  begin
    RegisterMethod(@TJvMail.Address, 'Address');
    RegisterMethod(@TJvMail.Clear, 'Clear');
    RegisterMethod(@TJvMail.ErrorCheck, 'ErrorCheck');
    RegisterMethod(@TJvMail.FindFirstMail, 'FindFirstMail');
    RegisterMethod(@TJvMail.FindNextMail, 'FindNextMail');
    RegisterMethod(@TJvMail.FreeSimpleMapi, 'FreeSimpleMapi');
    RegisterMethod(@TJvMail.LogOff, 'LogOff');
    RegisterMethod(@TJvMail.LogOn, 'LogOn');
    RegisterMethod(@TJvMail.ReadMail, 'ReadMail');
    RegisterMethod(@TJvMail.ResolveName, 'ResolveName');
    RegisterMethod(@TJvMail.SaveMail, 'SaveMail');
    RegisterMethod(@TJvMail.SendMail, 'SendMail');
    RegisterPropertyHelper(@TJvMail.ReadedMail_R,nil,'ReadedMail');
    RegisterPropertyHelper(@TJvMail.SeedMessageID_R,@TJvMail.SeedMessageID_W,'SeedMessageID');
    RegisterPropertyHelper(@TJvMail.SessionHandle_R,nil,'SessionHandle');
    RegisterPropertyHelper(@TJvMail.SimpleMAPI_R,nil,'SimpleMAPI');
    RegisterPropertyHelper(@TJvMail.UserLogged_R,nil,'UserLogged');
    RegisterPropertyHelper(@TJvMail.Attachment_R,@TJvMail.Attachment_W,'Attachment');
    RegisterPropertyHelper(@TJvMail.BlindCopy_R,@TJvMail.BlindCopy_W,'BlindCopy');
    RegisterPropertyHelper(@TJvMail.Body_R,@TJvMail.Body_W,'Body');
    RegisterPropertyHelper(@TJvMail.CarbonCopy_R,@TJvMail.CarbonCopy_W,'CarbonCopy');
    RegisterPropertyHelper(@TJvMail.LogonOptions_R,@TJvMail.LogonOptions_W,'LogonOptions');
    RegisterPropertyHelper(@TJvMail.LongMsgId_R,@TJvMail.LongMsgId_W,'LongMsgId');
    RegisterPropertyHelper(@TJvMail.Password_R,@TJvMail.Password_W,'Password');
    RegisterPropertyHelper(@TJvMail.ProfileName_R,@TJvMail.ProfileName_W,'ProfileName');
    RegisterPropertyHelper(@TJvMail.ReadOptions_R,@TJvMail.ReadOptions_W,'ReadOptions');
    RegisterPropertyHelper(@TJvMail.Recipient_R,@TJvMail.Recipient_W,'Recipient');
    RegisterPropertyHelper(@TJvMail.Subject_R,@TJvMail.Subject_W,'Subject');
  end;
end;

{$ELSE}
procedure TJvMailSubject_W(Self: TJvMail; const T: string);
begin Self.Subject := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailSubject_R(Self: TJvMail; var T: string);
begin T := Self.Subject; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipient_W(Self: TJvMail; const T: TJvMailRecipients);
begin Self.Recipient := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipient_R(Self: TJvMail; var T: TJvMailRecipients);
begin T := Self.Recipient; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailReadOptions_W(Self: TJvMail; const T: TJvMailReadOptions);
begin Self.ReadOptions := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailReadOptions_R(Self: TJvMail; var T: TJvMailReadOptions);
begin T := Self.ReadOptions; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailProfileName_W(Self: TJvMail; const T: string);
begin Self.ProfileName := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailProfileName_R(Self: TJvMail; var T: string);
begin T := Self.ProfileName; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailPassword_W(Self: TJvMail; const T: string);
begin Self.Password := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailPassword_R(Self: TJvMail; var T: string);
begin T := Self.Password; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailLongMsgId_W(Self: TJvMail; const T: Boolean);
begin Self.LongMsgId := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailLongMsgId_R(Self: TJvMail; var T: Boolean);
begin T := Self.LongMsgId; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailLogonOptions_W(Self: TJvMail; const T: TJvMailLogonOptions);
begin Self.LogonOptions := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailLogonOptions_R(Self: TJvMail; var T: TJvMailLogonOptions);
begin T := Self.LogonOptions; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailCarbonCopy_W(Self: TJvMail; const T: TJvMailRecipients);
begin Self.CarbonCopy := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailCarbonCopy_R(Self: TJvMail; var T: TJvMailRecipients);
begin T := Self.CarbonCopy; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailBody_W(Self: TJvMail; const T: TStrings);
begin Self.Body := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailBody_R(Self: TJvMail; var T: TStrings);
begin T := Self.Body; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailBlindCopy_W(Self: TJvMail; const T: TJvMailRecipients);
begin Self.BlindCopy := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailBlindCopy_R(Self: TJvMail; var T: TJvMailRecipients);
begin T := Self.BlindCopy; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailAttachment_W(Self: TJvMail; const T: TStrings);
begin Self.Attachment := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailAttachment_R(Self: TJvMail; var T: TStrings);
begin T := Self.Attachment; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailUserLogged_R(Self: TJvMail; var T: Boolean);
begin T := Self.UserLogged; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailSimpleMAPI_R(Self: TJvMail; var T: TJclSimpleMapi);
begin T := Self.SimpleMAPI; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailSessionHandle_R(Self: TJvMail; var T: THandle);
begin T := Self.SessionHandle; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailSeedMessageID_W(Self: TJvMail; const T: string);
begin Self.SeedMessageID := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailSeedMessageID_R(Self: TJvMail; var T: string);
begin T := Self.SeedMessageID; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailReadedMail_R(Self: TJvMail; var T: TJvMailReadedData);
begin T := Self.ReadedMail; end;

procedure RIRegister_TJvMail(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMail) do
  begin
    RegisterMethod(@TJvMail.Address, 'Address');
    RegisterMethod(@TJvMail.Clear, 'Clear');
    RegisterMethod(@TJvMail.ErrorCheck, 'ErrorCheck');
    RegisterMethod(@TJvMail.FindFirstMail, 'FindFirstMail');
    RegisterMethod(@TJvMail.FindNextMail, 'FindNextMail');
    RegisterMethod(@TJvMail.FreeSimpleMapi, 'FreeSimpleMapi');
    RegisterMethod(@TJvMail.LogOff, 'LogOff');
    RegisterMethod(@TJvMail.LogOn, 'LogOn');
    RegisterMethod(@TJvMail.ReadMail, 'ReadMail');
    RegisterMethod(@TJvMail.ResolveName, 'ResolveName');
    RegisterMethod(@TJvMail.SaveMail, 'SaveMail');
    RegisterMethod(@TJvMail.SendMail, 'SendMail');
    RegisterPropertyHelper(@TJvMailReadedMail_R,nil,'ReadedMail');
    RegisterPropertyHelper(@TJvMailSeedMessageID_R,@TJvMailSeedMessageID_W,'SeedMessageID');
    RegisterPropertyHelper(@TJvMailSessionHandle_R,nil,'SessionHandle');
    RegisterPropertyHelper(@TJvMailSimpleMAPI_R,nil,'SimpleMAPI');
    RegisterPropertyHelper(@TJvMailUserLogged_R,nil,'UserLogged');
    RegisterPropertyHelper(@TJvMailAttachment_R,@TJvMailAttachment_W,'Attachment');
    RegisterPropertyHelper(@TJvMailBlindCopy_R,@TJvMailBlindCopy_W,'BlindCopy');
    RegisterPropertyHelper(@TJvMailBody_R,@TJvMailBody_W,'Body');
    RegisterPropertyHelper(@TJvMailCarbonCopy_R,@TJvMailCarbonCopy_W,'CarbonCopy');
    RegisterPropertyHelper(@TJvMailLogonOptions_R,@TJvMailLogonOptions_W,'LogonOptions');
    RegisterPropertyHelper(@TJvMailLongMsgId_R,@TJvMailLongMsgId_W,'LongMsgId');
    RegisterPropertyHelper(@TJvMailPassword_R,@TJvMailPassword_W,'Password');
    RegisterPropertyHelper(@TJvMailProfileName_R,@TJvMailProfileName_W,'ProfileName');
    RegisterPropertyHelper(@TJvMailReadOptions_R,@TJvMailReadOptions_W,'ReadOptions');
    RegisterPropertyHelper(@TJvMailRecipient_R,@TJvMailRecipient_W,'Recipient');
    RegisterPropertyHelper(@TJvMailSubject_R,@TJvMailSubject_W,'Subject');
  end;
end;

{$ENDIF class_helper_present}
{$IFDEF DELPHI10UP}{$ENDREGION}{$ENDIF}

{$IFDEF DELPHI10UP}{$REGION 'TJvMailRecipients'}{$ENDIF}
{$IFDEF class_helper_present}
type
  TJvMailRecipients_PSHelper = class helper for TJvMailRecipients
  public
    procedure Items_R(var T: TJvMailRecipient; const t1: Integer);
    procedure Items_W(const T: TJvMailRecipient; const t1: Integer);
    procedure RecipientClass_R(var T: DWORD);
  end;

procedure TJvMailRecipients_PSHelper.RecipientClass_R(var T: DWORD);
begin T := Self.RecipientClass; end;


procedure TJvMailRecipients_PSHelper.Items_W(const T: TJvMailRecipient; const t1: Integer);
begin Self.Items[t1] := T; end;


procedure TJvMailRecipients_PSHelper.Items_R(var T: TJvMailRecipient; const t1: Integer);
begin T := Self.Items[t1]; end;

procedure RIRegister_TJvMailRecipients(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMailRecipients) do
  begin
    RegisterConstructor(@TJvMailRecipients.Create, 'Create');
    RegisterMethod(@TJvMailRecipients.Add, 'Add');
    RegisterMethod(@TJvMailRecipients.AddRecipient, 'AddRecipient');
    RegisterPropertyHelper(@TJvMailRecipients.Items_R,@TJvMailRecipients.Items_W,'Items');
    RegisterPropertyHelper(@TJvMailRecipients.RecipientClass_R,nil,'RecipientClass');
  end;
end;

{$ELSE}
(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientsRecipientClass_R(Self: TJvMailRecipients; var T: DWORD);
begin T := Self.RecipientClass; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientsItems_W(Self: TJvMailRecipients; const T: TJvMailRecipient; const t1: Integer);
begin Self.Items[t1] := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientsItems_R(Self: TJvMailRecipients; var T: TJvMailRecipient; const t1: Integer);
begin T := Self.Items[t1]; end;

procedure RIRegister_TJvMailRecipients(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMailRecipients) do
  begin
    RegisterConstructor(@TJvMailRecipients.Create, 'Create');
    RegisterMethod(@TJvMailRecipients.Add, 'Add');
    RegisterMethod(@TJvMailRecipients.AddRecipient, 'AddRecipient');
    RegisterPropertyHelper(@TJvMailRecipientsItems_R,@TJvMailRecipientsItems_W,'Items');
    RegisterPropertyHelper(@TJvMailRecipientsRecipientClass_R,nil,'RecipientClass');
  end;
end;

{$ENDIF class_helper_present}
{$IFDEF DELPHI10UP}{$ENDREGION}{$ENDIF}

{$IFDEF DELPHI10UP}{$REGION 'TJvMailRecipient'}{$ENDIF}
{$IFDEF class_helper_present}
type
  TJvMailRecipient_PSHelper = class helper for TJvMailRecipient
  public
    procedure Address_R(var T: string);
    procedure Address_W(const T: string);
    procedure Name_R(var T: string);
    procedure Name_W(const T: string);
    procedure Valid_R(var T: Boolean);
  end;

procedure TJvMailRecipient_PSHelper.Valid_R(var T: Boolean);
begin T := Self.Valid; end;


procedure TJvMailRecipient_PSHelper.Name_W(const T: string);
begin Self.Name := T; end;


procedure TJvMailRecipient_PSHelper.Name_R(var T: string);
begin T := Self.Name; end;


procedure TJvMailRecipient_PSHelper.Address_W(const T: string);
begin Self.Address := T; end;


procedure TJvMailRecipient_PSHelper.Address_R(var T: string);
begin T := Self.Address; end;


procedure TJvMailRecipientAddressAndName_R(Self: TJvMailRecipient; var T: string);
begin T := Self.AddressAndName; end;

procedure RIRegister_TJvMailRecipient(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMailRecipient) do
  begin
    RegisterPropertyHelper(@TJvMailRecipient.AddressAndName_R,nil,'AddressAndName');
    RegisterPropertyHelper(@TJvMailRecipient.Address_R,@TJvMailRecipient.Address_W,'Address');
    RegisterPropertyHelper(@TJvMailRecipient.Name_R,@TJvMailRecipient.Name_W,'Name');
    RegisterPropertyHelper(@TJvMailRecipient.Valid_R,nil,'Valid');
  end;
end;

{$ELSE}
(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientValid_R(Self: TJvMailRecipient; var T: Boolean);
begin T := Self.Valid; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientName_W(Self: TJvMailRecipient; const T: string);
begin Self.Name := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientName_R(Self: TJvMailRecipient; var T: string);
begin T := Self.Name; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientAddress_W(Self: TJvMailRecipient; const T: string);
begin Self.Address := T; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientAddress_R(Self: TJvMailRecipient; var T: string);
begin T := Self.Address; end;

(*----------------------------------------------------------------------------*)
procedure TJvMailRecipientAddressAndName_R(Self: TJvMailRecipient; var T: string);
begin T := Self.AddressAndName; end;

procedure RIRegister_TJvMailRecipient(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMailRecipient) do
  begin
    RegisterPropertyHelper(@TJvMailRecipientAddressAndName_R,nil,'AddressAndName');
    RegisterPropertyHelper(@TJvMailRecipientAddress_R,@TJvMailRecipientAddress_W,'Address');
    RegisterPropertyHelper(@TJvMailRecipientName_R,@TJvMailRecipientName_W,'Name');
    RegisterPropertyHelper(@TJvMailRecipientValid_R,nil,'Valid');
  end;
end;

{$ENDIF class_helper_present}
{$IFDEF DELPHI10UP}{$ENDREGION}{$ENDIF}

(*----------------------------------------------------------------------------*)
procedure RIRegister_JvMail(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TJvMail) do
  RIRegister_TJvMailRecipient(CL);
  RIRegister_TJvMailRecipients(CL);
  RIRegister_TJvMail(CL);
end;

{ TPSImport_JvMail }
(*----------------------------------------------------------------------------*)
procedure TPSImport_JvMail.CompOnUses(CompExec: TPSScript);
begin
  { nothing } 
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_JvMail.ExecOnUses(CompExec: TPSScript);
begin
  { nothing } 
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_JvMail.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_JvMail(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_JvMail.CompileImport2(CompExec: TPSScript);
begin
  { nothing } 
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_JvMail.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_JvMail(ri);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_JvMail.ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  { nothing } 
end;

end.
