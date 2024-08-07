unit KM_MessageStack;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_GameTypes, KM_Points;


type
  //Individual message
  TKMStackMessage = class
    fKind: TKMMessageKind;
    fLoc: TKMPoint;
    fText: UnicodeString;
  public
    IsRead: Boolean; //Does not gets saved, because it's UI thing
    constructor Create(aKind: TKMMessageKind; const aText: UnicodeString; const aLoc: TKMPoint);
    constructor Load(LoadStream: TKMemoryStream);

    function Icon: Word;
    function IsGoto: Boolean;
    property Loc: TKMPoint read fLoc;
    property Text: UnicodeString read fText;
    property Kind: TKMMessageKind read fKind;

    procedure Save(SaveStream: TKMemoryStream);
  end;

  TKMMessageStack = class
  private
    fCountStack: Integer;
    fListStack: array of TKMStackMessage;
    function GetMessageStack(aIndex: Integer): TKMStackMessage;
  public
    destructor Destroy; override;

    property CountStack: Integer read fCountStack;
    property MessagesStack[aIndex: Integer]: TKMStackMessage read GetMessageStack; default;

    procedure Add(aKind: TKMMessageKind; const aText: UnicodeString; const aLoc: TKMPoint);
    procedure RemoveStack(aIndex: Integer);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  SysUtils, Math;


{ TKMStackMessage }
constructor TKMStackMessage.Create(aKind: TKMMessageKind; const aText: UnicodeString; const aLoc: TKMPoint);
begin
  inherited Create;

  fKind := aKind;
  fLoc := aLoc;
  fText := aText;
end;


constructor TKMStackMessage.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;

  LoadStream.CheckMarker('StackMessage');
  LoadStream.Read(fLoc);
  LoadStream.ReadW(fText);
  LoadStream.Read(fKind, SizeOf(TKMMessageKind));
  LoadStream.Read(IsRead);
end;


//Check wherever message has a GoTo option
function TKMStackMessage.IsGoto: Boolean;
begin
  Result := fKind in [mkHouse, mkGroup];
end;


//GUIMain icon index associated with that message kind
function TKMStackMessage.Icon: Word;
begin
  Result := MSG_ICON[fKind];
end;


procedure TKMStackMessage.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('StackMessage');
  SaveStream.Write(fLoc);
  SaveStream.WriteW(fText);
  SaveStream.Write(fKind, SizeOf(TKMMessageKind));
  SaveStream.Write(IsRead);
end;


{ TKMMessageStack }
destructor TKMMessageStack.Destroy;
var
  I: Integer;
begin
  for I := 0 to fCountStack - 1 do
    FreeAndNil(fListStack[I]);

  inherited;
end;


function TKMMessageStack.GetMessageStack(aIndex: Integer): TKMStackMessage;
begin
  Assert(InRange(aIndex, 0, fCountStack - 1));
  Result := fListStack[aIndex];
end;


procedure TKMMessageStack.Add(aKind: TKMMessageKind; const aText: UnicodeString; const aLoc: TKMPoint);
begin
  SetLength(fListStack, fCountStack + 1);
  fListStack[fCountStack] := TKMStackMessage.Create(aKind, aText, aLoc);
  Inc(fCountStack);
end;


procedure TKMMessageStack.RemoveStack(aIndex: Integer);
begin
  if not InRange(aIndex, 0, fCountStack - 1) then Exit; // Somehow could happen (was reported), but hard to reproduce

  FreeAndNil(fListStack[aIndex]); //Release the deleted message

  //Move the messages to cover the gap
  if aIndex <> fCountStack - 1 then
    Move(fListStack[aIndex + 1], fListStack[aIndex], (fCountStack - 1 - aIndex) * SizeOf(TKMStackMessage));

  //Keep it neat
  Dec(fCountStack);
  SetLength(fListStack, fCountStack);
end;


procedure TKMMessageStack.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('MessageStack');
  SaveStream.Write(fCountStack);
  for I := 0 to fCountStack - 1 do
    MessagesStack[I].Save(SaveStream);
end;


procedure TKMMessageStack.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.CheckMarker('MessageStack');
  LoadStream.Read(fCountStack);
  SetLength(fListStack, fCountStack);

  for I := 0 to fCountStack - 1 do
    fListStack[I] := TKMStackMessage.Load(LoadStream);
end;


end.
