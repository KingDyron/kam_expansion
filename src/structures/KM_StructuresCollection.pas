unit KM_StructuresCollection;
{$I KaM_Remake.inc}
interface
uses KM_Defaults,
    KM_CommonTypes, KM_CommonClasses, KM_Points,
    KM_HandEntity, KM_HandTypes,
    KM_Structure,
    KM_ResStructures, KM_ResTypes;

type

  TKMStructuresCollection = class
  private
    fCount : integer;
    fStructures : array of TKMStructure;
    function GetStructure(aIndex : Integer) : TKMStructure;
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Load(LoadStream : TKMemoryStream);
    procedure Save(SaveStream : TKMemoryStream);
    procedure Paint(aRect : TKMRect);

    function AddStructure(aLoc : TKMPoint; aIndex, aRot : Integer; aOwner : TKMHandID) : TKMStructure;
    procedure AddStructureToList(aStructure : TKMStructure);

    function HitTest(aX, aY : Integer; aNoCompleted : Boolean = true) : TKMStructure;

    property Count : integer read fCount;
    property Structure[aIndex : Integer] : TKMStructure read GetStructure; default;

    function GetStructureByUID(aUID : Integer) : TKMStructure;
  end;

Implementation

uses
    KM_GameUIDTracker,
    KM_Resource,
    KM_Entity,
    Math, SysUtils;

constructor TKMStructuresCollection.Create;
begin
  fCount := 0;
  setLength(fStructures, fCount);
end;

destructor TKMStructuresCollection.Destroy;
var I : Integer;
begin
  for I := 0 to fCount - 1 do
      fStructures[I].Free;
  Inherited;
end;

procedure TKMStructuresCollection.Load(LoadStream: TKMemoryStream);
var I : Integer;
  str : TKMStructure;
  newCount : Integer;
begin
  LoadStream.CheckMarker('Structures');

  LoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin

    str := TKMStructure.Load(LoadStream);
    if str <> nil then
      AddStructureToList(str);
  end;

end;

procedure TKMStructuresCollection.Save(SaveStream: TKMemoryStream);
var I : Integer;
begin
  SaveStream.PlaceMarker('Structures');
  SaveStream.Write(fCount);

  for I := 0 to fCount - 1 do
    fStructures[I].Save(SaveStream);
end;

function  TKMStructuresCollection.GetStructure(aIndex: Integer): TKMStructure;
begin
  Assert(InRange(aIndex, 0, fCount - 1), 'No Structure with ID: ' + IntToStr(aIndex));

  Result := fStructures[aIndex];
end;

function TKMStructuresCollection.GetStructureByUID(aUID: Integer): TKMStructure;
var I : integer;
begin
  Result := nil;

  for I := 0 to fCount - 1 do
    if fStructures[I].UID = aUID then
      Exit(fStructures[I]);
end;

function TKMStructuresCollection.AddStructure(aLoc: TKMPoint; aIndex: Integer; aRot: Integer; aOwner: ShortInt) : TKMStructure;
var str : TKMStructure;
  uid : Integer;
begin
  uid := gUIDTracker.GetNewUID;

  str := TKMStructure.Create(uid, aIndex, aRot, aLoc, aOwner);
  Result := str;
  if str <> nil then
    AddStructureToList(str);

end;


procedure TKMStructuresCollection.AddStructureToList(aStructure: TKMStructure);
begin
  If aStructure = nil then
    Exit;

  Inc(fCount);
  Setlength(fStructures, fCount);
  fStructures[fCount - 1] := aStructure;
end;

function TKMStructuresCollection.HitTest(aX: Integer; aY: Integer; aNoCompleted : Boolean = true): TKMStructure;
var I : Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
    if not fStructures[I].IsComplete or not aNoCompleted then
      If fStructures[I].HitTest(aX, aY) then
        Exit(fStructures[I]);


end;

procedure TKMStructuresCollection.Paint(aRect : TKMRect);
var I : Integer;
begin
  for I := 0 to fCount - 1 do
    if KMInRect(fStructures[I].Position, aRect) then
      fStructures[I].Paint;
end;



end.

                 