unit KM_Entity;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses;

type
  // Abstract entity (has only the basic properties)
  TKMEntity = class abstract
  private
    fUID: Integer; //unique entity ID
  protected
    // Accessing field directly with a getter is a hair faster,
    // but it is more cumbersome to have 2 getters and we don't need it anyway,
    // since getting 65k UIDs (in XXL Terrain.Load) speeds up by only 0.05sec
    function GetUID: Integer; inline;
    procedure SetUID(aUID: Integer);
  public
    constructor Create(aUID: Integer);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure Save(SaveStream: TKMemoryStream); virtual;

    property UID: Integer read GetUID;

    function ObjToString(const aSeparator: String = '|'): String; virtual;
    function ObjToStringShort(const aSeparator: String = '|'): String; virtual;
  end;

implementation
uses
  SysUtils,
  KM_Defaults;


{ TKMEntity }
constructor TKMEntity.Create(aUID: Integer);
begin
  inherited Create;

  fUID := aUID;
end;


constructor TKMEntity.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;

  LoadStream.CheckMarker('Entity');
  LoadStream.Read(fUID);
end;


procedure TKMEntity.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('Entity');
  SaveStream.Write(fUID);
end;


procedure TKMEntity.SetUID(aUID: Integer);
begin
  fUID := aUID;
end;


function TKMEntity.GetUID: Integer;
begin
  if Self = nil then Exit(UID_NONE);

  Result := fUID;
end;


function TKMEntity.ObjToStringShort(const aSeparator: String = '|'): String;
begin
  Result := Format('UID = %d', [UID]);
end;


function TKMEntity.ObjToString(const aSeparator: String = '|'): String;
begin
  Result := ObjToStringShort(aSeparator);
end;


end.
