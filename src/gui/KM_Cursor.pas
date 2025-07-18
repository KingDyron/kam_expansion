unit KM_Cursor;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Defaults, KM_Points,
  KM_ResTilesetTypes,
  KM_AITypes,
  KM_MapEdTypes,
  KM_TerrainTypes;


type
  TKMMapEdModification = (medUnitBoots, medHouseForceWorking,
                          medHouseNoResource, medUnitNeverHungry);
  TKMCursor = class
  private
    fMode: TKMCursorMode; //Modes used in game (building, unit, road, etc..)
    procedure SetMode(aMode: TKMCursorMode);
    procedure Reset;
  public
    Pixel: TKMPoint;      //Cursor position in screen-space
    Float: TKMPointF;     //Precise cursor position in map coords
    Cell: TKMPoint;       //Cursor position cell
    PrevCell: TKMPoint;   //Cursor previous position cell
    SState: TShiftState;  //Thats actually used to see if Left or Right mouse button is pressed

    Tag1: Word;           //Tag to know building type, unit type etc
//    Tag2: Word;           //Extra Tag
    DragOffset: TKMPoint; //used to adjust actual Cursor Cell
    ObjectUID: Integer;   //Object found below cursor
    RoadType : TKMRoadType;
    GrainType : TKMGrainType;
    CampaignData : record
      Path, Name, ShortName : String;
      MissionID : Byte;
    end;

    // MapEd brushes page
    MapEdFieldAge: Integer;
    MapEdWineFieldAge: Integer;
    MapEdGrassFieldAge: Integer;
    MapEdVegeFieldAge: Integer;
    MapEdShape: TKMMapEdShape;
    MapEdSize: Byte;
    MapEdBrushMask: TKMTileMaskKind;
    MapEdUseMagicBrush: Boolean;
    MapEdRandomizeTiling: Boolean;
    MapEdOverrideCustomTiles: Boolean;
    MapEdBlendingLvl: Byte;
    MapEdUseTerrainObjects: Boolean;

    //Objects Brush
    MapEdCleanBrush,
    MapEdOverrideObjectsSingle,
    MapEdOverrideObjects: Boolean;
    MapEdObjectsType: array[TKMTerrainObjectType] of Boolean;
    MapEdForestAge: Integer;
    MapEdObjectsDensity: Integer;

    // MapEd elevations page
    MapEdSlope: Byte;
    MapEdSpeed: Byte;
    MapEdConstHeight: Byte;

    MapEdFishCount: Byte;

    // MapEd other pages
    // todo: refactor to use only 1 flag here
    MapEdDir: Byte;
    MapEdDirection: TKMDirection; //direction of Units and Defence Pos
    MapEdOverlayOnRoad : Word;
    MapEdApplyOverlayOnRoad : Boolean;

    // MapEd TownDefence
    MapEdDefPosGroupType: TKMGroupType; //group type of defence position
    MapEdDefPosGroupLevel: TKMGroupLevel; // group lvl which is added with def pos
    MapEdDefPosType: TKMAIDefencePosType; // defence pos type - defender/attacker
    MapEdDefPosSetGroup : Boolean; //add group with added defence pos
    MapEdGroupFormation : TKMFormation;//formations of added group


    MapEdUseTerrainMasks : Boolean;
    MapEdPatterns : record
      AddHeight : Integer;
    end;
    MapEd_HouseSite: Boolean;
    MapEd_HouseStyle: Byte;
    MapEd_HouseLevel: Byte;
    MapEd_UnitAddBoots: Boolean;
    MapEd_HouseFill: Byte;

    MapEd_Animals : TKMUnitTypeArray;
    MapEd_AnimalsPace,
    MapEd_AnimalsCount : Integer;

    MapEd_Modifications : set of TKMMapEdModification;
    Custom : Record
      RenderType : TKMCursorRenderType;
      Tag1, Tag2 : Integer;
    end;
    MapEd_WaresCount : Integer;
    Hint : String;
    MapEd_WaresMinCount : Byte;
    MapEd_WaresMaxCount : Byte;
    MapEd_WaresRandomCount : Boolean;

    constructor Create;
    procedure AddMod(aMod : TKMMapEdModification);
    procedure RemMod(aMod : TKMMapEdModification);
    function SwitchMod(aMod : TKMMapEdModification) : Boolean;


    property Mode: TKMCursorMode read fMode write SetMode;
  end;


var
  gCursor: TKMCursor;


implementation


{ TKMCursor }
constructor TKMCursor.Create;
begin
  inherited;

  MapEdDirection := dirS;
  MapEdDefPosGroupType := gtMelee;
  MapEdDefPosType := dtFrontLine;
  MapEdGroupFormation.NumUnits := 1;
  MapEdGroupFormation.UnitsPerRow := 1;

  Reset;
end;


procedure TKMCursor.AddMod(aMod : TKMMapEdModification);
begin
  MapEd_Modifications := MapEd_Modifications + [aMod];
end;

procedure TKMCursor.RemMod(aMod : TKMMapEdModification);
begin
  MapEd_Modifications := MapEd_Modifications - [aMod];

end;

function TKMCursor.SwitchMod(aMod : TKMMapEdModification) : Boolean;
begin
  Result := false;
  if aMod in MapEd_Modifications then
    RemMod(aMod)
  else
  begin
    AddMod(aMod);
    Result := true;
  end;
  
end;

procedure TKMCursor.Reset;
begin
  DragOffset := KMPOINT_ZERO;
  MapEdUseMagicBrush := False;
  SState := [];
  if fMode = cmNone then  //Reset Tag1 also, when reset mode
  begin
    Tag1 := 0;
//    Tag2 := 0;
  end;
  // Actually we need reset all fields when changing mode,
  // but lets reset only DragOffset for now, need to do lots of tests for other fields
end;


procedure TKMCursor.SetMode(aMode: TKMCursorMode);
begin
  fMode := aMode;

  Reset;
end;


end.
