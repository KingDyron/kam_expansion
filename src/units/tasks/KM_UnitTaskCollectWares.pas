unit KM_UnitTaskCollectWares;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses;


{Throw a rock}
type
  TKMTaskCollectWares = class(TKMUnitTask)
  private
  public
    constructor Create(aUnit: TKMUnit);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function WalkShouldAbandon : Boolean; override;

  end;

  TKMTaskUnloadWares = class(TKMUnitTask)
  private
    fShipYard : TKMHouseShipYard;
  public
    constructor Create(aUnit: TKMUnit; aShipyard : TKMHouseShipYard);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function WalkShouldAbandon : Boolean; override;
    property ShipYard : TKMHouseShipYard read fShipYard;

  end;


implementation
uses
  KM_Terrain,
  KM_CommonUtils,
  KM_HandsCollection, KM_Entity,
  KM_UnitWarrior,
  KM_Points,
  KM_ResTypes;


{ TKMTaskCollectWares }
constructor TKMTaskCollectWares.Create(aUnit: TKMUnit);
begin
  inherited Create(aUnit);
  fType := uttCollectWares;
end;


destructor TKMTaskCollectWares.Destroy;
begin
  inherited;
end;


constructor TKMTaskCollectWares.Load(LoadStream: TKMemoryStream);
begin
  inherited;
end;


procedure TKMTaskCollectWares.SyncLoad;
begin
  inherited;
end;

function TKMTaskCollectWares.WalkShouldAbandon: Boolean;
var S : TKMUnitWarriorBoat;
begin
  S := TKMUnitWarriorBoat(fUnit);
  Result := (S = nil)
            or S.IsDeadOrDying
            or not (S.CanCollectWares or S.CanCollectFish)
            or (S.TotalWaresCount > 20)
            or not (S.NextOrder in [woNone, woBoatCollectWares]);

end;

function TKMTaskCollectWares.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            Thought := thQuest;
            SetActionLockedStay(40, uaWalk);
          end;
      1:  begin
            Thought := thQuest;
            Inc(fPhase2);
            SetActionLockedStay(10, uaWalk);
            if fPhase2 < 5 then
              fPhase := 0;
          end;
      2:  begin
            fPhase2 := 0;
            Thought := thQuest;
            SetActionLockedStay(10, uaWalk);
            Direction := KaMRandomDir('TKMTaskCollectWares.Execute1');
          end;
      3:  begin
            Thought := thQuest;
            Inc(fPhase2);
            SetActionLockedStay(10, uaWalk);
            if fPhase2 < 5 then
              fPhase := 2;
          end;
      4:  begin
            If not TKMUnitWarriorBoat(fUnit).AddWare(gTerrain.FindWareForBoat(Position,
                                                                        SPec.MiningRange,
                                                                        TKMUnitWarriorBoat(fUnit).CanCollectWares,
                                                                        TKMUnitWarriorBoat(fUnit).CanCollectFish))
            then
            if TKMUnitWarriorBoat(fUnit).CanCollectWares then
              TKMUnitWarriorBoat(fUnit).AddVWare(gTerrain.FindVWareForBoat(Position,
                                                                          SPec.MiningRange));

            SetActionLockedStay(10, uaWalk);
            Thought := thQuest;
          end;
      5:  begin
            SetActionLockedStay(5, uaWalk);
            Thought := thNone;
            //fPhase := 0; //loop it until player turns this off
          end;
      else Result := trTaskDone;
    end;
  Inc(fPhase);
end;


procedure TKMTaskCollectWares.Save(SaveStream: TKMemoryStream);
begin
  inherited;
end;



{ TKMTaskUnloadWares }
constructor TKMTaskUnloadWares.Create(aUnit: TKMUnit; aShipyard: TKMHouseShipyard);
begin
  inherited Create(aUnit);
  fType := uttUnloadWares;
  fShipYard := TKMHouseShipyard(aShipyard.GetPointer);
end;


destructor TKMTaskUnloadWares.Destroy;
begin
  gHands.CleanUpHousePointer(TKMHouse(fShipYard));
  inherited;
end;


constructor TKMTaskUnloadWares.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fShipYard, 4);
end;


procedure TKMTaskUnloadWares.SyncLoad;
begin
  inherited;
  fShipYard := TKMHouseShipyard(gHands.GetHouseByUID(Integer(fShipYard)) );
end;

function TKMTaskUnloadWares.WalkShouldAbandon: Boolean;
begin
  Result := (fShipyard = nil)
            or (fShipyard.IsDestroyed)
            or (TKMUnitWarriorBoat(fUnit).TotalWaresCount = 0)
            or not gTerrain.IsTileNearLand(fUnit.Position);
end;

function TKMTaskUnloadWares.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            Thought := thHome;
            SetActionLockedStay(25, uaWalk);
          end;
      1:  begin
            SetActionLockedStay(15, uaWalk);
            Thought := thHome;
          end;
      2:  begin
            TKMUnitWarriorBoat(fUnit).UnloadWare(fShipYard);
            SetActionLockedStay(5, uaWalk);
            Thought := thNone;
            fPhase := 0; //loop it until boat is empty
          end;
      else Result := trTaskDone;
    end;
  Inc(fPhase);
end;


procedure TKMTaskUnloadWares.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fShipYard.UID);
end;

end.
