unit KM_UnitTaskMerchant;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_ResTypes,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses, KM_HouseStore;


{Throw a rock}
type
  TKMTaskMerchant = class(TKMUnitTask)
  private
    fStore : TKMHouseStore;
    fShipyard : TKMHouseShipYard;
    fToShipYard : Boolean;
    fToPlayer : ShortInt;
    fWares : TKMWarePlan;
  public
    constructor Create(aUnit: TKMUnit; aStore, aShipYard : TKMHouse; aToPlayer : Integer; aWares : TKMWarePlan);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function WalkShouldAbandon : Boolean; override;
    function CouldBeCancelled: Boolean; override;

  end;


implementation
uses
  KM_Terrain,
  KM_CommonUtils,
  KM_HandsCollection, KM_Entity,
  KM_UnitWarrior,
  KM_Points;


{ TKMTaskCollectWares }
constructor TKMTaskMerchant.Create(aUnit: TKMUnit; aStore, aShipYard : TKMHouse; aToPlayer : Integer; aWares : TKMWarePlan);
begin
  inherited Create(aUnit);
  fType := uttMerchant;

  if aStore <> nil then
    fStore := TKMHouseStore(aStore.GetPointer);
  if aShipYard <> nil then
    fShipyard := TKMHouseShipYard(aShipYard.GetPointer);
  fToShipYard := (fStore = nil) and fShipyard.IsValid(htShipYard, false, true);
  fToPlayer := aToPlayer;
  fWares := aWares;
end;


destructor TKMTaskMerchant.Destroy;
begin
  gHands.CleanUpHousePointer(TKMHouse(fStore));
  gHands.CleanUpHousePointer(TKMHouse(fShipyard));
  inherited;
end;


constructor TKMTaskMerchant.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fStore, 4);
  LoadStream.Read(fShipyard, 4);
  LoadStream.Read(fToShipYard);
  LoadStream.Read(fToPlayer);
  fWares.Load(LoadStream);
end;


procedure TKMTaskMerchant.SyncLoad;
begin
  inherited;

  fStore := TKMHouseStore(gHands.GetHouseByUID(Integer(fStore)));
  fShipyard := TKMHouseShipYard(gHands.GetHouseByUID(Integer(fShipyard)))
end;

function TKMTaskMerchant.CouldBeCancelled : Boolean;
begin
  Result := WalkShouldAbandon or (fPhase < 1);

end;

function TKMTaskMerchant.WalkShouldAbandon: Boolean;
begin
  if fToShipYard then
  begin
    Result := not fShipyard.IsValid(htShipYard, false, true)
              or (gHands[fToPlayer].Stats.GetHouseQty(htShipYard) = 0);
  end
  else
    Result := not fStore.IsValid(htStore, false, true);
  Result := Result or (fUnit.Owner = fToPlayer);
end;

function TKMTaskMerchant.Execute: TKMTaskResult;
var timeToStay : Word;
  P : TKMPoint;
  H : TKMHouse;
  I : Integer;
begin
  Result := trTaskContinues;
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;
  with fUnit do
    case fPhase of
      0: begin
          Thought := thQuest;
          SetActionGoIn(uaWalk, gdGoOutside, Home);
          Home.SetState(hstEmpty);

          for I := 0 to fWares.Count - 1 do
            if fWares[I].W <> wtNone then
              Home.WareTakeFromIn(fWares[I].W, fWares[I].C);
        end;
      1:begin
          if fToShipYard then
            SetActionWalkToSpot(fShipYard.PointBelowEntrance)
          else
            SetActionWalkToSpot(fStore.PointBelowEntrance);
        end;
      2:begin
          if fToShipYard then
            SetActionGoIn(uaWalk, gdGoInside, fShipYard)//enter the Shipyard
          else
            SetActionGoIn(uaWalk, gdGoInside, fStore);//enter the store
        end;
      3:begin
          if fToShipYard then
          begin
            H := gHands[fToPlayer].GetClosestHouse(fShipYard.Entrance, [htShipyard]);
            if H <> nil then
            begin
              P := H.Entrance;
              timeToStay := Round(KMLengthDiag(fShipYard.Entrance, P)) * 10;
            end else
              timeToStay := 0;
            
            SetActionLockedStay(timeToStay, uaWalk); //give wares
          end else
            SetActionLockedStay(25, uaWalk); //give wares
        end;
      4:begin
          if fToShipYard then
          begin
            SetActionGoIn(uaWalk, gdGoOutside, fShipYard);

            H := gHands[fToPlayer].GetClosestHouse(fShipYard.Entrance, [htShipyard]);
            if H <> nil then
            begin
              for I := 0 to fWares.Count - 1 do
                if (fWares[I].W <> wtNone) and (fWares[I].C > 0) then
                  H.WareAddToOut(fWares[I].W, fWares[I].C);
              fWares.Reset;
            end;

          end
          else
          begin
            SetActionGoIn(uaWalk, gdGoOutside, fStore);
            for I := 0 to fWares.Count - 1 do
              if fWares[I].W <> wtNone then
                fStore.WareAddToIn(fWares[I].W, fWares[I].C);
          end;
        end;
      5: SetActionWalkToSpot(Home.PointBelowEntrance);
      6: SetActionGoIn(uaWalk, gdGoInside, Home);
      //unit back at home
      7:  begin
            SetActionLockedStay(5, uaWalk);
            Home.SetState(hstIdle);
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


procedure TKMTaskMerchant.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fStore.UID);
  SaveStream.Write(fShipyard.UID);
  SaveStream.Write(fToShipYard);
  SaveStream.Write(fToPlayer);
  fWares.Save(SaveStream);
end;

end.
