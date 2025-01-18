unit KM_UnitTaskAttackHouse;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_Units, KM_UnitWarrior, KM_Points;


type
  // Attack a house
  TKMTaskAttackHouse = class(TKMUnitTask)
  private
    fHouse: TKMHouse;
  public
    constructor Create(aWarrior: TKMUnitWarrior; aHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    property House: TKMHouse read fHouse;

    function WalkShouldAbandon: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  // Attack a house
  TKMTaskAssignToShip = class(TKMUnitTask)
  private
    fShip: TKMUnitWarriorShip;
  public
    constructor Create(aWarrior: TKMUnit; aShip: TKMUnitWarriorShip);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    property Ship: TKMUnitWarriorShip read fShip;

    function WalkShouldAbandon: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  // Attack a house
  TKMTaskUnloadFromShip = class(TKMUnitTask)
  private
    fShip: TKMUnitWarriorShip;
  public
    constructor Create(aWarrior: TKMUnit; aShip: TKMUnitWarriorShip);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    property Ship: TKMUnitWarriorShip read fShip;

    function WalkShouldAbandon: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  // take over house
  TKMTaskTakeOverHouse = class(TKMUnitTask)
  private
    fHouse: TKMHouse;
  public
    constructor Create(aWarrior: TKMUnit; aHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    property House: TKMHouse read fHouse;

    function WalkShouldAbandon: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Entity,
  KM_HandsCollection,
  KM_Resource, KM_ResSound, KM_ResHouses, KM_ResUnits,
  KM_HandTypes, KM_Hand, KM_HandEntity,
  KM_ScriptingEvents,
  KM_Sound, KM_Projectiles, KM_GameParams,
  KM_SpecialAnim, KM_ResTypes;


const
  MeleeSoundsHouse: array [0..12] of TSoundFX = (
    sfxMelee37, sfxMelee38, sfxMelee39, sfxMelee40, sfxMelee41,
    sfxMelee42, sfxMelee43, sfxMelee47, sfxMelee51, sfxMelee52,
    sfxMelee53, sfxMelee54, sfxMelee57
  );


{ TTaskAttackHouse }
constructor TKMTaskAttackHouse.Create(aWarrior: TKMUnitWarrior; aHouse: TKMHouse);
begin
  inherited Create(aWarrior);
  fType := uttAttackHouse;
  fHouse := aHouse.GetPointer;
end;


constructor TKMTaskAttackHouse.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskAttackHouse');
  LoadStream.Read(fHouse, 4);
end;


procedure TKMTaskAttackHouse.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;


destructor TKMTaskAttackHouse.Destroy;
begin
  gHands.CleanUpHousePointer(fHouse);
  inherited;
end;


function TKMTaskAttackHouse.WalkShouldAbandon: Boolean;
begin
  Result := fHouse.IsDestroyed;
end;


function TKMTaskAttackHouse.Execute: TKMTaskResult;
var
   AnimLength: Integer;
   Delay, Cycle: Integer;
   closest: TKMPoint;
   dir : TKMDirection;
begin
  Result := trTaskContinues;

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWarrior(fUnit) do
    case fPhase of
      0:  if IsRanged then
            if fHouse.GetDistance(Position) < GetFightMinRange then
              //Archer is too close, try to step back to the minimum range
              SetActionWalkFromHouse(fHouse, GetFightMinRange)
            else
            if fHouse.GetDistance(Position) > GetFightMaxRange then
              SetActionWalkToHouse(fHouse, GetFightMaxRange)
            else
              SetActionStay(0, uaWalk)
          else
            SetActionWalkToHouse(fHouse, 1.42);
      1:  begin
            if IsRanged then
            begin
              //Check if the walk failed
              if (fHouse.GetDistance(Position) < GetFightMinRange) or (fHouse.GetDistance(Position) > GetFightMaxRange) then
              begin
                SetActionStay(0, uaWalk);
                Result := trTaskDone;
                Exit;
              end;
              if TKMUnitWarrior(fUnit).IsRanged then
                If not TKMUnitWarrior(fUnit).InfinityAmmo and (TKMUnitWarrior(fUnit).BoltCount <= 0) then
                begin
                  SetActionStay(0, uaWalk);
                  Exit;
                end;

              //Calculate base aiming delay
              Delay := AimingDelay;

              //Prevent rate of fire exploit by making archers pause for longer if they shot recently
              Cycle := Max(gRes.Units[UnitType].UnitAnim[uaWork, Direction].Count, 1) - FiringDelay;
              if NeedsToReload(Cycle) then
                Delay := Delay + Cycle - (gGameParams.Tick - LastShootTime);

              SetActionLockedStay(Delay,uaWork); //Pretend to aim

              if not KMSamePoint(Position, fHouse.GetClosestCell(Position)) then //Unbuilt houses can be attacked from within
              begin
                Direction := KMGetDirection(PositionNext, fHouse.Entrance); //Look at house
                if UnitType in UNITS_SHIPS then
                  Direction := DIR_TO_NEXT2[Direction];
              end;

              if gMySpectator.FogOfWar.CheckTileRevelation(Round(PositionF.X), Round(PositionF.Y)) >= 255 then
              case UnitType of
                utBattleShip,
                utCrossbowman: gSoundPlayer.Play(sfxCrossbowDraw, PositionF); //Aiming
                utArcher,
                utBowman:     gSoundPlayer.Play(sfxBowDraw,      PositionF); //Aiming
                utRogue:  ;
                utGolem,
                utCatapult:  gSoundPlayer.Play(sfxBowDraw,      PositionF); //Aiming
                utBallista:  gSoundPlayer.Play(sfxCrossbowDraw, PositionF); //Aiming
                else           raise Exception.Create('Unknown shooter');
              end;

            end
            else
            begin
              //Check if the walk failed
              if fHouse.GetDistance(Position) > GetFightMaxRange then
              begin
                SetActionStay(0, uaWalk);
                Result := trTaskDone;
                Exit;
              end;
              SetActionLockedStay(fUnit.UID mod 3,uaWork,true, 0); //Melee units pause after the hit
              closest := fHouse.GetClosestCell(PositionNext);
              if not KMSamePoint(PositionNext, closest) then //Unbuilt houses can be attacked from within
                Direction := KMGetDirection(PositionNext, closest); //Look at house

            end;
          end;
      2:  begin
            //Special case - slingshot, he has AimSoundDelay
            if UnitType = utRogue then
              SetActionLockedStay(AimSoundDelay, uaWork, False) //Start shooting before sound
            else
            begin
              Inc(fPhase); //Skip slingshot special phase
              AnimLength := gRes.Units[UnitType].UnitAnim[uaWork, Direction].Count;
              if IsRanged then
                SetActionLockedStay(FiringDelay, uaWork, False) //Start shooting
              else
              if fUnit.UnitType = utTorchMan then
                SetActionLockedStay(AnimLength, uaWork, false) // torchman does full animation before hit
              else
                SetActionLockedStay(AnimLength div 2, uaWork, False); //Start the hit
            end;
          end;
      3:  begin
            gSoundPlayer.Play(sfxSlingerShoot, PositionF);
            SetActionLockedStay(FiringDelay - AimSoundDelay, uaWork, False, 0, AimSoundDelay) //Start shooting before sound
          end;
      4:  begin
            if IsRanged then
            begin
              //Launch the missile and forget about it
              //Shooting range is not important now, houses don't walk (except Howl's Moving Castle perhaps)
              //todo: Slingers (rogues) should launch rock part on SLINGSHOT_FIRING_DELAY like they do in ActionFight (animation looks wrong now)
              if UnitType = utBattleShip then
              begin
                dir := DIR_TO_PREV2[Direction];
                if TakeBolt then
                  gProjectiles.AimTarget(fUnit.PositionF, KMPointAffectDir(KMPointF(fHouse.GetRandomCellWithin), dir, -1), 0.2, ProjectileType, fUnit, RangeMax, RangeMin);//aim to the left
                if TakeBolt then
                  gProjectiles.AimTarget(fUnit.PositionF, KMPointAffectDir(KMPointF(fHouse.GetRandomCellWithin), dir, -0.5), 0.2, ProjectileType, fUnit, RangeMax, RangeMin);//aim to the left

                if TakeBolt then
                  gProjectiles.AimTarget(fUnit.PositionF, KMPointAffectDir(KMPointF(fHouse.GetRandomCellWithin), dir, 0), 0.1, ProjectileType, fUnit, RangeMax, RangeMin);//aim directly at opponent

                if TakeBolt then
                  gProjectiles.AimTarget(fUnit.PositionF, KMPointAffectDir(KMPointF(fHouse.GetRandomCellWithin), dir, 0.5), 0.2, ProjectileType, fUnit, RangeMax, RangeMin);//aim to the right
                if TakeBolt then
                  gProjectiles.AimTarget(fUnit.PositionF, KMPointAffectDir(KMPointF(fHouse.GetRandomCellWithin), dir, 1), 0.2, ProjectileType, fUnit, RangeMax, RangeMin);//aim to the right
              end else
              If TakeBolt then
                gProjectiles.AimTarget(PositionF, fHouse, ProjectileType, fUnit, RangeMax, RangeMin);


              SetLastShootTime; //Record last time the warrior shot
              AnimLength := gRes.Units[UnitType].UnitAnim[uaWork, Direction].Count;
              SetActionLockedStay(AnimLength - FiringDelay, uaWork, False, 0, FiringDelay); //Reload for next attack
              fPhase := 0; //Go for another shot (will be 1 after inc below)
            end
            else
            begin
              AnimLength := gRes.Units[UnitType].UnitAnim[uaWork, Direction].Count;
              if fUnit.UnitType = utPyro then
                fHouse.SetOnFire;

              if fUnit.UnitType = utTorchMan then
              begin
                SetActionLockedStay(0, uaWork, true, 0, 1); // no pause for torchman
                gSpecAnim.Add(gRes.Units.Explosion, fUnit.PositionF, 1, rxUnits, true);
                if not fUnit.IsDeadOrDying then
                  gHands.HitAllInRadius(fUnit, fUnit, fUnit.PositionF, 2.5, 199, 2, 200);
                if not fUnit.IsDeadOrDying then
                  gHands.HitAllInRadius(fUnit, fUnit, fUnit.PositionF, 1.43, 220, 5, 350);
                //if not fUnit.Immortal then
                  fUnit.Kill(-1, false, false);
              end else
                SetActionLockedStay(AnimLength - AnimLength div 2, uaWork, False, 0, AnimLength div 2); // Pause for next attack
              if fUnit is TKMUnitWarriorSpy then
                TKMUnitWarriorSpy(fUnit).SetAttackedTime;//Spy can only attack house

              //All melee units do 2 damage per strike
              if not fUnit.IsDeadOrDying then
                if fUnit.UnitType <> utTorchMan then
                begin
                  if fHouse.HouseType in WALL_HOUSES then
                    fHouse.AddDamage(Max(TKMUnitWarrior(fUnit).DamageHouse div 3, 1), fUnit)
                  else
                    fHouse.AddDamage(TKMUnitWarrior(fUnit).DamageHouse, fUnit);
                end;
              //Play a sound. We should not use KaMRandom here because sound playback depends on FOW and is individual for each player
              if gMySpectator.FogOfWar.CheckTileRevelation(Position.X, Position.Y) >= 255 then
                gSoundPlayer.Play(MeleeSoundsHouse[Random(Length(MeleeSoundsHouse))], PositionF);

              fPhase := 1; //Go for another hit (will be 2 after inc below)
            end;
          end;
    end;

  Inc(fPhase);
end;


procedure TKMTaskAttackHouse.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('TaskAttackHouse');
  SaveStream.Write(fHouse.UID);
end;



constructor TKMTaskAssignToShip.Create(aWarrior: TKMUnit; aShip: TKMUnitWarriorShip);
begin
  Inherited Create(aWarrior);
  fType := uttGoToShip;
  fShip := aShip;
end;

constructor TKMTaskAssignToShip.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fShip, 4);
end;

destructor TKMTaskAssignToShip.Destroy;
begin
  Inherited;
end;

procedure TKMTaskAssignToShip.SyncLoad;
begin
  Inherited;
  fShip := TKMUnitWarriorShip(gHands.GetUnitByUID(Integer(fShip)));
end;

procedure TKMTaskAssignToShip.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fShip.UID);
end;

function TKMTaskAssignToShip.WalkShouldAbandon: Boolean;
begin
  Result := fShip.IsDeadOrDying or (fUnit is TKMUnitWarriorShip) or (fUnit.InShip = fShip) or not fShip.IsCloseToWater;
end;

function TKMTaskAssignToShip.Execute: TKMTaskResult;
var P : TKMPoint;
begin
  Result := trTaskContinues;

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWarrior(fUnit) do
    case fPhase of
      0:  begin
            P := fShip.GetClosestSpot;
            if KMlength(P, Position) > 2 then
              SetActionWalkToSpot(fShip.GetClosestSpot, uaWalk, 2)
            else
              SetActionLockedStay(5, uaWalk);
          end;
      1:  begin
            if fShip.WarriorMustWait then
            begin
              //SetActionStay(50, uaWalk);
              SetActionLockedStay(10, uaWalk);
              fPhase := 0;
            end
            else
              SetActionLockedStay(5, uaWalk);
          end;
      2:  begin
            if fShip.WarriorMustWait then
            begin
              //SetActionStay(50, uaWalk);
              SetActionLockedStay(10, uaWalk);
              fPhase := 0;
            end
            else
            if fShip.CanAssignWarrior(fUnit) then
            begin
              fShip.AssignWarrior(fUnit);
              gScriptEvents.ProcShipLoad(fShip, fUnit);
              {fUnit.Visible := false;
              fUnit.InShip := fShip;}
            end;
            SetActionLockedStay(10, uaWalk);

          end;
    else Result := trTaskDone;

    end;

  Inc(fPhase);
end;

constructor TKMTaskUnloadFromShip.Create(aWarrior: TKMUnit; aShip: TKMUnitWarriorShip);
begin
  Inherited Create(aWarrior);
  fType := uttUnloadFromShip;
  fShip := aShip;
end;


constructor TKMTaskUnloadFromShip.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fShip, 4);
end;

destructor TKMTaskUnloadFromShip.Destroy;
begin
  Inherited;
end;

procedure TKMTaskUnloadFromShip.SyncLoad;
begin
  Inherited;
  fShip := TKMUnitWarriorShip(gHands.GetUnitByUID(Integer(fShip)));
end;

procedure TKMTaskUnloadFromShip.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fShip.UID);
end;

function TKMTaskUnloadFromShip.WalkShouldAbandon: Boolean;
begin
  Result := fSHip.IsDeadOrDying or (fUnit is TKMUnitWarriorShip) or not fShip.IsCloseToWater;
end;

function TKMTaskUnloadFromShip.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWarrior(fUnit) do
    case fPhase of
      0:  begin
            SetActionLockedStay(5, uaWalk);//wait a little
          end;
      1:  begin  //check if unit can be unloaded or not
            if fShip.WarriorMustWaitToUnload then
            begin
              SetActionLockedStay(10, uaWalk);//wait a little
              fPhase := 0;
            end
            else
              SetActionLockedStay(5, uaWalk);
          end;
      2:  begin
            if fShip.WarriorMustWaitToUnload then
            begin
              //SetActionStay(50, uaWalk);
              SetActionLockedStay(10, uaWalk);
              fPhase := 0;
            end
            else
            if fShip.UnloadUnit(fUnit, fShip.GetUnloadingPoint) then
            begin
              SetActionLockedStay(5, uaWalk);
              gScriptEvents.ProcShipUnload(fShip, fUnit);
            end else
            begin
              SetActionLockedStay(5, uaWalk);
              fPhase := 0;
            end;
          end;
      3:   begin
            fUnit.Show;
            SetActionLockedStay(5, uaWalk);
          end
    else Result := trTaskDone;

    end;

  Inc(fPhase);
end;

constructor TKMTaskTakeOverHouse.Create(aWarrior: TKMUnit; aHouse: TKMHouse);
begin
  Inherited Create(aWarrior);
  fType := uttTakeOverHouse;
  fHouse := aHouse;
end;


constructor TKMTaskTakeOverHouse.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
end;

destructor TKMTaskTakeOverHouse.Destroy;
begin
  Inherited;
end;

procedure TKMTaskTakeOverHouse.SyncLoad;
begin
  Inherited;
  fHouse := TKMHouse(gHands.GetHouseByUID(Integer(fHouse)));
end;

procedure TKMTaskTakeOverHouse.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fHouse.UID);
end;

function TKMTaskTakeOverHouse.WalkShouldAbandon: Boolean;
begin
  Result := not fHouse.IsValid(htAny, false, true) or (fHouse.HouseType in WALL_HOUSES);
end;

function TKMTaskTakeOverHouse.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  //If the house is destroyed drop the task
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWarrior(fUnit) do
    case fPhase of
      0:  begin
            SetActionWalkToSpot(fHouse.PointBelowEntrance);
          end;

      1:  begin
            SetActionGoIn(uaWalk, gdGoInside, fHouse);//enter the well
            fHouse.IsClosedForWorker := true;
          end;
      2:  begin
            SetActionLockedStay(20, uaWalk);
          end;
      3:  begin
            SetActionGoIn(uaWalk, gdGoOutside, fHouse);//enter the well
          end;
      4:  begin
            Direction := dirN;
            SetActionLockedStay(gRes.Units[fUnit.UnitType].UnitAnim[uaWork, dirN].Count - 1, uaWork, false);
          end;
      5:  begin
            SetActionLockedStay(1, uaWalk);
            gHands[Owner].TakeOverHouse(fHouse);
          end;
    else Result := trTaskDone;

    end;

  Inc(fPhase);
end;

end.
