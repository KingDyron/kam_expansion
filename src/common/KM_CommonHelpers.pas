unit KM_CommonHelpers;
{$I KaM_Remake.inc}
interface
uses KM_MapTypes,
      KM_Hand,
      KM_Houses, KM_Units, KM_UnitWarrior;
type
  TKMWordHelper = record helper for Word
    function ToResText : String;
    function ToMBDDamage : Word;
    function ToMBDHDamage : Word;
  end;

  TKMMissionBuiltInDifficultyHelper = record helper for TKMMissionBuiltInDifficulty
    function IsEasy : Boolean;
    function IsNormal : Boolean;
    function IsHard : Boolean;
    function IsRealism : Boolean;
    function IsHardOrRealism : Boolean;
  end;

  TKMPointerHelper = record helper for Pointer
    function ToHand : TKMHand;
    function ToHouse : TKMHouse;
    function ToUnit : TKMUnit;
    function ToWarrior : TKMUnitWarrior;
    function ToRecruit : TKMUnitRecruit;
    function ToCitizen : TKMUnitCitizen;
  end;

implementation

uses
    Math,
    KM_ResTexts, KM_GameParams;

function TKMWordHelper.ToResText: string;
begin
  Result := gResTexts[self];
end;

function TKMWordHelper.ToMBDDamage: Word;
begin
  Result := self;
  case gGameParams.MBD of
    mdbEasy : Result := self * 2;
    mdbNormal : ;
    mdbHard,
    mdbRealism : Result := Max(Round(self * 0.75), 1);
  end;
end;

function TKMWordHelper.ToMBDHDamage: Word;
begin
  Result := self;
  case gGameParams.MBD of
    mdbEasy : Result := self * 2;
    mdbNormal : ;
    mdbHard,
    mdbRealism : Result := Max(Round(self * 0.75), 1);
  end;
end;

function TKMMissionBuiltInDifficultyHelper.IsEasy: Boolean;
begin
  Result := self = mdbEasy;
end;
function TKMMissionBuiltInDifficultyHelper.IsNormal: Boolean;
begin
  Result := self = mdbNormal;
end;
function TKMMissionBuiltInDifficultyHelper.IsHard: Boolean;
begin
  Result := self = mdbHard;
end;
function TKMMissionBuiltInDifficultyHelper.IsRealism: Boolean;
begin
  Result := self = mdbRealism;
end;

function TKMMissionBuiltInDifficultyHelper.IsHardOrRealism: Boolean;
begin
  Result := (self = mdbHard) or (self = mdbRealism);
end;

function TKMPointerHelper.ToHand: TKMHand;                begin Result := TKMHand(Self); end;
function TKMPointerHelper.ToHouse: TKMHouse;              begin Result := TKMHouse(Self); end;
function TKMPointerHelper.ToUnit: TKMUnit;                begin Result := TKMUnit(Self); end;
function TKMPointerHelper.ToWarrior: TKMUnitWarrior;      begin Result := TKMUnitWarrior(Self); end;
function TKMPointerHelper.ToRecruit: TKMUnitRecruit;      begin Result := TKMUnitRecruit(Self); end;
function TKMPointerHelper.ToCitizen: TKMUnitCitizen;      begin Result := TKMUnitCitizen(Self); end;

end.
