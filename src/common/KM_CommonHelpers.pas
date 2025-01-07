unit KM_CommonHelpers;
{$I KaM_Remake.inc}
interface
uses KM_MapTypes,
      KM_Hand;
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

function TKMPointerHelper.ToHand: TKMHand;
begin
  Result := TKMHand(Self);
end;

end.
