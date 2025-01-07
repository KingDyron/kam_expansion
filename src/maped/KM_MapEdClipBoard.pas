unit KM_MapEdClipBoard;
{$I KaM_Remake.inc}
interface
uses
  KM_AIGoals;

type
  TKMMapEdClipboard = class
  private
    fGoals : TKMGoalArray;
  public
    constructor Create;

    function HasGoals : Boolean;
    property Goals : TKMGoalArray read fGoals write fGoals;
  end;



implementation

constructor TKMMapEdClipboard.Create;
begin
  Inherited;
  SetLength(fGoals, 0);
end;

function TKMMapEdClipboard.HasGoals: Boolean;
begin
  Result := length(fGoals) > 0;
end;



end.

