unit KM_ResCosmetics;
{$I KaM_Remake.inc}
interface
uses
  Classes, SyncObjs,
  KM_GameOptions, KM_GameInfo, KM_Minimap;

type
  TKMGuiStyleRec = record
    Background, //rxGuiMain
    MainImage, //rxGuiMain
    MiniMap,  //rxGui
    LeftPanel,  //rxGui
    Button : Word; //rxGuiMain
    LeftTex,
    RightTex : Word;
    TextID,
    MainText : Word;
    procedure SetDefault;
  end;

  TKMGuiStyleRecArray = array of TKMGuiStyleRec;

  TKMResCosmetics = class
  private
    fGuiStyles : TKMGuiStyleRecArray;

    procedure AddNewGuiStyle(aTextID, aBack, aMain, aMini, aLeft, aButton, aLeftT, aRightT, aMainText : Word);
    function GetCurrentStyle : TKMGuiStyleRec;
    function GetGuiStyle(aIndex : Integer) : TKMGuiStyleRec;

    procedure LoadFromJSON;
  public
    constructor Create;

    property CurrentGuiStyle : TKMGuiStyleRec read GetCurrentStyle;
    property GuiStyle[aIndex : Integer] : TKMGuiStyleRec read GetGuiStyle;   Default;
    function GuiStylesCount : Word;
  end;

implementation
uses SysUtils,
  KM_Defaults, KM_MainSettings, KM_JsonHelpers;

procedure TKMGuiStyleRec.SetDefault;
begin
    Background := 17;
    MainImage := 18;
    MiniMap := 407;
    LeftPanel := 404;
    Button := 9;
    TextID := 2313;
    LeftTex := 5;
    RightTex := 6;
    MainText := 4;
end;

constructor TKMResCosmetics.Create;
begin
  Inherited;
  //add default
  AddNewGuiStyle(2313, 17, 18, 407, 404, 9, 5, 6, 4);
  //bricks
  //AddNewGuiStyle(2314, 106, 107, 1178, 1177, 109);
  LoadFromJSON;
end;

function TKMResCosmetics.GuiStylesCount: Word;
begin
  Result := length(fGuiStyles);
end;

function TKMResCosmetics.GetCurrentStyle: TKMGuiStyleRec;
begin
  Result := fGuiStyles[gMainSettings.GUIStyle];
end;

function TKMResCosmetics.GetGuiStyle(aIndex: Integer): TKMGuiStyleRec;
begin
  Result := fGuiStyles[aIndex];
end;

procedure TKMResCosmetics.AddNewGuiStyle(aTextID, aBack: Word; aMain: Word; aMini: Word; aLeft: Word; aButton, aLeftT, aRightT, aMainText: Word);
var I : integer;
begin
  I := length(fGuiStyles);
  SetLength(fGuiStyles, I + 1);

  with fGuiStyles[I] do
  begin
    SetDefault;
    If aTextID > 0 then       TextID := aTextID;
    If aBack > 0 then         Background := aBack;
    If aMain > 0 then         MainImage := aMain;

    If aMini > 0 then         MiniMap := aMini;
    If aLeft > 0 then         LeftPanel := aLeft;
    If aButton > 0 then       Button := aButton;

    LeftTex := aLeftT;
    RightTex := aRightT;
    If aMainText > 0 then MainText := aMainText;
  end;

end;

procedure TKMResCosmetics.LoadFromJSON;
var root, jCosm : TKMJsonObject;
  jArr : TKMJsonArrayNew;
  path : String;
  I : integer;
begin
  path := ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'Cosmetics.Json';

  If not FileExists(path) then
    Exit;
  root := TKMJsonObject.Create;

  root.LoadFromFile(path);

    jArr :=  root.A['GuiStyles'];
    for I := 0 to jArr.Count - 1 do
    begin
      jCosm := jArr.O[I];
      AddNewGuiStyle(jCosm.I['TextID'],
                    jCosm.I['Background'],
                    jCosm.I['Main'],
                    jCosm.I['Minimap'],
                    jCosm.I['LeftPanel'],
                    jCosm.I['Button'],
                    jCosm.I['LeftTexture'],
                    jCosm.I['RightTexture'],
                    jCosm.I['MainText']);
    end;

  FreeAndNil(root);
end;

end.
