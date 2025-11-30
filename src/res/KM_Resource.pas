unit KM_Resource;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, SysUtils,
  KM_CommonTypes, KM_Defaults,
  KM_ResTypes,
  KM_ResCursors,
  KM_ResFonts,
  KM_ResHouses,
  KM_ResLocales,
  KM_ResMapElements,
  KM_ResPalettes,
  KM_ResSound,
  KM_ResSprites,
  KM_ResTileset,
  KM_ResUnits,
  KM_ResWares,
  KM_ResStructures,
  KM_JsonData,
  KM_TerrainTypes,
  KM_ResPatterns,
  KM_ResDevelopment,
  KM_ResCosmetics;


type
  TResourceLoadState = (rlsNone, rlsMenu, rlsAll); //Resources are loaded in 2 steps, for menu and the rest

  TKMResource = class
  private
    fDataState: TResourceLoadState;
    fJson : TKMJsonData;
    fCursors: TKMResCursors;
    fFonts: TKMResFonts;
    fHouses: TKMResHouses;
    fPalettes: TKMResPalettes;
    fUnits: TKMResUnits;
    fWares: TKMResWares;
    fSounds: TKMResSounds;
    fSprites: TKMResSprites;
    fTileset: TKMResTileset;
    fMapElements: TKMResMapElements;
    fStructures : TKMResStructures;
    fPatterns : TKMResPatterns;
    fDevelopment : TKMDevelopmentTreeCollection;
    fCosmetics : TKMResCosmetics;

  public
    OnLoadingStep: TEvent;
    OnLoadingText: TUnicodeStringEvent;

    constructor Create(aOnLoadingStep: TEvent; aOnLoadingText: TUnicodeStringEvent);
    destructor Destroy; override;

    function GetDATCRC: Cardinal;
    procedure StepRefresh;
    procedure StepCaption(const aCaption: UnicodeString);

    procedure LoadMainResources(const aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
    procedure LoadLocaleAndFonts(const aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
    procedure LoadLocaleResources(const aLocale: AnsiString = '');
    procedure LoadGameResources(aAlphaShadows: Boolean; aForceReload: Boolean = False);
    procedure LoadLocaleFonts(const aLocale: AnsiString; aLoadFullFonts: Boolean);

    property JsonData: TKMJsonData read fJson;
    property DataState: TResourceLoadState read fDataState;
    property Palettes: TKMResPalettes read fPalettes;
    property Cursors: TKMResCursors read fCursors;
    property MapElements: TKMResMapElements read fMapElements;
    property Fonts: TKMResFonts read fFonts;
    property Sounds: TKMResSounds read fSounds;
    property Sprites: TKMResSprites read fSprites;
    property Tileset: TKMResTileset read fTileset;
    property Houses: TKMResHouses read fHouses;
    property Units: TKMResUnits read fUnits;
    property Wares: TKMResWares read fWares;
    property Structures: TKMResStructures read fStructures;
    property Patterns: TKMResPatterns read fPatterns;
    property Development: TKMDevelopmentTreeCollection read fDevelopment;
    property Cosmetics: TKMResCosmetics read fCosmetics;
    procedure ReloadJSONData(UpdateCRC : Boolean; aEvent : TAnsiStringEvent);


    procedure UpdateStateIdle;

    function IsMsgHouseUnnocupied(aMsgId: Word): Boolean;
  end;


var
  gRes: TKMResource;


implementation
uses
  IOUtils,
  TypInfo,
  {$IFNDEF NO_OGL}
  KM_System,
  {$ENDIF}
  KromUtils, KM_Log, KM_Points,
  KM_ResTexts, KM_ResKeyFuncs, KM_ResTilesetTypes, KM_CommonClasses,
  Math,
  KM_GameApp;


{ TKMResource }
constructor TKMResource.Create(aOnLoadingStep: TEvent; aOnLoadingText: TUnicodeStringEvent);
begin
  inherited Create;

  fDataState := rlsNone;
  gLog.AddTime('Resource loading state - None');

  OnLoadingStep := aOnLoadingStep;
  OnLoadingText := aOnLoadingText;
end;


destructor TKMResource.Destroy;
begin
  FreeAndNil(fCursors);
  FreeAndNil(fHouses);
  FreeAndNil(fJson);
  FreeAndNil(gResLocales);
  FreeAndNil(fMapElements);
  FreeAndNil(fPalettes);
  FreeAndNil(fFonts);
  FreeAndNil(fWares);
  FreeAndNil(fSprites);
  FreeAndNil(fSounds);
  FreeAndNil(gResTexts);
  FreeAndNil(fTileset);
  FreeAndNil(fUnits);
  FreeAndNil(gResKeyFuncs);
  FreeAndNil(fStructures);
  FreeAndNil(fPatterns);
  FreeAndNil(fDevelopment);
  FreeAndNil(fCosmetics);

  inherited;
end;


procedure TKMResource.UpdateStateIdle;
begin
  fSprites.UpdateStateIdle;
end;


procedure TKMResource.StepRefresh;
begin
  if Assigned(OnLoadingStep) then OnLoadingStep;
end;


procedure TKMResource.StepCaption(const aCaption: UnicodeString);
begin
  if Assigned(OnLoadingText) then OnLoadingText(aCaption);
end;


//CRC of data files that can cause inconsitencies
function TKMResource.GetDATCRC: Cardinal;
begin
  Result := fHouses.CRC xor
            fUnits.CRC xor
            fMapElements.CRC xor
            fTileset.CRC xor
            fWares.CRC xor
            fStructures.CRC xor
            fDevelopment.CRC
            //xor fSprites.CRC //skip it
            ;
end;


procedure TKMResource.LoadMainResources(const aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
var
  tileColors: TKMColor3bArray;
begin
  StepCaption('Reading palettes ...');
  fPalettes := TKMResPalettes.Create;
  //We are using only default palette in the game for now, so no need to load all palettes
  fPalettes.LoadDefaultPalette(ExeDir + 'data' + PathDelim + 'gfx' + PathDelim);
  gLog.AddTime('Reading palettes', True);
  fJson := TKMJsonData.Create;

  fSprites := TKMResSprites.Create(StepRefresh, StepCaption);
  fCursors := TKMResCursors.Create;

  fUnits := TKMResUnits.Create; // Load units prior to Sprites, as we could use it on SoftenShadows override for png in Sprites folder
  fSprites.LoadMenuResources;

  {$IFNDEF NO_OGL}
  gSystem.MakeCursors(fSprites[rxGui]);
  gSystem.Cursor := kmcDefault;
  {$ENDIF}
  fCursors.SetRXDataPointer(@fSprites[rxGui].RXData);

  gResKeyFuncs := TKMResKeyFuncs.Create;

  LoadLocaleAndFonts(aLocale, aLoadFullFonts);

  fTileset := TKMResTileset.Create;
  if not SKIP_RENDER then
  begin
    tileColors := fSprites.Sprites[rxTiles].GetAverageSpriteColors(TILES_CNT);
    fTileset.SetTileColors(tileColors);
  end;
  fMapElements := TKMResMapElements.Create;
  fMapElements.LoadFromFile(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'mapelem.dat');

  fSprites.ClearTemp;
  fWares := TKMResWares.Create;
  fHouses := TKMResHouses.Create;
  fStructures := TKMResStructures.Create;
  fUnits.SetUnitHousesList;
  fPatterns := TKMResPatterns.Create;
  StepRefresh;
  gLog.AddTime('ReadGFX is done');
  fDataState := rlsMenu;

  fDevelopment := TKMDevelopmentTreeCollection.Create;
  fDevelopment.LoadFromJson(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'DevelopmentTree.Json');

  fCosmetics := TKMResCosmetics.Create;

  gLog.AddTime('Resource loading state - Menu');

  fUnits.ExportCSV(ExeDir+'Export\Units.csv');
  fWares.ExportCSV(ExeDir+'Export\Wares.csv');
  fHouses.ExportCSV(ExeDir+'Export\Houses.csv');
  fMapElements.AfterResourceLoad;

end;


procedure TKMResource.LoadLocaleResources(const aLocale: AnsiString = '');
begin
  FreeAndNil(gResLocales);
  FreeAndNil(gResTexts);
  FreeAndNil(fSounds);

  gResLocales := TKMResLocales.Create(ExeDir + 'data' + PathDelim + 'locales.txt', aLocale);

  gResTexts := TKMTextLibraryMulti.Create;
  gResTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');
  //gResTexts.SaveToKamStarLanguage(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.kam.libx');
  fSounds := TKMResSounds.Create(gResLocales.UserLocale, gResLocales.FallbackLocale, gResLocales.DefaultLocale);
end;


procedure TKMResource.LoadLocaleAndFonts(const aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
begin
  // Locale info is needed for DAT export and font loading
  LoadLocaleResources(aLocale);

  StepCaption('Reading fonts ...');
  fFonts := TKMResFonts.Create;
  if aLoadFullFonts or gResLocales.LocaleByCode(aLocale).NeedsFullFonts then
    fFonts.LoadFonts(fllFull)
  else
    fFonts.LoadFonts(fllMinimal);
  gLog.AddTime('Read fonts is done');
end;


procedure TKMResource.LoadLocaleFonts(const aLocale: AnsiString; aLoadFullFonts: Boolean);
begin
  if (Fonts.LoadLevel <> fllFull)
    and (aLoadFullFonts or gResLocales.LocaleByCode(aLocale).NeedsFullFonts) then
    Fonts.LoadFonts(fllFull);
end;


procedure TKMResource.LoadGameResources(aAlphaShadows: Boolean; aForceReload: Boolean = False);
var
  doForceReload: Boolean;
begin

  gLog.AddTime('LoadGameResources ... AlphaShadows: ' + BoolToStr(aAlphaShadows, True) + '. Forced: ' + BoolToStr(aForceReload, True));
  doForceReload := aForceReload or (aAlphaShadows <> fSprites.AlphaShadows);
  if (fDataState <> rlsAll)
    {$IFDEF LOAD_GAME_RES_ASYNC}or not fSprites.GameResLoadCompleted {$ENDIF}
    or doForceReload then
  begin
    // Load game Reources
    // TempData is cleared while loading GameResources (after each step)
    fSprites.LoadGameResources(aAlphaShadows, doForceReload);



    fDataState := rlsAll;
  end;

  //fSprites[rxCustom].AddImage('E:\programowanie\projekty delphi\','8_0010.png', fSprites[rxCustom].RXData.Count + 1, false);
  gLog.AddTime('Resource loading state - Game');
end;


function TKMResource.IsMsgHouseUnnocupied(aMsgId: Word): Boolean;
begin
  Result := (aMsgId >= TX_MSG_HOUSE_UNOCCUPIED__22) and (aMsgId <= TX_MSG_HOUSE_UNOCCUPIED__22 + 22);
end;

procedure TKMResource.ReloadJSONData(UpdateCRC: Boolean; aEvent : TAnsiStringEvent);
begin

  fJson.Reload;
  //fSprites.Free;
  //fSprites := TKMResSprites.Create(nil, nil);
  //fSprites.LoadMenuResources;
  if Assigned(aEvent) then aEvent('Reloading Data|Sprites');
    fSprites.LoadGameResources(true, true);

  if Assigned(aEvent) then aEvent('Reloading Data|Sprites|Houses');
  fHouses.ReloadJSONData(UpdateCRC);
  if Assigned(aEvent) then aEvent('Reloading Data|Sprites|Houses|Bridges');
  fStructures.ReloadJSONData(UpdateCRC);
  if Assigned(aEvent) then aEvent('Reloading Data|Sprites|Houses|Bridges|Units');
  fUnits.ReloadJSONData(UpdateCRC);
  if Assigned(aEvent) then aEvent('Reloading Data|Sprites|Houses|Bridges|Units|Objects');
  fMapElements.ReloadJSONData(UpdateCRC);
  if Assigned(aEvent) then aEvent('Reloading Data|Sprites|Houses|Bridges|Units|Objects|Wares');
  fWares.ReloadJSONData(UpdateCRC);
  if Assigned(aEvent) then aEvent('Reloading Data|Sprites|Houses|Bridges|Units|Objects|Wares||DONE');

  fMapElements.AfterResourceLoad;

end;



end.
