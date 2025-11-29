unit KM_GUIMapEdPlayerCheck;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsPopUp,
   KM_Defaults, KM_ResTypes,
   KM_Points;

type
  TKMHousesViewer = class(TKMControl)
  private
    fHouses : TKMHouseTypeArray;
    fGap, fColumnsCount : Byte;
  public
    constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
    procedure Paint; override;
  end;

  TKMMapEdPlayerCheck = class(TKMPopUpPanel)
  private
    procedure CheckMissingHouses(sender : TObject);
  protected
    Button_CheckMissing : TKMButton;
    MissingHouses: TKMHousesViewer;
  public
    constructor Create(aParent : TKMPanel);

  end;

implementation
uses
  KM_CommonClasses,
  KM_HandsCollection,
  KM_RenderUI, KM_ResFonts, KM_Resource, KM_ResTexts,
  KM_ResHouses;

constructor TKMMapEdPlayerCheck.Create(aParent : TKMPanel);
begin
  Inherited Create(aParent.MasterPanel, 600, 500, gResTexts[2336]);
  BevelShade.HideParentOnClick;

  Button_CheckMissing := TKMButton.Create(ItemsPanel, 5, 5, 200, 25, gResTexts[2337], bsMenu);
  Button_CheckMissing.OnClick := CheckMissingHouses;

  MissingHouses := TKMHousesViewer.Create(ItemsPanel, 5, 30, ItemsPanel.Width - 40 * 2, 80);
  MissingHouses.fHouses := [htStore, htSchool, htInn];
end;


procedure TKMMapEdPlayerCheck.CheckMissingHouses(Sender : TObject);
var I, C : Integer;
  H, HT : TKMHouseType;
  isMissing : array[TKMHouseType] of Boolean;
  hasAny : Boolean;
  newArr : TKMHouseTypeArray;
begin
  FillChar(isMissing, sizeOf(isMissing), #0);
  newArr := [];
  for I := 0 to High(HOUSE_ID_TO_TYPE) do
  begin
    H := HOUSE_ID_TO_TYPE[I];
    //check if playerhas any house that unlocks this house
    If gMySpectator.Hand.Stats.GetHouseQty(H) > 0 then
    begin
      hasAny := false;

      for HT in gRes.Houses[H].ReleasedBy do
        If gMySpectator.Hand.Stats.GetHouseQty(HT) > 0 then
          hasAny := true;

      If hasAny then
        Continue;

      for HT in gRes.Houses[H].ReleasedBy do
      begin
        If not isMissing[HT] then
        begin
          newArr := newArr + [HT];
        end;
        isMissing[HT] := true;

      end;
    end;
  end;

  //MissingHouses.fHouses := [];


  {newArr := [];
  for HT := HOUSE_MIN to HOUSE_MAX do
    //If not (HT in [htAny, htNone]) then
      If isMissing[HT] then
      begin
        SetLength(newArr, length(newArr) + 1);
        newArr[high(newArr)] := HT;
      end;}

  MissingHouses.fHouses := newArr;




end;



constructor TKMHousesViewer.Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fColumnsCount := Width div 36;
  fGap := Width div fColumnsCount;
end;
procedure TKMHousesViewer.Paint;
var I : Integer;
begin
  Inherited;
  for I := 0 to High(fHouses) do
  If not (fHouses[I] in [htNone, htAny]) then
  begin
    TKMRenderUI.WriteBevel(AbsLeft + (I mod fColumnsCount) * fGap, AbsTop + (I div fColumnsCount) * fGap, 33, 33);
    TKMRenderUI.WritePicture(AbsLeft + (I mod fColumnsCount) * fGap, AbsTop + (I div fColumnsCount) * fGap, 33, 33,
                             [], rxGui, gRes.Houses[fHouses[I]].GuiIcon);
  end;

end;



end.
