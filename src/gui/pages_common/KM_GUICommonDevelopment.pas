unit KM_GUICommonDevelopment;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_CommonTypes, KM_Defaults, KM_ResTypes, KM_ResDevelopment;

type
  TKMDevButton = record
    Button_Tree : TKMButtonFlat;
    Next : array of TKMDevButton;
    Dev : PKMDevelopment;
  end;
  TKMGUICommonDevelopment = class(TKMPanel)
  private
    fLastPage : TKMDevelopmentTreeType;
    procedure SwitchPage(Sender : TObject); virtual;
    procedure HideFromID(aID : Integer);
    procedure ButtonClicked(Sender : TObject);
  protected
    Button_SwitchTree : array[TKMDevelopmentTreeType] of TKMButton;
      Tree : array[TKMDevelopmentTreeType] of record
        fCount : Word;
        Panel : TKMScrollPanel;
          ButtonsList : array of TKMButtonFlat;
          Button_Tree : TKMDevButton;
      end;

  public
    OnButtonClicked : TNotifyEvent;
    constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);

    procedure Paint; override;

    procedure ReloadTrees(aCurrentPageOnly : Boolean = true);
    procedure RefreshButton(aTag : Pointer);
    property CurrentPage : TKMDevelopmentTreeType read fLastPage;
  end;

implementation
uses
  SysUtils, Math, KM_UtilsExt,
  KM_Points,
  KM_ResSound, KM_InterfaceGame,
  KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Resource;

Const DISTANCE_BETWEEN_ROWS = 50;
constructor TKMGUICommonDevelopment.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
const
  TREE_TYPE_ICON : array[TKMDevelopmentTreeType] of Word = (39, 322);
var dtt : TKMDevelopmentTreeType;
  procedure CreateNext(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton;  aDevelopment : PKMDevelopment; aTop : Byte);
  var I: integer;
  begin
    with Tree[aType] do
    begin
      aToButton.Button_Tree := TKMButtonFlat.Create(Panel, 3 + aDevelopment.X * 34, aTop * DISTANCE_BETWEEN_ROWS, 31, 31, aDevelopment.GuiIcon);
      aToButton.Button_Tree.Hint := gRes.Development.GetText(aDevelopment.HintID);
      aToButton.Button_Tree.BackAlpha := 1;
      aToButton.Button_Tree.Tag := fCount;
      aToButton.Button_Tree.Tag := Integer(aDevelopment);
      aToButton.Button_Tree.OnClick := ButtonClicked;
      aToButton.Dev := aDevelopment;
      //aToButton.ID := fCount;
      Inc(fCount);
      If fCount > length(ButtonsList) then
        SetLength(ButtonsList, fCount + 32);
      ButtonsList[fCount - 1] := aToButton.Button_Tree;
    end;
    SetLength(aToButton.Next, length(aDevelopment.Next));
    for I := 0 to High(aDevelopment.Next) do
      CreateNext(aType, aToButton.Next[I], @aDevelopment.Next[I], aTop + 1);
  end;
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  AnchorsStretch;
  fLastPage := dttEconomy;
  for dtt := Low(TKMDevelopmentTreeType) to High(TKMDevelopmentTreeType) do
  begin
    Button_SwitchTree[dtt] := TKMButton.Create(self, byte(dtt) * 37, 5, 33, 33, TREE_TYPE_ICON[dtt], rxGui, bsPaper);
    Button_SwitchTree[dtt].Tag := byte(dtt);
    Button_SwitchTree[dtt].OnClick := SwitchPage;

    Tree[dtt].Panel := TKMScrollPanel.Create(self, 3, 42, Width - 6, Height - 42 - 5, [saVertical], bsMenu, ssGame);
    Tree[dtt].Panel.AnchorsStretch;
    Tree[dtt].Panel.ChildPanel.AnchorsStretch;
    CreateNext(dtt, Tree[dtt].Button_Tree, gRes.Development[dtt].FirstItem, 0);
    Tree[dtt].Panel.Visible := fLastPage = dtt;

    //SetLength(Tree[dtt].ButtonsList, Tree[dtt].fCount);
  end;
  //HideFromID(10);
end;

procedure TKMGUICommonDevelopment.HideFromID(aID: Integer);
  procedure CheckButtons(var aButton: TKMDevButton; aShow : Boolean);
  var I : integer;
    doShow : Boolean;
  begin
    doShow := true;
    If aButton.Button_Tree.Tag = aID then
      doShow := false;

    aButton.Button_Tree.Visible := aShow and doShow;

    for I := 0 to High(aButton.Next) do
      CheckButtons(aButton.Next[I], aShow and doShow);
  end;
begin
  CheckButtons(Tree[fLastPage].Button_Tree, true);
end;

procedure TKMGUICommonDevelopment.SwitchPage(Sender: TObject);
var dtt : TKMDevelopmentTreeType;
begin
  fLastPage := TKMDevelopmentTreeType(TKMControl(Sender).Tag);
  for dtt := Low(TKMDevelopmentTreeType) to High(TKMDevelopmentTreeType) do
  begin
    Button_SwitchTree[dtt].ShowImageEnabled := fLastPage = dtt;
    Tree[dtt].Panel.Visible := fLastPage = dtt;
  end;

end;

procedure TKMGUICommonDevelopment.ButtonClicked(Sender: TObject);
begin
  If Assigned(OnButtonClicked) then
    OnButtonClicked(Sender);
end;

procedure TKMGUICommonDevelopment.ReloadTrees(aCurrentPageOnly : Boolean = true);
var dtt : TKMDevelopmentTreeType;
    oldCount : Integer;

  procedure CreateNext(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton; aDevelopment : PKMDevelopment; aTop : Byte);
  var I: integer;
  begin
    with Tree[aType] do
    begin
      //aToButton.ID := fCount;
      Inc(fCount);
      If fCount > length(ButtonsList) then
        SetLength(ButtonsList, fCount + 32);

      If ButtonsList[fCount - 1] = nil then
        ButtonsList[fCount - 1] := TKMButtonFlat.Create(Panel, 3 + aDevelopment.X * 34, aTop * DISTANCE_BETWEEN_ROWS, 31, 31, aDevelopment.GuiIcon)
      else
      begin
        ButtonsList[fCount - 1].Left := 3 + aDevelopment.X * 34;
        ButtonsList[fCount - 1].Top := aTop * DISTANCE_BETWEEN_ROWS;
        ButtonsList[fCount - 1].TexID := aDevelopment.GuiIcon;
      end;
      ButtonsList[fCount - 1].Show;

      aToButton.Button_Tree := ButtonsList[fCount - 1];
      aToButton.Button_Tree.Hint := gRes.Development.GetText(aDevelopment.HintID);
      aToButton.Button_Tree.BackAlpha := 1;
      aToButton.Button_Tree.Tag := fCount - 1;
      aToButton.Button_Tree.Tag := Integer(aDevelopment);
      aToButton.Button_Tree.OnClick := ButtonClicked;
      aToButton.Dev := aDevelopment;
    end;
    SetLength(aToButton.Next, length(aDevelopment.Next));
    for I := 0 to High(aDevelopment.Next) do
      CreateNext(aType, aToButton.Next[I], @aDevelopment.Next[I], aTop + 1);
  end;

  procedure ReloadType(aType : TKMDevelopmentTreeType);
  var I : Integer;
  begin
    oldCount := Tree[aType].fCount;
    Tree[aType].fCount := 0;

    for I := 0 to oldCount - 1 do
      Tree[aType].ButtonsList[I].Hide;

    CreateNext(aType, Tree[aType].Button_Tree, gRes.Development[aType].FirstItem, 0);

    SetLength(Tree[aType].ButtonsList, Tree[aType].fCount);
  end;
begin
  If aCurrentPageOnly then
  begin
    ReloadType(fLastPage);
  end else
  for dtt := Low(TKMDevelopmentTreeType) to High(TKMDevelopmentTreeType) do
  begin
    ReloadType(dtt);
  end;
end;

procedure TKMGUICommonDevelopment.RefreshButton(aTag: Pointer);
var I : Integer;
  dev : PKMDevelopment;
begin

  for I := 0 to Tree[fLastPage].fCount - 1 do
    If Tree[fLastPage].ButtonsList[I].Tag = Integer(aTag) then
    begin
      dev := PKMDevelopment(aTag);
      Tree[fLastPage].ButtonsList[I].TexID := dev.GuiIcon;
      Tree[fLastPage].ButtonsList[I].Left := 3 + dev.X * 34;
    end;

end;

procedure TKMGUICommonDevelopment.Paint;
  procedure MakeLine(aFrom : TKMPoint; aTo : TKMDevButton);
  var I : Integer;
    cent : TKMPoint;
  begin
    If not aTo.Button_Tree.Visible then
      Exit;
    cent := aTo.Button_Tree.AbsCenter;
    TKMRenderUI.WriteLine(aFrom.X, aFrom.Y, aFrom.X, (DISTANCE_BETWEEN_ROWS div 2) + aFrom.Y, $FFFFFFFF);//  \/
    TKMRenderUI.WriteLine(aFrom.X, (DISTANCE_BETWEEN_ROWS div 2) + aFrom.Y,
                          cent.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2), $FFFFFFFF);//  <>

    TKMRenderUI.WriteLine(cent.X, cent.Y, cent.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2), $FFFFFFFF);//  /\
    for I := 0 to high(aTo.Next) do
      MakeLine(cent, aTo.Next[I]);
  end;
var I : integer;
begin
  If Visible then
  begin
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, 0.7);
    TKMRenderUI.SetupClipY(AbsTop + 42, AbsTop + Height - 5);
    for I := 0 to high(Tree[fLastPage].Button_Tree.Next) do
    begin
      MakeLine(Tree[fLastPage].Button_Tree.Button_Tree.AbsCenter,
                Tree[fLastPage].Button_Tree.Next[I]);
    end;
    TKMRenderUI.ReleaseClipY;
  end;

  Inherited;
end;


end.

