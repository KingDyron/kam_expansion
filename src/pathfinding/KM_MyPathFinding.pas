unit KM_MyPathFinding;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Math,
  KM_Defaults, KM_CommonClasses,
  KM_Points, VCL.Dialogs;

type
  TKMMNode = record
    TCost: Single;
    SCost, DCost : Single;//cost from start, cost to dest
    Parent : TKMPoint// where we came from
  end;

  TKMPathFindingNew = class
    private
      fMapX, fMapY : Word;
      fNodeMap : array of array of TKMMNode;
      fList : TKMPointList;
      procedure Clear;
    public
      constructor Create;
      Destructor Destroy; override;
      procedure NewMap(X,Y : Integer);
      procedure MakePath(aStart, aDestination : TKMPoint; out aList : TKMPointList; DoDiagonal : Boolean = true);
  end;

implementation
uses KM_Terrain, KM_TerrainTypes;

constructor TKMPathFindingNew.Create;
begin
  Inherited;
  fList := TKMpointList.Create;
end;

destructor TKMPathFindingNew.Destroy;
begin
  FreeAndNil(fList);
  Inherited;
end;

procedure TKMPathFindingNew.NewMap(X: Integer; Y: Integer);
begin
  fMapX := X;
  fMapY := Y;
  SetLength(fNodeMap, fMapX, fMapY);
end;

procedure TKMPathFindingNew.Clear;
var I, K : Integer;
begin
  fList.Clear;
  for I := 1 to fMapX - 1 do
    for K := 1 to fMapY - 1 do
    begin
      gTerrain.SetObject(KMPoint(I, K), 255);
      fNodeMap[I,K].SCost := 10000;
      fNodeMap[I,K].DCost := 10000;
      fNodeMap[I,K].TCost := fNodeMap[I,K].SCost + fNodeMap[I,K].DCost;
      fNodeMap[I,K].Parent := KMPOINT_INVALID_TILE;
    end;

end;

procedure TKMPathFindingNew.MakePath(aStart: TKMPoint; aDestination: TKMPoint; out aList: TKMPointList; DoDiagonal : Boolean = true);
  function GetClosest : TKMPoint;
  var I : Integer;
      P : TKMPoint;
  begin
    Result := KMPOINT_ZERO;
    for I := 0 to fList.Count - 1 do
    begin
      P := fList[I];
      if (Result <> KMPOINT_ZERO) then
      begin
        if fNodeMap[P.X,P.Y].TCost < fNodeMap[Result.X,Result.Y].TCost then
          Result := P;

      end else
      begin
        Result := fList[I];
      end;

    end;

      
  end;

  procedure MakePath;
  var P : TKMPoint;
  begin
    P := aDestination;
    while (P <> aStart) and (P <> KMPOINT_ZERO) do
    begin
      aList.Add(P);
      gTerrain.SetObject(P, 305);
      P := fNodeMap[P.X, P.Y].Parent;
    end;

  end;

  procedure AddNode(P, Parent : TKMPoint; aDistance : Single);
  begin
    if not InRange(P.X, 1, fMapX - 1) then
      Exit;
    if not InRange(P.Y, 1, fMapY - 1) then
      Exit;

    if fNodeMap[Parent.X, Parent.Y].SCost + aDistance >= fNodeMap[P.X, P.Y].SCost then
      Exit;
    if not gTerrain.CheckPassability(P, tpWalk) then
      Exit;

    fList.Add(P);
    fNodeMap[P.X, P.Y].Parent := Parent;
    fNodeMap[P.X, P.Y].SCost := fNodeMap[Parent.X, Parent.Y].SCost + aDistance;
    fNodeMap[P.X, P.Y].DCost := KMLengthDiag(aDestination, P);//Abs(aDestination.X - P.X) + Abs(aDestination.Y - P.Y);
    fNodeMap[P.X, P.Y].TCost := fNodeMap[P.X, P.Y].SCost + fNodeMap[P.X, P.Y].DCost;

  end;
var P : TKMPoint;
begin
  Clear;
  aList.Clear;
  fList.Add(aStart);
  fNodeMap[aStart.X, aStart.Y].TCost := 0;
  fNodeMap[aStart.X, aStart.Y].SCost := 0;
  fNodeMap[aStart.X, aStart.Y].DCost := 0;

  while not fList.IsEmpty do
  begin
    P := GetClosest;
    if P = aDestination then
    begin
      //ShowMessage('Found');
      MakePath;
      Break;
    end;
    fList.Remove(P);

    gTerrain.SetObject(P, 277);


    AddNode(KMPoint(P.X - 1, P.Y), P, 1);
    AddNode(KMPoint(P.X + 1, P.Y), P, 1);
    AddNode(KMPoint(P.X, P.Y - 1), P, 1);
    AddNode(KMPoint(P.X, P.Y + 1), P, 1);
    if DoDiagonal then
    begin
      AddNode(KMPoint(P.X - 1, P.Y - 1), P, 1.41);
      AddNode(KMPoint(P.X - 1, P.Y + 1), P, 1.41);
      AddNode(KMPoint(P.X + 1, P.Y - 1), P, 1.41);
      AddNode(KMPoint(P.X + 1, P.Y + 1), P, 1.41);
    end;

  end;

end;

end.
