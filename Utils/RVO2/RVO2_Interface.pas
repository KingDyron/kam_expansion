unit RVO2_Interface;
interface
uses Classes, Math, SysUtils, RVO2_Math, RVO2_Simulator, RVO2_Vector2;

type
  TRVO2Agent = class
    MaxSpeed: Single;
    Position: TRVOVector2;
    Radius: Single;
    RoutePos: Integer;
    Route: array of TRVOVector2;
    constructor Create;
    function CurrentGoal: TRVOVector2;
    procedure StepNext;
    function HasGoal: Boolean;
    function NearGoal: Boolean;
  end;

  TRVO2Obstacle = class
    Vertices: array of TRVOVector2;
  end;

  TRVO2 = class
  private
    //Agents defaults
    fneighborDist: Single;
    fmaxNeighbors: Integer;
    ftimeHorizon: Single;
    ftimeHorizonObst: Single;
    fradius: Single;
    fmaxSpeed: Single;

    fAgents: TList;
    fObstacles: TList;

    function GetAgents(I: Integer): TRVO2Agent;
    function GetObstacles(I: Integer): TRVO2Obstacle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAgent(aAgent: TRVO2Agent);
    procedure AddObstacle(v: array of TRVOVector2);
    procedure AddObstacleRect(x,y,w,h: Single);
    function AgentCount: Integer;
    property Agents[I: Integer]: TRVO2Agent read GetAgents;
    function ObstacleCount: Integer;
    property Obstacles[I: Integer]: TRVO2Obstacle read GetObstacles;

    procedure Step;
  end;


implementation


constructor TRVO2Agent.Create;
begin
  MaxSpeed := 2;
  Radius := 2.5;
end;


function TRVO2Agent.CurrentGoal: TRVOVector2;
begin
  if RoutePos < Length(Route) then
    Result := Route[RoutePos]
  else
    Result := Route[Length(Route) - 1];
end;


function TRVO2Agent.HasGoal: Boolean;
begin
  Result := Length(Route) > 0;
end;


procedure TRVO2Agent.StepNext;
begin
  Inc(RoutePos);
end;


function TRVO2Agent.NearGoal: Boolean;
begin
  Result := not HasGoal or (absSq(Vector2Sub(CurrentGoal, Position)) < Sqr(Radius * 2));
end;


{ TRVO2 }
constructor TRVO2.Create();
begin
  inherited;

  fneighborDist := 15;
  fmaxNeighbors := 10;
  ftimeHorizon := 10;
  ftimeHorizonObst := 5;
  fradius := 2;
  fmaxSpeed := 2;

  fAgents := TList.Create;
  fObstacles := TList.Create;

  gSimulator := TRVOSimulator.Create;
  gSimulator.Clear;
  gSimulator.setTimeStep(0.2);
end;


destructor TRVO2.Destroy;
begin
  FreeAndNil(gSimulator);
  fAgents.Free;
  fObstacles.Free;

  inherited;
end;


procedure TRVO2.AddAgent(aAgent: TRVO2Agent);
begin
  gSimulator.setAgentDefaults(fneighborDist, fmaxNeighbors, ftimeHorizon, ftimeHorizonObst, aAgent.Radius, aAgent.MaxSpeed, Vector2(0,0));
  gSimulator.addAgent(aAgent.Position);
  fAgents.Add(aAgent);
end;


// Add (polygonal) obstacle(s), specifying vertices in counterclockwise order.
procedure TRVO2.AddObstacle(v: array of TRVOVector2);
var
  O: TRVO2Obstacle;
begin
  O := TRVO2Obstacle.Create;
  SetLength(O.Vertices, Length(v));

  Move(v[0], O.Vertices[0], Length(v)*SizeOf(TRVOVector2));

  gSimulator.addObstacle(O.Vertices);

  gSimulator.processObstacles;

  fObstacles.Add(O);
end;


procedure TRVO2.AddObstacleRect(x, y, w, h: Single);
var
  O: TRVO2Obstacle;
begin
  O := TRVO2Obstacle.Create;
  SetLength(O.Vertices, 4);

  // Add (polygonal) obstacle(s), specifying vertices in counterclockwise order.
  O.Vertices[0] := Vector2(  x,   y);
  O.Vertices[1] := Vector2(x+w,   y);
  O.Vertices[2] := Vector2(x+w, y+h);
  O.Vertices[3] := Vector2(  x, y+h);

  gSimulator.addObstacle(O.Vertices);

  gSimulator.processObstacles;

  fObstacles.Add(O);
end;


function TRVO2.AgentCount: Integer;
begin
  Result := gSimulator.getNumAgents;
end;


function TRVO2.GetAgents(I: Integer): TRVO2Agent;
begin
  Result := fAgents[I];
end;


function TRVO2.GetObstacles(I: Integer): TRVO2Obstacle;
begin
  Result := fObstacles[I];
end;


function TRVO2.ObstacleCount: Integer;
begin
  Result := fObstacles.Count;
end;


procedure TRVO2.Step;
  procedure setPreferredVelocities;
  var
    i: Integer;
    angle,dist: Single;
  begin
    // Set the preferred velocity for each agent.
    for i := 0 to gSimulator.getNumAgents - 1 do
    begin
      Agents[I].Position := gSimulator.getAgentPosition(i);
      Agents[I].Position := gSimulator.getAgentPosition(i);
      if Agents[I].NearGoal then
      begin
        // Agent is within one radius of its goal, set preferred velocity to zero
        gSimulator.setAgentPrefVelocity(i, Vector2(0, 0));
        Agents[I].StepNext;
      end
      else

      {  if InRange(absSq(gSimulator.getAgentVelocity(i)), 0.0001, 0.1) then
          gSimulator.setAgentPrefVelocity(i, Vector2(Random, Random))
        else}
          // Agent is far away from its goal, set preferred velocity as unit vector towards agent's goal.
          gSimulator.setAgentPrefVelocity(i, normalize(Vector2Sub(Agents[i].CurrentGoal, gSimulator.getAgentPosition(i))));

		{// Perturb a little to avoid deadlocks due to perfect symmetry.
		angle := Random * 2 * Pi;
		dist := Random * 0.01;

		gSimulator.setAgentPrefVelocity(i, Vector2Add(gSimulator.getAgentPrefVelocity(i),
		                         Vector2Scale(dist, Vector2(cos(angle), sin(angle)))));}

    end;
  end;
begin
  setPreferredVelocities;

  gSimulator.doStep;
end;


end.
