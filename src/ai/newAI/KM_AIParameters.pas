{
Artificial intelligence
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_AIParameters;
{$I KaM_Remake.inc}

interface
uses
  SysUtils
{$IFDEF PARALLEL_RUNNER}, KM_CommonClasses;
{$ELSE};
{$ENDIF}

{$IFDEF PARALLEL_RUNNER}
  procedure LoadGAParameters(LoadStream: TKMemoryStream);
  procedure SaveGAParameters(SaveStream: TKMemoryStream);
{$ENDIF}


// AI parameters in enumerations
type

  TAIPar = (

    ATTACK_ArmyVectorField_START
    ,ATTACK_ArmyVectorField_FindPositions_DistEnemyOffset
    ,ATTACK_ArmyVectorField_FindPositions_DistEnemyOffsetFF
    ,ATTACK_ArmyVectorField_FindPositions_DistEnemyGainFF
    ,ATTACK_ArmyVectorField_FindPositions_DistCloseToEnemy
    ,ATTACK_ArmyVectorField_FindPositions_DistMaxWalk
    ,ATTACK_ArmyVectorField_FindPositions_FollowEnemyVectorFieldUth
    ,ATTACK_ArmyVectorField_FindPositions_AvoidTraffic
    ,ATTACK_ArmyVectorField_FindPositions_RallyPointOffset
    ,ATTACK_ArmyVectorField_EvalClusters_InPlace
    ,ATTACK_ArmyVectorField_EvalClusters_AtAdvantage
    ,ATTACK_ArmyVectorField_EvalClusters_Ambushed
    ,ATTACK_ArmyVectorField_DivideForces_DefendCityAdv
    ,ATTACK_ArmyVectorField_DivideForces_SupportAllyAdv
    ,ATTACK_ArmyVectorField_END

    ,ATTACK_SQUAD_START
    ,ATTACK_SQUAD_ChangeTarget_Delay
    ,ATTACK_SQUAD_MinWalkingDistance
    ,ATTACK_SQUAD_TargetReached_House
    ,ATTACK_SQUAD_TargetReached_Position
    ,ATTACK_SQUAD_TargetReached_RangedSquad
    ,ATTACK_SQUAD_TargetReached_Unit
    ,ATTACK_SQUAD_TrafficDetection_Limit
    ,ATTACK_SQUAD_TrafficDetection_Threshold
    ,ATTACK_SQUAD_END

    ,ATTACK_SUPERVISOR_START
    ,ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
    ,ATTACK_SUPERVISOR_EvalTarget_OpportunityDistGain
    ,ATTACK_SUPERVISOR_EvalTarget_OpportunityGain
    ,ATTACK_SUPERVISOR_EvalTarget_DecreaseRisk
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
    ,ATTACK_SUPERVISOR_END

    ,NAVMESH_PATHFINDING_START
    ,NAVMESH_PATHFINDING_AvoidTraffic
    ,NAVMESH_PATHFINDING_LineLength
    ,NAVMESH_PATHFINDING_AvoidSpecEnemy
    ,NAVMESH_PATHFINDING_END

    ,BUILDER_START
    ,BUILDER_BuildHouse_FieldMaxWork
    ,BUILDER_BuildHouse_RTPMaxWork
    ,BUILDER_BuildHouse_RoadMaxWork
    ,BUILDER_ChHTB_AllWorkerCoef
    ,BUILDER_ChHTB_FractionCoef
    ,BUILDER_ChHTB_FreeWorkerCoef
    ,BUILDER_ChHTB_TrunkBalance
    ,BUILDER_ChHTB_TrunkFactor
    ,BUILDER_CreateShortcuts_MaxWork
    ,BUILDER_Shortage_Gold
    ,BUILDER_Shortage_Stone
    ,BUILDER_Shortage_StoneReserve
    ,BUILDER_Shortage_Trunk
    ,BUILDER_Shortage_Wood
    ,BUILDER_END

    ,EYE_GetForests_START
    ,EYE_GetForests_MaxAB
    ,EYE_GetForests_MinRndSoil
    ,EYE_GetForests_MinTrees
    ,EYE_GetForests_Radius
    ,EYE_GetForests_SPRndOwnLimMax
    ,EYE_GetForests_SPRndOwnLimMin
    ,EYE_GetForests_END

    ,MANAGEMENT_START
    ,MANAGEMENT_CheckUnitCount_SerfGoldCoef
    ,MANAGEMENT_CheckUnitCount_SerfLimit1
    ,MANAGEMENT_CheckUnitCount_SerfLimit2
    ,MANAGEMENT_CheckUnitCount_SerfLimit3
    ,MANAGEMENT_CheckUnitCount_WorkerGoldCoef
    ,MANAGEMENT_GoldShortage
    ,MANAGEMENT_END

    ,PLANNER_FARM_START
    ,PLANNER_FARM_FieldCrit_FlatArea
    ,PLANNER_FARM_FieldCrit_PolyRoute
    ,PLANNER_FARM_FieldCrit_Soil
    ,PLANNER_FARM_FindPlaceForHouse_CityCenter
    ,PLANNER_FARM_FindPlaceForHouse_FlatArea
    ,PLANNER_FARM_FindPlaceForHouse_HouseDist
    ,PLANNER_FARM_FindPlaceForHouse_Route
    ,PLANNER_FARM_PlanFields_CanBuild
    ,PLANNER_FARM_PlanFields_Dist
    ,PLANNER_FARM_PlanFields_ExistField
    ,PLANNER_FARM_END

    ,PLANNER_FindPlaceForHouse_START
    ,PLANNER_FindPlaceForHouse_CityCenter
    ,PLANNER_FindPlaceForHouse_FlatArea
    ,PLANNER_FindPlaceForHouse_HouseDist
    ,PLANNER_FindPlaceForHouse_Route
    ,PLANNER_FindPlaceForHouse_SeedDist
    ,PLANNER_FindPlaceForHouse_SnapCrit
    ,PLANNER_FindPlaceForHouse_END

    ,PLANNER_FindPlaceForQuary_START
    ,MANAGEMENT_CheckStoreWares_Stone
    ,PREDICTOR_WareNeedPerAWorker_Stone
    ,PREDICTOR_WareNeedPerAWorker_StoneOffset
    ,PREDICTOR_WorkerCountCoef
    ,PLANNER_FindPlaceForQuary_StoneLoc_StoneCnt
    ,PLANNER_FindPlaceForQuary_StoneLoc_AlreadyMined
    ,PLANNER_FindPlaceForQuary_StoneLoc_Distance
    ,PLANNER_FindPlaceForQuary_DistCity
    ,PLANNER_FindPlaceForQuary_DistStone
    ,PLANNER_FindPlaceForQuary_DistTimer
    ,PLANNER_FindPlaceForQuary_Obstacle
    ,PLANNER_FindPlaceForQuary_SnapCrit
    ,PLANNER_FindPlaceForQuary_QtyStone
    ,PLANNER_FindPlaceForQuary_END

    ,PLANNER_FOREST_START
    ,PLANNER_FOREST_FindForestAround_MaxDist
    ,PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
    ,PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
    ,PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
    ,PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
    ,PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
    ,PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
    ,PLANNER_FOREST_FindPlaceForWoodcutter_Radius
    ,PLANNER_FOREST_FindPlaceForWoodcutter_Routes
    ,PLANNER_FOREST_FindPlaceForWoodcutter_Soil
    ,PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    ,PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
    ,PLANNER_FOREST_PlaceWoodcutter_DistFromForest
    ,PLANNER_FOREST_END

    ,PLANNER_Snap_START
    ,PLANNER_ObstaclesInHousePlan_Road
    ,PLANNER_ObstaclesInHousePlan_Tree
    ,PLANNER_SnapCrit_Field
    ,PLANNER_SnapCrit_HouseOrRoad
    ,PLANNER_SnapCrit_NoBuild
    ,PLANNER_SnapCrit_ObstacleInEntrance
    ,PLANNER_SnapCrit_Road
    ,PLANNER_SnapCrit_RoadInEntrance
    ,PLANNER_Snap_END

    ,PREDICTOR_START
    ,PREDICTOR_SecondSchool_MinRequiredUnits
    ,PREDICTOR_WareNeedPerAWorker_Wood
    ,PREDICTOR_END

    ,SHORTCUTS_START
    ,SHORTCUTS_BasePrice
    ,SHORTCUTS_Coal
    ,SHORTCUTS_Field
    ,SHORTCUTS_Forest
    ,SHORTCUTS_OtherCase
    ,SHORTCUTS_Road
    ,SHORTCUTS_TurnPenalization
    ,SHORTCUTS_noBuildArea
    ,SHORTCUTS_END

    ,ROADS_START
    ,ROADS_BasePrice
    ,ROADS_Coal
    ,ROADS_Field
    ,ROADS_Forest
    ,ROADS_OtherCase
    ,ROADS_Road
    ,ROADS_TurnPenalization
    ,ROADS_noBuildArea
    ,ROADS_END
  );


// Global constants for AI
//const

// Global variables for AI
{$IFDEF DEBUG_NewAI}
var
{$ELSE}
const
{$ENDIF}


       AI_Par: array[TAIPar] of Single = (
         0.0000000, // ATTACK_ArmyVectorField_START
        23.8595066, // ATTACK_ArmyVectorField_FindPositions_DistEnemyOffset
        17.4091473, // ATTACK_ArmyVectorField_FindPositions_DistEnemyOffsetFF
         1.5993488, // ATTACK_ArmyVectorField_FindPositions_DistEnemyGainFF
        16.9659538, // ATTACK_ArmyVectorField_FindPositions_DistCloseToEnemy
         6.5492649, // ATTACK_ArmyVectorField_FindPositions_DistMaxWalk
        15.9957075, // ATTACK_ArmyVectorField_FindPositions_FollowEnemyVectorFieldUth
         9.1061239, // ATTACK_ArmyVectorField_FindPositions_AvoidTraffic
         7.7757473, // ATTACK_ArmyVectorField_FindPositions_RallyPointOffset
         0.9000000, // ATTACK_ArmyVectorField_EvalClusters_InPlace
         2.1793294, // ATTACK_ArmyVectorField_EvalClusters_AtAdvantage
         0.0341255, // ATTACK_ArmyVectorField_EvalClusters_Ambushed
         0.5000000, // ATTACK_ArmyVectorField_DivideForces_DefendCityAdv
         0.1121276, // ATTACK_ArmyVectorField_DivideForces_SupportAllyAdv
         0.0000000, // ATTACK_ArmyVectorField_END

         0.0000000, // ATTACK_SQUAD_START
       416.4127197, // ATTACK_SQUAD_ChangeTarget_Delay
         5.0000000, // ATTACK_SQUAD_MinWalkingDistance
         8.0000000, // ATTACK_SQUAD_TargetReached_House
         5.0000000, // ATTACK_SQUAD_TargetReached_Position
        10.2428083, // ATTACK_SQUAD_TargetReached_RangedSquad
        35.0000000, // ATTACK_SQUAD_TargetReached_Unit
         1.0000000, // ATTACK_SQUAD_TrafficDetection_Limit
        10.0000000, // ATTACK_SQUAD_TrafficDetection_Threshold
         0.0000000, // ATTACK_SQUAD_END

         0.0000000, // ATTACK_SUPERVISOR_START
        22.1900902, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
         3.9091630, // ATTACK_SUPERVISOR_EvalTarget_OpportunityDistGain
         3.3577795, // ATTACK_SUPERVISOR_EvalTarget_OpportunityGain
         0.6074438, // ATTACK_SUPERVISOR_EvalTarget_DecreaseRisk
         6.6606860, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
         6.9580350, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
         2.6582916, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
         3.9733243, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
         2.8656545, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
         1.3565565, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
         0.0000000, // ATTACK_SUPERVISOR_END

         0.0000000, // NAVMESH_PATHFINDING_START
         0.4070168, // NAVMESH_PATHFINDING_AvoidTraffic
         1.1412202, // NAVMESH_PATHFINDING_LineLength
         5.1420155, // NAVMESH_PATHFINDING_AvoidSpecEnemy
         0.0000000, // NAVMESH_PATHFINDING_END

         0.0000000, // BUILDER_START
         1.6684477, // BUILDER_BuildHouse_FieldMaxWork
        11.6014986, // BUILDER_BuildHouse_RTPMaxWork
        21.6772480, // BUILDER_BuildHouse_RoadMaxWork
         8.1070709, // BUILDER_ChHTB_AllWorkerCoef
        21.3621922, // BUILDER_ChHTB_FractionCoef
        17.1468201, // BUILDER_ChHTB_FreeWorkerCoef
         0.2159205, // BUILDER_ChHTB_TrunkBalance
        11.5937538, // BUILDER_ChHTB_TrunkFactor
        10.0000000, // BUILDER_CreateShortcuts_MaxWork
        29.1807594, // BUILDER_Shortage_Gold
        11.7483110, // BUILDER_Shortage_Stone
        14.0460310, // BUILDER_Shortage_StoneReserve
         1.8335937, // BUILDER_Shortage_Trunk
        19.8352127, // BUILDER_Shortage_Wood
         0.0000000, // BUILDER_END

         0.0000000, // EYE_GetForests_START
        26.4097462, // EYE_GetForests_MaxAB
        45.2892570, // EYE_GetForests_MinRndSoil
         2.9898586, // EYE_GetForests_MinTrees
         6.1196084, // EYE_GetForests_Radius
       165.0055695, // EYE_GetForests_SPRndOwnLimMax
        98.0682449, // EYE_GetForests_SPRndOwnLimMin
         0.0000000, // EYE_GetForests_END

         0.0000000, // MANAGEMENT_START
         0.3049391, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
        22.1768589, // MANAGEMENT_CheckUnitCount_SerfLimit1
        34.9827499, // MANAGEMENT_CheckUnitCount_SerfLimit2
        51.9892654, // MANAGEMENT_CheckUnitCount_SerfLimit3
         2.2670105, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
         6.1128011, // MANAGEMENT_GoldShortage
         0.0000000, // MANAGEMENT_END

         0.0000000, // PLANNER_FARM_START
        13.7585087, // PLANNER_FARM_FieldCrit_FlatArea
         0.4989614, // PLANNER_FARM_FieldCrit_PolyRoute
         1.9767742, // PLANNER_FARM_FieldCrit_Soil
         5.6396270, // PLANNER_FARM_FindPlaceForHouse_CityCenter
        16.0000000, // PLANNER_FARM_FindPlaceForHouse_FlatArea
        19.9625053, // PLANNER_FARM_FindPlaceForHouse_HouseDist
        -1.1648518, // PLANNER_FARM_FindPlaceForHouse_Route
       759.2076416, // PLANNER_FARM_PlanFields_CanBuild
        32.8001213, // PLANNER_FARM_PlanFields_Dist
        95.6147385, // PLANNER_FARM_PlanFields_ExistField
         0.0000000, // PLANNER_FARM_END

         0.0000000, // PLANNER_FindPlaceForHouse_START
        40.0904541, // PLANNER_FindPlaceForHouse_CityCenter
         2.7339566, // PLANNER_FindPlaceForHouse_FlatArea
        43.2200470, // PLANNER_FindPlaceForHouse_HouseDist
         2.4208529, // PLANNER_FindPlaceForHouse_Route
        25.1297512, // PLANNER_FindPlaceForHouse_SeedDist
         0.8601482, // PLANNER_FindPlaceForHouse_SnapCrit
         0.0000000, // PLANNER_FindPlaceForHouse_END

         0.0000000, // PLANNER_FindPlaceForQuary_START
        27.4838066, // MANAGEMENT_CheckStoreWares_Stone
         0.6821785, // PREDICTOR_WareNeedPerAWorker_Stone
        14.5459347, // PREDICTOR_WareNeedPerAWorker_StoneOffset
         0.0047831, // PREDICTOR_WorkerCountCoef
         5.0048351, // PLANNER_FindPlaceForQuary_StoneLoc_StoneCnt
       125.4238663, // PLANNER_FindPlaceForQuary_StoneLoc_AlreadyMined
        13.8679466, // PLANNER_FindPlaceForQuary_StoneLoc_Distance
        20.9321098, // PLANNER_FindPlaceForQuary_DistCity
        25.5000973, // PLANNER_FindPlaceForQuary_DistStone
      3051.0822754, // PLANNER_FindPlaceForQuary_DistTimer
        90.0000000, // PLANNER_FindPlaceForQuary_Obstacle
        14.2139816, // PLANNER_FindPlaceForQuary_SnapCrit
        26.7360783, // PLANNER_FindPlaceForQuary_QtyStone
         0.0000000, // PLANNER_FindPlaceForQuary_END

         0.0000000, // PLANNER_FOREST_START
         8.3981934, // PLANNER_FOREST_FindForestAround_MaxDist
        43.0639000, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
        18.3647213, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
      6053.0366211, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
       359.9281311, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
         5.8913641, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
         4.6900163, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
         3.5208817, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
        -0.6038398, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
         0.1276233, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
         7.2485070, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
     21258.1250000, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
         0.2943508, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
         0.0000000, // PLANNER_FOREST_END

         0.0000000, // PLANNER_Snap_START
       293.8428345, // PLANNER_ObstaclesInHousePlan_Road
       599.5475464, // PLANNER_ObstaclesInHousePlan_Tree
        27.8181629, // PLANNER_SnapCrit_Field
         1.1461508, // PLANNER_SnapCrit_HouseOrRoad
        -9.2810154, // PLANNER_SnapCrit_NoBuild
       201.9597015, // PLANNER_SnapCrit_ObstacleInEntrance
        61.5381699, // PLANNER_SnapCrit_Road
        22.1572151, // PLANNER_SnapCrit_RoadInEntrance
         0.0000000, // PLANNER_Snap_END

         0.0000000, // PREDICTOR_START
        44.3221283, // PREDICTOR_SecondSchool_MinRequiredUnits
         0.3007088, // PREDICTOR_WareNeedPerAWorker_Wood
         0.0000000, // PREDICTOR_END

         0.0000000, // SHORTCUTS_START
        95.1310349, // SHORTCUTS_BasePrice
        49.1626778, // SHORTCUTS_Coal
        50.0000000, // SHORTCUTS_Field
        18.1300087, // SHORTCUTS_Forest
        24.3808022, // SHORTCUTS_OtherCase
        46.6517181, // SHORTCUTS_Road
        33.4370956, // SHORTCUTS_TurnPenalization
        25.7135334, // SHORTCUTS_noBuildArea
         0.0000000, // SHORTCUTS_END

         0.0000000, // ROADS_START
        42.2091637, // ROADS_BasePrice
        29.0325890, // ROADS_Coal
        46.5412674, // ROADS_Field
        57.8719749, // ROADS_Forest
        11.6849775, // ROADS_OtherCase
        23.1054745, // ROADS_Road
        50.0000000, // ROADS_TurnPenalization
        35.9236031, // ROADS_noBuildArea
         0.0000000  // ROADS_END

       );




{$IFDEF DEBUG_NewAI}
const
  AI_Par_Offset: array[TAIPar] of Single = (
        0.00, // ATTACK_ArmyVectorField_START
       14.00, // ATTACK_ArmyVectorField_FindPositions_DistEnemyOffset
       12.00, // ATTACK_ArmyVectorField_FindPositions_DistEnemyOffsetFF
        1.00, // ATTACK_ArmyVectorField_FindPositions_DistEnemyGainFF
       12.00, // ATTACK_ArmyVectorField_FindPositions_DistCloseToEnemy
        1.00, // ATTACK_ArmyVectorField_FindPositions_DistMaxWalk
        5.00, // ATTACK_ArmyVectorField_FindPositions_FollowEnemyVectorFieldUth
        1.00, // ATTACK_ArmyVectorField_FindPositions_AvoidTraffic
        1.00, // ATTACK_ArmyVectorField_FindPositions_RallyPointOffset
        0.40, // ATTACK_ArmyVectorField_EvalClusters_InPlace
        0.00, // ATTACK_ArmyVectorField_EvalClusters_AtAdvantage
        0.00, // ATTACK_ArmyVectorField_EvalClusters_Ambushed
        0.50, // ATTACK_ArmyVectorField_DivideForces_DefendCityAdv
        0.00, // ATTACK_ArmyVectorField_DivideForces_SupportAllyAdv
        0.00, // ATTACK_ArmyVectorField_END

        0.00, // ATTACK_SQUAD_START
      350.00, // ATTACK_SQUAD_ChangeTarget_Delay
        5.00, // ATTACK_SQUAD_MinWalkingDistance
        8.00, // ATTACK_SQUAD_TargetReached_House
        1.00, // ATTACK_SQUAD_TargetReached_Position
        9.00, // ATTACK_SQUAD_TargetReached_RangedSquad
       15.00, // ATTACK_SQUAD_TargetReached_Unit
        1.00, // ATTACK_SQUAD_TrafficDetection_Limit
       10.00, // ATTACK_SQUAD_TrafficDetection_Threshold
        0.00, // ATTACK_SQUAD_END

        0.00, // ATTACK_SUPERVISOR_START
       10.00, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
        1.00, // ATTACK_SUPERVISOR_EvalTarget_OpportunityDistGain
        1.00, // ATTACK_SUPERVISOR_EvalTarget_OpportunityGain
        0.50, // ATTACK_SUPERVISOR_EvalTarget_DecreaseRisk
        2.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
        1.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
        1.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
        1.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
        1.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
        0.00, // ATTACK_SUPERVISOR_END

        0.00, // NAVMESH_PATHFINDING_START
        0.00, // NAVMESH_PATHFINDING_AvoidTraffic
        0.00, // NAVMESH_PATHFINDING_LineLength
        5.00, // NAVMESH_PATHFINDING_AvoidSpecEnemy
        0.00, // NAVMESH_PATHFINDING_END

        0.00, // BUILDER_START
        1.00, // BUILDER_BuildHouse_FieldMaxWork
        1.00, // BUILDER_BuildHouse_RTPMaxWork
        5.00, // BUILDER_BuildHouse_RoadMaxWork
        8.00, // BUILDER_ChHTB_AllWorkerCoef
        5.00, // BUILDER_ChHTB_FractionCoef
        8.00, // BUILDER_ChHTB_FreeWorkerCoef
        0.00, // BUILDER_ChHTB_TrunkBalance
        8.00, // BUILDER_ChHTB_TrunkFactor
        1.00, // BUILDER_CreateShortcuts_MaxWork
        0.00, // BUILDER_Shortage_Gold
       10.00, // BUILDER_Shortage_Stone
       10.00, // BUILDER_Shortage_StoneReserve
        1.00, // BUILDER_Shortage_Trunk
        3.00, // BUILDER_Shortage_Wood
        0.00, // BUILDER_END

        0.00, // EYE_GetForests_START
        1.00, // EYE_GetForests_MaxAB
       40.00, // EYE_GetForests_MinRndSoil
        1.00, // EYE_GetForests_MinTrees
        5.00, // EYE_GetForests_Radius
      100.00, // EYE_GetForests_SPRndOwnLimMax
        0.00, // EYE_GetForests_SPRndOwnLimMin
        0.00, // EYE_GetForests_END

        0.00, // MANAGEMENT_START
        0.10, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
        5.00, // MANAGEMENT_CheckUnitCount_SerfLimit1
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit2
       40.00, // MANAGEMENT_CheckUnitCount_SerfLimit3
        0.10, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
        1.00, // MANAGEMENT_GoldShortage
        0.00, // MANAGEMENT_END

        0.00, // PLANNER_FARM_START
       10.00, // PLANNER_FARM_FieldCrit_FlatArea
        0.00, // PLANNER_FARM_FieldCrit_PolyRoute
        0.00, // PLANNER_FARM_FieldCrit_Soil
        0.00, // PLANNER_FARM_FindPlaceForHouse_CityCenter
       10.00, // PLANNER_FARM_FindPlaceForHouse_FlatArea
        5.00, // PLANNER_FARM_FindPlaceForHouse_HouseDist
       -2.00, // PLANNER_FARM_FindPlaceForHouse_Route
      500.00, // PLANNER_FARM_PlanFields_CanBuild
        0.00, // PLANNER_FARM_PlanFields_Dist
       30.00, // PLANNER_FARM_PlanFields_ExistField
        0.00, // PLANNER_FARM_END

        0.00, // PLANNER_FindPlaceForHouse_START
        0.00, // PLANNER_FindPlaceForHouse_CityCenter
        0.00, // PLANNER_FindPlaceForHouse_FlatArea
       25.00, // PLANNER_FindPlaceForHouse_HouseDist
        0.00, // PLANNER_FindPlaceForHouse_Route
        0.00, // PLANNER_FindPlaceForHouse_SeedDist
        0.00, // PLANNER_FindPlaceForHouse_SnapCrit
        0.00, // PLANNER_FindPlaceForHouse_END

        0.00, // PLANNER_FindPlaceForQuary_START
        0.00, // MANAGEMENT_CheckStoreWares_Stone
        0.50, // PREDICTOR_WareNeedPerAWorker_Stone
       10.00, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.00, // PREDICTOR_WorkerCountCoef
        0.00, // PLANNER_FindPlaceForQuary_StoneLoc_StoneCnt
      100.00, // PLANNER_FindPlaceForQuary_StoneLoc_AlreadyMined
        0.00, // PLANNER_FindPlaceForQuary_StoneLoc_Distance
        0.00, // PLANNER_FindPlaceForQuary_DistCity
        0.00, // PLANNER_FindPlaceForQuary_DistStone
        0.00, // PLANNER_FindPlaceForQuary_DistTimer
       80.00, // PLANNER_FindPlaceForQuary_Obstacle
        5.00, // PLANNER_FindPlaceForQuary_SnapCrit
        0.00, // PLANNER_FindPlaceForQuary_QtyStone
        0.00, // PLANNER_FindPlaceForQuary_END

        0.00, // PLANNER_FOREST_START
        5.00, // PLANNER_FOREST_FindForestAround_MaxDist
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
        8.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
        3.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
       -1.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    12000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
        0.00, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
        0.00, // PLANNER_FOREST_END

        0.00, // PLANNER_Snap_START
      200.00, // PLANNER_ObstaclesInHousePlan_Road
      500.00, // PLANNER_ObstaclesInHousePlan_Tree
      -20.00, // PLANNER_SnapCrit_Field
      -30.00, // PLANNER_SnapCrit_HouseOrRoad
      -30.00, // PLANNER_SnapCrit_NoBuild
        0.00, // PLANNER_SnapCrit_ObstacleInEntrance
       25.00, // PLANNER_SnapCrit_Road
        0.00, // PLANNER_SnapCrit_RoadInEntrance
        0.00, // PLANNER_Snap_END

        0.00, // PREDICTOR_START
       20.00, // PREDICTOR_SecondSchool_MinRequiredUnits
        0.01, // PREDICTOR_WareNeedPerAWorker_Wood
        0.00, // PREDICTOR_END

        0.00, // SHORTCUTS_START
       70.00, // SHORTCUTS_BasePrice
       20.00, // SHORTCUTS_Coal
        0.00, // SHORTCUTS_Field
       10.00, // SHORTCUTS_Forest
        0.00, // SHORTCUTS_OtherCase
       20.00, // SHORTCUTS_Road
       20.00, // SHORTCUTS_TurnPenalization
       20.00, // SHORTCUTS_noBuildArea
        0.00, // SHORTCUTS_END

        0.00, // ROADS_START
       25.00, // ROADS_BasePrice
        0.00, // ROADS_Coal
       20.00, // ROADS_Field
       30.00, // ROADS_Forest
        0.00, // ROADS_OtherCase
        0.00, // ROADS_Road
        0.00, // ROADS_TurnPenalization
       15.00, // ROADS_noBuildArea
        0.00  // ROADS_END
  );


  AI_Par_Gain: array[TAIPar] of Single = (
        0.00, // ATTACK_ArmyVectorField_START
       10.00, // ATTACK_ArmyVectorField_FindPositions_DistEnemyOffset
       10.00, // ATTACK_ArmyVectorField_FindPositions_DistEnemyOffsetFF
        1.00, // ATTACK_ArmyVectorField_FindPositions_DistEnemyGainFF
        5.00, // ATTACK_ArmyVectorField_FindPositions_DistCloseToEnemy
       10.00, // ATTACK_ArmyVectorField_FindPositions_DistMaxWalk
       15.00, // ATTACK_ArmyVectorField_FindPositions_FollowEnemyVectorFieldUth
        9.00, // ATTACK_ArmyVectorField_FindPositions_AvoidTraffic
       10.00, // ATTACK_ArmyVectorField_FindPositions_RallyPointOffset
        0.50, // ATTACK_ArmyVectorField_EvalClusters_InPlace
        3.00, // ATTACK_ArmyVectorField_EvalClusters_AtAdvantage
        0.25, // ATTACK_ArmyVectorField_EvalClusters_Ambushed
        0.00, // ATTACK_ArmyVectorField_DivideForces_DefendCityAdv
        0.90, // ATTACK_ArmyVectorField_DivideForces_SupportAllyAdv
        0.00, // ATTACK_ArmyVectorField_END

        0.00, // ATTACK_SQUAD_START
      100.00, // ATTACK_SQUAD_ChangeTarget_Delay
        0.00, // ATTACK_SQUAD_MinWalkingDistance
        0.00, // ATTACK_SQUAD_TargetReached_House
        4.00, // ATTACK_SQUAD_TargetReached_Position
        2.00, // ATTACK_SQUAD_TargetReached_RangedSquad
       20.00, // ATTACK_SQUAD_TargetReached_Unit
        0.00, // ATTACK_SQUAD_TrafficDetection_Limit
        0.00, // ATTACK_SQUAD_TrafficDetection_Threshold
        0.00, // ATTACK_SQUAD_END

        0.00, // ATTACK_SUPERVISOR_START
       15.00, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
        6.00, // ATTACK_SUPERVISOR_EvalTarget_OpportunityDistGain
        4.00, // ATTACK_SUPERVISOR_EvalTarget_OpportunityGain
        0.25, // ATTACK_SUPERVISOR_EvalTarget_DecreaseRisk
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
       10.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
        3.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
        3.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
        3.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
        3.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
        0.00, // ATTACK_SUPERVISOR_END

        0.00, // NAVMESH_PATHFINDING_START
        2.00, // NAVMESH_PATHFINDING_AvoidTraffic
        3.00, // NAVMESH_PATHFINDING_LineLength
        4.00, // NAVMESH_PATHFINDING_AvoidSpecEnemy
        0.00, // NAVMESH_PATHFINDING_END

        0.00, // BUILDER_START
        1.00, // BUILDER_BuildHouse_FieldMaxWork
       15.00, // BUILDER_BuildHouse_RTPMaxWork
       20.00, // BUILDER_BuildHouse_RoadMaxWork
       10.00, // BUILDER_ChHTB_AllWorkerCoef
       40.00, // BUILDER_ChHTB_FractionCoef
       20.00, // BUILDER_ChHTB_FreeWorkerCoef
        3.00, // BUILDER_ChHTB_TrunkBalance
       12.00, // BUILDER_ChHTB_TrunkFactor
        9.00, // BUILDER_CreateShortcuts_MaxWork
       35.00, // BUILDER_Shortage_Gold
       15.00, // BUILDER_Shortage_Stone
       40.00, // BUILDER_Shortage_StoneReserve
        3.00, // BUILDER_Shortage_Trunk
       20.00, // BUILDER_Shortage_Wood
        0.00, // BUILDER_END

        0.00, // EYE_GetForests_START
      200.00, // EYE_GetForests_MaxAB
       22.00, // EYE_GetForests_MinRndSoil
        4.00, // EYE_GetForests_MinTrees
        4.00, // EYE_GetForests_Radius
      155.00, // EYE_GetForests_SPRndOwnLimMax
      155.00, // EYE_GetForests_SPRndOwnLimMin
        0.00, // EYE_GetForests_END

        0.00, // MANAGEMENT_START
        3.00, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit1
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit2
       30.00, // MANAGEMENT_CheckUnitCount_SerfLimit3
        3.00, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
       15.00, // MANAGEMENT_GoldShortage
        0.00, // MANAGEMENT_END

        0.00, // PLANNER_FARM_START
       10.00, // PLANNER_FARM_FieldCrit_FlatArea
        5.00, // PLANNER_FARM_FieldCrit_PolyRoute
        3.00, // PLANNER_FARM_FieldCrit_Soil
       10.00, // PLANNER_FARM_FindPlaceForHouse_CityCenter
       10.00, // PLANNER_FARM_FindPlaceForHouse_FlatArea
       15.00, // PLANNER_FARM_FindPlaceForHouse_HouseDist
        4.00, // PLANNER_FARM_FindPlaceForHouse_Route
      500.00, // PLANNER_FARM_PlanFields_CanBuild
       75.00, // PLANNER_FARM_PlanFields_Dist
       75.00, // PLANNER_FARM_PlanFields_ExistField
        0.00, // PLANNER_FARM_END

        0.00, // PLANNER_FindPlaceForHouse_START
       80.00, // PLANNER_FindPlaceForHouse_CityCenter
        6.00, // PLANNER_FindPlaceForHouse_FlatArea
       20.00, // PLANNER_FindPlaceForHouse_HouseDist
        4.00, // PLANNER_FindPlaceForHouse_Route
       50.00, // PLANNER_FindPlaceForHouse_SeedDist
        3.00, // PLANNER_FindPlaceForHouse_SnapCrit
        0.00, // PLANNER_FindPlaceForHouse_END

        0.00, // PLANNER_FindPlaceForQuary_START
       50.00, // MANAGEMENT_CheckStoreWares_Stone
        0.30, // PREDICTOR_WareNeedPerAWorker_Stone
       10.00, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.01, // PREDICTOR_WorkerCountCoef
       10.00, // PLANNER_FindPlaceForQuary_StoneLoc_StoneCnt
      100.00, // PLANNER_FindPlaceForQuary_StoneLoc_AlreadyMined
       20.00, // PLANNER_FindPlaceForQuary_StoneLoc_Distance
       50.00, // PLANNER_FindPlaceForQuary_DistCity
       50.00, // PLANNER_FindPlaceForQuary_DistStone
    15000.00, // PLANNER_FindPlaceForQuary_DistTimer
       50.00, // PLANNER_FindPlaceForQuary_Obstacle
       20.00, // PLANNER_FindPlaceForQuary_SnapCrit
       50.00, // PLANNER_FindPlaceForQuary_QtyStone
        0.00, // PLANNER_FindPlaceForQuary_END

        0.00, // PLANNER_FOREST_START
        5.00, // PLANNER_FOREST_FindForestAround_MaxDist
      150.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
       20.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
    10000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
      500.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
        6.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
        6.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
        4.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
        2.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
        3.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
       21.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    12000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
        2.00, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
        0.00, // PLANNER_FOREST_END

        0.00, // PLANNER_Snap_START
      500.00, // PLANNER_ObstaclesInHousePlan_Road
     1000.00, // PLANNER_ObstaclesInHousePlan_Tree
       50.00, // PLANNER_SnapCrit_Field
       50.00, // PLANNER_SnapCrit_HouseOrRoad
       50.00, // PLANNER_SnapCrit_NoBuild
     1000.00, // PLANNER_SnapCrit_ObstacleInEntrance
       50.00, // PLANNER_SnapCrit_Road
      300.00, // PLANNER_SnapCrit_RoadInEntrance
        0.00, // PLANNER_Snap_END

        0.00, // PREDICTOR_START
       30.00, // PREDICTOR_SecondSchool_MinRequiredUnits
        0.30, // PREDICTOR_WareNeedPerAWorker_Wood
        0.00, // PREDICTOR_END

        0.00, // SHORTCUTS_START
       40.00, // SHORTCUTS_BasePrice
       50.00, // SHORTCUTS_Coal
       50.00, // SHORTCUTS_Field
       40.00, // SHORTCUTS_Forest
       50.00, // SHORTCUTS_OtherCase
       50.00, // SHORTCUTS_Road
       50.00, // SHORTCUTS_TurnPenalization
       50.00, // SHORTCUTS_noBuildArea
        0.00, // SHORTCUTS_END

        0.00, // ROADS_START
       30.00, // ROADS_BasePrice
       50.00, // ROADS_Coal
       40.00, // ROADS_Field
       50.00, // ROADS_Forest
       50.00, // ROADS_OtherCase
       50.00, // ROADS_Road
       50.00, // ROADS_TurnPenalization
       35.00, // ROADS_noBuildArea
        0.00  // ROADS_END
  );
{$ENDIF}

implementation

{$IFDEF PARALLEL_RUNNER}
procedure LoadGAParameters(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('LoadGAParameters');
  LoadStream.Read(AI_Par,SizeOf(AI_Par));
end;

procedure SaveGAParameters(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('LoadGAParameters');
  SaveStream.Write(AI_Par,SizeOf(AI_Par));
end;
{$ENDIF}

end.
