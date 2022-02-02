Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Game))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Game))==(Machine(Game));
  Level(Machine(Game))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Game)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Game))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Game))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Game))==(?);
  List_Includes(Machine(Game))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Game))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Game))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Game))==(?);
  Context_List_Variables(Machine(Game))==(?);
  Abstract_List_Variables(Machine(Game))==(?);
  Local_List_Variables(Machine(Game))==(gameStatus,squarePath,ladderEncounters,snakeEncounters,moveCount,recentDiceValue,currentPosition);
  List_Variables(Machine(Game))==(gameStatus,squarePath,ladderEncounters,snakeEncounters,moveCount,recentDiceValue,currentPosition);
  External_List_Variables(Machine(Game))==(gameStatus,squarePath,ladderEncounters,snakeEncounters,moveCount,recentDiceValue,currentPosition)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Game))==(?);
  Abstract_List_VisibleVariables(Machine(Game))==(?);
  External_List_VisibleVariables(Machine(Game))==(?);
  Expanded_List_VisibleVariables(Machine(Game))==(?);
  List_VisibleVariables(Machine(Game))==(?);
  Internal_List_VisibleVariables(Machine(Game))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Game))==(btrue);
  Gluing_List_Invariant(Machine(Game))==(btrue);
  Expanded_List_Invariant(Machine(Game))==(btrue);
  Abstract_List_Invariant(Machine(Game))==(btrue);
  Context_List_Invariant(Machine(Game))==(btrue);
  List_Invariant(Machine(Game))==(currentPosition: Gameboard & recentDiceValue: Dice & moveCount: NAT & snakeEncounters: NAT & ladderEncounters: NAT & squarePath: seq(Gameboard) & gameStatus <: GAMESTATUS)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Game))==(btrue);
  Abstract_List_Assertions(Machine(Game))==(btrue);
  Context_List_Assertions(Machine(Game))==(btrue);
  List_Assertions(Machine(Game))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Game))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Game))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Game))==(currentPosition,recentDiceValue,moveCount,snakeEncounters,ladderEncounters,squarePath,gameStatus:=1,1,0,0,0,[1],{New_Game});
  Context_List_Initialisation(Machine(Game))==(skip);
  List_Initialisation(Machine(Game))==(currentPosition:=1 || recentDiceValue:=1 || moveCount:=0 || snakeEncounters:=0 || ladderEncounters:=0 || squarePath:=[1] || gameStatus:={New_Game})
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Game))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Game))==(btrue);
  List_Constraints(Machine(Game))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Game))==(Move,GameStatistics,VisitedSquares,NewGame);
  List_Operations(Machine(Game))==(Move,GameStatistics,VisitedSquares,NewGame)
END
&
THEORY ListInputX IS
  List_Input(Machine(Game),Move)==(roll);
  List_Input(Machine(Game),GameStatistics)==(?);
  List_Input(Machine(Game),VisitedSquares)==(?);
  List_Input(Machine(Game),NewGame)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Game),Move)==(messageLogs,currentSquare);
  List_Output(Machine(Game),GameStatistics)==(currentSquare,noOfSnakes,noOfLadders,turns);
  List_Output(Machine(Game),VisitedSquares)==(path);
  List_Output(Machine(Game),NewGame)==(messageLogs)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Game),Move)==(messageLogs,currentSquare <-- Move(roll));
  List_Header(Machine(Game),GameStatistics)==(currentSquare,noOfSnakes,noOfLadders,turns <-- GameStatistics);
  List_Header(Machine(Game),VisitedSquares)==(path <-- VisitedSquares);
  List_Header(Machine(Game),NewGame)==(messageLogs <-- NewGame)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Game),Move)==(messageLogs: MOVEMENTLOGS & roll: Dice & Game_Not_Over: gameStatus);
  List_Precondition(Machine(Game),GameStatistics)==(moveCount>0);
  List_Precondition(Machine(Game),VisitedSquares)==(Game_Not_Over: gameStatus);
  List_Precondition(Machine(Game),NewGame)==(messageLogs: MOVEMENTLOGS)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Game),NewGame)==(messageLogs: MOVEMENTLOGS | currentPosition,recentDiceValue,moveCount,snakeEncounters,ladderEncounters,squarePath,gameStatus,messageLogs:=1,1,0,0,0,[1],{Game_Not_Over},Starting_A_New_Game);
  Expanded_List_Substitution(Machine(Game),VisitedSquares)==(Game_Not_Over: gameStatus | path:=squarePath);
  Expanded_List_Substitution(Machine(Game),GameStatistics)==(moveCount>0 | currentSquare,noOfSnakes,noOfLadders,turns:=currentPosition,snakeEncounters,ladderEncounters,moveCount);
  Expanded_List_Substitution(Machine(Game),Move)==(messageLogs: MOVEMENTLOGS & roll: Dice & Game_Not_Over: gameStatus | currentPosition+roll: Gameboard ==> (currentPosition+roll = 100 ==> recentDiceValue,squarePath,currentPosition,moveCount,currentSquare,messageLogs,gameStatus:=roll,squarePath^[currentPosition+roll],currentPosition+roll,succ(moveCount),currentPosition+roll,Landed_On_Final_Square_Game_Won_Start_A_New_Game,{Game_Won} [] not(currentPosition+roll = 100) ==> (currentPosition+roll: dom(Snakes) ==> recentDiceValue,squarePath,currentPosition,moveCount,snakeEncounters,currentSquare,messageLogs:=roll,squarePath^[currentPosition+roll]^[Snakes(currentPosition+roll)],Snakes(currentPosition+roll),succ(moveCount),succ(snakeEncounters),Snakes(currentPosition+roll),Went_Down_A_Snake_Game_Still_On_Play [] not(currentPosition+roll: dom(Snakes)) ==> (currentPosition+roll: dom(Ladders) ==> recentDiceValue,squarePath,currentPosition,moveCount,ladderEncounters,currentSquare,messageLogs:=roll,squarePath^[currentPosition+roll]^[Ladders(currentPosition+roll)],Ladders(currentPosition+roll),succ(moveCount),succ(ladderEncounters),Ladders(currentPosition+roll),Went_Up_A_Ladder_Game_Still_On_Play [] not(currentPosition+roll: dom(Ladders)) ==> recentDiceValue,squarePath,currentPosition,moveCount,currentSquare,messageLogs:=roll,squarePath^[currentPosition+roll],currentPosition+roll,succ(moveCount),currentPosition+roll,Landed_On_A_Normal_Square_Game_Still_On_Play))) [] not(currentPosition+roll: Gameboard) ==> recentDiceValue,moveCount,currentSquare,messageLogs:=roll,succ(moveCount),currentPosition,Unable_To_Proceed_Due_To_Roll_Being_Over_100);
  List_Substitution(Machine(Game),Move)==(IF currentPosition+roll: Gameboard THEN IF currentPosition+roll = 100 THEN recentDiceValue:=roll || squarePath:=squarePath^[currentPosition+roll] || currentPosition:=currentPosition+roll || moveCount:=succ(moveCount) || currentSquare:=currentPosition+roll || messageLogs:=Landed_On_Final_Square_Game_Won_Start_A_New_Game || gameStatus:={Game_Won} ELSE IF currentPosition+roll: dom(Snakes) THEN recentDiceValue:=roll || squarePath:=squarePath^[currentPosition+roll]^[Snakes(currentPosition+roll)] || currentPosition:=Snakes(currentPosition+roll) || moveCount:=succ(moveCount) || snakeEncounters:=succ(snakeEncounters) || currentSquare:=Snakes(currentPosition+roll) || messageLogs:=Went_Down_A_Snake_Game_Still_On_Play ELSIF currentPosition+roll: dom(Ladders) THEN recentDiceValue:=roll || squarePath:=squarePath^[currentPosition+roll]^[Ladders(currentPosition+roll)] || currentPosition:=Ladders(currentPosition+roll) || moveCount:=succ(moveCount) || ladderEncounters:=succ(ladderEncounters) || currentSquare:=Ladders(currentPosition+roll) || messageLogs:=Went_Up_A_Ladder_Game_Still_On_Play ELSE recentDiceValue:=roll || squarePath:=squarePath^[currentPosition+roll] || currentPosition:=currentPosition+roll || moveCount:=succ(moveCount) || currentSquare:=currentPosition+roll || messageLogs:=Landed_On_A_Normal_Square_Game_Still_On_Play END END ELSE recentDiceValue:=roll || moveCount:=succ(moveCount) || currentSquare:=currentPosition || messageLogs:=Unable_To_Proceed_Due_To_Roll_Being_Over_100 END);
  List_Substitution(Machine(Game),GameStatistics)==(currentSquare:=currentPosition || noOfSnakes:=snakeEncounters || noOfLadders:=ladderEncounters || turns:=moveCount);
  List_Substitution(Machine(Game),VisitedSquares)==(path:=squarePath);
  List_Substitution(Machine(Game),NewGame)==(currentPosition:=1 || recentDiceValue:=1 || moveCount:=0 || snakeEncounters:=0 || ladderEncounters:=0 || squarePath:=[1] || gameStatus:={Game_Not_Over} || messageLogs:=Starting_A_New_Game)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Game))==(Gameboard,Dice,Snakes,Ladders);
  Inherited_List_Constants(Machine(Game))==(?);
  List_Constants(Machine(Game))==(Gameboard,Dice,Snakes,Ladders)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Game),MOVEMENTLOGS)==({Went_Up_A_Ladder_Game_Still_On_Play,Went_Down_A_Snake_Game_Still_On_Play,Landed_On_A_Normal_Square_Game_Still_On_Play,Landed_On_Final_Square_Game_Won_Start_A_New_Game,Unable_To_Proceed_Due_To_Roll_Being_Over_100,Starting_A_New_Game});
  Context_List_Enumerated(Machine(Game))==(?);
  Context_List_Defered(Machine(Game))==(?);
  Context_List_Sets(Machine(Game))==(?);
  List_Valuable_Sets(Machine(Game))==(?);
  Inherited_List_Enumerated(Machine(Game))==(?);
  Inherited_List_Defered(Machine(Game))==(?);
  Inherited_List_Sets(Machine(Game))==(?);
  List_Enumerated(Machine(Game))==(MOVEMENTLOGS,GAMESTATUS);
  List_Defered(Machine(Game))==(?);
  List_Sets(Machine(Game))==(MOVEMENTLOGS,GAMESTATUS);
  Set_Definition(Machine(Game),GAMESTATUS)==({Game_Won,Game_Not_Over,New_Game})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Game))==(?);
  Expanded_List_HiddenConstants(Machine(Game))==(?);
  List_HiddenConstants(Machine(Game))==(?);
  External_List_HiddenConstants(Machine(Game))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Game))==(btrue);
  Context_List_Properties(Machine(Game))==(btrue);
  Inherited_List_Properties(Machine(Game))==(btrue);
  List_Properties(Machine(Game))==(Gameboard <: NAT1 & Gameboard = 1..100 & Dice <: NAT1 & Dice = 1..6 & Snakes <: NAT1*NAT1 & Snakes = {16|->13,31|->4,47|->25,63|->60,66|->52,97|->75} & Ladders <: NAT1*NAT1 & Ladders = {3|->39,10|->12,27|->53,56|->84,61|->99,72|->90} & MOVEMENTLOGS: FIN(INTEGER) & not(MOVEMENTLOGS = {}) & GAMESTATUS: FIN(INTEGER) & not(GAMESTATUS = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Game),Move)==(?);
  List_ANY_Var(Machine(Game),GameStatistics)==(?);
  List_ANY_Var(Machine(Game),VisitedSquares)==(?);
  List_ANY_Var(Machine(Game),NewGame)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Game)) == (Gameboard,Dice,Snakes,Ladders,MOVEMENTLOGS,GAMESTATUS,Went_Up_A_Ladder_Game_Still_On_Play,Went_Down_A_Snake_Game_Still_On_Play,Landed_On_A_Normal_Square_Game_Still_On_Play,Landed_On_Final_Square_Game_Won_Start_A_New_Game,Unable_To_Proceed_Due_To_Roll_Being_Over_100,Starting_A_New_Game,Game_Won,Game_Not_Over,New_Game | ? | gameStatus,squarePath,ladderEncounters,snakeEncounters,moveCount,recentDiceValue,currentPosition | ? | Move,GameStatistics,VisitedSquares,NewGame | ? | ? | ? | Game);
  List_Of_HiddenCst_Ids(Machine(Game)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Game)) == (Gameboard,Dice,Snakes,Ladders);
  List_Of_VisibleVar_Ids(Machine(Game)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Game)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Game)) == (Type(MOVEMENTLOGS) == Cst(SetOf(etype(MOVEMENTLOGS,0,5)));Type(GAMESTATUS) == Cst(SetOf(etype(GAMESTATUS,0,2))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Game)) == (Type(Went_Up_A_Ladder_Game_Still_On_Play) == Cst(etype(MOVEMENTLOGS,0,5));Type(Went_Down_A_Snake_Game_Still_On_Play) == Cst(etype(MOVEMENTLOGS,0,5));Type(Landed_On_A_Normal_Square_Game_Still_On_Play) == Cst(etype(MOVEMENTLOGS,0,5));Type(Landed_On_Final_Square_Game_Won_Start_A_New_Game) == Cst(etype(MOVEMENTLOGS,0,5));Type(Unable_To_Proceed_Due_To_Roll_Being_Over_100) == Cst(etype(MOVEMENTLOGS,0,5));Type(Starting_A_New_Game) == Cst(etype(MOVEMENTLOGS,0,5));Type(Game_Won) == Cst(etype(GAMESTATUS,0,2));Type(Game_Not_Over) == Cst(etype(GAMESTATUS,0,2));Type(New_Game) == Cst(etype(GAMESTATUS,0,2));Type(Gameboard) == Cst(SetOf(btype(INTEGER,"[Gameboard","]Gameboard")));Type(Dice) == Cst(SetOf(btype(INTEGER,"[Dice","]Dice")));Type(Snakes) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(Ladders) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Game)) == (Type(gameStatus) == Mvl(SetOf(etype(GAMESTATUS,?,?)));Type(squarePath) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(ladderEncounters) == Mvl(btype(INTEGER,?,?));Type(snakeEncounters) == Mvl(btype(INTEGER,?,?));Type(moveCount) == Mvl(btype(INTEGER,?,?));Type(recentDiceValue) == Mvl(btype(INTEGER,?,?));Type(currentPosition) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Game)) == (Type(NewGame) == Cst(etype(MOVEMENTLOGS,?,?),No_type);Type(VisitedSquares) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)),No_type);Type(GameStatistics) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(Move) == Cst(etype(MOVEMENTLOGS,?,?)*btype(INTEGER,?,?),btype(INTEGER,?,?)));
  Observers(Machine(Game)) == (Type(VisitedSquares) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)),No_type);Type(GameStatistics) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
