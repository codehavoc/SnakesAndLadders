/***************************************************
 * 6SENG001W Reasoning About Programs Coursework 02
 * w1714906 / 2018158
 * Snakes & Ladders Game
 * Author: Udara Nayana
 * Creation date: 12-Jan-22
 ***************************************************/
MACHINE
    Game
SETS
    MOVEMENTLOGS = { Went_Up_A_Ladder_Game_Still_On_Play , Went_Down_A_Snake_Game_Still_On_Play ,
                    Landed_On_A_Normal_Square_Game_Still_On_Play , Landed_On_Final_Square_Game_Won_Start_A_New_Game ,
                    Unable_To_Proceed_Due_To_Roll_Being_Over_100 , Starting_A_New_Game } ;
    GAMESTATUS = { Game_Won , Game_Not_Over , New_Game }
DEFINITIONS
    isSquareOnBoard ( xx ) == ( xx : Gameboard ) ;
    gameWon ( xx ) == ( xx = 100 ) ;
    snakeSquare ( xx ) == ( xx : dom ( Snakes ) ) ;
    ladderSquare ( xx ) == ( xx : dom ( Ladders ) )
CONCRETE_CONSTANTS
    Gameboard ,
    Dice ,
    Snakes ,
    Ladders
PROPERTIES
    Gameboard <: NAT1 &
    Gameboard = 1 .. 100 &
    Dice <: NAT1 &
    Dice = 1 .. 6 &
    Snakes <: ( NAT1 * NAT1 ) &
    Snakes = { ( 16 |-> 13 ) , ( 31 |-> 4 ) , ( 47 |-> 25 ) , ( 63 |-> 60 ) , ( 66 |-> 52 ) , ( 97 |-> 75 ) } &
    Ladders <: ( NAT1 * NAT1 ) &
    Ladders = { ( 3 |-> 39 ) , ( 10 |-> 12 ) , ( 27 |-> 53 ) , ( 56 |-> 84 ) , ( 61 |-> 99 ) , ( 72 |-> 90 ) }
ABSTRACT_VARIABLES
    currentPosition ,
    recentDiceValue ,
    moveCount ,
    snakeEncounters ,
    ladderEncounters ,
    squarePath ,
    gameStatus
INVARIANT
    currentPosition : Gameboard &
    recentDiceValue : Dice &
    moveCount : NAT &
    snakeEncounters : NAT &
    ladderEncounters : NAT &
    squarePath : seq ( Gameboard ) &
    gameStatus <: GAMESTATUS
INITIALISATION
    currentPosition := 1 ||
    recentDiceValue := 1 ||
    moveCount := 0 ||
    snakeEncounters := 0 ||
    ladderEncounters := 0 ||
    squarePath := [ 1 ] ||
    gameStatus := { New_Game }
OPERATIONS
    messageLogs , currentSquare <-- Move ( roll ) =
    PRE messageLogs : MOVEMENTLOGS & roll : Dice & Game_Not_Over : gameStatus
    THEN
        IF isSquareOnBoard ( currentPosition + roll )
        THEN
            IF gameWon ( currentPosition + roll )
            THEN
                recentDiceValue := roll ||
                squarePath := squarePath ^ [ currentPosition + roll ] ||
                currentPosition := currentPosition + roll ||
                moveCount := succ ( moveCount ) ||
                currentSquare := currentPosition + roll ||
                messageLogs := Landed_On_Final_Square_Game_Won_Start_A_New_Game ||
                gameStatus := { Game_Won }
            ELSE
                IF snakeSquare ( currentPosition + roll )
                THEN
                    recentDiceValue := roll ||
                    squarePath := squarePath ^ [ currentPosition + roll ] ^ [ Snakes ( currentPosition + roll ) ] ||
                    currentPosition := Snakes ( currentPosition + roll ) ||
                    moveCount := succ ( moveCount ) ||
                    snakeEncounters := succ ( snakeEncounters ) ||
                    currentSquare := Snakes ( currentPosition + roll ) ||
                    messageLogs := Went_Down_A_Snake_Game_Still_On_Play
                ELSIF ladderSquare ( currentPosition + roll )
                THEN
                    recentDiceValue := roll ||
                    squarePath := squarePath ^ [ currentPosition + roll ] ^ [ Ladders ( currentPosition + roll ) ] ||
                    currentPosition := Ladders ( currentPosition + roll ) ||
                    moveCount := succ ( moveCount ) ||
                    ladderEncounters := succ ( ladderEncounters ) ||
                    currentSquare := Ladders ( currentPosition + roll ) ||
                    messageLogs := Went_Up_A_Ladder_Game_Still_On_Play
                ELSE
                    recentDiceValue := roll ||
                    squarePath := squarePath ^ [ currentPosition + roll ] ||
                    currentPosition := currentPosition + roll ||
                    moveCount := succ ( moveCount ) ||
                    currentSquare := currentPosition + roll ||
                    messageLogs := Landed_On_A_Normal_Square_Game_Still_On_Play
                END
            END
        ELSE
            recentDiceValue := roll ||
            moveCount := succ ( moveCount ) ||
            currentSquare := currentPosition ||
            messageLogs := Unable_To_Proceed_Due_To_Roll_Being_Over_100
        END
    END ;


    currentSquare , noOfSnakes , noOfLadders , turns <-- GameStatistics =
    PRE moveCount > 0
    THEN
        currentSquare := currentPosition ||
        noOfSnakes := snakeEncounters ||
        noOfLadders := ladderEncounters ||
        turns := moveCount
    END ;


    path <-- VisitedSquares =
    PRE Game_Not_Over : gameStatus
    THEN
        path := squarePath
    END ;


    messageLogs <-- NewGame =
    PRE messageLogs : MOVEMENTLOGS
    THEN
        currentPosition := 1 ||
        recentDiceValue := 1 ||
        moveCount := 0 ||
        snakeEncounters := 0 ||
        ladderEncounters := 0 ||
        squarePath := [ 1 ] ||
        gameStatus := { Game_Not_Over } ||
        messageLogs := Starting_A_New_Game
    END
END
