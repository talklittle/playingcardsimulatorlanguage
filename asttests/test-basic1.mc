Include
{
}

CardEntities // Card entities are addressed with '$'
{
	dealer;
	player1;
	player2;
	flop;
}

Globals // Global variables are addressed with '#'
{
	var int currentPot;
	var int lastBid;
}

Start // Deal cards, chips
{
	Card AceSpade;
	Card KingSpade;
	var int i;

	#lastBid <- 0;
	#currentPot <- 0;

	$dealer <- AceSpade;  // build deck
	$dealer <- KingSpade;

	for (i <- 0; i <= 2; i += 1) // deal out 2 cards to each player
	{
	   $dealer.randomCard() -> $player1;
	   $dealer.randomCard() -> $player2;
	}
	
	$player1.chips <- 100; // start players off with 100 chips
	$player2.chips <- 100; 
}

PlayOrder //Order of play for all CardEntities
{
	$player1.play();
	$player2.play();
	while ($player2.isDoneBidding && $player1.isDoneBidding) {
	   $player1.play();
	   $player2.play();
	}

	$flop.play(); // flop
	evaluateHandWinner();
}

WinningCondition // Conditions for the game to end - evaluated every play turn
{
	if ($player1.chips <= 0) {
	   return $player2;
	} else {
	   if ($player2.chips <= 0) {
	      return $player1;
	   }
	}

	return null;
}

$player1.play() // Player 1 play function
{
	me.isDoneBidding <- false;
	me >> // print the cards
	
	if (me.bid == null || me.bid < #lastBid) // haven't bet or overbet
	{
	   me.bid << // input a bet (auto convert input to type of 'bid')
	   me.chips -= me.bid; // subtract bet from your current chips
	   #currentPot += me.bid; // add bet to the current pot
	   #lastBid <- me.bid; // record the last bid to check overbets
	}
	
	me.isDoneBidding <- true;
}

evaluateHandWinner()
{
	//The best poker hand wins
}	

