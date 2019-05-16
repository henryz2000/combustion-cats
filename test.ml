open OUnit2
open Cats 
open Command 
open State

let cats_tests = [

  "cards names test1" >:: (fun _ -> assert_equal
                              ["Rainbow Cat";] 
                              (Cats.cards_names [{name = "Rainbow Cat";
                                                  description = "HRNNGGGG!";}; 
                                                ]));

  "cards names test2" >:: (fun _ -> assert_equal
                              ["Rainbow Cat"; "Watermelon Cat"; 
                               "Taco Cat"; "Attack"] 
                              (Cats.cards_names [{name = "Rainbow Cat";
                                                  description = "HRNNGGGG!";}; 
                                                 {name = "Watermelon Cat";
                                                  description = 
                                                    "Watermelons are gooood!";}; 
                                                 {name = "Taco Cat"; 
                                                  description = 
                                                    "I am a palindrome!";}; 
                                                 {name = "Attack";
                                                  description = 
                                                  "Immediately end your turn(s) 
                                                 without drawing and 
     force the next player to take 2 turns in a row. The victim of this card 
     takes a turn as normal. Then, when their first turn is over, it’s their 
     turn again. 
     (If the victim of an Attack Card plays an Attack Card, 
     their turns are immediately over, 
     and the next player must take 2 turns.)";}]));

  "description test" >:: (fun _ -> assert_equal "HRNNGGGG!" 
                             (Cats.description "Rainbow Cat"));

  "contains test1" >:: (fun _ -> assert_equal true (Cats.contains 
                                                      [{name = "Taco Cat"; 
                                                        description = 
                                                          "I am a palindrome!"};
                                                       {name = "Potato Cat"; 
                                                        description = 
                                                          "Potato potato"};] 
                                                      "Taco Cat"));
  "contains test2" >:: (fun _ -> assert_equal false (Cats.contains 
                                                       [{name = "Taco Cat";
                                                         description = 
                                                          "I am a palindrome!"};
                                                        {name = "Potato Cat";
                                                         description = 
                                                           "Potato potato"};]
                                                       "Attack "));   
  "find name test" >:: (fun _ -> assert_equal "Taco Cat" 
                           (Cats.find_name [[{name="Potato Cat"; 
                                              description="Potato potato"};]; 
                                            [{name="Beard Cat";
                                              description="Beard?"};]; 
                                            [{name="Taco Cat";
                                              description=
                                                "I am a palindrome!"};]] 2 0));                 

]

let command_tests = [

  "pass parse test1" >:: (fun _ -> assert_equal
                             Command.Pass (Command.parse "pass" []));

  "pass parse test2" >:: (fun _ -> assert_equal
                             Command.Pass (Command.parse "   pass " []));
  "quit parse test1" >:: (fun _ -> assert_equal
                             Command.Quit (Command.parse "quit" []));
  "quit parse test2" >:: (fun _ -> assert_equal
                             Command.Quit (Command.parse " quit    " []));

  "description parse test1" >:: (fun _ -> assert_equal
                                    (Command.Description ["Skip"]) 
                                    (Command.parse " description Skip" []));
  "description parse test2" >:: (fun _ -> assert_equal
                                    (Command.Description ["Skip"]) 
                                    (Command.parse " description    Skip " []));
  "draw parse test" >:: (fun _ -> assert_equal Command.Draw
                            (Command.parse "draw   " []));
  "draw parse test2" >:: (fun _ -> assert_equal Command.Draw
                             (Command.parse "draw" []));
  "Should quit1" >:: (fun _ -> assert_equal (parse " quit  " []) Quit) ;
  "Should quit2" >:: (fun _ -> assert_equal (parse "quit" []) Quit) ;
  "empty"    >:: (fun _ -> assert_raises (Command.Empty) 
                     (fun () -> Command.parse "" []));
  "empty"    >:: (fun _ -> assert_raises (Empty) (fun () -> parse " " []));
  "Malformed1"    >:: (fun _ -> assert_raises (Malformed)
                          (fun () -> parse "play q;iwhd;" []));
  "Malformed2"    >:: (fun _ -> assert_raises (Malformed)
                          (fun () -> parse "quit a" []));
  "Malformed3"    >:: (fun _ -> assert_raises (Malformed)
                          (fun () -> parse "abc" []));
  "Malformed4"    >:: (fun _ -> assert_raises (Malformed)
                          (fun () -> parse "draw oife" []));
  "Malformed5" >:: (fun _ ->   assert_raises (Malformed)
                       (fun() -> Command.parse "pair  Favor with 2"
                           [{name = "Favor";
                             description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."}] ));
  "Malformed6" >:: (fun _ ->   assert_raises (Malformed)
                       (fun() -> Command.parse "three Favor with 2"
                           [{name = "Favor";
                             description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."}] ));
  "play parse test" >:: (fun _ -> assert_equal (Command.Play ["Attack"]) 
                            (Command.parse "play  Attack  " [{name = "Attack"; 
                                                              description=
                                                                "Immediately 
                                                                end your turn(s)
                                                                 without
                             drawing and force the next player to take 2 turns 
                             in a row. The victim of this card takes a turn as 
                             normal. Then, when their first turn is over, it’s 
                             their turn again. (If the victim of an Attack Card
                              plays an Attack Card, their turns are 
                              immediately over,
                               and the next player must take 2 turns.)"};])); 

  "give parse test" >:: (fun _ -> assert_equal (Command.Give ["Attack"]) 
                            (Command.parse 
                               "  give  Attack  " 
                               [{name = "Attack"; 
                                 description="Immediately end your turn(s) 
                                 without drawing and force the next player
                                  to take 2 turns in a row. 
                                  The victim of this card takes a turn as 
                             normal. Then, when their first turn is over, it’s 
                             their turn again. (If the victim of an Attack Card
                              plays an Attack Card, their turns are 
                              immediately over,
                               and the next player must take 2 turns.)"};])); 
  "use parse test" >:: (fun _ ->  assert_equal
                           (Command.Use ("Favor", 2)) 
                           (Command.parse "use   Favor with 2"
                              [{name = "Favor";
                                description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."}] ));
  "pair parse test" >:: (fun _ ->  assert_equal
                            (Command.Pair ("Favor", 2)) 
                            (Command.parse "pair   Favor with 2"
                               [{name = "Favor";
                                 description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."};
                                {name = "Favor";
                                 description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."};
                               ] ));
  "three parse test" >:: (fun _ ->  assert_equal
                             (Command.Three ("Favor", 2)) 
                             (Command.parse "   three  Favor with 2"
                                [{name = "Favor";
                                  description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."};
                                 {name = "Favor";
                                  description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."};
                                 {name = "Favor";
                                  description = "Force any other player to give
                                 you 1 card from their hand. They choose which
                                  card to give you."}] )); 



]

let state_tests = [

  "init state test1" >:: (fun _ -> assert_equal 
                             [State.Alive; State.Alive; State.Alive; 
                              State.Alive; State.Alive] 
                             (State.init_states 5 []));

  "init state test2" >:: (fun _ -> assert_equal 
                             [] 
                             (State.init_states 0 []));

  "boom i test1" >:: (fun _ -> assert_equal 
                         [State.Dead;State.Alive; State.Alive; State.Alive] 
                         (State.boom_i 0 true 
                            [State.Alive; State.Alive; 
                             State.Alive; State.Alive]));

  "boom i test2" >:: (fun _ -> assert_equal 
                         [State.Alive;State.Alive; State.Alive; State.Dead] 
                         (State.boom_i 3 true 
                            [State.Alive; State.Alive; 
                             State.Alive; State.Alive]));

  "change states test" >:: (fun _ -> assert_equal 
                               [State.Alive;State.Alive; State.Alive;State.Dead] 
                               (State.change_state 3 Dead 
                                  [State.Alive;State.Alive;
                                   State.Alive; State.Alive]));                        
]

let suite =
  "test suite for A6"  >::: List.flatten [
    cats_tests; 
    command_tests; 
    state_tests; 
  ]

let _ = run_test_tt_main suite