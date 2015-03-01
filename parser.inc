//{$DEFINE DEBUG_PARSER}

   function CanEnd(): Boolean;
   begin
      Result := (CurrentToken >= Length(Tokens)) or
                (Tokens[CurrentToken] = 'then') or
                (Tokens[CurrentToken] = 'and') or
                (Tokens[CurrentToken] = ';') or
                ((CurrentToken+1 < Length(Tokens)) and
                 (Tokens[CurrentToken] = ',') and
                 ((Tokens[CurrentToken+1] = 'then') or
                  (Tokens[CurrentToken+1] = 'and')));
   end;

   function IsQuote(): Boolean;
   begin
      Result := (CurrentToken < Length(Tokens)) and
                (Length(Tokens[CurrentToken]) >= 2) and
                (Tokens[CurrentToken][1] = '"');
   end;

   procedure EnsureNoPunctuation();
   begin
      Assert(not CanEnd());
      if (Tokens[CurrentToken] = ',') then
         Fail('I don''t understand your use of commas.');
      if (Tokens[CurrentToken] = ';') then
         Fail('I don''t understand your use of semicolons.');
      if (Tokens[CurrentToken] = ':') then
         Fail('I don''t understand your use of colons.');
      if (Tokens[CurrentToken] = '.') then
         Fail('I don''t understand your use of periods.');
      if (Tokens[CurrentToken] = '?') then
         Fail('I don''t understand your use of question marks.');
      if (Tokens[CurrentToken] = '!') then
         Fail('I don''t understand your use of exclamation marks.');
      if ((Length(Tokens[CurrentToken]) >= 2) and (Tokens[CurrentToken][1] = '"')) then
         Fail('I don''t understand your use of quotation marks.');
   end;

   procedure SkipComma();
   begin
      if ((CurrentToken < Length(Tokens)) and (Tokens[CurrentToken] = ',')) then
         Inc(CurrentToken);
   end;

   function GetContinuation(): Boolean; { returns true if there's another action to parse }
   begin
      Assert(CanEnd());
      if (CurrentToken < Length(Tokens)) then
      begin
         if (Tokens[CurrentToken] = ';') then
         begin
            Inc(CurrentToken);
            if (CurrentToken >= Length(Tokens)) then
               Fail('I don''t understand your use of semicolons.');
         end
         else
         if (Tokens[CurrentToken] = ',') then
         begin
            Inc(CurrentToken);
            if (CurrentToken >= Length(Tokens)) then
               raise Exception.Create('Found inconsistency between CanEnd() and GetContinuation() when parsing comma in "' + Command + '".');
         end;
         if (Tokens[CurrentToken] = 'and') then
         begin
            Inc(CurrentToken);
            SkipComma();
            if (CurrentToken >= Length(Tokens)) then
               Fail('And what?');
         end;
         if (Tokens[CurrentToken] = 'then') then
         begin
            Inc(CurrentToken);
            SkipComma();
            if (CurrentToken >= Length(Tokens)) then
               Fail('Then what?');
         end;
         Result := True;
      end
      else
         Result := False;
   end;

   type
      TDirectionOptions = set of (optAllowDefiniteArticle, optRequireCompasDirection);

   function TryDirection(out Direction: TCardinalDirection; Options: TDirectionOptions): Boolean;
   var
      FoundDefiniteArticle: Boolean = False;
   begin
      Result := False;
      if ((optAllowDefiniteArticle in Options) and (Tokens[CurrentToken] = 'the')) then
      begin
         FoundDefiniteArticle := True;
         Inc(CurrentToken);
         if (CanEnd()) then
         begin
            Dec(CurrentToken);
            Exit;
         end;
      end;
      if (Tokens[CurrentToken] = 'north') or (Tokens[CurrentToken] = 'n') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdNorth;
         if (not CanEnd()) then
         begin
            if (Tokens[CurrentToken] = 'east') or (Tokens[CurrentToken] = 'e') then
            begin
               Inc(CurrentToken);
               Direction := cdNorthEast;
            end
            else if (Tokens[CurrentToken] = 'west') or (Tokens[CurrentToken] = 'w') then
            begin
               Inc(CurrentToken);
               Direction := cdNorthWest;
            end
         end;
      end
      else if (Tokens[CurrentToken] = 'northeast') or (Tokens[CurrentToken] = 'ne') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdNorthEast;
      end
      else if (Tokens[CurrentToken] = 'east') or (Tokens[CurrentToken] = 'e') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdEast;
      end
      else if (Tokens[CurrentToken] = 'southeast') or (Tokens[CurrentToken] = 'se') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdSouthEast;
      end
      else if (Tokens[CurrentToken] = 'south') or (Tokens[CurrentToken] = 's') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdSouth;
         if (not CanEnd()) then
         begin
            if (Tokens[CurrentToken] = 'east') or (Tokens[CurrentToken] = 'e') then
            begin
               Inc(CurrentToken);
               Direction := cdSouthEast;
            end
            else if (Tokens[CurrentToken] = 'west') or (Tokens[CurrentToken] = 'w') then
            begin
               Inc(CurrentToken);
               Direction := cdSouthWest;
            end
         end;
      end
      else if (Tokens[CurrentToken] = 'southwest') or (Tokens[CurrentToken] = 'sw') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdSouthWest;
      end
      else if (Tokens[CurrentToken] = 'west') or (Tokens[CurrentToken] = 'w') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdWest;
      end
      else if (Tokens[CurrentToken] = 'northwest') or (Tokens[CurrentToken] = 'nw') then
      begin
         Result := True;
         Inc(CurrentToken);
         Direction := cdNorthWest;
      end
      else if ((not FoundDefiniteArticle) and (not (optRequireCompasDirection in Options))) then
      begin
         if ((Tokens[CurrentToken] = 'up') or (Tokens[CurrentToken] = 'u')) then
         begin
            Result := True;
            Inc(CurrentToken);
            Direction := cdUp;
         end
         else if ((Tokens[CurrentToken] = 'down') or (Tokens[CurrentToken] = 'd')) then
         begin
            Result := True;
            Inc(CurrentToken);
            Direction := cdDown;
         end
         else if ((Tokens[CurrentToken] = 'out') or (Tokens[CurrentToken] = 'outside')) then
         begin
            Result := True;
            Inc(CurrentToken);
            Direction := cdOut;
         end;
      end;
      if (FoundDefiniteArticle and not Result) then
         Dec(CurrentToken);
   end;

   function TryPrepositionDirection(Preposition: UTF8String; out Direction: TCardinalDirection): Boolean;
   begin
      Result := False;
      if (Tokens[CurrentToken] = Preposition) then
      begin
         Inc(CurrentToken);
         if ((not CanEnd()) and (TryDirection(Direction, [optAllowDefiniteArticle, optRequireCompasDirection]))) then
            Result := True
         else
            Dec(CurrentToken);
      end;
   end;

   { You can assume that either CanEnd() is true or EnsureNoPunctuation() has been called just before this returns }
   { Do not call if CanEnd(). }
   function GetThing(out Thing: TThing; EndingClauses: TEndingClauseKinds; VerbString: UTF8String): Boolean;
   var
      Things: TThingList;
      Disambiguate: Boolean;
      Count: Cardinal;
   begin
      Assert(not CanEnd());
      EnsureNoPunctuation();
      Result := GlobalThingCollector.Collect(Self, Tokens, OriginalTokens, CurrentToken, [], [], EndingClauses, VerbString);
      if (Result) then
      begin
         Count := GlobalThingCollector.GetTokenCount();
         Disambiguate := GlobalThingCollector.GetDisambiguate();
         Things := GlobalThingCollector.GetThingList(); { resets the thing collector }
         Assert(Things.Length = 1);
         try
            if (not CanEnd()) then
               EnsureNoPunctuation();
            Thing := Things.First;
            Inc(CurrentToken, Count);
            if (Disambiguate) then
               AutoDisambiguated(Thing.GetDefiniteName(Self)); // XXX should really let the caller do that
         finally
            Things.Free();
         end;
      end;
   end;

   { You can assume that either CanEnd() is true or EnsureNoPunctuation() has been called just before this returns }
   { Do not call if CanEnd(). }
   function GetThing(out Thing: TAtom; EndingClauses: TEndingClauseKinds; VerbString: UTF8String): Boolean;
   var
      ActualThing: TThing;
   begin
      Result := GetThing(ActualThing, EndingClauses, VerbString);
      Thing := ActualThing;
   end;

   { You can assume that either CanEnd() is true or EnsureNoPunctuation() has been called just before this returns }
   { Do not call if CanEnd(). }
   function GetThings(out Things: TThingList; AllImpliedScope: TAllImpliedScope; EndingClauses: TEndingClauseKinds; VerbString: UTF8String; NothingFail: UTF8String = ''): Boolean;
   var
      Disambiguate: Boolean;
      Count: Cardinal;
   begin
      Things := nil;
      Assert(not CanEnd());
      EnsureNoPunctuation();
      Result := GlobalThingCollector.Collect(Self, Tokens, OriginalTokens, CurrentToken, [optAllowMultiples], AllImpliedScope, EndingClauses, VerbString);
      if (Result) then
      begin
         Count := GlobalThingCollector.GetTokenCount();
         Disambiguate := GlobalThingCollector.GetDisambiguate();
         Things := GlobalThingCollector.GetThingList(); { resets the thing collector }
         try
            if (not CanEnd()) then
               EnsureNoPunctuation();
            if (Things.Length = 0) then
            begin
               if (NothingFail <> '') then
                  Fail(NothingFail)
               else
                  Fail('I don''t see anything to ' + VerbString + ' here.');
            end;
            Inc(CurrentToken, Count);
            if (Disambiguate) then
               AutoDisambiguated(Things.GetDefiniteString(Self, 'and')); // XXX should really let the caller do that
         except
            { in case we fail }
            Things.Free();
            Things := nil;
            raise;
         end;
      end;
   end;

   { You can assume EnsureNoPunctuation() has been called just before this returns }
   function GetCompoundPrepositionThing(Prepositions: array of UTF8String; EndingClauses: TEndingClauseKinds; VerbString, PrepositionString: UTF8String; out Thing: TThing): Boolean;
   var
      Direction: TCardinalDirection;
      Index: Cardinal;
   begin
      Assert(Length(Prepositions) > 0);
      if (CurrentToken + Length(Prepositions) <= Length(Tokens)) then
      begin
         for Index := Low(Prepositions) to High(Prepositions) do // $R-
         begin
{$IFDEF DEBUG_PARSER} Writeln('GetCompoundPrepositionThing() comparing "', Tokens[CurrentToken+Index], '" to "', Prepositions[Index], '"'); {$ENDIF}
            if (Tokens[CurrentToken+Index] <> Prepositions[Index]) then
            begin
               Result := False;
{$IFDEF DEBUG_PARSER} Writeln('GetCompoundPrepositionThing() failed'); {$ENDIF}
               Exit;
            end;
         end;
{$IFDEF DEBUG_PARSER} Writeln('GetCompoundPrepositionThing() invoking the thingseeker'); {$ENDIF}
         Inc(CurrentToken, Length(Prepositions));
         Result := True;
         if (CanEnd()) then
            Fail('What do you want to ' + VerbString + ' ' + PrepositionString + '?');
         if (not GetThing(Thing, EndingClauses, VerbString + ' ' + PrepositionString)) then
         begin
            if (TryDirection(Direction, [optAllowDefiniteArticle])) then
               Fail('That''s a direction, I don''t know how to ' + VerbString + ' ' + PrepositionString + ' it.')
            else
               Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here to ' + VerbString + ' ' + PrepositionString + '.');
         end;
      end
      else
         Result := False;
   end;

   { You can assume EnsureNoPunctuation() has been called just before this returns }
   function GetCompoundPrepositionThing(Prepositions: array of UTF8String; EndingClauses: TEndingClauseKinds; VerbString, PrepositionString: UTF8String; out Thing: TAtom): Boolean;
   var
      ActualThing: TThing;
   begin
      Result := GetCompoundPrepositionThing(Prepositions, EndingClauses, VerbString, PrepositionString, ActualThing);
      Thing := ActualThing;
   end;

   { You can assume EnsureNoPunctuation() has been called just before this returns }
   function GetPrepositionThing(Preposition: UTF8String; EndingClauses: TEndingClauseKinds; VerbString: UTF8String; out Thing: TAtom): Boolean;
   begin
      Result := GetCompoundPrepositionThing([Preposition], EndingClauses, VerbString, Preposition, Thing);
   end;

   { You can assume EnsureNoPunctuation() has been called just before this returns }
   function GetPrepositionThing(Preposition: UTF8String; EndingClauses: TEndingClauseKinds; VerbString: UTF8String; out Thing: TThing): Boolean;
   begin
      Result := GetCompoundPrepositionThing([Preposition], EndingClauses, VerbString, Preposition, Thing);
   end;

   function GetLook(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if (Tokens[CurrentToken] = 'look') or (Tokens[CurrentToken] = 'l') then
      begin
         Inc(CurrentToken);
         Result := True;
         if (CanEnd()) then
         begin
            Action.Verb := avLook;
         end
         else
         if (TryDirection(Action.LookDirection, []) or
             TryPrepositionDirection('at', Action.LookDirection) or
             TryPrepositionDirection('to', Action.LookDirection) or
             TryPrepositionDirection('towards', Action.LookDirection)) then
         begin
            Action.Verb := avLookDirectional;
            if (not CanEnd()) then
               Fail('I don''t understand how to look ' + CardinalDirectionToString(Action.LookDirection) + ' "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         if (GetPrepositionThing('at', [], 'look', Action.LookAtSubject)) then
         begin
            Action.Verb := avLookAt;
            if (not CanEnd()) then
               Fail('I don''t understand how to look at things "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         if (GetPrepositionThing('under', [], 'look', Action.LookUnder)) then
         begin
            Action.Verb := avLookUnder;
            if (not CanEnd()) then
               Fail('I don''t understand how to look under things "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         if (GetPrepositionThing('below', [], 'look', Action.LookUnder)) then
         begin
            Action.Verb := avLookUnder;
            if (not CanEnd()) then
               Fail('I don''t understand how to look below things "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         if (GetPrepositionThing('in', [], 'look', Action.LookIn)) then
         begin
            Action.Verb := avLookIn;
            if (not CanEnd()) then
               Fail('I don''t understand how to look in things "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         if (GetPrepositionThing('inside', [], 'look', Action.LookIn)) then
         begin
            Action.Verb := avLookIn;
            if (not CanEnd()) then
               Fail('I don''t understand how to look inside things "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         if (Tokens[CurrentToken] = 'for') then
         begin
            Inc(CurrentToken);
            Action.Verb := avFind;
            if (CanEnd()) then
               Fail('What do you want to look for?');
            if (not GetThing(Action.FindSubject, [], 'look for')) then
            begin
               if (TryDirection(Direction, [optAllowDefiniteArticle])) then
                  Fail('I don''t understand how to look for a direction.')
               else
                  Fail('I can''t find anything like ' + IndefiniteArticle(Tokens[CurrentToken]) + ' "' + OriginalTokens[CurrentToken] + '" here.');
            end;
            if (not CanEnd()) then
               Fail('I don''t understand how to look for things "' + OriginalTokens[CurrentToken] + '".');
         end
         else
         begin
            Fail('I don''t understand how to look "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
         Result := False;
   end;

   function GetExamine(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if (Tokens[CurrentToken] = 'examine') or (Tokens[CurrentToken] = 'x') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avExamine;
         if (CanEnd()) then
            Fail('What do you want to examine?');
         if (not GetThing(Action.ExamineSubject, [], 'examine')) then
         begin
            if (TryDirection(Direction, [optAllowDefiniteArticle])) then
               Fail('That''s a direction, I don''t know how to examine it.')
            else
               Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here to examine.');
         end;
         if (not CanEnd()) then
            Fail('I don''t understand how to examine things "' + OriginalTokens[CurrentToken] + '".');
      end
      else
         Result := False;
   end;

   function GetRead(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if (Tokens[CurrentToken] = 'read') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avRead;
         if (CanEnd()) then
            Fail('A fine notion, but what do you want to read?');
         if (not GetThing(Action.ReadSubject, [], 'read')) then
         begin
            if (TryDirection(Direction, [optAllowDefiniteArticle])) then
               Fail('That''s a direction, I don''t know how to read it.')
            else
               Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here to read.');
         end;
         if (not CanEnd()) then
            Fail('I don''t understand how to read things "' + OriginalTokens[CurrentToken] + '".');
      end
      else
         Result := False;
   end;

   function GetInventory(var Action: TAction): Boolean;
   begin
      if (Tokens[CurrentToken] = 'inventory') or (Tokens[CurrentToken] = 'i') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avInventory;
         if (not CanEnd()) then
         begin
            EnsureNoPunctuation();
            Fail('I don''t understand how to inventory "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
         Result := False;
   end;

   function GetFind(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if (Tokens[CurrentToken] = 'find') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avFind;
         if (CanEnd()) then
            Fail('What do you want to find?');
         if (TryDirection(Direction, [optAllowDefiniteArticle, optRequireCompasDirection])) then
            Fail(Capitalise(CardinalDirectionToDefiniteString(Direction)) + ' is ' + CardinalDirectionToDirectionString(Direction) + '.')
         else
         if (not GetThing(Action.FindSubject, [], 'find')) then
         begin
            if (TryDirection(Direction, [optAllowDefiniteArticle])) then
               Fail('I don''t understand how to find a direction.')
            else
               Fail('I can''t find anything like ' + IndefiniteArticle(Tokens[CurrentToken]) + ' "' + OriginalTokens[CurrentToken] + '" here.');
         end;
         if (not CanEnd()) then
            Fail('I don''t understand how to find things "' + OriginalTokens[CurrentToken] + '".');
      end
      else
         Result := False;
   end;

   function GetGo(var Action: TAction): Boolean;
   begin
      if (Tokens[CurrentToken] = 'go') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avGo;
         if (CanEnd()) then
            Fail('Where do you want to go?');
         EnsureNoPunctuation();
         if (not TryDirection(Action.GoDirection, [])) then
            Fail('I don''t understand how to go "' + OriginalTokens[CurrentToken] + '".');
         if (not CanEnd()) then
         begin
            EnsureNoPunctuation();
            Fail('I don''t understand how to go ' + CardinalDirectionToString(Action.GoDirection) + ' "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
      if (TryDirection(Action.GoDirection, [optAllowDefiniteArticle])) then
      begin
         Result := True;
         Action.Verb := avGo;
         if (not CanEnd()) then
         begin
            EnsureNoPunctuation();
            Fail('I don''t understand how to go ' + CardinalDirectionToString(Action.GoDirection) + ' "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
         Result := False;
   end;

   function GetEnter(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if (Tokens[CurrentToken] = 'enter') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avEnter;
         if (CanEnd()) then
            Fail('What do you want to enter?');
         if (GetThing(Action.EnterSubject, [], 'enter')) then
         begin
            if (not CanEnd()) then
            begin
               EnsureNoPunctuation();
               Fail('I don''t understand how to enter things "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
         if (TryDirection(Direction, [optAllowDefiniteArticle])) then
            Fail('That''s a direction, I don''t know how to enter it.')
         else
            Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
      end
      else
         Result := False;
   end;

   function GetLeave(var Action: TAction): Boolean;
   var
      Verb: UTF8String;
   begin
      if ((Tokens[CurrentToken] = 'leave') or (Tokens[CurrentToken] = 'exit')) then
      begin
         Verb := Tokens[CurrentToken];
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avGo;
         if (not CanEnd()) then
         begin
            EnsureNoPunctuation();
            if (not TryDirection(Action.GoDirection, [])) then
               Fail('I don''t understand how to ' + Verb + ' "' + OriginalTokens[CurrentToken] + '".');
            if (not CanEnd()) then
            begin
               EnsureNoPunctuation();
               Fail('I don''t understand how to ' + Verb + ' ' + CardinalDirectionToString(Action.GoDirection) + ' "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
            Action.GoDirection := cdOut;
      end
      else
         Result := False;
   end;

   function GetTake(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if ((Tokens[CurrentToken] = 'take') or
          (Tokens[CurrentToken] = 'get') or
          (Tokens[CurrentToken] = 'grab') or
          (Tokens[CurrentToken] = 'carry')) then
      begin
         Result := True;
         Inc(CurrentToken);
      end
      else
      if ((CurrentToken + 1 < Length(Tokens)) and (Tokens[CurrentToken] = 'pick') and (Tokens[CurrentToken+1] = 'up')) then
      begin
         Result := True;
         Inc(CurrentToken, 2);
      end
      else
         Result := False;
      if (Result) then          
      begin
         Action.Verb := avTake;
         Action.TakeSubject := nil;
         if (CanEnd()) then
            Fail('What do you want to take?');
         if (GetThings(Action.TakeSubject, [aisSurroundings], [], 'take')) then
         begin
            if (not CanEnd()) then
            begin
               EnsureNoPunctuation();
               Fail('I don''t understand how to take things "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
         if (TryDirection(Direction, [optAllowDefiniteArticle])) then
            Fail('That''s a direction, I don''t know how to take it.')
         else
            Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
      end;
   end;

   function GetPut(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
      Verb, WhatQ, DirectionQ: UTF8String;
   begin
      if (Tokens[CurrentToken] = 'put') then
      begin
         Result := True;
         Action.PutCarefully := True;
         Verb := 'put';
         WhatQ := 'What do you want to put somewhere?';
         DirectionQ := 'That''s a direction; it is where it is.';
      end
      else if (Tokens[CurrentToken] = 'place') then
      begin
         Result := True;
         Action.PutCarefully := True;
         Verb := 'place';
         WhatQ := 'What do you want to carefully place somewhere?';
         DirectionQ := 'You can''t place a direction.';
      end
      else if (Tokens[CurrentToken] = 'drop') then
      begin
         Result := True;
         Action.PutCarefully := False;
         Verb := 'drop';
         WhatQ := 'What do you want to carelessly drop?';
         DirectionQ := 'What do you want to drop there?';
      end
      else
         Result := False;
      if (Result) then
      begin
         Inc(CurrentToken);
         Action.Verb := avPut;
         Action.PutSubject := nil;
         if (CanEnd()) then
            Fail(WhatQ);
         if (GetThings(Action.PutSubject, [aisSelf], [eckIn, eckOn], Verb, 'You have nothing to ' + Verb + ' anywhere.')) then
         begin
            if (CanEnd()) then
            begin
               if (FPosition in tpContained) then
               begin
                  Action.PutPosition := tpIn;
                  Action.PutTarget := FParent.GetInside(Action.PutPosition)
                  {$IFOPT C+} ; Assert(Assigned(Action.PutTarget)); Action.PutTarget := Action.PutTarget {$ENDIF}
                                                                           .GetSurface();
               end
               else
               begin
                  Action.PutPosition := tpOn;
                  Action.PutTarget := FParent.GetRepresentative().GetSurface();
               end;
               Assert(Assigned(Action.PutTarget));
               if (Action.PutCarefully) then
                  AutoDisambiguated(ThingPositionToString(Action.PutPosition) + ' ' + Action.PutTarget.GetDefiniteName(Self));
            end
            else
            if (GetPrepositionThing('onto', [], Verb + ' things', Action.PutTarget) or
                GetCompoundPrepositionThing(['on', 'to'], [], Verb + ' things', 'on', Action.PutTarget) or
                GetPrepositionThing('on', [], Verb + ' things', Action.PutTarget)) then
            begin
               Action.PutPosition := tpOn;
               if (not CanEnd()) then
                  Fail('I don''t understand how to ' + Verb + ' things on other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (GetPrepositionThing('into', [], Verb + ' things', Action.PutTarget) or
                GetCompoundPrepositionThing(['in', 'to'], [], Verb + ' things', 'in', Action.PutTarget) or
                GetPrepositionThing('in', [], Verb + ' things', Action.PutTarget) or
                GetPrepositionThing('inside', [], Verb + ' things', Action.PutTarget)) then
            begin
               Action.PutPosition := tpIn;
               if (not CanEnd()) then
                  Fail('I don''t understand how to ' + Verb + ' things in other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            begin
               Fail('I don''t understand how to ' + Verb + ' things "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
         if (TryDirection(Direction, [optAllowDefiniteArticle])) then
            Fail(DirectionQ)
         else
            Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
      end;
   end;

   function GetMove(var Action: TAction): Boolean;
   var
      PositionOverride: TThingPosition;
   begin
      if (Tokens[CurrentToken] = 'move') then
      begin
         Inc(CurrentToken);
         if (CanEnd()) then
         begin
            // XXX pick a random direction and imply it
            Fail('Move in what direction?');
         end;
         if (TryDirection(Action.GoDirection, []) or
             TryPrepositionDirection('to', Action.GoDirection) or
             TryPrepositionDirection('towards', Action.GoDirection)) then
         begin
            Action.Verb := avGo;
            Result := True;
         end
         else
         if (GetPrepositionThing('onto', [], 'move', Action.ClimbOnSubject) or
             GetCompoundPrepositionThing(['on', 'to'], [], 'move', 'onto', Action.ClimbOnSubject)) then
         begin
            Action.Verb := avClimbOn;
            if (not CanEnd()) then
               Fail('I don''t understand how to move onto things "' + OriginalTokens[CurrentToken] + '".');
            Result := True;
         end
         else
         if (GetPrepositionThing('into', [], 'move', Action.EnterSubject) or
             GetCompoundPrepositionThing(['in', 'to'], [], 'move', 'into', Action.EnterSubject)) then
         begin
            Action.Verb := avEnter;
            if (not CanEnd()) then
               Fail('I don''t understand how to move into things "' + OriginalTokens[CurrentToken] + '".');
            Result := True;
         end
         else
         if (GetThings(Action.MoveSubject, [aisSurroundings, aisSelf], [eckIn, eckOn], 'move')) then
         begin
            Action.Verb := avMove;
            Result := True;
            if (CanEnd()) then
            begin
               Action.MoveTarget := nil;
               Action.MovePosition := tpOn;
            end
            else
            if (GetPrepositionThing('onto', [], 'move things', Action.MoveTarget) or
                GetPrepositionThing('over', [], 'move things', Action.MoveTarget) or
                GetCompoundPrepositionThing(['on', 'to'], [], 'move things', 'onto', Action.MoveTarget) or
                GetCompoundPrepositionThing(['on'], [], 'move things', 'onto', Action.MoveTarget)) then
            begin
               Action.MovePosition := tpOn;
               if (not CanEnd()) then
                  Fail('I don''t understand how to move things onto other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (GetPrepositionThing('into', [], 'move things', Action.MoveTarget) or
                GetCompoundPrepositionThing(['in', 'to'], [], 'move things', 'into', Action.MoveTarget) or
                GetCompoundPrepositionThing(['in'], [], 'move things', 'into', Action.MoveTarget) or
                GetCompoundPrepositionThing(['to', 'inside', 'of'], [], 'move things', 'into', Action.MoveTarget) or
                GetCompoundPrepositionThing(['to', 'inside'], [], 'move things', 'into', Action.MoveTarget)) then
            begin
               Action.MovePosition := tpIn;
               if (not CanEnd()) then
                  Fail('I don''t understand how to move things into other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (GetPrepositionThing('to', [], 'move things', Action.MoveTarget)) then
            begin
               PositionOverride := tpIn;
               if (Assigned(Action.MoveTarget.GetInside(PositionOverride))) then
               begin
                  // XXX could mention PositionOverride - '(inside hut, on floor)' - if we could also parse that
                  AutoDisambiguated('inside ' + Action.MoveTarget.GetDefiniteName(Self));
                  Action.MovePosition := tpIn;
               end
               else
               begin
                  AutoDisambiguated('on ' + Action.MoveTarget.GetDefiniteName(Self));
                  Action.MovePosition := tpOn;
               end;
               if (not CanEnd()) then
                  Fail('I don''t understand how to move things to other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (Tokens[CurrentToken] = 'out') then
            begin
               Inc(CurrentToken);
               Assert(@Action.MoveSubject = @Action.RemoveSubject);
               Action.Verb := avRemove;
               Action.RemoveFromPosition := tpIn;
               Action.RemoveFromObject := nil;
               if (not CanEnd()) then
               begin
                  if (Tokens[CurrentToken] = 'of') then
                  begin
                     Inc(CurrentToken);
                     if (CanEnd()) then
                        Fail('What do you want to move things out of?');
                     if (not GetThing(Action.RemoveFromObject, [], 'move things out of')) then
                        Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
                     if (not CanEnd()) then
                        Fail('I don''t understand how to move things out of things "' + OriginalTokens[CurrentToken] + '".');
                  end
                  else
                  begin
                     EnsureNoPunctuation();
                     Fail('I don''t understand how to move things out "' + OriginalTokens[CurrentToken] + '".');
                  end;
               end;
            end
            else
            if (Tokens[CurrentToken] = 'off') then
            begin
               Inc(CurrentToken);
               Assert(@Action.MoveSubject = @Action.RemoveSubject);
               Action.Verb := avRemove;
               Action.RemoveFromPosition := tpOn;
               Action.RemoveFromObject := nil;
               if (not CanEnd()) then
               begin
                  if (Tokens[CurrentToken] = 'of') then
                  begin
                     Inc(CurrentToken);
                     if (CanEnd()) then
                        Fail('What do you want to move things off?');
                  end;
                  if (not GetThing(Action.RemoveFromObject, [], 'move things off')) then
                     Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
                  if (not CanEnd()) then
                     Fail('I don''t understand how to move things off things "' + OriginalTokens[CurrentToken] + '".');
               end;
            end
            else
            if (TryDirection(Action.PushDirection, []) or
                TryPrepositionDirection('to', Action.PushDirection) or
                TryPrepositionDirection('towards', Action.PushDirection)) then
            begin
               Assert(@Action.MoveSubject = @Action.PushSubject);
               Action.Verb := avPush;
               if (not CanEnd()) then
               begin
                  EnsureNoPunctuation();
                  Fail('I don''t understand how to move things in a direction "' + OriginalTokens[CurrentToken] + '".');
               end;
            end
            else
            begin
               Fail('I don''t understand how to move things "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
            Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
      end
      else
         Result := False;
   end;

   function GetPush(var Action: TAction): Boolean;
   var
      PositionOverride: TThingPosition;
   begin
      if (Tokens[CurrentToken] = 'push') then
      begin
         Inc(CurrentToken);
         if (CanEnd()) then
            Fail('Push what in what direction?');
         if (GetThings(Action.PushSubject, [aisSurroundings], [eckIn, eckOn], 'push')) then
         begin
            Action.Verb := avPush;
            Result := True;
            if (CanEnd()) then
            begin
               Assert(@Action.PushSubject = @Action.PressSubject);
               Action.Verb := avPress;
            end
            else
            if (GetPrepositionThing('onto', [], 'push things', Action.MoveTarget) or
                GetCompoundPrepositionThing(['on', 'to'], [], 'push things', 'onto', Action.MoveTarget) or
                GetCompoundPrepositionThing(['on'], [], 'push things', 'onto', Action.MoveTarget)) then
            begin
               Assert(@Action.PushSubject = @Action.MoveSubject);
               Action.Verb := avMove;
               Action.MovePosition := tpOn;
               if (not CanEnd()) then
                  Fail('I don''t understand how to push things onto other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (GetPrepositionThing('into', [], 'push things', Action.MoveTarget) or
                GetCompoundPrepositionThing(['in', 'to'], [], 'push things', 'into', Action.MoveTarget) or
                GetCompoundPrepositionThing(['in'], [], 'push things', 'into', Action.MoveTarget) or
                GetCompoundPrepositionThing(['to', 'inside', 'of'], [], 'push things', 'into', Action.MoveTarget) or
                GetCompoundPrepositionThing(['to', 'inside'], [], 'push things', 'into', Action.MoveTarget)) then
            begin
               Assert(@Action.PushSubject = @Action.MoveSubject);
               Action.Verb := avMove;
               Action.MovePosition := tpIn;
               if (not CanEnd()) then
                  Fail('I don''t understand how to push things into other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (GetPrepositionThing('to', [], 'push things', Action.MoveTarget)) then
            begin
               PositionOverride := tpIn;
               if (Assigned(Action.MoveTarget.GetInside(PositionOverride))) then
               begin
                  // XXX could mention PositionOverride - '(inside hut, on floor)' - if we could also parse that
                  AutoDisambiguated('inside ' + Action.MoveTarget.GetDefiniteName(Self));
                  Assert(@Action.PushSubject = @Action.MoveSubject);
                  Action.Verb := avMove;
                  Action.MovePosition := tpIn;
               end
               else
               begin
                  AutoDisambiguated('on ' + Action.MoveTarget.GetDefiniteName(Self));
                  Assert(@Action.PushSubject = @Action.MoveSubject);
                  Action.Verb := avMove;
                  Action.MovePosition := tpOn;
               end;
               if (not CanEnd()) then
                  Fail('I don''t understand how to push things to other things "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            if (TryDirection(Action.PushDirection, []) or
                TryPrepositionDirection('to', Action.PushDirection) or
                TryPrepositionDirection('towards', Action.PushDirection)) then
            begin
               if (not CanEnd()) then
               begin
                  EnsureNoPunctuation();
                  Fail('I don''t understand how to push things in a direction "' + OriginalTokens[CurrentToken] + '".');
               end;
            end
            else
            begin
               EnsureNoPunctuation();
               Fail('I don''t understand how to push things "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
            Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
      end
      else
         Result := False;
   end;

   function GetShake(var Action: TAction): Boolean;
   var
      Direction: TCardinalDirection;
   begin
      if (Tokens[CurrentToken] = 'shake') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avShake;
         Action.ShakeSubject := nil;
         if (CanEnd()) then
            Fail('What do you want to shake?');
         if (GetThings(Action.ShakeSubject, [aisSurroundings, aisSelf], [], 'shake')) then
         begin
            if (not CanEnd()) then
            begin
               EnsureNoPunctuation();
               Fail('I don''t understand how to shake things "' + OriginalTokens[CurrentToken] + '".');
            end;
         end
         else
         if (TryDirection(Direction, [optAllowDefiniteArticle])) then
            Fail('That''s a direction, I don''t know how to shake it.')
         else
            Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
      end
      else
         Result := False;
   end;

   function GetDig(var Action: TAction): Boolean;
   var
      Message: UTF8String;
      GotThing: Boolean;
   begin
      if (Tokens[CurrentToken] = 'dig') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avDig;
         Action.DigSpade := nil;
         Action.DigTarget := nil;
         GotThing := False;
         if (not CanEnd()) then
         begin
            if (TryDirection(Action.DigDirection, [])) then
               Action.Verb := avDigDirection
            else
               GotThing := GetThing(Action.DigTarget, [], 'dig');
            if (not CanEnd()) then
            begin
               if (GetPrepositionThing('with', [], 'dig', Action.DigSpade)) then
               begin
                  if (not CanEnd()) then
                     Fail('I don''t understand how to dig "' + OriginalTokens[CurrentToken] + '".');
               end
               else
               begin
                  if (GotThing) then
                     Fail('I don''t understand how to dig "' + OriginalTokens[CurrentToken] + '".')
                  else
                     Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here.');
               end;
            end;
         end;
         Message := '';
         if ((Action.Verb = avDig) and (not Assigned(Action.DigTarget))) then
         begin
            Action.DigTarget := GetImpliedThing([aisSurroundings, aisSelf], [tfDiggable]);
            if (Assigned(Action.DigTarget)) then
               Message := Action.DigTarget.GetDefiniteName(Self)
            else
               Fail('What do you want to dig?');
         end;
         if (not Assigned(Action.DigSpade)) then
         begin
            Action.DigSpade := GetImpliedThing([aisSurroundings, aisSelf], [tfCanDig]);
            if (Assigned(Action.DigSpade)) then
            begin
               if (Length(Message) > 0) then
                  Message := Message + ' ';
               Message := Message + 'with ' + Action.DigSpade.GetDefiniteName(Self)
            end
            else
            begin
               Fail('What do you want to dig with?');
            end;
         end;
         if (Length(Message) > 0) then
            AutoDisambiguated(Message);
      end
      else
         Result := False;
   end;

   function GetTalk(var Action: TAction): Boolean;
   begin
      if ((Tokens[CurrentToken] = 'shout') or (Tokens[CurrentToken] = 'yell')) then
      begin
         Result := True;
         Action.TalkVolume := tvShouting;
      end
      else
      if (Tokens[CurrentToken] = 'say') then
      begin
         Result := True;
         Action.TalkVolume := tvSpeaking;
      end
      else
      if (Tokens[CurrentToken] = 'whisper') then
      begin
         Result := True;
         Action.TalkVolume := tvWhispering;
      end
      else
         Result := False;
      if (Result) then
      begin
         Inc(CurrentToken);
         if (CanEnd()) then
            Fail('What do you want to say?');
         if (not IsQuote()) then
            EnsureNoPunctuation();
         Action.Verb := avTalk;
         New(Action.TalkMessage);
         Action.TalkMessage^.Message := Tokens[CurrentToken];
         Action.TalkTarget := nil;
         Inc(CurrentToken);
         if (not CanEnd()) then
         begin
            if (GetPrepositionThing('to', [], 'say something', Action.TalkTarget)) then
            begin
               if (not CanEnd()) then
                  Fail('I don''t understand how to say something to ' + Action.TalkTarget.GetIndefiniteName(Self) + ' "' + OriginalTokens[CurrentToken] + '".');
            end
            else
            begin
               EnsureNoPunctuation();
               Fail('I don''t understand how to say something "' + OriginalTokens[CurrentToken] + '". Please quote what you want to say.');
            end;
         end;
      end;
   end;

   function GetDance(var Action: TAction): Boolean;
   begin
      if (Tokens[CurrentToken] = 'dance') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avDance;
         if (not CanEnd()) then
         begin
            EnsureNoPunctuation();
            Fail('I don''t understand how to dance "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
         Result := False;
   end;

   function GetBug(var Action: TAction): Boolean;
   begin
      if (Tokens[CurrentToken] = 'bug') then
      begin
         Fail('Your bug report has been noted. Whether anyone will do anything about it is another question.');
         // XXX should also support "pray", to notify an admin
         // XXX should also support "i wish ..."
      end
      else
      {$IFDEF DEBUG}
      if (Tokens[CurrentToken] = 'debug') then
      begin
         Inc(CurrentToken);
         if (not CanEnd()) then
         begin
            if (Tokens[CurrentToken] = 'status') then
            begin
               Inc(CurrentToken);
               Result := True;
               Action.Verb := avDebugStatus;
               if (not CanEnd()) then
               begin
                  EnsureNoPunctuation();
                  Fail('Syntax: "debug status"');
               end;
            end
            else
            if (Tokens[CurrentToken] = 'location') then
            begin
               Inc(CurrentToken);
               Result := True;
               Action.Verb := avDebugLocation;
               if (not CanEnd()) then
               begin
                  EnsureNoPunctuation();
                  Fail('Syntax: "debug location"');
               end;
            end
            else
            if (Tokens[CurrentToken] = 'things') then
            begin
               Inc(CurrentToken);
               Result := True;
               Action.Verb := avDebugThings;
               Action.DebugThings := nil;
               if (not CanEnd()) then
               begin
                  if (not GetThings(Action.DebugThings, [aisSelf, aisSurroundings], [], 'debug', 'There''s nothing implicitly accessible here. Use "debug things" to get a list of explicitly accessible things.')) then
                     Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here. Use "debug things" to get a list of accessible things.');
                  if (not CanEnd()) then
                     Fail('Syntax: "debug things <things>". The word "' + OriginalTokens[CurrentToken] + '" doesn''t seem to be part of the name of ' + Action.DebugThings.GetLongDefiniteString(Self, 'or') + '; use "debug things" to get a list of accessible things if that isn''t what you meant.');
               end;
            end
            else
            if (Tokens[CurrentToken] = 'thing') then
            begin
               Inc(CurrentToken);
               Result := True;
               Action.Verb := avDebugThing;
               if (CanEnd()) then
                  Fail('Syntax: "debug thing <thing>"');
               if (not GetThing(Action.DebugThing, [], 'debug')) then
                  Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here. Use "debug things" to get a list of accessible things.');
               if (not CanEnd()) then
                  Fail('Syntax: "debug thing <thing>". The word "' + OriginalTokens[CurrentToken] + '" doesn''t seem to be part of the name of ' + Action.DebugThing.GetDefiniteName(Self) + '; use "debug things" to get a list of accessible things if that isn''t what you meant.');
            end
            else
            if (Tokens[CurrentToken] = 'teleport') then
            begin
               // XXX also support specifiying position to teleport to
               Inc(CurrentToken);
               Result := True;
               Action.Verb := avDebugTeleport;
               if (CanEnd()) then
                  Fail('Syntax: "debug teleport <thing>"');
               if (not GetThing(Action.DebugTarget, [], 'debug')) then
                  Fail('I can''t see any "' + OriginalTokens[CurrentToken] + '" here. Use "debug things" to get a list of accessible things.');
               if (not CanEnd()) then
                  Fail('Syntax: "debug teleport <thing>". The word "' + OriginalTokens[CurrentToken] + '" doesn''t seem to be part of the name of ' + Action.DebugTarget.GetDefiniteName(Self) + '; use "debug things" to get a list of accessible things if that isn''t what you meant.');
            end
            else
            begin
               EnsureNoPunctuation();
               Fail('Unrecognised debug command.');
            end;
         end
         else
            Fail('Syntax: "debug <command>", where <command> is one of "status", "location", "things [<things>]", "thing <thing>", "teleport <thing>".');
      end
      else
      {$ENDIF}
         Result := False;
   end;

   function GetHelp(var Action: TAction): Boolean;
   begin
      if (Tokens[CurrentToken] = 'help') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avHelp;
         if (not CanEnd()) then
         begin
            // XXX we should add 'help <thing>', especially 'help me'.
            // XXX not sure how to respond to 'help fred' where fred is another player, though. Maybe just "How?"?
            EnsureNoPunctuation();
            Fail('I don''t understand how to help "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
         Result := False;
   end;

   function GetQuit(var Action: TAction): Boolean;
   begin
      if (Tokens[CurrentToken] = 'quit') then
      begin
         Inc(CurrentToken);
         Result := True;
         Action.Verb := avQuit;
         if (not CanEnd()) then
         begin
            EnsureNoPunctuation();
            Fail('I don''t understand how to quit "' + OriginalTokens[CurrentToken] + '".');
         end;
      end
      else
         Result := False;
   end;

   function ParseAction(var Action: TAction): Boolean; { returns true if there's another action to parse }
   begin
      if (GetLook(Action) or GetExamine(Action) or GetRead(Action) or GetInventory(Action) or GetFind(Action) or
          GetGo(Action) or GetEnter(Action) or GetLeave(Action) or
          GetTake(Action) or GetPut(Action) or GetMove(Action) or GetPush(Action) or GetShake(Action) or GetDig(Action) or
          GetTalk(Action) or GetDance(Action) or
          GetBug(Action) or GetHelp(Action) or GetQuit(Action)) then
         Result := GetContinuation() { consumes the "next" and verifies there's something else coming }
      else
         Fail('I don''t understand how to "' + OriginalTokens[CurrentToken] + '".');
   end;
