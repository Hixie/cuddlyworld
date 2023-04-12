{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
program tests;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, storable, matcher, lists, physics, player, locations, things, grammarian, cuddlycamp, world, threshold,
   testmechanics, testmechanics1, testmechanics2, testmechanics3, testmechanics4, testmechanics5,
   base64encoder, client; // client is used just to make sure it gets compiled when compiling tests

const
   TestSaveFileName = '/tmp/tests.map.dat';

procedure TestBase64Encoder();
var
   Failed: Boolean = False;

   procedure CheckBase64(S1, S2: UTF8String);
   begin
      if (Base64(S1) <> S2) then
      begin
         Writeln('Base64: Encoding "', S1, '" gave "', Base64(S1), '" instead of "', S2, '".');
         Failed := True;
      end;
   end;

begin
   // Test Vectors from RFC 4648
   CheckBase64('', '');
   CheckBase64('f', 'Zg==');
   CheckBase64('fo', 'Zm8=');
   CheckBase64('foo', 'Zm9v');
   CheckBase64('foob', 'Zm9vYg==');
   CheckBase64('fooba', 'Zm9vYmE=');
   CheckBase64('foobar', 'Zm9vYmFy');
   if (Failed) then
      Halt(1);
end;

procedure TestPlot();
var
   TestWorld, TestWorld2: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
   Failed: Boolean;
   OldF: File;
begin
   Writeln('PLOT');
   Proxy := TTestProxy.Create();
   Proxy.Test('InitEden()');
   TestWorld := InitEden();
   Failed := False;
   try
      try
         TestPlayer := TPlayer.Create('Tester', '', pIt);
         TestPlayer.Adopt(@Proxy.HandleAvatarMessage, @Proxy.HandleForceDisconnect);
         TestWorld.AddPlayer(TestPlayer);
         TestPlayer.AnnounceAppearance();

         { Starting room test }
         Proxy.Test('Starting location');
         Proxy.ExpectString('Cave');
         Proxy.SkipEverything();
         TestWorld.Perform('look', TestPlayer);
         Proxy.StopSkipping();

         Proxy.ExpectString('Tunnel Trail');
         Proxy.SkipEverything();
         TestWorld.Perform('east', TestPlayer);
         Proxy.StopSkipping();

         Proxy.ExpectString('(the embroidered bag of holding labeled Tester)');
         Proxy.ExpectString('You already have that.');
         TestWorld.Perform('take all, and bag', TestPlayer);

         { test round-tripping }
         Proxy.Test('Round-tripping');
         StoreObjectToFile(TestSaveFileName, TestWorld, kSaveDataVersion);
         TestWorld2 := ReadObjectFromFile(TestSaveFileName) as TWorld;
         TestWorld2.Free();
         Assign(OldF, TestSaveFileName);
         {$I-} Erase(OldF); {$I+}

         Proxy.Test('End');

      except
         on E: ETestError do
         begin
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            Proxy.Clear();
            Failed := True;
         end;
         on E: Exception do
         begin
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            raise;
         end;
      end;
      Proxy.ExpectDone();
      Proxy.ExpectDisconnect(True);
   finally
      EmptyDisposalQueue();
      TestWorld.Free();
      Proxy.Free();
   end;
   if (Failed) then
   begin
      Writeln('FAILURE DETECTED');
      Halt(1);
   end;
end;

procedure TestMatcher();
var
   TestID: Cardinal;
   Failed: Boolean;

   procedure RunMatcherTest(TestMatcher: TMatcher; Candidate: TTokens; Start: Cardinal; Pass: Cardinal; Flags: TMatcherFlags = 0);
   var
      Result: Cardinal;
   begin
      Inc(TestID);
      Result := TestMatcher.Matches(Candidate, Start, Flags);
      if (Result <> Pass) then
      begin
         Writeln('FAILED matcher test ', TestID, '; expected to match ', Pass, ' tokens but matched ', Result);
         Failed := True;
      end;
   end;

   procedure RunCanonicalMatchTest(TestMatcher: TMatcher; Pass: UTF8String; Flags: TMatcherFlags = 0);
   var
      Result: UTF8String;
   begin
      Inc(TestID);
      Result := TestMatcher.GetCanonicalMatch(' ', Flags);
      if (Result <> Pass) then
      begin
         Writeln('FAILED matcher test ', TestID, '; expected to find longest match "', Pass, '" but got "', Result, '"');
         Failed := True;
      end;
   end;

var
   TestMatcher, OtherMatcher: TMatcher;
   Strings: TTokens;
begin
   Writeln('PATTERN COMPILER');

   Failed := False;

   SetLength(Strings, 4); // $DFA- for Strings
   Strings[0] := 'the';
   Strings[1] := 'green';
   Strings[2] := 'lantern';
   Strings[3] := 'glowing';

   TestID := 0;

   CompilePattern('the', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the) green', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the green');
   RunMatcherTest(TestMatcher, Strings, 0, 2);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(((the green lantern glowing)))', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the green lantern glowing');
   RunMatcherTest(TestMatcher, Strings, 0, 4);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the? (green lantern)? glowing', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the green lantern glowing');
   RunMatcherTest(TestMatcher, Strings, 0, 4);
   RunMatcherTest(TestMatcher, Strings, 1, 3);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the? ((glowing green)# lantern)&', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing green lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 3);
   RunMatcherTest(TestMatcher, Strings, 1, 2);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(glowing lantern)@', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'glowing');
   RunMatcherTest(TestMatcher, Strings, 0, 0);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)*', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 2);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)#', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 2);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)%', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)&', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the+ glowing? lantern+', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 0);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the/fail FAIL?/fail green lantern/fail (FAIL FAIL)?/glowing', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the FAIL green lantern FAIL FAIL');
   RunMatcherTest(TestMatcher, Strings, 0, 3);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   RunCanonicalMatchTest(OtherMatcher, 'fail fail green fail glowing');
   RunMatcherTest(OtherMatcher, Strings, 0, 0);
   RunMatcherTest(OtherMatcher, Strings, 1, 0);
   RunMatcherTest(OtherMatcher, Strings, 2, 0);
   RunMatcherTest(OtherMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the (glowing)?/(yellow green blue red)& lantern/(glowing lantern)*', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 0);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   RunCanonicalMatchTest(OtherMatcher, 'the yellow green blue red glowing lantern');
   RunMatcherTest(OtherMatcher, Strings, 0, 4);
   RunMatcherTest(OtherMatcher, Strings, 1, 0);
   RunMatcherTest(OtherMatcher, Strings, 2, 0);
   RunMatcherTest(OtherMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('((((navy blue)& (wooden wood)@)# ((archway/archways arch/arches)@ (to the (north n)@)?))& (((navy blue)& (wooden wood)@ (north northern n)@)# (archway/archways arch/arches)@)&)@', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'navy blue wooden archway to the north');
   RunCanonicalMatchTest(OtherMatcher, 'navy blue wooden archways to the north');
   TestMatcher.Free();
   OtherMatcher.Free();

   SetLength(Strings, 3);
   Strings[0] := 'burning';
   Strings[1] := 'container';
   Strings[2] := 'open';

   CompilePattern('(burning:0 open:1 container/containers:2)#', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'burning container', 5); // flags 0 and 2 set
   RunMatcherTest(TestMatcher, Strings, 0, 2, 5); // flags 0 and 2 set
   RunMatcherTest(TestMatcher, Strings, 1, 1, 5); // flags 0 and 2 set
   RunMatcherTest(TestMatcher, Strings, 2, 0, 5); // flags 0 and 2 set
   RunMatcherTest(TestMatcher, Strings, 0, 0, 6); // flags 1 and 2 set
   RunMatcherTest(TestMatcher, Strings, 1, 2, 6); // flags 1 and 2 set
   RunMatcherTest(TestMatcher, Strings, 2, 1, 6); // flags 1 and 2 set
   RunCanonicalMatchTest(OtherMatcher, 'burning containers', 5); // flags 0 and 2 set
   RunMatcherTest(OtherMatcher, Strings, 0, 1, 5); // flags 0 and 2 set // test 86
   RunMatcherTest(OtherMatcher, Strings, 1, 0, 5); // flags 0 and 2 set
   RunMatcherTest(OtherMatcher, Strings, 2, 0, 5); // flags 0 and 2 set
   RunMatcherTest(OtherMatcher, Strings, 0, 0, 6); // flags 1 and 2 set
   RunMatcherTest(OtherMatcher, Strings, 1, 0, 6); // flags 1 and 2 set
   RunMatcherTest(OtherMatcher, Strings, 2, 1, 6); // flags 1 and 2 set
   TestMatcher.Free();
   OtherMatcher.Free();

   SetLength(Strings, 1);
   Strings[0] := 'side';

   CompilePattern('varnished? front?:1 side/sides', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'varnished side', 0);
   RunMatcherTest(TestMatcher, Strings, 0, 1, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   if (Failed) then Halt(1);
end;

type
   TMolecule = class(TSpade)
   end;

type
   TMoleculeList = specialize TStorableList<TMolecule>;

procedure TestLists();
var
   List1, List2: TMoleculeList;
   Mol1, Mol2, Mol3: TMolecule;
   Enum1, Enum2: TMoleculeList.TEnumerator;
   Failed: Boolean;
   TestCount: Cardinal;

   procedure Check(Success: Boolean; S: UTF8String);
   begin
      Inc(TestCount);
      if (not Success) then
      begin
         Writeln('Test #', TestCount, ': ', S);
         Failed := True;
      end;
   end;

begin
   RegisterStorableClass(TMoleculeList);
   RegisterStorableClass(TMolecule);

   Writeln('LISTS');
   Failed := False;
   TestCount := 0;
   Mol1 := TMolecule.Create();
   Mol2 := TMolecule.Create();
   Mol3 := TMolecule.Create();
   List1 := TMoleculeList.Create([slOwner]);
   List1.AppendItem(Mol1);
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List2 := TMoleculeList.Create([slOwner]);
   Enum1 := List2.GetEnumerator();
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   List2.AdoptItem(Enum1);
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List1.GetEnumerator();
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List2.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List1.AppendItem(Mol2);   
   List2.AdoptList(List1);
   Enum1 := List1.GetEnumerator();
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List2.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol2, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List2.AppendItem(Mol3);
   Enum1 := List2.GetEnumerator(tdForward);
   Check(Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Check(Enum1.Current = Mol1, 'Test after adding Mol3 failed');
   Check(Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Check(Enum1.Current = Mol2, 'Test after adding Mol3 failed');
   Check(Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Check(Enum1.Current = Mol3, 'Test after adding Mol3 failed');
   Check(not Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Enum1.Free();
   Enum1 := List2.GetEnumerator(tdForward);
   Check(Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.Current = Mol1, 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.Current = Mol2, 'Test for adopting Mol2 from the middle failed');
   List1.AdoptItem(Enum1);
   Check(Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.Current = Mol3, 'Test for adopting Mol2 from the middle failed');
   Check(not Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Enum1.Free();
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test checking Mol2 got adopted failed');
   Check(Enum1.Current = Mol2, 'Test checking Mol2 got adopted failed');
   Check(not Enum1.MoveNext(), 'Test checking Mol2 got adopted failed');
   Enum1.Free();

   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'List adoption test failed - setup');
   Check(Enum1.Current = Mol2, 'List adoption test failed - setup');
   Enum2 := List2.GetEnumerator();
   Check(Enum2.MoveNext(), 'List adoption test failed - setup');
   Check(Enum2.Current = Mol1, 'List adoption test failed - setup');
   Enum1.Free(); { can't have any live enumerators during adoption }
   List2.AdoptList(List1);
   Check(Enum2.MoveNext(), 'List adoption test failed - post adoption, pre traversal');
   Check(Enum2.Current = Mol3, 'List adoption test failed - post adoption, pre traversal: Mol3 missing');
   Check(Enum2.MoveNext(), 'List adoption test failed - post adoption, failed to traverse');
   Check(Enum2.Current = Mol2, 'List adoption test failed - post adoption: Mol2 missing');
   List1.AdoptItem(Enum2);
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol2, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Check(not Enum2.MoveNext(), 'Test failed');
   Enum2.Free();
   Enum2 := List2.GetEnumerator();
   Check(Enum2.MoveNext(), 'Test failed');
   Check(Enum2.Current = Mol1, 'Test failed');
   List1.AdoptItem(Enum2);
   Enum2.Free();
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol2, 'Test failed');
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List2.AdoptList(List1);
   Enum1 := List2.GetEnumerator(tdReverse);
   Check(Enum1.MoveNext(), 'Reverse test failed - List2 empty');
   Check(Enum1.Current = Mol1, 'Reverse test failed - Mol1 not at end');
   Check(Enum1.MoveNext(), 'Reverse test failed');
   Check(Enum1.Current = Mol2, 'Reverse test failed - Mol2 not in middle');
   Check(Enum1.MoveNext(), 'Reverse test failed');
   Check(Enum1.Current = Mol3, 'Reverse test failed - Mol3 not at start');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   { molecules get freed by lists }
   List1.Free();
   List2.Free();
   if (Failed) then Halt(1);
end;

begin
   Writeln('CuddlyWorld Tests initializing...');
   RegisterStorableClass(TTestWorld);
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   TestBase64Encoder();
   TestMatcher();
   TestLists();
   TestMechanics5.TestMechanics5();
   TestMechanics4.TestMechanics4();
   TestMechanics3.TestMechanics3();
   TestMechanics2.TestMechanics2();
   TestMechanics1.TestMechanics1();
   TestPlot();
   Writeln('CuddlyWorld Tests complete.');
end.
