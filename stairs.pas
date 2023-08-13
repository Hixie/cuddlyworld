{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit stairs;

interface

uses
   locations, things, grammarian, matcher, storable, physics, messages, thingdim;

type
   TVerticalPathLocation = class(TNamedLocation) // @RegisterStorableClass
    protected
      function GetPath(): TThing; // or nil if there isn't one
      function GetCouldBePath(Thing: TThing; ThingPosition: TThingPosition): Boolean;
      function GetBetween(Perspective: TAvatar; Context: TAtom): UTF8String;
   public
      procedure EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); override;
      function GetBasicDescription(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      function GetPresenceStatementFragment(Perspective: TAvatar; PertinentPosition: TThingPosition): UTF8String; override;
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetAtomForDirectionalNavigation(Direction: TCardinalDirection): TAtom; override;
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
      function GetSurface(): TThing; override;
      property Path: TThing read GetPath; // the stairs, ladder, whatever; can be nil, if there's no path
   end;

   TStairwellOpening = class(TOpening) // @RegisterStorableClass
      function GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      procedure EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter); override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar); override;
   end;

   TStairs = class(TScenery) // @RegisterStorableClass
      function GetTransportationDestination(Perspective: TAvatar): TTransportationInstruction; override;
   end;
   
// Same as ConnectLocations but puts a stairwell between them. Direction is always up/down.
//
// Return value must be added to the World. (If you see a memory leak on exit, you probably forgot to do that.)
// If you omit loAutoDescribe from the last argument, then the stairs won't be mentioned in descriptions of rooms that contain them.
// Flags will always contain loPermissibleNavigationTarget and loThreshold regardless of the provided argument.
function ConnectStairs(BottomLocation, TopLocation: TLocation; Staircase: TThing = nil; Flags: TLandmarkOptions = [loAutoDescribe]): TVerticalPathLocation;

implementation

uses
   lists, exceptions, typinfo;

function ConnectStairs(BottomLocation, TopLocation: TLocation; Staircase: TThing = nil; Flags: TLandmarkOptions = [loAutoDescribe]): TVerticalPathLocation;
var
   Surface: TAtom;
   Opening: TThing;
begin
   Assert(Flags - [loAutoDescribe] = [], 'ConnectStairs flags is only allowed to contain loAutoDescribe.');
   if (not Assigned(Staircase)) then
   begin
      Staircase := TStairs.Create('stairs', '(stair/stairs stairwell/stairwells step/steps)@', 'The stairs have steps.');
      (Staircase as TScenery).CannotMoveExcuse := 'The stairs are part of the overall structure and quite immovable.';
   end;
   Result := TVerticalPathLocation.Create('stairwell', 'the stairwell', 'a stairwell', 'The stairwell is a space for vertical movement.');
   Result.Add(Staircase, tpInstalledIn);
   Flags := Flags + [loPermissibleNavigationTarget, loThreshold];
   Surface := BottomLocation.GetAtomForDirectionalNavigation(cdUp);
   if (Assigned(Surface) and (Surface is TThing)) then
   begin
      Opening := TStairwellOpening.Create('opening for the stairwell', '((opening/openings (for the? (up upwards)@? stairwell/((stairwells stairwell)@))?) ((up upwards)@? stairwell opening/openings))@', 'The stairs lead up through an opening for the stairwell.', Result, tsMassive);
      Surface.Add(Opening, tpDirectionalOpening);
      BottomLocation.AddLandmark(cdUp, Result, Flags + [loConsiderDirectionUnimportantWhenFindingChildren]);
   end
   else
   begin
      BottomLocation.AddLandmark(cdUp, Result, Flags + [loConsiderDirectionUnimportantWhenFindingChildren]);
   end;
   Surface := TopLocation.GetAtomForDirectionalNavigation(cdDown);
   if (Assigned(Surface) and (Surface is TThing)) then
   begin
      Opening := TStairwellOpening.Create('opening for the stairwell', '((opening/openings (for the? (down downwards)@? stairwell/((stairwells stairwell)@))?) ((down downwards)@? stairwell opening/openings))@', 'The stairs lead down through an opening for the stairwell.', Result, tsMassive);
      Surface.Add(Opening, tpDirectionalOpening);
      TopLocation.AddLandmark(cdDown, Result, Flags + [loConsiderDirectionUnimportantWhenFindingChildren]);
   end
   else
   begin
      TopLocation.AddLandmark(cdDown, Result, Flags + [loConsiderDirectionUnimportantWhenFindingChildren]);
   end;
   Result.AddLandmark(cdDown, BottomLocation, [loAutoDescribe, loPermissibleNavigationTarget]);
   Result.AddLandmark(cdUp, TopLocation, [loAutoDescribe, loPermissibleNavigationTarget]);
end;


procedure TVerticalPathLocation.EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
var
   ThePath: TThing;
begin
   inherited;
   ThePath := Path;
   if (Assigned(ThePath) and FromFarAway) then
   begin
      ThePath.EnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, True, FromFarAway, Directions, Reporter);
   end;
end;

function TVerticalPathLocation.GetBetween(Perspective: TAvatar; Context: TAtom): UTF8String;
const
   kNecessaryOptions = [loAutoDescribe, loPermissibleNavigationTarget];
var
   Direction: TCardinalDirection;
   Index: Cardinal;
   List: TAtomList;
begin
   Assert(Context <> Self);
   List := TAtomList.Create([slDropDuplicates]);
   for Direction in cdOrderedCardinalDirections do
      if (Length(FDirectionalLandmarks[Direction]) > 0) then
         for Index := Low(FDirectionalLandmarks[Direction]) to High(FDirectionalLandmarks[Direction]) do
         begin
            Assert(Context <> FDirectionalLandmarks[Direction][Index].Atom);
            if (FDirectionalLandmarks[Direction][Index].Options * kNecessaryOptions = kNecessaryOptions) then
               List.AppendItem(FDirectionalLandmarks[Direction][Index].Atom);
         end;
   if (List.Length >= 2) then
      Result := 'between ' + List.GetDefiniteString(Perspective, 'and')
   else
   if (List.Length = 1) then
      Result := 'near ' + List.First.GetDefiniteName(Perspective)
   else
      Result := '';
   List.Free();
end;

function TVerticalPathLocation.GetBasicDescription(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;
var
   ThePath: TThing;
begin
   ThePath := Path;
   if (Assigned(ThePath) and (Context = ThePath)) then
   begin
      Result := '[context is path]';
   end
   else
   begin
      Result := inherited;
   end;
end;

function TVerticalPathLocation.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
var
   ThePath: TThing;
   TheDestination: TAtom;
begin
   ThePath := Path;
   TheDestination := GetAtomForDirectionalNavigation(Direction);
   if (Assigned(ThePath) and Assigned(TheDestination) and (Direction in cdVerticalDirections)) then
   begin
      Result := Capitalise(ThePath.GetIndefiniteName(Perspective)) + ' ' + TernaryConditional('leads', 'lead', ThePath.IsPlural(Perspective)) + ' ' + CardinalDirectionToString(Direction) + ' to ' + TheDestination.GetDefiniteName(Perspective) + '.';
   end
   else
   begin
      Result := inherited;
   end;
end;

function TVerticalPathLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
var
   ThePath: TAtom;
   Surroundings: TAtom;
begin
   if (Direction = cdOut) then
   begin
      Surroundings := GetAtomForDirectionalNavigation(cdDown);
      if (Assigned(Surroundings)) then
      begin
         ThePath := Path;
         if (Assigned(ThePath)) then
            LeadingPhrase := LeadingPhrase + ' past ' + ThePath.GetDefiniteName(Perspective);
         Result := LeadingPhrase + ', ' + 
            Perspective.GetDefiniteName(Perspective) + ' ' +
            TernaryConditional('sees', 'see', Perspective.IsPlural(Perspective)) + ' ' +
            Surroundings.GetDefiniteName(Perspective) + '.' +
            WithSpaceIfNotEmpty(Surroundings.GetHorizonDescription(Perspective));
      end
      else
         Result := inherited;
   end
   else
      Result := inherited;
end;

function TVerticalPathLocation.GetDescriptionRemoteHorizon(Perspective: TAvatar; Context: TThing; Depth: Cardinal): UTF8String;
var
   ThePath: TAtom;
begin
   ThePath := Path;
   if (Assigned(ThePath) and (Context = ThePath)) then
   begin
      Result := ''; // we don't want to give the above/below stuff again because we already gave that in the presence statement (see GetPresenceStatementFragment)
   end
   else
   if (Depth = 0) then
   begin
      Result := GetDescriptionRemoteDetailed(Perspective, cdOut, 'Looking out', []);
   end
   else
      Result := inherited;
end;

function TVerticalPathLocation.GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String;
begin
   Result := GetBetween(Perspective, Context);
   if (Result = '') then
      Result := inherited;
end;

function TVerticalPathLocation.GetPresenceStatementFragment(Perspective: TAvatar; PertinentPosition: TThingPosition): UTF8String;
begin
   Result := GetBetween(Perspective, nil);
   if (Result = '') then
      Result := inherited;
end;

procedure TVerticalPathLocation.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Direction: TCardinalDirection;
begin
   for Direction := Low(FDirectionalLandmarks) to High(FDirectionalLandmarks) do
      if (Length(FDirectionalLandmarks[Direction]) > 0) then
         FDirectionalLandmarks[Direction][0].Atom.GetNearbyThingsByClass(List, True, Filter);
end;

function TVerticalPathLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   NotificationList.AppendItem(Self);
   Result := GetAtomForDirectionalNavigation(Direction);
   if (Assigned(Result)) then
      Result := Result.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   else
      Result := inherited;
end;

function TVerticalPathLocation.GetAtomForDirectionalNavigation(Direction: TCardinalDirection): TAtom;
begin
   if (Direction = cdOut) then
      Direction := cdDown;
   Result := inherited;
end;

function TVerticalPathLocation.GetPath(): TThing;
var
   Child: TThing;
begin
   Result := nil;
   for Child in FChildren do
      if (GetCouldBePath(Child, Child.Position)) then
      begin
         Assert(not Assigned(Result));
         Result := Child;
         {$IFOPT C-} exit; {$ENDIF}
      end;
end;

function TVerticalPathLocation.GetCouldBePath(Thing: TThing; ThingPosition: TThingPosition): Boolean;
begin
   Result := ThingPosition = tpInstalledIn;
end;

function TVerticalPathLocation.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
begin
   Assert(Message.IsValid);
   Result := inherited;
   if (not Assigned(Path) and (Direction in cdVerticalDirections)) then
      Result.RequiredAbilities := [naFly];
end;

procedure TVerticalPathLocation.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
var
   NoticeableThings: TThingList;
   ThePath: TThing;
begin
   NoticeableThings := TThingList.Create([slDropDuplicates]);
   try
      ThePath := GetPath();
      if (Assigned(ThePath)) then
      begin
         // not fomDeep
         ThePath.EnumerateChildren(NoticeableThings, tpNoticeable);
      end;
      if (NoticeableThings.Length > 0) then
      begin
         if (Perspective = Traveller) then
         begin
            Perspective.AutoDisambiguated('past ' + NoticeableThings.GetIndefiniteString(Perspective, 'and'));
         end;
      end;
   finally
      NoticeableThings.Free();
   end;
end;

function TVerticalPathLocation.GetSurface(): TThing;
begin
   Result := Path;
end;


function TStairwellOpening.GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
begin
   Result := FDestination.GetDescriptionRemoteBrief(Perspective, Mode, Direction) + ' @@@';
end;

function TStairwellOpening.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   Assert(Assigned(Traveller));
   if (CanEnter(Traveller, Direction, Perspective, Message)) then
   begin
      DisambiguationOpening := Self;       
      Result := FDestination.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList);
   end
   else
   begin
      Result := nil;
   end;
end;

procedure TStairwellOpening.EnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside, FromFarAway: Boolean; Directions: TCardinalDirectionSet; Reporter: TThingReporter);
begin
   if (FromFarAway) then
      FDestination.ProxiedEnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, True, True, Directions, Reporter);
   inherited;
end;

function TStairwellOpening.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; Options: TFindThingOptions): Boolean;
begin
   Result := FDestination.ProxiedFindThingTraverser(Thing, Perspective, Options);
end;

procedure TStairwellOpening.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FDestination.ProxiedFindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TStairwellOpening.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := FDestination.GetSurface().CanPut(Thing, tpOn, Care, Perspective, Message);
end;

procedure TStairwellOpening.Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
begin
   FDestination.GetSurface().Put(Thing, ThingPosition, Care, Perspective);
end;


function TStairs.GetTransportationDestination(Perspective: TAvatar): TTransportationInstruction;
var
   Ancestor: TAtom;
   SubjectiveInformation: TSubjectiveInformation;
   CandidateDirection: TCardinalDirection;
begin
   Ancestor := Self;
   Assert(Ancestor is TThing); // because we are a TThing
   repeat
      Ancestor := (Ancestor as TThing).Parent;
   until (not (Ancestor is TThing)) or (Ancestor = Perspective);
   if (Ancestor = Perspective) then
   begin
      Result.TravelType := ttNone;
   end
   else
   begin
      SubjectiveInformation := Perspective.Locate(Self, [foFindAnywhere, foWithPreciseDirections]);
      if (PopCnt(Cardinal(SubjectiveInformation.Directions)) <> 1) then
      begin
         // e.g. if you're on the stairs
         Result.TravelType := ttNone;
      end
      else
      begin
         Result.TravelType := ttByDirection;
         for CandidateDirection in SubjectiveInformation.Directions do
            Result.Direction := CandidateDirection; // there can only be one at this point, so this should be enough
      end;
   end;
end;

initialization
{$INCLUDE registrations/stairs.inc}
end.
