

not( X ) :- X, !, fail.
not( _ ).

degmin_in_rads( degmin( Degrees, Minutes ), Rads ) :-
    Degs is Degrees + Minutes / 60,
    Rads is Degs * pi / 180.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961. 

timeInHours( time( Hours, Mins ), HoursToTime ) :-
    HoursToTime is Hours + Mins / 60.

distance( Airport1, Airport2, Distance ) :-
   airport( Airport1, _, Lat1, Lon1 ),
   airport( Airport2, _, Lat2, Lon2 ),
   degmin_in_rads( Lat1, Lat1_float ),
   degmin_in_rads( Lat2, Lat2_float ),
   degmin_in_rads( Lon1, Lon1_float ),
   degmin_in_rads( Lon2, Lon2_float ),
   haversine_radians( Lat1_float, Lon1_float, Lat2_float, Lon2_float,
               Distance).

printDigits( Timedigits ) :-
    Timedigits < 10, print( 0 ), print( Timedigits ). 

printDigits( Timedigits ) :-
    Timedigits >= 10, print( Timedigits ).

printTime( HoursToTime ) :-
    MinsDigits is floor( HoursToTime * 60 ),
    Hours is MinsDigits // 60,
    Mins is MinsDigits mod 60,
    printDigits( Hours ), print( ':' ), printDigits( Mins ).

fly( Depart, Depart ) :-
    write( 'Error:the depart and arrival time of: ' ), write(Depart),
    write( ' to '), write(Depart), write( ' are the same.' ),
    nl,
    !, fail.

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ), 
    airport( Arrive, _, _, _ ),
    createflight( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    writepath( List ),
    true.

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: flight from: ' ), write(Depart),
    write( ' to '), write(Arrive), write( ' is impossible.' ),
    !, fail.

fly( _, _) :- 
    write( 'Error: airports do not exist in database.' ), nl,
!,fail.

createflight( Terminal, Terminal, _, [Terminal], _ ).
createflight( Prev, Terminal, Visited, 
    [[Prev, FlightDep, FlightArr] | List], FlightDepartInHoursMin ) :-
    flight( Prev, Terminal, FlightDepartInHoursMin ),
    not( member( Terminal, Visited ) ),
    timeInHours( FlightDepartInHoursMin, FlightDep ),
    distance( Prev, Terminal, FDistance ),
    TimeDiff is FDistance / 500,
    FlightArr is FlightDep + TimeDiff,
    FlightArr < 24.0,
    createflight( Terminal, Terminal, [Terminal | Visited], List, _).
createflight( Prev, Terminal, Visited, 
    [[Prev, FlightDep, FlightArr] | List], FlightDepartInHoursMin ) :-
    flight( Prev, Next, FlightDepartInHoursMin ),
    not( member( Next, Visited ) ),
    timeInHours( FlightDepartInHoursMin, FlightDep ),
    distance( Prev, Next, FDistance ),
    TimeDiff is FDistance/500,    
    FlightArr is FlightDep + TimeDiff,
    FlightArr < 24.0,
    flight( Next, _, NextFlightDepartInHoursMin ),
    timeInHours( NextFlightDepartInHoursMin, NextFlightDep ),
    AdjustedTime is NextFlightDep - FlightArr - 0.5,
    AdjustedTime >= 0,
    createflight( Next, Terminal, [Next | Visited], 
        List, NextFlightDepartInHoursMin ).

writepath( [] ) :-
    nl.
writepath( [[X, XDTime, XATime], Y | []] ) :-
    airport( X, Depart_Ext, _, _), airport( Y, Arrive_Ext, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( X ), write( '  ' ),
    write( Depart_Ext ), printTime( XDTime ), nl,
    write( '     ' ), write( 'arrive  ' ),
    write( Y ), write( '  ' ),
    write( Arrive_Ext ), printTime( XATime ), nl,
    !, true.

writepath( [[X, XDTime, XATime], [Y, YDTime, YATime] | Z] ) :-
    airport( X, Depart_Ext, _, _), airport( Y, Arrive_Ext, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( X ), write( '  ' ),
    write( Depart_Ext ), printTime( XDTime ), nl,
    write( '     ' ), write( 'arrive  ' ),
    write( Y ), write( '  ' ),
    write( Arrive_Ext ), printTime( XATime ), nl,
    !, writepath( [[Y, YDTime, YATime] | Z] ).


