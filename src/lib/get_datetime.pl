get_datetime( Dtime ) :-
         get_time(T),stamp_date_time(T,Date,local), 
          Date = date(Y,M,D,H,N,S,_,_,_),
          Secs is integer(S),
          Dtime = datetime(Y,M,D,H,N,Secs).
