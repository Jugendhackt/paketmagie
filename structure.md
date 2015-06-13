Users can have paths, meaning they'll travel along it,
a path has a StartPoint, an EndPoint (actual locations )
and the exchangePoints on that route.
There's also two timestamps in which the User probably will be between these exchangePoints

Then there are Packages that have a start and end -loc (in exP), a maxTime in which the package should arrive,
as well as the ID of the Route, a Volume and a boolean if the package is currently travelling.

In a Route Object there's info about the exP's on the way, the timestamps when the package should arrive there and one
when the package should leave there, and a UserID who transports between those two locations
(the one mentioned is travelling from the first exP to the second, so the last element is empty)

An exchangePoint (exP) has the following: an ID, a real location, a Volume and some Timestamps,
as to which package is there and how long.
