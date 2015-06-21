# Get the Database up and running

## Download Neo4j
From here: [http://neo4j.com/download/](http://neo4j.com/download/)

## Get it running
For that, navigate in the extracted folder of the downloaded folder.
what you need to do anyways is to navigate to `/conf/` and edit in the 
`neo4j-server.properties` - file the `dbms.security.auth_enabled=true` 
setting from true to `false` (Necessary, or otherwise the server will 
refuse any connection without authentication, but a auth-header is 
not yet being sent with).

After that, navigate to `/../bin/`, and type `./neo4j console` to 
get it running on the console. Wait until it shows 

`INFO  [API] Remote interface ready and available at [http://localhost:7474/]`

somewhere at the bottom. Now you can call the server by typing 
[http://localhost:7474/](http://localhost:7474/)in your browser. 
If you did everything correct a neo4j instance should be running there.

## add Data to the database
For the api you need to install py2neo, which is easily to get with 
`sudo pip install py2neo`, and after this you can execute (in this directory) 
`python converter.py`, to load the database with some exchangePoints from the 
Json-files and some random probabilities (you will see some cypher-commands 
flying over the screen, don't worry, everything's alright).

If you want to see what data exactly you added to the database, open the neo4j gui 
in your browser and type in the command line at the top the following command:
`MATCH n RETURN n`. Hit enter and there should be a visualization of what data 
is currently loaded in your database, and how it is connected.

Have fun!