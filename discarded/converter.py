"""
author: Bez || blueburningcoder
This converter converts the given JSON data to a cypher-script-command
to load it in a neo4j database (the cypher-commands are being printed to
the console too)

INFO: the Database must not be password-protected since no Authorization
header is being send currently

"""
# __author__ = 'bez' || blueburningcoder

from py2neo import Graph
import LocationHandler
ticks_num = 96
data = LocationHandler.get_json_data(ticks_num)['edges']

# print data
# print data.__sizeof__() TODO: why 240? it's only 20 big

graph = Graph()
tx = graph.cypher.begin()
unique_ex_points = []

""" returns all the probs listed after another, with a 'tick_j:' between each other
"""
def probs_string(probs):
    ret = ""
    for j in range(0, ticks_num):
        ret += "tick_" + str(j) + ":" + str(probs[j])
        if j < ticks_num - 1:
            ret += ", "

    return ret

for i in range(0, 20):
    point = "MERGE (point:ExchangePoint {name:{name}}) RETURN point"
    print point + " (" + data[i]['from'] + ")"
    tx.append(point, name=data[i]['from'])

"""
commits the previously appended commands (creating the Nodes), MERGE only
creates if they weren't created beforehand (prevents a inconsistent second
type) after that, the Probabilities between the nodes are being transformed
in a cypher-command that is being printed too.

"""

tx.commit()

# Creates the expected
for i in range(0, 20):
    comm = "MATCH (point1:ExchangePoint), (point2:ExchangePoint) WHERE " \
           "point1.name=\"" + data[i]['from'] + "\" AND point2.name=\"" + \
           data[i]['to'] + "\" MERGE (point1)-[p:Probabilities {" + \
           probs_string(data[i]['probabilities']) + "}]->(point2) RETURN p"
    print comm
    graph.cypher.execute(comm)


"""







///////////////////////////////////////////////////////////////////////////////////////
////                      Random test-foo
///////////////////////////////////////////////////////////////////////////////////////




# sort of Datastructure for what:
# ExchangePoints - Probabilities - ExchangePoints -> many to many
# Paths - Route -> many to many
#
# Probabilities - Connections -> one to one
# Users - Paths -> one to many
# Packages - Route - Paths -> one to many


from py2neo import Graph, Path, watch

watch("httpstream")

graph = Graph()

graph.cypher.run("MATCH (p)-[r]->() OPTIONAL MATCH (n) DELETE n,p,r")

tx = graph.cypher.begin()

for name in ["Alice", "Bob", "Carol"]:
    tx.append("CREATE (person:Person {name:{name}, age:{age}}) RETURN person",
               name=name, age=20)
alice, bob, carol = [result.one for result in tx.commit()]


friends = Path(alice, "KNOWS", bob, "KNOWS", carol, "KNOWS", alice)
print friends
# graph.create(friends)


# from py2neo import Graph, Node, Relationship
# import LocationHandler
# # import json
#
# graph = Graph()
# graph.cypher.run("MATCH (p)-[r]->() OPTIONAL MATCH (n) DELETE n,p,r")
#
# alice = Node("Person", name="Alice")
# bob = Node("Person", name="Bob", age=20)
#
# print bob.labels
# print bob.properties
#
# alice_knows_bob = Relationship(alice, "KNOWS", bob, since=2012)
# print alice_knows_bob
# graph.create(alice_knows_bob)
#
# for rec in graph.cypher.execute("MATCH (p:Person) RETURN p.name AS name"):
#     print rec[0]
#
# data = LocationHandler.get_json_data(2)

"""
